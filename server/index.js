import WebSocket from 'ws'
import merge from 'lodash/merge'
import max from 'lodash/max'
import filter from 'lodash/filter'
import forEach from 'lodash/forEach'
import find from 'lodash/find'
import cloneDeep from 'lodash/cloneDeep'

const wss = new WebSocket.Server({
    port: 3000,
    perMessageDeflate: {
        zlibDeflateOptions: { // See zlib defaults.
            chunkSize: 1024,
            memLevel: 7,
            level: 3,
        },
        zlibInflateOptions: {
            chunkSize: 10 * 1024
        },
        // Other options settable:
        clientNoContextTakeover: true, // Defaults to negotiated value.
        serverNoContextTakeover: true, // Defaults to negotiated value.
        clientMaxWindowBits: 10,       // Defaults to negotiated value.
        serverMaxWindowBits: 10,       // Defaults to negotiated value.
        // Below options specified as default values.
        concurrencyLimit: 10,          // Limits zlib concurrency for perf.
        threshold: 1024,               // Size (in bytes) below which messages
        // should not be compressed.
    }
})

const broadcast = (data) => {
    data = JSON.stringify(data)
    wss.clients.forEach((client) => {
        if (client.readyState === WebSocket.OPEN) {
            client.send(data)
        }
    })
}

let curMaze = null
let players = []

let curId = 0

const assignId = () => {
    return ++curId
}

const broadcastState = () => {
    broadcast({
        t: "stateUpdate",
        players
    })        
}

const handleMazeUpdate = (maze, shouldReset, id) => {
    if (!curMaze || shouldReset) curMaze = maze
    let me = find(players, (p) => (p.id === id))
    if (!me) {
        const assignedId = assignId()
        me = {
            id: assignedId,
            lastAliveAt: new Date().getTime(),
            x: 0,
            y: 0,
            mode: "ModeVim"
        }
        players.push(me)
    }
    return {
        t: "newGameUpdate",
        maze: curMaze,
        me,
        players
    }
}

const handleKeepAlive = (id) => {
    let i = 0
    for (i = 0; i < players.length; i++) {
        if (players[i].id === id) players[i].lastAliveAt = new Date().getTime()
    }
    return null
}

const handlePlayerStateUpdate = ({ id, x, y, mode }) => {
    let i
    for (i = 0; i < players.length; i++) {
        if (players[i].id === id) {
            players[i].x = x
            players[i].y = y
            players[i].mode = mode
        }
    }
    broadcastState()
}

const handleWinnerUpdate = ({ result }) => {
    broadcast({
        t: "winnerUpdate",
        result
    })        
}

const handleMsg = (msg) => {
    const t = msg.t
    switch (t) {
        case "mazeUpdate":
            return handleMazeUpdate(msg.maze, msg.newGame, msg.id)
        case "keepAlive":
            return handleKeepAlive(msg.id)
        case "playerStateUpdate":
            return handlePlayerStateUpdate(msg)
        case "winnerUpdate":
            return handleWinnerUpdate(msg)
    }
}

const handleNewGameUpdateSend = (msg, toSend) => {
    if (!msg.newGame) {
        return true
    }
    wss.clients.forEach((client) => {
        if (client.readyState === WebSocket.OPEN) {
            const player = players.find((p) => p.id === client.id)
            if (player) {
                const toSendClone = cloneDeep(toSend)
                const data = 
                    merge(toSendClone, {
                        me: merge(player, { x: 0, y: 0 })
                    })
                client.send(JSON.stringify(data))
            }
        }
    })
    return false
}

wss.on('connection', (ws) => {
    ws.on('message', (rawMsg) => {
        const msg = JSON.parse(rawMsg)
        const toSend = handleMsg(msg)
        let shouldUnicast = true
        if (toSend) {
            if (toSend.t === 'newGameUpdate') {
                shouldUnicast = handleNewGameUpdateSend(msg, toSend)
                ws.id = toSend.me.id
            }
            if (shouldUnicast) ws.send(JSON.stringify(toSend))
        }
    })
    //  ws.send('something')
})

setInterval(() => {
    const now = new Date().getTime()
    const prevLen = players.length
    players = filter(players, (p) => now - p.lastAliveAt < 3000)
    // This means we need to update
    broadcastState()
}, 1000)
