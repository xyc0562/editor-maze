'use strict';

var _ws = require('ws');

var _ws2 = _interopRequireDefault(_ws);

var _merge = require('lodash/merge');

var _merge2 = _interopRequireDefault(_merge);

var _max = require('lodash/max');

var _max2 = _interopRequireDefault(_max);

var _filter = require('lodash/filter');

var _filter2 = _interopRequireDefault(_filter);

var _forEach = require('lodash/forEach');

var _forEach2 = _interopRequireDefault(_forEach);

var _find = require('lodash/find');

var _find2 = _interopRequireDefault(_find);

var _cloneDeep = require('lodash/cloneDeep');

var _cloneDeep2 = _interopRequireDefault(_cloneDeep);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var wss = new _ws2.default.Server({
    port: 3000,
    perMessageDeflate: {
        zlibDeflateOptions: { // See zlib defaults.
            chunkSize: 1024,
            memLevel: 7,
            level: 3
        },
        zlibInflateOptions: {
            chunkSize: 10 * 1024
        },
        // Other options settable:
        clientNoContextTakeover: true, // Defaults to negotiated value.
        serverNoContextTakeover: true, // Defaults to negotiated value.
        clientMaxWindowBits: 10, // Defaults to negotiated value.
        serverMaxWindowBits: 10, // Defaults to negotiated value.
        // Below options specified as default values.
        concurrencyLimit: 10, // Limits zlib concurrency for perf.
        threshold: 1024 // Size (in bytes) below which messages
        // should not be compressed.
    }
});

var broadcast = function broadcast(data) {
    data = JSON.stringify(data);
    wss.clients.forEach(function (client) {
        if (client.readyState === _ws2.default.OPEN) {
            client.send(data);
        }
    });
};

var curMaze = null;
var players = [];

var curId = 0;

var assignId = function assignId() {
    return ++curId;
};

var broadcastState = function broadcastState() {
    broadcast({
        t: "stateUpdate",
        players: players
    });
};

var handleMazeUpdate = function handleMazeUpdate(maze, shouldReset, id) {
    if (!curMaze || shouldReset) curMaze = maze;
    var me = (0, _find2.default)(players, function (p) {
        return p.id === id;
    });
    if (!me) {
        var assignedId = assignId();
        me = {
            id: assignedId,
            lastAliveAt: new Date().getTime(),
            x: 0,
            y: 0,
            mode: "ModeVim"
        };
        players.push(me);
    }
    return {
        t: "newGameUpdate",
        maze: curMaze,
        me: me,
        players: players
    };
};

var handleKeepAlive = function handleKeepAlive(id) {
    var i = 0;
    for (i = 0; i < players.length; i++) {
        if (players[i].id === id) players[i].lastAliveAt = new Date().getTime();
    }
    return null;
};

var handlePlayerStateUpdate = function handlePlayerStateUpdate(_ref) {
    var id = _ref.id,
        x = _ref.x,
        y = _ref.y,
        mode = _ref.mode;

    var i = void 0;
    for (i = 0; i < players.length; i++) {
        if (players[i].id === id) {
            players[i].x = x;
            players[i].y = y;
            players[i].mode = mode;
        }
    }
    broadcastState();
};

var handleWinnerUpdate = function handleWinnerUpdate(_ref2) {
    var result = _ref2.result;

    broadcast({
        t: "winnerUpdate",
        result: result
    });
};

var handleMsg = function handleMsg(msg) {
    var t = msg.t;
    switch (t) {
        case "mazeUpdate":
            return handleMazeUpdate(msg.maze, msg.newGame, msg.id);
        case "keepAlive":
            return handleKeepAlive(msg.id);
        case "playerStateUpdate":
            return handlePlayerStateUpdate(msg);
        case "winnerUpdate":
            return handleWinnerUpdate(msg);
    }
};

var handleNewGameUpdateSend = function handleNewGameUpdateSend(msg, toSend) {
    if (!msg.newGame) {
        return true;
    }
    wss.clients.forEach(function (client) {
        if (client.readyState === _ws2.default.OPEN) {
            var player = players.find(function (p) {
                return p.id === client.id;
            });
            if (player) {
                var toSendClone = (0, _cloneDeep2.default)(toSend);
                var data = (0, _merge2.default)(toSendClone, {
                    me: (0, _merge2.default)(player, { x: 0, y: 0 })
                });
                client.send(JSON.stringify(data));
            }
        }
    });
    return false;
};

wss.on('connection', function (ws) {
    ws.on('message', function (rawMsg) {
        var msg = JSON.parse(rawMsg);
        var toSend = handleMsg(msg);
        var shouldUnicast = true;
        if (toSend) {
            if (toSend.t === 'newGameUpdate') {
                shouldUnicast = handleNewGameUpdateSend(msg, toSend);
                ws.id = toSend.me.id;
            }
            if (shouldUnicast) ws.send(JSON.stringify(toSend));
        }
    });
    //  ws.send('something')
});

setInterval(function () {
    var now = new Date().getTime();
    var prevLen = players.length;
    players = (0, _filter2.default)(players, function (p) {
        return now - p.lastAliveAt < 3000;
    });
    // This means we need to update
    broadcastState();
}, 1000);