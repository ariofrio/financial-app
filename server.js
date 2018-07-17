
const WebSocket = require('ws');
const ws = new WebSocket.Server({
  perMessageDeflate: false,
  port: process.env.PORT || 8001
});

const Elm = require('build/Store.js').worker();
const store = Elm.worker(Elm.Store);

// Initialize list of messages before doing anything else.
var messages = [];
const dataFile = './data/messages.json';
try {
  messages = JSON.parse(fs.readFileSync(dataFile, 'utf8'));
} catch (e) {
  if (e.code !== 'ENOENT')
    throw e;
  messages = [];
}

store.ports.appendMessage.subscribe(function(message) {
  message.push();
})

ws.on('connection', function onConnection(ws) {
  ws.send({
    init: 
  });
  ws.on('message', function onMessage(message) {
    // TODO: append actions to action list, validating permissions
  });
});

