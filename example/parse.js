#!/usr/bin/env node

var stdin = process.stdin,
    inputChunks = '';

stdin.resume();
stdin.setEncoding('utf8');

stdin.on('data', function(chunk) {
    console.log('are you ok, node?');
    inputChunks += chunk;
});

stdin.on('end', function() {
    console.log('ok');
    var parsedData = JSON.parse(inputJSON);
    var app = Elm.Display.worker(parsedData);
    app.ports.emit.subscribe(function(data) {
        console.log(data);
    });
});
