import React, { Component } from 'react';
var WS = require('websocket').w3cwebsocket
WS === window.WebSocket
var map = require ("./map.bs.js");

function component(map_info) {
  // Lodash, currently included via a script, is required for this line to work
  //element.innerHTML = "test";
  var guest = map.build_tiles(50, 50, map_info);
  return guest;
}

var ws = new WS('ws://127.0.0.1:9001', '', 'http://example.com');
var timer_status = 0;
ws.onopen = function() {
    console.log('ws open');
    connected = true;
};
var connected = false;
ws.onmessage = function(event) {
    if (connected) {
        console.log(event.data);
        var json = JSON.parse(event.data);
        var ihtml = "";
        for(const npc in json.Npcs) {
            console.log(npc);
            ihtml += map.build_npc(key);
        };
        var map = json.tiles;
        document.getElementById("test").innerHTML = component(map);
        //document.getElementById("npcs").innerHTML = ihtml;
    } else {
        console.log("not connected!");
        console.log(event.json);
    }
};

window.fetch_status_data = function() {
  if (ws.readyState == WS.OPEN) {
    ws.send("get_data");
  };
}
timer_status = setInterval("window.fetch_status_data()",100);


