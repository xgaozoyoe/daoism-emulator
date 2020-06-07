import React, { Component } from 'react';
var WS = require('websocket').w3cwebsocket
WS === window.WebSocket
var universe = require ("./universe.bs.js");

function component(map_info) {
  // Lodash, currently included via a script, is required for this line to work
  //element.innerHTML = "test";
  var guest = universe.build_tiles(50, 50, map_info.tiles);
  var guest = guest + universe.build_npcs(map_info.npcs);
  return guest;
}

var ws = new WS('ws://127.0.0.1:9001', '', 'http://example.com');
var timer_status = 0;
ws.onopen = function() {
    console.log('ws open');
    connected = true;
    ws.send("get_data");
};
var connected = false;
var tiles = null;
ws.onmessage = function(event) {
    if (connected) {
        console.log(event.data);
        var json = JSON.parse(event.data);
        if (json.method == "global") {
          var ihtml = "";
          var global = json.data;
          var map = global;
          universe.initialize_coordinate(50,50,map.tiles);
          document.getElementById("test").innerHTML = universe.build_tiles(50,50,map.tiles)
            + universe.build_npcs(map.npcs);
        } else if (json.method == "update") {
          var npcs = json.data.updates;
          for (const idx in json.data.updates) {
            console.log(npcs[idx].name);
            var element = document.getElementById(npcs[idx].name);
            if (element) {
              element.outerHTML = universe.update_npc(npcs[idx]);
            } else {
              document.getElementById("test").innerHTML += universe.update_npc(npcs[idx])
            }
          }
        } else {
          console.log(json);
        }
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
//timer_status = setInterval("window.fetch_status_data()",100);


