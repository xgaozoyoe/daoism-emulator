import React, { Component } from 'react';
var WS = require('websocket').w3cwebsocket
WS === window.WebSocket
var ws = new WS('ws://127.0.0.1:9001', '', 'http://example.com');
var timer_status = 0;
window.ws = ws;
var universe = require ("../lib/js/src/universe.bs.js");

ws.onopen = function() {
    console.log('ws open');
    connected = true;
    universe.fetch_data ();
};
var connected = false;
var tiles = null;
ws.onmessage = function(event) {
    var tiles_container = document.getElementById("tiles");
    var npcs_container = document.getElementById("npcs");
    if (connected) {
        var json = JSON.parse(event.data);
        console.log(json);
        if (json.method == "global") {
          var global = json.data;
          console.log(global);
          universe.build_tiles(global.tiles, global.world, tiles_container);
          universe.build_npcs(global.npcs, global.world, npcs_container);
        } else if (json.method == "update") {
          console.log(json);
          var npcs = json.data.updates;
          var world = json.data.world;
          for (const idx in json.data.updates) {
            universe.update_npc(npcs[idx], world, npcs_container);
          }
        } else {
          console.log(json);
        }
    } else {
        console.log("not connected!");
        console.log(event.json);
    }
};

var focus = function(id) {
  alert (id);
}

var isDown = false; // whether mouse is pressed
var startCoords = []; // 'grab' coordinates when pressing mouse
var trans = [0, 0]; // previous coordinates of mouse release

document.getElementById("svg-container").onmousedown = function (e) {
  isDown = true;
  startCoords = [
        e.clientX, // set start coordinates
        e.clientY
  ];
}
document.getElementById("svg-container").onmouseup = function (e) {
  isDown = false;
  var x = e.clientX;
  var y = e.clientY;
  var matrix = this.createSVGMatrix();
  matrix = matrix.translate (trans[0] + x - startCoords[0], trans[1] + y - startCoords[1]);
  document.getElementById("map").transform.baseVal.getItem(0).setMatrix(matrix);
  trans = [
        trans[0] + e.clientX- startCoords[0], // set last coordinates
        trans[1] + e.clientY- startCoords[1]
  ];


}

document.getElementById("svg-container").onmousemove = function (e)
{
    if(!isDown) return; // don't pan if mouse is not pressed

    var x = e.clientX;
    var y = e.clientY;

    // set the canvas' transformation matrix by setting the amount of movement:
    // 1  0  dx
    // 0  1  dy
    // 0  0  1
    var matrix = this.createSVGMatrix();
    matrix = matrix.translate (trans[0] + x - startCoords[0], trans[1] + y - startCoords[1]);
    document.getElementById("map").transform.baseVal.getItem(0).setMatrix(matrix);
}


