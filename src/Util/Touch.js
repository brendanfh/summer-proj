"use strict";

//module Util.Touch

function getTouchIndex(touches, findID) {
    for (var i=0; i < touches.length; i++) {
        var id = touches[i].identifier;
        if(id == findID) {
             return i;
        }
    }
    return -1;
}

var mouseDown = false;

exports.setupTouch = function (touchRef) {
    return function(GAME_WIDTH) {
        return function(GAME_HEIGHT) {
            return function() {
                document.addEventListener("touchstart", function(evt) {
                    var touches = evt.changedTouches;
                    for (var i=0; i < touches.length; i++) {
                        touchRef.value.push({ x : (touches[i].pageX / window.innerWidth) * GAME_WIDTH
                                            , y : (touches[i].pageY / window.innerHeight) * GAME_HEIGHT
                                            , identifier : touches[i].identifier});
                    }
                    
                    return false;
                });
                document.addEventListener("touchmove", function(evt) {
                    var touches = evt.changedTouches;
                    for (var i=0; i< touches.length; i++) {
                        var idx = getTouchIndex(touchRef.value, touches[i].identifier);
                        touchRef.value.splice(idx, 1, { x : (touches[i].pageX / window.innerWidth) * GAME_WIDTH
                                                      , y : (touches[i].pageY / window.innerHeight) * GAME_HEIGHT
                                                      , identifier : touches[i].identifier});
                    }
                    
                    return false;
                });
                document.addEventListener("touchend", function(evt) {
                    var touches = evt.changedTouches;
                    for (var i=0; i < touches.length; i++) {
                        var idx = getTouchIndex(touchRef.value, touches[i].identifier);
                        touchRef.value.splice(idx, 1);
                    }
                    
                    return false;
                });
                document.addEventListener("touchcancel", function(evt) {
                    var touches = evt.changedTouches;
                    for (var i=0; i < touches.length; i++) {
                        var idx = getTouchIndex(touchRef.value, touches[i].identifier);
                        touchRef.value.splice(idx, 1);
                    }
                    
                    return false;
                });
                document.addEventListener("mousedown", function(evt) {
                    mouseDown = true;
                    touchRef.value.push({ x : evt.pageX
                                        , y : evt.pageY
                                        , identifier : 100});
                    return false;
                });
                document.addEventListener("mousemove", function(evt) {
                    if (!mouseDown) return false;
                    var idx = getTouchIndex(touchRef.value, 100);
                    touchRef.value.splice(idx, 1, { x : evt.pageX
                                                  , y : evt.pageY
                                                  , identifier : 100 });
                    return false; 
                });
                document.addEventListener("mouseup", function(evt) {
                    mouseDown = false;
                    var idx = getTouchIndex(touchRef.value, 100);
                    touchRef.value.splice(idx, 1);
                    return false;
                });
                return {};
            }
        }
    }
};