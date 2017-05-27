"use strict";

var now, delta, last;

exports.requestAnimationFrame = function(callback) {
    return function() {
        last = last || Date.now();
        window.requestAnimationFrame(function(_) {
            now = Date.now();
            delta = (now - last) / 1000.0;
            last = now;
            callback(delta)();
        });
    }
}