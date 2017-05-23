"use strict";

exports.onKeyDown = function(callback) {
    return function() {
        document.addEventListener("keydown", function(event) {
            callback(event.which)();
        });
    }
}

exports.onKeyUp = function(callback) {
    return function() {
        document.addEventListener("keyup", function(event) {
            callback(event.which)();
        });
    }
}