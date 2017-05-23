"use strict";

var elem = null;

exports.logImpl = function(str) {
    return function(ending) {
        return function() {
            elem = elem || document.getElementById("console");
            elem.innerHTML += str + ending;
        }
    }
}