/* global window */
/* global document */

"use strict";

exports.onDOMContentLoaded = function(callback){
    return function(){
        window.addEventListener("DOMContentLoaded", function(){
            callback();
        });
    };
};

exports.querySelectorCanvas = function(selector){
    return function(){
        return document.querySelector(selector);
    };
};