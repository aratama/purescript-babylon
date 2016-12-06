"use strict";

exports.add = function(callback){
    return function(observable){
        return function(){
            observable.add(function(value){
                callback(value)();
            });
        };
    };
};