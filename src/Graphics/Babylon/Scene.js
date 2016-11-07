exports.createScene = function(engine){
    return function(){
        return new BABYLON.Scene(engine);
    }
}

exports.render = function(scene){
    return function(){
        scene.render();
    }
}