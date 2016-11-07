exports.createEngine = function(canvas){
    return function(antialias){
        return function(){
            return new BABYLON.Engine(canvas, antialias);
        }
    }
}


exports.runRenderLoop = function(callback){
    return function(engine){
        return function(){
            engine.runRenderLoop(callback);
        }
    }
}