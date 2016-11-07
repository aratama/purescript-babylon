exports.createFreeCamera = function(id){
    return function(position){
        return function(scene){
            return function(){
                return new BABYLON.FreeCamera(id, position, scene);
            }
        }
    }
}

exports.setTarget = function(position){
    return function(camera){
        return function(){
            camera.setTarget(position);
        }
    }
}

exports.attachControl = function(canvas){
    return function(noPreventDefault){
        return function(camera){
            return function(){
                camera.attachControl(canvas, noPreventDefault);
            }
        }
    }
}