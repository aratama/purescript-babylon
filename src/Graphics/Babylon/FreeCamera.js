exports.createFreeCamera = function(id){
    return function(position){
        return function(scene){
            return function(){
                return new BABYLON.FreeCamera(id, position, scene);
            }
        }
    }
}

exports.freeCameraToCamera = function(camera){
    return camera;
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

exports.setCheckCollisions = function(checkCollisions){
    return function(camera){
        return function(){
            camera.checkCollisions = checkCollisions;
        }
    }
}

exports.setApplyGravity  = function(applyGravity ){
    return function(camera){
        return function(){
            camera.applyGravity  = applyGravity ;
        }
    }
}

