exports.createTargetCamera = function(name){
    return function(position){
        return function(scene){
            return function(){
                return new BABYLON.TargetCamera(name, position, scene);
            }
        }
    }
}

exports.targetCameraToCamera = function(camera){
    return camera;
}

exports.setSpeed = function(speed){
    return function(camera){
        return function(){
            camera.speed = speed;
        }
    }
}


exports.getCameraRotation = function(camera){
    return function(){
        return camera.cameraRotation;
    }
}

exports.getRotation = function(camera){
    return function(){
        return camera.rotation;
    }
}

exports.setTarget = function(position){
    return function(camera){
        return function(){
            camera.setTarget(position);
        }
    }
}

exports.getTarget = function(camera){
    return function(){
        return camera.getTarget();
    }
}
