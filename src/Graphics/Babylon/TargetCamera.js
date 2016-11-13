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