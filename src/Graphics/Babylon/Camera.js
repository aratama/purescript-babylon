exports.getPosition = function(camera){
    return function(){
        return camera.position;
    }
}

exports.setPosition = function(position){
    return function(camera){
        return function(){
            camera.position = position;
        }
    }
}

exports.pERSPECTIVE_CAMERA = BABYLON.Camera.PERSPECTIVE_CAMERA;

exports.oRTHOGRAPHIC_CAMERA = BABYLON.Camera.ORTHOGRAPHIC_CAMERA;

exports.setMode = function(mode){
    return function(camera){
        return function(){
            camera.mode = mode;
        }
    }
}

exports.setOrthoLeft = function(orthoLeft){
    return function(camera){
        return function(){
            camera.orthoLeft = orthoLeft;
        }
    }
}

exports.setOrthoRight = function(orthoRight){
    return function(camera){
        return function(){
            camera.orthoRight = orthoRight;
        }
    }
}

exports.setOrthoTop = function(orthoTop){
    return function(camera){
        return function(){
            camera.orthoTop  = orthoTop;
        }
    }
}

exports.setOrthoBottom = function(orthoBottom){
    return function(camera){
        return function(){
            camera.orthoBottom = orthoBottom;
        }
    }
}

exports.setViewport = function(viewport){
    return function(camera){
        return function(){
            camera.viewport = viewport;
        }
    }
}