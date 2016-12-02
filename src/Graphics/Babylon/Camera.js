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

exports.pERSPECTIVE_CAMERA = 0; // BABYLON.Camera.PERSPECTIVE_CAMERA

exports.oRTHOGRAPHIC_CAMERA = 1; // BABYLON.Camera.ORTHOGRAPHIC_CAMERA

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

exports.setMaxZ = function(maxZ){
    return function(camera){
        return function(){
            camera.maxZ = maxZ;
        }
    }
}

exports.setMinZ = function(minZ){
    return function(camera){
        return function(){
            camera.minZ = minZ;
        }
    }
}