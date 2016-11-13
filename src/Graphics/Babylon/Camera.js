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