exports.setSpeed = function(speed){
    return function(camera){
        return function(){
            camera.speed = speed;
        }
    }
}