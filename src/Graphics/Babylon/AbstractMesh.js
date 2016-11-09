exports.setCheckCollisions = function(checkCollisions){
    return function(mesh){
        return function(){
            mesh.checkCollisions = checkCollisions;
        }
    }
}