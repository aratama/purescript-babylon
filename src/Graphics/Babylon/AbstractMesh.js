exports.setCheckCollisions = function(checkCollisions){
    return function(mesh){
        return function(){
            mesh.checkCollisions = checkCollisions;
        }
    }
}

exports.abstractMeshToNode = function(mesh){
    return mesh;
}