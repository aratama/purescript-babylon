exports.createVertexData = function(props){
    return function(){
        var v = new BABYLON.VertexData();
        v.indices = props.indices.slice();
        v.positions = props.positions.slice();
        v.normals = props.normals.slice();
        v.uvs = props.uvs.slice();
        return v;
    }
}

exports.applyToMesh = function(mesh){
    return function(updatable){
        return function(vertexData){
            return function(){
                vertexData.applyToMesh(mesh, updatable);
            }
        }
    }
}

exports.merge = function(other){
    return function(mesh){
        return function(){
            mesh.merge(other);
        }
    }
}

exports.getIndices = function(dat){
    return function(){
        return dat.indices.slice();
    }
}