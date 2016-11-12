exports.createVertexData = function(props){
    return function(){
        var v = new BABYLON.VertexData();
        v.indices = props.indices;
        v.positions = props.positions;
        v.normals = props.normals;
        v.uvs = props.uvs;
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