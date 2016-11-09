exports.createStandardMaterial = function(id){
    return function(scene){
        return function(){
            return new BABYLON.StandardMaterial(id, scene);
        }
    }
}

exports.tandardMaterialToMaterial = function(mat){
    return mat;
}

exports.setDiffuseTexture = function(tex){
    return function(mat){
        return function(){
            mat.diffuseTexture = tex;
        }
    }
}

exports.setReflectionTexture = function(tex){
    return function(mat){
        return function(){
            mat.reflectionTexture = tex;
        }
    }
}

exports.standardMaterialToMaterial = function(mat){
    return mat;
}

exports.setBackFaceCulling = function(value){
    return function(mat){
        return function(){
            mat.backFaceCulling = value;
        }
    }
}

exports.setDisableLighting  = function(value){
    return function(mat){
        return function(){
            mat.disableLighting = value;
        }
    }
}

exports.setDiffuseColor  = function(value){
    return function(mat){
        return function(){
            mat.diffuseColor  = value;
        }
    }
}

exports.setSpecularColor = function(value){
    return function(mat){
        return function(){
            mat.specularColor = value;
        }
    }
}

exports.setSpecularPower = function(specularPower){
    return function(mat){
        return function(){
            mat.specularPower = specularPower;
        }
    }
}