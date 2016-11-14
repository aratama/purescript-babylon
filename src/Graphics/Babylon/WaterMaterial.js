exports.createWaterMaterial = function(name){
    return function(scene){
        return function(){
            return new BABYLON.WaterMaterial(name, scene);
        }
    }
}

exports.waterMaterialToMaterial = function(mat){
    return mat;
}

exports.setBumpTexture = function(tex){
    return function(mat){
        return function(){
            mat.bumpTexture = tex;
        }
    }
}

exports.addToRenderList = function(mesh){
    return function(mat){
        return function(){
            mat.addToRenderList(mesh);
        }
    }
}

exports.setWaveHeight = function(waveHeight){
    return function(mat){
        return function(){
            mat.waveHeight = waveHeight;
        }
    }
}

exports.setWindForce = function(windForce){
    return function(mat){
        return function(){
            mat.windForce = windForce;
        }
    }
}

