exports.createShadowGenerator = function(size){
    return function(light){
        return function(){
            return new BABYLON.ShadowGenerator(size, light);
        }
    }
}

exports.getShadowMap = function(shadowGenerator){
    return function(){
        return shadowGenerator.getShadowMap();
    }
}

exports.getRenderList = function(shadowMap){
    return function(){
        return shadowMap.renderList;
    }
}

exports.pushToRenderList = function(mesh){
    return function(renderList){
        return function(){
            renderList.push(mesh);
        }
    }
}

exports.setRenderList = function(renderList){
    return function(shadowMap){
        return function(){
            shadowMap.renderList = renderList.filter(function(mesh){
                return 0 < mesh.getTotalVertices();
            });
        }
    }
}

exports.setBias = function(bias){
    return function(generator){
        return function(){
            generator.bias = bias;
        }
    }
}

exports.setUsePoissonSampling = function(usePoissonSampling){
    return function(generator){
        return function(){
            generator.usePoissonSampling = usePoissonSampling;
        }
    }
}