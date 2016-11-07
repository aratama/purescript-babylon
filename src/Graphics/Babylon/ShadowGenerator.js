exports.createShadowGenerator = function(size){
    return function(light){
        return function(){
            var s = new BABYLON.ShadowGenerator(size, light);
            //s.useBlurVarianceShadowMap  =true;
            //s.blurScale = 0.9;
            //s.blurBoxOffset = 0.1;
            return s;
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

exports.setBias = function(bias){
    return function(generator){
        return function(){
            generator.bias = bias;
        }
    }
}