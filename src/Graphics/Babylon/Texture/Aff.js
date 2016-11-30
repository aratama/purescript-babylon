exports._loadTexture = function(path){
    return function(scene){
        return function(options){
            return function(reject){
                return function(resolve){
                    return function(){
                        var texture = new BABYLON.Texture(path, scene, options.noMipmap, options.invertY, options.samplingMode,
                            function(){
                                options.onLoad({})();
                                resolve(texture)();
                            },
                            function(){
                                options.onError({})();
                                reject(new Error())();
                            }
                        );
                    }
                }
            }
        }
    }
}