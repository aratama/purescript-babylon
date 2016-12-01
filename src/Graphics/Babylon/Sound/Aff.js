exports._loadSound = function(name){
    return function(url){
        return function(scene){
            return function(options){
                return function(reject){
                    return function(resolve){
                        return function(){
                            var sound = new BABYLON.Sound(name, url, scene, function(){
                                resolve(sound)();
                            }, options);
                        }
                    }
                }
            }
        }
    }
}