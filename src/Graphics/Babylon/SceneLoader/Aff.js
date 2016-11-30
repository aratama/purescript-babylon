exports._loadMesh = function(meshesNames){
    return function(rootUrl){
        return function(sceneFilename){
            return function(scene){
                return function(onsuccess){
                    return function(progressCallBack){
                        return function(onerror){
                            return function(){
                                BABYLON.SceneLoader.ImportMesh(
                                    meshesNames, rootUrl, sceneFilename, scene,
                                    function(result){
                                        onsuccess(result)()
                                    },
                                    function(e){
                                        progressCallBack(e)()
                                    },
                                    function(e){
                                        onerror(e)()
                                    }
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}