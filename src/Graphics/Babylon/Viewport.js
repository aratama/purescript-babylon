exports.createViewport = function(x){
    return function(y){
        return function(w){
            return function(h){
                return function(){
                    return new BABYLON.Viewport(x, y, w, h);
                }
            }
        }
    }
}