exports.setFogEnabled = function(fogEnabled){
    return function(material){
        return function(){
            material.fogEnabled = fogEnabled;
        }
    }
}