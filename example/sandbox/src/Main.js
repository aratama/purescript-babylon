exports.onMouseMove = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("mousemove", function(e){
            callback(e)();
        });
    }
}