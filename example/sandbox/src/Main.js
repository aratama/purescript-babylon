exports.onMouseMove = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("mousemove", function(e){
            callback(e)();
        });
    }
}

exports.onMouseClick = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("click", function(e){
            callback(e)();
        });
    }
}

exports.onButtonClick = function(id){
    return function(callback){
        return function(){
            document.getElementById(id).addEventListener("click", function(){
                callback();
            });
        }
    }
}