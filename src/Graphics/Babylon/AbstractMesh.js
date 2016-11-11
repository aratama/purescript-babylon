exports.setCheckCollisions = function(checkCollisions){
    return function(mesh){
        return function(){
            mesh.checkCollisions = checkCollisions;
        }
    }
}

exports.abstractMeshToNode = function(mesh){
    return mesh;
}

exports.applyImpulse = function(force){
    return function(contactPoint){
        return function(mesh){
            return function(){
                mesh.applyImpulse(force, contactPoint);
            }
        }
    }
}

exports.moveWithCollisions = function(velocity){
    return function(mesh){
        return function(){
            mesh.moveWithCollisions(velocity);
        }
    }
}

exports.intersects = function(ray){
    return function(fastCheck){
        return function(mesh){
            return function(){
                mesh.intersects(ray, fastCheck);
            }
        }
    }
}

exports.intersectsMesh = function(mesh){
    return function(fastCheck){
        return function(mesh){
            return function(){
                mesh.intersectsMesh(ray, fastCheck);
            }
        }
    }
}

exports.intersectsPoint = function(point){
    return function(mesh){
        return function(){
            mesh.intersectsPoint(point);
        }
    }
}

exports.lookAt = function(targetPoint){
    return function(yawCor){
        return function(pitchCor){
            return function(rollCor){
                return function(mesh){
                    return function(){
                        mesh.lookat(targetPoint, yawCor, pitchCor, rollCor, mesh);
                    }
                }
            }
        }
    }
}

exports.dispose = function(mesh){
    return function(doNotRecurse){
        return function(){
            mesh.dispose(doNotRecurse);
        }
    }
}


exports.setIsPickable = function(isPickable){
    return function(mesh){
        return function(){
            mesh.isPickable = isPickable;
        }
    }
}