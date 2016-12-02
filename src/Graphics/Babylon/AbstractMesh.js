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

exports.dispose = function(doNotRecurse){
    return function(mesh){
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

exports.setEllipsoid = function(ellipsoid){
    return function(mesh){
        return function(){
            mesh.ellipsoid = ellipsoid;
        }
    }
}

exports.setEllipsoidOffset = function(ellipsoidOffset){
    return function(mesh){
        return function(){
            mesh.ellipsoidOffset = ellipsoidOffset;
        }
    }
}

exports.getPosition = function(mesh){
    return function(){
        return mesh.position;
    }
}

exports.setPosition = function(position){
    return function(mesh){
        return function(){
            mesh.position = position;
        }
    }
}

exports.setRotation = function(rotation){
    return function(mesh){
        return function(){
            mesh.rotation = rotation;
        }
    }
}

exports.setPhysicsImpostor = function(impostor){
    return function(mesh){
        return function(){
            mesh.physicsImpostor = impostor;
        }
    }
}

exports.setIsVisible = function(isVisible){
    return function(mesh){
        return function(){
            mesh.isVisible = isVisible;
        }
    }
}

exports.setUseVertexColors  = function(useVertexColors ){
    return function(mesh){
        return function(){
            mesh.useVertexColors  = useVertexColors ;
        }
    }
}

exports.onCollisionPositionChangeObservable = function(mesh){
    return mesh.onCollisionPositionChangeObservable;
}

exports.setRenderingGroupId = function(value){
    return function(mesh){
        return function(){
            mesh.renderingGroupId = value;
        }
    }
}

exports.setReceiveShadows = function(receiveShadows){
    return function(mesh){
        return function(){
            mesh.receiveShadows = receiveShadows;
        }
    }
}

exports.getSkeleton = function(mesh){
    return function(){
        return mesh.skeleton;
    }
}

exports.setMaterial = function(mat){
    return function(mesh){
        return function(){
            mesh.material = mat;
        }
    }
}


exports.setVisibility = function(visibility){
    return function(mesh){
        return function(){
            mesh.visibility = visibility;
        }
    }
}