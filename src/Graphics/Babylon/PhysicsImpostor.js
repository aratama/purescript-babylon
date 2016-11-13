exports.createPhysicsImpostor = function(object){
    return function(type){
        return function(options){
            return function(scene){
                return function(){
                    return new BABYLON.PhysicsImpostor(object, type, options, scene);
                }
            }
        }
    }
}

exports.sphereImpostor = BABYLON.PhysicsImpostor.SphereImpostor;

exports.boxImpostor = BABYLON.PhysicsImpostor.BoxImpostor;

exports.planeImpostor = BABYLON.PhysicsImpostor.PlaneImpostor;

exports.meshImpostor = BABYLON.PhysicsImpostor.MeshImpostor;

exports.cylinderImpostor = BABYLON.PhysicsImpostor.CylinderImpostor;

exports.particleImpostor = BABYLON.PhysicsImpostor.ParticleImpostor;

exports.heightmapImpostor = BABYLON.PhysicsImpostor.HeightmapImpostor;