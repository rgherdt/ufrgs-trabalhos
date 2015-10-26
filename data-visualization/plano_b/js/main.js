var scene = new THREE.Scene();
var camera = new THREE.OrthographicCamera(
	window.innerWidth / -100,
	window.innerWidth / 100,
	window.innerHeight / 100, 
	window.innerHeight / -100,
	-100, 100);

var renderer = new THREE.WebGLRenderer();
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

var material = new THREE.LineBasicMaterial({
	color: 0x0000ff
});

var geometry = new THREE.Geometry();
geometry.vertices.push(
	new THREE.Vector3( -1, 0, 0 ),
	new THREE.Vector3( 1, 0, 0 ),
	new THREE.Vector3( 0, -1, 0 ),
	new THREE.Vector3( 0, 1, 0 ),
	new THREE.Vector3( 0, 0, -1 ),
	new THREE.Vector3( 0, 0, 1 )
);

var line = new THREE.Line( geometry, material );
scene.add( line );

camera.position.z = 5;

var render = function () {
	requestAnimationFrame( render );

	renderer.render(scene, camera);
};

render();