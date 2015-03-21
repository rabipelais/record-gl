#version 440 core
uniform mat3 cam;

in vec2 vertexCoord;
in vec2 texCoord;

out vec2 texCoordFrag;

void main() {
	gl_Position = vec4(cam * (vec3(vertexCoord, 1) * 2 - 1), 1);
	texCoordFrag = texCoord;
}
