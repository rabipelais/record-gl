#version 440 core
uniform sampler2D tex;

in vec2 texCoordFrag;

out vec4 colour;

void main() {
	colour = texture(tex, texCoordFrag);
}
