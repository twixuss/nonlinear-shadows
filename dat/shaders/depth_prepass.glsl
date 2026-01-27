#ifdef VERTEX_SHADER
layout(location=0) in vec3 position;
layout(location=1) in vec3 normal;
layout(location=2) in vec2 uv;
#endif

uniform mat4 model_to_world;
uniform mat4 model_to_ndc;

#ifdef VERTEX_SHADER
void main() {
	gl_Position = model_to_ndc * vec4(position, 1);
}
#endif
