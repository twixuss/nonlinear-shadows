#ifdef VERTEX_SHADER
#define VS2FS out
#else
#define VS2FS in
#endif

uniform sampler2D input_texture;

VS2FS vec2 uv;

#ifdef VERTEX_SHADER

void main() {
	vec4[] positions = vec4[](
		vec4(-1, -1, 0, 1),
		vec4(-1,  1, 0, 1),
		vec4( 1, -1, 0, 1),
		vec4( 1, -1, 0, 1),
		vec4(-1,  1, 0, 1),
		vec4( 1,  1, 0, 1)
	);
	gl_Position = positions[gl_VertexID];

	uv = gl_Position.xy * 0.5 + 0.5;
}

#endif

#ifdef FRAGMENT_SHADER

out vec4 frag_color;

void main() {
	frag_color = pow(texture(input_texture, uv), vec4(1.0 / 2.2));
}

#endif
