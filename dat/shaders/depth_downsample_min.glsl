#ifdef VERTEX_SHADER
#define VS2FS out
#else
#define VS2FS in
#endif

uniform sampler2D input_texture;
uniform float sample_lod;

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
	frag_color = vec4(1e9, 0, 0, 0);

	for (int dx = 0; dx < 2; ++dx) {
	for (int dy = 0; dy < 2; ++dy) {
		ivec2 d = ivec2(gl_FragCoord.xy) * 2;

		float p = texelFetchOffset(input_texture, d, 0, ivec2(dx, dy)).x;

		frag_color.x = min(frag_color.x, p);
	}
	}
}

#endif
