#ifdef VERTEX_SHADER
#define VS2FS out
#else
#define VS2FS in
#endif

uniform sampler2D input_texture;
uniform vec2 input_texture_inv_resolution;
uniform vec2 sample_offset_factor;
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
	frag_color = vec4(0);
	frag_color += textureLod(input_texture, uv + vec2(-2) * input_texture_inv_resolution * sample_offset_factor, sample_lod) * (1 / 16.0);
	frag_color += textureLod(input_texture, uv + vec2(-1) * input_texture_inv_resolution * sample_offset_factor, sample_lod) * (4 / 16.0);
	frag_color += textureLod(input_texture, uv + vec2( 0) * input_texture_inv_resolution * sample_offset_factor, sample_lod) * (6 / 16.0);
	frag_color += textureLod(input_texture, uv + vec2(+1) * input_texture_inv_resolution * sample_offset_factor, sample_lod) * (4 / 16.0);
	frag_color += textureLod(input_texture, uv + vec2(+2) * input_texture_inv_resolution * sample_offset_factor, sample_lod) * (1 / 16.0);
}

#endif
