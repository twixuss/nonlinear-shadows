#include shadow_warping.glsli

ENUM_TO_GLSL(Algorithm, ENUMERATE_ALGORITHMS)
uniform int algorithm;

ENUM_TO_GLSL(TapPattern, ENUMERATE_TAP_PATTERN)
uniform int tap_pattern;

#ifdef VERTEX_SHADER
#define VS2FS out
#else
#define VS2FS in
#endif

#ifdef VERTEX_SHADER
layout(location=0) in vec3 position;
layout(location=1) in vec3 normal;
layout(location=2) in vec2 uv;
#endif

uniform mat4 model_to_world;
uniform mat4 model_to_ndc;
uniform mat4 world_to_light;
uniform vec3 light_direction;
uniform sampler2D shadow_map_color;
uniform sampler2D shadow_map_depth;
uniform sampler2DShadow shadow_map_depth_cmp;
uniform sampler2D ao_map;
uniform sampler3D random_texture;
uniform float shadow_map_world_width;
uniform float shadow_resolution;
uniform float pcf_radius;
uniform int pcf_tap_count;
uniform float transform_shadow_uvs_in_fragment_shader;
uniform float shadow_softness;
uniform vec2 shadow_sample_density;
uniform float variance_mip_level;
uniform float variance_light_bleeding_reduction;
uniform bool variance_variable_penumbra;
uniform float variance_min_limit;
uniform bool only_ambient;
uniform vec3 light_color;


VS2FS vec3 v_normal;
VS2FS vec3 v_world_position;
VS2FS vec2 v_uv;
VS2FS vec4 v_shadow_ndc;

#ifdef VERTEX_SHADER
void main() {
	v_normal = (model_to_world * vec4(normal, 0)).xyz;
	v_world_position = (model_to_world * vec4(position, 1)).xyz;
	v_uv = uv;
	gl_Position = model_to_ndc * vec4(position, 1);

	v_shadow_ndc = world_to_light * vec4(v_world_position, 1);
}
#endif

#ifdef FRAGMENT_SHADER

out vec4 fragment_color;

float map(float x, float a, float b, float c, float d) { return (x - a) / (b - a) * (d - c) + c; }
float map_clamped(float x, float a, float b, float c, float d) { return (clamp(x, min(a, b), max(a, b)) - a) / (b - a) * (d - c) + c; }

vec3 shadow_uv_from_ndc(vec3 ndc) {
	vec3 ndc_encoded = ndc;
	ndc_encoded.xy = encode(ndc_encoded.xy);
	return ndc_encoded * 0.5 + 0.5;
}

float ChebyshevUpperBound(vec2 Moments, float t) {
	// One-tailed inequality valid if t > Moments.x
	float p = float(t <= Moments.x);
	// Compute variance.
	float Variance = Moments.y - (Moments.x * Moments.x);
	Variance = max(Variance, variance_min_limit);
	// Compute probabilistic upper bound.
	float d = t - Moments.x;
	float p_max = Variance / (Variance + d * d);
	p = max(p, p_max);
	//p = map_clamped(p, 0.5, 1, 0, 1);
	return p;
}

vec2[] pcf_circular_offsets = vec2[](
	vec2(0,0),
	vec2(0.866,0.5),
	vec2(-0,1),
	vec2(-0.866,0.5),
	vec2(-0.866,-0.5),
	vec2(0,-1),
	vec2(0.866,-0.5),
	vec2(1.848,0.765),
	vec2(1.218,1.587),
	vec2(0.261,1.983),
	vec2(-0.765,1.848),
	vec2(-1.587,1.218),
	vec2(-1.983,0.261),
	vec2(-1.848,-0.765),
	vec2(-1.218,-1.587),
	vec2(-0.261,-1.983),
	vec2(0.765,-1.848),
	vec2(1.587,-1.218),
	vec2(1.983,-0.261),
	vec2(2.898,0.776),
	vec2(2.457,1.721),
	vec2(1.721,2.457),
	vec2(0.776,2.898),
	vec2(-0.261,2.989),
	vec2(-1.268,2.719),
	vec2(-2.121,2.121),
	vec2(-2.719,1.268),
	vec2(-2.989,0.261),
	vec2(-2.898,-0.776),
	vec2(-2.457,-1.721),
	vec2(-1.721,-2.457),
	vec2(-0.776,-2.898),
	vec2(0.261,-2.989),
	vec2(1.268,-2.719),
	vec2(2.121,-2.121),
	vec2(2.719,-1.268),
	vec2(2.989,-0.261),
	vec2(3.966,0.522),
	vec2(3.696,1.531),
	vec2(3.173,2.435),
	vec2(2.435,3.173),
	vec2(1.531,3.696),
	vec2(0.522,3.966),
	vec2(-0.522,3.966),
	vec2(-1.531,3.696),
	vec2(-2.435,3.173),
	vec2(-3.173,2.435),
	vec2(-3.696,1.531),
	vec2(-3.966,0.522),
	vec2(-3.966,-0.522),
	vec2(-3.696,-1.531),
	vec2(-3.173,-2.435),
	vec2(-2.435,-3.173),
	vec2(-1.531,-3.696),
	vec2(-0.522,-3.966),
	vec2(0.522,-3.966),
	vec2(1.531,-3.696),
	vec2(2.435,-3.173),
	vec2(3.173,-2.435),
	vec2(3.696,-1.531),
	vec2(3.966,-0.522),
	vec2(5,0),
	vec2(4.891,1.04),
	vec2(4.568,2.034),
	vec2(4.045,2.939),
	vec2(3.346,3.716),
	vec2(2.5,4.33),
	vec2(1.545,4.755),
	vec2(0.523,4.973),
	vec2(-0.523,4.973),
	vec2(-1.545,4.755),
	vec2(-2.5,4.33),
	vec2(-3.346,3.716),
	vec2(-4.045,2.939),
	vec2(-4.568,2.034),
	vec2(-4.891,1.04),
	vec2(-5,-0),
	vec2(-4.891,-1.04),
	vec2(-4.568,-2.034),
	vec2(-4.045,-2.939),
	vec2(-3.346,-3.716),
	vec2(-2.5,-4.33),
	vec2(-1.545,-4.755),
	vec2(-0.523,-4.973),
	vec2(0.523,-4.973),
	vec2(1.545,-4.755),
	vec2(2.5,-4.33),
	vec2(3.346,-3.716),
	vec2(4.045,-2.939),
	vec2(4.568,-2.034),
	vec2(4.891,-1.04)
);

vec2[] pcf_quadratic_offsets = vec2[](
	vec2(0,0),
	vec2(1,-1),
	vec2(-1,-1),
	vec2(1,0),
	vec2(-1,0),
	vec2(1,1),
	vec2(-1,1),
	vec2(0,1),
	vec2(0,-1),
	vec2(2,-2),
	vec2(-2,-2),
	vec2(2,-1),
	vec2(-2,-1),
	vec2(2,0),
	vec2(-2,0),
	vec2(2,1),
	vec2(-2,1),
	vec2(2,2),
	vec2(-2,2),
	vec2(-1,2),
	vec2(-1,-2),
	vec2(0,2),
	vec2(0,-2),
	vec2(1,2),
	vec2(1,-2),
	vec2(3,-3),
	vec2(-3,-3),
	vec2(3,-2),
	vec2(-3,-2),
	vec2(3,-1),
	vec2(-3,-1),
	vec2(3,0),
	vec2(-3,0),
	vec2(3,1),
	vec2(-3,1),
	vec2(3,2),
	vec2(-3,2),
	vec2(3,3),
	vec2(-3,3),
	vec2(-2,3),
	vec2(-2,-3),
	vec2(-1,3),
	vec2(-1,-3),
	vec2(0,3),
	vec2(0,-3),
	vec2(1,3),
	vec2(1,-3),
	vec2(2,3),
	vec2(2,-3),
	vec2(4,-4),
	vec2(-4,-4),
	vec2(4,-3),
	vec2(-4,-3),
	vec2(4,-2),
	vec2(-4,-2),
	vec2(4,-1),
	vec2(-4,-1),
	vec2(4,0),
	vec2(-4,0),
	vec2(4,1),
	vec2(-4,1),
	vec2(4,2),
	vec2(-4,2),
	vec2(4,3),
	vec2(-4,3),
	vec2(4,4),
	vec2(-4,4),
	vec2(-3,4),
	vec2(-3,-4),
	vec2(-2,4),
	vec2(-2,-4),
	vec2(-1,4),
	vec2(-1,-4),
	vec2(0,4),
	vec2(0,-4),
	vec2(1,4),
	vec2(1,-4),
	vec2(2,4),
	vec2(2,-4),
	vec2(3,4),
	vec2(3,-4),
	vec2(5,-5),
	vec2(-5,-5),
	vec2(5,-4),
	vec2(-5,-4),
	vec2(5,-3),
	vec2(-5,-3),
	vec2(5,-2),
	vec2(-5,-2),
	vec2(5,-1),
	vec2(-5,-1),
	vec2(5,0),
	vec2(-5,0),
	vec2(5,1),
	vec2(-5,1),
	vec2(5,2),
	vec2(-5,2),
	vec2(5,3),
	vec2(-5,3),
	vec2(5,4),
	vec2(-5,4),
	vec2(5,5),
	vec2(-5,5),
	vec2(-4,5),
	vec2(-4,-5),
	vec2(-3,5),
	vec2(-3,-5),
	vec2(-2,5),
	vec2(-2,-5),
	vec2(-1,5),
	vec2(-1,-5),
	vec2(0,5),
	vec2(0,-5),
	vec2(1,5),
	vec2(1,-5),
	vec2(2,5),
	vec2(2,-5),
	vec2(3,5),
	vec2(3,-5),
	vec2(4,5),
	vec2(4,-5)
);

struct BlockerSearchResult {
	bool has_blocker;
	float average_occluder_depth;
};

BlockerSearchResult blocker_search(float shadow_uv_z) {
	BlockerSearchResult result;

	result.has_blocker = false;
	result.average_occluder_depth = 0;
	float average_occluder_depth_divider = 0;
	
	for (int i = 0; i < pcf_tap_count; ++i) {
		vec2 d = tap_pattern == TapPattern_circular ? pcf_circular_offsets[i] : pcf_quadratic_offsets[i];

		vec2 occluder_uv = encode(v_shadow_ndc.xy + d / shadow_resolution / shadow_sample_density * 2) * 0.5 + 0.5;
		
		//float occluder_depth = texture(shadow_map_depth, occluder_uv).x;

		//float occluder_depth = texelFetch(shadow_map_depth, ivec2(occluder_uv * shadow_resolution), 0).x;
		
		vec4 n = textureGather(shadow_map_depth, occluder_uv, 0);
		float occluder_depth = min(min(n.x, n.y), min(n.z, n.w));

		if (occluder_depth < shadow_uv_z) {
			result.average_occluder_depth += occluder_depth;
			average_occluder_depth_divider += 1;
		}
	}

	if (average_occluder_depth_divider > 0) {
		result.average_occluder_depth /= average_occluder_depth_divider;
		result.has_blocker = true;
	}

	return result;
}

void main() {
	vec3 ambient_color = vec3(1,2,3) * 0.03;
	float ambient_occlusion = texture(ao_map, v_uv).x;
	if (only_ambient) {
		fragment_color = vec4(ambient_color * ambient_occlusion, 1);
		return;
	}

	vec3 shadow_ndc_encoded = v_shadow_ndc.xyz;
	shadow_ndc_encoded.xy = encode(shadow_ndc_encoded.xy);
	vec3 shadow_uv = shadow_ndc_encoded.xyz * 0.5 + 0.5;

	float lightness = max(1e-3f, dot(v_normal, light_direction));

	float shadow_mask = clamp(map(max(max(abs(v_shadow_ndc.x), abs(v_shadow_ndc.y)), abs(v_shadow_ndc.z)), 0.9, 1, 1, 0), 0, 1);

	if (shadow_mask > 0) {
		switch (algorithm) {
			default: {
				switch (sampling_mode) {
					default: {
						lightness *= mix(1, texture(shadow_map_depth_cmp, shadow_uv), shadow_mask);
						break;
					}
					case SamplingMode_soft_uniform: {
						float light_visibility = 0;

						for (int i = 0; i < pcf_tap_count; ++i) {
							vec2 d = tap_pattern == TapPattern_circular ? pcf_circular_offsets[i] : pcf_quadratic_offsets[i];
							vec3 p = vec3(encode(v_shadow_ndc.xy + d / shadow_resolution / shadow_sample_density * 2) * 0.5 + 0.5, shadow_uv.z);
							light_visibility += texture(shadow_map_depth_cmp, p);
						}

						lightness *= mix(1, light_visibility / pcf_tap_count, shadow_mask);

						break;
					}
					case SamplingMode_soft_dynamic: {
						BlockerSearchResult blocker = blocker_search(shadow_uv.z);

						if (blocker.has_blocker) {
							float light_visibility = 0;
							float iters = 0;

							float sample_radius = clamp((shadow_uv.z - blocker.average_occluder_depth) * shadow_softness, 0, 1);

							//int r = int(gl_FragCoord.y) * 3037000507 + int(gl_FragCoord.x) * 13879027;
							//vec2 random_offset = vec2(
							//	texelFetch(random_texture, ivec3(r >>  8, r >>  4, r >>  0) & 15, 0).x,
							//	texelFetch(random_texture, ivec3(r >> 20, r >> 16, r >> 12) & 15, 0).x) - 0.5;
							
							vec3 seed = mat3(0.875, 0.433, -0.217, -0.217, 0.75, 0.625, 0.433, -0.5, 0.75) * v_world_position * (1 << 8);
							
							vec2 random_offset = vec2(
								texture(random_texture, seed + 0.0, 0).x,
								texture(random_texture, seed + 0.5, 0).x) - 0.5;

							for (int i = 0; i < pcf_tap_count; ++i) {
								vec2 d = tap_pattern == TapPattern_circular ? pcf_circular_offsets[i] : pcf_quadratic_offsets[i];
								//d += random_offset / shadow_sample_density * 0.5;
								//float weight = map_clamped(length(d), pcf_radius, pcf_radius + 1, 1, 0);
								vec3 p = vec3(encode(v_shadow_ndc.xy + d / shadow_resolution / shadow_sample_density * sample_radius * 2) * 0.5 + 0.5, shadow_uv.z);
								light_visibility += texture(shadow_map_depth_cmp, p)/* * weight*/;
								iters += 1; //weight;
							}
							lightness *= mix(1, light_visibility / iters, shadow_mask);
						}
						break;
					}
				}
				break;
			}
			case Algorithm_variance: {
				if (variance_variable_penumbra) {
					BlockerSearchResult blocker = blocker_search(shadow_uv.z);
					
					if (blocker.has_blocker) {
						float softness = (shadow_uv.z - blocker.average_occluder_depth) * shadow_softness * 16;
						float mip_level = log2(max(softness, 1));

						vec2 moments = textureLod(shadow_map_color, shadow_uv.xy, mip_level).rg;
						lightness *= mix(1, map_clamped(ChebyshevUpperBound(moments, shadow_uv.z), variance_light_bleeding_reduction, 1, 0, 1), shadow_mask);
					}
				} else {
					vec2 moments = textureLod(shadow_map_color, shadow_uv.xy, variance_mip_level).rg;
					lightness *= mix(1, map_clamped(ChebyshevUpperBound(moments, shadow_uv.z), variance_light_bleeding_reduction, 1, 0, 1), shadow_mask);
				}
				break;
			}
		}

		//fragment_color = vec4(shadow_uv.xy, 0, 1);

		//if (f > 0)
		//	fragment_color = vec4(f);
		//else
		//	fragment_color = vec4(-f, 0, 0, 1);
		//return;
	}

	vec3 light = vec3(lightness) * light_color;

	fragment_color = vec4(light * ambient_occlusion, 1);
}

#endif