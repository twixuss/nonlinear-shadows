ENUM_TO_GLSL(Algorithm, ENUMERATE_ALGORITHMS)

#ifdef VERTEX_SHADER
#define VS2FS out
#else
#define VS2FS in
#endif

uniform mat4 model_to_ndc;
uniform int algorithm;
uniform float constant_bias;
uniform float slope_scaled_bias;
uniform float normal_bias;
uniform vec3 light_direction;

VS2FS vec3 v_normal;
VS2FS float depth;

#ifdef VERTEX_SHADER

#include shadow_warping.glsli

#define VERTEX_PULLING 0

#if VERTEX_PULLING
struct BufferVertex {
	float[3] position;
	float[2] uv;
	float[3] normal;
};

struct Vertex {
	vec3 position;
	vec2 uv;
	vec3 normal;
};

layout(std430, binding=0) restrict readonly buffer Vertices { BufferVertex vertices[]; };
layout(std430, binding=1) restrict readonly buffer Indices { uint indices[]; };

vec2 to_vec2(float[2] f) { return vec2(f[0], f[1]); }
vec3 to_vec3(float[3] f) { return vec3(f[0], f[1], f[2]); }
Vertex unfuck(BufferVertex b) {
	Vertex v;
	v.position = to_vec3(b.position);
	v.uv = to_vec2(b.uv);
	v.normal = to_vec3(b.normal);
	return v;
}
#else
layout(location=0) in vec3 position;
layout(location=1) in vec2 uv;
layout(location=2) in vec3 normal;
#endif

void main() {
	#if VERTEX_PULLING
	// Compiler needs these
	BufferVertex b = vertices[indices[gl_VertexID]];
	BufferVertex b0 = vertices[indices[gl_VertexID / 3 * 3 + 0]];
	BufferVertex b1 = vertices[indices[gl_VertexID / 3 * 3 + 1]];
	BufferVertex b2 = vertices[indices[gl_VertexID / 3 * 3 + 2]];
	Vertex v = unfuck(b);
	Vertex v0 = unfuck(b0);
	Vertex v1 = unfuck(b1);
	Vertex v2 = unfuck(b2);

	v.position -= normalize(cross(v0.position - v1.position, v0.position - v2.position)) * normal_bias;

	vec3 position = v.position;
	vec2 uv = v.uv;
	vec3 normal = v.normal;
	#endif

	v_normal = normal;

	gl_Position = model_to_ndc * vec4(position, 1);
	
	depth = gl_Position.z * 0.5 + 0.5;

	gl_Position.xy = encode(gl_Position.xy);
}

#endif

#ifdef FRAGMENT_SHADER

out vec2 frag_color;

void main() {
	float slope = abs(dFdx(gl_FragCoord.z) + dFdy(gl_FragCoord.z));
	switch (algorithm) {
		default:
		case Algorithm_regular:
			frag_color = vec2(depth, slope);
			break;
		case Algorithm_variance:
			frag_color = vec2(depth, depth*depth);
			break;
	}
	gl_FragDepth = gl_FragCoord.z + slope * slope_scaled_bias + (1 - abs(dot(v_normal, light_direction))) * normal_bias + constant_bias;
}

#endif
