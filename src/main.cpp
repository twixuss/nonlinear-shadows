#define TL_IMPL
#include <tl/main.h>
#include <tl/common.h>
#include <tl/opengl.h>
#include <tl/win32.h>
#include <tl/win32_error.h>
#include <tl/gltf.h>
#include <tl/qoi.h>
#include <tl/includer.h>
#include <tl/block_list.h>
#include <tl/random.h>
#include <gl/GL.h>

#include <imgui.h>
#include <backends/imgui_impl_win32.h>
#include <backends/imgui_impl_opengl3.h>

ImVec2 operator-(ImVec2 a, ImVec2 b) { return {a.x - b.x, a.y - b.y}; }


using namespace tl;

HWND hwnd;
v2s screen_size;

LRESULT CALLBACK wnd_proc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp) {

	extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	if (ImGui_ImplWin32_WndProcHandler(hwnd, msg, wp, lp))
		return true;

	switch (msg) {
		case WM_CLOSE: {
			PostQuitMessage(0);
			return 0;
		}
		case WM_SIZE: {
			v2s new_size = {
				LOWORD(lp),
				HIWORD(lp),
			};

			if (!new_size.x || !new_size.y || (wp == SIZE_MINIMIZED))
				return 0;

			screen_size = new_size;
			return 0;
		}
	}
	return DefWindowProcW(hwnd, msg, wp, lp);
}

using String = Span<utf8>;

String program_path;
String program_directory;

String resource_path(String relative_path) {
	return tformat(u8"{}/../dat/{}"s, program_directory, relative_path);
};

bool create_window() {
	WNDCLASSEXW c {
		.cbSize = sizeof c,
		.lpfnWndProc = wnd_proc,
		.hInstance = GetModuleHandleW(0),
		.hCursor = LoadCursorW(0, IDC_ARROW),
		.lpszClassName = L"non-linear-shadow-mapping",
	};
	if (!RegisterClassExW(&c)) {
		current_logger.error("RegisterClassExW failed. {}", win32_error());
		return false;
	}
	hwnd = CreateWindowExW(0, c.lpszClassName, c.lpszClassName, WS_OVERLAPPEDWINDOW | WS_VISIBLE, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 0, 0, c.hInstance, 0);
	if (!hwnd || hwnd == INVALID_HANDLE_VALUE) {
		current_logger.error("CreateWindowExW failed. {}", win32_error());
		return false;
	}

	init_rawinput(RawInput_mouse);

	if (!gl::init_opengl((NativeWindowHandle)hwnd, gl::Init_debug)) {
		current_logger.error("gl::init_opengl failed. {}", win32_error());
		return false;
	}

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glDepthFunc(GL_LESS);

	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImGuiIO &io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;
	io.ConfigDragClickToInputText = true;
	
	auto &style = ImGui::GetStyle();
	style.HoverDelayShort = 2.0f;
	
	ImGui::StyleColorsDark();

	ImGui_ImplWin32_InitForOpenGL(hwnd);
	ImGui_ImplOpenGL3_Init();

	return true;
}

void bind_texture(GLuint program, u32 slot, char const *name, GLuint texture, GLuint sampler = 0, GLenum target = GL_TEXTURE_2D) {
	glActiveTexture(GL_TEXTURE0 + slot); 
	glBindTexture(target, texture); 
	gl::set_uniform(program, name, (int)slot); 
	glBindSampler(slot, sampler);
}


#define DEFINE_ENUM__N(name) name,

#define DEFINE_ENUM(enum_name, enumerate) \
	enum class enum_name {                \
		enumerate(DEFINE_ENUM__N)         \
		count,                            \
	};


#define DEFINE_PREVIEW_STRING__ADN(name, enum_type) case enum_type::name: dev_name = #name; break;

#define DEFINE_PREVIEW_STRING(enum_type, enumerate)                        \
	char const *preview_string(enum_type x) {                              \
		scoped(current_temporary_allocator);                               \
		char const *dev_name;                                              \
		switch (x) {                                                       \
			enumerate(DEFINE_PREVIEW_STRING__ADN, enum_type)               \
			default: return tformat("(" #enum_type "){}\0"s, (int)x).data; \
		}                                                                  \
		List<char> result;                                                 \
		for (char const *p = dev_name; *p; ++p) {                          \
			char c = *p;                                                   \
			switch (c) {                                                   \
				case '_': c = ' '; break;                                  \
			}                                                              \
			result.add(c);                                                 \
		}                                                                  \
		result[0] = to_upper(result[0]);                                   \
		result.add('\0');                                                  \
		return result.data;                                                \
	}


#define ENUM_TO_GLSL__N(name, enum_name, start) "#define " #enum_name "_" #name " (" STRINGIZE(__COUNTER__) "-" STRINGIZE(start) "-" STRINGIZE(1) ")\n"

#define ENUM_TO_GLSL(enum_name, enumerate) \
	enumerate(ENUM_TO_GLSL__N, enum_name, __COUNTER__)

#define DEFINE_ENUM_ALL(enum_type, enumerate) \
	DEFINE_ENUM(enum_type, enumerate) \
	DEFINE_PREVIEW_STRING(enum_type, enumerate) \



#define ENUMERATE_WARP_MODES(x, ...) \
	x(reciprocal_continuous       __VA_OPT__(,) __VA_ARGS__) \
	x(reciprocal_piecewise_linear __VA_OPT__(,) __VA_ARGS__) \

DEFINE_ENUM_ALL(WarpMode, ENUMERATE_WARP_MODES)

#define ENUMERATE_SAMPLING_MODES(x, ...) \
	x(hard         __VA_OPT__(,) __VA_ARGS__) \
	x(soft_uniform __VA_OPT__(,) __VA_ARGS__) \
	x(soft_dynamic __VA_OPT__(,) __VA_ARGS__) \

DEFINE_ENUM_ALL(SamplingMode, ENUMERATE_SAMPLING_MODES)

#define ENUMERATE_ALGORITHMS(x, ...) \
	x(regular  __VA_OPT__(,) __VA_ARGS__) \
	x(variance __VA_OPT__(,) __VA_ARGS__) \
	x(overkill __VA_OPT__(,) __VA_ARGS__) \

DEFINE_ENUM_ALL(Algorithm, ENUMERATE_ALGORITHMS)

#define ENUMERATE_TAP_PATTERN(x, ...) \
	x(circular  __VA_OPT__(,) __VA_ARGS__) \
	x(quadratic __VA_OPT__(,) __VA_ARGS__) \

DEFINE_ENUM_ALL(TapPattern, ENUMERATE_TAP_PATTERN)

#define ENUMERATE_VARIANCE_FILTER(x, ...) \
	x(none    __VA_OPT__(,) __VA_ARGS__) \
	x(gauss5  __VA_OPT__(,) __VA_ARGS__) \
	x(gauss11 __VA_OPT__(,) __VA_ARGS__) \

DEFINE_ENUM_ALL(VarianceFilter, ENUMERATE_VARIANCE_FILTER)

template <AnEnum Enum>
bool Combo(char const *name, Enum *e) {
	bool result = false;
	if (ImGui::BeginCombo(name, preview_string(*e))) {
		for (int i = 0; i < (int)Enum::count; ++i) {
			if (ImGui::Selectable(preview_string((Enum)i), *e == (Enum)i)) {
				*e = (Enum)i;
				result = true;
			}
		}
		ImGui::EndCombo();
	}
	return result;
}

bool SliderFloat2Sep(bool *separate, const char* label, float v[2], float v_min, float v_max, const char* format = "%.3f", ImGuiSliderFlags flags = 0) {
	bool result = false;
	if (ImGui::BeginTable(label, 2)) {
		defer { ImGui::EndTable(); };

		ImGui::TableSetupColumn(0, ImGuiTableColumnFlags_WidthStretch, 1);
		ImGui::TableSetupColumn(0, ImGuiTableColumnFlags_WidthFixed, ImGui::CalcTextSize(label).x + 30);
        ImGui::TableNextRow();
		ImGui::TableNextColumn();
		if (*separate) {
			result = ImGui::SliderFloat2(label, v, v_min, v_max, format, flags);
		} else {
			result = ImGui::SliderFloat(label, v, v_min, v_max, format, flags);
			v[1] = v[0];
		}
		ImGui::TableNextColumn();
		ImGui::Checkbox("Separate axes", separate);
	}
	return result;
};

void generate_circular_sampling_pattern(int radius, auto &&handle_point) {
	handle_point(v2f{});
	for (int j = 1; j <= radius; ++j) {
		for (int i = 0; i < j*6; ++i) {
			float a = i*pi/(j*3) + (5-j)*pi/24;
			handle_point(j * cos_sin(a));
		}
	}
}

struct LoadedMesh {
	GLuint vb, ib, va;
	u32 vertex_count, index_count;
};

LoadedMesh load_mesh(Scene3D::Node *node) {
	struct MyVertex {
		v3f position;
		v3f normal;
		v2f uv;
	};
	
	List<MyVertex> vertices;
	defer { free(vertices); };
	
	for (auto v : node->mesh->vertices) {
		vertices.add({
			.position = v.position,
			.normal = v.normal,
			.uv = v.uv,
		});
	}
	
	auto &indices = node->mesh->indices;

	LoadedMesh result = {};
	
	glCreateBuffers(1, &result.vb);
	glNamedBufferStorage(result.vb, sizeof(vertices[0]) * vertices.count, vertices.data, 0);

	glCreateBuffers(1, &result.ib);
	glNamedBufferStorage(result.ib, sizeof(indices[0]) * indices.count, indices.data, 0);

	glCreateVertexArrays(1, &result.va);
	glVertexArrayVertexBuffer(result.va, 0, result.vb, 0, sizeof(MyVertex));
	glVertexArrayElementBuffer(result.va, result.ib);
	glEnableVertexArrayAttrib(result.va, 0); glVertexArrayAttribBinding(result.va, 0, 0); glVertexArrayAttribFormat(result.va, 0, 3, GL_FLOAT, false, offsetof(MyVertex, position));
	glEnableVertexArrayAttrib(result.va, 1); glVertexArrayAttribBinding(result.va, 1, 0); glVertexArrayAttribFormat(result.va, 1, 3, GL_FLOAT, false, offsetof(MyVertex, normal));
	glEnableVertexArrayAttrib(result.va, 2); glVertexArrayAttribBinding(result.va, 2, 0); glVertexArrayAttribFormat(result.va, 2, 2, GL_FLOAT, false, offsetof(MyVertex, uv));

	result.vertex_count = vertices.count;
	result.index_count = indices.count;

	return result;
}

struct Entity {
	LoadedMesh mesh;
	v3f position;
	v3f euler_angles;
};

Entity create_entity(Scene3D::Node *node) {
	return {
		.mesh = load_mesh(node),
		.position = node->position,
		.euler_angles = {},
	};
}

s32 tl_main(Span<String> args) {
	//auto m = m3::rotation_r_zxy(V3f(pi/6));
	//println("mat3({}, {}, {}, {}, {}, {}, {}, {}, {})", m.s[0], m.s[1], m.s[2], m.s[3], m.s[4], m.s[5], m.s[6], m.s[7], m.s[8]);


	//List<v2s> o;
	//o.add({0, 0});
	//for (int i = 1; i <= 5; ++i) {
	//	for (int j = -i; j <= i; ++j) {
	//		o.add({+i,j});
	//		o.add({-i,j});
	//	}
	//	for (int j = -i+1; j <= i-1; ++j) {
	//		o.add({j,+i});
	//		o.add({j,-i});
	//	}
	//}
	//for (auto p : o) {
	//	println("vec2({},{}),", p.x, p.y);
	//}


	program_path = args[0];
	replace_inplace(program_path, u8'\\', u8'/');
	program_directory = parse_path(program_path).directory;

	if (!create_window()) {
		return 1;
	}

	// 
	// Load scene
	//

	auto scene = glb::parse_from_file(resource_path(u8"scene.glb"s)).value().scene;

	auto environment_entity = create_entity(scene.get_node(u8"environment"s));
	auto object_entity = create_entity(scene.get_node(u8"object"s));

	Entity *all_entities[] = { &environment_entity, &object_entity };

	auto qoi = read_entire_file(resource_path(u8"scene_ao.qoi"s));
	auto image = qoi::decode(qoi).value();

	GLuint ao_texture;
	glGenTextures(1, &ao_texture);
	glBindTexture(GL_TEXTURE_2D, ao_texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, image.size.x, image.size.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, image.pixels);
	glGenerateMipmap(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);

	//
	// Back buffer setup
	//
	
	GLuint backbuffer_color_texture;
	glGenTextures(1, &backbuffer_color_texture);
	glBindTexture(GL_TEXTURE_2D, backbuffer_color_texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	GLuint backbuffer_depth_texture;
	glGenTextures(1, &backbuffer_depth_texture);
	glBindTexture(GL_TEXTURE_2D, backbuffer_depth_texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	GLuint back_framebuffer;
	glGenFramebuffers(1, &back_framebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, back_framebuffer);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, backbuffer_color_texture, 0);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, backbuffer_depth_texture, 0);
	glReadBuffer(GL_NONE);
	

	//
	// Light setup
	//

	int shadow_resolution = 4096;
	
	struct Texture {
		GLuint texture;
		v2u resolution;
		GLint internal_format;
		GLenum format;
		bool use_mipmaps;
	};

	auto create_texture = [&](GLint min_filter = GL_LINEAR_MIPMAP_LINEAR, GLint mag_filter = GL_LINEAR, GLint wrap = GL_CLAMP) {
		GLuint texture;
		glGenTextures(1, &texture);
		glBindTexture(GL_TEXTURE_2D, texture);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, min_filter);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mag_filter);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrap);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrap);
		glBindTexture(GL_TEXTURE_2D, 0);
		return texture;
	};

	GLuint shadow_color_texture_rg32f;
	glGenTextures(1, &shadow_color_texture_rg32f);
	glBindTexture(GL_TEXTURE_2D, shadow_color_texture_rg32f);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	GLuint shadow_color_texture2_rg32f;
	glGenTextures(1, &shadow_color_texture2_rg32f);
	glBindTexture(GL_TEXTURE_2D, shadow_color_texture2_rg32f);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	GLuint shadow_depth_texture;
	glGenTextures(1, &shadow_depth_texture);
	glBindTexture(GL_TEXTURE_2D, shadow_depth_texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	GLuint shadow_depth_texture_array[19];
	glGenTextures(count_of(shadow_depth_texture_array), shadow_depth_texture_array);
	for (auto tex : shadow_depth_texture_array) {
		glBindTexture(GL_TEXTURE_2D, tex);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
		glBindTexture(GL_TEXTURE_2D, 0);
	}

	auto update_shadow_resolution = [&] {
		glBindTexture(GL_TEXTURE_2D, shadow_color_texture_rg32f);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RG32F, shadow_resolution, shadow_resolution, 0, GL_RG, GL_FLOAT, NULL);
		glGenerateMipmap(GL_TEXTURE_2D);
		glBindTexture(GL_TEXTURE_2D, 0);
		
		glBindTexture(GL_TEXTURE_2D, shadow_color_texture2_rg32f);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RG32F, shadow_resolution, shadow_resolution, 0, GL_RG, GL_FLOAT, NULL);
		glGenerateMipmap(GL_TEXTURE_2D);
		glBindTexture(GL_TEXTURE_2D, 0);
		
		glBindTexture(GL_TEXTURE_2D, shadow_depth_texture);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, shadow_resolution, shadow_resolution, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
		glBindTexture(GL_TEXTURE_2D, 0);
		
		for (auto tex : shadow_depth_texture_array) {
			glBindTexture(GL_TEXTURE_2D, tex);
			glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, shadow_resolution, shadow_resolution, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
			glBindTexture(GL_TEXTURE_2D, 0);
		}
	};

	update_shadow_resolution();

	GLuint depth_regular_sampler;
	glGenSamplers(1, &depth_regular_sampler);
	glSamplerParameteri(depth_regular_sampler, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glSamplerParameteri(depth_regular_sampler, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glSamplerParameteri(depth_regular_sampler, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glSamplerParameteri(depth_regular_sampler, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glSamplerParameterfv(depth_regular_sampler, GL_TEXTURE_BORDER_COLOR, v4f{1,1,1,1}.s);
	
	GLuint depth_comparison_sampler;
	glGenSamplers(1, &depth_comparison_sampler);
	glSamplerParameteri(depth_comparison_sampler, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glSamplerParameteri(depth_comparison_sampler, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glSamplerParameteri(depth_comparison_sampler, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glSamplerParameteri(depth_comparison_sampler, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glSamplerParameterfv(depth_comparison_sampler, GL_TEXTURE_BORDER_COLOR, v4f{1,1,1,1}.s);
	glSamplerParameteri(depth_comparison_sampler, GL_TEXTURE_COMPARE_FUNC, GL_LESS);
	glSamplerParameteri(depth_comparison_sampler, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
	
	// Build the framebuffer.
	GLuint shadow_map_framebuffer;
	glGenFramebuffers(1, &shadow_map_framebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, shadow_map_framebuffer);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, shadow_color_texture_rg32f, 0);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, shadow_depth_texture, 0);
	//glDrawBuffer(GL_NONE);
	glReadBuffer(GL_NONE);

	GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (status != GL_FRAMEBUFFER_COMPLETE) {
		current_logger.error("Failed to create light framebuffer");
		return 1;
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	
	GLuint reciprocal_piecewise_linear_texture;
	{
		f32 reciprocal_piecewise_linear_pixels[4][17] = {
			0/16., 1/16., 2/16., 3/16.,  4/16.,  5/16.,  6/16.,    7/16.,  8/16.,    9/16., 10/16.,   11/16., 12/16.,    13/16.,   14/16.,    15/16., 16/16., 
			0/16., 2/16., 4/16., 6/16.,  8/16.,  9/16., 10/16.,   11/16., 12/16., 12.5/16., 13/16., 13.5/16., 14/16.,  14.5/16.,   15/16.,  15.5/16., 16/16., 
			0/16., 3/16., 6/16., 8/16., 9/16., 10/16., 11/16.,   12/16., 13/16., 13.5/16., 14/16., 14.333/16., 14.666/16.,  15/16.,   15.333/16.,  15.666/16., 16/16., 
			0/16., 4/16., 8/16., 10/16., 12/16., 12.5/16., 13/16., 13.5/16., 14/16., 14.25/16., 14.5/16., 14.75/16., 15/16., 15.25/16., 15.5/16., 15.75/16., 16/16., 
		};
		glGenTextures(1, &reciprocal_piecewise_linear_texture);
		glBindTexture(GL_TEXTURE_2D, reciprocal_piecewise_linear_texture);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, 17, 4, 0, GL_RED, GL_FLOAT, reciprocal_piecewise_linear_pixels);
		glBindTexture(GL_TEXTURE_2D, 0);
	}
	
	GLuint random_texture;
	{
		u32 const size = 16;
		f32 random_pixels[size*size*size];

		xorshift32 gen = {3037000507};

		for (u32 i = 0; i < count_of(random_pixels); ++i) {
			random_pixels[i] = next_f32(gen);
		}

		glGenTextures(1, &random_texture);
		glBindTexture(GL_TEXTURE_3D, random_texture);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_REPEAT);
		glTexImage3D(GL_TEXTURE_3D, 0, GL_R32F, size, size, size, 0, GL_RED, GL_FLOAT, random_pixels);
		glBindTexture(GL_TEXTURE_3D, 0);
	}

	// 
	// Shaders
	//

	struct Program {
		struct Dependency : includer::Dependency {
			FileTime last_write_time = 0;
			
			void init() {
				last_write_time = get_file_write_time(path).value_or(0);;
			}
			bool was_modified() {
				return last_write_time < get_file_write_time(path).value_or(0);
			}
		};

		String path;

		GLenum vs = 0;
		GLenum fs = 0;
		GLuint program = 0;
		includer::PreprocessResult<Dependency> source = {
			.append_debug_info = [](StringBuilder &builder, String path, u32 line) {
				append_format(builder, "\n#line {} \"{}\"\n", line, path);
			}
		};

		void load() {
			println("Recompiling {}", path);

			source.load(path);

			List<utf8> preprocessed;
			defer { 
				tl::free(preprocessed);
			};

			auto cursor = source.text;

			while (cursor.count) {
				constexpr String macros[][2] = {
					{ u8"ENUM_TO_GLSL(WarpMode, ENUMERATE_WARP_MODES)"s, u8"" ENUM_TO_GLSL(WarpMode, ENUMERATE_WARP_MODES) ""s },
					{ u8"ENUM_TO_GLSL(SamplingMode, ENUMERATE_SAMPLING_MODES)"s, u8"" ENUM_TO_GLSL(SamplingMode, ENUMERATE_SAMPLING_MODES) ""s },
					{ u8"ENUM_TO_GLSL(Algorithm, ENUMERATE_ALGORITHMS)"s, u8"" ENUM_TO_GLSL(Algorithm, ENUMERATE_ALGORITHMS) ""s },
					{ u8"ENUM_TO_GLSL(TapPattern, ENUMERATE_TAP_PATTERN)"s, u8"" ENUM_TO_GLSL(TapPattern, ENUMERATE_TAP_PATTERN) ""s },

				};

				for (auto &macro : macros) {
					if (starts_with(cursor, macro[0])) {
						preprocessed.add(macro[1]);
						cursor.set_begin(cursor.begin() + macro[0].count);
						goto replaced_macro;
					}
				}

				preprocessed.add(cursor[0]);
				cursor.set_begin(cursor.begin() + 1);

			replaced_macro:;
			}

			vs = gl::create_shader(GL_VERTEX_SHADER, 430, true, as_chars(preprocessed));
			fs = gl::create_shader(GL_FRAGMENT_SHADER, 430, true, as_chars(preprocessed));
			program = gl::create_program({.vertex = vs, .fragment = fs});

			if (!vs || !fs || !program) {
				//println("Preprocessed shader code:\n{}", preprocessed);
			}
		}

		void unload() {
			glDeleteProgram(program);
			glDeleteShader(fs);
			glDeleteShader(vs);
		}
		void free() {
			source.free();
		}
		bool needs_reload() {
			for (auto &dependency : source.dependencies) {
				if (dependency.was_modified()) {
					return true;
				}
			}
			return false;
		}
	};

	Program surface_program = {.path = to_list(resource_path(u8"shaders/surface.glsl"s))};
	Program shadow_program = {.path = to_list(resource_path(u8"shaders/shadow.glsl"s))};
	Program gauss5_program = {.path = to_list(resource_path(u8"shaders/filter_gauss1x5.glsl"s))};
	Program gauss11_program = {.path = to_list(resource_path(u8"shaders/filter_gauss1x11.glsl"s))};
	Program depth_prepass_program = {.path = to_list(resource_path(u8"shaders/depth_prepass.glsl"s))};
	Program depth_downsample_min_program = {.path = to_list(resource_path(u8"shaders/depth_downsample_min.glsl"s))};
	Program postprocess_program = {.path = to_list(resource_path(u8"shaders/postprocess.glsl"s))};

	Program *all_programs[] = { &surface_program, &shadow_program, &gauss5_program, &gauss11_program, &depth_prepass_program, &depth_downsample_min_program, &postprocess_program };

	for (auto program : all_programs) {
		program->load();
	}
	
	//
	// State
	//
	
	v3f camera_position = V3f(4.251, 7.537, 11.446);
	v3f camera_angles = V3f(0.297, 5.808, 0);
	v2f light_angles = {-0.7, 4};
	f32 warping_factor_setting = 4;
	s32 pcf_tap_count = 1 + 6 + 12 + 18 + 24 + 30;
	f32 shadow_constant_bias = 0;
	f32 shadow_slope_scaled_bias = 0;
	f32 shadow_normal_bias = 0;
	f32 shadow_softness = 8;
	v2f shadow_sample_density = V2f(0.2);
	f32 variance_mip_level = 0;
	f32 variance_light_bleeding_reduction = 0.25f;
	f32 variance_min_limit = 1e-6f;
	bool variance_use_gauss_filter = true;
	bool variance_variable_penumbra = true;
	bool do_texel_snap = true;
	f32 sublight_angle = 0.1f;
	
	WarpMode warp_mode = WarpMode::reciprocal_continuous;
	SamplingMode sampling_mode = SamplingMode::soft_dynamic;
	Algorithm algorithm = Algorithm::regular;
	TapPattern tap_pattern = TapPattern::circular;
	VarianceFilter variance_filter = VarianceFilter::gauss5;

	
	#define CULL_NONE 0
	#define CULL_FRONT 1
	#define CULL_BACK 2

	int cull_mode = CULL_FRONT;

	//
	// Stuff
	//

	f32 frame_time = 1.0f / 60;
	f32 time = 0;
	PreciseTimer frame_timer = create_precise_timer();
	v2s old_screen_size = {-1,-1};

	while (1) {
		MSG msg;
		while (PeekMessageW(&msg, 0, 0, 0, PM_REMOVE)) {
			switch (msg.message) {
				case WM_QUIT: {
					return 0;
				}
			}

			TranslateMessage(&msg);
			DispatchMessageW(&msg);
		}
		
		if (any(screen_size != old_screen_size)) {
			old_screen_size = screen_size;
			glBindTexture(GL_TEXTURE_2D, backbuffer_color_texture);
			glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, screen_size.x, screen_size.y, 0, GL_RGBA, GL_FLOAT, NULL);
			glBindTexture(GL_TEXTURE_2D, backbuffer_depth_texture);
			glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, screen_size.x, screen_size.y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
			glBindTexture(GL_TEXTURE_2D, 0);
		}

		for (auto program : all_programs) {
			if (program->needs_reload()) {
				program->unload();
				program->load();
			}
		}

		// 
		// Begin GUI frame
		// 
		ImGui_ImplOpenGL3_NewFrame();
		ImGui_ImplWin32_NewFrame();
		ImGui::NewFrame();
		
		static f32 shadow_map_world_width = 256;
		
		// 
		// GUI
		//
		
		ImGui::Begin("Menu");
		
		if (ImGui::CollapsingHeader("Light", ImGuiTreeNodeFlags_DefaultOpen)) {
			ImGui::DragFloat2("Light angles", light_angles.s, 0.01f);
		}

		static v3f object_angular_velocity = V3f(0.125f);
		if (ImGui::CollapsingHeader("Object")) {
			ImGui::DragFloat3("Position", object_entity.position.s, 0.01f);
			ImGui::DragFloat3("Euler angles", object_entity.euler_angles.s, 0.01f);
			ImGui::DragFloat3("Angular Velocity", object_angular_velocity.s, 0.01f);
		}
		
		if (ImGui::CollapsingHeader("Shadow map settings", ImGuiTreeNodeFlags_DefaultOpen)) {
			Combo("Algorithm", &algorithm);
			if (ImGui::SliderInt("Resolution", &shadow_resolution, 256, 4096, "%d texels", ImGuiSliderFlags_Logarithmic)) {
				update_shadow_resolution();
			}
			ImGui::SliderFloat("World width", &shadow_map_world_width, 16, 512, "%.0f meters");
			ImGui::SliderFloat("Constant bias", &shadow_constant_bias, 0, 1e-1, "%.6f", ImGuiSliderFlags_Logarithmic);
			ImGui::SliderFloat("Slope scaled bias", &shadow_slope_scaled_bias, 0, 1e+2, "%.6f", ImGuiSliderFlags_Logarithmic);
			ImGui::SliderFloat("Normal bias", &shadow_normal_bias, 0, 1e-1, "%.6f", ImGuiSliderFlags_Logarithmic);
			ImGui::Checkbox("Snap to texel", &do_texel_snap);

			switch (algorithm) {
				case Algorithm::regular:
				case Algorithm::overkill: {
					char const *cull_mode_labels[] {
						"None",
						"Front",
						"Back",
					};
					if (ImGui::BeginCombo("Cull mode", cull_mode_labels[cull_mode])) {
						for (int i = 0; i < 3; ++i) {
							if (ImGui::Selectable(cull_mode_labels[i], cull_mode == i)) {
								cull_mode = i;
							}
						}
						ImGui::EndCombo();
					}
					break;
				}
			}
			switch (algorithm) {
				case Algorithm::overkill: {
					ImGui::SliderFloat("Angle between sublights", &sublight_angle, 0.001f, pi/4, "%.3f radians");
					break;
				}
			}
		}
		
		switch (algorithm) {
			case Algorithm::regular: 
			case Algorithm::overkill: {
				if (ImGui::CollapsingHeader("Warping", ImGuiTreeNodeFlags_DefaultOpen)) {
					Combo("Warp mode", &warp_mode);
					ImGui::SliderFloat("Factor", &warping_factor_setting, 1, 16);
					switch (warp_mode) {
						case WarpMode::reciprocal_continuous:
							ImGui::Text("+ Smooth transition between high and low quality shadows.\n- Pixel shimmer when moving camera.");
							break;
						case WarpMode::reciprocal_piecewise_linear:
							ImGui::TextWrapped("Only one variant implemented for factor > 1.5. Consists of 3 lines with slopes 2, 1 and 0.5\n+ No pixel shimmer when moving camera.\n- Noticeable transitions.");
							break;
					}
				}
			}
		}

		auto TapPatternGui = [&] (char const *label) {
			Combo("Tap pattern", &tap_pattern);
			switch (tap_pattern) {
				case TapPattern::circular: {
					ImGui::SliderInt(label, &pcf_tap_count, 1, 91);

					// pcf_tap_count should be a (triangular number * 6 + 1)
					// this gets an index of it, rounds it and converts it back

					s32 t = (pcf_tap_count - 1) / 6;
					s32 n = round_to_int((sqrtf(8 * t + 1) - 1) * 0.5f);
					pcf_tap_count = clamp(n * (n + 1) * 3 + 1, 1, 91);
					break;
				}
				case TapPattern::quadratic: {
					ImGui::SliderInt(label, &pcf_tap_count, 1, 81);

					s32 n = round_to_int(sqrtf(pcf_tap_count - 1) / 2);
					pcf_tap_count = clamp(pow2(n * 2 + 1), 1, 81);
					break;
				}
			}
		};

		if (ImGui::CollapsingHeader("Sampling", ImGuiTreeNodeFlags_DefaultOpen)) {
			switch (algorithm) {
				case Algorithm::regular:
				case Algorithm::overkill: {
					Combo("Sampling mode", &sampling_mode);
					switch (sampling_mode) {
						case SamplingMode::soft_uniform: {
							TapPatternGui("PCF taps");
							static bool sepax = false;
							SliderFloat2Sep(&sepax, "Sample density", shadow_sample_density.s, 0.01, 1, "%.3f", ImGuiSliderFlags_Logarithmic);
							break;
						}
						case SamplingMode::soft_dynamic: {
							TapPatternGui("Blocker and PCF taps");
							static bool sepax = false;
							SliderFloat2Sep(&sepax, "Sample density", shadow_sample_density.s, 0.01, 1, "%.3f", ImGuiSliderFlags_Logarithmic);
							ImGui::SliderFloat("Shadow softness", &shadow_softness, 0, 100);
							break;
						}
					}
					break;
				}
				case Algorithm::variance: {
					ImGui::SliderFloat("Light bleeding reduction", &variance_light_bleeding_reduction, 0, 0.5f, "%.3f");
					ImGui::SliderFloat("Minimal variance", &variance_min_limit, 0, 0.1f, "%.6f", ImGuiSliderFlags_Logarithmic);
					Combo("Filter", &variance_filter);
					ImGui::Checkbox("Variable penumbra", &variance_variable_penumbra);
					if (variance_variable_penumbra) {
						TapPatternGui("Blocker taps");
						static bool sepax = false;
						SliderFloat2Sep(&sepax, "Sample density", shadow_sample_density.s, 0.01, 1, "%.3f", ImGuiSliderFlags_Logarithmic);
						ImGui::DragFloat("Shadow softness", &shadow_softness, 1, 0, 100);
					} else {
						ImGui::SliderFloat("Mip level", &variance_mip_level, 0, 5, "%.3f");
					}
					
					break;
				}
			}
		}
		ImGui::TextWrapped(R"(Warping allows for increased shadow resolution close to the camera while rendering the scene just once.
The main issue is need for dense enough meshes to minimize shadow bending.
Non-front face culling produces unusable results.
Also there is no way to avoid pixel jitter when camera moves.

Notes:
1) Because warping is done for each axis independently, it is unnoticeable when a shadow casting edge is axis aligned in light's view. Would be perfect for minecraft :)
2) (PCSS) Least artifacts are produced with front face culling)");
		ImGui::End();
		
		auto target_shadow_color_texture = shadow_color_texture_rg32f;

		ImGui::SetNextWindowSize({256, 256}, ImGuiCond_FirstUseEver);
		ImGui::Begin("Shadow color preview", 0, ImGuiWindowFlags_NoScrollbar);
		ImGui::Image((ImTextureID)target_shadow_color_texture, ImGui::GetWindowContentRegionMax() - ImGui::GetWindowContentRegionMin(), ImVec2{0, 1}, ImVec2{1, 0});
		ImGui::End();

		ImGui::SetNextWindowSize({256, 256}, ImGuiCond_FirstUseEver);
		ImGui::Begin("Shadow depth preview", 0, ImGuiWindowFlags_NoScrollbar);
		ImGui::Image((ImTextureID)shadow_depth_texture, ImGui::GetWindowContentRegionMax() - ImGui::GetWindowContentRegionMin(), ImVec2{0, 1}, ImVec2{1, 0});
		ImGui::End();

		ImGui::Render();
		
		// 
		// Update state
		//

		static ImVec2 prev_mouse_position;
		ImVec2 mouse_position = ImGui::GetMousePos();
		ImVec2 mouse_delta = {mouse_position.x - prev_mouse_position.x, mouse_position.y - prev_mouse_position.y};
		prev_mouse_position = mouse_position;

		if (ImGui::IsMouseDragging(ImGuiMouseButton_Right) && !ImGui::GetIO().WantCaptureMouse) {
			camera_angles.x += mouse_delta.y * 0.003f;
			camera_angles.y += mouse_delta.x * 0.003f;
		}

		f32 speed = 5;
		if (ImGui::IsKeyDown(ImGuiKey_LeftShift)) speed *= 10;
		if (ImGui::IsKeyDown(ImGuiKey_LeftAlt)) speed /= 10;

		camera_position += m3::rotation_r_zxy(-camera_angles) * (frame_time * speed * v3f {
			(f32)(ImGui::IsKeyDown(ImGuiKey_D) - ImGui::IsKeyDown(ImGuiKey_A)),
			(f32)(ImGui::IsKeyDown(ImGuiKey_E) - ImGui::IsKeyDown(ImGuiKey_Q)),
			(f32)(ImGui::IsKeyDown(ImGuiKey_S) - ImGui::IsKeyDown(ImGuiKey_W)),
		});
	
		v3f light_direction = m3::rotation_r_zxy(light_angles.x, light_angles.y, 0) * v3f{0,0,1};
		
		object_entity.euler_angles += V3f(frame_time * object_angular_velocity);

		//
		// Shadow render
		//
		
		f32 warping_factor = warping_factor_setting;

		switch (algorithm) {
			case Algorithm::variance: {
				warping_factor = 1;
				break;
			}
		}

		struct Light {
			GLuint depth_texture;
			v3f direction;
			v3f color;

			m4 world_to_light;
		};

		StaticList<Light, 19> lights;

		auto render_shadow_map = [&] (Light &light, int direction_offset_index = 0) {
			
			gl::clear_color(v4f{1,1,1,1});
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		
			glEnable(GL_DEPTH_TEST);
			glDepthFunc(GL_LESS);

			switch (algorithm) {
				case Algorithm::regular:
				case Algorithm::overkill: {
					switch (cull_mode) {
						case CULL_NONE:
							glDisable(GL_CULL_FACE);
							break;
						case CULL_FRONT:
							glEnable(GL_CULL_FACE);
							glCullFace(GL_FRONT);
							break;
						case CULL_BACK:
							glEnable(GL_CULL_FACE);
							glCullFace(GL_BACK);
							break;
					}
					break;
				}
				case Algorithm::variance: {
					glDisable(GL_CULL_FACE);
					break;
				}
			}

			glViewport(0, 0, shadow_resolution, shadow_resolution);

			v3f light_position = camera_position;
		
		recalc_basis_vectors:
			v3f z = normalize(-light.direction);
			v3f x = normalize(cross(z, z.y > 0.99f ? v3f{1,0,0} : v3f{0,1,0}));
			v3f y = normalize(-cross(z, x));

			

			if (direction_offset_index != 0) {
				if (count_of(shadow_depth_texture_array) == 7) {
					v3f d = rotate_around(light.direction, x, sublight_angle);
					light.direction = rotate_around(d, light.direction, (direction_offset_index-1)*(tau/6));
				} else {
					if (direction_offset_index < 7) {
						v3f d = rotate_around(light.direction, x, sublight_angle);
						light.direction = rotate_around(d, light.direction, (direction_offset_index-1)*(tau/6));
					} else if (direction_offset_index < 19) {
						v3f d = rotate_around(light.direction, x, sublight_angle * 2);
						light.direction = rotate_around(d, light.direction, (direction_offset_index-7)*(tau/12));
					}
				}
				direction_offset_index = 0;
				goto recalc_basis_vectors;
			}

			m3 light_rotation_matrix = transpose(m3{x,y,z});

			if (do_texel_snap) {
				f32 snap = 0;

				switch (algorithm) {
					case Algorithm::regular: 
					case Algorithm::overkill: {
						switch (warp_mode) {
							case WarpMode::reciprocal_continuous:
								if (warping_factor == 1) {
									snap = warping_factor;
								}
								break;
							case WarpMode::reciprocal_piecewise_linear:
								if (warping_factor <= 1.5f) snap = 1;
								snap = 0.5;
								break;
						}
						break;
					}
					case Algorithm::variance: {
						int snap_mip_level = 2;
						snap = 1.0f / (1 << snap_mip_level);
						break;
					}
				}

				if (snap > 0) {
					snap *= shadow_resolution / shadow_map_world_width;
					light_position = light_rotation_matrix * light_position;
					light_position = round(light_position * snap) / snap;
					light_position = inverse(light_rotation_matrix) * light_position;
				}
			}

			light.world_to_light = m4::ortho_right_handed(shadow_map_world_width, 1, 0, shadow_map_world_width * 0.5f) * to_m4(light_rotation_matrix) * m4::translation(-light_position);
		
			glUseProgram(shadow_program.program);
			gl::set_uniform(shadow_program.program, "warping_factor", warping_factor);
			gl::set_uniform(shadow_program.program, "warp_mode", (int)warp_mode);
			gl::set_uniform(shadow_program.program, "algorithm", (int)algorithm);
			gl::set_uniform(shadow_program.program, "constant_bias", shadow_constant_bias);
			gl::set_uniform(shadow_program.program, "slope_scaled_bias", shadow_slope_scaled_bias);
			gl::set_uniform(shadow_program.program, "normal_bias", shadow_normal_bias);
			gl::set_uniform(shadow_program.program, "light_direction", light.direction);
		
			for (auto entity : all_entities) {
				m4 model_to_world = m4::translation(entity->position) * m4::rotation_r_zxy(entity->euler_angles);
				m4 model_to_ndc = light.world_to_light * model_to_world;

				gl::set_uniform(shadow_program.program, "model_to_world", model_to_world);
				gl::set_uniform(shadow_program.program, "model_to_ndc", model_to_ndc);

				glBindVertexArray(entity->mesh.va);
				glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, entity->mesh.vb);
				glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, entity->mesh.ib);
				//glDrawArrays(GL_TRIANGLES, 0, entity->mesh.index_count);
				glDrawElements(GL_TRIANGLES, entity->mesh.index_count, GL_UNSIGNED_INT, 0);
			}
		};


		glBindFramebuffer(GL_FRAMEBUFFER, shadow_map_framebuffer);

		switch (algorithm) {
			case Algorithm::regular: {
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, shadow_depth_texture, 0);
				Light &light = lights.add();
				light.depth_texture = shadow_depth_texture;
				light.direction = light_direction;
				light.color = {1,1,1};
				render_shadow_map(light);
				break;
			}
			case Algorithm::variance: {
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, target_shadow_color_texture, 0);
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, shadow_depth_texture, 0);
				Light &light = lights.add();
				light.depth_texture = shadow_depth_texture;
				light.direction = light_direction;
				light.color = {1,1,1};
				render_shadow_map(light);
				break;
			}
			case Algorithm::overkill: {
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);

				for (int i = 0; i < count_of(shadow_depth_texture_array); ++i) {
					glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, shadow_depth_texture_array[i], 0);
					Light &light = lights.add();
					light.depth_texture = shadow_depth_texture_array[i];
					light.direction = light_direction;
					light.color = v3f{1,1,1} / count_of(shadow_depth_texture_array);
					render_shadow_map(light, i);
				}
				break;
			}
		}

	
		glBindTexture(GL_TEXTURE_2D, target_shadow_color_texture);
		glGenerateMipmap(GL_TEXTURE_2D);
		glBindTexture(GL_TEXTURE_2D, 0);

		switch (algorithm) {
			case Algorithm::variance: {
				if (variance_filter != VarianceFilter::none) {
					glDisable(GL_DEPTH_TEST);

					auto program = variance_filter == VarianceFilter::gauss5 ? &gauss5_program : &gauss11_program;

					glUseProgram(program->program);

					u32 mip_resolution = shadow_resolution;
					s32 mip_level = 0;

					while (mip_resolution) {
						v2f input_texture_inv_resolution = V2f(1) / mip_resolution;
		
						glViewport(0, 0, mip_resolution, mip_resolution);

						gl::set_uniform(program->program, "input_texture_inv_resolution", input_texture_inv_resolution);
						gl::set_uniform(program->program, "sample_lod", (f32)mip_level);

						glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, shadow_color_texture2_rg32f, mip_level);
						bind_texture(program->program, 0, "input_texture", shadow_color_texture_rg32f);
						gl::set_uniform(program->program, "sample_offset_factor", v2f{1, 0});
						glDrawArrays(GL_TRIANGLES, 0, 6);
					
						glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, shadow_color_texture_rg32f, mip_level);
						bind_texture(program->program, 0, "input_texture", shadow_color_texture2_rg32f);
						gl::set_uniform(program->program, "sample_offset_factor", v2f{0, 1});
						glDrawArrays(GL_TRIANGLES, 0, 6);

						++mip_level;
						mip_resolution /= 2;
					}
				}
				break;
			}
		}

		// 
		// Main render
		//

		glBindFramebuffer(GL_FRAMEBUFFER, back_framebuffer);
		glClearColor(.3, .6, .9, 1);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glEnable(GL_DEPTH_TEST);
		glDepthFunc(GL_LESS);

		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);

		glViewport(0, 0, screen_size.x, screen_size.y);

		m4 world_to_ndc = m4::perspective_right_handed((f32)screen_size.x / screen_size.y, pi/2, 0.01f, 1000.0f) * m4::rotation_r_yxz(camera_angles) * m4::translation(-camera_position);

		// Z prepass
		glUseProgram(depth_prepass_program.program);
		for (auto entity : all_entities) {
			m4 model_to_world = m4::translation(entity->position) * m4::rotation_r_zxy(entity->euler_angles);
			m4 model_to_ndc = world_to_ndc * model_to_world;

			gl::set_uniform(depth_prepass_program.program, "model_to_world", model_to_world);
			gl::set_uniform(depth_prepass_program.program, "model_to_ndc", model_to_ndc);

			glBindVertexArray(entity->mesh.va);
			glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, entity->mesh.vb);
			glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, entity->mesh.ib);
			glDrawElements(GL_TRIANGLES, entity->mesh.index_count, GL_UNSIGNED_INT, 0);
		}

		// Surface pass
		glDepthFunc(GL_EQUAL);

		glUseProgram(surface_program.program);
		gl::set_uniform(surface_program.program, "warping_factor", warping_factor);
		gl::set_uniform(surface_program.program, "shadow_map_world_width", shadow_map_world_width);
		gl::set_uniform(surface_program.program, "shadow_resolution", (f32)shadow_resolution);
		gl::set_uniform(surface_program.program, "pcf_tap_count", pcf_tap_count);
		gl::set_uniform(surface_program.program, "warp_mode", (int)warp_mode);
		gl::set_uniform(surface_program.program, "shadow_softness", shadow_softness);
		gl::set_uniform(surface_program.program, "shadow_sample_density", shadow_sample_density);
		gl::set_uniform(surface_program.program, "sampling_mode", (int)sampling_mode);
		gl::set_uniform(surface_program.program, "algorithm", (int)algorithm);
		gl::set_uniform(surface_program.program, "tap_pattern", (int)tap_pattern);
		gl::set_uniform(surface_program.program, "variance_mip_level", variance_mip_level);
		gl::set_uniform(surface_program.program, "variance_light_bleeding_reduction", variance_light_bleeding_reduction);
		gl::set_uniform(surface_program.program, "variance_variable_penumbra", variance_variable_penumbra);
		gl::set_uniform(surface_program.program, "variance_min_limit", variance_min_limit);

		bind_texture(surface_program.program, 0, "shadow_map_color", target_shadow_color_texture);
		bind_texture(surface_program.program, 3, "ao_map", ao_texture);
		bind_texture(surface_program.program, 4, "random_texture", random_texture, 0, GL_TEXTURE_3D);
		
		auto draw_all_entities = [&] {
			for (auto entity : all_entities) {
				m4 model_to_world = m4::translation(entity->position) * m4::rotation_r_zxy(entity->euler_angles);
				m4 model_to_ndc = world_to_ndc * model_to_world;

				gl::set_uniform(surface_program.program, "model_to_world", model_to_world);
				gl::set_uniform(surface_program.program, "model_to_ndc", model_to_ndc);

				glBindVertexArray(entity->mesh.va);
				glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, entity->mesh.vb);
				glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, entity->mesh.ib);
				glDrawElements(GL_TRIANGLES, entity->mesh.index_count, GL_UNSIGNED_INT, 0);
			}
		};


		glDisable(GL_BLEND);
		gl::set_uniform(surface_program.program, "only_ambient", 1);
		draw_all_entities();

		glEnable(GL_BLEND);
		glBlendEquation(GL_FUNC_ADD);
		glBlendFunc(GL_ONE, GL_ONE);
		gl::set_uniform(surface_program.program, "only_ambient", 0);
		for (auto light : lights) {
			bind_texture(surface_program.program, 1, "shadow_map_depth", light.depth_texture, depth_regular_sampler);
			bind_texture(surface_program.program, 2, "shadow_map_depth_cmp", light.depth_texture, depth_comparison_sampler);
			gl::set_uniform(surface_program.program, "light_color", light.color);
			gl::set_uniform(surface_program.program, "world_to_light", light.world_to_light);
			gl::set_uniform(surface_program.program, "light_direction", light.direction);
			draw_all_entities();
		}
		
		// 
		// Blit to window
		//

		glBindFramebuffer(GL_FRAMEBUFFER, 0);
		glDisable(GL_DEPTH_TEST);
		glDisable(GL_BLEND);
		glDisable(GL_CULL_FACE);

		glUseProgram(postprocess_program.program);
		bind_texture(postprocess_program.program, 0, "input_texture", backbuffer_color_texture);
		glDrawArrays(GL_TRIANGLES, 0, 6);

		ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

		//
		// Finish frame
		//

		gl::present();

		// println("v3f camera_position = V3f{};\nv3f camera_angles = V3f{};", camera_position, camera_angles);

		frame_time = reset(frame_timer);
		time += frame_time;

		current_temporary_allocator.clear();
	}

	return 0;
}

