#define TL_IMPL
#include <tl/main.h>
#include <tl/common.h>
#include <tl/opengl.h>
#include <tl/win32.h>
#include <tl/win32_error.h>
#include <tl/gltf.h>
#include <gl/GL.h>
#include <imgui.h>
#include <backends/imgui_impl_win32.h>
#include <backends/imgui_impl_opengl3.h>

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
	return tformat(u8"{}\\..\\dat\\{}"s, program_directory, relative_path);
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
	style.HoverDelayShort = 1.0f;
	
    ImGui::StyleColorsDark();

	ImGui_ImplWin32_InitForOpenGL(hwnd);
	ImGui_ImplOpenGL3_Init();

	return true;
}

s32 tl_main(Span<String> args) {
	program_path = args[0];
	program_directory = parse_path(program_path).directory;

	if (!create_window()) {
		return 1;
	}

	// 
	// Load scene
	//

	auto imported = glb::parse_from_file(resource_path(u8"scene.glb"s)).value().meshes[0];
	
	struct MyVertex {
		v3f position;
		v3f normal;
	};

	List<MyVertex> mesh;
	for (auto i : imported.indices) {
		auto v = imported.vertices[i];
		mesh.add({
			.position = v.position,
			.normal = v.normal,
		});
	}

	GLuint va = 0;
	glGenVertexArrays(1, &va);
	glBindVertexArray(va);

	GLuint vb = 0;
	glGenBuffers(1, &vb);
	glBindBuffer(GL_ARRAY_BUFFER, vb);
	
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, false, sizeof(MyVertex), (void *)offsetof(MyVertex, position));
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, false, sizeof(MyVertex), (void *)offsetof(MyVertex, normal));

	glBufferData(GL_ARRAY_BUFFER, sizeof(mesh[0]) * mesh.count, mesh.data, GL_STATIC_DRAW);
	
	//
	// Light setup
	//

	u32 shadow_resolution = 4096;

	// Build the texture that will serve as the depth attachment for the framebuffer.
	GLuint depth_texture;
	glGenTextures(1, &depth_texture);
	glBindTexture(GL_TEXTURE_2D, depth_texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, v4f{1,1,1,1}.s);
	//glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LESS);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, shadow_resolution, shadow_resolution, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	glBindTexture(GL_TEXTURE_2D, 0);

	// Build the framebuffer.
	GLuint shadow_map_framebuffer;
	glGenFramebuffers(1, &shadow_map_framebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, shadow_map_framebuffer);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, depth_texture, 0);
	glDrawBuffer(GL_NONE);
	glReadBuffer(GL_NONE);

	GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (status != GL_FRAMEBUFFER_COMPLETE) {
		current_logger.error("Failed to create light framebuffer");
		return 1;
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	// 
	// Shaders
	//

	auto vs = gl::create_shader(GL_VERTEX_SHADER, R"(#version 330 core

layout(location=0) in vec3 position;
layout(location=1) in vec3 normal;

uniform mat4 model_to_world;
uniform mat4 model_to_ndc;

out vec3 v_normal;
out vec3 v_world_position;

void main() {
	v_normal = normal;
	v_world_position = (model_to_world * vec4(position, 1)).xyz;
	gl_Position = model_to_ndc * vec4(position, 1);
}

)"s);

	auto fs = gl::create_shader(GL_FRAGMENT_SHADER, R"(#version 330 core

uniform vec3 light_direction;
uniform sampler2DShadow shadow_map;
uniform mat4 world_to_light;
uniform float density_factor;
uniform float shadow_map_world_width;
uniform float shadow_resolution;
uniform float pcf_radius = 3;

in vec3 v_normal;
in vec3 v_world_position;

out vec4 fragment_color;

float pow2(float x) { return x*x; }
vec2 pow2(vec2 x) { return x*x; }
//vec2 encode(vec2 x) { return (1 - pow2(1 - min(abs(x), 1))) * sign(x); }
//vec2 encode(vec2 x) { float f = 3; return (1 - exp2(-f * abs(x))) / (1 - exp2(-f)) * sign(x); }
//vec2 encode(vec2 x) { return sqrt(abs(x)) * sign(x); }
//vec2 encode(vec2 x) { return normalize(x)*sqrt(length(x)); }
//vec2 encode(vec2 x) { return pow(abs(x), vec2(0.6)) * sign(x); }
//vec2 encode(vec2 x) { float t = 4; return (exp(-t*abs(x)) - 1) / (exp(-t) - 1) * sign(x); }
vec2 encode(vec2 x) { float t = density_factor; return (1 - ((1 - abs(x)) / (1 + (t - 1) * abs(x)))) * sign(x); }

float map(float x, float a, float b, float c, float d) { return (x - a) / (b - a) * (d - c) + c; }
float map_clamped(float x, float a, float b, float c, float d) { return (clamp(x, min(a, b), max(a, b)) - a) / (b - a) * (d - c) + c; }

void main() {
	vec4 shadow_ndc = world_to_light * vec4(v_world_position, 1);
	vec3 shadow_ndc_encoded = shadow_ndc.xyz;
	if (density_factor > 1)
		shadow_ndc_encoded.xy = encode(shadow_ndc_encoded.xy);
	vec3 shadow_uv = shadow_ndc_encoded.xyz * 0.5 + 0.5;

	float lightness = max(1e-3f, dot(v_normal, light_direction));

	float shadow_mask = clamp(map(max(max(abs(shadow_ndc.x), abs(shadow_ndc.y)), abs(shadow_ndc.z)), 0.9, 1, 1, 0), 0, 1);

	if (shadow_mask > 0) {
		float light_visibility = 0;
		float iters = 0;
		for (float dx = -pcf_radius; dx <= pcf_radius; ++dx) {
		for (float dy = -pcf_radius; dy <= pcf_radius; ++dy) {
			//if (dx*dx + dy*dy > pcf_radius*pcf_radius) continue;
			float weight = map_clamped(length(vec2(dx, dy)), pcf_radius, pcf_radius + 1, 1, 0);
			light_visibility += texture(shadow_map, vec3(shadow_uv.xy + vec2(dx, dy) / shadow_resolution, shadow_uv.z - 1e-4)) * weight;
			iters += weight;
		}
		}
		lightness *= mix(1, light_visibility / iters, shadow_mask);

		//fragment_color = vec4(shadow_uv.xy, 0, 1);
		//return;
	}

	fragment_color = vec4(vec3(lightness) + vec3(0.1,0.2,0.3), 1);
}

)"s);

	auto surface_program = gl::create_program({.vertex = vs, .fragment = fs});
	
	vs = gl::create_shader(GL_VERTEX_SHADER, R"(#version 330 core

layout(location=0) in vec3 position;

uniform mat4 model_to_ndc;
uniform float density_factor;

vec2 pow2(vec2 x) { return x*x; }
//vec2 encode(vec2 x) { return (1 - pow2(1 - min(abs(x), 1))) * sign(x); }
//vec2 encode(vec2 x) { float f = 3; return (1 - exp2(-f * abs(x))) / (1 - exp2(-f)) * sign(x); }
//vec2 encode(vec2 x) { return sqrt(abs(x)) * sign(x); }
//vec2 encode(vec2 x) { return normalize(x)*sqrt(length(x)); }
//vec2 encode(vec2 x) { return pow(abs(x), vec2(0.6)) * sign(x); }
//vec2 encode(vec2 x) { float t = 4; return (exp(-t*abs(x)) - 1) / (exp(-t) - 1) * sign(x); }
vec2 encode(vec2 x) { float t = density_factor; return (1 - ((1 - abs(x)) / (1 + (t - 1) * abs(x)))) * sign(x); }

void main() {
	gl_Position = model_to_ndc * vec4(position, 1);
	if (density_factor > 1)
		gl_Position.xy = encode(gl_Position.xy);
}

)"s);

	auto shadow_map_program = gl::create_program({.vertex = vs, .fragment = 0});

	//
	// State
	//
	
	v3f camera_position = V3f(4.251, 7.537, 11.446);
	v3f camera_angles = V3f(0.297, 5.808, 0);
	v2f light_angles = {-0.7, 4};
	f32 density_factor = 16;
	f32 pcf_radius = 3;

	//
	// Stuff
	//

	f32 frame_time = 1.0f / 60;
	f32 time = 0;
	PreciseTimer frame_timer = create_precise_timer();

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
		ImGui::DragFloat2("Light angles", light_angles.s, 0.01f);
		ImGui::SliderFloat("Upclose texel density factor", &density_factor, 1, 16);
		ImGui::SliderFloat("Shadow map world width", &shadow_map_world_width, 16, 256, "%.0f meters");
		ImGui::SliderFloat("PCF radius", &pcf_radius, 0, 3, "%.0f texels");
		ImGui::Text("Shadow map resolution: %d pixels", (int)shadow_resolution);
		ImGui::TextWrapped(R"(This is an alternative to cascaded shadow maps.
It allows for increased shadow resolution close to the camera while rendering the scene just once.
The main issue with this technique is need for dense enough meshes to minimize shadow bending.
Also there is no way to avoid pixel jitter when camera moves)");
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
		if (ImGui::IsKeyDown(ImGuiKey_LeftCtrl)) speed /= 10;

		camera_position += m3::rotation_r_zxy(-camera_angles) * (frame_time * speed * v3f {
			(f32)(ImGui::IsKeyDown(ImGuiKey_D) - ImGui::IsKeyDown(ImGuiKey_A)),
			(f32)(ImGui::IsKeyDown(ImGuiKey_E) - ImGui::IsKeyDown(ImGuiKey_Q)),
			(f32)(ImGui::IsKeyDown(ImGuiKey_S) - ImGui::IsKeyDown(ImGuiKey_W)),
		});
	
		v3f light_direction = m3::rotation_r_zxy(light_angles.x, light_angles.y, 0) * v3f{0,0,1};
		
		//
		// Shadow render
		//
		
		glBindFramebuffer(GL_FRAMEBUFFER, shadow_map_framebuffer);
		glClear(GL_DEPTH_BUFFER_BIT);
		
		glCullFace(GL_FRONT);

		glViewport(0, 0, shadow_resolution, shadow_resolution);

		v3f light_position = camera_position;
		
		v3f z = normalize(-light_direction);
		v3f x = normalize(cross(z, z.y > 0.09f ? v3f{1,0,0} : v3f{0,1,0}));
		v3f y = normalize(-cross(z, x));

		m3 light_rotation_matrix = transpose(m3{x,y,z});

		if (density_factor == 1) {
			// Snap to texel grid to avoid flickering
			f32 texels_per_meter = shadow_resolution / shadow_map_world_width * density_factor;

			light_position = light_rotation_matrix * light_position;
			light_position = round(light_position * texels_per_meter) / texels_per_meter;
			light_position = inverse(light_rotation_matrix) * light_position;
		}

		m4 world_to_light = m4::ortho_right_handed(shadow_map_world_width, 1, -shadow_map_world_width/2, shadow_map_world_width/2) * to_m4(light_rotation_matrix) * m4::translation(-light_position);
		
		glUseProgram(shadow_map_program);
		gl::set_uniform(shadow_map_program, "model_to_world", m4::identity());
		gl::set_uniform(shadow_map_program, "model_to_ndc", world_to_light);
		gl::set_uniform(shadow_map_program, "density_factor", density_factor);
		
		glDrawArrays(GL_TRIANGLES, 0, mesh.count);

		// 
		// Main render
		//

		glBindFramebuffer(GL_FRAMEBUFFER, 0);
		glClearColor(.3, .6, .9, 1);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glCullFace(GL_BACK);

		glViewport(0, 0, screen_size.x, screen_size.y);

		m4 model_to_ndc = m4::perspective_right_handed((f32)screen_size.x / screen_size.y, pi/2, 0.01f, 1000.0f) * m4::rotation_r_yxz(camera_angles) * m4::translation(-camera_position);

		glUseProgram(surface_program);
		gl::set_uniform(surface_program, "model_to_world", m4::identity());
		gl::set_uniform(surface_program, "model_to_ndc", model_to_ndc);
		gl::set_uniform(surface_program, "world_to_light", world_to_light);
		gl::set_uniform(surface_program, "light_direction", light_direction);
		gl::set_uniform(surface_program, "density_factor", density_factor);
		gl::set_uniform(surface_program, "shadow_map_world_width", shadow_map_world_width);
		gl::set_uniform(surface_program, "shadow_resolution", (f32)shadow_resolution);
		gl::set_uniform(surface_program, "pcf_radius", pcf_radius);
		
		//gl::set_uniform(surface_program, "shadow_map", depth_texture);
		glBindTexture(GL_TEXTURE_2D, depth_texture);
		glDrawArrays(GL_TRIANGLES, 0, mesh.count);

		ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

		//
		// Finish frame
		//

		gl::present();

		println("v3f camera_position = V3f{};\nv3f camera_angles = V3f{};", camera_position, camera_angles);

		frame_time = reset(frame_timer);
		time += frame_time;

		current_temporary_allocator.clear();
	}

	return 0;
}

