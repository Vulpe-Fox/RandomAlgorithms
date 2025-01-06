#version 330 core

out vec4 FragColor;
in vec3 FragPos;
in vec2 TexCoords;
in vec3 Normal;

uniform float time;
uniform vec3 lightDir;
uniform vec3 deepColor;
uniform vec3 shallowColor;
uniform float waveStrength;
uniform float waveSpeed;
uniform float waterDepth;

float wave(vec2 position) {
    float frequency = 0.1;
    float amplitude = waveStrength;
    float speed = waveSpeed;
    float value = 
        sin(position.x * frequency + time * speed) * 
        sin(position.y * frequency + time * speed) * amplitude;
    return value;
}

vec3 getColorBasedOnDepth(float depth) {
    float depthFactor = clamp(depth / waterDepth, 0.0, 1.0);
    return mix(shallowColor, deepColor, depthFactor);
}

void main() {
    float waveEffect = wave(FragPos.xz);
    vec3 normalWithWaves = normalize(vec3(Normal.x + waveEffect, 1.0, Normal.z + waveEffect));

    float lightIntensity = max(dot(normalWithWaves, normalize(lightDir)), 0.0);
    vec3 color = getColorBasedOnDepth(FragPos.y);

    // Depth-based opacity (more transparent with depth)
    float opacity = clamp(1.0 - (FragPos.y / waterDepth), 0.3, 1.0);

    FragColor = vec4(color * lightIntensity, opacity);
}