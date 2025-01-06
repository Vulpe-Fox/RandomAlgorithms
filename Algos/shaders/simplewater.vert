#version 330 core

layout(location = 0) in vec3 aPos;
layout(location = 1) in vec2 aTexCoords;
layout(location = 2) in vec3 aNormal;

out vec3 FragPos;
out vec2 TexCoords;
out vec3 Normal;

uniform float time;
uniform float waveStrength;
uniform float waveSpeed;

vec3 calculateWaveOffset(vec3 position) {
    float frequency = 0.1;
    float amplitude = waveStrength;
    float speed = waveSpeed;

    float waveX = sin(position.x * frequency + time * speed) * amplitude;
    float waveZ = sin(position.z * frequency + time * speed) * amplitude;
    
    return vec3(0.0, waveX + waveZ, 0.0);
}

void main() {
    vec3 waveOffset = calculateWaveOffset(aPos);
    vec3 newPosition = aPos + waveOffset;

    FragPos = newPosition;
    TexCoords = aTexCoords;
    Normal = normalize(vec3(aNormal.x + waveOffset.x, aNormal.y + 1.0, aNormal.z + waveOffset.z));

    gl_Position = vec4(newPosition, 1.0);
}