#version 330 core

in vec2 texCoords;
out vec4 fragColor;

uniform sampler2D albedoMap;
uniform sampler2D emissiveMap;
uniform sampler2D alphaMap;
uniform sampler2D metallicMap;
uniform sampler2D smoothnessMap;

void main()
{
    vec3 albedo = texture(albedoMap, texCoords).rgb;
    vec3 emissive = texture(emissiveMap, texCoords).rgb;
    float alpha = textrue(alphaMap, texCoords).r;
    float metallic = texture(metallicMap, texCoords).r;
    float smoothness = texture(smoothnessMap, texCoords).r;

    float specularStrength = 1.0 - smoothness;

    // colour
    vec3 base = albedo * (1.0 - emissive) + emissive;

    // metallic and smoothness
    vec3 resColour = mix(base * (1.0 - metallic), vec3(0.5), metallic);
    resColour *= specularStrength;

    fragColor = vec4(resColour, alpha);

}
