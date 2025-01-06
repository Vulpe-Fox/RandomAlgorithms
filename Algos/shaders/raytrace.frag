#version 450 core

out vec4 FragColor;
in vec2 TexCoords;

uniform vec3 cameraPosition;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;

struct Light {
    vec3 position;
    vec3 color;
    float intensity;
};

uniform Light lights[4];
uniform int numLights;
uniform vec3 ambientColor;
uniform vec3 backgroundColor;

struct Material {
    vec3 diffuse;
    vec3 specular;
    float shininess;
    float reflectivity;
};

vec3 rayDirection(vec2 uv) {
    vec4 clipCoords = vec4(uv, -1.0, 1.0);
    vec4 viewCoords = inverse(projectionMatrix) * clipCoords;
    return normalize((inverse(viewMatrix) * vec4(viewCoords.xyz, 0.0)).xyz);
}

float intersectSphere(vec3 origin, vec3 dir, vec3 sphereCenter, float radius) {
    vec3 oc = origin - sphereCenter;
    float a = dot(dir, dir);
    float b = 2.0 * dot(oc, dir);
    float c = dot(oc, oc) - radius * radius;
    float discriminant = b * b - 4.0 * a * c;

    if (discriminant > 0.0) {
        float t1 = (-b - sqrt(discriminant)) / (2.0 * a);
        float t2 = (-b + sqrt(discriminant)) / (2.0 * a);
        return min(t1, t2) > 0.0 ? min(t1, t2) : max(t1, t2);
    }
    return -1.0;
}

vec3 computeLighting(vec3 point, vec3 normal, vec3 viewDir, Material material) {
    vec3 result = ambientColor;

    for (int i = 0; i < numLights; ++i) {
        vec3 lightDir = normalize(lights[i].position - point);
        float diff = max(dot(normal, lightDir), 0.0);
        vec3 diffuse = lights[i].color * diff * material.diffuse * lights[i].intensity;

        vec3 reflectDir = reflect(-lightDir, normal);
        float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
        vec3 specular = lights[i].color * spec * material.specular * lights[i].intensity;

        result += diffuse + specular;
    }

    return result;
}

void main() {
    vec3 rayDir = rayDirection(TexCoords);

    // Define a simple sphere for demonstration
    vec3 sphereCenter = vec3(0.0, 0.0, -5.0);
    float sphereRadius = 1.0;
    Material sphereMaterial = Material(vec3(0.8, 0.3, 0.2), vec3(1.0, 1.0, 1.0), 32.0, 0.5);

    float t = intersectSphere(cameraPosition, rayDir, sphereCenter, sphereRadius);
    if (t > 0.0) {
        vec3 hitPoint = cameraPosition + t * rayDir;
        vec3 normal = normalize(hitPoint - sphereCenter);
        vec3 viewDir = normalize(cameraPosition - hitPoint);

        vec3 localColor = computeLighting(hitPoint, normal, viewDir, sphereMaterial);

        // Simple reflection
        if (sphereMaterial.reflectivity > 0.0) {
            vec3 reflectionDir = reflect(rayDir, normal);
            vec3 reflectionColor = backgroundColor;  // In a full renderer, trace the reflected ray
            localColor = mix(localColor, reflectionColor, sphereMaterial.reflectivity);
        }

        FragColor = vec4(localColor, 1.0);
    } else {
        FragColor = vec4(backgroundColor, 1.0);
    }
}