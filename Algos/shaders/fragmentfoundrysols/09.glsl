#pragma solution

uniform vec2 iResolution;
uniform float iGlobalTime;

float smin(float a, float b, float k) {
  float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
  return mix(b, a, h) - k * h * (1.0 - h);
}

float distanceField(vec2 point, vec2 origin1, vec2 origin2, float radius) {
  float d1 = length(point - origin1) - radius;
  float d2 = length(point - origin2) - radius;
  return smin(d1, d2, 0.1);
}

void main() {
  vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
  vec2 origin = vec2(sin(iGlobalTime * 0.11) * 0.3);
  float radius = (sin(iGlobalTime * 0.25) * 0.5 + 0.5) * 0.3 + 0.05;
  float dist = distanceField(uv, origin, -origin, radius);

  gl_FragColor = vec4(draw_distance(dist, uv), 1);
}