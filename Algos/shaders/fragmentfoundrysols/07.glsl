#pragma solution

uniform vec2 iResolution;
uniform float iGlobalTime;

float distanceField(vec2 point, vec2 origin, float radius) {
  return length(point - origin) - radius;
}

void main() {
  vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
  vec2 origin = vec2(sin(iGlobalTime * 0.3) * 0.3);
  float radius = (sin(iGlobalTime * 0.25) * 0.5 + 0.5) * 0.5 + 0.3;
  float dist = distanceField(uv, origin, radius);

  gl_FragColor = vec4(draw_distance(dist, uv), 1);
}