#pragma solution

uniform vec2 iResolution;
uniform float iGlobalTime;

float distanceFromCircle(vec2 point, float radius) {
  point = mod(point + 0.25, 0.5) - 0.25;
  vec2 q = abs(point);
  return max((q.x * 0.866025 + q.y * 0.5), q.y) - radius;
}

void main() {
  vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
  float radius = (sin(iGlobalTime * 0.25) * 0.5 + 0.5) * 0.1 + 0.05;
  float dist = distanceFromCircle(uv, radius);

  gl_FragColor = vec4(draw_distance(dist, uv), 1);
}