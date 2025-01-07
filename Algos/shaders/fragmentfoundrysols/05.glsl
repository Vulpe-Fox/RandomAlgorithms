#pragma solution

uniform vec2 iResolution;
uniform float iGlobalTime;

bool inCircle(vec2 uv, float radius) {
  return length(uv) <= radius;
}

void main() {
  vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
  float radius = (sin(iGlobalTime * 0.25) * 0.5 + 0.5) * 0.5 + 0.3;
  if (inCircle(uv, radius)) {
    gl_FragColor = vec4(1, 0.6, 0.5, 1);
  } else {
    gl_FragColor = vec4(0.5, 0.8, 1, 1);
  }
}