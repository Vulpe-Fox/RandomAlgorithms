#pragma solution

uniform vec2 iResolution;

bool inBox(vec2 uv) {
  return uv.x <= 0.5 && uv.x >= -0.5 && uv.y <= 0.5 && uv.y >= -0.5;
}

void main() {
  vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
  if (inBox(uv)) {
    gl_FragColor = vec4(1, 0.6, 0.5, 1);
  } else {
    gl_FragColor = vec4(0.5, 0.8, 1, 1);
  }
}