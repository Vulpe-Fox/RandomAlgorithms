#pragma solution

float getDistanceFromPoint(vec3 point) {
  vec3 offset = vec3(0.75, 0, 0) * abs(sin(iGlobalTime * 0.1));
  float radius = 0.1;

  float pi = 3.14159;
  float angle = atan(point.z, point.x);
  float dist = length(point.xz);
  float count = 5.0;
  float period = 2.0 * pi / count;

  angle = mod(angle + period * 0.5, period) - period * 0.5;
  point.xz = vec2(dist * cos(angle), dist * sin(angle));

  return length(point - offset) - radius;
}