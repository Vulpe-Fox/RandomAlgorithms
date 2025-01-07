#pragma solution

float getDistanceFromPoint(vec3 point) {
  float period = 0.4;
  float radius = 0.1;
  point.xz = mod(point.xz + period * 0.5, period) - period * 0.5;
  return length(point) - radius;
}