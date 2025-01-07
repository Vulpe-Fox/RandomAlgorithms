#pragma solution

float getDistanceFromPoint(vec3 point) {
  vec3 offset = vec3(0.25, 0, 0);
  float radius = 0.3;
  float d1 = length(point - offset) - radius;
  float d2 = length(point + offset) - radius;
  float d3 = length(point - offset.zyx) - radius;
  float d4 = length(point + offset.zyx) - radius;

  float min1 = min(d1, d2);
  float min2 = min(d3, d4);

  return min(min1, min2);
}