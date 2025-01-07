#pragma solution

float getDistanceFromPoint(vec3 point) {
  float radius = (sin(iGlobalTime * 0.1) * 0.5 + 0.5) * 0.25;
  return length(point - vec3(0, radius, 0)) - radius;
}