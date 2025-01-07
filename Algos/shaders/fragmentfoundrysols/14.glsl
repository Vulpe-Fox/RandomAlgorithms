#pragma solution

float smin( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}

float getDistanceFromPoint(vec3 point) {
  vec3 offset = vec3(0.25 * sin(iGlobalTime * 0.15), 0, 0);
  float d1 = length(point - offset) - 0.1;
  float d2 = length(point + offset) - 0.3;

  return smin(d1, d2, 0.1);
}