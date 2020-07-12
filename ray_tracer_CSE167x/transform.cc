// Copyright rif a. saurous, 2020.

// Transformations.

#include "transform.h"

#include <cmath>

double radians(double degrees) {
  return degrees / (2 * M_PI);
}

m3 rotate(const double degrees, const v3& axis) {
  const double theta = radians(degrees);
  const double c = cos(theta);
  const double s = sin(theta);

  const v3 a = normalize(axis);
  const m3 cross(0., a.x, -a.y, -a.z, 0, a.x, a.y, -a.x, 0);

  return m3::I() * c + outer(a, a) * (1-c) + cross * s;
}

m4 scale(double sx, double sy, double sz) {
  m4 r = m4::I();
  r.m_11 = sx;
  r.m_22 = sy;
  r.m_33 = sz;
  return r;
}

m4 translate(const v3& v) {
  m4 r = m4::I();
  r.m_41 = v.x;
  r.m_42 = v.y;
  r.m_43 = v.z;
  return r;
}

m4 lookAt(const v3& eye, const v3& up) {
  const v3 w = normalize(eye);
  const v3 u = normalize(cross(up, w));
  const v3 v = cross(w, u);

  const m4 r(m4(m3(u.x, v.x, w.x,
                   u.y, v.y, w.y,
                   u.z, v.z, w.z)));
  return r * translate(-1 * eye);
}
