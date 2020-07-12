// Copyright rif a. saurous, 2020.

// Linear algebra helper classes.

#include <iostream>
#include <tgmath.h>

#include "linalg.h"

using namespace std;

v3::v3(double x, double y, double z)
    : x(x), y(y), z(z) {}
v3::v3() { x = y = z = 0; }

v3 normalize(const v3& v) {
  double normalizer = sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  return v3(v.x / normalizer, v.y / normalizer, v.z / normalizer);
}

void v3::print(const string& name) const {
  cout << "--- " << name << " ---" << endl;
  cout << this->x << " " << this->y << " " << this->z << endl;
  cout << "-----" << endl;
}

v3 operator*(const v3& v, double d) {
  return v3(v.x * d, v.y * d, v.z * d);
}

v3 operator*(double d, const v3& v) {
  return v * d;
}

m3::m3(double m_11, double m_21, double m_31,
       double m_12, double m_22, double m_32,
       double m_13, double m_23, double m_33)
    : m_11(m_11), m_21(m_21), m_31(m_31),
      m_12(m_12), m_22(m_22), m_32(m_32),
      m_13(m_13), m_23(m_23), m_33(m_33) {}

m3 operator*(const m3& m, double d) {
  return m3(m.m_11 * d, m.m_12 * d, m.m_13 * d,
            m.m_21 * d, m.m_22 * d, m.m_23 * d,
            m.m_31 * d, m.m_32 * d, m.m_33 * d);
}

m3 operator*(double d, const m3& m) {
  return m * d;
}

v3 matvec(const m3& m, const v3& v) {
  return v3(m.m_11 * v.x + m.m_12 * v.y + m.m_13 * v.z,
            m.m_21 * v.x + m.m_22 * v.y + m.m_23 * v.z,
            m.m_31 * v.x + m.m_22 * v.y + m.m_23 * v.z);
}

v3 cross(const v3& a, const v3& b) {
  return matvec(m3(0., a.z, -a.y, -a.z, 0, a.x, a.y, -a.x, 0), b);
}

m3 outer(const v3& a, const v3& b) {
  return m3(a.x * a.x, a.x * a.y, a.x * a.z,
            a.y * a.x, a.y * a.y, a.y * a.z,
            a.z * a.x, a.z * a.y, a.z * a.z);
}

m4::m4(double m_11, double m_21, double m_31, double m_41,
       double m_12, double m_22, double m_32, double m_42,
       double m_13, double m_23, double m_33, double m_43,
       double m_14, double m_24, double m_34, double m_44)
    : m_11(m_11), m_21(m_21), m_31(m_31), m_41(m_41),
      m_12(m_12), m_22(m_22), m_32(m_32), m_42(m_42),
      m_13(m_13), m_23(m_23), m_33(m_33), m_43(m_43),
      m_14(m_14), m_24(m_24), m_34(m_34), m_44(m_44) {}

m4::m4(const m3& m3)
    : m_11(m3.m_11), m_21(m3.m_21), m_31(m3.m_31), m_41(0.),
      m_12(m3.m_12), m_22(m3.m_22), m_32(m3.m_32), m_42(0.),
      m_13(m3.m_13), m_23(m3.m_23), m_33(m3.m_33), m_43(0.),
      m_14(0.), m_24(0.), m_34(0.), m_44(1.) {}

void m4::print(const string& name) const {
  cout << "--- " << name << " ---" << endl;
  cout << m_11 << " " << m_12 << " " << m_13 << " " << m_14 << endl;
  cout << m_21 << " " << m_22 << " " << m_23 << " " << m_24 << endl;
  cout << m_31 << " " << m_32 << " " << m_33 << " " << m_34 << endl;
  cout << m_41 << " " << m_42 << " " << m_43 << " " << m_44 << endl;
  cout << "-----" << endl;
}

m4 matmul(const m4& m1, const m4& m2) {
  return m4(m1.m_11 * m2.m_11 + m1.m_12 * m2.m_21 + m1.m_13 * m2.m_31 + m1.m_14 * m2.m_41,  // 11
            m1.m_21 * m2.m_11 + m1.m_22 * m2.m_21 + m1.m_23 * m2.m_31 + m1.m_24 * m2.m_41,  // 21
            m1.m_31 * m2.m_11 + m1.m_32 * m2.m_21 + m1.m_33 * m2.m_31 + m1.m_34 * m2.m_41,  // 31
            m1.m_41 * m2.m_11 + m1.m_42 * m2.m_21 + m1.m_43 * m2.m_31 + m1.m_44 * m2.m_41,  // 41

            m1.m_11 * m2.m_12 + m1.m_12 * m2.m_22 + m1.m_13 * m2.m_32 + m1.m_14 * m2.m_42,  // 12
            m1.m_21 * m2.m_12 + m1.m_22 * m2.m_22 + m1.m_23 * m2.m_32 + m1.m_24 * m2.m_42,  // 22
            m1.m_31 * m2.m_12 + m1.m_32 * m2.m_22 + m1.m_33 * m2.m_32 + m1.m_34 * m2.m_42,  // 32
            m1.m_41 * m2.m_12 + m1.m_42 * m2.m_22 + m1.m_43 * m2.m_32 + m1.m_44 * m2.m_42,  // 42

            m1.m_11 * m2.m_13 + m1.m_12 * m2.m_23 + m1.m_13 * m2.m_33 + m1.m_14 * m2.m_43,  // 13
            m1.m_21 * m2.m_13 + m1.m_22 * m2.m_23 + m1.m_23 * m2.m_33 + m1.m_24 * m2.m_43,  // 23
            m1.m_31 * m2.m_13 + m1.m_32 * m2.m_23 + m1.m_33 * m2.m_33 + m1.m_34 * m2.m_43,  // 33
            m1.m_41 * m2.m_13 + m1.m_42 * m2.m_23 + m1.m_43 * m2.m_33 + m1.m_44 * m2.m_43,  // 43

            m1.m_11 * m2.m_14 + m1.m_12 * m2.m_24 + m1.m_13 * m2.m_34 + m1.m_14 * m2.m_44,  // 14
            m1.m_21 * m2.m_14 + m1.m_22 * m2.m_24 + m1.m_23 * m2.m_34 + m1.m_24 * m2.m_44,  // 24
            m1.m_31 * m2.m_14 + m1.m_32 * m2.m_24 + m1.m_33 * m2.m_34 + m1.m_34 * m2.m_44,  // 34
            m1.m_41 * m2.m_14 + m1.m_42 * m2.m_24 + m1.m_43 * m2.m_34 + m1.m_44 * m2.m_44); // 44
}

m4 operator*(const m4& m1, const m4& m2) {
  return matmul(m1, m2);
}
