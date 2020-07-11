// Copyright rif a. saurous, 2020.

// Linear algebra helper classes.

#ifndef LINALG_H
#define LINALG_H

#include <string>

struct v3 {
  v3(double x, double y, double);
  v3();
  
  double x, y, z;
};  // struct v3

struct m3 {
  m3(double m_11, double m_21, double m_31,
     double m_12, double m_22, double m_32,
     double m_13, double m_23, double m_33);

  double m_11, m_21, m_31;
  double m_12, m_22, m_32;
  double m_13, m_23, m_33;
};  // struct m3

v3 matvec(const m3& m, const v3& v);

v3 cross(const v3& a, const v3& b);

struct m4 {
 m4(double m_11, double m_21, double m_31, double m_41,
    double m_12, double m_22, double m_32, double m_42,
    double m_13, double m_23, double m_33, double m_43,
    double m_14, double m_24, double m_34, double m_44);

  // Build an m4 from an m3 by putting the m3 in the upper-left 3-by-3
  // submatrix, a 1 in the lower right, and zeros along the rest of the last row
  // and column.
  m4(const m3& m3);
  
  static m4 eye() {
    return m4(1., 0., 0., 0.,
              0., 1., 0., 0.,
              0., 0., 1., 0.,
              0., 0., 0., 1.);
  }

  void print(const std::string& name);

  double m_11, m_21, m_31, m_41;
  double m_12, m_22, m_32, m_42;
  double m_13, m_23, m_33, m_43;
  double m_14, m_24, m_34, m_44;
};  // struct m4

m4 matmul(const m4& m1, const m4& m2);

# endif  // LINALG_H
