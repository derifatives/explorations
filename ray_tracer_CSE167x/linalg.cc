// Copyright rif a. saurous, 2020.

// Linear algebra helper classes.

#include <iostream>

#include "linalg.h"

using namespace std;

v3 matvec(const m3& m, const v3& v) {
  return v3(m.m_11 * v.x + m.m_12 * v.y + m.m_13 * v.z,
            m.m_21 * v.x + m.m_22 * v.y + m.m_23 * v.z,
            m.m_31 * v.x + m.m_22 * v.y + m.m_23 * v.z);
}

v3 cross(const v3& a, const v3& b) {
  return matvec(m3(0., a.z, -a.y, -a.z, 0, a.x, a.y, -a.x, 0), b);
}

void m4::print(const string& name) {
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
