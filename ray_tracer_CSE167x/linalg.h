// Copyright rif a. saurous, 2020.

// Linear algebra helper classes.

#include <string>

struct v3 {
  v3(float x, float y, float z)
      : x(x), y(y), z(z) {}
  v3() { x = y = z = 0; }  
  
  float x, y, z;
};  // struct v3

struct m3 {
  m3(float m_11, float m_21, float m_31,
     float m_12, float m_22, float m_32,
     float m_13, float m_23, float m_33) :
  m_11(m_11), m_21(m_21), m_31(m_31),
      m_12(m_12), m_22(m_22), m_32(m_32),
      m_13(m_13), m_23(m_23), m_33(m_33) {}

  float m_11, m_21, m_31;
  float m_12, m_22, m_32;
  float m_13, m_23, m_33;
};  // struct m3

v3 matvec(const m3& m, const v3& v);

v3 cross(const v3& a, const v3& b);

struct m4 {
 m4(float m_11, float m_21, float m_31, float m_41,
    float m_12, float m_22, float m_32, float m_42,
    float m_13, float m_23, float m_33, float m_43,
    float m_14, float m_24, float m_34, float m_44) :
  m_11(m_11), m_21(m_21), m_31(m_31), m_41(m_41),
      m_12(m_12), m_22(m_22), m_32(m_32), m_42(m_42),
      m_13(m_13), m_23(m_23), m_33(m_33), m_43(m_43),
      m_14(m_14), m_24(m_24), m_34(m_34), m_44(m_44) {}

  static m4 eye() {
    return m4(1., 0., 0., 0.,
              0., 1., 0., 0.,
              0., 0., 1., 0.,
              0., 0., 0., 1.);
  }

  void print(const std::string& name);

  float m_11, m_21, m_31, m_41;
  float m_12, m_22, m_32, m_42;
  float m_13, m_23, m_33, m_43;
  float m_14, m_24, m_34, m_44;
};  // struct m4

m4 matmul(const m4& m1, const m4& m2);
      
