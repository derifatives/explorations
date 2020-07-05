// Copyright rif a. saurous, 2020.

// Linear algebra helper classes.

struct v3 {
public:

  v3(float x, float y, float z)
      : x(x), y(y), z(z) {}
  v3() { x = y = z = 0; }  
  
  float x, y, z;
};  // struct v3
