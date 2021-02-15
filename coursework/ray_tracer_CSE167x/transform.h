// Copyright rif a. saurous, 2020.

// Transformations.

#ifndef TRANSFORM_H
#define TRANSFORM_H

#include "linalg.h"

m3 rotate(double degrees, const v3& axis);
m4 scale(double sx, double sy, double sz);
m4 translate(const v3& v);
m4 lookAt(const v3& eye, const v3& up);
  
#endif  // TRANSFORM_H
