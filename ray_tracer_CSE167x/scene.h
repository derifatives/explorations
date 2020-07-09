// Copyright rif a. saurous, 2020.

// Top-level scene abstraction for ray_tracer.

#include "linalg.h"

struct Camera {
  Camera(v3 eye, v3 center, v3 up)
      : eye(eye), center(center), up(up) {}
  v3 eye, center, up;
};

struct Film {
  Film(int size_x, int size_y)
      : size_x(size_x), size_y(size_y) {}
  int size_x, size_y;
};
    
struct Scene {
  Scene(Camera camera, Film film)
      : camera(camera), film(film){} 

  Camera camera;
  Film film;
};
