// Copyright rif a. saurous, 2020.

// Top-level scene abstraction for ray_tracer.

#ifndef SCENE_H
#define SCENE_H

#include <vector>

#include "linalg.h"

using namespace::std;

struct Camera {
 Camera(v3 eye, v3 center, v3 up, double fovy)
      : eye(eye), center(center), up(up), fovy(fovy) {}
  v3 eye, center, up;
  double fovy;
};  // struct Camera 

struct Film {
 Film(int size_x, int size_y)
     : size_x(size_x), size_y(size_y) {}
  int size_x, size_y;
};  // struct Film

// Materials. We choose to represent the complete material properties as a
// single object, and then well keep a vector of these for the scene. This is
// efficient when many objects share all the same materials properties or few
// properties.
struct Materials {
 Materials(v3 ambient, v3 diffuse, v3 specular, double shininess) :
  ambient(ambient), diffuse(diffuse), specular(specular), shininess(shininess) {}

  v3 ambient;
  v3 diffuse;
  v3 specular;
  double shininess;
};

struct Triangle {
 Triangle(int v1, int v2, int v3) : v1(v1), v2(v2), v3(v3) {}
  int v1, v2, v3;
};

// Default constructor with empty vectors is fine.
struct TriangleGroup {
  vector<Triangle> triangles_v;
};  // struct FaceSet

struct Scene {
 Scene(Camera camera,
       Film film,
       vector<Materials> materials_v,
       vector<v3> vertices_v,
       vector<m4> transformations_v,
       vector<TriangleGroup> triangle_groups_v)
     : camera(camera),
      film(film),
      materials_v(materials_v),
      vertices_v(vertices_v),
      transformations_v(transformations_v),
      triangle_groups_v(triangle_groups_v) {} 
  
  Camera camera;
  Film film;
  vector<Materials> materials_v;
  vector<v3> vertices_v;
  vector<m4> transformations_v;
  vector<TriangleGroup> triangle_groups_v;
  // vector<Sphere> sphere_v;
};  // struct Scene

# endif  // SCENE_H
