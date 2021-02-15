// Copyright rif a. saurous, 2020.

// Scene file reader.

#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>

#include "linalg.h"
#include "readfile.h"

using namespace std;

int readInt(stringstream& s) {
  int i;
  s >> i;
  if (s.fail()) {
    cerr << "Failed to read int " << endl;
    exit(-1);
  }
  return i;
}

double readDouble(stringstream& s) {
  double f;
  s >> f;
  if (s.fail()) {
    cerr << "Failed to read double " << endl;
    exit(-1);
  }
  return f;
}

v3 readV3(stringstream& s) {
  vector<double> fv(3, 0.);
  for (int i = 0; i < 3; ++i) {
    fv[i] = readDouble(s);
  }
  return v3(fv[0], fv[1], fv[2]);
}

Triangle readTriangle(stringstream& s) {
  int i1 = readInt(s);
  int i2 = readInt(s);
  int i3 = readInt(s);
  return Triangle(i1, i2, i3);
}

Scene readfile(const char* filename) {
  ifstream in;
  string line, command;
  in.open(filename);
  bool parsed_camera(false), parsed_film(false);

  int size_x, size_y;
  v3 eyeinit, upinit, center;
  double fovy;

  // Initialize materials.
  v3 ambient(0.2, 0.2, 0.2);
  v3 diffuse(0., 0., 0.);
  v3 specular(0., 0., 0.);
  double shininess(0.);
  Materials materials(ambient, diffuse, specular, shininess);

  vector<Materials> materials_v;
  materials_v.push_back(materials);
  bool materials_changed = false;

  // Initialize transformation.
  m4 transformation = m4::eye();
  vector<m4> transformations_v;
  transformations_v.push_back(transformation);
  bool transformation_changed = false;

  // Initialize vertices.
  vector<v3> vertices_v;

  // Initialize triangle group.
  TriangleGroup triangle_group;
  vector<TriangleGroup> triangle_groups_v;
  triangle_groups_v.push_back(triangle_group);


  if (!in.is_open()) {
    cerr << "Failed to open " << filename << endl;
    exit(-1);
  }

  getline(in, line);
  while (in) {
    stringstream s(line);
    s >> command;

    if ((line.find_first_not_of(" \t\r\n") == string::npos) ||
        (line[0] == '#')) {
    } else if (command == "size") {
      if (parsed_film) {
        cerr << "Trying to parse size twice." << endl;
        exit(-1);
      }

      size_x = readInt(s);
      size_y = readInt(s);
      parsed_film = true;
    } else if (command == "camera") {
      if (parsed_camera) {
        cerr << "Trying to parse camera twice." << endl;
        exit(-1);
      }

      eyeinit = readV3(s);
      center = readV3(s);
      upinit = readV3(s);
      fovy = readDouble(s);

      parsed_camera = true;
    } else if (command == "ambient") {
      ambient = readV3(s);
      materials_changed = true;
    } else if (command == "diffuse") {
      diffuse = readV3(s);
      materials_changed = true;
    } else if (command == "specular") {
      specular = readV3(s);
      materials_changed = true;
    } else if (command == "shininess") {
      shininess = readDouble(s);
      materials_changed = true;
    } else if (command == "vertex") {
      vertices_v.push_back(readV3(s));
    } else if (command == "tri") {  // Add sphere here.
      if (transformation_changed) {
        transformations_v.push_back(transformation);
        triangle_groups_v.push_back(triangle_group);
        triangle_group = TriangleGroup();
        transformation_changed = false;
      }

      if (materials_changed) {
        materials_v.push_back(materials);
        materials_changed = false;
      }

      if (command == "tri") {
        triangle_group.triangles_v.push_back(readTriangle(s));
      }
    } else {
      cerr << "Skipping unknown command " << command << endl;
    }

    getline(in, line);
  }

  if (!parsed_camera) {
    cerr << "No camera parsed." << endl;
    exit(-1);
  }
  if (!parsed_film) {
    cerr << "No film parsed." << endl;
    exit(-1);
  }

  return Scene(Camera(eyeinit, center, upinit, fovy),
               Film(size_x, size_y),
               materials_v,
               vertices_v,
               transformations_v,
               triangle_groups_v);
}
