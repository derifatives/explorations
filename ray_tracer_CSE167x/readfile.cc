// Copyright rif a. saurous, 2020.

// Scene file reader.

#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>

#include "linalg.h"

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

float readFloat(stringstream& s) {
  float f;
  s >> f;
  if (s.fail()) {
    cerr << "Failed to read float " << endl;
    exit(-1);
  }
  return f;
}


v3 readV3(stringstream& s) {
  vector<float> fv(3, 0.);
  for (int i = 0; i < 3; ++i) {
    fv[i] = readInt(s);
  }
  return v3(fv[0], fv[1], fv[2]);
}


void readfile(const char* filename) {
  ifstream in;
  string line, command;
  in.open(filename);
  bool parsed_camera(false), parsed_size(false);

  int size_x, size_y;
  v3 eyeinit, upinit, center;
  float fovy;

  if (!in.is_open()) {
    cerr << "Failed to open " << filename;
    exit(-1);
  }

  getline(in, line);
  while (in) {
    stringstream s(line);
    s >> command;

    if ((line.find_first_not_of(" \t\r\n") == string::npos) ||
        (line[0] == '#')) {
    } else if (command == "size") {
      if (parsed_size) {
        cerr << "Trying to parse size twice." << endl;
        exit(-1);
      }

      size_x = readInt(s);
      size_y = readInt(s);
      parsed_size = true;
    } else if (command == "camera") {
      if (parsed_camera) {
        cerr << "Trying to parse camera twice." << endl;
        exit(-1);
      }

      eyeinit = readV3(s);
      center = readV3(s);
      upinit = readV3(s);
      fovy = readFloat(s);

      parsed_camera = true;
    } else {
      cout << "Skipping unknown command " << command << endl;
    }

    getline(in, line);
  }

  return;
}
