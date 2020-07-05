// Copyright rif a. saurous, 2020.

// Main program for ray_tracer.

#include <iostream>

#include "readfile.h"

using namespace std;

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << "Usage: " << argv[0] << " scene_file" << endl;
    exit(-1);
  }

  cout << "I'd like to read " << argv[1] << endl;
  readfile(argv[1]);
  exit(0);
}
