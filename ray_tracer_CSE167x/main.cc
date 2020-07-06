// Copyright rif a. saurous, 2020.

// Main program for ray_tracer.

#include <iostream>
#include <FreeImage.h>

#include "readfile.h"

using namespace std;

// NOTE: The channel layout seems to BGR (not RGB), both empirically and looking
// at the documentation.
void SaveDummyImage() {
  FreeImage_Initialise();
  int width = 256, height = 400, num_pixels = width * height;
  BYTE* pixels = new BYTE[num_pixels * 3];

  int byte_index = 0;
  for (int i = 0; i < height; ++i) {
    for (int j = 0; j < width; ++j) {
      pixels[byte_index++] = j; // Blue
      pixels[byte_index++] = 0; // Green
      pixels[byte_index++] = 0; // Red
    }
  }

  // Note: The mask arguments don't seem to do anything.
  FIBITMAP *img = FreeImage_ConvertFromRawBits(pixels, width, height, width * 3, 24, 0xFF0000, 0x00FF00, 0x0000FF, true);
  FreeImage_Save(FIF_PNG, img, "screenshot.png", 0);

  delete[] pixels;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << "Usage: " << argv[0] << " scene_file" << endl;
    exit(-1);
  }

  cout << "I'd like to read " << argv[1] << endl;
  readfile(argv[1]);

  SaveDummyImage();
  exit(0);
}
