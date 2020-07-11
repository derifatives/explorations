CC = g++

all: ray_tracer
ray_tracer: main.o readfile.o linalg.o scene.h linalg.h
	$(CC) -o ray_tracer main.o readfile.o linalg.o -lfreeimage
readfile.o: readfile.cc readfile.h
	$(CC) -c readfile.cc
linalg.o: linalg.cc linalg.h
	$(CC) -c linalg.cc
