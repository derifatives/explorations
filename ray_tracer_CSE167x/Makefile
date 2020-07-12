CC = g++

all: ray_tracer
ray_tracer: main.o linalg.o readfile.o scene.o transform.o linalg.h readfile.h scene.h transform.h
	$(CC) -o ray_tracer main.o linalg.o readfile.o scene.o transform.o -lfreeimage
readfile.o: readfile.cc readfile.h
	$(CC) -c readfile.cc
linalg.o: linalg.cc linalg.h
	$(CC) -c linalg.cc
scene.o: scene.h scene.cc linalg.h
	$(CC) -c scene.cc
transform.o: transform.h transform.cc linalg.h
	$(CC) -c transform.cc
