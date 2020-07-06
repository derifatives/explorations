CC = g++

all: ray_tracer
ray_tracer: main.o readfile.o 
	$(CC) -o ray_tracer main.o readfile.o -lfreeimage
readfile.o: readfile.cc readfile.h
	$(CC) -c readfile.cc

