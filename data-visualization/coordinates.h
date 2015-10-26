#ifndef COORDINATES_H
#define COORDINATES_H

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif


typedef struct coordinates {
	GLfloat x;
	GLfloat y;
	GLfloat z;
} coordinates;

typedef struct scale {
	GLfloat cur_val;
	GLfloat limit; // suppose we won't need different min
	GLfloat step;
} scale;

coordinates *getNormal(coordinates v1,coordinates v2,coordinates v3);
coordinates *coordinates_init(GLfloat x,GLfloat y,GLfloat z);
void coordinates_free(coordinates* vector);

scale *scale_init(GLfloat limit, GLfloat step);
void scale_inc(scale *s);
void scale_dec(scale *s);


#endif /* COORDINATES_H */
