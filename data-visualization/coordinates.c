#include "coordinates.h"
#include "math.h"
#include <stdio.h>

coordinates* getNormal(coordinates v1,coordinates v2,coordinates v3){
    coordinates a = {v2.x - v1.x, v2.y - v1.y, v2.z - v1.z};
    coordinates b = {v3.x - v2.x, v3.y - v2.y, v3.z - v2.z};
    
    coordinates* cross =  (coordinates*) malloc(sizeof(coordinates));
    cross->x = a.y*b.z - a.z*b.y;
    cross->y = a.x*b.z - a.z*b.x;
    cross->z = a.x*b.y - a.y*b.x;
    
    float size = sqrtf(powf(cross->x, 2) + powf(cross->y, 2) + powf(cross->z, 2));
    cross->x /= size;
    cross->y /= size;
    cross->z /= size;
    
    return cross;
}

coordinates* coordinates_init(GLfloat x, GLfloat y, GLfloat z){
    coordinates* res = (coordinates*) malloc(sizeof(coordinates));
    res->x = x;
    res->y = y;
    res->z = z;
    return res;
}

void coordinates_free(coordinates* vector){
    free(vector);
}

/* Might not be necessary, since we probably won't need to change scales
 * at runtime
 */
scale *scale_init(GLfloat limit, GLfloat step)
{
	scale *new_scale;
	new_scale = malloc(sizeof(scale));

	new_scale->cur_val = 0.0;
	new_scale->limit = limit;
	new_scale->step = step;
	return new_scale;
}

void scale_inc(scale *s)
{
	if (s->cur_val >= s->limit)
		printf("Already at bound\n");
	else
		s->cur_val += s->step;
}

void scale_dec(scale *s)
{
	if (s->cur_val <= -(s->limit))
		printf("Already at bound\n");
	else
		s->cur_val -= s->step;
}


