#include "data-serializate.h"
#include "error.h"
#include <stdlib.h>
#include <stdio.h>

matrix3D matrix3D_init(int n, int m, int k){
	matrix3D matrix = {n,m,k};
	matrix.data = (float*) malloc(sizeof(float)*n*m*k);
	
	return matrix;
}

float matrix3D_get(matrix3D matrix, int x, int y, int z){
	//x + y * WIDTH + z * WIDTH * DEPTH
	if(x < matrix.n && y < matrix.m && z < matrix.k){
		return matrix.data[x + matrix.n*(y+matrix.k+z)];
	} else {
		printf("out of bounds");
		return 0;
	}
}

void matrix3D_set(matrix3D matrix, int x, int y, int z, float el){
	if(x < matrix.n && y < matrix.m && z < matrix.k){
		matrix.data[x + matrix.n*(y+matrix.k+z)] = el;
	} else {
		//we could also resize here but resizing is really expensive in this case
		printf("out of bounds");
	}
}

matrix3D matrix3D_resize(matrix3D matrix, int n, int m, int k){
	matrix3D new = {n,m,k};
	new.data = (float*) malloc(sizeof(float)*(matrix.n+n)*(matrix.m+m) * (matrix.k+k));
	
	int x,y,z;
	for (z = 0; z < matrix.k; z++){
		for (y = 0; y < matrix.m; y++){
			for (x = 0; x < matrix.n; x++){
				matrix3D_set(new,x,y,z,matrix3D_get(matrix,x,y,z));
			}
		}
	}
	return new;
}

void matrix3D_free(matrix3D matrix){
	free(matrix.data);
}
