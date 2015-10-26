#ifndef DATA_SERIALIZATE_H
#define DATA_SERIALIZATE_H

// just use float they are both either 32 bit or 64 bit
typedef union element {
	int i;
	float f;
} element;


/* lets do it first for three dimensions should be enough because we won't ever
 * use more then 3 dimension as we can't represent more than 3 dimension in 3d
 * space. If we can still nest more than three times through simply creating
 * more 3D matrices. each 3D matrice will be used for each level of nesting. to
 * represent the actual graph we can use a 2D array which is a 3D matrix with
 * depth of 0.
 */
typedef struct matrix3D {
	int n; //width
	int m; //height
	int k; //depth
	
	float *data;
} matrix3D;

matrix3D matrix3D_init(int n, int m, int k);
float matrix3D_get(matrix3D matrix, int x, int y, int z);
void matrix3D_set(matrix3D matrix, int x, int y, int z, float el);
matrix3D matrix3D_resize(matrix3D matrix, int n, int m, int k);
void matrix3D_free(matrix3D matrix);


/*
 would be sooo much easier doing this generic in c++ :/
 the generic way would be kind of hard and difficult to unserstand
*/

typedef struct matrix {
	int dim_num;
	int *dim_sizes; // array containing size of each dimension

	element *m;	 // pointer to data. It's an uni-dimensional array
					 // representing the whole matrix
} matrix;



element *read_element(char *str);


#endif /* DATA_SERIALIZAT_H */
