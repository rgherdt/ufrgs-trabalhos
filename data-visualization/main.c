#include "data-serializate.h"
#include "coordinates.h"
#include "error.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <stdlib.h>
#include <string.h>
#include <GL/glut.h>
#endif

#define AXE_LEN 1
#define WIN_HEIGHT 400
#define WIN_WIDTH 600
#define LINE_WIDTH 1.5
#define TICK_SIZE 0.01
#define ZOOMSTEP 0.1
#define MAXDIM 6
#define MAXSTR 64
#define FUNCNUM 1

void init();
void display();
void reshape();
void print_text(int x, int y, char *str);
void print_data();
void mouse_func(int button, int state, int x, int y);
void motion_func(int x, int y);
void keyboard_func(unsigned char key, int x, int y);
void keyboard_special_func(int key, int x, int y);
void draw_axes(int dimension, int var0, int var1, int var2);
void draw_dipstick(int x, int y);
GLfloat function(GLfloat x,GLfloat y);
void draw_surface(GLfloat limit, GLfloat d);
char *labels[MAXDIM] = {"X", "Y", "Z", "L", "M", "N"};


GLfloat (*functions[1])(GLfloat x, GLfloat y) = {function};
int cur_func = 0;
GLfloat zoom = 0.5;
GLfloat horiz_angle = M_PI/4;
GLfloat horiz_delta_angle = 0.0;
GLfloat vert_angle = M_PI/2;
GLfloat vert_delta_angle = 0.0;
GLfloat x_drag_start = 0;
GLfloat y_drag_start = 0;
GLint dimension;
GLboolean is_dragging = 0;
coordinates camera;
scale scales[MAXDIM];
int vars[3] = {0, 1, 2};

int main (int argc, char *argv[])
{
	int i, start_pos;
	int opt;

	while ((opt = getopt(argc, argv, "x:y:z:")) != -1) {
		switch (opt) {
			case 'x':
				vars[0] = atoi(optarg);
				break;
			case 'y':
				vars[1] = atoi(optarg);
				break;
			case 'z':
				vars[2] = atoi(optarg);
				break;
			default:
				printf("Unknown option: %c\n", optopt);
				break;
		}
	}



	/*
	if (argc < 2) {
		eprintf("Usage: %s <arquivo.data>", argv[0]);
	}
	*/

	/*
	 * As we don't have so much time we should restrain to a set of data and
	 * don't make everything generic
	 *
	 */

	// set axes properties in all dimensions, as well as start position

	for (i = 0; i < MAXDIM; i++) {
		scales[i].limit = 1.0;
		scales[i].step  = 0.1;
		scales[i].cur_val = 0.0;
	}
	/* currently used number of dimensions */
	dimension = 6;

	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGB);
	glutInitWindowSize(WIN_WIDTH, WIN_HEIGHT);

	start_pos = floor(WIN_HEIGHT / 4);
	glutInitWindowPosition(start_pos, start_pos);

	glutCreateWindow(argv[0]);
	init();
	GLfloat light_pos[] =  {1, 1, 1, 1};
	GLfloat light_col[] = {1, 1, 1, 1};
	GLfloat ambient_col[] = {0.6,0.6,0.6, 1};
	 
	glEnable(GL_LIGHT0);
	glEnable(GL_LIGHTING);
	glLightfv(GL_LIGHT0, GL_POSITION, light_pos);
	glLightfv(GL_LIGHT0, GL_AMBIENT, ambient_col);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, light_col);
	glShadeModel(GL_SMOOTH);
	glFrontFace(GL_CCW);
    
	glutMotionFunc(motion_func);
	glutMouseFunc(mouse_func);
	glutKeyboardFunc(keyboard_func);
	glutSpecialFunc(keyboard_special_func);
	glutDisplayFunc(display);
	glutReshapeFunc(reshape);
	glutMainLoop();
	exit(0);
}

void init()
{
	glClearColor(0.0, 0.0, 1.0, 0.0);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity();
	/* some math to set camera's start position */
	camera.x = sin(horiz_angle + horiz_delta_angle);
	camera.z = sin(vert_angle + vert_delta_angle);
	camera.y = cos(horiz_angle + horiz_delta_angle);

	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity();
}

void display()
{
	GLfloat function_color[] = {1, 0.45, 0, 0};
	GLfloat axes_color0[] = {1, 0.3, 0.3, 0};
	GLfloat axes_color1[] = {1, 1, 1, 0};
	GLfloat dipstick_color[] = {0, 1, 0};

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(-1.0 - zoom, 1.0 + zoom, -1.0 - zoom, 1.0 + zoom, -5.0, 5.0);
	glMatrixMode(GL_MODELVIEW);

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_DEPTH_TEST);
 
	glLoadIdentity();
	gluLookAt(camera.x,
		  camera.y,
		  camera.z,
		  0.0, 0.0, 0.0, 0.0, 1.0, 0.0); 

	glRotatef(90, 1.0, 0, 0);
	glScalef(1, -1, -1);

	glMaterialfv(GL_FRONT_AND_BACK,
			GL_AMBIENT_AND_DIFFUSE,
			axes_color0);

	draw_axes(0, 0, 1, 2);

	glTranslatef(scales[0].cur_val,
		     scales[1].cur_val,
		     scales[2].cur_val);

	glPushMatrix();
		glScalef(0.5, 0.5, 0.5);

		glMaterialfv(GL_FRONT_AND_BACK,
				GL_AMBIENT_AND_DIFFUSE,
				axes_color1);

		draw_axes(1, 3, 4, 5);

		glMaterialfv(GL_FRONT_AND_BACK,
				GL_AMBIENT_AND_DIFFUSE,
				dipstick_color);

		draw_dipstick(3, 4);
	
		glPushMatrix();
		glMaterialfv(GL_FRONT_AND_BACK,
				GL_AMBIENT_AND_DIFFUSE,
				function_color);

		draw_surface(1, 0.05);
		glPopMatrix();
	glPopMatrix();
	print_data();

	glutSwapBuffers();

}

void draw_surface(GLfloat limit, GLfloat d)
{
	GLfloat x;
	GLfloat y;
	GLfloat fx_y, fxd_y, fx_yd, fxd_yd, fx2d_y, fx2d_yd, fxd_y2d, fx2d_y2d;
	coordinates* normal;
	coordinates* v1;
	coordinates* v2;
	coordinates* v3;
    
	//TODO: specify normal vector for each vertice
	for (y = -limit ; y < limit; y += d){
		for (x = -limit; x < limit; x += d){
			fx_y = (*functions[cur_func])(x, y);
			fxd_y = (*functions[cur_func])(x + d, y);
			fx_yd = (*functions[cur_func])(x, y + d);
			fxd_yd = (*functions[cur_func])(x + d, y + d);
			fxd_y2d = (*functions[cur_func])(x + d, y + 2 * d);
			fx2d_y = (*functions[cur_func])(x + 2 * d, y);
			fx2d_yd = (*functions[cur_func])(x + 2 * d, y + d);
			fx2d_y2d = (*functions[cur_func])(x + 2 * d, y + 2 * d);

			glBegin(GL_QUADS);
			v1 = coordinates_init(x, y, fx_y);
			v2 = coordinates_init(x+d, y, fxd_y);
			v3 = coordinates_init(x+d,y+d, fxd_yd);
			normal = getNormal(*v1, *v2, *v3);
            
			glNormal3f(normal->x, normal->y, normal->z);
			glVertex3f(x, y, fx_y);

			coordinates_free(v1);
			coordinates_free(v2);
			coordinates_free(v3);
			coordinates_free(normal);


			v1 = coordinates_init(x+d, y, fxd_y);
			v2 = coordinates_init(x+2*d,y,fx2d_y);
			v3 = coordinates_init(x+2*d,y+d, fx2d_yd);
			normal = getNormal(*v1, *v2, *v3);
            
			glNormal3f(normal->x, normal->y, normal->z);
			glVertex3f(x+d,y, fxd_y);

			coordinates_free(v1);
			coordinates_free(v2);
			coordinates_free(v3);
			coordinates_free(normal);


			v1 = coordinates_init(x+d, y+d, fxd_yd);
			v2 = coordinates_init(x+2*d,y+d, fx2d_yd);
			v3 = coordinates_init(x+2*d,y+2*d,fx2d_y2d);
			normal = getNormal(*v1, *v2, *v3);
            
			glNormal3f(normal->x, normal->y, normal->z);
			glVertex3f(x+d,y+d,fxd_yd);

			coordinates_free(v1);
			coordinates_free(v2);
			coordinates_free(v3);
			coordinates_free(normal);


			v1 = coordinates_init(x, y+d, fx_yd);
			v2 = coordinates_init(x+d,y+d, fxd_yd);
			v3 = coordinates_init(x+d,y+2*d,fxd_y2d);
			normal = getNormal(*v1, *v2, *v3);
            
            glNormal3f(normal->x, normal->y, normal->z);
			glVertex3f(x,y+d,fx_yd);

			coordinates_free(v1);
			coordinates_free(v2);
			coordinates_free(v3);
			coordinates_free(normal);

			glEnd();
		}
	}
}

/* print vars at right side of graph */
void print_data()
{
	int i;
	char str[MAXSTR];
	GLfloat font_color[] = {1, 1, 1, 0};

	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();
	glTranslatef(0.7, 1.0, 0);
	glMaterialfv(GL_FRONT_AND_BACK,
			GL_AMBIENT_AND_DIFFUSE,
			font_color);

//	glTranslatef(0.5, 0, 0);
	glScalef(0.5, 0.5, 0.5);
	for (i = 0; i < MAXDIM - 1; i++) {
		sprintf(str, "%s: %.1f ", labels[i], scales[i].cur_val);
		glTranslatef(0.0, -0.1, 0);
		print_text(1, -1, str);
	}
	glPopMatrix();
}

/* TODO Function should have 5 or more variables, that's why the graph doesn't
 * get rebuild. 
 */
 
/*
float function(float w0, float w1, float w2, float w3, float w4, float w5){
	float result;
	result = w1 * sinf(w2) + w3 * cosf(4*w4)*sinf(4*w5);
	return result;
}
*/


// only a test, constants should be arguments
/*
float function(GLfloat x, GLfloat y) {
	float result;

	result = (scales[0].cur_val + 0.125) *
		(sinf(M_PI/2 - scales[1].cur_val)) *
		scales[2].cur_val * cosf(4*x)*sinf(4*y);
	return result;
}
*/

float function(GLfloat x, GLfloat y) {
    return 0.25 * 
	   cosf(8 * scales[2].cur_val) *
	   cosf(4 * scales[1].cur_val * x) *
	   sinf(4 * scales[0].cur_val * y);
}

void reshape( int w, int h)
{
	glViewport (0, 0, (GLsizei) w, (GLsizei) h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(-1.0 - zoom, 1.0 + zoom, -1.0 - zoom, 1.0 + zoom, -5.0, 5.0);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

}

void print_text(int x, int y, char *str)
{
	int i;
	
	glPushAttrib(GL_DEPTH_TEST);
	glDisable(GL_DEPTH_TEST);

	glRasterPos2i(x, y);
	for (i = 0; i < strlen(str); i++)
	{
		glutBitmapCharacter(GLUT_BITMAP_9_BY_15, str[i]);
	}

	glPopAttrib();
}


void mouse_func (int button, int state, int x, int y)
{
	switch(button) {
		case (GLUT_LEFT_BUTTON):
			if (state == GLUT_DOWN) {
				is_dragging = 1;
				x_drag_start = x;
				y_drag_start = y;
			} else {
				horiz_angle += horiz_delta_angle;
				vert_angle += vert_delta_angle;
				is_dragging = 0;
			}
			break;
		case (3): // scroll up
			if (state == GLUT_DOWN) {
				zoom -= ZOOMSTEP;
			}
			glutPostRedisplay();
			break;
		case (4): // scroll down
			if (state == GLUT_DOWN) {
				zoom += ZOOMSTEP;
			}
			glutPostRedisplay();
			break;
		default:
			;
	}
}

void motion_func (int x, int y)
{
	horiz_delta_angle = (x - x_drag_start) * 0.005;
	vert_delta_angle = (y - y_drag_start) * 0.008;
	camera.x = cos(horiz_angle + horiz_delta_angle);
	camera.y = sin(vert_angle + vert_delta_angle);
	camera.z = sin(horiz_angle + horiz_delta_angle);
	glutPostRedisplay();
}


void keyboard_func(unsigned char key, int x, int y)
{
	switch(key) {
		case '+':
			zoom -= ZOOMSTEP;
			break;
		case '-':
			zoom += ZOOMSTEP;
			break;
		case '=':
			zoom = 0.0;
			break;
		case 'a':
			scale_dec(&scales[3]);
			break;
		case 'd':
			scale_inc(&scales[3]);
			break;
		case 'w':
			scale_inc(&scales[4]);
			break;
		case 's':
			scale_dec(&scales[4]);
			break;
		default:
			;
	}
	glutPostRedisplay();
}

void keyboard_special_func(int key, int x, int y)
{
	switch(key) {
		case GLUT_KEY_LEFT:
			scale_dec(&scales[0]);
			break;
		case GLUT_KEY_RIGHT:
			scale_inc(&scales[0]);
			break;
		case GLUT_KEY_UP:
			scale_inc(&scales[1]);
			break;
		case GLUT_KEY_DOWN:
			scale_dec(&scales[1]);
			break;
		case GLUT_KEY_PAGE_UP:
			scale_inc(&scales[2]);
			break;
		case GLUT_KEY_PAGE_DOWN:
			scale_dec(&scales[2]);
			break;
		default:
			;
	}

	glutPostRedisplay();
}

void draw_axes(int dimension, int var0, int var1, int var2)
{
	int i, nticks;
	GLfloat step, limit, tick_val;
	glLineWidth(LINE_WIDTH);
	char str[MAXSTR];

	glBegin(GL_LINES);
		glVertex3i(-AXE_LEN,0,0);
		glVertex3i(AXE_LEN,0,0);

		glVertex3i(0,-AXE_LEN,0);
		glVertex3i(0,AXE_LEN,0);

		glVertex3i(0,0,-AXE_LEN);
		glVertex3i(0,0,AXE_LEN);
	glEnd();

	/* draw scale ticks */
	limit = scales[var0].limit;
	step = scales[var0].step;
	nticks = floor(limit / step);
	for (i = -nticks; i <= nticks; i++) {
		tick_val = i * step;
		glBegin(GL_LINES);
			glVertex3f(tick_val, 0, -TICK_SIZE);
			glVertex3f(tick_val, 0, TICK_SIZE);
		glEnd();
		if ((i % 5) == 0) {
			glPushMatrix();
			glTranslatef(tick_val, -0.1, 0);
			sprintf(str, "%2.1f", tick_val);
			print_text(0, 0, str);
			glPopMatrix();
		}
	}

	limit = scales[var1].limit;
	step = scales[var1].step;
	nticks = floor(limit / step);
	for (i = -nticks; i <= nticks; i++) {
		tick_val = i * step;
		glBegin(GL_LINES);
			glVertex3f(0, tick_val, -TICK_SIZE);
			glVertex3f(0, tick_val, TICK_SIZE);
		glEnd();
		if ((i % 5) == 0) {
			glPushMatrix();
			glTranslatef(-0.1, tick_val, 0);
			sprintf(str, "%2.1f", tick_val);
			print_text(0, 0, str);
			glPopMatrix();
		}
	}

	limit = scales[var2].limit;
	step = scales[var2].step;
	nticks = floor(limit / step);
	for (i = -nticks; i <= nticks; i++) {
		tick_val = i * step;
		glBegin(GL_LINES);
			glVertex3f(-TICK_SIZE, 0, tick_val);
			glVertex3f(TICK_SIZE, 0, tick_val);
		glEnd();
		if ((i % 5) == 0) {
			glPushMatrix();
			glTranslatef(-0.1, 0, tick_val);
			sprintf(str, "%2.1f", tick_val);
			print_text(0, 0, str);
			glPopMatrix();
		}
	}


	glColor3f(1.0, 1.0, 1.0);

	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glTranslatef(1.1, 0, 0.1);
	print_text(0, 0, labels[var0]);
	glPopMatrix();

	glPushMatrix();
	glTranslatef(-0.1, 1.1, 0.0);
	print_text(0, 0, labels[var1]);
	glPopMatrix();

	glPushMatrix();
	glTranslatef(0.1, 0.0, 1.1);
	print_text(0, 0, labels[var2]);
	glPopMatrix();

}

void draw_dipstick(int x, int y)
{
	char str[MAXSTR];
	GLfloat val;
	val = function(scales[x].cur_val, scales[y].cur_val);
	sprintf(str, "%.5f", val);

	glLineWidth(LINE_WIDTH);
	glPushMatrix();

	glTranslatef(scales[x].cur_val, scales[y].cur_val, 0);
	glBegin(GL_LINES);
		glVertex3i(0, 0, -AXE_LEN);
		glVertex3i(0, 0, AXE_LEN);
	glEnd();

	glTranslatef(-0.1, 0.0, 1.0);
	print_text(0, 0, str);

	glPopMatrix();

}

