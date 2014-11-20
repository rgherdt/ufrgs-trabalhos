/*
 * main.c
 *
 *  Created on: Nov 19, 2014
 *      Author: mhbackes
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>

#include "AdjList.h"
#include "Graph.h"
#include "MinHeap.h"

int main(int argc, char** argv) {
	// create the graph given in above fugure
	int V = 1000;
	struct Graph* graph = createGraph(V);
	addEdge(graph, 0, 1, 4);
	addEdge(graph, 0, 7, 8);
	addEdge(graph, 1, 2, 8);
	addEdge(graph, 1, 7, 11);
	addEdge(graph, 2, 3, 7);
	addEdge(graph, 2, 8, 2);
	addEdge(graph, 2, 5, 4);
	addEdge(graph, 3, 4, 9);
	addEdge(graph, 3, 5, 14);
	addEdge(graph, 4, 5, 10);
	addEdge(graph, 5, 6, 2);
	addEdge(graph, 6, 7, 1);
	addEdge(graph, 6, 8, 6);
	addEdge(graph, 7, 8, 7);

	clock_t begin, end;
	double time_spent;

	begin = clock();
	dijkstra(graph, 0);
	end = clock();

	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

	printf("Total Time: %lf", time_spent);

	return 0;
}

