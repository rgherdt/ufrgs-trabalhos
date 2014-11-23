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

int main(int argc, char** argv) {
	// create the graph given in above fugure
	FILE *fp;
	int V = 1000;

	int source, target, distance;
	clock_t begin, end;
	double time_spent;
	struct Graph* graph = createGraph(V);

	if (argc != 2) {
		printf("Usage: %s <input_graph>\n", argv[0]);
		exit(1);
	}
	if ((fp = fopen(argv[1], "r")) == NULL) {
		perror("Couldn't open input graph file\n");
	}

	while (fscanf(fp, "%d%d%d", &source, &target, &distance) != EOF) {
		addEdge(graph, source, target, distance);
	}

	begin = clock();
	dijkstra(graph, 0);
	end = clock();

	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

	printf("Total Time: %lf", time_spent);

	return 0;
}

