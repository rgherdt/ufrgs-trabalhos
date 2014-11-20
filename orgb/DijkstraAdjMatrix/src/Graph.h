/*
 * Graph.h
 *
 *  Created on: Nov 19, 2014
 *      Author: mhbackes
 */

#ifndef DIJKSTRAPQ_SRC_GRAPH_H_
#define DIJKSTRAPQ_SRC_GRAPH_H_

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdbool.h>

#include "AdjList.h"

// A structure to represent a graph. A graph is an array of adjacency lists.
// Size of array will be V (number of vertices in graph)
struct Graph
{
    int V;
    int** array;
};

struct Graph* createGraph(int V);

void addEdge(struct Graph* graph, int src, int dest, int weight);

int minDistance(int dist[], bool sptSet[], int V);

void dijkstra(struct Graph* graph, int src);

void printSolution(int dist[], int n);

#endif /* DIJKSTRAPQ_SRC_GRAPH_H_ */
