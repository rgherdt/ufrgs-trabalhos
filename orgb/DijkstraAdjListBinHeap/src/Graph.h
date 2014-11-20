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

#include "AdjList.h"
#include "MinHeap.h"

// A structure to represent a graph. A graph is an array of adjacency lists.
// Size of array will be V (number of vertices in graph)
struct Graph
{
    int V;
    struct AdjList* array;
};

struct Graph* createGraph(int V);

void addEdge(struct Graph* graph, int src, int dest, int weight);

void dijkstra(struct Graph* graph, int src);

void printArr(int dist[], int n);

#endif /* DIJKSTRAPQ_SRC_GRAPH_H_ */
