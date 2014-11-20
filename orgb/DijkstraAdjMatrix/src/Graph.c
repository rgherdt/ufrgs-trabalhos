/*
 * Graph.c
 *
 *  Created on: Nov 19, 2014
 *      Author: mhbackes
 */

#include "Graph.h"

// A utility function that creates a graph of V vertices
struct Graph* createGraph(int V) {
	struct Graph* graph = (struct Graph*) malloc(sizeof(struct Graph));
	graph->V = V;

	// Create a matrix.
	int* a1 = malloc(V * V * sizeof(int));
	int** array = malloc(V * sizeof(int*));
	int row;
	for (row = 0; row < V; row++)
		array[row] = a1 + row * V;

	int i, j;
	// Initialize 0 matrix
	for (i = 0; i < V; ++i)
		for (j = 0; j < V; j++)
			array[i][j] = 0;

	graph->array = array;

	return graph;
}

// Adds an edge to an undirected graph
void addEdge(struct Graph* graph, int src, int dest, int weight) {
	graph->array[src][dest] = weight;
	graph->array[dest][src] = weight;
}

// A utility function to find the vertex with minimum distance value, from
// the set of vertices not yet included in shortest path tree
int minDistance(int dist[], bool sptSet[], int V) {
	// Initialize min value
	int min = INT_MAX, min_index;
	int v;
	for (v = 0; v < V; v++)
		if (sptSet[v] == false && dist[v] <= min)
			min = dist[v], min_index = v;

	return min_index;
}

// Funtion that implements Dijkstra's single source shortest path algorithm
// for a graph represented using adjacency matrix representation
void dijkstra(struct Graph* graph, int src) {
	int V = graph->V;
	int** array = graph->array;

	int dist[V];     // The output array.  dist[i] will hold the shortest
					 // distance from src to i

	bool sptSet[V]; // sptSet[i] will true if vertex i is included in shortest
					// path tree or shortest distance from src to i is finalized

	// Initialize all distances as INFINITE and stpSet[] as false
	int i;
	for (i = 0; i < V; i++)
		dist[i] = INT_MAX, sptSet[i] = false;

	// Distance of source vertex from itself is always 0
	dist[src] = 0;

	// Find shortest path for all vertices
	int count;
	for (count = 0; count < V - 1; count++) {
		// Pick the minimum distance vertex from the set of vertices not
		// yet processed. u is always equal to src in first iteration.
		int u = minDistance(dist, sptSet, V);

		// Mark the picked vertex as processed
		sptSet[u] = true;

		// Update dist value of the adjacent vertices of the picked vertex.
		int v;
		for (v = 0; v < V; v++)

			// Update dist[v] only if is not in sptSet, there is an edge from
			// u to v, and total weight of path from src to  v through u is
			// smaller than current value of dist[v]
			if (!sptSet[v] && array[u][v] && dist[u] != INT_MAX
					&& dist[u] + array[u][v] < dist[v])
				dist[v] = dist[u] + array[u][v];
	}

	// print the constructed distance array
	printSolution(dist, V);
}

// A utility function used to print the solution
void printSolution(int dist[], int n) {
	printf("Vertex   Distance from Source\n");
	int i;
	for (i = 0; i < n; ++i)
		printf("%d \t\t %d\n", i, dist[i]);
}
