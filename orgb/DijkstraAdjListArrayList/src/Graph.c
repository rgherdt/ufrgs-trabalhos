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

	// Create an array of adjacency lists.  Size of array will be V
	graph->array = (struct AdjList*) malloc(V * sizeof(struct AdjList));

	int i;
	// Initialize each adjacency list as empty by making head as NULL
	for (i = 0; i < V; ++i)
		graph->array[i].head = NULL;

	return graph;
}

// Adds an edge to an undirected graph
void addEdge(struct Graph* graph, int src, int dest, int weight) {
	// Add an edge from src to dest.  A new node is added to the adjacency
	// list of src.  The node is added at the begining
	struct AdjListNode* newNode = newAdjListNode(dest, weight);
	newNode->next = graph->array[src].head;
	graph->array[src].head = newNode;

	// Since graph is undirected, add an edge from dest to src also
	newNode = newAdjListNode(src, weight);
	newNode->next = graph->array[dest].head;
	graph->array[dest].head = newNode;
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

// The main function that calulates distances of shortest paths from src to all
// vertices. It is a O(ELogV) function
void dijkstra(struct Graph* graph, int src) {
	int V = graph->V;    // Get the number of vertices in graph
	int dist[V];      // dist values used to pick minimum weight edge in cut

	bool sptSet[V]; // sptSet[i] will true if vertex i is included in shortest
					// path tree or shortest distance from src to i is finalized
	int v;
	// Initialize min heap with all vertices. dist value of all vertices
	for (v = 0; v < V; ++v) {
		dist[v] = INT_MAX;
		sptSet[v] = false;
	}

	dist[src] = 0;

	// In the followin loop, min heap contains all nodes
	// whose shortest distance is not yet finalized.
	int count;
	// Find shortest path for all vertices
	for (count = 0; count < V - 1; count++) {
		// Pick the minimum distance vertex from the set of vertices not
		// yet processed. u is always equal to src in first iteration.
		int u = minDistance(dist, sptSet, graph->V);

		// Mark the picked vertex as processed
		sptSet[u] = true;

		// Traverse through all adjacent vertices of u (the extracted
		// vertex) and update their distance values
		struct AdjListNode* pCrawl = graph->array[u].head;
		while (pCrawl != NULL) {
			int v = pCrawl->dest;

			// If shortest distance to v is not finalized yet, and distance to v
			// through u is less than its previously calculated distance
			if (!sptSet[v] && dist[u] != INT_MAX
					&& pCrawl->weight + dist[u] < dist[v]) {
				dist[v] = dist[u] + pCrawl->weight;

			}
			pCrawl = pCrawl->next;
		}
	}

	// print the calculated shortest distances
	printArr(dist, V);
}

// A utility function used to print the solution
void printArr(int dist[], int n) {
	printf("Vertex   Distance from Source\n");
	int i;
	for (i = 0; i < n; ++i)
		printf("%d \t\t %d\n", i, dist[i]);
}
