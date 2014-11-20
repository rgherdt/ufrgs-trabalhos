/*
 * AdjList.h
 *
 *  Created on: Nov 19, 2014
 *      Author: mhbackes
 */

#ifndef ADJLIST_H_
#define ADJLIST_H_

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

// A structure to represent a node in adjacency list
struct AdjListNode
{
    int dest;
    int weight;
    struct AdjListNode* next;
};

// A structure to represent an adjacency liat
struct AdjList
{
    struct AdjListNode *head;  // pointer to head node of list
};

// A utility function to create a new adjacency list node
struct AdjListNode* newAdjListNode(int dest, int weight);

#endif /* ADJLIST_H_ */
