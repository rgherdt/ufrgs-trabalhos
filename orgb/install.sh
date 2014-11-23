#!/bin/bash

cur_dir=`pwd`

cd "DijkstraAdjListArrayList";
make clean;
make;
cd "../DijkstraAdjListBinHeap";
make clean;
make;
cd "../DijkstraAdjMatrix";
make clean;
make;
cd $cur_dir;
mkdir -p build;
cp "DijkstraAdjListArrayList/build/dk_adjlist_arrlist" build;
cp "DijkstraAdjListBinHeap/build/dk_adjlist_bin_heap" build;
cp "DijkstraAdjMatrix/build/dk_adjmatrix" build;
