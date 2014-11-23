#!/bin/bash

echo "Configuração 1\n";
cd conf1;
./run_benchmark;
echo "Configuração 2\n";
cd ../conf2;
./run_benchmark;
echo "Configuração 3\n";
cd ../conf3;
./run_benchmark;

echo "Concluído";
