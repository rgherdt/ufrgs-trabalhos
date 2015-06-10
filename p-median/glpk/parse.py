#!/usr/bin/python

import sys

inf = float('inf')

def init(i, j):
	if i == j:
		return 0
	else:
		return inf

ifile = sys.argv[1]
ofile = sys.argv[2]
ifp = open(sys.argv[1])
ofp = open(sys.argv[2], 'w')

line1 = ifp.readline()
line1 = line1.split(' ')
n = int(line1[1])
p = int(line1[3])

# initilize
mat = [[init(i,j) for i in xrange(n)] for j in xrange(n)]
for line in ifp:
	x = line.split(' ')
	mat[int(x[1])-1][int(x[2])-1] = int(x[3])
	mat[int(x[2])-1][int(x[1])-1] = int(x[3])
ifp.close()

# shortest path
for i in range(n):
	for j in range(n):
		if mat[j][i] < inf:
			for k in range(n):
				if mat[i][k] < inf:
					s = mat[j][i] + mat[i][k]
					if s < mat[k][j]:
						mat[j][k] = s
						mat[k][j] = s

# writing to 
ofp.write("data;\n")
ofp.write("param p := %d"%p+";\n")
ofp.write("param n := %d"%n+";\n")
ofp.write("param d : \t")
for i in range(n):
	ofp.write('\t%d'%(i+1))
ofp.write(" :=\n")
for i in range(n):
	ofp.write("\t\t\t%d"%(i+1))
	for j in range(n):
		ofp.write("\t%d"%mat[i][j])
	ofp.write("\n")
ofp.write("\t;\nend;")
ofp.close()
