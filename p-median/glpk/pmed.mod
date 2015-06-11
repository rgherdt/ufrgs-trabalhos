param n;				# number of nodes
param p;				# number of deposits
set N := 1..n;			# set of nodes
param d {N,N};			# distance

var x {N,N} binary;		# edge between client i and closest deposit j
var k {N} binary;		# position of each deposit

minimize min_distance: sum {i in N, j in N} x[i,j] * d[i,j];

subject to c1 {j in N}:
	sum {i in N} x[i,j] = 1;
	
subject to c2 {i in N}:
	sum {j in N} x[i,j] <= k[i]*(n+1);
	
subject to c3:
	sum {i in N} k[i] = p;

end;
