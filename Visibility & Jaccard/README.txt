
Matrices_horVA = 
Coll, Conf, Red, Supp -> matrices of the simulated time series (imported from R), 
                         each column has one time serie (x,y,z).
                         02, 05, 1, 15, 2, are the 5 scenarios from varying the noise
                         (sd: 0.2, 0.5, 1, 1.5, 2)

On these time series the HORIZONTAL Visibility algorithm has been applied, 
resulting in their respective AD = Adjacency matrix and K= node degree

Matrices_natVA = 
Coll, Conf, Red, Supp -> matrices of the simulated time series (imported from R), 
                         each column has one time serie (x,y,z).
                         02, 05, 1, 15, 2, are the 5 scenarios from varying 
                         the noise (sd: 0.2, 0.5, 1, 1.5, 2)

On these time series the NATURAL Visibility algorithm has been applied, 
resulting in their respective AD = Adjacency matrix and K= node degree

Visibility_code.mlx =
Script with the code where first the horizontal and then the natural 
visibility algorithm have been applied

Jaccard_calculationHOR.mat=
Same workspace as Matrices_horVA.mat +
J and Jp = Jaccard and Partial Jaccard index for each network (time serie)

Jaccard_calculationNAT.mat=
Same workspace as NEW_Matrices_natVA.mat +
J and Jp = Jaccard and Partial Jaccard index for each network (time serie)

Jaccard_calculation =
Script with the code for the calculation of the Jaccard and Partial Jaccard 
index on each graph in which the time series have been tranformed using the
visibility algorithm. First part on the output of the horizontal visibility and 
second part on the output of the natural visibility.

Visibility.m (cpp) = Visibility algorithm as in https://github.com/danielemarinazzo/Visibility