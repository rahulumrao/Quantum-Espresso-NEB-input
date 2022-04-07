# Quantum-Espresso-NEB-input
FORTRAN program to help create coordinate description of NEB input for Quantum Espresso

Compile using 'gfortran' compiler \
gfortran QE_NEB_IMAGE.F90 -o QE_NEB_IMAGE.x

# INPUT
relax.xyz (cartesian coordinates of relax atoms [default filename]) \
fix.xyz   (cartesian coordinates of fix atoms [default filename])

# OUTPUT
traj.xyz [default]

# RUN
./QE_NEB_IMAGE.x \
ENTER THE OUTPUT FILENAME \
test  \
COMBINED TRAJECTORY WRITTEN IN traj.xyz FILE \
NEB COORDINATES DESCRIPTION WRITTEN IN test FILE 


```
WRITTEN BY
Rahul Verma
vrahul@iitk.ac.in
IIT Kanpur
```
