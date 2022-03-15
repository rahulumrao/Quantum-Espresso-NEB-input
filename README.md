# Quantum-Espresso-NEB-input
FORTRAN program to help create coordinate description of NEB input for Quantum Espresso

Compile using 'gfortran' compiler
gfortran QE_NEB_IMAGE.F90 -o QE_NEB_IMAGE.x

# INPUT
relax.xyz (cartesian coordinates of relax atoms [default]) \
fix.xyz   (cartesian coordinates of fix atoms [default])

# OUTPUT
traj.xyz [default]
