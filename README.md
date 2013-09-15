This program shows how to use Higher-order functions in Fortran. The aim is to create programs easily parallelizable in a Functional Programming style. It also has OpenMP directives. To run it in Linux type on the command line:
ulimit -s unlimited
export OMP_NUM_THREADS=4
f95 -xopenmp=parallel -o HOF ModuleFL.f95 ModuleHOF.f95 MainHOF.f95
./ HOF

---------

The code is under the GPL license so feel free to use it!

(c) Ricardo Miranda, 2013, mail@ricardomiranda.com.
