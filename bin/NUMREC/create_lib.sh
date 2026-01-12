#!/bin/bash
rm libnumrec.a -f

gfortran -c nrtype.f90
gfortran -c nr.f90
gfortran -c nrutil.f90
gfortran -c chixy.f90
gfortran -c *.f90

ar crs libnumrec.a *.o *.mod
