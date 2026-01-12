#!/bin/bash

rm libpers.a -f
rm *.mod *.o -f
test -d mod || mkdir mod
rm mod/* -f

for i in $(ls mod_*.f90|grep -v mod_read_ar); do
  gfortran -Wall -c -fbounds-check $i
done
gfortran -Wall -c -fbounds-check mod_read_ar.f90
gfortran -Wall -c -fbounds-check mod_read_ar_test.f90

mv *.mod mod/

ar crs libpers.a *.o

