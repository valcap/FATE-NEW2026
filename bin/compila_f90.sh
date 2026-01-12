#!/bin/bash

if [ -z $1 ]; then
  echo "ops input file not provided"
  echo "usage: $0 FILE_F90"
  exit 1
fi
if [ ! -e $1 ]; then
  echo "ops input file is missing"
  echo "usage: $0 FILE_F90"
  exit 1
fi

extension="${1##*.}"
filename="${1%.*}"
echo  "${extension}" "${filename}"
if [ "${extension}" != "${filename}" ]; then
  echo "ERRORE: IL PARAMETRO PASSATO HA ESTENSIONE ${extension}"
  exit 1
fi
echo "$filename"

gfortran -Wall -fbounds-check -o $1 $1.f90 -I/home/report/bin/NUMREC -I/home/report/bin/LIBPERSO/mod -L/home/report/bin/LIBPERSO -L/home/report/bin/NUMREC -J/home/report/bin/NUMREC -lpgplot -lpng -lz -lpers -lnumrec

#################################################################################
#gfortran -Wall -Wno-unused -fbounds-check -o $1 $1.f90 -I/home/turchi/programmi/LIBPERSO -L/home/turchi/programmi/LIBPERSO -lpgplot -lpng -lz mod_arrays.o mod_cumdist.o mod_arrays_wd.o mod_statistics.o mod_statistics_wd.o mod_cumdist.o
#gfortran -Wall -Wno-unused -fbounds-check -o $1 $1.f90 -I/home/turchi/programmi/LIBPERSO -L/home/turchi/programmi/LIBPERSO -lpgplot -lpng -lz mod_arrays.o mod_arrays_wd.o mod_statistics.o mod_statistics_wd.o mod_cumdist.o
