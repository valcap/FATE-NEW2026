#!/bin/bash

############################################################################
# Usage
############################################################################
if [ $# -ne 3 ]; then
  echo 'Not enough/too many arguments'
  echo "Usage: $0 env_file FCST_DAY FCST_LEN"
  echo "Example: $0 /home/report/NEW2026/SCRIPTS/fate-report-astro.env [night || day] 1"
  echo ""
  exit 1
else
  envfile=$1
  FCST_DAY=$2
  FCST_DAY_SHORT=`echo $FCST_DAY | cut -c1-3`
  FCST_LEN=$3
fi

# Source of env file
if [ -e $envfile ]; then
  source $envfile
else 
  echo "ops $envfile does not exist in "`pwd`; exit 1
fi

# Source of functions
if [ -e $funcfile ]; then
  source $funcfile
else
  echo "ops $funcfile does not exist in "`pwd`; exit 1
fi

# Start
notice "Start of "`basename $0`

#########################################
## Working on ws
#
prefix='ws'
get_var_attr "$prefix"
# now you got these variables:
# prefixUC
# descri
# unitof
# suffix
#
##
#########################################

#########################################
## Check directories 
#
if [ ! -d $DATA_ROOT_DIR ]; then
  echo "ops $DATA_ROOT_DIR is missing or is not a directory"; exit 1
fi
if [ ! -d $FIGS_ROOT_DIR ]; then
  echo "ops $FIGS_ROOT_DIR is missing or is not a directory"; exit 1
fi
if [ ! -d $PROG_ROOT_DIR ]; then
  echo "ops $PROG_ROOT_DIR is missing or is not a directory"; exit 1
fi
#
## End of check directories
#########################################

#########################################
## Check working directory 
#
WRKDIR=$WRKDIR'/'${FCST_DAY}${FCST_LEN}
if [ ! -d $WRKDIR ]; then
  mkdir -p $WRKDIR
fi
#
## End of check directories
#########################################

#########################################
## Check whether day or night 
#
IS_DAY='FALSE'
if [ ${FCST_DAY} == 'day' ]; then
  IS_DAY='TRUE'
fi
#
## End of check directories
#########################################

##################################################################################
##################################################################################
#                                   BEFORE and AFTER data 
##################################################################################
##################################################################################

#########################################
## Compute graphics and statistics
#
notice "Creating figures and calculating skills for BEFAFT $prefix ($descri)"
cd $PROG_ROOT_DIR
rm -f $WRKDIR/${skills_file}_BEFAFT_${prefix}

JOB=$prefix'_mnh_ar_hit_def_stan_resamp'
if [ ! -e ${JOB}.f90 ]; then
  echo "ops ${JOB}.f90 is missing"; exit 1
fi

FILE_LIST="list_"$prefixUC".txt"
ln -svf /TERASTARMET/FATE_DOCUMENTATION/MONTHLY_REPORT_ELENA_TO_VALERIO_UPGRADE_FATE_2025_07/PROG/TEST_AR/AR_10min/night1/list_WS_${GG}_${HH}_2024_aug.txt ./$FILE_LIST
if [ ! -e $FILE_LIST ]; then
  echo "ops $FILE_LIST is missing in "`pwd`; exit 1
fi
NbNights=`wc -l $FILE_LIST | cut -d ' ' -f 1`

ROOT=$DATA_ROOT_DIR"/${prefixUC}_TREATED/"
if [ ! -d $ROOT ]; then
  echo "ops $ROOT is missing or is not a directory"; exit 1
fi
STARTIN="${prefixUC}_ARevol_"${HH}_${FCST_DAY}_
TAIL=".dat"
LIMIT=0.     # limite inferiore da usarsi quando si vuole studiare lo scattering plot di WS sopra una certa soglia.
             # Se si vuole considerare tutto il sample mettere LIMIT=0.
MAXWS=999.   # put 999. if one wants to consider the whole values without filtering
             # ATT: use the option 999 if you wish to calculate the contingency tables

rm -f ${JOB}.exe
test -f out_scatter_for_python_bef.dat && rm -f out_scatter_for_python_bef.dat
test -f out_scatter_for_python_aft.dat && rm -f out_scatter_for_python_aft.dat
# Compile f90 file
gfortran -Wall -fbounds-check -o ${JOB}.exe ${JOB}.f90 -I$NUMREC_DIR -I$LIBPERSO_DIR/mod -L$LIBPERSO_DIR -L$NUMREC_DIR -J$NUMREC_DIR -lpgplot -lpng -lz -lpers -lnumrec > /dev/null 2>&1
if [ ! -e ${JOB}.exe ]; then
  echo "ops problem in compiling ${JOB}.f90"; exit 1
fi

subnotice "Running F90 program"
# Run f90 file
#./${JOB}.exe<<EOF > $WRKDIR/${skills_file}_BEFAFT_${prefix}
./${JOB}.exe<<EOF 
${GG}
${HH}
${NbNights}
${STARTMINUTE}
${ENDMINUTE}
${LIMIT}
${MAXWS}
"${ROOT}"
"${TAIL}"
"${STARTIN}"
'$FILE_LIST'
'$FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_stan_resamp.ps/cps'
'$FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_stan_resamp.ps/cps'
EOF
rm -f ${JOB}.exe
rm -f out_scatter_for_python_bef.dat out_scatter_for_python_aft.dat
#
## End of computing graphics and statistics for BEFORE and AFTER data
#########################################
exit
#########################################
## check a file named tmpfile_NAME-OF-THE-VARIABLE, which is expected in $PROG_ROOT_DIR
#
if [ ! -e $WRKDIR/${skills_file}_BEFAFT_${prefix} ]; then
  error "$WRKDIR/${skills_file}_BEFAFT_${prefix} not produced"
fi
#
##
#########################################

#########################################
## check .ps files (see naming below), which are expected in $FIGS_ROOT_DIR
#
if [ ! -e $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}.ps ]; then
  error "ops $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}.ps not produced"
else
  rm -f $FIGS_ROOT_DIR/pippo.pdf
  ps2pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}.ps $FIGS_ROOT_DIR/pippo.pdf
  pdfcrop $FIGS_ROOT_DIR/pippo.pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}.pdf > /dev/null 2>&1
fi
if [ ! -e $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}.ps ]; then
  error "ops $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}.ps not produced"
else
  ps2pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}.ps $FIGS_ROOT_DIR/pippo.pdf
  pdfcrop $FIGS_ROOT_DIR/pippo.pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}.pdf > /dev/null 2>&1
fi
rm -f $FIGS_ROOT_DIR/pippo.pdf
if [ ! -d $FIGS_ROOT_DIR/$FCST_DAY$FCST_LEN ]; then
  mkdir -p $FIGS_ROOT_DIR/$FCST_DAY$FCST_LEN
fi
mv $FIGS_ROOT_DIR/*.pdf $FIGS_ROOT_DIR/$FCST_DAY$FCST_LEN
#
##
#########################################

#########################################
## Compute statistics
## FOR LAST MONTH ONLY !!!!!!!!!!!!!!!!!
#
notice "Creating figures and calculating skills for BEFAFT $prefix ($descri) FOR LAST MONTH ONLY"
cd $PROG_ROOT_DIR
rm -f $WRKDIR/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}

JOB=$prefix'_mnh_ar_hit_def_stan'
if [ ! -e ${JOB}.f90 ]; then
  echo "ops ${JOB}.f90 is missing"; exit 1
fi
IDELTA=10
FILE_LIST="list_"$prefixUC"_${skills_file_lastmonth}.txt"
if [ ! -e $FILE_LIST ]; then
  echo "ops $FILE_LIST is missing in "`pwd`; exit 1
fi
NbNights=`wc -l $FILE_LIST | cut -d ' ' -f 1`
ROOT=$DATA_ROOT_DIR"/${prefixUC}_TREATED/"
if [ ! -d $ROOT ]; then
  echo "ops $ROOT is missing or is not a directory"; exit 1
fi
STARTIN="${prefixUC}_ARevol_"
TAIL=".dat"
LIMIT=0.     # limite inferiore da usarsi quando si vuole studiare lo scattering plot di WS sopra una certa soglia.
             # Se si vuole considerare tutto il sample mettere LIMIT=0.
MAXWS=999.   # put 999. if one wants to consider the whole values without filtering
             # ATT: use the option 999 if you wish to calculate the contingency tables

rm -f ${JOB}.exe
test -f out_scatter_for_python_bef.dat && rm -f out_scatter_for_python_bef.dat
test -f out_scatter_for_python_aft.dat && rm -f out_scatter_for_python_aft.dat
# Compile f90 file
gfortran -Wall -fbounds-check -o ${JOB}.exe ${JOB}.f90 -I$NUMREC_DIR -I$LIBPERSO_DIR/mod -L$LIBPERSO_DIR -L$NUMREC_DIR -J$NUMREC_DIR -lpgplot -lpng -lz -lpers -lnumrec > /dev/null 2>&1
if [ ! -e ${JOB}.exe ]; then
  echo "ops problem in compiling ${JOB}.f90"; exit 1
fi

subnotice "Running F90 program"
# Run f90 file
./${JOB}.exe<<EOF > $WRKDIR/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}
${FCST_DAY_SHORT}${FCST_LEN}
${HH}
${IDELTA}
${NbNights}
${STARTMINUTE}
${ENDMINUTE}
${LIMIT}
"${ROOT}"
"${TAIL}"
"${STARTIN}"
'$FILE_LIST'
'$FIGS_ROOT_DIR/temp1.ps/cps'
'$FIGS_ROOT_DIR/temp2.ps/cps'
FALSE
$IS_DAY
EOF
rm -f ${JOB}.exe
rm -f out_scatter_for_python_bef.dat out_scatter_for_python_aft.dat
rm -f $FIGS_ROOT_DIR/temp1.ps $FIGS_ROOT_DIR/temp2.ps

#########################################
## check a file named tmpfile_NAME-OF-THE-VARIABLE, which is expected in $PROG_ROOT_DIR
#
if [ ! -e $WRKDIR/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth} ]; then
  error "$WRKDIR/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth} not produced"
fi
#
##
#########################################

#########################################
## Compute graphics and statistics and FIXED THRESHOLDS
#
notice "Creating figures and calculating skills for BEFAFT $prefix ($descri) and FIXED THRESHOLDS"
cd $PROG_ROOT_DIR
rm -f $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES

JOB=$prefix'_mnh_ar_hit_def_stan'
if [ ! -e ${JOB}.f90 ]; then
  echo "ops ${JOB}.f90 is missing"; exit 1
fi
IDELTA=10
FILE_LIST="list_"$prefixUC".txt"
if [ ! -e $FILE_LIST ]; then
  echo "ops $FILE_LIST is missing in "`pwd`; exit 1
fi
NbNights=`wc -l $FILE_LIST | cut -d ' ' -f 1`
ROOT=$DATA_ROOT_DIR"/${prefixUC}_TREATED/"
if [ ! -d $ROOT ]; then
  echo "ops $ROOT is missing or is not a directory"; exit 1
fi
STARTIN="${prefixUC}_ARevol_"
TAIL=".dat"
LIMIT=0.     # limite inferiore da usarsi quando si vuole studiare lo scattering plot di WS sopra una certa soglia.
             # Se si vuole considerare tutto il sample mettere LIMIT=0.
MAXWS=999.   # put 999. if one wants to consider the whole values without filtering
             # ATT: use the option 999 if you wish to calculate the contingency tables

rm -f ${JOB}.exe
test -f out_scatter_for_python_bef.dat && rm -f out_scatter_for_python_bef.dat
test -f out_scatter_for_python_aft.dat && rm -f out_scatter_for_python_aft.dat
# Compile f90 file
gfortran -Wall -fbounds-check -o ${JOB}.exe ${JOB}.f90 -I$NUMREC_DIR -I$LIBPERSO_DIR/mod -L$LIBPERSO_DIR -L$NUMREC_DIR -J$NUMREC_DIR -lpgplot -lpng -lz -lpers -lnumrec > /dev/null 2>&1
if [ ! -e ${JOB}.exe ]; then
  echo "ops problem in compiling ${JOB}.f90"; exit 1
fi

subnotice "Running F90 program"
# Run f90 file
./${JOB}.exe<<EOF > $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES
${FCST_DAY_SHORT}${FCST_LEN}
${HH}
${IDELTA}
${NbNights}
${STARTMINUTE}
${ENDMINUTE}
${LIMIT}
"${ROOT}"
"${TAIL}"
"${STARTIN}"
'$FILE_LIST'
'$FIGS_ROOT_DIR/tmp1.ps/cps'
'$FIGS_ROOT_DIR/tmp2.ps/cps'
TRUE
$IS_DAY
EOF
rm -f ${JOB}.exe
rm -f out_scatter_for_python_bef.dat out_scatter_for_python_aft.dat
rm -f $FIGS_ROOT_DIR/tmp1.ps $FIGS_ROOT_DIR/tmp2.ps

#
## End of computing graphics and statistics for BEFORE and AFTER data
#########################################

#########################################
## check a file named tmpfile_NAME-OF-THE-VARIABLE, which is expected in $PROG_ROOT_DIR
#
if [ ! -e $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES ]; then
  error "$WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES not produced"
fi
#
##
#########################################

#########################################
## check .ps files (see naming below), which are expected in $FIGS_ROOT_DIR
#

##################################################################################
##################################################################################
#                                   PERSISTENCE data 
##################################################################################
##################################################################################

#########################################
## Compute graphics and statistics
#
notice "Creating figures and calculating skills for PERSISTENCE $prefix ($descri)"
cd $PERS_ROOT_DIR
rm -f $WRKDIR/${skills_file}_PER_${prefix} 

JOB=$prefix'_mnh_ar_hit_def_stan'
if [ ! -e ${JOB}.f90 ]; then
  echo "ops ${JOB}.f90 is missing"; exit 1
fi
IDELTA=10
FILE_LIST="list_"$prefixUC".txt"
if [ ! -e $FILE_LIST ]; then
  echo "ops $FILE_LIST is missing in "`pwd`; exit 1
fi
NbNights=`wc -l $FILE_LIST | cut -d ' ' -f 1`
ROOT=$DATA_PERS_DIR"/${prefixUC}_TREATED/"
if [ ! -d $ROOT ]; then
  echo "ops $ROOT is missing or is not a directory"; exit 1
fi
STARTIN="${prefixUC}_PERSIST_"
TAIL=".dat"
LIMIT=0.     # limite inferiore da usarsi quando si vuole studiare lo scattering plot di WS sopra una certa soglia.
             # Se si vuole considerare tutto il sample mettere LIMIT=0.
MAXWS=999.   # put 999. if one wants to consider the whole values without filtering
             # ATT: use the option 999 if you wish to calculate the contingency tables

rm -f ${JOB}.exe
test -f out_scatter_for_python_bef.dat && rm -f out_scatter_for_python_bef.dat
test -f out_scatter_for_python_aft.dat && rm -f out_scatter_for_python_aft.dat
# Compile f90 file
gfortran -Wall -fbounds-check -o ${JOB}.exe ${JOB}.f90 -I$NUMREC_DIR -I$LIBPERSO_DIR/mod -L$LIBPERSO_DIR -L$NUMREC_DIR -J$NUMREC_DIR -lpgplot -lpng -lz -lpers -lnumrec > /dev/null 2>&1
if [ ! -e ${JOB}.exe ]; then
  echo "ops problem in compiling ${JOB}.f90"; exit 1
fi

subnotice "Running F90 program"
# Run f90 file
./${JOB}.exe<<EOF > $WRKDIR/${skills_file}_PER_${prefix}
${GG}
${HH}
${IDELTA}
${NbNights}
${STARTMINUTE}
${ENDMINUTE}
${LIMIT}
"${ROOT}"
"${TAIL}"
"${STARTIN}"
'$FILE_LIST'
'$FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_stan_per.ps/cps'
'$FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_stan_per.ps/cps'
FALSE
EOF
rm -f ${JOB}.exe
rm -f out_scatter_for_python_bef.dat out_scatter_for_python_aft.dat
#
## End of computing graphics and statistics for PERSISTENCE data
#########################################

# check tmpfile file
# a file named tmpfile_NAME-OF-THE-VARIABLE is expected in $PROG_ROOT_DIR
if [ ! -e $WRKDIR/${skills_file}_PER_${prefix} ]; then
  error "$WRKDIR/${skills_file}_PER_${prefix} not produced"
fi
# check .ps files
# .ps files (see naming below) are also expected in $FIGS_ROOT_DIR
if [ ! -e $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}_per.ps ]; then
  error "ops $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}_per.ps not produced"
else
  rm -f $FIGS_ROOT_DIR/pippo.pdf
  ps2pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}_per.ps $FIGS_ROOT_DIR/pippo.pdf
  pdfcrop $FIGS_ROOT_DIR/pippo.pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}_per.pdf  > /dev/null 2>&1
fi
  if [ ! -e $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}_per.ps ]; then
  error "ops $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}_per.ps not produced"
else
  ps2pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}_per.ps $FIGS_ROOT_DIR/pippo.pdf
  pdfcrop $FIGS_ROOT_DIR/pippo.pdf $FIGS_ROOT_DIR/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}_per.pdf  > /dev/null 2>&1
fi
rm -f $FIGS_ROOT_DIR/pippo.pdf
if [ ! -d $FIGS_ROOT_DIR/$FCST_DAY$FCST_LEN ]; then
  mkdir -p $FIGS_ROOT_DIR/$FCST_DAY$FCST_LEN
fi
mv $FIGS_ROOT_DIR/*.pdf $FIGS_ROOT_DIR/$FCST_DAY$FCST_LEN

#########################################
## Compute statistics
## FOR LAST MONTH ONLY !!!!!!!!!!!!!!!!!
#
#########################################
## Compute graphics and statistics
#
notice "Creating figures and calculating skills for PERSISTENCE $prefix ($descri) FOR LAST MONTH ONLY !!!!!!"
cd $PERS_ROOT_DIR
rm -f $WRKDIR/${skills_file}_PER_${prefix}_${skills_file_lastmonth}

JOB=$prefix'_mnh_ar_hit_def_stan'
if [ ! -e ${JOB}.f90 ]; then
  echo "ops ${JOB}.f90 is missing"; exit 1
fi
IDELTA=10
FILE_LIST="list_"$prefixUC"_${skills_file_lastmonth}.txt"
if [ ! -e $FILE_LIST ]; then
  echo "ops $FILE_LIST is missing in "`pwd`; exit 1
fi
NbNights=`wc -l $FILE_LIST | cut -d ' ' -f 1`
ROOT=$DATA_PERS_DIR"/${prefixUC}_TREATED/"
if [ ! -d $ROOT ]; then
  echo "ops $ROOT is missing or is not a directory"; exit 1
fi
STARTIN="${prefixUC}_PERSIST_"
TAIL=".dat"
LIMIT=0.     # limite inferiore da usarsi quando si vuole studiare lo scattering plot di WS sopra una certa soglia.
             # Se si vuole considerare tutto il sample mettere LIMIT=0.
MAXWS=999.   # put 999. if one wants to consider the whole values without filtering
             # ATT: use the option 999 if you wish to calculate the contingency tables

rm -f ${JOB}.exe
test -f out_scatter_for_python_bef.dat && rm -f out_scatter_for_python_bef.dat
test -f out_scatter_for_python_aft.dat && rm -f out_scatter_for_python_aft.dat
# Compile f90 file
gfortran -Wall -fbounds-check -o ${JOB}.exe ${JOB}.f90 -I$NUMREC_DIR -I$LIBPERSO_DIR/mod -L$LIBPERSO_DIR -L$NUMREC_DIR -J$NUMREC_DIR -lpgplot -lpng -lz -lpers -lnumrec > /dev/null 2>&1
if [ ! -e ${JOB}.exe ]; then
  echo "ops problem in compiling ${JOB}.f90"; exit 1
fi

subnotice "Running F90 program"
# Run f90 file
./${JOB}.exe<<EOF > $WRKDIR/${skills_file}_PER_${prefix}_${skills_file_lastmonth}
${GG}
${HH}
${IDELTA}
${NbNights}
${STARTMINUTE}
${ENDMINUTE}
${LIMIT}
"${ROOT}"
"${TAIL}"
"${STARTIN}"
'$FILE_LIST'
'$FIGS_ROOT_DIR/pippo1.ps/cps'
'$FIGS_ROOT_DIR/pippo2.ps/cps'
FALSE
EOF
rm -f ${JOB}.exe
rm -f out_scatter_for_python_bef.dat out_scatter_for_python_aft.dat
rm -f $FIGS_ROOT_DIR/pippo1.ps $FIGS_ROOT_DIR/pippo2.ps
#
## End of computing graphics and statistics for PERSISTENCE data
#########################################

#########################################
## check a file named tmpfile_NAME-OF-THE-VARIABLE, which is expected in $PROG_ROOT_DIR
#
if [ ! -e $WRKDIR/${skills_file}_PER_${prefix}_${skills_file_lastmonth} ]; then
  error "$WRKDIR/${skills_file}_PER_${prefix}_${skills_file_lastmonth} not produced"
fi
#
##
#########################################

##################################################################################
##################################################################################
#                                CREATE LATEX AND PDF FILES
##################################################################################
##################################################################################

#                         #########################################
#                                1. FIGURES
#                         #########################################

#########################################
## Start of figures 
#
notice "Creating latex file with figures"
EPSBEF=$FIGS_ROOT_DIR/${FCST_DAY}${FCST_LEN}/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_BEF_${suffix}.pdf
EPSAFT=$FIGS_ROOT_DIR/${FCST_DAY}${FCST_LEN}/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}.pdf
EPSPER=$FIGS_ROOT_DIR/${FCST_DAY}${FCST_LEN}/${prefix}_sim_mnh_ar_dimm_${STARTMINUTE}_${ENDMINUTE}_AFT_${suffix}_per.pdf
cat << EOF > $WRKDIR/figures_${prefix}.tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{figure}[h!]
\centering
\subfloat[]{\includegraphics[width=.33\linewidth,angle=0]{$EPSBEF}}
\subfloat[]{\includegraphics[width=.33\linewidth,angle=0]{$EPSAFT}}
\subfloat[]{\includegraphics[width=.33\linewidth,angle=0]{$EPSPER}}
\caption{${FCST_DAY}${FCST_LEN} - {$descri} ($unitof): (a) STANDARD CONFIGURATION, (b) WITH AR (1H), (c) PERSISTENCE (1H)}
\label{fig:$prefix:${FCST_DAY}${FCST_LEN}}
\end{figure}
EOF
#
## End of figures 
#########################################

#                         #########################################
#                                2. CONTINGENCY TABLES
#                         #########################################

notice "Extracting skills and creating contingency tables contingency_tableBEF${prefix}.tex and contingency_tableAFT${prefix}.tex"
rm -f $WRKDIR/contingency_tableBEF${prefix}.tex
rm -f $WRKDIR/contingency_tableAFT${prefix}.tex

cd $WRKDIR

# BEFORE STUFF
PERC1=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep BEF | grep PERCENTILES | grep BEF | grep X33_ | awk '{print $5}'`
PERC2=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep BEF | grep PERCENTILES | grep BEF |grep X66_ | awk '{print $5}'`
VAL1=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW1 | awk '{print $5}'`
VAL2=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW1 | awk '{print $6}'`
VAL3=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW1 | awk '{print $7}'`
VAL4=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW2 | awk '{print $5}'`
VAL5=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW2 | awk '{print $6}'`
VAL6=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW2 | awk '{print $7}'`
VAL7=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW3 | awk '{print $5}'`
VAL8=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW3 | awk '{print $6}'`
VAL9=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep ROW3 | awk '{print $7}'`
#SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep NbLines_TOT | awk '{print $3}'`
#SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep 'Hit rate computed on' | head -n 1`
SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep 'Number of points' | head -n 1`
POD1BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD1 | awk '{print $5}'`
POD2BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD2 | awk '{print $5}'`
POD3BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD3 | awk '{print $5}'`
PCBEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep PC | awk '{print $5}'`
EBDBEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep EBD | awk '{print $5}'`
my_nice_caption=${FCST_DAY}${FCST_LEN}' - Contingency table for {'$descri'} ('$unitof') in standard configuration'
cat << EOF >> $WRKDIR/contingency_tableBEF${prefix}.tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{table}[h!]
\begin{center}
\begin{tabular}{llccc}
\hline
{$descri}                                       &                                                    & \multicolumn{3}{c}{Observations}                 \\\\
{$unitof}                                       &                             & $prefix $<$ $PERC1   & $PERC1 $<$ $prefix $<$ $PERC2 & $prefix $>$ $PERC2 \\\\
\hline
\multicolumn{1}{c}{\multirow{3}{*}{Model data}}  & $prefix $<$ $PERC1             & $VAL1                & $VAL2                       & $VAL3              \\\\
                                                 & $PERC1  $<$ $prefix $<$ $PERC2 & $VAL4                & $VAL5                       & $VAL6              \\\\
                                                 & $prefix $>$ $PERC2             & $VAL7                & $VAL8                       & $VAL9              \\\\
\hline 
\multicolumn{5}{l}{$SAMPSIZ PC=$PCBEF\\%; EBD=$EBDBEF\\%; POD1=$POD1BEF\\%; POD2=$POD2BEF\\%; POD3=$POD3BEF\\%}                 \\\\
\end{tabular}
\end{center}
\caption{$my_nice_caption}
\label{tab:contingency${prefix}BEF:${FCST_DAY}${FCST_LEN}}
\end{table}
EOF

# AFTER STUFF
PERC1=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep AFT | grep PERCENTILES | grep AFT | grep X33_ | awk '{print $5}'`
PERC2=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep AFT | grep PERCENTILES | grep AFT |grep X66_ | awk '{print $5}'`
VAL1=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW1 | awk '{print $5}'`
VAL2=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW1 | awk '{print $6}'`
VAL3=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW1 | awk '{print $7}'`
VAL4=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW2 | awk '{print $5}'`
VAL5=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW2 | awk '{print $6}'`
VAL6=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW2 | awk '{print $7}'`
VAL7=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW3 | awk '{print $5}'`
VAL8=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW3 | awk '{print $6}'`
VAL9=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW3 | awk '{print $7}'`
#SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep NbLines_TOT | awk '{print $3}'`
#SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep 'Hit rate computed on' | head -n 1`
SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep 'Number of points' | head -n 1`
POD1AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD1 | awk '{print $5}'`
POD2AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD2 | awk '{print $5}'`
POD3AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD3 | awk '{print $5}'`
PCAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep PC | awk '{print $5}'`
EBDAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep EBD | awk '{print $5}'`
my_nice_caption=${FCST_DAY}${FCST_LEN}' - Contingency table for  '$descri' ('$unitof') processed with AR (1H)'
cat << EOF >> $WRKDIR/contingency_tableAFT${prefix}.tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{table}[h!]
\begin{center}
\begin{tabular}{llccc}
\hline
{$descri}                                       &                                                    & \multicolumn{3}{c}{Observations}                 \\\\
{$unitof}                                       &                             & $prefix $<$ $PERC1   & $PERC1 $<$ $prefix $<$ $PERC2 & $prefix $>$ $PERC2 \\\\
\hline
\multicolumn{1}{c}{\multirow{3}{*}{Model data}}  & $prefix $<$ $PERC1             & $VAL1                & $VAL2                       & $VAL3              \\\\
                                                 & $PERC1  $<$ $prefix $<$ $PERC2 & $VAL4                & $VAL5                       & $VAL6              \\\\
                                                 & $prefix $>$ $PERC2             & $VAL7                & $VAL8                       & $VAL9              \\\\
\hline 
\multicolumn{5}{l}{$SAMPSIZ PC=$PCAFT\\%; EBD=$EBDAFT\\%; POD1=$POD1AFT\\%; POD2=$POD2AFT\\%; POD3=$POD3AFT\\%}                 \\\\
\end{tabular}
\end{center}
\caption{$my_nice_caption}
\label{tab:contingency${prefix}AFT:${FCST_DAY}${FCST_LEN}}
\end{table}
EOF

# AFTER STUFF AND FIXED THRESHOLDS
PERC1='12.0'
PERC2='18.0'
VAL1=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW1 | awk '{print $5}'`
VAL2=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW1 | awk '{print $6}'`
VAL3=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW1 | awk '{print $7}'`
VAL4=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW2 | awk '{print $5}'`
VAL5=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW2 | awk '{print $6}'`
VAL6=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW2 | awk '{print $7}'`
VAL7=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW3 | awk '{print $5}'`
VAL8=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW3 | awk '{print $6}'`
VAL9=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep ROW3 | awk '{print $7}'`
#SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep NbLines_TOT | awk '{print $3}'`
#SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep 'Hit rate computed on' | head -n 1`
SAMPSIZ=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep 'Number of points' | head -n 1`
POD1AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD1 | awk '{print $5}'`
POD2AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD2 | awk '{print $5}'`
POD3AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD3 | awk '{print $5}'`
PCAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep PC | awk '{print $5}'`
EBDAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep EBD | awk '{print $5}'`
my_nice_caption=${FCST_DAY}${FCST_LEN}' - Contingency table for  '$descri' ('$unitof') processed with AR (1H) and thresholds (12.0,18.0) '$unitof
cat << EOF >> $WRKDIR/contingency_tableAFT${prefix}_FT.tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{table}[h!]
\begin{center}
\begin{tabular}{llccc}
\hline
{$descri}                                       &                                                    & \multicolumn{3}{c}{Observations}                 \\\\
{$unitof}                                       &                             & $prefix $<$ $PERC1   & $PERC1 $<$ $prefix $<$ $PERC2 & $prefix $>$ $PERC2 \\\\
\hline
\multicolumn{1}{c}{\multirow{3}{*}{Model data}}  & $prefix $<$ $PERC1             & $VAL1                & $VAL2                       & $VAL3              \\\\
                                                 & $PERC1  $<$ $prefix $<$ $PERC2 & $VAL4                & $VAL5                       & $VAL6              \\\\
                                                 & $prefix $>$ $PERC2             & $VAL7                & $VAL8                       & $VAL9              \\\\
\hline 
\multicolumn{5}{l}{$SAMPSIZ PC=$PCAFT\\%; EBD=$EBDAFT\\%; POD1=$POD1AFT\\%; POD2=$POD2AFT\\%; POD3=$POD3AFT\\%}                 \\\\
\end{tabular}
\end{center}
\caption{$my_nice_caption}
\label{tab:contingency${prefix}AFT:${FCST_DAY}${FCST_LEN}}
\end{table}
EOF


#                         #########################################
#                                3. PODs FROR EACH CLASS
#                         #########################################

notice "Extracting skills and creating PODs table tablePODs${prefix}.tex"
## PODs for BEF and AFT
#
# BEF
POD1BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD1 | awk '{print $5}'`
POD2BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD2 | awk '{print $5}'`
POD3BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD3 | awk '{print $5}'`
PCBEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep PC | awk '{print $5}'`
EBDBEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep BEF | grep EBD | awk '{print $5}'`
# AFT
POD1AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD1 | awk '{print $5}'`
POD2AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD2 | awk '{print $5}'`
POD3AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD3 | awk '{print $5}'`
PCAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep PC | awk '{print $5}'`
EBDAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix} | grep LOGINFO | grep CONTTABLE | grep AFT | grep EBD | awk '{print $5}'`

my_caption=${FCST_DAY}${FCST_LEN}' - PODs for '$descri' ('$unitof')'
cat << EOF > $WRKDIR/tablePODs${prefix}.tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{table}[h!]
\begin{center}
\begin{tabular}{|l|l|l|l|}
\hline
\multicolumn{1}{|c|}{\cellcolor[HTML]{C0C0C0}\textbf{PARAMETER}} & \multicolumn{1}{c|}{\cellcolor[HTML]{C0C0C0}\textbf{STANDARD}} & \multicolumn{1}{c|}{\cellcolor[HTML]{C0C0C0}\textbf{WITH AR (1H)}} \\\\
\hline
\cellcolor[HTML]{C0C0C0}POD1  & $POD1BEF                                & $POD1AFT         \\\\
\cellcolor[HTML]{C0C0C0}POD2  & $POD2BEF                                & $POD2AFT         \\\\
\cellcolor[HTML]{C0C0C0}POD3  & $POD3BEF                                & $POD3AFT         \\\\
\cellcolor[HTML]{C0C0C0}PC    & $PCBEF                                  & $PCAFT           \\\\
\cellcolor[HTML]{C0C0C0}EBD   & $EBDBEF                                 & $EBDAFT          \\\\
\hline
\end{tabular}
\caption{$my_caption}
\label{tab:pod${prefix}:${FCST_DAY}${FCST_LEN}}
\end{center}
\end{table}
EOF

notice "Extracting skills and creating PODs table tablePODs${prefix}.tex and FIXED THRESHOLDS"
## PODs for BEF and AFT
#
# BEF
# questo da modificare il programma in PROG perche' non e' gestito il caso BEF (idem per rh)
POD1BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD1 | awk '{print $5}'`
POD2BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD2 | awk '{print $5}'`
POD3BEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep BEF | grep POD3 | awk '{print $5}'`
PCBEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep BEF | grep PC | awk '{print $5}'`
EBDBEF=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep BEF | grep EBD | awk '{print $5}'`
# AFT
POD1AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD1 | awk '{print $5}'`
POD2AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD2 | awk '{print $5}'`
POD3AFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep POD3 | awk '{print $5}'`
PCAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep PC | awk '{print $5}'`
EBDAFT=`cat $WRKDIR/${skills_file}_BEFAFT_${prefix}_FIXTHRES | grep LOGINFO | grep CONTTABLE | grep AFT | grep EBD | awk '{print $5}'`

my_caption=${FCST_DAY}${FCST_LEN}' - PODs for '$descri' ('$unitof') and thresholds (12.0,18.0) '$unitof

cat << EOF > $WRKDIR/tablePODs${prefix}_FT.tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{table}[h!]
\begin{center}
\begin{tabular}{|l|l|l|l|}
\hline
\multicolumn{1}{|c|}{\cellcolor[HTML]{C0C0C0}\textbf{PARAMETER}} & \multicolumn{1}{c|}{\cellcolor[HTML]{C0C0C0}\textbf{STANDARD}} & \multicolumn{1}{c|}{\cellcolor[HTML]{C0C0C0}\textbf{WITH AR (1H)}} \\\\
\hline
\cellcolor[HTML]{C0C0C0}POD1  & $POD1BEF   & $POD1AFT         \\\\
\cellcolor[HTML]{C0C0C0}POD2  & $POD2BEF   & $POD2AFT         \\\\
\cellcolor[HTML]{C0C0C0}POD3  & $POD3BEF   & $POD3AFT         \\\\
\cellcolor[HTML]{C0C0C0}PC    & $PCBEF     & $PCAFT           \\\\
\cellcolor[HTML]{C0C0C0}EBD   & $EBDBEF    & $EBDAFT          \\\\
\hline
\end{tabular}
\caption{$my_caption}
\label{tab:pod${prefix}FT:${FCST_DAY}${FCST_LEN}}
\end{center}
\end{table}
EOF

####################################################
notice "End of "`basename $0`
exit 0
####################################################

