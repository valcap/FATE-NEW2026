#!/bin/bash

############################################################################
# Usage
############################################################################
if [ $# -ne 3 ]; then
  echo 'Not enough/too many arguments'
  echo "Usage: $0 env_file FCST_DAY FCST_LEN"
  echo "Example: $0 $HOME/SCRIPTS/fate-report.env [night || day] [1 || 2 || 3]"
  echo ""
  exit 1
else
  envfile=$1
  FCST_DAY=$2
  FCST_DAY_SHORT=`echo $FCST_DAY | cut -c1-3`
  FCST_LEN=$3
fi
#echo $envfile $FCST_DAY $FCST_DAY_SHORT $FCST_LEN

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

#################################################################
# START OF STEP 2
# Use the outputs of the program by Elena to compile the latex file
notice "Start of "`basename $0`

#################################################################
# HOUSEKEEPING
for fff in table_tmpl_BEF-${FCST_DAY}.tex table_tmpl_AFT-${FCST_DAY}.tex table_tmpl_AFT-LASTM-${FCST_DAY}.tex
do
  if [ ! -e $SCRDIR/TMPL_LATEX/$fff ]; then
    echo "ops cannot find $fff"; exit 1;
  fi
done
cp $SCRDIR/TMPL_LATEX/table_tmpl_BEF-${FCST_DAY}.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_BEF.tex
if [ ${FCST_LEN} -eq 1 ]; then
  cp $SCRDIR/TMPL_LATEX/table_tmpl_AFT-${FCST_DAY}.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT.tex
  cp $SCRDIR/TMPL_LATEX/table_tmpl_AFT-LASTM-${FCST_DAY}.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT-LASTM.tex
fi

#################################################################
#                     BEFORE DATA (i.e. standard))
#                     GREP SKILLS FOR EACH VARIABLE
#################################################################

# Loop over the climatic variables
for prefix in ws wd rh pwv
do
  get_var_attr "$prefix"
  # BEFAFT DATA
  FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}
  BIAS=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep BEF | grep BIAS | cut -d '=' -f2`
  RMSE=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep BEF | grep RMSE | cut -d '=' -f2`
  SD=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep BEF | grep SIGMA | cut -d '=' -f2`
  # LAST MONTH DATA
  FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}
  BIAS_LAST=`cat $FILE_SKILLS | grep LOGINFO | grep BEF | grep BIAS | cut -d '=' -f2`
  RMSE_LAST=`cat $FILE_SKILLS | grep LOGINFO | grep BEF | grep RMSE | cut -d '=' -f2`
  SD_LAST=`cat $FILE_SKILLS | grep LOGINFO | grep BEF | grep SIGMA | cut -d '=' -f2`
  my_caption=${FCST_DAY}${FCST_LEN}' - Statistics for variables in standard configuration: incremental month (i.e., since the begining of service) \\textit\{vs\} last month ('$LASTMONTHSTRING'-'$LASTYEARSTRING')'
  my_label='tab:'${FCST_DAY}${FCST_LEN}':statBEF'
  if [ ${FCST_DAY} == "day" ]; then
    cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_BEF.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                     sed -e "s!${prefixUC}RMSE!$RMSE!"            | \
                                     sed -e "s!${prefixUC}SD!$SD!"                | \
                                     sed -e "s!${prefixUC}biasLM!$BIAS_LAST!"     | \
                                     sed -e "s!${prefixUC}rmseLM!$RMSE_LAST!"     | \
                                     sed -e "s!${prefixUC}sdLM!$SD_LAST!"         | \
                                     sed -e "s!SEEBIAS!-!"|sed -e "s!SEERMSE!-!"|sed -e "s!SEESD!-!"|
                                     sed -e "s!TAUBIAS!-!"|sed -e "s!TAURMSE!-!"|sed -e "s!TAUSD!-!"|
                                     sed -e "s!GLFBIAS!-!"|sed -e "s!GLFRMSE!-!"|sed -e "s!GLFSD!-!"|
                                     sed -e "s!SEEbiasLM!-!"|sed -e "s!SEErmseLM!-!"|sed -e "s!SEEsdLM!-!"|
                                     sed -e "s!TAUbiasLM!-!"|sed -e "s!TAUrmseLM!-!"|sed -e "s!TAUsdLM!-!"|
                                     sed -e "s!GLFbiasLM!-!"|sed -e "s!GLFrmseLM!-!"|sed -e "s!GLFsdLM!-!"|
                                     sed -e "s!MYLABEL!$my_label!"   |
                                     sed -e "s!TABCAPTION!$my_caption!"   \
                                     > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
  else
    cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_BEF.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                     sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                     sed -e "s!${prefixUC}SD!$SD!"        | \
                                     sed -e "s!${prefixUC}biasLM!$BIAS_LAST!"        | \
                                     sed -e "s!${prefixUC}rmseLM!$RMSE_LAST!"        | \
                                     sed -e "s!${prefixUC}sdLM!$SD_LAST!"        | \
                                     sed -e "s!MYLABEL!$my_label!"   |
                                     sed -e "s!TABCAPTION!$my_caption!"   \
                                     > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
  fi
  mv $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_BEF.tex 
done

# Astro-climatic variable
if [ ${FCST_DAY} == "night" ]; then
  for prefix in see tau glf
  do
    get_var_attr "$prefix"
    case "$prefix" in
    see)
      FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_0.24
     ;;
    tau)
      FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_1.22
      ;;
    glf)
      FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_0.14
      ;;
    *) echo "Lo sai chi ti saluta?"
       exit 1
       ;;
    esac
    BIAS=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep BEF | grep BIAS | cut -d '=' -f2`
    RMSE=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep BEF | grep RMSE | cut -d '=' -f2`
    SD=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep BEF | grep SIGMA | cut -d '=' -f2`
    # LAST MONTH DATA
    FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}
    BIAS_LAST=`cat $FILE_SKILLS | grep LOGINFO | grep BEF | grep BIAS | cut -d '=' -f2`
    RMSE_LAST=`cat $FILE_SKILLS | grep LOGINFO | grep BEF | grep RMSE | cut -d '=' -f2`
    SD_LAST=`cat $FILE_SKILLS | grep LOGINFO | grep BEF | grep SIGMA | cut -d '=' -f2`
    cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_BEF.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                       sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                       sed -e "s!${prefixUC}SD!$SD!"        | \
                                       sed -e "s!${prefixUC}biasLM!$BIAS_LAST!"        | \
                                       sed -e "s!${prefixUC}rmseLM!$RMSE_LAST!"        | \
                                       sed -e "s!${prefixUC}sdLM!$SD_LAST!"        | \
                                       sed -e "s!MYLABEL!$my_label!"   |
                                       sed -e "s!TABCAPTION!$my_caption!"   \
                                       > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
    mv $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_BEF.tex
  done
fi

#################################################################
#                     AFTER DATA (i.e. processed with AR)
#                     GREP SKILLS FOR EACH VARIABLE
#                     >>>>>>>>INCREMENTAL MONTH<<<<<<<<
#################################################################

if [ ${FCST_LEN} -eq "1" ]; then
# Loop over the climatic variables
for prefix in ws wd rh pwv
do
  get_var_attr "$prefix"
  # BEFAFT DATA
  FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}
  BIAS=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep BIAS | cut -d '=' -f2`
  RMSE=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep RMSE | cut -d '=' -f2`
  SD=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep SIGMA | cut -d '=' -f2`
  # PERSISTENCE DATA
  FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}
  BIAS_PERS=`cat $FILE_SKILLS | grep LOGINFO | grep AFT | grep BIAS | cut -d '=' -f2`
  RMSE_PERS=`cat $FILE_SKILLS | grep LOGINFO | grep AFT | grep RMSE | cut -d '=' -f2`
  SD_PERS=`cat $FILE_SKILLS | grep LOGINFO | grep AFT | grep SIGMA | cut -d '=' -f2`
  my_caption=${FCST_DAY}${FCST_LEN}' - Incremental month (i.e., since the beginning of service): statistics for variables with AR (1H) \\textit\{vs\} persistence (1H)'
  my_label='tab:'${FCST_DAY}${FCST_LEN}':statAFT:IM'
  if [ ${FCST_DAY} == "day" ]; then
  cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                     sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                     sed -e "s!${prefixUC}SD!$SD!"        | \
                                     sed -e "s!${prefixUC}BIASPER!$BIAS_PERS!"        | \
                                     sed -e "s!${prefixUC}RMSEPER!$RMSE_PERS!"        | \
                                     sed -e "s!${prefixUC}SDPER!$SD_PERS!"        | \
                                     sed -e "s!SEEBIAS!-!"|sed -e "s!SEERMSE!-!"|sed -e "s!SEESD!-!"|
                                     sed -e "s!TAUBIAS!-!"|sed -e "s!TAURMSE!-!"|sed -e "s!TAUSD!-!"|
                                     sed -e "s!GLFBIAS!-!"|sed -e "s!GLFRMSE!-!"|sed -e "s!GLFSD!-!"|
                                     sed -e "s!SEERMSEPER!-!"|sed -e "s!SEESDPER!-!"|
                                     sed -e "s!TAURMSEPER!-!"|sed -e "s!TAUSDPER!-!"|
                                     sed -e "s!GLFRMSEPER!-!"|sed -e "s!GLFSDPER!-!"|
                                     sed -e "s!MYLABEL!$my_label!"   |
                                     sed -e "s!TABCAPTION!$my_caption!"   \
                                     > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
  else
  cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                     sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                     sed -e "s!${prefixUC}SD!$SD!"        | \
                                     sed -e "s!${prefixUC}BIASPER!$BIAS_PERS!"        | \
                                     sed -e "s!${prefixUC}RMSEPER!$RMSE_PERS!"        | \
                                     sed -e "s!${prefixUC}SDPER!$SD_PERS!"        | \
                                     sed -e "s!MYLABEL!$my_label!"   |
                                     sed -e "s!TABCAPTION!$my_caption!"   \
                                     > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
  fi
  mv $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT.tex
done

# Loop over Astro-climatic variables
if [ ${FCST_DAY} == "night" ]; then
  for prefix in see tau glf
  do
  ############ ATT PER LA PERSISTENZA 
    get_var_attr "$prefix"
    case "$prefix" in
    see)
      FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_0.24
      FILE_SKILLS_PER=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}_0.24
     ;;
    tau)
      FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_1.22
      FILE_SKILLS_PER=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}_1.22
      ;;
    glf)
      FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_0.14
      FILE_SKILLS_PER=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}_0.14
      ;;
    *) echo "Lo sai chi ti saluta?"
       exit 1
       ;;
    esac
    BIAS=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep BIAS | cut -d '=' -f2`
    RMSE=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep RMSE | cut -d '=' -f2`
    SD=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep SIGMA | cut -d '=' -f2`
    # PERSISTENCE DATA
    BIAS_PERS=`cat $FILE_SKILLS_PER | grep LOGINFO | grep AFT | grep BIAS | cut -d '=' -f2`
    RMSE_PERS=`cat $FILE_SKILLS_PER | grep LOGINFO | grep AFT | grep RMSE | cut -d '=' -f2`
    SD_PERS=`cat $FILE_SKILLS_PER | grep LOGINFO | grep AFT | grep SIGMA | cut -d '=' -f2`
    cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                       sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                       sed -e "s!${prefixUC}SD!$SD!"        | \
                                       sed -e "s!${prefixUC}RMSEPER!$RMSE_PERS!"        | \
                                       sed -e "s!${prefixUC}SDPER!$SD_PERS!"        | \
                                       sed -e "s!MYLABEL!$my_label!"   |
                                       sed -e "s!TABCAPTION!$my_caption!"   \
                                       > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
    mv $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT.tex
  done
fi

#################################################################
#                     AFTER DATA (i.e. processed with AR)
#                     GREP SKILLS FOR EACH VARIABLE
#                     >>>>>>>>LAST MONTH<<<<<<<<
#                     TABLE 2 (as of August 2023)
#################################################################

# Loop over the climatic variables
for prefix in ws wd rh pwv
do
  get_var_attr "$prefix"
  # BEFAFT DATA
  FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}
  BIAS=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep BIAS | cut -d '=' -f2`
  RMSE=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep RMSE | cut -d '=' -f2`
  SD=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep SIGMA | cut -d '=' -f2`
  # PERSISTENCE DATA
  FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}_${skills_file_lastmonth}
  BIAS_PERS=`cat $FILE_SKILLS | grep LOGINFO | grep AFT | grep BIAS | cut -d '=' -f2`
  RMSE_PERS=`cat $FILE_SKILLS | grep LOGINFO | grep AFT | grep RMSE | cut -d '=' -f2`
  SD_PERS=`cat $FILE_SKILLS | grep LOGINFO | grep AFT | grep SIGMA | cut -d '=' -f2`
  my_caption=${FCST_DAY}${FCST_LEN}' - Last month ('$LASTMONTHSTRING'-'$LASTYEARSTRING'): statistics for variables with AR (1H) \\textit\{vs\} persistence (1H)'
  my_label='tab:'${FCST_DAY}${FCST_LEN}':statAFT:LM'
  if [ ${FCST_DAY} == "day" ]; then
  cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT-LASTM.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                     sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                     sed -e "s!${prefixUC}SD!$SD!"        | \
                                     sed -e "s!${prefixUC}BIASPER!$BIAS_PERS!"        | \
                                     sed -e "s!${prefixUC}RMSEPER!$RMSE_PERS!"        | \
                                     sed -e "s!${prefixUC}SDPER!$SD_PERS!"        | \
                                     sed -e "s!SEEBIAS!-!"|sed -e "s!SEERMSE!-!"|sed -e "s!SEESD!-!"|
                                     sed -e "s!TAUBIAS!-!"|sed -e "s!TAURMSE!-!"|sed -e "s!TAUSD!-!"|
                                     sed -e "s!GLFBIAS!-!"|sed -e "s!GLFRMSE!-!"|sed -e "s!GLFSD!-!"|
                                     sed -e "s!SEERMSEPER!-!"|sed -e "s!SEESDPER!-!"|
                                     sed -e "s!TAURMSEPER!-!"|sed -e "s!TAUSDPER!-!"|
                                     sed -e "s!GLFRMSEPER!-!"|sed -e "s!GLFSDPER!-!"|
                                     sed -e "s!MYLABEL!$my_label!"   |
                                     sed -e "s!TABCAPTION!$my_caption!"   \
                                     > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
  else
  cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT-LASTM.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                     sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                     sed -e "s!${prefixUC}SD!$SD!"        | \
                                     sed -e "s!${prefixUC}BIASPER!$BIAS_PERS!"        | \
                                     sed -e "s!${prefixUC}RMSEPER!$RMSE_PERS!"        | \
                                     sed -e "s!${prefixUC}SDPER!$SD_PERS!"        | \
                                     sed -e "s!MYLABEL!$my_label!"   |
                                     sed -e "s!TABCAPTION!$my_caption!"   \
                                     > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
  fi
  mv $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT-LASTM.tex
done

# Loop over Astro-climatic variables
if [ ${FCST_DAY} == "night" ]; then
for prefix in see tau glf
do
############ ATT PER LA PERSISTENZA 
  get_var_attr "$prefix"
  case "$prefix" in
  see)
    FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}
    FILE_SKILLS_PER=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}_${skills_file_lastmonth}
   ;;
  tau)
    FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}
    FILE_SKILLS_PER=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}_${skills_file_lastmonth}
    ;;
  glf)
    FILE_SKILLS=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_BEFAFT_${prefix}_${skills_file_lastmonth}
    FILE_SKILLS_PER=$WRKDIR/${FCST_DAY}${FCST_LEN}/${skills_file}_PER_${prefix}_${skills_file_lastmonth}
    ;;
  *) echo "Lo sai chi ti saluta?"
     exit 1
     ;;
  esac
  BIAS=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep BIAS | cut -d '=' -f2`
  RMSE=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep RMSE | cut -d '=' -f2`
  SD=`cat $FILE_SKILLS | grep LOGINFO | grep ${prefixUC} | grep AFT | grep SIGMA | cut -d '=' -f2`
  # PERSISTENCE DATA
  BIAS_PERS=`cat $FILE_SKILLS_PER | grep LOGINFO | grep AFT | grep BIAS | cut -d '=' -f2`
  RMSE_PERS=`cat $FILE_SKILLS_PER | grep LOGINFO | grep AFT | grep RMSE | cut -d '=' -f2`
  SD_PERS=`cat $FILE_SKILLS_PER | grep LOGINFO | grep AFT | grep SIGMA | cut -d '=' -f2`
  cat $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT-LASTM.tex | sed -e "s!${prefixUC}BIAS!$BIAS!"    | \
                                     sed -e "s!${prefixUC}RMSE!$RMSE!"    | \
                                     sed -e "s!${prefixUC}SD!$SD!"        | \
                                     sed -e "s!${prefixUC}RMSEPER!$RMSE_PERS!"        | \
                                     sed -e "s!${prefixUC}SDPER!$SD_PERS!"        | \
                                     sed -e "s!MYLABEL!$my_label!"   |
                                     sed -e "s!TABCAPTION!$my_caption!"   \
                                     > $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex
  mv $WRKDIR/${FCST_DAY}${FCST_LEN}/table_tmpl_TEMP.tex $WRKDIR/${FCST_DAY}${FCST_LEN}/table_skills_AFT-LASTM.tex
done
fi
fi

#################################################################
notice "End of "`basename $0`
exit 0

