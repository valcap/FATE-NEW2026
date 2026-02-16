#!/bin/bash

############################################################################
# Usage
############################################################################
if [ $# -ne 1 ]; then
  echo 'Not enough/too many arguments'
  echo "Usage: $0 env_file"
  echo "Example: $0 $HOME/SCRIPTS/fate-report.env"
  echo ""
  exit 1
else
  envfile=$1
fi

#################################################################
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

notice "Start of "`basename $0`

#################################################################
# HOUSEKEEPING
rm -f `echo $report_tex_file | sed -e "s!tex!!"`*.*
cp $LOGOSDIR/INAF_Arcetri_colore.png $WRKDIR/
cp $LOGOSDIR/logo_lamma.png $WRKDIR/
cp $LOGOSDIR/fate_logo_11def.png $WRKDIR/
cp $fate_sty $WRKDIR/

######################
# NIGHT 1
if [ ! -d $WRKDIR/night1 ]; then
  mkdir $WRKDIR/night1
fi
cd $WRKDIR/night1
# NIGHT 1
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' > night1.tex
echo '\section{NIGHT1: Statistical analysis of forecast performances}\label{sec:night1}' >> night1.tex
cp night1.tex $WRKDIR/
# FIGURES
echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" > night1_figures.tex
echo '\subsection{NIGHT1: Scatter plots (bias, RMSE, SD) for long and short forecast time scale}\label{subsec:night1:fig}' >> night1_figures.tex
echo 'Figures in setion \ref{subsec:night1:fig} are included in the range: \ref{fig:ws:night1}-\ref{fig:glf:night1}.' >> night1_figures.tex
cat figures_ws.tex figures_wd.tex figures_rh.tex figures_pwv.tex figures_see.tex figures_tau.tex figures_glf.tex >> night1_figures.tex 
cp night1_figures.tex $WRKDIR/
# STATISTICS
echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" > night1_statistics.tex
echo '\subsection{NIGHT1: Summarizing values of bias, RMSE, SD for long and short forecast time scales}\label{subsec:night1:stat}' >> night1_statistics.tex
echo 'Tables in section \ref{subsec:night1:stat} are included in the range: \ref{tab:night1:statBEF}-\ref{tab:night1:statAFT:IM}.' >>  night1_statistics.tex
cat table_skills_BEF.tex table_skills_AFT-LASTM.tex table_skills_AFT.tex >> night1_statistics.tex
echo '\clearpage' >> night1_statistics.tex
cp night1_statistics.tex $WRKDIR/
# CONTINGENCY TABLES
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' > night1_contingency.tex
echo '\section{NIGHT1: Contingency tables}\label{sec:night1:cont}' >> night1_contingency.tex
echo 'Tables in section \ref{sec:night1:cont} are included in the range: \ref{tab:contingencywsBEF:night1}-\ref{tab:contingencyglfAFT:night1}.' >> night1_contingency.tex
cat contingency_tableBEFws.tex contingency_tableAFTws.tex contingency_tableAFTws_FT.tex >> night1_contingency.tex
cat contingency_tableBEFwd.tex contingency_tableAFTwd.tex >> night1_contingency.tex
cat contingency_tableBEFrh.tex contingency_tableAFTrh.tex contingency_tableAFTrh_FT.tex >> night1_contingency.tex
cat contingency_tableBEFpwv.tex contingency_tableAFTpwv.tex >> night1_contingency.tex
echo '\clearpage' >> night1_contingency.tex
cat contingency_tableBEFsee_0.0.tex contingency_tableAFTsee_0.0.tex >> night1_contingency.tex 
cat contingency_tableBEFsee_0.24.tex contingency_tableAFTsee_0.24.tex >> night1_contingency.tex
echo '\clearpage' >> night1_contingency.tex
cat contingency_tableBEFtau_0.0.tex contingency_tableAFTtau_0.0.tex >> night1_contingency.tex
cat contingency_tableBEFtau_1.22.tex contingency_tableAFTtau_1.22.tex >> night1_contingency.tex
echo '\clearpage' >> night1_contingency.tex
cat contingency_tableBEFglf_0.0.tex contingency_tableAFTglf_0.0.tex >> night1_contingency.tex 
cat contingency_tableBEFglf_0.14.tex contingency_tableAFTglf_0.14.tex >> night1_contingency.tex
cp night1_contingency.tex $WRKDIR/
# PODs TABLES
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' > night1_PODs.tex
echo '\subsection{NIGHT1: Probability of detection (PODi) for long and short forecast time scales}\label{subsec:night1:pod}' >> night1_PODs.tex
echo 'Tables in section \ref{subsec:night1:pod} are included in the range: \ref{tab:podws:night1}-\ref{tab:podglf0.14:night1}.' >> night1_PODs.tex
cat tablePODsws.tex tablePODsws_FT.tex tablePODswd.tex >> night1_PODs.tex
cat tablePODsrh.tex tablePODsrh_FT.tex tablePODspwv.tex >> night1_PODs.tex
cat tablePODssee_0.0.tex tablePODssee_0.24.tex >> night1_PODs.tex
cat tablePODstau_0.0.tex tablePODstau_1.22.tex >> night1_PODs.tex
cat tablePODsglf_0.0.tex tablePODsglf_0.14.tex >> night1_PODs.tex
cp night1_PODs.tex $WRKDIR/

######################
# DAY 1
if [ ! -d $WRKDIR/day1 ]; then
  mkdir $WRKDIR/day1
fi
cd $WRKDIR/day1
# DAY 1
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' > day1.tex
echo '\section{DAY1: Statistical analysis of forecast performances related}\label{sec:day1}' >> day1.tex
cp day1.tex $WRKDIR/
# FIGURES
echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" > day1_figures.tex
echo '\subsection{DAY1: Scatter plots (bias, RMSE, SD) for long and short forecast time scale}\label{subsec:day1:fig}' >> day1_figures.tex
echo 'Figures in section \ref{subsec:day1:fig} are included in the range: \ref{fig:ws:day1}-\ref{fig:pwv:day1}.' >> day1_figures.tex
cat figures_ws.tex figures_wd.tex figures_rh.tex figures_pwv.tex >> day1_figures.tex
cp day1_figures.tex $WRKDIR/
# STATISTICS
echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" > day1_statistics.tex
echo '\subsection{DAY1: Summarizing values of bias, RMSE, SD for long and short forecast time scales}\label{subsec:day1:stat}' >> day1_statistics.tex
echo 'Tables in section \ref{subsec:day1:stat} are included in the range: \ref{tab:day1:statBEF}-\ref{tab:day1:statAFT:IM}.' >> day1_statistics.tex
cat table_skills_BEF.tex table_skills_AFT-LASTM.tex table_skills_AFT.tex >> day1_statistics.tex
echo '\clearpage' >> day1_contingency.tex
cp day1_statistics.tex $WRKDIR/
# CONTINGENCY TABLES
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' > day1_contingency.tex
echo '\section{DAY1: Contingency tables}\label{sec:day1:cont}' >> day1_contingency.tex
echo 'Tables in section \ref{sec:day1:cont} are included in the range: \ref{tab:contingencywsBEF:day1}-\ref{tab:contingencypwvAFT:day1}.' >> day1_contingency.tex
cat contingency_tableBEFws.tex contingency_tableAFTws.tex contingency_tableAFTws_FT.tex >> day1_contingency.tex
cat contingency_tableBEFwd.tex contingency_tableAFTwd.tex >> day1_contingency.tex
cat contingency_tableBEFrh.tex contingency_tableAFTrh.tex contingency_tableAFTrh_FT.tex >> day1_contingency.tex
cat contingency_tableBEFpwv.tex contingency_tableAFTpwv.tex >> day1_contingency.tex
cp day1_contingency.tex $WRKDIR/
# PODs TABLES
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%' > day1_PODs.tex
echo '\subsection{DAY1: Probability of detection (PODi) for long and short forecast time scales}\label{subsec:day1:pod}' >> day1_PODs.tex
echo 'Tables in section \ref{subsec:day1:pod} are included in the range: \ref{tab:podws:day1}-\ref{tab:podpwv:day1}.' >> day1_PODs.tex
cat tablePODsws.tex tablePODsws_FT.tex tablePODswd.tex \
    tablePODsrh.tex tablePODsrh_FT.tex tablePODspwv.tex >> day1_PODs.tex
cp day1_PODs.tex $WRKDIR/

##################################################################################
# Copy static Sections to the working directory
for f in $textLOGs_file $introduction_file $references_file
do
  if [ ! -e $f ]; then
    warning "file $f is missing";
  else
    cp $f $WRKDIR
  fi
done

# TEMPLATE
# Check and modify the template Latex file
if [ ! -e $main_tmpl_file ]; then
  error "$main_tmpl_file doe not exist"
fi
export LC_TIME="en_US.UTF-8"
cat $main_tmpl_file | sed -e "s!TODAYDATESTRING!$TODAYDATESTRING!"    | \
                      sed -e "s!TODAYMONTHSTRING!$TODAYMONTHSTRING!"  | \
                      sed -e "s!TODAYYEARSTRING!$TODAYYEARSTRING!"    | \
                      sed -e "s!LASTMONTHSTRING!$LASTMONTHSTRING!"    | \
                      sed -e "s!LASTYEARSTRING!$LASTYEARSTRING!"    > $WRKDIR/main.tex
if [ $? != 0 ]; then
  error "Problem in creating $WRKDIR/main.tex file"
fi
if [ ! -f "$WRKDIR/main.tex" ]; then
  error "Cannot create $WRKDIR/main.tex. Exiting..."
fi

#Annex
if [ ! -e $annex_tmpl_file ]; then
  error "$annex_tmpl_file doe not exist"
fi
export LC_TIME="en_US.UTF-8"
cat $annex_tmpl_file | sed -e "s!TODAYDATESTRING!$TODAYDATESTRING!"    | \
                      sed -e "s!TODAYMONTHSTRING!$TODAYMONTHSTRING!"  | \
                      sed -e "s!TODAYYEARSTRING!$TODAYYEARSTRING!"    | \
                      sed -e "s!LASTMONTHSTRING!$LASTMONTHSTRING!"    | \
                      sed -e "s!LASTYEARSTRING!$LASTYEARSTRING!"    > $WRKDIR/annex.tex
if [ $? != 0 ]; then
  error "Problem in creating $WRKDIR/annex.tex file"
fi
if [ ! -f "$WRKDIR/annex.tex" ]; then
  error "Cannot create $WRKDIR/annex.tex. Exiting..."
fi

#################################################################
# COMPILING THE LATEX FILE
cd $WRKDIR
notice "Compiling the MAIN REPORT file"
pdflatex main.tex > /dev/null 2>&1
pdflatex main.tex > /dev/null 2>&1
if [ $? != 0 ]; then
  error "Problem in compiling $WRKDIR/main.tex file"
else
  cp main.pdf $report_pdf_file
  if [ $? == 0 ]; then
    notice "OK $report_pdf_file"
  else
    error "Generic error in cp -v main.pdf $report_pdf_file"
  fi
fi
notice "Compiling the ANNEX file"
pdflatex annex.tex > /dev/null 2>&1
pdflatex annex.tex > /dev/null 2>&1
if [ $? != 0 ]; then
  error "Problem in compiling $WRKDIR/annex.tex file"
else
  cp annex.pdf $annex_pdf_file
  if [ $? == 0 ]; then
    notice "OK $annex_pdf_file"
  else
    error "Generic error in cp -v main.pdf $annex_pdf_file"
  fi
fi

#################################################################
# exit
notice "End of "`basename $0`
exit 0
#################################################################

