#!/bin/bash

function notice ( )
{
  echo `date +%H:%M:%S`" * "$@
  return 0
}

function subnotice ( )
{
  echo `date +%H:%M:%S`" -- "$@
  return 0
}


function warning ( )
{
  echo `date +%H:%M:%S`" --- WARNING - "$@
  return 0
}

function error ( )
{
  echo `date +%H:%M:%S`" +++ ERROR - "$@
  echo `date +%H:%M:%S`" - "End
  exit 1
}

function get_var_attr ()
{
  case "$1" in
  ws)
    export prefixUC='WS'
    export descri='wind speed'
    export unitof='$m s^{-1}$'
    export suffix='stan'
    ;;
  wd)
    export prefixUC='WD'
    export descri='wind direction'
    export unitof='degree'
#    export suffix='stan_0_90'
    export suffix='stan'
    ;;
  rh)
    export prefixUC='RH'
    export descri='relative humidity'
    export unitof='percent'
    export suffix='stan'
    ;;
  pwv)
    export prefixUC='PWV'
    export descri='precipitable water vapor'
    export unitof='mm'
    export suffix='stan'
    ;;
  see)
    export prefixUC='SEE'
    export descri='total seeing'
    export unitof='arcsec'
    export suffix='os18_1000'
   ;;
  tau)
    export prefixUC='TAU'
    export descri='wavefront coherence time'
    export unitof='ms'
    export suffix='os18_1000'
    ;;
  glf)
    export prefixUC='GLF'
    export descri='ground layer fraction'
    export unitof='dimensionless'
    export suffix='os18'
    ;;
  *) echo "Lo sai chi ti saluta?"
     exit 1
     ;;
  esac
}

# Funzione per convertire una data in timestamp
function converti_in_timestamp() {
    local data=$1
    local timestamp=$(date -d "$data" +%s)
    echo $timestamp
}

# Funzione per calcolare la differenza in giorni tra due timestamp
function differenza_in_giorni() {
    local timestamp1=$1
    local timestamp2=$2
    local differenza=$(( ($timestamp2 - $timestamp1) / 86400 )) # 86400 secondi in un giorno
    echo $differenza
}
