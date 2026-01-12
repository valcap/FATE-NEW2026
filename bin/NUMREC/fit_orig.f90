SUBROUTINE fit_orig(x,y,b,sigb,chi2,q,sig)
! INPUT:
! X=set of data from which we want to calculate the fit 
! Y=set of data from which we want to calculate the fit 
! sig= stdv of input (equal to 1 if you do not know them)
!
! OUTPUTS:
! a=offset on y-axis (staccata all'aorigine)
! b=slope
! siga=stdv of a
! sigb=stdv of b
! chi2
! q
!
! CASEA) non si mette il sig in entrata:
! il modello mette sigi=1 e poi calcola i siga e i sigb
! corretti dal coefficiente sqrt(chi2/N-2)
! Eq.(15.2.17) - (15.2.22). In questo caso Q=1
! perche' e' come assumere che il modello Y=a+bx e' corretto
!
! CASEB) si mette un sigi in entrata e il modello
! calcola siga e sigb come da manuale
! Eq.(15.2.17) - (15.2.22). q viene calcolato dal 
! modello secondo eq. (15.2.12). Attenzione: 
! puo' essere che si ottiene q=1 anche per un valore di chi2.ne.0
! perche' la curva gammaq puo' assumere un valore uguale a 1 per chi2.ne.0
! se N grande. Quando N piccolo, q si discosta da uno piu' facilmente
!
! se se si hanno dei dubbi sul valore del sigi da fornire alla routine, conviene
! non metterlo quindi usare il CASEA. 
!
USE nrtype; USE nrutil, ONLY : assert_eq
USE nr, ONLY : gammq
IMPLICIT NONE
REAL(SP), DIMENSION(:), INTENT(IN) :: x,y
REAL(SP), INTENT(OUT) :: b,sigb,chi2,q
REAL(SP), DIMENSION(:), OPTIONAL, INTENT(IN) :: sig
INTEGER(I4B) :: ndata,i
REAL(SP), DIMENSION(size(x)) :: delta
REAL(SP) :: invx
REAL(SP) :: sigdat,ss,sx,sxoss,sy,st2
REAL(SP), DIMENSION(size(x)), TARGET :: t
REAL(SP), DIMENSION(:), POINTER :: wt
print *, 'ENTERING fit routine'
if (present(sig)) then
print *, 'ARRAYS SIZE: ',size(x),size(y),size(sig)
ndata=assert_eq(size(x),size(y),size(sig),'fit_orig')
print *, 'ndata= ', ndata
else
print *, 'ARRAYS SIZE: ',size(x),size(y)
ndata=assert_eq(size(x),size(y),'fit_orig')
print *, 'ndata= ', ndata
endif
sx=sum(x)
print*,'sx=',sx
sy=sum(y)
b=sy/sx
invx=(1./sx)**2
print*,'invx=',invx
if (present(sig)) then
delta=((y-b*x)/sig)**2
chi2=sum(delta)
sigb=0
do i=1,size(x)
sigb=sigb+invx*sig(i)**2
enddo
sigb=sqrt(sigb)
if (ndata > 2) q=gammq(0.5_sp*(size(x)-2),0.5_sp*chi2)
print*,'q=',q,'ndata=',ndata,'size(x)=',size(x),'chi2=',chi2,'sigb=',sigb
else
delta=((y-b*x))**2
chi2=sum(delta)
sigb=sqrt(size(x)*invx)*sqrt(chi2/(size(x)-2))
print*,'q=',q,'ndata=',ndata,'size(x)=',size(x),'chi2=',chi2,'sigb=',sigb
end if
END SUBROUTINE fit_orig
