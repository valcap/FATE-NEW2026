SUBROUTINE fit(x,y,a,b,siga,sigb,chi2,q,sig)
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
REAL(SP), INTENT(OUT) :: a,b,siga,sigb,chi2,q
REAL(SP), DIMENSION(:), OPTIONAL, INTENT(IN) :: sig
INTEGER(I4B) :: ndata
REAL(SP) :: sigdat,ss,sx,sxoss,sy,st2
REAL(SP), DIMENSION(size(x)), TARGET :: t
REAL(SP), DIMENSION(:), POINTER :: wt
print *, 'ENTERING fit routine'
if (present(sig)) then
print *, 'ARRAYS SIZE: ',size(x),size(y),size(sig)
ndata=assert_eq(size(x),size(y),size(sig),'fit')
print *, 'ndata= ', ndata
wt=>t
wt(:)=1.0_sp/(sig(:)**2)
ss=sum(wt(:))
sx=dot_product(wt,x)
sy=dot_product(wt,y)
else
ndata=assert_eq(size(x),size(y),'fit')
ss=real(size(x),sp)
sx=sum(x)
sy=sum(y)
end if
sxoss=sx/ss
t(:)=x(:)-sxoss
if (present(sig)) then
t(:)=t(:)/sig(:)
b=dot_product(t/sig,y)
else
b=dot_product(t,y)
end if
st2=dot_product(t,t)
b=b/st2
a=(sy-sx*b)/ss
siga=sqrt((1.0_sp+sx*sx/(ss*st2))/ss)
sigb=sqrt(1.0_sp/st2)
t(:)=y(:)-a-b*x(:)
q=1.0
if (present(sig)) then
t(:)=t(:)/sig(:)
chi2=dot_product(t,t)
if (ndata > 2) q=gammq(0.5_sp*(size(x)-2),0.5_sp*chi2)
print*,'q=',q,'ndata=',ndata,'size(x)=',size(x),'chi2=',chi2
else
chi2=dot_product(t,t)
sigdat=sqrt(chi2/(size(x)-2))
siga=siga*sigdat
sigb=sigb*sigdat
end if
END SUBROUTINE fit
