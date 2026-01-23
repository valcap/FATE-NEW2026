!******************************************************************************
module mod_scienceperso      ! subrotines for reading files                      !
!See the comments in each subroutine
!******************************************************************************

  implicit none

contains

!******************************************************************************
!******************************************************************************
SUBROUTINE STATS(ARRAY,SIZE_ARRAY,MEAN,SD,INC,NO_VALUE)
!Compute mean and sd over an array
!******************************************************************************
 implicit none
 INTEGER, INTENT(IN) :: SIZE_ARRAY
 REAL, DIMENSION(SIZE_ARRAY), INTENT(IN) :: ARRAY
 REAL, INTENT(IN) :: NO_VALUE
 REAL, INTENT(OUT) :: MEAN,SD
 INTEGER, INTENT(OUT) :: INC

 INTEGER :: I
 REAL :: SUM_DIFF

 MEAN=0
 INC=0
 DO I=1,SIZE_ARRAY
  IF (ARRAY(I).NE.NO_VALUE) THEN
   INC = INC+1
   MEAN = MEAN + ARRAY(I)
  ENDIF
 ENDDO
 IF (INC.ne.0.) MEAN = MEAN / INC
 IF (INC.eq.0.) MEAN=NO_VALUE

IF (INC.ne.0.) THEN
 SUM_DIFF=0
 SD=0
 DO I=1,SIZE_ARRAY
  IF (ARRAY(I).NE.NO_VALUE) THEN
   SUM_DIFF = SUM_DIFF + (ARRAY(I) - MEAN)**2
  ENDIF
 ENDDO
 IF(INC.NE.1)THEN
 SUM_DIFF = SUM_DIFF / (INC-1)
 ELSE
 SUM_DIFF = SUM_DIFF
 ENDIF
 SD = SQRT(SUM_DIFF)
ELSE
 SD = NO_VALUE
ENDIF
END SUBROUTINE STATS
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE STATS_WIND_DIRECTION(VX,VY,SIZE_ARRAY,MEAN,SD,INC,NO_VALUE)
!Compute mean and sd over an array - WIND DIRECTION
!******************************************************************************
 implicit none
 INTEGER, INTENT(IN) :: SIZE_ARRAY
 REAL, DIMENSION(SIZE_ARRAY), INTENT(IN) :: VX,VY
 REAL, INTENT(IN) :: NO_VALUE
 REAL, INTENT(OUT) :: MEAN,SD
 INTEGER, INTENT(OUT) :: INC

 INTEGER :: I
 REAL :: SUM_DIFF_WD
 REAL :: MEAN_VX,MEAN_VY,Pi
 REAL, DIMENSION(SIZE_ARRAY) :: WD

 Pi=3.141592654
 MEAN_VX=0
 MEAN_VY=0
 INC=0
 MEAN=NO_VALUE
 DO I=1,SIZE_ARRAY
  IF ( (VX(I).NE.NO_VALUE).AND.(VY(I).NE.NO_VALUE) ) THEN
   INC = INC+1
   MEAN_VX = MEAN_VX + VX(I)
   MEAN_VY = MEAN_VY + VY(I)
  ENDIF
 ENDDO
 if (mean_vx.eq.0.) then
  if (mean_vy.lt.0.) mean=0.
  if (mean_vy.gt.0.) mean=180.
 else  
  IF (INC.ne.0) then 
   MEAN = 180*ATAN(MEAN_VY/MEAN_VX)/Pi !Resulting mean wind direction
   if (mean_vx.lt.0.) MEAN = 90 - MEAN
   if (mean_vx.gt.0.) MEAN = 270 - MEAN
   IF (MEAN.lt.0) MEAN = MEAN +360.
  endif
 endif
 MEAN_VX = MEAN_VX / INC
 MEAN_VY = MEAN_VY / INC


 WD=NO_VALUE
 DO I=1,SIZE_ARRAY
  IF ( (VX(I).NE.NO_VALUE).AND.(VY(I).NE.NO_VALUE) ) then 
    WD(I) = 180*ATAN(VY(I)/VX(I))/Pi
    if (vx(i).lt.0.) WD(I) = 90 - WD(I)
    if (vx(i).gt.0.) WD(I) = 270 - WD(I)
    IF (WD(I).lt.0.) WD(I)=WD(I)+360.
  endif
 ENDDO



 SUM_DIFF_WD=0
 SD=0
 INC=0
 DO I=1,SIZE_ARRAY
  IF ( (WD(I).NE.NO_VALUE) ) THEN
   INC = INC+1
   IF ( ABS(WD(I) - MEAN).le.180 ) SUM_DIFF_WD = SUM_DIFF_WD + (WD(I) - MEAN)**2
   IF ( ABS(WD(I) - MEAN).ge.180 ) THEN 
    IF ( WD(I).gt.MEAN ) SUM_DIFF_WD = SUM_DIFF_WD + ( (WD(I)-360) - MEAN)**2
    IF ( WD(I).lt.MEAN ) SUM_DIFF_WD = SUM_DIFF_WD + ( (WD(I)+360) - MEAN)**2
   ENDIF
  ENDIF
 ENDDO
 IF(INC.NE.1) THEN
  SUM_DIFF_WD = SUM_DIFF_WD / (INC-1)
 ENDIF
 SD = SQRT(SUM_DIFF_WD)
END SUBROUTINE STATS_WIND_DIRECTION
!******************************************************************************

!******************************************************************************
!******************************************************************************
REAL FUNCTION  Median(X, N, NO_VALUE)
! --------------------------------------------------------------------
! REAL FUNCTION  Median() :
!    This function receives an array X of N entries, copies its value
! to a local array Temp(), sorts Temp() and computes the median.
!    The returned value is of REAL type.
! --------------------------------------------------------------------
!******************************************************************************
 IMPLICIT  NONE
 real, DIMENSION(:), INTENT(IN)  :: X
 INTEGER, INTENT(IN)             :: N
 REAL, INTENT(IN)                :: NO_VALUE 
 real, DIMENSION(N)              :: Temp
 real                            :: Tmp,MMAX
 INTEGER                         :: i, Location, count_mmax, Ndim

 Ndim=N
 MMAX=maxval(X,1)+1
 count_mmax=0
 DO i = 1, N                       ! make a copy
  if (X(i) == NO_VALUE) then
   Temp(i)=MMAX
   count_mmax=count_mmax+1
  else
   Temp(i) = X(i)
  end if
 END DO
 !Subtract count_mmax values
 if (count_mmax > 0) then
  Ndim=N-count_mmax
 end if
 !Sort the array
 DO i = 1, Ndim-1                      ! except for the last
  Location = minloc(Temp(i:Ndim),1)+i-1
  ! swap this and the minimum
  Tmp = Temp(i)
  Temp(i)    = Temp(Location)
  Temp(Location)    = Tmp
 END DO
 if (Ndim==0) then
  Median=NO_VALUE
 else
  IF (MOD(Ndim,2) == 0) THEN           ! compute the median
   Median = (Temp(Ndim/2) + Temp(Ndim/2+1)) / 2.0
  ELSE
   Median = Temp(Ndim/2+1)
  END IF
 end if
END FUNCTION  Median
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE BIAS_RMSE(ARRAY1,ARRAY2,SIZE_ARRAY,BIAS,RMSE,INC,NO_VALUE)
!Computes standard statistical operators BIAS and RMSE between two arrays
!******************************************************************************
 implicit none
 INTEGER, INTENT(IN) :: SIZE_ARRAY
 REAL, DIMENSION(SIZE_ARRAY), INTENT(IN) :: ARRAY1,ARRAY2
 REAL, INTENT(IN) :: NO_VALUE
 REAL, INTENT(OUT) :: BIAS,RMSE
 INTEGER, INTENT(OUT) :: INC

 INTEGER :: I

 BIAS=0.
 RMSE=0.
 INC=0
 DO I=1,SIZE_ARRAY
  IF ( (ARRAY1(I).NE.NO_VALUE) .AND. (ARRAY2(I).NE.NO_VALUE) ) THEN
   INC = INC+1
   BIAS = BIAS + (ARRAY2(I)-ARRAY1(I))
!   BIAS = BIAS + ABS(ARRAY2(I)-ARRAY1(I))
   RMSE = RMSE + (ARRAY2(I)-ARRAY1(I))**2
  ENDIF
 ENDDO
 if (INC.ne.0.) BIAS = BIAS / INC
 if (INC.ne.0.) RMSE = sqrt (RMSE / INC)
END SUBROUTINE BIAS_RMSE
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE BIAS_RMSE_SIGMA(ARRAY1,ARRAY2,SIZE_ARRAY,BIAS,RMSE,SIGMA,INC,NO_VALUE)
!Computes standard statistical operators BIAS RMSE and SIGMA between two arrays
!******************************************************************************
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: SIZE_ARRAY
 REAL, DIMENSION(SIZE_ARRAY), INTENT(IN) :: ARRAY1,ARRAY2
 REAL, INTENT(IN) :: NO_VALUE
 REAL, INTENT(OUT) :: BIAS,RMSE,SIGMA
 INTEGER, INTENT(OUT) :: INC

 INTEGER :: I

 BIAS=0.
 RMSE=0.
 SIGMA=0.
 INC=0
 DO I=1,SIZE_ARRAY
  IF ( (ARRAY1(I).NE.NO_VALUE) .AND. (ARRAY2(I).NE.NO_VALUE) ) THEN
   INC = INC+1
   BIAS = BIAS + (ARRAY2(I)-ARRAY1(I))
!   BIAS = BIAS + ABS(ARRAY2(I)-ARRAY1(I))
   RMSE = RMSE + (ARRAY2(I)-ARRAY1(I))**2
  ENDIF
 ENDDO
 if (INC.ne.0.) BIAS = BIAS / INC
 if (INC.ne.0.) RMSE = sqrt (RMSE / INC)
 if (INC.ne.0.) SIGMA=SQRT(RMSE**2-BIAS**2)

END SUBROUTINE BIAS_RMSE_SIGMA
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE BIAS_RMSE_WIND_DIRECTION(ARRAY2,ARRAY1,SIZE_ARRAY,BIAS,RMSE,INC,NO_VALUE)
!******************************************************************************
 implicit none
 INTEGER, INTENT(IN) :: SIZE_ARRAY
 REAL, DIMENSION(SIZE_ARRAY), INTENT(IN) :: ARRAY1,ARRAY2
 REAL, INTENT(IN) :: NO_VALUE
 REAL, INTENT(OUT) :: BIAS,RMSE
 INTEGER, INTENT(OUT) :: INC

 INTEGER :: I
 REAL    :: DIFF

 DIFF=0.d0
 BIAS=0
 RMSE=0
 INC=0
 DO I=1,SIZE_ARRAY
  IF ( (ARRAY1(I).NE.NO_VALUE) .AND. (ARRAY2(I).NE.NO_VALUE) ) THEN
   INC = INC+1
   IF ( ABS(ARRAY1(I) - ARRAY2(I)).le.180 ) DIFF = ARRAY1(I) - ARRAY2(I) 
   IF ( ABS(ARRAY1(I) - ARRAY2(I)).ge.180 ) THEN
    IF ( ARRAY1(I).gt.ARRAY2(I) ) DIFF =  (ARRAY1(I)-360) - ARRAY2(I)
    IF ( ARRAY1(I).lt.ARRAY2(I) ) DIFF =  (ARRAY1(I)+360) - ARRAY2(I)
   ENDIF
   BIAS = BIAS + DIFF
   RMSE = RMSE + (DIFF)**2
  ENDIF
 ENDDO
 if (INC.ne.0.) then
  BIAS = BIAS / INC
  RMSE = sqrt (RMSE / INC)
 else
  BIAS=NO_VALUE
  RMSE=NO_VALUE
 endif
END SUBROUTINE BIAS_RMSE_WIND_DIRECTION
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE BIAS_RMSE_SIGMA_WIND_DIRECTION(ARRAY2,ARRAY1,SIZE_ARRAY,BIAS,RMSE,SIGMA,INC,NO_VALUE)
!******************************************************************************
 implicit none
 INTEGER, INTENT(IN) :: SIZE_ARRAY
 REAL, DIMENSION(SIZE_ARRAY), INTENT(IN) :: ARRAY1,ARRAY2
 REAL, INTENT(IN) :: NO_VALUE
 REAL, INTENT(OUT) :: BIAS,RMSE,SIGMA
 INTEGER, INTENT(OUT) :: INC

 INTEGER :: I
 REAL    :: DIFF

 DIFF=0.d0
 BIAS=0       
 RMSE=0        
 INC=0
 DO I=1,SIZE_ARRAY
  IF ( (ARRAY1(I).NE.NO_VALUE) .AND. (ARRAY2(I).NE.NO_VALUE) ) THEN
   INC = INC+1
   IF ( ABS(ARRAY1(I) - ARRAY2(I)).le.180 ) DIFF = ARRAY1(I) - ARRAY2(I)
   IF ( ABS(ARRAY1(I) - ARRAY2(I)).ge.180 ) THEN
    IF ( ARRAY1(I).gt.ARRAY2(I) ) DIFF =  (ARRAY1(I)-360) - ARRAY2(I)
    IF ( ARRAY1(I).lt.ARRAY2(I) ) DIFF =  (ARRAY1(I)+360) - ARRAY2(I)
   ENDIF
   BIAS = BIAS + DIFF
   RMSE = RMSE + (DIFF)**2
  ENDIF
 ENDDO
 if (INC.ne.0.) then 
  BIAS = BIAS / INC
  RMSE = sqrt (RMSE / INC)
  SIGMA=SQRT(RMSE**2-BIAS**2)
 else
  BIAS=NO_VALUE
  RMSE=NO_VALUE
  SIGMA=NO_VALUE
 endif
END SUBROUTINE BIAS_RMSE_SIGMA_WIND_DIRECTION
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE SD_REG_LIN(SIZE_ARRAY,XARRAY,YARRAY,SLOPE,NO_VALUE,SD)
! SLOPE=slope of the regression line
! SD=standard deviation with respect to the regression line
! SIZE_ARRAY has to be the same for XARRAY and YARRAY
!******************************************************************************
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: SIZE_ARRAY
 REAL, DIMENSION(SIZE_ARRAY), INTENT(IN) :: XARRAY,YARRAY
 REAL, INTENT(IN) :: SLOPE
 REAL, INTENT(IN) :: NO_VALUE
 REAL, INTENT(OUT) :: SD

 INTEGER :: I,INC

 SD=0.
 INC=0
 DO I=1,SIZE_ARRAY
  IF ( (XARRAY(I).NE.NO_VALUE).AND.(YARRAY(I).NE.NO_VALUE) ) THEN
   INC = INC+1
   SD = SD + (YARRAY(I) - ABS(SLOPE)*XARRAY(I))**2
  ENDIF
 ENDDO
 IF (INC.NE.0.) SD = SQRT(SD / INC)
 IF (INC.EQ.0.) SD = NO_VALUE

END SUBROUTINE
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE CORR(N,X,Y,NO_VALUE_X,CORR_COEF,NO_VALUE_Y)
!Compute correlation coefficient between two arrays
!******************************************************************************
 implicit none
 INTEGER, INTENT(IN) :: N
 REAL, INTENT(IN)  :: NO_VALUE_X
 REAL, OPTIONAL, INTENT(IN)  :: NO_VALUE_Y
 REAL, DIMENSION(N),INTENT(IN)  :: X
 REAL, DIMENSION(N),INTENT(IN)  :: Y
 REAL,INTENT(OUT)   :: CORR_COEF 

 INTEGER  :: I,INC
 REAL     :: MEAN_X,MEAN_Y,VAR_X,VAR_Y,SOM_XY
 REAL,DIMENSION(N) :: DIF,DIFX2,DIFY2

DIF(:)=0.
DIFX2(:)=0.
DIFY2(:)=0.
CORR_COEF=0.

MEAN_X=0
MEAN_Y=0
INC=0
print *, 'Subroutine CORR is called.'
IF (PRESENT(NO_VALUE_Y)) THEN
 print *, 'NO_VALUE_X=',NO_VALUE_X,' and NO_VALUE_Y=',NO_VALUE_Y
 DO I=1,N
  IF ( (X(I).NE.NO_VALUE_X).AND.(Y(I).NE.NO_VALUE_Y) ) THEN
   INC = INC+1
   MEAN_X = MEAN_X + X(I)
   MEAN_Y = MEAN_Y + Y(I)
  ENDIF
 ENDDO
ELSE
 print *, 'NO_VALUE_X=',NO_VALUE_X, '; NO_VALUE_Y absent, set to NO_VALUE_X.'
 DO I=1,N
  IF ( (X(I).NE.NO_VALUE_X).AND.(Y(I).NE.NO_VALUE_X) ) THEN
   INC = INC+1
   MEAN_X = MEAN_X + X(I)
   MEAN_Y = MEAN_Y + Y(I)
  ENDIF
 ENDDO
ENDIF
MEAN_X = MEAN_X / INC
print *, 'INC CC = ', INC
MEAN_Y = MEAN_Y / INC

VAR_X=0
VAR_Y=0
IF (PRESENT(NO_VALUE_Y)) THEN
 DO I=1,N
  IF ( (X(I).NE.NO_VALUE_X).AND.(Y(I).NE.NO_VALUE_Y) ) THEN
   DIFX2(I)=(X(I)-MEAN_X)*(X(I)-MEAN_X) 
   VAR_X=VAR_X+DIFX2(I)
   DIFY2(I)=(Y(I)-MEAN_Y)*(Y(I)-MEAN_Y)
   VAR_Y=VAR_Y+DIFY2(I)
  ENDIF
 ENDDO
ELSE
 DO I=1,N
  IF ( (X(I).NE.NO_VALUE_X).AND.(Y(I).NE.NO_VALUE_X) ) THEN
   DIFX2(I)=(X(I)-MEAN_X)*(X(I)-MEAN_X)
   VAR_X=VAR_X+DIFX2(I)
   DIFY2(I)=(Y(I)-MEAN_Y)*(Y(I)-MEAN_Y)
   VAR_Y=VAR_Y+DIFY2(I)
  ENDIF
 ENDDO
ENDIF
VAR_Y=SQRT(VAR_Y)
VAR_X=SQRT(VAR_X)

SOM_XY=0
IF (PRESENT(NO_VALUE_Y)) THEN
 DO I=1,N
  IF ( (X(I).NE.NO_VALUE_X).AND.(Y(I).NE.NO_VALUE_Y) ) THEN
   DIF(I)=(X(I)-MEAN_X)*(Y(I)-MEAN_Y)
   SOM_XY=SOM_XY+DIF(I)
  ENDIF
 ENDDO
ELSE
 DO I=1,N
  IF ( (X(I).NE.NO_VALUE_X).AND.(Y(I).NE.NO_VALUE_X) ) THEN
   DIF(I)=(X(I)-MEAN_X)*(Y(I)-MEAN_Y)
   SOM_XY=SOM_XY+DIF(I)
  ENDIF
 ENDDO
ENDIF
CORR_COEF=SOM_XY/(VAR_X*VAR_Y)
END SUBROUTINE CORR
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine ccc(n,wd1,wd2,noval1,coeff,noval2)
!Circular coefficient correlation by Fisher and Lee (1983).
!http://cnx.org/content/m22974/latest/
!******************************************************************************
implicit none
integer, intent(in)            :: n
real, intent(in)               :: noval1
real, intent(in),optional      :: noval2
real, dimension(n), intent(in) :: wd1,wd2
real, intent(out)              :: coeff

real, dimension(n) :: wd_1,wd_2
integer :: i,j
real    :: sum1,sum2,sum3,tmp
real    :: Pi

tmp=noval1
tmp=noval2

Pi=3.141592654
do i=1,n
 wd_1(i) = wd1(i) * Pi / 180.
 wd_2(i) = wd2(i) * Pi / 180.
enddo

sum1=0.
do i=1,n-1
 do j=i+1,n
  sum1 = sum1 + sin(wd_1(i)-wd_1(j)) * sin(wd_2(i)-wd_2(j))
 enddo
enddo
sum2=0.
do i=1,n-1
 do j=i+1,n
  sum2 = sum2 + ( sin(wd_1(i)-wd_1(j)) )**2
 enddo
enddo
sum3=0.
do i=1,n-1
 do j=i+1,n
  sum3 = sum3 + ( sin(wd_2(i)-wd_2(j)) )**2
 enddo
enddo

coeff = sum1 / sqrt (sum2*sum3)
end subroutine ccc
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine hit_rate(array1,array2,size_array,xlim1,xlim2,noval,tab_hr,pod,pc,ebd,lflag)
!array1=reference (obs) from which xlim1 and xlim2 have been extracted
! To be used for 3x3 tables
! valcap 20230314 viene utilizzata questa
!******************************************************************************
implicit none

logical, intent(in), optional :: lflag !TRUE=with percent, FALSE=number of points
integer, intent(in) :: size_array
real,dimension(size_array), intent(in) :: array1,array2
real, intent(in) :: xlim1,xlim2,noval
real, dimension(3,3), intent(out) :: tab_hr
real, dimension(3), intent(out) ::pod
real, intent(out) :: pc, ebd

integer :: i,icount
real :: a,b,c,d,e,f,g,h,ii
if (present(lflag)) then
 if (lflag.eqv..true.) print *, 'Contingency table filled with percent.'
 if (lflag.eqv..false.) print *, 'Contingency table filled with numbers.'
else
 print *, 'Contingency table filled with percent.'
endif

tab_hr=0
icount=0
do i=1,size_array
 if ( (array1(i).ne.noval).and.(array2(i).ne.noval) ) then
  icount = icount + 1
  if (array1(i).le.xlim1) then
   if (array2(i).le.xlim1)                              tab_hr(1,1) = tab_hr(1,1) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,1) = tab_hr(2,1) + 1 
   if (array2(i).gt.xlim2)                              tab_hr(3,1) = tab_hr(3,1) + 1
  endif
  if ( (array1(i).gt.xlim1).and.(array1(i).le.xlim2) ) then 
   if (array2(i).le.xlim1)                              tab_hr(1,2) = tab_hr(1,2) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,2) = tab_hr(2,2) + 1
   if (array2(i).gt.xlim2)                              tab_hr(3,2) = tab_hr(3,2) + 1
  endif
  if (array1(i).gt.xlim2) then
   if (array2(i).le.xlim1)                              tab_hr(1,3) = tab_hr(1,3) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,3) = tab_hr(2,3) + 1
   if (array2(i).gt.xlim2)                              tab_hr(3,3) = tab_hr(3,3) + 1  
  endif
 endif
enddo

print *, 'LOGINFO Hit rate computed on',icount,'points.'
print *, 'Number of points ',icount,';'
!print *, 'LOGINFO Number of points ',icount


!  Expressed in percent
if ( (present(lflag).and.(lflag.eqv..True.)).or.(.not.present(lflag)) ) then 
tab_hr = (tab_hr / icount)*100. !in %

a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(2,1)
e = tab_hr(2,2)
f = tab_hr(2,3)
g = tab_hr(3,1)
h = tab_hr(3,2)
ii = tab_hr(3,3)
print *, 'X<',xlim1,xlim1,'<X<',xlim2,'X>',xlim2
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3) 
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3) 
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3) 
pod(1)=a/(a+d+g)*100
print *, 'POD(event1)=', pod(1)
pod(2)=e/(b+e+h)*100
print *, 'POD(event2)=', pod(2)
pod(3)=ii/(c+f+ii)*100
print *, 'POD(event3)=', pod(3)
pc=(a+e+ii)
print*,'C=',pc
ebd=(c+g)
print*,'ebd=',ebd
endif

! Expressed in numbers
if ( (present(lflag).and.(lflag.eqv..false.)) ) then
tab_hr = tab_hr !in %

a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(2,1)
e = tab_hr(2,2)
f = tab_hr(2,3)
g = tab_hr(3,1)
h = tab_hr(3,2)
ii = tab_hr(3,3)
print *, 'LOGINFO X<',xlim1,xlim1,'<X<',xlim2,'X>',xlim2
print *, 'LOGINFO ',tab_hr(1,1),tab_hr(1,2),tab_hr(1,3)
print *, 'LOGINFO ',tab_hr(2,1),tab_hr(2,2),tab_hr(2,3)
print *, 'LOGINFO ',tab_hr(3,1),tab_hr(3,2),tab_hr(3,3)
pod(1)=a/(a+d+g)*100
print *, 'LOGINFO POD(event1)=', pod(1)
pod(2)=e/(b+e+h)*100
print *, 'LOGINFO POD(event2)=', pod(2)
pod(3)=ii/(c+f+ii)*100
print *, 'LOGINFO POD(event3)=', pod(3)
pc=((a+e+ii)/icount)*100
print*,'LOGINFO PC=',pc
ebd=((c+g)/icount)*100
print*,'LOGINFO ebd=',ebd
endif

end subroutine hit_rate
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine hit_rate_diff(array1,array2,size_array,xlim1_array1,xlim2_array1,xlim1_array2,xlim2_array2,noval,tab_hr,pod,pc,ebd,lflag)
!array1=reference (obs) from which xlim1 and xlim2 have been extracted
! To be used for 3x3 tables
!It uses a different limit for array1 and array2
!******************************************************************************
implicit none

logical, intent(in), optional :: lflag !TRUE=with percent, FALSE=number of points
integer, intent(in) :: size_array
real,dimension(size_array), intent(in) :: array1,array2
real, intent(in) :: xlim1_array1,xlim2_array1,xlim1_array2,xlim2_array2,noval
real, dimension(3,3), intent(out) :: tab_hr
real, dimension(3), intent(out) ::pod
real, intent(out) :: pc, ebd

integer :: i,icount
real :: a,b,c,d,e,f,g,h,ii

if (present(lflag)) then
 if (lflag.eqv..true.) print *, 'Contingency table filled with percent.'
 if (lflag.eqv..false.) print *, 'Contingency table filled with numbers.'
else
 print *, 'Contingency table filled with percent.'
endif

tab_hr=0
icount=0
do i=1,size_array
 if ( (array1(i).ne.noval).and.(array2(i).ne.noval) ) then
  icount = icount + 1
  if (array1(i).le.xlim1_array1) then
   if (array2(i).le.xlim1_array2)                                     tab_hr(1,1) = tab_hr(1,1) + 1
   if ( (array2(i).gt.xlim1_array2).and.(array2(i).le.xlim2_array2) ) tab_hr(2,1) = tab_hr(2,1) + 1
   if (array2(i).gt.xlim2_array2)                                     tab_hr(3,1) = tab_hr(3,1) + 1
  endif
  if ( (array1(i).gt.xlim1_array1).and.(array1(i).le.xlim2_array1) ) then
   if (array2(i).le.xlim1_array2)                                     tab_hr(1,2) = tab_hr(1,2) + 1
   if ( (array2(i).gt.xlim1_array2).and.(array2(i).le.xlim2_array2) ) tab_hr(2,2) = tab_hr(2,2) + 1
   if (array2(i).gt.xlim2_array2)                                     tab_hr(3,2) = tab_hr(3,2) + 1
  endif
  if (array1(i).gt.xlim2_array1) then
   if (array2(i).le.xlim1_array2)                                     tab_hr(1,3) = tab_hr(1,3) + 1
   if ( (array2(i).gt.xlim1_array2).and.(array2(i).le.xlim2_array2) ) tab_hr(2,3) = tab_hr(2,3) + 1
   if (array2(i).gt.xlim2_array2)                                     tab_hr(3,3) = tab_hr(3,3) + 1
  endif
 endif
enddo

print *, 'Hit rate computed on',icount,'points.'
!print *, 'Number of points ',icount,';'

!  Expressed in percent
if ( (present(lflag).and.(lflag.eqv..True.)).or.(.not.present(lflag)) ) then
tab_hr = (tab_hr / icount)*100. !in %

a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(2,1)
e = tab_hr(2,2)
f = tab_hr(2,3)
g = tab_hr(3,1)
h = tab_hr(3,2)
ii = tab_hr(3,3)
print *, 'X<',xlim1_array1,xlim1_array1,'<X<',xlim2_array1,'X>',xlim2_array1
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3)
pod(1)=a/(a+d+g)*100
print *, 'POD(event1)=', pod(1)
pod(2)=e/(b+e+h)*100
print *, 'POD(event2)=', pod(2)
pod(3)=ii/(c+f+ii)*100
print *, 'POD(event3)=', pod(3)
pc=(a+e+ii)
print*,'PC=',pc
ebd=(c+g)
print*,'ebd=',ebd
endif

! Expressed in numbers
if ( (present(lflag).and.(lflag.eqv..false.)) ) then
tab_hr = tab_hr !in %

a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(2,1)
e = tab_hr(2,2)
f = tab_hr(2,3)
g = tab_hr(3,1)
h = tab_hr(3,2)
ii = tab_hr(3,3)
print *, 'X<',xlim1_array1,xlim1_array1,'<X<',xlim2_array1,'X>',xlim2_array1
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3)
pod(1)=a/(a+d+g)*100
print *, 'POD(event1)=', pod(1)
pod(2)=e/(b+e+h)*100
print *, 'POD(event2)=', pod(2)
pod(3)=ii/(c+f+ii)*100
print *, 'POD(event3)=', pod(3)
pc=((a+e+ii)/icount)*100
print*,'PC=',pc
ebd=((c+g)/icount)*100
print*,'ebd=',ebd
endif

end subroutine hit_rate_diff
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine hit_rate_mod(array1,array2,size_array,xlim1,xlim2,noval,tab_hr,pod,pc,ebd,rmse,lflag)
!As hit_rate but with RMSE extra parameter
!It considers a hit if model comes withing "rmse" from measure
! it has been built starting from the hit_rate adding a paramater 'rmse'
! array1=reference
!******************************************************************************
implicit none

logical, intent(in), optional :: lflag !TRUE=with percent, FALSE=number of points
integer, intent(in) :: size_array
real,dimension(size_array), intent(in) :: array1,array2
real, intent(in) :: xlim1,xlim2,noval
real, dimension(3,3), intent(out) :: tab_hr
real, dimension(3), intent(out) ::pod
real, intent(out) :: pc, ebd
real, intent(in) :: rmse

integer :: i,icount
real :: a,b,c,d,e,f,g,h,ii

if (present(lflag)) then
 if (lflag.eqv..true.) print *, 'Contingency table filled with percent.'
 if (lflag.eqv..false.) print *, 'Contingency table filled with numbers.'
else
 print *, 'Contingency table filled with percent.'
endif

tab_hr=0.
icount=0
do i=1,size_array
 if ( (array1(i).ne.noval).and.(array2(i).ne.noval) ) then
  icount = icount + 1
  if (array1(i).le.xlim1) then
   if ((array2(i)-rmse).le.xlim1) then
       tab_hr(1,1) = tab_hr(1,1) + 1.
   else
       if ( ((array2(i)).gt.xlim1).and.((array2(i)).le.xlim2) ) tab_hr(2,1) = tab_hr(2,1) + 1.
       if ((array2(i)).gt.xlim2)      tab_hr(3,1) = tab_hr(3,1) + 1.
   end if
  endif
  if ( (array1(i).gt.xlim1).and.(array1(i).le.xlim2) ) then
   if ( ((array2(i))+rmse.gt.xlim1).and.((array2(i))-rmse.le.xlim2) ) then
       tab_hr(2,2) = tab_hr(2,2) + 1.
   else
       if ((array2(i)).le.xlim1) tab_hr(1,2) = tab_hr(1,2) + 1.
       if ((array2(i)).gt.xlim2)      tab_hr(3,2) = tab_hr(3,2) + 1.
   end if
  endif
  if (array1(i).gt.xlim2) then
   if ((array2(i)+rmse).gt.xlim2) then
       tab_hr(3,3) = tab_hr(3,3) + 1.
   else
       if ( ((array2(i)+rmse).gt.xlim1).and.((array2(i)-rmse).le.xlim2) ) tab_hr(2,3) = tab_hr(2,3) + 1.
       if ((array2(i)-rmse).le.xlim1)  tab_hr(1,3) = tab_hr(1,3) + 1.
   end if
  endif
 endif
enddo

print *, 'Hit rate computed on',icount,'points.'
!print *, 'Number of points ',icount

!  Expressed in percent
if ( (present(lflag).and.(lflag.eqv..True.)).or.(.not.present(lflag)) ) then
tab_hr = (tab_hr / icount)*100. !in %

a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(2,1)
e = tab_hr(2,2)
f = tab_hr(2,3)
g = tab_hr(3,1)
h = tab_hr(3,2)
ii = tab_hr(3,3)
print *, 'X<',xlim1,xlim1,'<X<',xlim2,'X>',xlim2
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3)
pod(1)=a/(a+d+g)*100
print *, 'POD(event1)=', pod(1)
pod(2)=e/(b+e+h)*100
print *, 'POD(event2)=', pod(2)
pod(3)=ii/(c+f+ii)*100
print *, 'POD(event3)=', pod(3)
pc=(a+e+ii)
print*,'PC=',pc
ebd=(c+g)
print*,'ebd=',ebd
endif

! Expressed in numbers
if ( (present(lflag).and.(lflag.eqv..false.)) ) then
tab_hr = tab_hr !in %

a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(2,1)
e = tab_hr(2,2)
f = tab_hr(2,3)
g = tab_hr(3,1)
h = tab_hr(3,2)
ii = tab_hr(3,3)
print *, 'X<',xlim1,xlim1,'<X<',xlim2,'X>',xlim2
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3)
pod(1)=a/(a+d+g)*100
print *, 'POD(event1)=', pod(1)
pod(2)=e/(b+e+h)*100
print *, 'POD(event2)=', pod(2)
pod(3)=ii/(c+f+ii)*100
print *, 'POD(event3)=', pod(3)
pc=((a+e+ii)/icount)*100
print*,'PC=',pc
ebd=((c+g)/icount)*100
print*,'ebd=',ebd
endif

end subroutine hit_rate_mod
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine hit_rate_four(array1,array2,size_array,xlim1,xlim2,xlim3,noval,tab_hr,pod,pc,ebd,lflag)
!Come hit_rate ma per il calcolo di una tablella generica 4x4
!La routine hit_rate_quart si trova in mod_statistics_wd.f90 e serve solo e soltanto per il calcolo della direzione del vento. La
!differenza tra hit_rate_quart e hit_rate_four è il calcolo di EBD
!******************************************************************************
implicit none

logical, intent(in), optional :: lflag !TRUE=with percent, FALSE=number of points
integer, intent(in) :: size_array
real,dimension(size_array), intent(in) :: array1,array2
real, intent(in) :: xlim1,xlim2,xlim3,noval
real, dimension(4,4), intent(out) :: tab_hr
real, dimension(4), intent(out) ::pod
real, intent(out) :: pc, ebd

integer :: i,icount
real :: a,b,c,d,e,f,g,h,ii,j,k,l,m,n,o,p

if (present(lflag)) then
 if (lflag.eqv..true.) print *, 'Contingency table filled with percent.'
 if (lflag.eqv..false.) print *, 'Contingency table filled with numbers.'
else
 print *, 'Contingency table filled with percent.'
endif

tab_hr=0
icount=0
do i=1,size_array
 if ( (array1(i).ne.noval).and.(array2(i).ne.noval) ) then
  icount = icount + 1
  if (array1(i).le.xlim1) then
   if (array2(i).le.xlim1)                              tab_hr(1,1) = tab_hr(1,1) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,1) = tab_hr(2,1) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,1) = tab_hr(3,1) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,1) = tab_hr(4,1) + 1
  endif
  if ( (array1(i).gt.xlim1).and.(array1(i).le.xlim2) ) then
   if (array2(i).le.xlim1)                              tab_hr(1,2) = tab_hr(1,2) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,2) = tab_hr(2,2) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,2) = tab_hr(3,2) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,2) = tab_hr(4,2) + 1
  endif
  if ( (array1(i).gt.xlim2).and.(array1(i).le.xlim3) ) then
   if (array2(i).le.xlim1)                              tab_hr(1,3) = tab_hr(1,3) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,3) = tab_hr(2,3) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,3) = tab_hr(3,3) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,3) = tab_hr(4,3) + 1
  endif
   if (array1(i).gt.xlim3) then
   if (array2(i).le.xlim1)                              tab_hr(1,4) = tab_hr(1,4) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,4) = tab_hr(2,4) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,4) = tab_hr(3,4) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,4) = tab_hr(4,4) + 1
  endif
 endif
enddo

print *, 'Hit rate computed on',icount,'points.'
!print *, 'Number of points ',icount

if ( (present(lflag).and.(lflag.eqv..True.)).or.(.not.present(lflag)) ) then
tab_hr = (tab_hr / icount)*100. !in %
a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(1,4)
e = tab_hr(2,1)
f = tab_hr(2,2)
g = tab_hr(2,3)
h = tab_hr(2,4)
ii = tab_hr(3,1)
j = tab_hr(3,2)
k = tab_hr(3,3)
l = tab_hr(3,4)
m = tab_hr(4,1)
n = tab_hr(4,2)
o = tab_hr(4,3)
p = tab_hr(4,4)

print *, 'X<',xlim1,'<X<',xlim2,'<X<',xlim3,'<X'
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3),tab_hr(1,4)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3),tab_hr(2,4)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3),tab_hr(3,4)
print *, tab_hr(4,1),tab_hr(4,2),tab_hr(4,3),tab_hr(4,4)
pod(1)=a/(a+e+ii+m)
print *, 'POD(event1)=', pod(1)
pod(2)=f/(b+f+j+n)
print *, 'POD(event2)=', pod(2)
pod(3)=k/(c+g+k+o)
print *, 'POD(event3)=', pod(3)
pod(4)=p/(d+h+l+p)
print *, 'POD(event4)=', pod(4)
pc=(a+f+k+p)
print*,'PC=',pc
ebd=(c+h+ii+n+d+m)
print*,'ebd=',ebd
endif

! Expressed in numbers
if ( (present(lflag).and.(lflag.eqv..false.)) ) then
tab_hr = tab_hr !in %
a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(1,4)
e = tab_hr(2,1)
f = tab_hr(2,2)
g = tab_hr(2,3)
h = tab_hr(2,4)
ii = tab_hr(3,1)
j = tab_hr(3,2)
k = tab_hr(3,3)
l = tab_hr(3,4)
m = tab_hr(4,1)
n = tab_hr(4,2)
o = tab_hr(4,3)
p = tab_hr(4,4)

print *, 'X<',xlim1,'<X<',xlim2,'<X<',xlim3,'<X'
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3),tab_hr(1,4)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3),tab_hr(2,4)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3),tab_hr(3,4)
print *, tab_hr(4,1),tab_hr(4,2),tab_hr(4,3),tab_hr(4,4)
pod(1)=(a/(a+e+ii+m))*100
print *, 'POD(event1)=', pod(1)
pod(2)=(f/(b+f+j+n))*100
print *, 'POD(event2)=', pod(2)
pod(3)=(k/(c+g+k+o))*100
print *, 'POD(event3)=', pod(3)
pod(4)=(p/(d+h+l+p))*100
print *, 'POD(event4)=', pod(4)
pc=((a+f+k+p)/icount)*100
print*,'PC=',pc
ebd=((c+h+ii+n+d+m)/icount)*100
print*,'ebd=',ebd
endif

end subroutine hit_rate_four
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine hit_rate_quart(array1,array2,size_array,xlim1,xlim2,xlim3,noval,tab_hr,pod,pc,ebd,lflag)
!array1=reference (obs) from which xlim1 and xlim2 and xlim3 have been extracted
!******************************************************************************
implicit none

logical, intent(in), optional :: lflag !TRUE=with percent, FALSE=number of points
integer, intent(in) :: size_array
real,dimension(size_array), intent(in) :: array1,array2
real, intent(in) :: xlim1,xlim2,xlim3,noval
real, dimension(4,4), intent(out) :: tab_hr
real, dimension(4), intent(out) ::pod
real, intent(out) :: pc, ebd

integer :: i,icount
real :: a,b,c,d,e,f,g,h,ii,j,k,l,m,n,o,p

if (present(lflag)) then
 if (lflag.eqv..true.) print *, 'Contingency table filled with percent.'
 if (lflag.eqv..false.) print *, 'Contingency table filled with numbers.'
else
 print *, 'Contingency table filled with percent.'
endif

tab_hr=0
icount=0
do i=1,size_array
 if ( (array1(i).ne.noval).and.(array2(i).ne.noval) ) then
  icount = icount + 1
  if (array1(i).le.xlim1) then
   if (array2(i).le.xlim1)                              tab_hr(1,1) = tab_hr(1,1) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,1) = tab_hr(2,1) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,1) = tab_hr(3,1) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,1) = tab_hr(4,1) + 1
  endif
  if ( (array1(i).gt.xlim1).and.(array1(i).le.xlim2) ) then
   if (array2(i).le.xlim1)                              tab_hr(1,2) = tab_hr(1,2) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,2) = tab_hr(2,2) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,2) = tab_hr(3,2) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,2) = tab_hr(4,2) + 1
  endif
  if ( (array1(i).gt.xlim2).and.(array1(i).le.xlim3) ) then
   if (array2(i).le.xlim1)                              tab_hr(1,3) = tab_hr(1,3) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,3) = tab_hr(2,3) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,3) = tab_hr(3,3) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,3) = tab_hr(4,3) + 1
  endif
   if (array1(i).gt.xlim3) then
   if (array2(i).le.xlim1)                              tab_hr(1,4) = tab_hr(1,4) + 1
   if ( (array2(i).gt.xlim1).and.(array2(i).le.xlim2) ) tab_hr(2,4) = tab_hr(2,4) + 1
   if ( (array2(i).gt.xlim2).and.(array2(i).le.xlim3) ) tab_hr(3,4) = tab_hr(3,4) + 1
   if (array2(i).gt.xlim3)                              tab_hr(4,4) = tab_hr(4,4) + 1
  endif
 endif
enddo

!print *, 'Hit rate computed on',icount,'points.'
print *, 'Number of points ',icount,';'

if ( (present(lflag).and.(lflag.eqv..True.)).or.(.not.present(lflag)) ) then
tab_hr = (tab_hr / icount)*100. !in %
a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(1,4)
e = tab_hr(2,1)
f = tab_hr(2,2)
g = tab_hr(2,3)
h = tab_hr(2,4)
ii = tab_hr(3,1)
j = tab_hr(3,2)
k = tab_hr(3,3)
l = tab_hr(3,4)
m = tab_hr(4,1)
n = tab_hr(4,2)
o = tab_hr(4,3)
p = tab_hr(4,4)

print *, 'X<',xlim1,'<X<',xlim2,'<X<',xlim3,'<X'
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3),tab_hr(1,4)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3),tab_hr(2,4)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3),tab_hr(3,4)
print *, tab_hr(4,1),tab_hr(4,2),tab_hr(4,3),tab_hr(4,4)
pod(1)=a/(a+e+ii+m)
print *, 'POD(event1)=', pod(1)
pod(2)=f/(b+f+j+n)
print *, 'POD(event2)=', pod(2)
pod(3)=k/(c+g+k+o)
print *, 'POD(event3)=', pod(3)
pod(4)=p/(d+h+l+p)
print *, 'POD(event4)=', pod(4)
pc=(a+f+k+p)
print*,'PC=',pc
ebd=(c+h+ii+n)
print*,'ebd=',ebd
endif

! Expressed in numbers
if ( (present(lflag).and.(lflag.eqv..false.)) ) then
tab_hr = tab_hr !in %
a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(1,3)
d = tab_hr(1,4)
e = tab_hr(2,1)
f = tab_hr(2,2)
g = tab_hr(2,3)
h = tab_hr(2,4)
ii = tab_hr(3,1)
j = tab_hr(3,2)
k = tab_hr(3,3)
l = tab_hr(3,4)
m = tab_hr(4,1)
n = tab_hr(4,2)
o = tab_hr(4,3)
p = tab_hr(4,4)

print *, 'X<',xlim1,'<X<',xlim2,'<X<',xlim3,'<X'
print *, tab_hr(1,1),tab_hr(1,2),tab_hr(1,3),tab_hr(1,4)
print *, tab_hr(2,1),tab_hr(2,2),tab_hr(2,3),tab_hr(2,4)
print *, tab_hr(3,1),tab_hr(3,2),tab_hr(3,3),tab_hr(3,4)
print *, tab_hr(4,1),tab_hr(4,2),tab_hr(4,3),tab_hr(4,4)
pod(1)=(a/(a+e+ii+m))*100
print *, 'POD(event1)=', pod(1)
pod(2)=(f/(b+f+j+n))*100
print *, 'POD(event2)=', pod(2)
pod(3)=(k/(c+g+k+o))*100
print *, 'POD(event3)=', pod(3)
pod(4)=(p/(d+h+l+p))*100
print *, 'POD(event4)=', pod(4)
pc=((a+f+k+p)/icount)*100
print*,'PC=',pc
ebd=((c+h+ii+n)/icount)*100
print*,'ebd=',ebd
endif

end subroutine hit_rate_quart
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine contingency(array1,array2,size_array,xlim,noval,tab_hr,statnum,lflag)
!array1=reference (obs) from which xlim has been extracted
!******************************************************************************
implicit none

logical, intent(in), optional :: lflag !TRUE=with percent, FALSE=number of points
integer, intent(in) :: size_array
real,dimension(size_array), intent(in) :: array1,array2
real, intent(in) :: xlim,noval
real, dimension(2,2), intent(out) :: tab_hr
real, dimension(11), intent(out)   :: statnum
!statnum(1) = accuracy ACC
!statnum(2) = hit rate (probability of detection) POD
!statnum(3) = false alarm ratio (Fraction of forecasted events that were false alarms) FAR
!statnum(4) = false alarm rate (probability of false detection) POFD
!statnum(5) = frequency (bias) FBIAS
!statnum(6) = threat score (critical success index) CSI
!statnum(7) = Heidke Skill Score HSC
!statnum(8) = Hanssen-Kuipers Discriminant HKD
!statnum(9) = Odds Ratio
!statnum(10)= OR Skill Score
!statnum(11)= Gilber SS or ETS

!Local variables
integer :: i,icount
real :: a,b,c,d,N,ar

if (present(lflag)) then
 if (lflag.eqv..true.) print *, 'Contingency table filled with percent.'
 if (lflag.eqv..false.) print *, 'Contingency table filled with numbers.'
else
 print *, 'Contingency table filled with percent.'
endif

tab_hr=0
icount=0
do i=1,size_array
 if ( (array1(i).ne.noval).and.(array2(i).ne.noval) ) then
  icount = icount + 1
  if (array1(i).le.xlim) then
   if (array2(i).le.xlim)                              tab_hr(1,1) = tab_hr(1,1) + 1
   if (array2(i).gt.xlim)                              tab_hr(2,1) = tab_hr(2,1) + 1
  endif
  if (array1(i).gt.xlim) then
   if (array2(i).le.xlim)                              tab_hr(1,2) = tab_hr(1,2) + 1
   if (array2(i).gt.xlim)                              tab_hr(2,2) = tab_hr(2,2) + 1
  endif
 endif
enddo

print *, 'Hit rate computed on',icount,'points.'
!print *, 'Number of points ',icount

a = tab_hr(1,1)
b = tab_hr(1,2)
c = tab_hr(2,1)
d = tab_hr(2,2)
N = icount
ar = (a+b)*(a+c) / N !number of hits expected by forecasts independent of observations (pure chance)

statnum(1) = (a + d) / N      !ACC, or PC (Percent Correct)  
statnum(2) = a / (a + c)      !POD
statnum(3) = b / (a + b)      !FAR
statnum(4) = b / (b + d)      !POFD
statnum(5) = (a + b) / (a + c) !FBIAS
statnum(6) = a / (a + b + c)    !CSI
statnum(7) = ( (a*d) - (b*c) ) / ( (((a+c)*(c+d)) + ((a+b)*(b+d))) / 2 )  !HSC
statnum(8) = ( (a*d) - (b*c) ) / ( (a+c)*(b+d) ) !HKD = POD-POFD, or Peirce's Skill Score
statnum(9) = (a*d) / (b*c) ! OR (Odds Ratio)
statnum(10)= ( (a*d) - (b*c) ) / ( (a*d) + (b*c) ) ! OR Skill Score
statnum(11)= (a - ar) / (a -ar + b + c) ! GSS (Gilbert Skill Score) or ETS (Equitable Threat Score)

!tab_hr = (tab_hr / icount)*100. !in %
if ( (present(lflag).and.(lflag.eqv..True.)).or.(.not.present(lflag)) ) tab_hr = (tab_hr / icount)*100. !in %

print *, '------------'
print *, 'LOGINFO X<',xlim,xlim,'<X<'
print *, tab_hr(1,1),tab_hr(1,2)
print *, tab_hr(2,1),tab_hr(2,2)
print *, '------------'
print *, 'PC (Percent Correct) =', statnum(1)
print *, 'POD (or Hit Rate) =',statnum(2)
print *, 'M (Miss Rate) = 1 - POD =', 1-statnum(2)
print *, 'FAR (False Alarm Ratio) =', statnum(3)
print *, 'F (False Alarm Rate, or POFD) =', statnum(4)
print *, 'Bias =', statnum(5)
print *, '------------'
print *, 'CSI (Critical success index) =', statnum(6)
print *, 'HSS (Heidke Skill Score) = (PC-PCrandom)/(1-PCrandom) =', statnum(7)
print *, 'Peirce Skill Score = 1 - M - F =', statnum(8)
print *, 'Odds Ratio (OR) =', statnum(9)
print *, 'OR Skill Score (ORSS) =', statnum(10)
print *, 'Number of hits expected by forecasts independent of observations (pure chance), ar =', ar
print *, 'ETS ( or GSS = HSS / (2 - HSS) ) =', statnum(11)


end subroutine contingency
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE CONVOL(X,Y,NDIM,XC,DX,YC,NB,NOVAL)
implicit none
!****************************************************
!* LEGENDE
!*    x: variable indépendante
!*    y: variable à convoluer
!*    ndim: nombre de points de départ
!*    xc: point de départ
!*    dx: base du triangle sur lequel on fait la convolution (mètres) (input)
!*    yc: valeur ponctuelle de la variable convoluée
!*    nb: nombre de points sur lesquels on fait la convolution (output)
!****************************************************

INTEGER, INTENT(IN)                ::  NDIM
INTEGER, INTENT(OUT)               ::  NB
REAL, DIMENSION(:), INTENT(IN)     ::  X(NDIM),Y(NDIM)
REAL, INTENT(IN)                   ::  DX
REAL, INTENT(IN)                   ::  XC
REAL, INTENT(IN) :: NOVAL
REAL, INTENT(OUT)                  ::  YC

REAL                               ::  XDEB,XFIN,DXS2,XSUM,YR
REAL :: YI,XI,YI1,XI1
INTEGER :: I
REAL :: TR,TR1

DXS2=DX/2.
YR=0.
NB=0
XSUM=0.

XDEB=XC-DXS2
XFIN=XC+DXS2


DO I=1,NDIM-1
   YI=Y(I)
   XI=X(I)
   YI1=Y(I+1)
   XI1=X(I+1)

   IF(XFIN.LT.X(1))EXIT
   IF(XDEB.GT.X(NDIM))EXIT
   IF(XI1.GT.XFIN)EXIT

   IF(XI.GE.XDEB.AND.XI1.LE.XFIN)THEN
      TR=(1.-(ABS(XI-XC)/DXS2))
      TR1=(1.-(ABS(XI1-XC)/DXS2))
      NB=NB+1
      IF((YI.ne.NOVAL).and.(YI1.ne.NOVAL)) THEN 
       YR=YR+0.5*(YI*TR+YI1*TR1)*(XI1-XI)
       XSUM=XSUM+0.5*(TR+TR1)*(XI1-XI)
      ENDIF
   ENDIF
ENDDO

YC=NOVAL
IF(YR.ne.0.) THEN 
 IF(NB.EQ.0)THEN
  YC=0.
 ELSE
  YC=YR/XSUM
 ENDIF
ENDIF
END SUBROUTINE CONVOL
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE ITP(LevIni,LevInt,ARRIni,ALTIni,ARRInt,ALTInt,delta,NOVAL)
!Input vector ARRIni of dimension LevIni; interpolated on LevInt levels, ouptut vector: ARRInt
!-noval: if values at z or z+1 (of ARRIni) is equal noval, no interpolation is done between the 2 points and all resulted 
!interpolated levels between z and z+1 (so for ARRInt) are put equal to noval
!-delta: the resolution of the interpolated grid  
!!!! ALTInt first grid point has to be equal to the first grid point of AltIni !!!!
! Routine created to reinterpolate the Meso-Nh grid with a regular grid having the grid equal to the 
! smallest grid point of the Meso-Nh grid i..e the first grid point above the ground.
!******************************************************************************
implicit none
INTEGER, INTENT(IN)  :: LevIni,LevInt
REAL, INTENT(IN) :: delta,NOVAL

REAL, DIMENSION(:), INTENT(IN)  :: ARRIni,ALTIni
REAL, DIMENSION(:), INTENT(OUT) :: ARRInt,ALTInt

INTEGER :: ICOUNT, JCOUNT, LevInf
REAL :: lmin,lmax,d1,d2

DO ICOUNT=1,LevInt
 ALTInt(ICOUNT)=ALTIni(1) - delta + (ICOUNT*delta)
 ARRInt(ICOUNT)=0.
ENDDO

ARRInt(1)=ARRIni(1)

DO ICOUNT=1, LevInt
 DO JCOUNT=1, LevIni-1
  IF (ALTIni(JCOUNT)<=ALTInt(ICOUNT)) THEN
   lmin=ARRIni(JCOUNT)
   LevInf=JCOUNT
  ENDIF
  lmax=ARRIni(LevInf+1)
 ENDDO
 IF (ALTInt(ICOUNT)==ALTIni(LevInf)) THEN
  ARRInt(ICOUNT)=ARRIni(LevInf)
 ELSE

  if ( (lmin.ne.noval).and.(lmax.ne.noval) ) then  !loop test noval

   d1=ALTInt(ICOUNT)-ALTIni(LevInf)
   d2=ALTIni(LevInf+1)-ALTInt(ICOUNT)
   ARRInt(ICOUNT)=(lmin/d1 + lmax/d2) / (1/d1 + 1/d2)

  else !else loop test noval

   ARRInt(ICOUNT)=noval

  endif !loop test noval 

 ENDIF
ENDDO
END SUBROUTINE ITP
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE ITP_dp(LevIni,LevInt,ARRIni,ALTIni,ARRInt,ALTInt,delta)
! Routine equal to ITP but for inputs in double precision
!******************************************************************************
implicit none
INTEGER, INTENT(IN)  :: LevIni,LevInt
REAL, INTENT(IN) :: delta

DOUBLE PRECISION, DIMENSION(:), INTENT(IN)  :: ARRIni,ALTIni
DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: ARRInt,ALTInt

INTEGER :: ICOUNT, JCOUNT, LevInf
DOUBLE PRECISION :: lmin,lmax,d1,d2

DO ICOUNT=1,LevInt
 ALTInt(ICOUNT)=ALTIni(1) - delta + (ICOUNT*delta)
 ARRInt(ICOUNT)=0.
ENDDO

ARRInt(1)=ARRIni(1)

DO ICOUNT=1, LevInt
 DO JCOUNT=1, LevIni-1
  IF (ALTIni(JCOUNT)<=ALTInt(ICOUNT)) THEN
   lmin=ARRIni(JCOUNT)
   LevInf=JCOUNT
  ENDIF
  lmax=ARRIni(LevInf+1)
 ENDDO
 IF (ALTInt(ICOUNT)==ALTIni(LevInf)) THEN
  ARRInt(ICOUNT)=ARRIni(LevInf)
 ELSE
  d1=ALTInt(ICOUNT)-ALTIni(LevInf)
  d2=ALTIni(LevInf+1)-ALTInt(ICOUNT)
  ARRInt(ICOUNT)=(lmin/d1 + lmax/d2) / (1/d1 + 1/d2)
 ENDIF
ENDDO
END SUBROUTINE ITP_dp

!******************************************************************************
!******************************************************************************
subroutine irregular_itp(LevIni,LevInt,ARRIni,ALTIni,ALTInt,ARRInt,NOVAL)
!Interpolates a vertical profile of LevIni points (Altitude=ALTIni and Value=ARRIni) on a new vertical grid which levels 
!are defined in ALTInt. ALTInt is an input parameter. The output is a vector of reinterpolated values (ARRInt).
!******************************************************************************
implicit none

integer, intent(in)  :: LevIni,LevInt

real, dimension(LevIni), intent(in)  :: ARRIni,ALTIni
real, dimension(LevInt), intent(in)  :: ALTInt
real, dimension(LevInt), intent(out) :: ARRInt
real, intent(in) :: NOVAL

integer :: icount, jcount, LevInf, LevMax
real :: lmin,lmax,d1,d2

!print *, 'Number of level for output matrice:', LevInt
do icount=1, LevInt
 do jcount=1, LevIni
  if (ALTIni(jcount).gt.ALTInt(icount)) then
!   print *, 'WARNING!!'
!   print *, 'The value of the first altitude of the interpolated field is inferior'
!   print *, 'to the value of the first altitude of the initial field.'
!   print *, 'ALTIni(jcount)=',ALTIni(jcount),'and ALTInt(icount)=',ALTInt(icount),'with jcount / LevIni=',jcount, &
!            'and icount / LevInt=',icount
!   print *, 'PROGRAM ENDS NOW.'
   exit
  endif
  if (ALTIni(jcount)<=ALTInt(icount)) then
   LevInf=jcount
   lmin=ARRIni(LevInf)
  endif
  if (LevInf.lt.LevIni) then 
   LevMax=LevInf+1
  else 
   LevMax=LevInf
  endif
  lmax=ARRIni(LevMax)
 enddo
 if (LevMax.gt.LevInf) then !Check of array bounds
  if (ALTInt(icount)==ALTIni(LevInf)) then
   ARRInt(icount)=ARRIni(LevInf)
  else

   if ( (lmin.ne.noval).and.(lmax.ne.noval) ) then  !loop test noval
 
    d1=ALTInt(icount)-ALTIni(LevInf)
    d2=ALTIni(LevMax)-ALTInt(icount)
    ARRInt(icount)=(lmin/d1 + lmax/d2) / (1/d1 + 1/d2)
!    print*, "A",lmin,lmax,icount
  
   else !else loop test noval
 
    ARRInt(ICOUNT)=noval

   endif !loop test noval 

  endif
 else
  ARRInt(icount)=noval
 endif
 if ( (LevMax.eq.LevInf).and.(ALTInt(icount).eq.ALTIni(LevInf)) ) ARRInt(icount)=ARRIni(LevInf) !Very rare case when the last level of the input field has the same altitude than the last level of the output field.
enddo
end subroutine irregular_itp
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine irregular_itp_dp(LevIni,LevInt,ARRIni,ALTIni,ALTInt,ARRInt)
!******************************************************************************
implicit none

integer, intent(in)  :: LevIni,LevInt

double precision, dimension(LevIni), intent(in)  :: ARRIni,ALTIni
double precision, dimension(LevInt), intent(in)  :: ALTInt
double precision, dimension(LevInt), intent(out) :: ARRInt

integer :: icount, jcount, LevInf, LevMax
double precision :: lmin,lmax,d1,d2

print *, 'Number of level for output matrice:', LevInt
do icount=1, LevInt
 do jcount=1, LevIni
  if (ALTIni(jcount).gt.ALTInt(icount)) then
!   print *, 'WARNING!!'
!   print *, 'The value of the first altitude of the interpolated field is inferior'
!   print *, 'to the value of the first altitude of the initial field.'
!   print *, 'ALTIni(jcount)=',ALTIni(jcount),'and ALTInt(icount)=',ALTInt(icount),'with jcount / LevIni=',jcount, &
!            'and icount / LevInt=',icount
!   print *, 'PROGRAM ENDS NOW.'
   exit
  endif
  if (ALTIni(jcount)<=ALTInt(icount)) then
   LevInf=jcount
   lmin=ARRIni(LevInf)
  endif
  if (LevInf.lt.LevIni) then
   LevMax=LevInf+1
  else
   LevMax=LevInf
  endif
  lmax=ARRIni(LevMax)
 enddo
 if (LevMax.gt.LevInf) then !Check of array bounds
  if (ALTInt(icount)==ALTIni(LevInf)) then
   ARRInt(icount)=ARRIni(LevInf)
  else
   d1=ALTInt(icount)-ALTIni(LevInf)
   d2=ALTIni(LevMax)-ALTInt(icount)
   ARRInt(icount)=(lmin/d1 + lmax/d2) / (1/d1 + 1/d2)
  endif
 else
  ARRInt(icount)=-9999.
 endif
 if ( (LevMax.eq.LevInf).and.(ALTInt(icount).eq.ALTIni(LevInf)) ) ARRInt(icount)=ARRIni(LevInf) !Very rare case when the last level of the input field has the same altitude than the last level of the output field.
enddo
end subroutine irregular_itp_dp
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE ITP_CIRCULAR(LevIni,LevInt,ARRIni,ALTIni,ARRInt,ALTInt,delta,xmodulo,noval)
!Interpolation of a circular variable
!Input vector ARRIni of dimension LevIni; interpolated on LevInt levels, ouptut vector: ARRInt
!-noval: if values at z or z+1 (of ARRIni) is equal noval, no interpolation is done between the 2 points and all resulted 
!interpolated levels between z and z+1 (so for ARRInt) are put equal to noval
!-xmodulo: for the wind direction, please put xmodulo=360 (in degrees)
!-delta: the resolution of the interpolated grid  
!******************************************************************************
implicit none
INTEGER, INTENT(IN)  :: LevIni,LevInt
REAL, INTENT(IN) :: delta,xmodulo,noval

REAL, DIMENSION(:), INTENT(IN)  :: ARRIni,ALTIni
REAL, DIMENSION(:), INTENT(OUT) :: ARRInt,ALTInt

INTEGER :: ICOUNT, JCOUNT, LevInf
REAL :: lmin,lmax,d1,d2

DO ICOUNT=1,LevInt !construction of the interpolated grid with a resolution of delta
 ALTInt(ICOUNT)=ALTIni(1) - delta + (ICOUNT*delta)
 ARRInt(ICOUNT)=0.
ENDDO

ARRInt(1)=ARRIni(1)

DO ICOUNT=1, LevInt
 DO JCOUNT=1, LevIni-1
  IF (ALTIni(JCOUNT)<=ALTInt(ICOUNT)) THEN
   lmin=ARRIni(JCOUNT)
   LevInf=JCOUNT
  ENDIF
  lmax=ARRIni(LevInf+1)
 ENDDO
 IF (ALTInt(ICOUNT)==ALTIni(LevInf)) THEN
  ARRInt(ICOUNT)=ARRIni(LevInf)
 ELSE

  if ( (lmin.ne.noval).and.(lmax.ne.noval) ) then  !loop test noval

  d1=ALTInt(ICOUNT)-ALTIni(LevInf)
  d2=ALTIni(LevInf+1)-ALTInt(ICOUNT)
  if(abs(lmax-lmin).gt.(xmodulo/2.)) then 
!   print *, 'Routine ITP_CIRCULAR activated, |lmin-lmax|>',xmodulo/2,'between levels',LevInf,'and',LevInf+1
!   print *, 'lmin at level',LevInf,'=',lmin,'and lmax at level',LevInf+1,'=',lmax
   if(lmin.lt.(xmodulo/2.)) then 
    lmin = lmin + xmodulo
   else
    lmax = lmax + xmodulo
   endif
   ARRInt(ICOUNT) = (lmin/d1 + lmax/d2) / (1/d1 + 1/d2)
   if (ARRInt(ICOUNT).gt.xmodulo) ARRInt(ICOUNT) = ARRInt(ICOUNT) - xmodulo
!   print *, 'Interpolated value=',ARRInt(ICOUNT),'between lmin(mod)=',lmin,'and lmax(mod)=',lmax
  else
   ARRInt(ICOUNT)=(lmin/d1 + lmax/d2) / (1/d1 + 1/d2)
  endif

  else !else loop test noval

  ARRInt(ICOUNT)=noval

  endif !loop test noval   

 ENDIF
ENDDO
END SUBROUTINE ITP_CIRCULAR
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE RESAMPLING_AVERAGE(ARRAY,SIZE_ARRAY,NP,RS_ARRAY,SIZE_RS_ARRAY,SD_ARRAY,NO_VALUE)
!Resampling of a data set by computing the average every NP points by NP points.
!New dimension is equal to SIZE_ARRAY/NP or INT(SIZE_ARRAY/NP) + 1.
!ATTENTION: the input parameters must verify SIZE_RS_ARRAY * NP = SIZE_ARRAY
!call resampling_average(input_array,size_input_array,number_of_points_for_the_average,output_array,size_output_array,sigma_array,
!value_to_be_discarded_from_average_computation)
!******************************************************************************
! use mod_statistics
 implicit none

 integer,                     intent(in)  :: size_array     ! size of the input array
 integer,                     intent(in)  :: size_rs_array  ! size of the output array < size of the input array
 integer,                     intent(in)  :: np             ! number of points used in the computation of the average
 real,                        intent(in)  :: no_value       ! discarded value in the computation of the moving average
 real, dimension(size_array), intent(in)  :: array          ! input array

 real, dimension(size_rs_array),    intent(out)  :: rs_array       ! output array
 real, dimension(size_rs_array),    intent(out)  :: sd_array       ! standard deviation of the output values


 integer                                  :: i,j,inc
 integer                                  :: ibeg,iend
 integer, dimension(size_rs_array)        :: inc_a

if ( (size_rs_array*np).ne.size_array ) then
  print *, 'ALARM! ALARM! ALARM! Wrong size for output array. Please check.'
  print *, 'It should be', int(size_array / np),'.'
  print *, 'Input array dimension  = ',size_array
  print *, 'Output array dimension = ',size_rs_array
  print *, 'Number of point used in the computation of the average = ', np
  print *, 'Input array dimension / number of points used for the average = ',size_array/np
  stop
 endif

 rs_array=no_value
 sd_array=0

 do i=1,size_rs_array
  ibeg = i*np - np + 1
  iend = i*np
 inc=0
 do j=ibeg,iend
  if (array(j).ne.no_value) inc=inc+1
 enddo
  if (inc.ne.0.) call stats(array(ibeg:iend),np,rs_array(i),sd_array(i),inc_a(i),no_value)
 enddo
END SUBROUTINE RESAMPLING_AVERAGE
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE RESAMPLING_AVERAGE_WIND_DIRECTION(VX,VY,SIZE_ARRAY,NP,RS_WD,SIZE_RS_WD,SD_WD,NO_VALUE)
!Resampling of a data set by computing the average every NP points by NP points.
!New dimension is equal to SIZE_ARRAY/NP or INT(SIZE_ARRAY/NP) + 1.
!ATTENTION: the input parameters must verify SIZE_RS_ARRAY * NP = SIZE_ARRAY
!call resampling_average(input_array,size_input_array,number_of_points_for_the_average,output_array,size_output_array,sigma_array,
!value_to_be_discarded_from_average_computation)
!******************************************************************************
! use mod_statistics_wd
 implicit none
 integer,                     intent(in)  :: size_array     ! size of the input array
 integer,                     intent(in)  :: size_rs_wd     ! size of the output array < size of the input array
 integer,                     intent(in)  :: np             ! number of points used in the computation of the average
 real,                        intent(in)  :: no_value       ! discarded value in the computation of the moving average
 real, dimension(size_array), intent(in)  :: vx,vy          ! input array

 real, dimension(size_rs_wd),    intent(out)  :: rs_wd          ! output array
 real, dimension(size_rs_wd),    intent(out)  :: sd_wd          ! standard deviation of the output values
 


 integer                                  :: i,j,inc
 integer                                  :: ibeg,iend
 integer, dimension(size_rs_wd)        :: inc_a


if ( (size_rs_wd*np).ne.size_array ) then
  print *, 'ALARM! ALARM! ALARM! Wrong size for output array. Please check.'
  print *, 'It should be', int(size_array / np),'.'
  print *, 'Input array dimension  = ',size_array
  print *, 'Output array dimension = ',size_rs_wd  
  print *, 'Number of point used in the computation of the average = ', np
  print *, 'Input array dimension / number of points used for the average = ',size_array/np
  stop
 endif

 rs_wd=no_value
 sd_wd=0

 do i=1,size_rs_wd
  ibeg = i*np - np + 1
  iend = i*np
 inc=0
 do j=ibeg,iend
  if ( (vx(j).ne.no_value).and.(vy(j).ne.no_value) ) inc=inc+1
 enddo
  if (inc.ne.0.) call stats_wind_direction(vx(ibeg:iend),vy(ibeg:iend),np,rs_wd(i),sd_wd(i),inc_a(i),no_value)
 enddo
END SUBROUTINE RESAMPLING_AVERAGE_WIND_DIRECTION
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE MOVING_AVERAGE(ARRAY,SIZE_ARRAY,NP_M_A,M_A_ARRAY,NO_VALUE)
!Compute a moving average of data.
!How to call it:
!call moving_average(input_array,size_of_input_array,N,output_array,discarded_value)
!Average at i€[1,size_of_input_array] is computed over the [i-N,i+N] interval (2N+1 points); 
!'output_array' has the same dimension as input_array;
!'discarded_value' is the value used to discard points from the average computation. A test is perform to see 
!wether it exists at least one point different from discarded_value in the interval [i-N,...,i,...,i+N]. If not, 
!then output_array(i)=discarded_value.
!******************************************************************************
! use mod_statistics
 implicit none

 integer,                     intent(in)  :: size_array  ! size of the input array
 integer,                     intent(in)  :: np_m_a      ! number of points used in the computation of the moving average = 2*np_m_a + 1
                                                         ! [i-np_m_a,.....,i,......,i+np_m_a] ----> i
 real,                        intent(in)  :: no_value    ! discarded value in the computation of the moving average
 real, dimension(size_array), intent(in)  :: array       ! input array
 
 real, dimension(size_array), intent(out) :: m_a_array   ! output array

 integer                                  :: i,j,itot,inc
 real, dimension(size_array)              :: sd_m_a
 real, dimension(2*np_m_a + 1)            :: zero   
 integer, dimension(size_array)           :: inc_m_a


 m_a_array = no_value
 sd_m_a    = 0.        
 inc_m_a   = 0
 zero = no_value
 
 do i=1,np_m_a
 inc=0
  do j=1,i+np_m_a
   if (array(j).ne.no_value) inc=inc+1
  enddo
  if (inc.ne.0.) call stats(array(1:i+np_m_a),i+np_m_a,m_a_array(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 itot=2*np_m_a+1
 do i=np_m_a+1,size_array-np_m_a-1
  inc=0
  do j=i-np_m_a,i+np_m_a
   if (array(j).ne.no_value) inc=inc+1
  enddo
  if (inc.ne.0) call stats(array(i-np_m_a:i+np_m_a),itot,m_a_array(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 do i=size_array-np_m_a,size_array
  inc=0
  do j=i-np_m_a,size_array
   if (array(j).ne.no_value) inc=inc+1
  enddo
  if (inc.ne.0.) call stats(array(i-np_m_a:size_array),size_array-i+np_m_a+1,m_a_array(i),sd_m_a(i),inc_m_a(i),no_value)
  if (inc.eq.0.) m_a_array(i)=no_value
 enddo
END SUBROUTINE MOVING_AVERAGE
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE MOVING_AVERAGE_SD(ARRAY,SIZE_ARRAY,NP_M_A,M_A_ARRAY,sd_m_a,NO_VALUE)
!Identical to the preceding routine, except for the extra output variable SD_M_A
!******************************************************************************
! use mod_statistics
 implicit none

 integer,                     intent(in)  :: size_array  ! size of the input array
 integer,                     intent(in)  :: np_m_a      ! number of points used in the computation of the moving average = 2*np_m_a + 1
                                                         ! [i-np_m_a,.....,i,......,i+np_m_a] ----> i
 real,                        intent(in)  :: no_value    ! discarded value in the computation of the moving average
 real, dimension(size_array), intent(in)  :: array       ! input array
 
 real, dimension(size_array), intent(out) :: m_a_array   ! output array

 integer                                  :: i,j,itot,inc
 real, dimension(size_array), intent(out) :: sd_m_a
 real, dimension(2*np_m_a + 1)            :: zero   
 integer, dimension(size_array)           :: inc_m_a


 m_a_array = no_value
 sd_m_a    = 0.        
 inc_m_a   = 0
 zero = no_value
 
 do i=1,np_m_a
 inc=0
  do j=1,i+np_m_a
   if (array(j).ne.no_value) inc=inc+1
  enddo
  if (inc.ne.0.) call stats(array(1:i+np_m_a),i+np_m_a,m_a_array(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 itot=2*np_m_a+1
 do i=np_m_a+1,size_array-np_m_a-1
  inc=0
  do j=i-np_m_a,i+np_m_a
   if (array(j).ne.no_value) inc=inc+1
  enddo
  if (inc.ne.0) call stats(array(i-np_m_a:i+np_m_a),itot,m_a_array(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 do i=size_array-np_m_a,size_array
  inc=0
  do j=i-np_m_a,size_array
   if (array(j).ne.no_value) inc=inc+1
  enddo
  if (inc.ne.0.) call stats(array(i-np_m_a:size_array),size_array-i+np_m_a+1,m_a_array(i),sd_m_a(i),inc_m_a(i),no_value)
  if (inc.eq.0.) m_a_array(i)=no_value
 enddo
END SUBROUTINE MOVING_AVERAGE_SD
!******************************************************************************

!******************************************************************************
!******************************************************************************
SUBROUTINE MOVING_AVERAGE_WIND_DIRECTION(VX,VY,SIZE_ARRAY,NP_M_A,M_A_WD,NO_VALUE)
!Compute a moving average of data.
!How to call it:
!call moving_average(input_array,size_of_input_array,N,output_array,discarded_value)
!Average at i€[1,size_of_input_array] is computed over the [i-N,i+N] interval (2N+1 points); 
!'output_array' has the same dimension as input_array;
!'discarded_value' is the value used to discard points from the average computation. A test is perform to see 
!wether it exists at least one point different from discarded_value in the interval [i-N,...,i,...,i+N]. If not, 
!then output_array(i)=discarded_value.
!******************************************************************************
! use mod_statistics_wd
 implicit none
 integer,                     intent(in)  :: size_array  ! size of the input array
 integer,                     intent(in)  :: np_m_a      ! number of points used in the computation of the moving average = 2*np_m_a + 1
                                                         ! [i-np_m_a,.....,i,......,i+np_m_a] ----> i
 real,                        intent(in)  :: no_value    ! discarded value in the computation of the moving average
 real, dimension(size_array), intent(in)  :: vx,vy       ! input array
 
 real, dimension(size_array), intent(out) :: m_a_wd   ! output array


 integer                                  :: i,j,itot,inc
 real, dimension(size_array)              :: sd_m_a
 integer, dimension(size_array)           :: inc_m_a

 m_a_wd = no_value
 sd_m_a    = 0.        
 
 do i=1,np_m_a
 inc=0
  do j=1,i+np_m_a
   if ( (vx(j).ne.no_value).and.(vy(j).ne.no_value) ) inc=inc+1
  enddo
  if (inc.ne.0.) call stats_wind_direction(vx(1:i+np_m_a),vy(1:i+np_m_a),i+np_m_a,m_a_wd(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 itot=2*np_m_a+1
 do i=np_m_a+1,size_array-np_m_a-1
  inc=0
  do j=i-np_m_a,i+np_m_a
   if ( (vx(j).ne.no_value).and.(vy(j).ne.no_value) ) inc=inc+1
  enddo
  if (inc.ne.0.) call stats_wind_direction(vx(i-np_m_a:i+np_m_a),vy(i-np_m_a:i+np_m_a),itot,m_a_wd(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 do i=size_array-np_m_a,size_array
  inc=0
  do j=i-np_m_a,size_array
   if ( (vx(j).ne.no_value).and.(vy(j).ne.no_value) ) inc=inc+1
  enddo
  if (inc.ne.0.) call stats_wind_direction(vx(i-np_m_a:size_array),vy(i-np_m_a:size_array),size_array-i+np_m_a+1,&
                                           m_a_wd(i),sd_m_a(i),inc_m_a(i),no_value)
  if (inc.eq.0.) m_a_wd(i)=no_value
 enddo
END SUBROUTINE MOVING_AVERAGE_WIND_DIRECTION
!******************************************************************************


!******************************************************************************
!******************************************************************************
SUBROUTINE MOVING_AVERAGE_WIND_DIRECTION_SD(VX,VY,SIZE_ARRAY,NP_M_A,M_A_WD,sd_m_a,NO_VALUE)
!Identical to the preceding routine, except for the extra output variable SD_M_A
!******************************************************************************
! use mod_statistics_wd
 implicit none
 integer,                     intent(in)  :: size_array  ! size of the input array
 integer,                     intent(in)  :: np_m_a      ! number of points used in the computation of the moving average = 2*np_m_a + 1
                                                         ! [i-np_m_a,.....,i,......,i+np_m_a] ----> i
 real,                        intent(in)  :: no_value    ! discarded value in the computation of the moving average
 real, dimension(size_array), intent(in)  :: vx,vy       ! input array
 
 real, dimension(size_array), intent(out) :: m_a_wd   ! output array


 integer                                  :: i,j,itot,inc
 real, dimension(size_array), intent(out) :: sd_m_a
 integer, dimension(size_array)           :: inc_m_a

 m_a_wd = no_value
 sd_m_a    = 0.        
 
 do i=1,np_m_a
 inc=0
  do j=1,i+np_m_a
   if ( (vx(j).ne.no_value).and.(vy(j).ne.no_value) ) inc=inc+1
  enddo
  if (inc.ne.0.) call stats_wind_direction(vx(1:i+np_m_a),vy(1:i+np_m_a),i+np_m_a,m_a_wd(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 itot=2*np_m_a+1
 do i=np_m_a+1,size_array-np_m_a-1
  inc=0
  do j=i-np_m_a,i+np_m_a
   if ( (vx(j).ne.no_value).and.(vy(j).ne.no_value) ) inc=inc+1
  enddo
  if (inc.ne.0.) call stats_wind_direction(vx(i-np_m_a:i+np_m_a),vy(i-np_m_a:i+np_m_a),itot,m_a_wd(i),sd_m_a(i),inc_m_a(i),no_value)
 enddo

 do i=size_array-np_m_a,size_array
  inc=0
  do j=i-np_m_a,size_array
   if ( (vx(j).ne.no_value).and.(vy(j).ne.no_value) ) inc=inc+1
  enddo
  if (inc.ne.0.) call stats_wind_direction(vx(i-np_m_a:size_array),vy(i-np_m_a:size_array),size_array-i+np_m_a+1,&
                                           m_a_wd(i),sd_m_a(i),inc_m_a(i),no_value)
  if (inc.eq.0.) m_a_wd(i)=no_value
 enddo
END SUBROUTINE MOVING_AVERAGE_WIND_DIRECTION_SD
!******************************************************************************

!******************************************************************************
!******************************************************************************
  subroutine cumdist_quartiles(N,F,CDX,CDY,XMED,X25,X75)
!  Subroutine de calcul de la distribution cumulee des valeurs de F
!  ENTREE:F:tableau de dimension N
!         N: dimension de F
!  SORTIE: CDX: abscissa de la distribution cumulee
!          CDY: ordonnees de la distribution cumulee
!          xmed: mediane
!  F et CDX peuvent etre le meme tableau lors de l'appel
!******************************************************************************
    implicit none
    integer, intent(in) :: N
    real, dimension(N), intent(in) :: F
    real, dimension(N), intent(out) :: CDX,CDY
    real, dimension(N) :: AUX
    real, intent(out) :: XMED,X25,X75
    integer :: i,N2,N3,N4
    integer, dimension(N) :: indx

    call indexx (N,F,indx)
    AUX=F
    CDX=AUX(indx)
    N2=N/2
    N3=N/4
    N4=3*N/4
    do i=1,N
       CDY(i)=(real(i-1)/real(N-1))
    enddo
    if(2*N2.eq.N)then
       XMED=0.5*(CDX(N2)+CDX(N2+1))
    else
       XMED=CDX(N2+1)  
    endif
    if(4*N3.eq.N)then
       X25=0.5*(CDX(N3)+CDX(N3+1))
    else
       X25=CDX(N3+1)
    endif
    if((4./3.)*N4.eq.N)then
       X75=0.5*(CDX(N4)+CDX(N4+1))
    else
       X75=CDX(N4+1)
    endif

!  equivalemment, au lieu du XMED construit comme ca
!  on peut le definir aussi
!            XMED=CDX(N2)+((0.5-CDY(N2))*(CDX(N2+1)-CDX(N2))
!     1   /(CDY(N2+1)-CDY(N2)))
    return
  end subroutine cumdist_quartiles
!******************************************************************************

!******************************************************************************
!******************************************************************************
  subroutine cumdist_tertiles(N,F,CDX,CDY,XMED,X33,X66)
!  Subroutine de calcul de la distribution cumulee des valeurs de F
!  ENTREE:F:tableau de dimension N
!         N: dimension de F
!  SORTIE: CDX: abscissa de la distribution cumulee
!          CDY: ordonnees de la distribution cumulee
!          xmed: mediane
!  F et CDX peuvent etre le meme tableau lors de l'appel
!******************************************************************************
    implicit none
    integer, intent(in) :: N
    real, dimension(N), intent(in) :: F
    real, dimension(N), intent(out) :: CDX,CDY
    real, dimension(N) :: AUX
    real, intent(out) :: XMED,X33,X66
    integer :: i,N2,N3,N4
    integer, dimension(N) :: indx

    call indexx (N,F,indx)
    AUX=F
    CDX=AUX(indx)
    N2=N/2
    N3=N/3
    N4=2*N/3
    do i=1,N
       CDY(i)=(real(i-1)/real(N-1))
    enddo
    if(2*N2.eq.N)then
       XMED=0.5*(CDX(N2)+CDX(N2+1))
    else
       XMED=CDX(N2+1)  
    endif
    if(3*N3.eq.N)then
       X33=0.5*(CDX(N3)+CDX(N3+1))
    else
       X33=CDX(N3+1)
    endif
    if((3./2)*N4.eq.N)then
       X66=0.5*(CDX(N4)+CDX(N4+1))
    else
       X66=CDX(N4+1)
    endif

!  equivalemment, au lieu du XMED construit comme ca
!  on peut le definir aussi
!            XMED=CDX(N2)+((0.5-CDY(N2))*(CDX(N2+1)-CDX(N2))
!     1   /(CDY(N2+1)-CDY(N2)))
    return
  end subroutine cumdist_tertiles
!******************************************************************************

!******************************************************************************
!******************************************************************************
  subroutine cumdist(N,F,CDX,CDY,XMED)
!  Subroutine de calcul de la distribution cumulee des valeurs de F
!  ENTREE:F:tableau de dimension N
!         N: dimension de F
!  SORTIE: CDX: abscissa de la distribution cumulee
!          CDY: ordonnees de la distribution cumulee
!          xmed: mediane
!  F et CDX peuvent etre le meme tableau lors de l'appel
!******************************************************************************
    implicit none
    integer, intent(in) :: N
    real, dimension(N), intent(in) :: F
    real, dimension(N), intent(out) :: CDX,CDY
    real, dimension(N) :: AUX
    real, intent(out) :: XMED
    integer :: i,N2
    integer, dimension(N) :: indx

    call indexx (N,F,indx)
    AUX=F
    CDX=AUX(indx)
    N2=N/2
    do i=1,N
       CDY(i)=(real(i-1)/real(N-1))
    enddo
    if(2*N2.eq.N)then
       XMED=0.5*(CDX(N2)+CDX(N2+1))
    else
       XMED=CDX(N2+1)  
    endif
!  equivalemment, au lieu du XMED construit comme ca
!  on peut le definir aussi
!            XMED=CDX(N2)+((0.5-CDY(N2))*(CDX(N2+1)-CDX(N2))
!     1   /(CDY(N2+1)-CDY(N2)))
    return
    end subroutine cumdist
!******************************************************************************

!******************************************************************************
!******************************************************************************
 subroutine indexx (input_size, input_array, output_index)
!Output an ascending index vector output_index from an input array input_array
!of size input_size
!******************************************************************************
   integer, intent(in):: input_size
   real, dimension(input_size), intent(in):: input_array
   integer, dimension(input_size), intent(out):: output_index
   integer :: i,j,ix,iix,jix
      
   do i = 1, input_size
     output_index(i) = i
   end do
      
   ! Selection sort
   do i = 1, input_size - 1
     iix = output_index(i)
     ix = i
     do j = i + 1, input_size
       jix = output_index(j)
       ! Do your comparison here
       if (input_array(iix) .gt. input_array(jix)) then
         ! Record the smallest
         ix = j
         iix = jix
       end if
     end do
     ! Swap
     output_index(ix) = output_index(i)
     output_index(i) = iix
   end do
   return
 end subroutine indexx
!******************************************************************************
!******************************************************************************
  subroutine masswf (xh,xhmass1,xhmass,ndimhmass,xwf_inf,xwf_sup)

real, intent(in) :: xh,xhmass1
integer, intent(in) :: ndimhmass
real, intent(in),dimension(ndimhmass) :: xhmass
real, intent(out) :: xwf_inf,xwf_sup

REAL :: phmassloc_inf,phmassloc_sup
INTEGER :: inc

!PRINT *, 'xh=',xh,'xhmass1=',xhmass1,'xhmass(2)=',xhmass(2)
IF (xh.lt.xhmass1) THEN
 xwf_inf=0.
 xwf_sup=0.
! PRINT *, 'IF 1 ','xh=',xh,'xwf_inf=',xwf_inf,'xwf_sup=',xwf_sup
ENDIF

IF ( (xh.ge.xhmass1).AND.(xh.lt.xhmass(2)) ) THEN
 xwf_sup=0.
 xwf_inf=1-ABS(1-(LOG10(xh/xhmass(1)))/LOG10(2.))
! PRINT *, 'IF 2 ','xh=',xh,'xwf_inf=',xwf_inf,'xwf_sup=',xwf_sup
ENDIF

IF ( (xh.ge.xhmass(2)).AND.(xh.lt.xhmass(ndimhmass)) ) THEN
 inc=1
 DO WHILE (xh.ge.xhmass(inc))
  phmassloc_inf=XHMASS(inc)
  phmassloc_sup=XHMASS(inc+1)
  INC=INC+1
 ENDDO
 xwf_inf=1-ABS(1-(LOG10(xh/phmassloc_inf))/LOG10(2.))
 xwf_sup=ABS(1-ABS(1-(LOG10(xh/phmassloc_sup))/LOG10(2.)))
! PRINT *, 'IF 3 ','xh=',xh,'xwf_inf=',xwf_inf,'xwf_sup=',xwf_sup
! PRINT *, 'IF 3 ','xh=',xh,'phmassloc_inf=',phmassloc_inf,'phmassloc_sup=',phmassloc_sup,'inc=',inc
ENDIF

IF ( xh.ge.xhmass(ndimhmass) ) THEN
 xwf_inf=1.
 xwf_sup=1.
! PRINT *, 'IF 4 ','xh=',xh,'xwf_inf=',xwf_inf,'xwf_sup=',xwf_sup
ENDIF

end subroutine masswf

!******************************************************************************
!******************************************************************************
  subroutine wfglf (xh,xhmass1,xhmass2,xwf)

real, intent(in) :: xh,xhmass1,xhmass2 
real, intent(out) :: xwf 

!PRINT *, 'XH=',XH,'XHMASS1=',XHMASS1,'XHMASS(2)=',XHMASS(2)
IF (xh.lt.xhmass1)  THEN
 xwf=1.
! PRINT *, 'IF 1 ','XH=',XH,'XWF_INF=',XWF_INF,'XWF_SUP=',XWF_SUP
ENDIF

IF ((xh.ge.xhmass1).AND.(xh.lt.xhmass2)) THEN
 xwf=-(LOG10(xh/xhmass2))/LOG10(2.)
! PRINT *, 'IF 2 ','XH=',XH,'XWF=',XWF
ENDIF

IF (xh.ge.xhmass2) THEN
 xwf=0.
ENDIF

end subroutine wfglf

end module mod_scienceperso
