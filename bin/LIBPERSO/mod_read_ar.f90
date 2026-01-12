!******************************************************************************
module mod_read_ar  ! subrotines for reading .dat files                      !
!******************************************************************************

! FORTRAN 2008 stuff (need updating gcc)
!  use, intrinsic :: iso_fortran_env
  implicit none
!  integer, parameter :: sp = REAL32
!  integer, parameter :: dp = REAL64
!  integer, parameter :: qp = REAL128
  integer, parameter :: dp = kind(1.d0)
  private
  public :: read_arfile,read_arfile_resample,read_datafile_fromar,read_datafile_fromar_resample,read_arfile_extremes

contains
!******************************************************************************
subroutine read_arfile(namefile,readminutes,variable)
!Read "readminutes" minutes at a time from an "*_AR.dat" file produced by the autoregression.
!The autoregression run each hour and produces a forecast for 4 hours.
!This routine puts in a consecutive array readminutes data computed each hour (typically readminutes=60).
!The output contains an array with the concatenated data from consecutive AR computations
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, parameter :: headerlines=5
  integer, intent(in) :: readminutes
  integer :: i,j,error,nline,ncol,countsets
  real :: tmp
  real, dimension(:,:), allocatable, intent(inout) :: variable
  real, dimension(:,:), allocatable :: variabletmp
  real, dimension(:,:,:), allocatable :: idxsets
  logical :: ok
  character(480), intent(in) :: namefile
  character(maxlinelenght) :: line
  character(32) :: word

  if (allocated(variable)) then
    print*, '!!! read_arfile: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_arfile: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
  nline=0
  ncol=0
  error=0
  i=1
  read(10,'(A)' ) line
  do while ( error == 0 )
    read(line,*, iostat=error) ( word, j=1,i )
    i=i+1
    if (i>maxcol) then
       print*, '!!! read_file: too many columns in file ',trim(namefile)
       call exit(1)
    end if
    if ( error == 0 ) then
      ncol=ncol+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_file: error while determining columns number from file ',trim(namefile),' at line 1, column',ncol
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  error=0
  do while ( error == 0 )
    read(10,*,iostat=error) tmp
    if ( error == 0 ) then
      nline=nline+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_arfile: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  allocate(variabletmp(nline,ncol))
  do i=1,nline
    read(10,*,IOSTAT=error) (variabletmp(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do
  close(10)

  countsets=0
  do i=1,nline
    if (i.gt.1) then
      if (variabletmp(i,1).lt.variabletmp(i-1,1)) then
        countsets=countsets+1
      end if
    end if
  end do
  countsets=countsets+1
  if (countsets.eq.1) then
    if (nline.lt.readminutes) then
      print*, '!!! read_arfile: there are no usable data in file ',trim(namefile)
    end if
  end if


  allocate(idxsets(countsets,readminutes,ncol))
  countsets=1
  idxsets(countsets,:,:)=variabletmp(1:readminutes,:)
  do i=1,nline
    if (i.eq.1) then
      idxsets(countsets,:,:)=variabletmp(1:readminutes,:)
    else
      if ((variabletmp(i,1).lt.variabletmp(i-1,1)) .and. (i+readminutes-1 .le. nline)) then
        countsets=countsets+1
        idxsets(countsets,:,:)=variabletmp(i:i+readminutes-1,:)
      end if
    end if
  end do

  allocate(variable(countsets*readminutes,ncol))
  do i=1,countsets
    variable((readminutes*(i-1))+1:(readminutes*(i-1))+readminutes,:)=idxsets(i,:,:)
  end do

end subroutine read_arfile

!******************************************************************************
subroutine read_arfile_resample(namefile,readminutes,variable,resample,noval)
!Read "readminutes" minutes at a time from an "*_AR.dat" file produced by the autoregression.
!The autoregression run each hour and produces a forecast for N hours.
!This routine puts in a consecutive array readminutes data computed each hour (typically readminutes=60).
!The output contains an array with the concatenated data from consecutive AR computations
!If resample is greater than 1, the routine wil resample the output over resample minutes
!******************************************************************************
  use mod_scienceperso

  implicit none
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, parameter :: headerlines=5
  integer, parameter :: tstep=1 !The timestep of the AR files in minutes
  integer, intent(in) :: readminutes,resample
  integer :: i,j,error,nline,ncol,countsets,restime,readminutes_resample
  real :: tmp
  real, intent(in) :: noval
  real, dimension(:,:), allocatable, intent(inout) :: variable
  real, dimension(:,:), allocatable :: variabletmp
  real, dimension(:,:,:), allocatable :: idxsets,idxsets_resample
  logical :: ok
  character(480), intent(in) :: namefile
  character(maxlinelenght) :: line
  character(32) :: word

  if (allocated(variable)) then
    print*, '!!! read_arfile: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_arfile: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
  nline=0
  ncol=0
  error=0
  i=1
  read(10,'(A)' ) line
  do while ( error == 0 )
    read(line,*, iostat=error) ( word, j=1,i )
    i=i+1
    if (i>maxcol) then
       print*, '!!! read_file: too many columns in file ',trim(namefile)
       call exit(1)
    end if
    if ( error == 0 ) then
      ncol=ncol+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_file: error while determining columns number from file ',trim(namefile),' at line 1, column',ncol
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  error=0
  do while ( error == 0 )
    read(10,*,iostat=error) tmp
    if ( error == 0 ) then
      nline=nline+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_arfile: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  allocate(variabletmp(nline,ncol))
  do i=1,nline
    read(10,*,IOSTAT=error) (variabletmp(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do
  close(10)

  if (ncol .ne. 2) then
    print*, "!!! ERROR in mod_read_ar.f90: INVALID FORMAT OF INPUT FILES (columns != 2)"
    call exit(1)
  end if

  countsets=0
  do i=1,nline
    if (i.gt.1) then
      if (variabletmp(i,1).lt.variabletmp(i-1,1)) then
        countsets=countsets+1
      end if
    end if
  end do
  countsets=countsets+1
  if (countsets.eq.1) then
    if (nline.lt.readminutes) then
      print*, '!!! read_arfile: there are no usable data in file ',trim(namefile)
    end if
  end if

  restime=int(real(resample)/real(tstep))    
  print*, 'RESAMPLING every ',resample,' minutes -- This means every ',restime,' samples'
  if (restime*tstep .ne. resample) then
    print*, "   -- !! Warning! resample time is not a multiple of timestep!!"
    call exit(1)
  end if
  readminutes_resample=int(real(readminutes)/real(restime))
  if (readminutes_resample*restime .ne. readminutes) then
    print*, "   -- !! Warning! resample time is not a multiple of timestep!!"
    call exit(1)
  end if
  allocate(idxsets_resample(countsets,readminutes_resample,ncol+1))

  allocate(idxsets(countsets,readminutes,ncol))
  countsets=1
  idxsets(countsets,:,:)=variabletmp(1:readminutes,:)
  do i=1,nline
    if (i.eq.1) then
      idxsets(countsets,:,:)=variabletmp(1:readminutes,:)
      do j=1,readminutes_resample
        idxsets_resample(countsets,j,1)=variabletmp(i+restime*j-1,1)
      end do
      call resampling_average(idxsets(countsets,:,2),readminutes,restime,idxsets_resample(countsets,:,2),&
           &readminutes_resample,idxsets_resample(countsets,:,3),noval)
    else
      if ((variabletmp(i,1).lt.variabletmp(i-1,1)) .and. (i+readminutes-1 .le. nline)) then
        countsets=countsets+1
        idxsets(countsets,:,:)=variabletmp(i:i+readminutes-1,:)
        do j=1,readminutes_resample
          idxsets_resample(countsets,j,1)=variabletmp(i+readminutes_resample*j-1,1)
        end do
        call resampling_average(idxsets(countsets,:,2),readminutes,restime,idxsets_resample(countsets,:,2),&
             &readminutes_resample,idxsets_resample(countsets,:,3),noval)
      end if
    end if
  end do

  allocate(variable(countsets*readminutes_resample,ncol+1))
  do i=1,countsets
    variable((readminutes_resample*(i-1))+1:(readminutes_resample*(i-1))+readminutes_resample,:)=idxsets_resample(i,:,:)
  end do

end subroutine read_arfile_resample

!******************************************************************************
subroutine read_datafile_fromar(namefile,variableAR,noval,variable)
!Read the MNH or TELEMETRY file produced by the AR and select only the data corresponding to the variableAR array red by read_arfile
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, parameter :: headerlines=5
  integer :: i,j,error,nline,ncol,lineread,sizeAR
  real :: tmp
  real, intent(in) :: noval
  real, dimension(:,:), intent(in) :: variableAR
  real, dimension(:,:), allocatable, intent(inout) :: variable
  real, dimension(:,:), allocatable :: variabletmp
  logical :: ok
  character(480), intent(in) :: namefile
  character(maxlinelenght) :: line
  character(32) :: word

  if (allocated(variable)) then
    print*, '!!! read_arfile: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_arfile: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
  nline=0
  ncol=0
  error=0
  i=1
  read(10,'(A)' ) line
  do while ( error == 0 )
    read(line,*, iostat=error) ( word, j=1,i )
    i=i+1
    if (i>maxcol) then
       print*, '!!! read_file: too many columns in file ',trim(namefile)
       call exit(1)
    end if
    if ( error == 0 ) then
      ncol=ncol+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_file: error while determining columns number from file ',trim(namefile),' at line 1, column',ncol
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  error=0
  do while ( error == 0 )
    read(10,*,iostat=error) tmp
    if ( error == 0 ) then
      nline=nline+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_arfile: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  allocate(variabletmp(nline,ncol))
  do i=1,nline
    read(10,*,IOSTAT=error) (variabletmp(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do
  close(10)

  sizeAR=size(variableAR,1)
  allocate(variable(sizeAR,ncol))
  variable(:,:)=noval
  lineread=1
  do i=1,nline
    if (variabletmp(i,1) .eq. variableAR(lineread,1)) then
      variable(lineread,:)=variabletmp(i,:)
      lineread=lineread+1
      if (lineread.gt.sizeAR) exit
    end if
  end do

end subroutine read_datafile_fromar

!******************************************************************************
subroutine read_datafile_fromar_resample(namefile,variableAR,noval,variable,resample)
!Read the MNH or TELEMETRY file produced by the AR and select only the data corresponding to the variableAR array red by read_arfile_resample
!It will resample the output over resample minutes
!******************************************************************************
  use mod_scienceperso

  implicit none
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, parameter :: headerlines=5
  integer, parameter :: tstep=1 !The timestep of the AR files in minutes  
  integer :: i,j,error,nline,ncol,lineread,sizeAR,tmpint,restime
  real :: tmp
  real, intent(in) :: noval
  integer, intent(in) :: resample
  real, dimension(:,:), intent(in) :: variableAR
  real, dimension(:,:), allocatable, intent(inout) :: variable
  real, dimension(:,:), allocatable :: variabletmp
  logical :: ok
  character(480), intent(in) :: namefile
  character(maxlinelenght) :: line
  character(32) :: word

  if (allocated(variable)) then
    print*, '!!! read_arfile: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_arfile: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
  nline=0
  ncol=0
  error=0
  i=1
  read(10,'(A)' ) line
  do while ( error == 0 )
    read(line,*, iostat=error) ( word, j=1,i )
    i=i+1
    if (i>maxcol) then
       print*, '!!! read_file: too many columns in file ',trim(namefile)
       call exit(1)
    end if
    if ( error == 0 ) then
      ncol=ncol+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_file: error while determining columns number from file ',trim(namefile),' at line 1, column',ncol
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  error=0
  do while ( error == 0 )
    read(10,*,iostat=error) tmp
    if ( error == 0 ) then
      nline=nline+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_arfile: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  allocate(variabletmp(nline,ncol))
  do i=1,nline
    read(10,*,IOSTAT=error) (variabletmp(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do
  close(10)

  if (ncol .ne. 2) then
    print*, "!!! ERROR in mod_read_ar.f90: INVALID FORMAT OF INPUT FILES (columns != 2)"
    call exit(1)
  end if

  restime=int(real(resample)/real(tstep))    
  print*, 'RESAMPLING every ',resample,' minutes -- This means every ',restime,' samples'
  if (restime*tstep .ne. resample) then
    print*, "   -- !! Warning! resample time is not a multiple of timestep!!"
    call exit(1)
  end if

  sizeAR=size(variableAR,1)
  if (size(variableAR,2) .ne. 3) then
    print*, "!!! ERROR in read_datafile_fromar_resample: variableAR must have 3 columns"
    call exit(1)
  end if

  allocate(variable(sizeAR,ncol+1))
  variable(:,:)=noval
  lineread=1
  do i=1,nline
    if (variabletmp(i,1) .eq. variableAR(lineread,1)) then
      variable(lineread,1)=variabletmp(i,1)
      call stats(variabletmp(i-restime+1:i,2),restime,variable(lineread,2),variable(lineread,3),tmpint,noval)
      lineread=lineread+1
      if (lineread.gt.sizeAR) exit
    end if
  end do

end subroutine read_datafile_fromar_resample

!******************************************************************************
subroutine read_arfile_extremes(namefile,startmin,endmin,variable)
!Read "endmin-startmin+1" minutes at a time from an "*_AR.dat" file produced by the autoregression.
!The autoregression run each hour and produces a forecast for 4 hours.
!This routine puts in a consecutive array data computed each hour from startmin to endmin (e.g. startmin=61 endmin=120).
!The output contains an array with the concatenated data from consecutive AR computations
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, parameter :: headerlines=5
  integer, intent(in) :: startmin,endmin
  integer :: i,j,error,nline,ncol,countsets
  real :: tmp
  real, dimension(:,:), allocatable, intent(inout) :: variable
  real, dimension(:,:), allocatable :: variabletmp
  real, dimension(:,:,:), allocatable :: idxsets
  logical :: ok
  character(480), intent(in) :: namefile
  character(maxlinelenght) :: line
  character(32) :: word

  if (allocated(variable)) then
    print*, '!!! read_arfile: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_arfile: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
  nline=0
  ncol=0
  error=0
  i=1
  read(10,'(A)' ) line
  do while ( error == 0 )
    read(line,*, iostat=error) ( word, j=1,i )
    i=i+1
    if (i>maxcol) then
       print*, '!!! read_file: too many columns in file ',trim(namefile)
       call exit(1)
    end if
    if ( error == 0 ) then
      ncol=ncol+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_file: error while determining columns number from file ',trim(namefile),' at line 1, column',ncol
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  error=0
  do while ( error == 0 )
    read(10,*,iostat=error) tmp
    if ( error == 0 ) then
      nline=nline+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_arfile: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  allocate(variabletmp(nline,ncol))
  do i=1,nline
    read(10,*,IOSTAT=error) (variabletmp(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do
  close(10)

  countsets=0
  do i=1,nline
    if (i.gt.1) then
      if (variabletmp(i,1).lt.variabletmp(i-1,1)) then
        countsets=countsets+1
      end if
    end if
  end do
  countsets=countsets+1
  if (countsets.eq.1) then
    if (nline.lt.endmin) then
      print*, '!!! read_arfile: there are no usable data in file ',trim(namefile)
    end if
  end if


  allocate(idxsets(countsets,endmin-startmin+1,ncol))
  countsets=1
  idxsets(countsets,:,:)=variabletmp(startmin:endmin,:)
  do i=1,nline
    if (i.eq.1) then
      idxsets(countsets,:,:)=variabletmp(startmin:endmin,:)
    else
      if ((variabletmp(i,1).lt.variabletmp(i-1,1)) .and. (i+endmin-1 .le. nline)) then
        countsets=countsets+1
        idxsets(countsets,:,:)=variabletmp(i+startmin-1:i+endmin-1,:)
      end if
    end if
  end do

  allocate(variable(countsets*(endmin-startmin+1),ncol))
  do i=1,countsets
    variable(((endmin-startmin+1)*(i-1))+1:((endmin-startmin+1)*(i-1))+(endmin-startmin+1),:)=idxsets(i,:,:)
!    print*, "AAA", variable(((endmin-startmin+1)*(i-1))+1,:),variable(((endmin-startmin+1)*(i-1))+(endmin-startmin+1),:)
  end do

end subroutine read_arfile_extremes

end module mod_read_ar

