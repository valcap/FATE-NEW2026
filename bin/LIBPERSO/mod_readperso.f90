!******************************************************************************
module mod_readperso      ! subrotines for reading files                      !
!See the comments in each subroutine
!******************************************************************************

  implicit none

contains

!******************************************************************************
subroutine count_file_size(namefile,headerlines,nline,ncol)
!Count the number of lines and columns in a file
!namefile=filename
!headerlines=number of header lines to skip during read (not counted)
!nline=number of lines
!ncol=number of columns
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=200                                !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  character(360), intent(in) :: namefile
  integer, intent(in) :: headerlines
  integer, intent(out) :: nline,ncol
  integer :: i,j,error
  real :: tmp
  logical :: ok
  character(maxlinelenght) :: line
  character(32) :: word

  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_file: file ',trim(namefile),' does not exists'
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
        print*, '!!! read_file: error while determining line number from file ',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  close(10)
end subroutine count_file_size
!******************************************************************************

!******************************************************************************
subroutine read_generic_ascii_file(namefile,headerlines,variable,nline)
!Read a generic ascii file that contains only numbers (no characters) organized in lines and columns separated by space

!The output is an array "variable" that contains all the data in the file in the exact same form (lines and columns)
!You must pass the following UNALLOCATED variable to the routine
!  real, dimension(:,:), allocatable, intent(inout) :: variable

!namefile=filename
!headerlines=number of header lines to skip during read
!variable=the output array
!nline=number of lines in the output array
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=300                                 !Maximum column number
  integer, parameter :: maxlinelenght=5000                         !Maximum line lenght
  integer, intent(in) :: headerlines
  integer :: i,j,error,ncol
  integer, intent(out) :: nline
  real, dimension(:,:), allocatable, intent(inout) :: variable
  logical :: ok
  character(360), intent(in) :: namefile
  character(maxlinelenght) :: line

  if (allocated(variable)) then
    print*, '!!! read_generic_ascii_file: input variable is already allocated when calling the subroutine'
    call exit(1)
  end if
  
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_generic_ascii_file: file ',trim(namefile),' does not exists'
    call exit(1)
  end if

  call count_file_size(namefile,headerlines,nline,ncol)

  allocate(variable(nline,ncol))

  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
 
  do i=1,nline
    read(10,*,IOSTAT=error) (variable(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do

  close(10)

end subroutine read_generic_ascii_file

!******************************************************************************
subroutine read_eso_database_file_setdate(namefile,headerlines,variable,nlinedate,years,months,days,hs,yearf,monthf,dayf,hf)
!Read a generic treated file produced by the ESO ambient condition database
!This function read only the data in a specific range of dates
!It is simply a file with only numbers (not characters) and the first columns like this:
!YEAR MONTH DAY HOUR MINUTE SECOND
!The output is an array "variable" that contains all the data in the file in the exact same form (lines and columns)
!You must pass the following UNALLOCATED variable to the routine
!  real, dimension(:,:), allocatable, intent(inout) :: variable

!namefile=filename
!headerlines=number of header lines to skip during read
!variable=the output array
!nlinedate=number of lines in the output array

!years,months,days,hs is the YEAR, MONTH, DAY and HOUR of start for the data extraction
!yearf,monthf,dayf,hf is the YEAR, MONTH, DAY and HOUR of end for the data extraction
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=300                                 !Maximum column number
  integer, parameter :: maxlinelenght=5000                         !Maximum line lenght
  integer, intent(in) :: headerlines
  integer :: i,j,error,ncol,nline,startcount
  integer, intent(in) :: years,months,days,yearf,monthf,dayf,hs,hf
  integer, intent(out) :: nlinedate
  real, dimension(:,:), allocatable :: variable_temp  
  real, dimension(:,:), allocatable, intent(inout) :: variable
  logical :: ok
  character(360), intent(in) :: namefile
  character(maxlinelenght) :: line

  if (allocated(variable)) then
    print*, '!!! read_eso_database_file: input datelist_ymdhms is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_eso_database_file: file ',trim(namefile),' does not exists'
    call exit(1)
  end if

  call count_file_size(namefile,headerlines,nline,ncol)

  allocate(variable_temp(nline,ncol))

  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
 
  do i=1,nline
    read(10,*,IOSTAT=error) (variable_temp(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do

  close(10)

  nlinedate=0
  startcount=0
  do i=1,nline
    if (int(variable_temp(i,1)) .eq. years) then
      if (int(variable_temp(i,2)) .eq. months) then
        if (int(variable_temp(i,3)) .eq. days) then
          if (int(variable_temp(i,4)) .ge. hs) then
            startcount=1
          end if
        else if ((int(variable_temp(i,3)) .gt. days).or.(int(variable_temp(i,2)) .gt. months)&
            &.or.(int(variable_temp(i,1)) .gt. years)) then
            startcount=1
        end if 
      else if ((int(variable_temp(i,2)) .gt. months).or.(int(variable_temp(i,1)) .gt. years)) then
        startcount=1
      end if
    else if (int(variable_temp(i,1)) .gt. years) then
      startcount=1
    end if
    if (int(variable_temp(i,1)) .eq. yearf) then
      if (int(variable_temp(i,2)) .eq. monthf) then
        if (int(variable_temp(i,3)) .eq. dayf) then
          if (int(variable_temp(i,4)) .gt. hf) then
            startcount=0
          end if
        else if ((int(variable_temp(i,3)) .gt. dayf).or.(int(variable_temp(i,2)) .gt. monthf)&
            &.or.(int(variable_temp(i,1)) .gt. yearf)) then
          startcount=0
        end if
      else if ((int(variable_temp(i,2)) .gt. monthf).or.(int(variable_temp(i,1)) .gt. yearf)) then
        startcount=0
      end if
    else if (int(variable_temp(i,1)) .gt. yearf) then
      startcount=0
    end if

    if (startcount .eq. 1) then
      nlinedate=nlinedate+1
    end if

  end do

  allocate(variable(nlinedate,ncol))

  nlinedate=0
  startcount=0
  do i=1,nline
    if (int(variable_temp(i,1)) .eq. years) then
      if (int(variable_temp(i,2)) .eq. months) then
        if (int(variable_temp(i,3)) .eq. days) then
          if (int(variable_temp(i,4)) .ge. hs) then
            startcount=1
          end if
        else if ((int(variable_temp(i,3)) .gt. days).or.(int(variable_temp(i,2)) .gt. months)&
            &.or.(int(variable_temp(i,1)) .gt. years)) then
          startcount=1
        end if
      else if ((int(variable_temp(i,2)) .gt. months).or.(int(variable_temp(i,1)) .gt. years)) then
        startcount=1
      end if
    else if (int(variable_temp(i,1)) .gt. years) then
      startcount=1
    end if
    if (int(variable_temp(i,1)) .eq. yearf) then
      if (int(variable_temp(i,2)) .eq. monthf) then
        if (int(variable_temp(i,3)) .eq. dayf) then
          if (int(variable_temp(i,4)) .gt. hf) then
            startcount=0
          end if
        else if ((int(variable_temp(i,3)) .gt. dayf).or.(int(variable_temp(i,2)) .gt. monthf)&
            &.or.(int(variable_temp(i,1)) .gt. yearf)) then
          startcount=0
        end if
      else if ((int(variable_temp(i,2)) .gt. monthf).or.(int(variable_temp(i,1)) .gt. yearf)) then
        startcount=0
      end if
    else if (int(variable_temp(i,1)) .gt. yearf) then
      startcount=0
    end if

    if (startcount .eq. 1) then
      nlinedate=nlinedate+1
      variable(nlinedate,:)=variable_temp(i,:)
    end if
  end do

  deallocate(variable_temp)

end subroutine read_eso_database_file_setdate


!******************************************************************************
subroutine read_eso_dimm_file(namefile,headerlines,datelist_ymdhms,integtime,target_RA_DEC_AZ_ELEV,dimm_seeing,rel_flux_rms,&
        &airmass,nline)
!Read the data from a TREATED file obtained from the ESO ambient condition database
!This function reads the whole file
!You must pass the following UNALLOCATED variables to the routine
!  real, dimension(:,:), allocatable, intent(inout) :: datelist_ymdhms,target_RA_DEC_AZ_ELEV,rel_flux_rms
!  real, dimension(:), allocatable, intent(inout) :: integtime,dimm_seeing,airmass

!years,months,days,hs is the YEAR, MONTH, DAY and HOUR of start for the data extraction
!yearf,monthf,dayf,hf is the YEAR, MONTH, DAY and HOUR of end for the data extraction

!namefile = filename to read
!headerlines = number of headerlines to skip during read
!Read the readme of the files to understand each field
!datelist_ymdhms=array 6xN  containing the year, month, day, hour, minute, second of the corresponding line. One data for each of the N lines
!target_RA_DEC_AZ_ELEV= array 4xN  containing on each of the N lines the following measures: target_RA, target_DEC, target_AZIMUTH, target_ELEVATION
!rel_flux_rms=arrray 2xN containing on each of the N lines the following measures: Relative_Flux_RMS,Relative_Flux_RMS_base_time
!dimm_seeing=array 1xN the dimm seeing
!arimass=array 1xN airmass
!integtime=array 1xN integration time
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, intent(in) :: headerlines
  integer :: i,j,error,ncol
  integer, intent(out) :: nline
  real, dimension(:,:), allocatable :: variable
  real, dimension(:,:), allocatable, intent(inout) :: target_RA_DEC_AZ_ELEV,rel_flux_rms
  integer, dimension(:,:), allocatable :: datelist_ymdhms
  real, dimension(:), allocatable, intent(inout) :: integtime,dimm_seeing,airmass
  logical :: ok
  character(360), intent(in) :: namefile
  character(maxlinelenght) :: line
  integer, parameter :: numberofcolumns=15

  if (allocated(datelist_ymdhms)) then
    print*, '!!! read_eso_dimm_file: input datelist_ymdhms is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(target_RA_DEC_AZ_ELEV)) then
    print*, '!!! read_eso_dimm_file: input target_RA_DEC_AZ_ELEV is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(rel_flux_rms)) then
    print*, '!!! read_eso_dimm_file: input rel_flux_rms is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(integtime)) then
    print*, '!!! read_eso_dimm_file: input integtime is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(dimm_seeing)) then
    print*, '!!! read_eso_dimm_file: input dimm_seeing is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(airmass)) then
    print*, '!!! read_eso_dimm_file: input airmass is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_eso_dimm_file: file ',trim(namefile),' does not exists'
    call exit(1)
  end if

  call count_file_size(namefile,headerlines,nline,ncol)

  if (ncol .ne. numberofcolumns) then
    print*, '!!! read_eso_dimm_file: file ',trim(namefile),' has a wrong number of columns'
    call exit(1)
  end if

  allocate(variable(nline,ncol))
  allocate(datelist_ymdhms(6,nline))
  allocate(integtime(nline))
  allocate(target_RA_DEC_AZ_ELEV(4,nline))
  allocate(dimm_seeing(nline))
  allocate(rel_flux_rms(2,nline))
  allocate(airmass(nline))

  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
 
  do i=1,nline
    read(10,*,IOSTAT=error) (variable(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do

  close(10)

  do i=1,nline
    datelist_ymdhms(1,i)=int(variable(i,1))
    datelist_ymdhms(2,i)=int(variable(i,2))
    datelist_ymdhms(3,i)=int(variable(i,3))
    datelist_ymdhms(4,i)=int(variable(i,4))
    datelist_ymdhms(5,i)=int(variable(i,5))
    datelist_ymdhms(6,i)=int(variable(i,6))
    integtime(i)=variable(i,7)
    target_RA_DEC_AZ_ELEV(1,i)=variable(i,8)
    target_RA_DEC_AZ_ELEV(2,i)=variable(i,9)
    target_RA_DEC_AZ_ELEV(3,i)=variable(i,10)
    target_RA_DEC_AZ_ELEV(4,i)=variable(i,11)
    dimm_seeing(i)=variable(i,12)
    rel_flux_rms(1,i)=variable(i,13)
    rel_flux_rms(2,i)=variable(i,14)
    airmass(i)=variable(i,15)
  end do
  deallocate(variable)

end subroutine read_eso_dimm_file

!******************************************************************************
subroutine read_eso_dimm_file_setdate(namefile,headerlines,datelist_ymdhms,integtime,target_RA_DEC_AZ_ELEV,&
        &dimm_seeing,rel_flux_rms,airmass,nlinedate,years,months,days,hs,&
        &yearf,monthf,dayf,hf)
!Read the data from a TREATED file obtained from the ESO ambient condition database
!This function allows to select a time interval for the data
!You must pass the following UNALLOCATED variables to the routine
!  real, dimension(:,:), allocatable, intent(inout) :: datelist_ymdhms,target_RA_DEC_AZ_ELEV,rel_flux_rms
!  real, dimension(:), allocatable, intent(inout) :: integtime,dimm_seeing,airmass

!years,months,days,hs is the YEAR, MONTH, DAY and HOUR of start for the data extraction
!yearf,monthf,dayf,hf is the YEAR, MONTH, DAY and HOUR of end for the data extraction

!namefile = filename to read
!headerlines = number of headerlines to skip during read
!Read the readme of the files to understand each field
!datelist_ymdhms=array 6xN  containing the year, month, day, hour, minute, second of the corresponding line. One data for each of the N lines
!target_RA_DEC_AZ_ELEV= array 4xN  containing on each of the N lines the following measures: target_RA, target_DEC, target_AZIMUTH, target_ELEVATION
!rel_flux_rms=arrray 2xN containing on each of the N lines the following measures: Relative_Flux_RMS,Relative_Flux_RMS_base_time
!dimm_seeing=array 1xN the dimm seeing
!arimass=array 1xN airmass
!integtime=array 1xN integration time
!******************************************************************************
  implicit none
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, intent(in) :: headerlines
  integer :: i,j,error,ncol,nline,startcount
  integer, intent(in) :: years,months,days,yearf,monthf,dayf,hs,hf
  integer, intent(out) :: nlinedate
  real, dimension(:,:), allocatable :: variable
  real, dimension(:,:), allocatable, intent(inout) :: target_RA_DEC_AZ_ELEV,rel_flux_rms
  integer, dimension(:,:), allocatable, intent(inout) :: datelist_ymdhms
  real, dimension(:), allocatable, intent(inout) :: integtime,dimm_seeing,airmass
  logical :: ok
  character(360), intent(in) :: namefile
  character(maxlinelenght) :: line
  integer, parameter :: numberofcolumns=15

  if (allocated(datelist_ymdhms)) then
    print*, '!!! read_eso_dimm_file: input datelist_ymdhms is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(target_RA_DEC_AZ_ELEV)) then
    print*, '!!! read_eso_dimm_file: input target_RA_DEC_AZ_ELEV is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(rel_flux_rms)) then
    print*, '!!! read_eso_dimm_file: input rel_flux_rms is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(integtime)) then
    print*, '!!! read_eso_dimm_file: input integtime is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(dimm_seeing)) then
    print*, '!!! read_eso_dimm_file: input dimm_seeing is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  if (allocated(airmass)) then
    print*, '!!! read_eso_dimm_file: input airmass is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_eso_dimm_file: file ',trim(namefile),' does not exists'
    call exit(1)
  end if

  call count_file_size(namefile,headerlines,nline,ncol)

  if (ncol .ne. numberofcolumns) then
    print*, '!!! read_eso_dimm_file: file ',trim(namefile),' has a wrong number of columns'
    call exit(1)
  end if

  allocate(variable(nline,ncol))

  open(unit=10,status='old',file=namefile,access='sequential')
  do i=1,headerlines
    read(10,'(A)') line
  end do
 
  do i=1,nline
    read(10,*,IOSTAT=error) (variable(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do

  close(10)

  nlinedate=0
  startcount=0
  do i=1,nline
    if (int(variable(i,1)) .eq. years) then
      if (int(variable(i,2)) .eq. months) then
        if (int(variable(i,3)) .eq. days) then
          if (int(variable(i,4)) .ge. hs) then
            startcount=1
          end if
        else if ((int(variable(i,3)) .gt. days).or.(int(variable(i,2)) .gt. months).or.(int(variable(i,1)) .gt. years)) then
            startcount=1
        end if
      else if ((int(variable(i,2)) .gt. months).or.(int(variable(i,1)) .gt. years)) then
        startcount=1
      end if
    else if (int(variable(i,1)) .gt. years) then
      startcount=1
    end if
    if (int(variable(i,1)) .eq. yearf) then
      if (int(variable(i,2)) .eq. monthf) then
        if (int(variable(i,3)) .eq. dayf) then
          if (int(variable(i,4)) .gt. hf) then
            startcount=0
          end if
        else if ((int(variable(i,3)) .gt. dayf).or.(int(variable(i,2)) .gt. monthf).or.(int(variable(i,1)) .gt. yearf)) then
          startcount=0
        end if
      else if ((int(variable(i,2)) .gt. monthf).or.(int(variable(i,1)) .gt. yearf)) then
        startcount=0
      end if
    else if (int(variable(i,1)) .gt. yearf) then
      startcount=0
    end if

    if (startcount .eq. 1) then
      nlinedate=nlinedate+1
    end if

  end do

  allocate(datelist_ymdhms(6,nlinedate))
  allocate(integtime(nlinedate))
  allocate(target_RA_DEC_AZ_ELEV(4,nlinedate))
  allocate(dimm_seeing(nlinedate))
  allocate(rel_flux_rms(2,nlinedate))
  allocate(airmass(nlinedate))

  nlinedate=0
  startcount=0
  do i=1,nline
    if (int(variable(i,1)) .eq. years) then
      if (int(variable(i,2)) .eq. months) then
        if (int(variable(i,3)) .eq. days) then
          if (int(variable(i,4)) .ge. hs) then
            startcount=1
          end if
        else if ((int(variable(i,3)) .gt. days).or.(int(variable(i,2)) .gt. months).or.(int(variable(i,1)) .gt. years)) then
          startcount=1
        end if
      else if ((int(variable(i,2)) .gt. months).or.(int(variable(i,1)) .gt. years)) then
        startcount=1
      end if
    else if (int(variable(i,1)) .gt. years) then
      startcount=1
    end if
    if (int(variable(i,1)) .eq. yearf) then
      if (int(variable(i,2)) .eq. monthf) then
        if (int(variable(i,3)) .eq. dayf) then
          if (int(variable(i,4)) .gt. hf) then
            startcount=0
          end if
        else if ((int(variable(i,3)) .gt. dayf).or.(int(variable(i,2)) .gt. monthf).or.(int(variable(i,1)) .gt. yearf)) then
          startcount=0
        end if
      else if ((int(variable(i,2)) .gt. monthf).or.(int(variable(i,1)) .gt. yearf)) then
        startcount=0
      end if
    else if (int(variable(i,1)) .gt. yearf) then
      startcount=0
    end if

    if (startcount .eq. 1) then 
      nlinedate=nlinedate+1
      datelist_ymdhms(1,nlinedate)=int(variable(i,1))
      datelist_ymdhms(2,nlinedate)=int(variable(i,2))
      datelist_ymdhms(3,nlinedate)=int(variable(i,3))
      datelist_ymdhms(4,nlinedate)=int(variable(i,4))
      datelist_ymdhms(5,nlinedate)=int(variable(i,5))
      datelist_ymdhms(6,nlinedate)=int(variable(i,6))
      integtime(nlinedate)=variable(i,7)
      target_RA_DEC_AZ_ELEV(1,nlinedate)=variable(i,8)
      target_RA_DEC_AZ_ELEV(2,nlinedate)=variable(i,9)
      target_RA_DEC_AZ_ELEV(3,nlinedate)=variable(i,10)
      target_RA_DEC_AZ_ELEV(4,nlinedate)=variable(i,11)
      dimm_seeing(nlinedate)=variable(i,12)
      rel_flux_rms(1,nlinedate)=variable(i,13)
      rel_flux_rms(2,nlinedate)=variable(i,14)
      airmass(nlinedate)=variable(i,15)
    end if
  end do

  deallocate(variable)

end subroutine read_eso_dimm_file_setdate

!******************************************************************************
!******************************************************************************
subroutine telemetry_lbt(namefile,variable_resample)
!Read a .asc telemetry file from LBT and outputs a variable containing all lines and columns of data
!Data is resampled in order to have an homogeneous array
!The resampled variable array has one line each minute and noval for empty data
!Year=variable(:,1)
!Month=variable(:,2)
!Day=variable(:,3)
!Hour=variable(:,4)
!Minute=variable(:,5)
!Second=variable(:,6)
!Measure_interval=variable(:,7)
!Temperature=variable(:,8)
!Dewpoint=variable(:,9)
!Humidity=variable(:,10)
!Pressure=variable(:,11)
!Windspeed=variable(:,12)
!Windspeed_front=variable(:,13)
!Winddirection=variable(:,14)
!Winddirection_front=variable(:,15)
!******************************************************************************
  integer, parameter :: dp = kind(1.d0)
  integer, parameter :: noval=9999                                                !NOVAL value
  integer, parameter :: coldatanumber=15                                          !Expected number of columns in data file
  integer, parameter :: headerlines=1                                             !Expected number of header lines in data file
  integer :: i,j,numlines,numcol
  integer :: minutenow,hournow,minutenext,hournext,tmpline
  integer, dimension(:), allocatable :: numavg
  real, dimension(:), allocatable :: average
  real, dimension(:,:), allocatable :: variable, variable_tmp
  real, dimension(:,:), allocatable, intent(inout) :: variable_resample
  character(120), intent(in) :: namefile
  real, dimension(6) :: last_good_date

  if (allocated(variable_resample)) then
    print*, '!!! read_dat_file_pv: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

!Read the measure file from LBT telemetry
  call read_file_lbt(namefile,headerlines,variable)
  numlines=size(variable,1)
  numcol=size(variable,2)
  if (numcol .ne. coldatanumber) then
    print*, "!!! ERROR: Number of columns in measure data file is incorrect"
    print*, "!!! ",numcol,'!=',coldatanumber
    call exit(1)
  end if

  allocate(variable_tmp(1440,numcol))

!Resample data from aproximately every second to absolutely every minute
  last_good_date=0.d0
  hournext=0
  minutenext=0
  hournow=int(variable(1,4))
  minutenow=int(variable(1,5))
  if (hournow .eq. 23) then
    hournext=0
  else
    hournext=hournow+1
  end if
  if (minutenow .eq. 59) then
    minutenext=0
  else
    minutenext=minutenow+1
  end if
  allocate(average(numcol))
  allocate(numavg(numcol))
  average=0.d0
  numavg=0
  tmpline=0
  do i=1,numlines
!If hour and minute correctly switched to the next one, restart the procedure
    if (((variable(i,4) .eq. hournext) .and. (variable(i,5) .eq. minutenext)) .or.& 
       &((variable(i,4) .eq. hournow) .and. (variable(i,5) .eq. minutenext))) then 
      hournow=int(variable(i,4))
      minutenow=int(variable(i,5))
      if (hournow .eq. 23) then
        hournext=0
      else
        hournext=hournow+1
      end if
      if (minutenow .eq. 59) then
        minutenext=0
      else
        minutenext=minutenow+1
      end if
!Perform the average and store the averaged data
      do j=1,numcol
!If no accumulated data is present, then set to noval
        if (numavg(j) .eq. 0) then
          average(j)=noval
        else
          average(j)=average(j)/numavg(j)
        end if
      end do
      tmpline=tmpline+1
      if (tmpline .gt. 1440) then
        print*, "!!! WARNING: found more minutes than 1440"
      end if
      variable_tmp(tmpline,:)=average(:)
      numavg=0
      average=0.d0
    end if
!If hour and minute is the same, accumulate on the variable
    if ((variable(i,4) .eq. hournow) .and. (variable(i,5) .eq. minutenow)) then
      do j=1,6
        last_good_date(j)=variable(i,j)
      end do
      do j=1,numcol
!Accumulate only if variable isn't noval
        if (variable(i,j) .ne. noval) then
          average(j)=average(j)+variable(i,j)
          numavg(j)=numavg(j)+1        
        end if
      end do
    else
!there is a hole in the data
      print*, "!!! WARNING: there is a hole in the data"
      if (minutenow .eq. 59) then
        minutenext=0
        if (hournow .eq. 23) then
          hournext=0
        else
          hournext=hournow+1
        end if
      else
        minutenext=minutenow+1
      end if
      do j=1,6
          average(j)=last_good_date(j)
      end do
      average(4)=hournext
      average(5)=minutenext
      do j=7,numcol
          average(j)=noval
      end do
      tmpline=tmpline+1
      variable_tmp(tmpline,:)=average(:)
      numavg=0
      average=0.d0
    end if
  end do
!Last execution of average
  do j=1,numcol
!If no accumulated data is present, then set to noval
    if (numavg(j) .eq. 0) then
      average(j)=noval
    else
      average(j)=average(j)/numavg(j)
    end if
  end do
  tmpline=tmpline+1
  if (tmpline .gt. 1440) then
    print*, "!!! ERROR: found more minutes than 1440 on last execution of loop"
    call exit(1)
  end if
  if (tmpline .lt. 1440) then
    print*, "!!! ERROR: found less minutes than 1440 on last execution of loop"
    call exit(1)
  end if
  variable_tmp(tmpline,:)=average(:)

!Write everything into variable_resample
  allocate(variable_resample(tmpline,numcol))
  variable_resample(:,:)=variable_tmp(1:tmpline,:)

!Free some memory
  deallocate(variable)
  deallocate(variable_tmp)

end subroutine telemetry_lbt

!******************************************************************************
!******************************************************************************
subroutine read_file_lbt(namefile,headerlines,variable)
!It is used by telemetry_lbt subroutine to read the raw lbt telemetry data from .asc files
!It can be used separately to read the raw LBT telemetry file without resampling
!******************************************************************************
  integer, parameter :: maxcol=30                                 !Maximum column number
  integer, parameter :: maxlinelenght=2000                        !Maximum line lenght
  integer, intent(in) :: headerlines
  integer :: i,j,error,nline,ncol
  real :: tmp
  real, dimension(:,:), allocatable, intent(inout) :: variable
  logical :: ok
  character(120), intent(in) :: namefile
  character(maxlinelenght) :: line
  character(32) :: word

  if (allocated(variable)) then
    print*, '!!! read_dat_file_pv: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_file: file ',trim(namefile),' does not exists'
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
        print*, '!!! read_file: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  do i=1,headerlines
    read(10,'(A)') line
  end do
  allocate(variable(nline,ncol))
  do i=1,nline
    read(10,*,IOSTAT=error) (variable(i,j),j=1,ncol)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do
  close(10)

end subroutine read_file_lbt
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine read_file_FICVAL(namefile,klevel,isextraction,isconversion,islevelK,varname,timevar,variable)
!Subroutine to read a FICVAL file produced by diaprog
!This is VERY tricky!
!namefile=name of file to read
!klevel=livello (in P o K) dell'estrazione contenuta nel FICVAL
!isextraction= logical variable, this is a generic data extraction file if the varialble is true
!isconversion= logical variable, this is a convjjtolatlon file if the varialble is true
!islevelK=if this is true it is an extraction on a model level (K) instead of a pressure level (P)
!varname= the name of the extracted variable, if applicable (not a conversion file)
!timevar= time of the extraction, if applicable
!variable= NXxNY array containing the data of the extracted surface 
!******************************************************************************
  implicit none
  integer :: i,j,nchar=0,error,flag,tmp
  character(120), intent(in) :: namefile
  logical :: ok
  character(30) :: word
  character(120) :: line
  integer, intent(out) :: klevel
  logical, intent(out) :: isextraction,isconversion,islevelK
  logical :: islevelP
  character(30), intent(out) :: varname,timevar
  real,dimension(:,:), allocatable, intent(inout) :: variable
  integer :: niinf,nisup,njinf,njsup,nx,ny,iter

  isextraction=.FALSE.
  isconversion=.FALSE.
  islevelK=.FALSE.
  islevelP=.FALSE.
  niinf=0
  njinf=0
  nisup=0
  njsup=0
  klevel=-10
  iter=0
  nx=0
  ny=0
  varname='NULL'
  timevar='NULL'

  if (allocated(variable)) then
    deallocate(variable)
  end if

  !Read the line containing the variable name and time, among other text
  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! error: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')
!*******FIRST HEADER LINE*************
  read(10,'(A)' ) line
  flag=0
  do i =1,40   ! The very maximum that the string can contain
    read(line,*, iostat=error) ( word, j=1,i )
    if ( error == 0 ) then
    ! We assume that second character is G:varname and fifth character is time.s'
      flag=flag+1
      !If the above asumption is wrong, it may be a data conversion file
      if (flag==1) then
        if (trim(word)=='FICHIER:') then
          print*, 'probably this is a conversion file, not a data extraction'
          isconversion=.TRUE.
        end if
      else if (flag==2) then
!        tmp=len_trim(word)
        !If the following character is present it probably is a data extraction file
        if (word(1:2)=='G:') then
          print*, 'probably this is a data extraction file'
          isextraction=.TRUE.
        end if
!        varname=word(3:tmp)
!        print *, 'VAR=',varname
      else if (flag==5) then
        tmp=len_trim(word)-1
        timevar=word(1:tmp)
        print *, 'TIME=',timevar
      else if (flag>5) then
        print*, '!!! error: error reading from file ',trim(namefile),': header line 1 malformed'
        close(10)
        call exit(1)
      end if
    else if ( error /= 0 ) then
      nchar = i - 1
      exit
    end if
  end do
  !nchar should be 5 if line is a regular header. If it is greater, then abort and check what's going on
  if (nchar > 5) then
    print*, '!!! error: error reading from file ',trim(namefile),': too many characters in header line 1'
    close(10)
    call exit(1)
  end if
!*******SECOND HEADER LINE*************
  read(10,'(A)' ) line
  flag=0
  do i =1,40   ! The very maximum that the string can contain
    read(line,*, iostat=error) ( word, j=1,i )
    if ( error == 0 ) then
    ! We check that word 8 is 'CONVERSION' or we read the variable name
      flag=flag+1
      !If the above asumption is wrong, it may be a data conversion file
      if (flag==1) then
        varname=trim(word)
      else if (flag==6) then
        if (trim(word)=='ITER') then
          print*, 'just guessing, maybe it is a conversion file?'
          isconversion=.TRUE.
        end if
      else if (flag==7) then
        if (is_numeric(word)) then
          if (isconversion) read(word,'(I8)') iter
        end if
      else if (flag==8) then
        if (trim(word)=='CONVERSION') then
          print*, 'definitely this is a conversion file'
          isconversion=.TRUE.
          varname='CONVERSION'
        end if
      end if
    else if ( error /= 0 ) then
      nchar = i - 1
      exit
    end if
  end do
  print *, 'VAR=',varname
  if (isextraction) print *, 'TIME=',timevar
!*******THIRD HEADER LINE*************
  read(10,'(A)' ) line
  flag=0
  do i =1,40   ! The very maximum that the string can contain
    read(line,*, iostat=error) ( word, j=1,i )
    if ( error == 0 ) then
      flag=flag+1
      if (is_numeric(word)) then
        if (flag==2) then
          read(word,'(I8)') niinf
        else if (flag==4) then
          read(word,'(I8)') njinf
        else if (flag==6) then
          read(word,'(I8)') nisup
        else if (flag==8) then
          read(word,'(I8)') njsup
        end if
      end if
      if (flag==9) then
        if (trim(word)=='K') then
          print*, 'this is an extraction on a K level'
          islevelK=.TRUE.
        else if (trim(word)=='P') then
          print*, 'this is an extraction on a P level'
          islevelP=.TRUE.
        end if
      else if (flag==10) then
         if ((islevelK).and.(is_numeric(word))) then
           read(word,'(I8)') klevel
           print*, 'K=',klevel
         else if ((islevelP).and.(is_numeric(word))) then
           read(word,'(I8)') klevel
           print*, 'P=',klevel
         end if
      else if (flag>10) then
        print*, '!!! error: error reading from file ',trim(namefile),': header line 1 malformed'
        close(10)
        call exit(1)
      end if
    else if ( error /= 0 ) then
      nchar = i - 1
      exit
    end if
  end do
  print*, 'niinf=',niinf,'njinf=',njinf,'nisup=',nisup,'njsup=',njsup
  if (islevelK) print*, 'K=',klevel
  if (islevelP) print*, 'P=',klevel
  !nchar should be 8 if it is a conversion and 10 if it is an extraction on a level K. If it is greater, then abort and check what's going on
  if (nchar > 8) then
    if ((islevelK .or. islevelP) .and. (nchar > 10)) then
      print*, '!!! error: error reading from file ',trim(namefile),': too many characters in header line 3'
      close(10)
      call exit(1)
    else if (nchar > 11) then
      print*, '!!! error: error reading from file ',trim(namefile),': too many characters in header line 3'
      close(10)    
      call exit(1)
    end if
  end if
!*******FOURTH HEADER LINE*************
  !Read the line containing the number of grid points, among other text
  read(10,'(A)' ) line
  flag=0
  do i =1,40   ! The very maximum that the string can contain
    read(line,*, iostat=error) ( word, j=1,i )
    if ( error == 0 ) then
      if (is_numeric(word)) then
        flag=flag+1
        if (flag==1) then
          read(word,'(I8)') nx
        else if (flag==2) then
          read(word,'(I8)') ny
        else if (flag==3) then
          if (.not. isconversion) then
            read(word,'(I8)') iter
          end if
        else
          print*, '!!! error: error reading from file ',trim(namefile),': header line 4 malformed'
          close(10)
          call exit(1)
        end if
      end if
    else if ( error /= 0 ) then
      nchar = i - 1
      exit
    end if
  end do
  !nchar should be 8 if it is a conversion and 10 if it is an extraction. If it is greater, then abort and check what's going on
  if (nchar > 8) then
    if (isextraction .and. (nchar > 10)) then
      print*, '!!! error: error reading from file ',trim(namefile),': too many characters in header line 4'
      close(10)    
      call exit(1)
    else if (nchar > 11) then
      print*, '!!! error: error reading from file ',trim(namefile),': too many characters in header line 4'
      close(10)    
      call exit(1)
    end if
  end if
  close(10)
  print*, 'I=',nx,'J=',ny,'ITER=',iter

!*******FINAL CHECKS ON PARAMETERS*************
  if (isextraction .and. ((niinf/=2).or.(njinf/=2).or.(nisup/=(nx+1)).or.(njsup/=(ny+1)))) then
    print*, '!!! error: extrema data array are non-standard for an extraction'
    call exit(1)
  end if
  if (isconversion .and. ((niinf/=1).or.(njinf/=1).or.(nisup/=(nx)).or.(njsup/=(ny)))) then
    print*, '!!! error: extrema data array are non-standard for an conversion'
    call exit(1)
  end if
  if ((.not.isextraction).and.(.not. isconversion)) then
    print*, '!!! error: unrecognized data format'
    call exit(1)
  end if
  if ((isextraction).and.((.not. (islevelK .or. islevelP)).or.(klevel==-10))) then
    print*, '!!! error: it seems a data extraction but the level K of P is unspecified'
    call exit(1)
  end if
  !I don't want to export a dummy variable again, so we export just one.
  islevelK=islevelP

!*******START READING VALUES*************

  if (isextraction) then
    allocate(variable(nx,ny))
  else if (isconversion) then
    allocate(variable(2*nx,ny))
  end if

  inquire( file=namefile, exist=ok )
  if (ok) then
    print*, 'reading file ',namefile
    if (isextraction) call read_varextract(namefile,1,nx,ny,iter,niinf,njinf,variable)
    if (isconversion) call read_varextract(namefile,2,nx,ny,iter,niinf,njinf,variable)
  end if

end subroutine read_file_FICVAL
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine read_varextract(namefile,vartype,nx,ny,iter,startx,starty,variable)
!Subroutine used by read_file_FICVAL to actually do the reading after one understands WHAT it's reading!
!Please never use this directly, pretty please!
!******************************************************************************
  implicit none
  character(120), intent(in) :: namefile
  integer, intent(in) :: nx,ny,iter,vartype,startx,starty
  integer :: i,j,niter,nj,IOstatus,tmp,k,itercolumns,columns,starti,startj
  real,dimension(:,:), allocatable, intent(inout) :: variable

!VARTYPE=1 is data extraction file
!VARTYPE=2 is data conversion file

  if (vartype==1) then
    itercolumns=ceiling(real(nx)/real(iter))
    columns=itercolumns
  else if (vartype==2) then
    itercolumns=ceiling(real(nx)/real(iter))
    columns=itercolumns*2
  else
    print*, '!!! error: vartype not recognized'
    call exit(1)
  end if

  starti=startx-1
  startj=starty-1

  open(unit=20,status='old',file=namefile,access='sequential')
  !skip header
  do i=1,4
    read(20,*,IOSTAT=IOstatus)
  enddo
  niter=1
  i=1
  do while (niter <= iter)
    !skip separators
    read(20,*,IOSTAT=IOstatus)
    call check_readerr(IOstatus,namefile,niter)
    if (IOstatus < 0) exit
    read(20,*,IOSTAT=IOstatus)
    call check_readerr(IOstatus,namefile,niter)
    if (IOstatus < 0) exit
    read(20,*,IOSTAT=IOstatus)
    call check_readerr(IOstatus,namefile,niter)
    if (IOstatus < 0) exit
    !start reading something useful
    if (niter<iter) then
      do nj=1,ny
        read(20,*,IOSTAT=IOstatus) j,(variable(i+k,j-startj),k=0,columns-1)
        call check_readerr(IOstatus,namefile,niter)
        if (IOstatus < 0) exit
      end do
    else
      tmp=nx-(iter-1)*itercolumns
      if (vartype==2) tmp=tmp*2
      if (tmp>columns) then
        print*, '!!! error: remaining number of columns is wrong'
        call exit(1)
      end if
      do nj=1,ny
        read(20,*,IOSTAT=IOstatus) j,(variable(i+k,j-startj),k=0,tmp-1)
        call check_readerr(IOstatus,namefile,niter)
        if (IOstatus < 0) exit
      end do
    end if
    if (vartype==1) then
      !skip another separator
      read(20,*,IOSTAT=IOstatus)
      call check_readerr(IOstatus,namefile,niter)
      if (IOstatus < 0) exit
    end if
    niter=niter+1
    i=i+columns
    if (niter>1000) then
      print*, '!!! error: too many lines in FICVAL file'
      exit
    end if
  enddo
  close(20)
end subroutine read_varextract

!******************************************************************************
!******************************************************************************
logical function is_numeric(string)
!Function used by read_file_FICVAL internally to test if a string is a number
!NEVER CHANGE!
!May be useful for other purposes somewhere?
!******************************************************************************
  implicit none
  character(len=*), intent(in) :: string
  real :: x
  integer :: e,n
  character(12) :: fmt

!  is_numeric = .false.
!  x = FOR_S_NAN
!  READ(string,*,IOSTAT=e) x
!  is_numeric = ((e == 0) .and. (.NOT. ISNAN(X))

!  READ(string,*,IOSTAT=e, ADVANCE='NO', EOR=999) x
!  is_numeric = e == 0
!  999 CONTINUE

  n=len_trim(string)
  write(fmt,'("(F",I0,".0)")') n
  read(string,fmt,IOSTAT=e) x
  is_numeric = e == 0

end function is_numeric

!******************************************************************************
!******************************************************************************
subroutine check_readerr(IOstatus,namefile,niter)
!Subroutine used by read_file_FICVAL internally to test if reading a file gives out an error
!Created for the automation but not really useful here. Its here in order not to rewrite read_file_FICVAL
!******************************************************************************
  implicit none
  integer, intent(in) :: IOstatus,niter
  character(32), intent(in) :: namefile

  if (IOstatus > 0) then
    print*, '!!! error: error reading from file ',trim(namefile),' at iteration ',niter
  end if
end subroutine check_readerr
!******************************************************************************

!******************************************************************************
!******************************************************************************
subroutine read_dat_file_pv(namefile,variable)
!Read a .dat file and outputs an array with data
!The subroutine assumes first colon is the processor number and skip it

!You must pass the following UNALLOCATED variable to the routine
!  real, dimension(:,:), allocatable, intent(inout) :: variable

!namefile=name of the file to read
!variable=the 2-dim array that contains each line and column of the .dat file, except for the first colum (proc number)
!******************************************************************************
  implicit none
  integer :: i,j,error,nline,ncol,inttmp
  real :: tmp
  real, dimension(:,:), allocatable, intent(inout) :: variable
  logical :: ok
  character(360), intent(in) :: namefile
  integer, parameter :: maxlinelenght=1000
  character(maxlinelenght) :: line
  character(32) :: word

  if (allocated(variable)) then
    print*, '!!! read_dat_file_pv: input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_dat_file_pv: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')
  nline=0
  ncol=0
  error=0
  i=1
  read(10,'(A)' ) line
  do while ( error == 0 )
    read(line,*, iostat=error) ( word, j=1,i )
!    read(10,'(I5,E20.10)',advance='NO',iostat=error) inttmp,(tmp,j=1,i)
!    read(10,*,iostat=error) (tmp,j=1,i)
!    read(10,*,iostat=error) (tmp,j=1,i)
    i=i+1
    if (i>20) then
       print*, '!!! read_dat_file_pv: too many columns in file ',trim(namefile)
       call exit(1)
    end if
    if ( error == 0 ) then
      ncol=ncol+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_dat_file_pv: error while determining columns number from file ',trim(namefile),' at line 1, column',ncol
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)
  error=0
  do while ( error == 0 )
    read(10,*,iostat=error) tmp
    if ( error == 0 ) then
      nline=nline+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_dat_file_pv: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  
  rewind(10)
  allocate(variable(nline,ncol-1))

  do i=1,nline
    read(10,*,IOSTAT=error) inttmp,(variable(i,j),j=1,ncol-1)
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_dat_file_pv: error reading from file ',trim(namefile),' at line',i
      call exit(1)
    end if
  end do

  close(10)

end subroutine read_dat_file_pv

end module mod_readperso
