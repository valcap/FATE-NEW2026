!******************************************************************************
module mod_read_shabar  ! subrotines for reading .dat files                      !
!******************************************************************************

! FORTRAN 2008 stuff (need updating gcc)
!  use, intrinsic :: iso_fortran_env
  implicit none
!  integer, parameter :: sp = REAL32
!  integer, parameter :: dp = REAL64
!  integer, parameter :: qp = REAL128
  integer, parameter :: dp = kind(1.d0)
  integer, parameter :: maxlinelength=2000       !Maximum line lenght
  integer, parameter :: maxwordlength=2000       !Maximum word lenght
  integer, parameter :: maxcol=30                !Maximum column number
  integer, parameter :: maxnamefile=480          !Maximum length of namefile
  private
  public :: read_shabar

contains
!******************************************************************************
subroutine read_shabar(namefile,header,heigth,cn2)
!The routine takes as an input "namefile" (char length=maxnamefile=480)
!OUTPUTS:
!header (ncol,nprofiles) contains the header of each profile (year, month, day,
!hour, minute, second, zenith angle) in each row, and has nprofiles rows
!height (proflevels) contains the heigth in meters corresponding to each level
!(71 levels), rescaled to the zenith)
!cn2 (proflevels,nprofiles) contains the cn2 profiles on the 71 levels, and has
!nprofiles profiles
!!!!ATTENTION!!!!
! THE OUTPUTS (header,heigth, cn2) MUST BE ALLOCATABLE REAL AND MUST BE GIVEN AS
! UNALLOCATED AS AN INPUT. THE ROUTINE WILL ALLOCATE THEM TO THE RIGHT DIMENSION
!******************************************************************************
  implicit none
  integer, parameter :: headercolumns=7          !the number of columns in the header
  integer, parameter :: datacolumns=2            !the number of columns in the data (cn2 profiles)
  integer :: i,j,error,nline,ncol,countline,idx,nprofiles
  real :: tmp
  real, dimension(:,:), allocatable, intent(inout) :: header,cn2
  real, dimension(:), allocatable, intent(inout) :: heigth
  logical :: ok
  character(maxnamefile), intent(in) :: namefile
  character(maxlinelength) :: line

  if (allocated(cn2)) then
    print*, '!!! read_shabar: fourth input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

  if (allocated(heigth)) then
    print*, '!!! read_shabar: third input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

  if (allocated(header)) then
    print*, '!!! read_shabar: second input variable is already allocated when &
            &calling the subroutine'
    call exit(1)
  end if

  inquire( file=namefile, exist=ok )
  if (.not. ok) then
    print*, '!!! read_shabar: file ',trim(namefile),' does not exists'
    call exit(1)
  end if
  open(unit=10,status='old',file=namefile,access='sequential')

  !Count the number of lines in headerline
  ncol=0
  read(10,'(A)' ) line
  call countcolumns_in_line(line,ncol)
  if (ncol.ne.headercolumns) then
      print*, '!!! read_shabar: the number of header columns is different from 5'
      print*, '!!!              columns found:',ncol
      print*, '!!!              wrong format!'
      call exit(1)
  end if
  rewind(10)

  !Count the total number of lines
  nline=0  
  error=0
  do while ( error == 0 )
    read(10,*,iostat=error) tmp
    if ( error == 0 ) then
      nline=nline+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! read_shabar: error while determining line number from file',trim(namefile),' at line',nline
        call exit(1)
      end if
      exit
    end if
  end do
  rewind(10)

  !Count the number of lines for each cn2 profile
  countline=0
  !Jump the first header line
  read(10,'(A)' ) line
  do while ( countline .lt. nline)
    ncol=0
    read(10,'(A)' ) line
    call countcolumns_in_line(line,ncol)
    if (ncol .eq. datacolumns) then
        countline=countline+1
    else if (ncol .eq. headercolumns) then
        exit
    else
        print*, '!!! read_shabar: too many columns in file ',trim(namefile),' at line ',countline+1
    end if
  end do
  rewind(10)

  !There should be countline+1 lines for each cn2 profile
  nprofiles=int(real(nline)/real(countline+1))
  if ( (real(nline)/real(countline+1)) .ne. real(int(real(nline)/real(countline+1))) ) then
    print*, '!!! read_shabar: the number of lines does not match with the number of profiles'
    print*, '                 there should be countline+1 lines for each cn2 profile'
    print*, '                 nline=',nline
    print*, '                 countline=',countline
    print*, '                 nprofiles=',nprofiles
    call exit(1)
  end if

  print*, 'read_shabar:',trim(namefile),' has number of profiles =',nprofiles

  !Read everything
  allocate(cn2(countline,nprofiles))
  allocate(heigth(countline))
  allocate(header(headercolumns,nprofiles))
  idx=0
  do i=1,nprofiles
    read(10,*,IOSTAT=error) (header(j,i),j=1,headercolumns)
    idx=idx+1
    if (error < 0) exit
    if (error > 0) then
      print*, '!!! read_file: error reading from file ',trim(namefile),' at line',idx
      call exit(1)
    end if
    do j=1,countline
      read(10,*,IOSTAT=error) heigth(j),cn2(j,i)
      idx=idx+1
      if (error < 0) exit
      if (error > 0) then
        print*, '!!! read_file: error reading from file ',trim(namefile),' at line',idx
        call exit(1)
      end if
    end do
  end do
  close(10)

end subroutine read_shabar

!******************************************************************************
subroutine countcolumns_in_line(line,ncol)

  character(maxlinelength), intent(in) :: line
  integer, intent(out) :: ncol
  integer :: i,j,error
  character(maxwordlength) :: word

  error=0
  i=1
  do while ( error == 0 )
    read(line,*, iostat=error) ( word, j=1,i )
    i=i+1
    if (i>maxcol) then
       print*, '!!! countcolumns_in_line: too many columns in file '
       call exit(1)
    end if
    if ( error == 0 ) then
      ncol=ncol+1
    else if ( error /= 0 ) then
      if ( error > 0 ) then
        print*, '!!! countcolumns_in_line: error while determining columns number'
        call exit(1)
      end if
      exit
    end if
  end do
end subroutine countcolumns_in_line
!******************************************************************************

  
end module mod_read_shabar

