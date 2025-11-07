module coreutils__string
  !! Module contains string manipulation utilities.
  use coreutils__kind, only: real32
  implicit none


  private

  public :: icount
  public :: flagmaker
  public :: to_upper, to_lower
  public :: strip_null
  public:: count_occ



contains

!###############################################################################
  pure function icount(line,fs) result(items)
    !! Count the number of fields separated by specified delimiter.
    !!
    !! This function counts the number of fields separated by a specified
    !! delimiter in a string. The default delimiter is a space.
    implicit none

    ! Arguments
    character(*) :: line
    !! String to be counted.
    character(*), intent(in), optional :: fs
    !! Optional. Delimiter (aka field separator).

    ! Local variables
    integer :: k
    !! Loop index.
    integer :: items, pos, length
    !! Number of fields and position in the string.
    character(len=:), allocatable :: fs_
    !! Delimiter (aka field separator).


    items=0
    pos=1
    length=1
    if(present(fs)) length=len(trim(fs))
    allocate(character(len=length) :: fs_)
    if(present(fs)) then
       if(trim(fs) == " ") then
          fs_=" "
       else
          fs_=trim(fs)
       end if
    else
       fs_=" "
    end if

    loop: do
       k=verify(line(pos:),fs_)
       if (k.eq.0) exit loop
       items=items+1
       pos=k+pos-1
       k=scan(line(pos:),fs_)
       if (k.eq.0) exit loop
       pos=k+pos-1
    end do loop
    icount=items

  end function icount
!###############################################################################


!###############################################################################
  subroutine flagmaker(buffer,flag,i,skip,empty)
    !! Assign variables of flags from get_command_argument.
    implicit none

    ! Arguments
    character(*), intent(inout) :: buffer
    !! Buffer to be assigned a flag.
    character(*), intent(in) :: flag
    !! Flag to look for.
    integer :: i
    !! Index of command argument.
    logical :: skip
    !! Boolean whether to skip the next argument.
    logical, intent(out) :: empty
    !! Boolean whether the buffer is empty.


    skip = .false.
    empty = .false.
    if(len(trim(buffer)).eq.len(trim(flag))) then
       call get_command_argument(i+1,buffer)
       if(scan(buffer,'-').eq.1.or.buffer.eq.'') then
          buffer=""
          empty=.true.
       else
          skip=.true.
       end if
    else
       buffer=buffer(len(trim(flag))+1:)
    end if

  end subroutine flagmaker
!###############################################################################


!###############################################################################
  pure function to_upper(buffer) result(upper)
    !! Convert a string to upper case.
    implicit none

    ! Arguments
    character(*), intent(in) :: buffer
    !! String to be converted to upper case.
    character(len=:),allocatable :: upper
    !! Upper case string.

    ! Local variables
    integer :: i,j
    !! Loop index.


    allocate(character(len=len(buffer)) :: upper)
    do i = 1, len(buffer)
       j=iachar(buffer(i:i))
       if(j.ge.iachar("a").and.j.le.iachar("z"))then
          upper(i:i)=achar(j-32)
       else
          upper(i:i)=buffer(i:i)
       end if
    end do

  end function to_upper
!###############################################################################


!###############################################################################
  pure function to_lower(buffer) result(lower)
    !! Convert a string to lower case.
    implicit none

    ! Arguments
    character(*), intent(in) :: buffer
    !! String to be converted to lower case.
    character(len=:), allocatable :: lower
    !! Lower case string.

    ! Local variables
    integer :: i,j
    !! Loop index.


    allocate(character(len=len(buffer)) :: lower)
    do i = 1, len(buffer)
       j=iachar(buffer(i:i))
       if(j.ge.iachar("A").and.j.le.iachar("Z"))then
          lower(i:i)=achar(j+32)
       else
          lower(i:i)=buffer(i:i)
       end if
    end do

  end function to_lower
!###############################################################################


!###############################################################################
  pure function strip_null(buffer) result(stripped)
    !! Strip null characters from a string.
    !!
    !! This is meant for handling strings passed from Python, which gain
    !! null characters at the end. The procedure finds the first null
    !! character and truncates the string at that point.
    !! Null characters are represented by ASCII code 0.
    implicit none

    ! Arguments
    character(*), intent(in) :: buffer
    !! String to be stripped.
    character(len=len(buffer)) :: stripped
    !! Stripped string.

    ! Local variables
    integer :: i
    !! Loop index.

    stripped = ""
    do i = 1, len(buffer)
       if(iachar(buffer(i:i)).ne.0)then
          stripped(i:i)=buffer(i:i)
       else
          exit
       end if
    end do

  end function strip_null
!###############################################################################


!###############################################################################
  pure function count_occ(string, substring) result(num_occ)
    !! Count occurrences of a substring in a string.
    !!
    !! This function counts the number of times a specified substring
    !! appears within a given string.
    implicit none

    ! Arguments
    character(*), intent(in) :: string
    !! The main string to search within.
    character(*), intent(in) :: substring
    !! The substring to count occurrences of.

    ! Local variables
    integer :: pos, i
    !! Position and length variables for searching.
    integer :: num_occ
    !! Counter for occurrences.

    num_occ = 0
    pos = 1

    if(len_trim(substring) .eq. 0) then
       num_occ = 0
       return
    end if

    countloop: do
       i = verify(string(pos:), substring)
       if(i .eq. 0) exit countloop
       if( pos .eq. len(string)) exit countloop
       num_occ = num_occ + 1
       i = scan(string(pos:), ' ')
       if( i .eq. 0) exit countloop
       pos = i + pos - 1
    end do countloop

  end function count_occ
!###############################################################################

!###############################################################################
  subroutine read_cl(full_line, store, fs)
    !! Read command line into array of strings.
    !!
    !! This subroutine reads a full command line string and splits it into
    !! individual components based on a specified delimiter (default is space).
    !! The components are stored in an allocatable array of strings.
    implicit none

    ! Arguments
    character(*), intent(in) :: full_line
    !! Full command line string to be split.
    character(*), allocatable, dimension(:), optional, intent(inout) :: store
    !! Optional. Array to store the split components.
    character(*), optional, intent(in) :: fs
    !! Optional. Delimiter (aka field separator).

    ! Local variables
    character(len=:),allocatable :: fs_
    !! Delimiter (aka field separator).
    character(100),dimension(1000) :: tmp_store
    !! Temporary storage for split components.
    integer :: items, pos, k, length
    !! Number of components, position in string, loop index, length of fs.

    pos = 1
    items = 0
    length = 1
    if(present(fs)) length = len(trim(fs))
    allocate(character(len=length) :: fs_)
    if(present(fs)) then
       fs_ = fs
    else
       fs_ = " "
    end if

    loop: do
       k = verify( full_line(pos:), fs_ )
       if( k .eq. 0) exit loop
       pos = k + pos - 1
       k = scan( full_line(pos:), fs_ )
       if( k .eq. 0 ) exit loop
       items = items + 1
       tmp_store(items) = full_line( pos : pos + k - 1 )
       pos = k + pos - 1
    end do loop

    if(present(store))then
       if(.not.allocated(store)) allocate(store(items))
       do k = 1, items
          store(k) = trim(tmp_store(k))
       end do
    end if

  end subroutine read_cl
!###############################################################################

end module coreutils__string
