module coreutils__file
  !! Module contains file manipulation utilities.
  use coreutils__kind, only: real32
  use coreutils__error, only: stop_program
  use coreutils__string, only: to_upper
  implicit none


  private

  public :: grep, jump, file_check, touch



contains

!###############################################################################
  subroutine grep(unit,input,lstart,lline,success)
    !! Search a file for a pattern.
    !!
    !! This subroutine searches a file for a pattern. It can search for the
    !! first line that contains the pattern or for the first line that starts
    !! with the pattern.
    implicit none

    ! Arguments
    integer :: unit
    !! Unit number of the file.
    character(*) :: input
    !! Pattern to search for.
    logical, intent(in), optional :: lstart
    !! Optional. Boolean whether to rewind file.
    logical, intent(in), optional :: lline
    !! Optional. Boolean whether the pattern is at the start of the line.
    logical, intent(out), optional :: success
    !! Optional. Boolean whether the pattern is found.

    ! Local variables
    integer :: iostat
    !! I/O status.
    character(1024) :: buffer
    !! Buffer for reading lines.
    logical :: lline_
    !! Boolean whether the pattern is at the start of the line.
    logical :: success_
    !! Boolean whether the pattern is found.


    lline_ = .false.
    success_ = .false.
    if(present(lstart))then
       if(lstart) rewind(unit)
    else
       rewind(unit)
    end if

    if(present(lline)) lline_ = lline
    if(lline_)then
       wholeloop: do
          read(unit,'(A100)',iostat=iostat) buffer
          if(is_iostat_end(iostat))then
             exit wholeloop
          elseif(iostat.ne.0)then
             call stop_program('I/O stat error encounted when reading file')
          end if
          if(index(trim(buffer),trim(input)).eq.1)then
             success_ = .true.
             exit wholeloop
          end if
       end do wholeloop
    else
       greploop: do
          read(unit,'(A100)',iostat=iostat) buffer
          if(is_iostat_end(iostat))then
             exit greploop
          elseif(iostat.ne.0)then
             call stop_program('I/O stat error encounted when reading file')
          end if
          if(index(trim(buffer),trim(input)).ne.0)then
             success_ = .true.
             exit greploop
          end if
       end do greploop
    end if

    if(present(success)) success = success_
  end subroutine grep
!###############################################################################


!###############################################################################
  subroutine jump(unit,linenum)
    !! Go to a specific line in a file.
    implicit none

    ! Arguments
    integer :: unit
    !! Unit number of the file.
    integer :: linenum
    !! Line number to jump to.

    ! Local variables
    integer :: i
    !! Loop index.


    rewind(unit)
    do i = 1, linenum, 1
       read(unit,*)
    end do

  end subroutine jump
!###############################################################################


!###############################################################################
  subroutine file_check(unit, filename, action)
    !! Check if a file exists and open it.
    implicit none

    ! Arguments
    integer, intent(inout) :: unit
    !! Unit number of the file.
    character(*), intent(inout) :: filename
    !! Name of the file.
    character(len=20), optional, intent(in) :: action
    !! Optional. Action to be taken on the file.

    ! Local variables
    integer :: i
    !! Loop index.
    integer :: iostat
    !! I/O status.
    character(20) :: action_
    !! Action to be taken on the file.
    logical :: filefound
    !! Boolean whether the file is found.


    action_="READWRITE"
    if(present(action)) action_=action
    action_=to_upper(action_)
    do i = 1, 5
       inquire(file=trim(filename),exist=filefound)
       if(.not.filefound) then
          write(6,'("File name ",A," not found.")')&
               "'"//trim(filename)//"'"
          write(6,'("Supply another filename: ")')
          read(*,*) filename
       else
          write(6,'("Using file ",A)')  &
               "'"//trim(filename)//"'"
          exit
       end if
       if(i.ge.4) then
          stop "Nope"
       end if
    end do
    if(trim(adjustl(action_)).eq.'NONE')then
       write(6,*) "File found, but not opened."
    else
       open(newunit=unit,file=trim(filename),&
            action=trim(action_),iostat=iostat)
    end if

  end subroutine file_check
!###############################################################################


!###############################################################################
  subroutine touch(file)
    !! Create a directory if it does not exist.
    implicit none

    ! Arguments
    character(*), intent(in) :: file
    !! Directory to be created.

    ! Local variables
    logical :: exists
    !! Boolean whether the directory exists.

    inquire(file=file, exist=exists)
    if(.not.exists) call execute_command_line("mkdir -p "//file)
  end subroutine touch
!###############################################################################

end module coreutils__file
