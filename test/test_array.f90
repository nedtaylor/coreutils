program test_array
  use coreutils__const, only: real32
  use coreutils__array
  implicit none

  logical :: success = .true.

  call test_sort_str(success)
  call test_sort_str_order(success)
  call test_isort1D(success)
  call test_rsort1D(success)
  call test_iset(success)
  call test_rset(success)
  call test_cset(success)
  call test_ishuffle(success)
  call test_rshuffle(success)
  call test_rswap(success)
  call test_rswap_vec(success)


  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_array passed all tests'
  else
     write(0,*) 'test_array failed one or more tests'
     stop 1
  end if

contains

  subroutine test_sort_str(success)
    implicit none
    logical, intent(inout) :: success
    character(len=20), dimension(5) :: list
    character(len=20), dimension(5) :: expected_list
    list = [ &
         'banana    ', 'apple     ', 'cherry    ', 'date      ', 'elderberry' &
    ]
    expected_list = [ &
         'apple     ', 'banana    ', 'cherry    ', 'date      ', 'elderberry' &
    ]
    call sort_str(list)
    call assert( &
         all(list .eq. expected_list), &
         'test_sort_str failed', success &
    )

    list = [ &
         'Banana    ', 'cherry    ', 'banana    ', 'date      ', 'elderberry' &
    ]
    expected_list = [ &
         'Banana    ', 'banana    ', 'cherry    ', 'date      ', 'elderberry' &
    ]
    call sort_str(list, lcase=.true.)
    call assert( &
         all(list .eq. expected_list), &
         'test_sort_str failed with ignore case', success &
    )
  end subroutine test_sort_str

  subroutine test_sort_str_order(success)
    implicit none
    logical, intent(inout) :: success
    character(len=20), dimension(5) :: list
    integer, dimension(5) :: expected_order = (/ 2, 1, 3, 4, 5 /)
    integer, dimension(:), allocatable :: order
    list = [ &
         'banana    ', 'apple     ', 'cherry    ', 'date      ', 'elderberry' &
    ]
    allocate(order, source=sort_str_order(list))
    call assert( &
         all(order .eq. expected_order), &
         'test_sort_str_order failed', success &
    )

    list = [ &
         'banana    ', 'cherry    ', 'Banana    ', 'date      ', 'elderberry' &
    ]
    expected_order = [ 1, 3, 2, 4, 5 ]
    order = sort_str_order(list, lcase=.true.)
    call assert( &
         all(order .eq. expected_order), &
         'test_sort_str_order failed with ignore case', success &
    )
  end subroutine test_sort_str_order

  subroutine test_isort1D(success)
    implicit none
    logical, intent(inout) :: success
    integer, dimension(5) :: arr = [5, 3, 4, 1, 2]
    integer, dimension(5) :: expected_arr = [1, 2, 3, 4, 5]
    call sort1D(arr)
    call assert( &
         all(arr .eq. expected_arr), &
         'test_isort1D failed', success &
    )
    expected_arr = [5, 4, 3, 2, 1]
    call sort1D(arr, reverse=.true.)
    call assert( &
         all(arr .eq. expected_arr), &
         'test_isort1D failed with reverse', success &
    )
  end subroutine test_isort1D

  subroutine test_rsort1D(success)
    implicit none
    logical, intent(inout) :: success
    real(real32), dimension(5) :: arr = &
         [5._real32, 3._real32, 4._real32, 1._real32, 2._real32]
    real(real32), dimension(5) :: expected_arr = &
         [1._real32, 2._real32, 3._real32, 4._real32, 5._real32]
    call sort1D(arr)
    call assert( &
         all( abs(arr - expected_arr) .lt. 1.E-6), &
         'test_rsort1D failed', success &
    )
    expected_arr = [5._real32, 4._real32, 3._real32, 2._real32, 1._real32]
    call sort1D(arr, reverse=.true.)
    call assert( &
         all(arr .eq. expected_arr), &
         'test_rsort1D failed with reverse', success &
    )
  end subroutine test_rsort1D

  subroutine test_iset(success)
    implicit none
    logical, intent(inout) :: success
    integer, dimension(:), allocatable :: arr
    integer, dimension(:), allocatable :: expected_arr
    allocate(arr(6))
    arr = [1, 2, 2, 3, 3, 3]
    allocate(expected_arr(3))
    expected_arr = [1, 2, 3]
    call set(arr)
    call assert( &
         all(arr .eq. expected_arr), &
         'test_iset failed', success &
    )
  end subroutine test_iset

  subroutine test_rset(success)
    implicit none
    logical, intent(inout) :: success
    real(real32), dimension(:), allocatable :: arr
    real(real32), dimension(:), allocatable :: expected_arr
    allocate(arr(6))
    arr = [1._real32, 2._real32, 2._real32, 3._real32, 3._real32, 3._real32]
    allocate(expected_arr(3))
    expected_arr = [1._real32, 2._real32, 3._real32]
    call set(arr)
    call assert( &
         all( abs(arr - expected_arr) .lt. 1.E-6), &
         'test_rset failed', success &
    )
    arr = [1._real32, 2._real32, 2.00001_real32, 3._real32, 3._real32]
    expected_arr = [1._real32, 2._real32, 2.00001_real32, 3._real32]
    call set(arr, tol=1.E-6)
    call assert( &
         all( abs(arr - expected_arr) .lt. 1.E-6), &
         'test_rset failed with lower tolerance', success &
    )
  end subroutine test_rset

  subroutine test_cset(success)
    implicit none
    logical, intent(inout) :: success
    character(len=20), dimension(:), allocatable :: arr
    character(len=20), dimension(:), allocatable :: expected_arr
    allocate(arr(6))
    arr(:) = [ 'apple ', 'banana', 'banana', 'cherry', 'cherry', 'cherry' ]
    allocate(expected_arr(3))
    expected_arr(:) = [ 'apple ', 'banana', 'cherry' ]
    call set(arr)
    call assert( &
         all(arr .eq. expected_arr), &
         'test_cset failed', success &
    )
    deallocate(arr)
    allocate(arr(6))
    arr = [ 'apple ', 'Banana', 'banana', 'cherry', 'cherry', 'cherry' ]
    expected_arr(:) = [ 'apple ', 'banana', 'cherry' ]
    call set(arr, lcase=.true.)
    call assert( &
         all(arr .eq. expected_arr), &
         'test_cset failed with ignore case', success &
    )
    deallocate(arr)
    allocate(arr(6))
    arr = [ 'apple ', 'Banana', 'banana', 'cherry', 'cherry', 'cherry' ]
    deallocate(expected_arr)
    allocate(expected_arr(6))
    expected_arr(:4) = &
         [ 'Banana', 'apple ', 'banana', 'cherry' ]
    expected_arr(5:) = ''
    call set(arr, lkeep_size=.true.)
    call assert( &
         size(arr) .eq. 6, &
         'test_cset failed to keep size', success &
    )
    call assert( &
         all(arr(:4) .eq. expected_arr(:4)), &
         'test_cset failed with keep_size', success &
    )
  end subroutine test_cset

  subroutine test_ishuffle(success)
    implicit none
    logical, intent(inout) :: success
    integer  :: i
    logical :: ltmp1
    integer :: arr(1,5)
    integer :: original_arr(1,5)

    arr(1,:) = [1, 2, 3, 4, 5]
    original_arr(1,:) = arr(1,:)
    call shuffle(arr, dim=2, seed=0)
    ltmp1 = .true.
    do i = 1, size(arr,dim=2)
       if(all(abs(arr(1,i) - original_arr(1,:)).gt.0)) then
          ltmp1 = .false.
          exit
       end if
    end do
    call assert(ltmp1, "ishuffle failed", success)
  end subroutine test_ishuffle

  subroutine test_rshuffle(success)
    implicit none
    logical, intent(inout) :: success
    integer  :: i
    logical :: ltmp1
    real(real32) :: arr(1,5)
    real(real32) :: original_arr(1,5)

    arr(1,:) = [1._real32, 2._real32, 3._real32, 4._real32, 5._real32]
    original_arr(1,:) = arr(1,:)
    call shuffle(arr, dim=2, seed=0)
    ltmp1 = .true.
    do i = 1, size(arr,dim=2)
       if(all(abs(arr(1,i) - original_arr(1,:)).gt.1.E-6)) then
          ltmp1 = .false.
          exit
       end if
    end do
    call assert(ltmp1, "rshuffle failed", success)
  end subroutine test_rshuffle

  subroutine test_rswap(success)
    implicit none
    logical, intent(inout) :: success
    real(real32) :: a = 1._real32
    real(real32) :: b = 2._real32
    real(real32) :: expected_a = 2._real32
    real(real32) :: expected_b = 1._real32

    call swap(a, b)
    call assert( &
         abs(a - expected_a).lt. 1.E-6 .and. &
         abs(b - expected_b).lt. 1.E-6, &
         "rswap failed", success &
    )

  end subroutine test_rswap

  subroutine test_rswap_vec(success)
    implicit none
    logical, intent(inout) :: success
    real(real32), dimension(2) :: a = [1._real32, 2._real32]
    real(real32), dimension(2) :: b = [3._real32, 4._real32]

    call swap(a, b)
    call assert( &
         all( abs(a - [3._real32, 4._real32]) .lt. 1.E-6_real32 ) .and. &
         all( abs(b - [1._real32, 2._real32]) .lt. 1.E-6_real32 ), &
         "rswap_vec failed", success &
    )

  end subroutine test_rswap_vec

!###############################################################################

  subroutine assert(condition, message, success)
    implicit none
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    logical, intent(inout) :: success
    if (.not. condition) then
       write(0,*) "Test failed: ", message
       success = .false.
    end if
  end subroutine assert

end program test_array
