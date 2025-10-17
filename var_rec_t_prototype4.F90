module set_precision
  use iso_fortran_env, only : real64, int32, int64
  implicit none
  private
  public :: dp, i4, i8
  integer, parameter :: dp  = real64
  integer, parameter :: i4  = int32
  integer, parameter :: i8  = int64
end module set_precision

module set_constants
  use set_precision, only : dp
  implicit none
  private
  public :: zero, one, two, three, four
  public :: half, third, fourth
  public :: pi, large, near_zero
  public :: max_text_line_length
  real(dp), parameter :: zero      = 0.0_dp
  real(dp), parameter :: one       = 1.0_dp
  real(dp), parameter :: two       = 2.0_dp
  real(dp), parameter :: three     = 3.0_dp
  real(dp), parameter :: four      = 4.0_dp
  real(dp), parameter :: third     = one / three
  real(dp), parameter :: fourth    = 0.25_dp
  real(dp), parameter :: half      = 0.50_dp
  real(dp), parameter :: large  = huge(one)
  real(dp), parameter :: pi     = acos(-one)
  real(dp), parameter :: near_zero = epsilon(one)
  integer,  parameter :: max_text_line_length = 1024
end module set_constants

module project_inputs
  implicit none
  private
  public :: verbose_level
  integer :: verbose_level = 0
end module project_inputs

module message

  implicit none

  private

  public :: error_message, warning_message
  public :: WARN_ALWAYS, WARN_SOMETIMES, WARN_RARELY

  integer, parameter :: WARN_ALWAYS    = 0
  integer, parameter :: WARN_SOMETIMES = 1
  integer, parameter :: WARN_RARELY    = 2

contains

!================================ error_message ==============================80
!>
!! Description: Writes an error message to the screen.
!!
!! Inputs:      routine_name: Routine in which error is occuring
!!              message:      Error message to print to the screen
!!
!! Outputs:     err:          Flag indicating an error
!<
!=============================================================================80
  function error_message( routine_name, message ) result( err )

    use ISO_FORTRAN_ENV, only : error_unit

    implicit none

    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: err

    continue

    err = .true.

    write(error_unit,*)
    write(error_unit,*) ' ERROR: In ' // trim(routine_name)
    write(error_unit,*) '   ', trim(message)
    write(error_unit,*) ' Stopping ...'
    call abort
    stop

  end function error_message

!=============================== warning_message =============================80
!>
!! Description: Writes a warning message to the screen.
!!
!! Inputs:      warn_level:   Important level for warning output
!!              routine_name: Routine in which warning is occuring
!!              message:      Warning message to print to the screen
!!
!! Outputs:     warn:         Flag indicating an warning
!<
!=============================================================================80
  function warning_message( warn_level, routine_name, message ) result( warn )

    use ISO_FORTRAN_ENV, only : error_unit
    use project_inputs,  only : verbose_level

    implicit none

    integer,      intent(in) :: warn_level
    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: warn

    continue

    ! Setup
    warn = .true.

    ! Print Warning Message
    if ( warn_level <= verbose_level ) then
      write(error_unit,*)
      write(error_unit,*) ' WARNING: In ' // trim(routine_name)
      write(error_unit,*) '   ', trim(message)
    end if

  end function warning_message

end module message

module debug

  implicit none

  private

  public :: write_array

  interface write_array
    procedure write_integer_array_to_stdout
    procedure write_integer_array_to_file
    procedure write_real_array_to_stdout
    procedure write_real_array_to_file
    procedure write_logical_array_to_stdout
    procedure write_logical_array_to_file
  end interface write_array

contains

  subroutine write_real_array_to_file( x, dim, filename )

    use set_precision, only : dp

    real(dp), dimension(:), intent(in) :: x
    integer,  dimension(:), intent(in) :: dim
    character(*),           intent(in) :: filename
    integer :: fid, ndim, nx, i
    continue
    ndim = size(dim)
    nx   = size(x)

    if (nx /= product(dim) ) then
      write(*,*) 'ERROR: Array dimensions are incorrect.'
      return
    end if
    ! Open File
    open( newunit=fid, file=trim(filename), status='replace',action='write')
    ! Write Array Size
    write(fid,*) ( dim(i), i = 1, ndim )
    ! Write Array
    do i = 1,nx
      write(fid,*) x(i)
    end do

    close(fid)

  end subroutine write_real_array_to_file

  subroutine write_integer_array_to_file( x, dim, filename )
    integer, dimension(:), intent(in) :: x
    integer, dimension(:), intent(in) :: dim
    character(*),          intent(in) :: filename
    integer :: fid, ndim, nx, i
    
    ndim = size(dim)
    nx   = size(x)
    if (nx /= product(dim) ) then
      write(*,*) 'ERROR: Array dimensions are incorrect.'
      return
    end if
    ! Open File
    open( newunit=fid, file=trim(filename), status='replace',action='write')
    ! Write Array Size
    write(fid,*) ( dim(i), i = 1, ndim )
    ! Write Array
    do i = 1,nx
      write(fid,*) x(i)
    end do
    close(fid)

  end subroutine write_integer_array_to_file

  subroutine write_logical_array_to_file( x, dim, filename )
    logical, dimension(:), intent(in) :: x
    integer, dimension(:), intent(in) :: dim
    character(*),          intent(in) :: filename
    integer :: fid, ndim, nx, i

    ndim = size(dim)
    nx   = size(x)
    if (nx /= product(dim) ) then
      write(*,*) 'ERROR: Array dimensions are incorrect.'
      return
    end if
    ! Open File
    open( newunit=fid, file=trim(filename), status='replace',action='write')
    ! Write Array Size
    write(fid,*) ( dim(i), i = 1, ndim )
    ! Write Array
    do i = 1,nx
      write(fid,*) x(i)
    end do
    close(fid)

  end subroutine write_logical_array_to_file

  subroutine write_real_array_to_stdout( x )
    use set_precision, only : dp
    real(dp), dimension(:), intent(in) :: x
    integer :: i
    do i = 1,size(x)
      write(*,*) x(i)
    end do
  end subroutine write_real_array_to_stdout

  subroutine write_integer_array_to_stdout( x )
    integer, dimension(:), intent(in) :: x
    integer :: i
    do i = 1,size(x)
      write(*,'(I0)') x(i)
    end do
  end subroutine write_integer_array_to_stdout

  subroutine write_logical_array_to_stdout( x )
    logical, dimension(:), intent(in) :: x
    integer :: i
    do i = 1,size(x)
      write(*,'(I0)') x(i)
    end do
  end subroutine write_logical_array_to_stdout

end module debug

module timer_derived_type

  use set_precision, only : dp
  use set_constants, only : zero

  implicit none

  private

  public :: basic_timer_t

  type :: basic_timer_t
    private
    real(dp)         :: time_start   = zero
    real(dp), public :: time_elapsed = zero
  contains
    private
    procedure, public, pass :: tic => timer_tick
    procedure, public, pass :: toc => timer_tock
  end type basic_timer_t

contains


function get_time()
  integer(kind=8) :: ticks, ticks_per_sec, max_ticks
  real(dp) :: get_time

  call system_clock( count      = ticks,                                     &
                     count_rate = ticks_per_sec,                             &
                     count_max  = max_ticks )

  if ( ticks_per_sec == 0 ) then
    get_time = zero
  else
    get_time = real(ticks,dp) / real(ticks_per_sec,dp)
  end if
end function get_time

  subroutine timer_tick( this )
    class(basic_timer_t), intent(inout) :: this
    this%time_elapsed = zero
    this%time_start   = get_time()
  end subroutine timer_tick

  function timer_tock( this )
    class(basic_timer_t), intent(in) :: this
    real(dp)                         :: timer_tock
    timer_tock = get_time() - this%time_start
  end function timer_tock

end module timer_derived_type

module quick_sort
  implicit none
  private
  public :: qsort_i8_1D
contains
  pure recursive subroutine qsort_i8_1D( m, A, indx )
    use set_precision, only : i8
    integer(i8),               intent(in)    :: m
    integer(i8), dimension(m), intent(inout) :: A
    integer(i8), dimension(m), intent(inout) :: indx
    integer(i8) :: iq
    if ( m > 1 ) then
      call partition_i8_1D( m, A, indx, iq )
      call qsort_i8_1D( iq-1  , A(1:iq-1), indx(1:iq-1) )
      call qsort_i8_1D( m-iq+1, A(iq:m)  , indx(iq:m)   )
    end if
  end subroutine qsort_i8_1D
  pure subroutine partition_i8_1D( m, A, indx, marker )
    use set_precision, only : i8
    integer(i8),               intent(in)    :: m
    integer(i8), dimension(m), intent(inout) :: A
    integer(i8), dimension(m), intent(inout) :: indx
    integer(i8),               intent(out)   :: marker
    integer :: return_val
    integer(i8) :: i, j
    integer(i8) :: temp_indx
    integer(i8) :: x, temp_A
    x = A(1)
    i = 0
    j = m+1
    do
      do
        j = j-1
        return_val = compare_i8_1D( A(j), x )
        if ( return_val == -1 .or. return_val == 0 ) exit
        ! if ( A(j) <= x ) exit
      end do
      do
        i = i+1
        return_val = compare_i8_1D( A(i), x )
        if ( return_val == +1 .or. return_val == 0 ) exit
        ! if ( A(j) >= x ) exit
      end do
      if ( i < j ) then
        temp_A    = A(i)
        temp_indx = indx(i)
        A(i)      = A(j)
        indx(i)   = indx(j)
        A(j)      = temp_A
        indx(j)   = temp_indx
      elseif ( i == j ) then
        marker = i+1
        return
      else
        marker = i
        return
      end if
    end do
  end subroutine partition_i8_1D

  pure elemental function compare_i8_1D( a, b ) result( comparison )
    use set_precision, only : i8
    integer(i8), intent(in) :: a, b
    integer                 :: comparison
    comparison = 0
    if ( a < b ) then
      comparison = -1
    elseif ( a > b ) then
      comparison = +1
    end if
  end function compare_i8_1D
end module quick_sort

module sort_unique
  use set_precision, only : i4, i8
  implicit none
  private
  public :: unique, count_unique

  interface unique
    module procedure unique_i4
    module procedure unique_i8
  end interface unique

  interface count_unique
    module procedure count_unique_i4
    module procedure count_unique_i8
  end interface count_unique
contains

  pure subroutine unique_i8(list,unique_list,n_unique)
    use quick_sort, only : qsort_i8_1D
    !! usage sortedlist=Unique(list)
    integer(i8), dimension(:),          intent(in) :: list
    integer(i8), dimension(size(list)), intent(out) :: unique_list
    integer(i8),                        intent(out) :: n_unique
    integer(i8), dimension(size(list)) :: tmp_list, idx
    logical,     dimension(size(list)) :: mask
    integer(i8) :: i, N
    ! sort
    tmp_list = list
    N=size(list)
    call qsort_i8_1D( N, tmp_list, idx )

    ! cull duplicate indices
    mask = .false.
    mask(1:N-1) = ( tmp_list(1:N-1)==tmp_list(2:N) )
    unique_list = 0
    n_unique = count(.not.mask)
    unique_list(1:n_unique) = pack(tmp_list,.not.mask)
  end subroutine unique_i8

  pure function count_unique_i8(list)
    integer(i8), dimension(:), intent(in) :: list
    integer(i8), dimension(size(list)) :: unique_list
    integer(i8) :: count_unique_i8
    call unique_i8( list, unique_list, count_unique_i8 )
  end function count_unique_i8

  pure subroutine unique_i4(list,unique_list,n_unique)
    use quick_sort, only : qsort_i8_1D
    !! usage sortedlist=Unique(list)
    integer(i4), dimension(:),          intent(in) :: list
    integer(i4), dimension(size(list)), intent(out) :: unique_list
    integer(i4),                        intent(out) :: n_unique
    integer(i8), dimension(size(list)) :: tmp_list, unique_list_i8
    logical,     dimension(size(list)) :: mask
    integer(i8) :: n_unique_i8
    call unique_i8( int(list,i8), unique_list_i8, n_unique_i8 )
    unique_list = int(unique_list_i8,i4)
    n_unique    = int( n_unique, i4)
  end subroutine unique_i4

  pure function count_unique_i4(list)
    integer(i4), dimension(:), intent(in) :: list
    integer(i4), dimension(size(list)) :: unique_list
    integer(i4) :: count_unique_i4
    call unique_i4( list, unique_list, count_unique_i4 )
  end function count_unique_i4

end module sort_unique

module index_conversion
  implicit none
  private
  public :: global2local, global2local_bnd, global2local_ghost
  public :: local2global, local2global_bnd, local2global_ghost
  public :: in_bound, cell_face_nbors
  public :: get_face_idx_from_id
  public :: get_reshape_indices
  public :: get_number_of_faces, enumerate_faces, fetch_faces
  public :: get_number_of_interior_faces, get_number_of_boundary_faces
  public :: global2local_face, local2global_face
  public :: z_indexer
  public :: generate_faces
  public :: generate_face_idx_z_order
  public :: generate_cell_idx_z_order
  public :: get_cell_face_nbors
  public :: remove_duplicates_unsorted

  interface cell_face_nbors
    module procedure cell_face_nbors_lin
    module procedure cell_face_nbors_sub
  end interface cell_face_nbors
  
  type z_indexer
    private
    integer(kind=8), dimension (0:1023) :: morton_table
  contains
    private
    procedure, public, pass :: pos_to_z
    
  end type z_indexer

  interface z_indexer
    module procedure constructor
  end interface z_indexer
contains

  pure elemental function constructor() result(this)
    type(z_indexer) :: this
    integer :: b, v, z
    do v=0, 1023
        z = 0
        do b=0, 9
            call mvbits(v,b,1,z,3*b)
        end do
        this%morton_table(v) = z
    end do
  end function constructor

  pure elemental function pos_to_z( this, i, j, k) result(z)
    use set_precision, only : i8
    class(z_indexer), intent(in) :: this
    integer, intent(in) :: i, j, k
    integer(i8) :: z
    integer(i8) :: ii, jj, kk
    integer(i8), parameter :: sz = 1023
    ii = i-1
    jj = j-1
    kk = k-1

    ! z =           this%morton_table(iand(      kk,      sz))
    ! z = z + ishft(this%morton_table(iand(      jj,      sz)),1)
    ! z = z + ishft(this%morton_table(iand(      ii,      sz)),2)
    ! z = z + ishft(this%morton_table(iand(ishft(kk,-10), sz)),30)
    ! z = z + ishft(this%morton_table(iand(ishft(jj,-10), sz)),31)
    ! z = z + ishft(this%morton_table(iand(ishft(ii,-10), sz)),32) + 1

    z =           this%morton_table(iand(      ii,      sz))
    z = z + ishft(this%morton_table(iand(      jj,      sz)),1)
    z = z + ishft(this%morton_table(iand(      kk,      sz)),2)
    z = z + ishft(this%morton_table(iand(ishft(ii,-10), sz)),30)
    z = z + ishft(this%morton_table(iand(ishft(jj,-10), sz)),31)
    z = z + ishft(this%morton_table(iand(ishft(kk,-10), sz)),32) + 1

  end function pos_to_z

  pure elemental function pos_to_z_alt( i, j, k ) result(z)
    use set_precision, only : i8
    integer, intent(in) :: i, j, k
    integer(i8) :: z
    integer(i8) :: b, ii, jj, kk
    ii = i-1
    jj = j-1
    kk = k-1

    z = 0
    do b=0, 19
      call mvbits(kk,b,1,z,3*b+2)
      call mvbits(ii,b,1,z,3*b+1)
      call mvbits(jj,b,1,z,3*b  )
      ! call mvbits(ii,b,1,z,3*b+2)
      ! call mvbits(jj,b,1,z,3*b+1)
      ! call mvbits(kk,b,1,z,3*b  )
    end do

    z = z+1

  end function pos_to_z_alt

  subroutine generate_face_idx_z_order(n_dim,n_cells,n_faces,face_map)
    use set_precision, only : i8
    use quick_sort, only : qsort_i8_1D
    integer,               intent(in) :: n_dim
    integer, dimension(3), intent(in) :: n_cells
    integer,               intent(in) :: n_faces
    integer, dimension(n_faces,2), intent(out) :: face_map
    integer(i8), dimension(n_faces) :: indx1, indx2
    integer, dimension(3) :: sz_tmp
    integer :: i, j, k, cnt, tmp1, tmp2
    sz_tmp = n_cells
    sz_tmp(1:n_dim) = 2*n_cells(1:n_dim)

    cnt = 0
    do k = 2,sz_tmp(3),2
      do j = 2,sz_tmp(2),2
        do i = 1,sz_tmp(1)+1,2
          cnt = cnt + 1
          indx2(cnt) = pos_to_z_alt(i,j,k)
        end do
      end do
    end do

    do k = 2,sz_tmp(3),2
      do j = 1,sz_tmp(2)+1,2
        do i = 2,sz_tmp(1),2
          cnt = cnt + 1
          indx2(cnt) = pos_to_z_alt(i,j,k)
        end do
      end do
    end do

    if (n_dim == 3) then
      do k = 1,sz_tmp(3)+1,2
        do j = 2,sz_tmp(2),2
          do i = 2,sz_tmp(1),2
            cnt = cnt + 1
            indx2(cnt) = pos_to_z_alt(i,j,k)
          end do
        end do
      end do
    end if

    indx1 = [(i,i=1,n_faces)]
    ! call qsort_i8_1D(int(n_faces,i8),indx2,indx1)
    face_map(:,1) = int(indx1)

    indx2 = [(i,i=1,n_faces)]
    ! call qsort_i8_1D(int(n_faces,i8),indx1,indx2)
    face_map(:,2) = int(indx2)
  end subroutine generate_face_idx_z_order

  subroutine generate_cell_idx_z_order(n_dim,n_cells,n_cells_lin,cell_map)
    use set_precision, only : i8
    use quick_sort, only : qsort_i8_1D
    integer,               intent(in) :: n_dim
    integer, dimension(3), intent(in) :: n_cells
    integer,               intent(in) :: n_cells_lin
    integer, dimension(n_cells_lin,2), intent(out) :: cell_map
    integer(i8), dimension(product(n_cells)) :: indx1, indx2
    integer :: i, j, k, cnt

    cnt = 0
    do k = 1,n_cells(3)
      do j = 1,n_cells(2)
        do i = 1,n_cells(1)
          cnt = cnt + 1
          indx2(cnt) = pos_to_z_alt(i,j,k)
        end do
      end do
    end do

    indx1 = [(i,i=1,n_cells_lin)]
    ! call qsort_i8_1D(int(n_cells_lin,i8),indx2,indx1)
    cell_map(:,1) = int(indx1)

    indx2 = [(i,i=1,n_cells_lin)]
    ! call qsort_i8_1D(int(n_cells_lin,i8),indx1,indx2)
    cell_map(:,2) = int(indx2)
  end subroutine generate_cell_idx_z_order

  subroutine remove_duplicates_unsorted(input,output,n_unique)
    integer, dimension(:),           intent(in)  :: input
    integer, dimension(size(input)), intent(out) :: output
    integer,                         intent(out) :: n_unique
    integer :: i, n_max
    n_unique = 1
    output    = 0
    output(1) = input(1)
    n_max = size(input)
    do i = 2,n_max
      if (any(output==input(i))) cycle
      n_unique = n_unique + 1
      output(n_unique) = input(i)
    end do
  end subroutine remove_duplicates_unsorted
  
  pure function global2local(iG,nSub) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: nSub
    integer, dimension(size(nSub)) :: iSub
    integer :: i, nDims, p, iGtmp, iTmp
    nDims = size(nSub)
    if (nDims==1) then
      iSub(1) = iG
      return
    end if
    p = product(nSub)
    iGtmp = iG
    do i = nDims,1,-1
      p = p/nSub(i)
      iTmp = mod(iGtmp-1,p) + 1
      iSub(i) = (iGtmp-iTmp)/p + 1
      iGtmp = iTmp
    end do
  end function global2local

  pure function local2global(iSub,nSub) result(iG)
    integer, dimension(:), intent(in) :: iSub, nSub
    integer :: iG
    integer :: nDims, p, i
    nDims = size(iSub)
    p = 1
    iG = 1
    do i = 1,nDims
        iG = iG + ( iSub(i) - 1 )*p
        p = p*nSub(i)
    end do
  end function local2global

  pure function global2local_ghost(iG,nSub,nGhost) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: nSub, nGhost
    integer, dimension(size(nSub)) :: iSub, nSub2
    nSub2 = nSub + 2*nGhost
    iSub = global2local(iG,nSub2)
    iSub = iSub - nGhost
  end function global2local_ghost

  pure function local2global_ghost(iSub,nSub,nGhost) result(iG)
    integer, dimension(:), intent(in) :: iSub, nSub, nGhost
    integer, dimension(size(nSub)) :: iSub2, nSub2
    integer :: iG
    iSub2 = iSub + nGhost
    nSub2 = nSub + 2*nGhost
    iG = local2global(iSub2,nSub2)
  end function local2global_ghost

  pure function global2local_bnd(iG,lo,hi) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: lo, hi
    integer, dimension(size(lo)) :: iSub, nSub
    nSub = hi - lo + 1
    iSub = global2local(iG,nSub)
    iSub = iSub + lo - 1
  end function global2local_bnd

  pure function local2global_bnd(iSub,lo,hi) result(iG)
    integer, dimension(:), intent(in) :: iSub, lo, hi
    integer, dimension(size(iSub)) :: idx, nSub
    integer :: iG
    idx  = iSub - lo + 1
    nSub = hi - lo + 1
    iG   = local2global(idx,nSub)
  end function local2global_bnd

  pure function get_face_intervals(n_dim,n_cells) result(intervals)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,  dimension(n_dim)             :: intervals
    integer, dimension(n_dim) :: tmp
    integer :: d
    tmp = n_cells
    tmp(1) = tmp(1) + 1
    intervals(1) = product(tmp)
    do d = 2,n_dim
      tmp = n_cells
      tmp(d) = tmp(d) + 1
      intervals(d) = intervals(d-1) + product(tmp)
    end do
  end function get_face_intervals

  pure function get_number_of_faces(n_dim,n_cells) result(n_faces)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer                                :: n_faces
    integer, dimension(n_dim) :: tmp
    integer :: d
    n_faces = 0
    do d = 1,n_dim
      tmp = n_cells
      tmp(d) = tmp(d) + 1
      n_faces = n_faces + product(tmp)
    end do
  end function get_number_of_faces

  pure function get_number_of_interior_faces(n_dim,n_cells) result(n_faces)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer                                :: n_faces
    integer, dimension(n_dim) :: tmp
    integer :: d
    n_faces = 0
    do d = 1,n_dim
      tmp = n_cells
      tmp(d) = tmp(d) - 1
      n_faces = n_faces + product(tmp)
    end do
  end function get_number_of_interior_faces

  pure function get_number_of_boundary_faces(n_dim,n_cells) result(n_faces)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer                                :: n_faces
    n_faces = get_number_of_faces(n_dim,n_cells) - get_number_of_interior_faces(n_dim,n_cells)
  end function get_number_of_boundary_faces

  pure subroutine local2global_face(n_dim,n_cells,dir,local_idx,lin_face_idx)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,                   intent(in)  :: dir
    integer, dimension(n_dim), intent(in)  :: local_idx
    integer,                   intent(out) :: lin_face_idx
    integer, dimension(n_dim) :: idx_extents
    integer, dimension(n_dim) :: nsub
    idx_extents = get_face_intervals(n_dim,n_cells)
    nsub = n_cells
    nsub(dir) = nsub(dir) + 1
    lin_face_idx = ( idx_extents(dir) - idx_extents(1) ) + local2global(local_idx,nsub)
  end subroutine local2global_face

  pure subroutine global2local_face(n_dim,n_cells,lin_face_idx,dir,local_idx)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,                   intent(in)  :: lin_face_idx
    integer,                   intent(out) :: dir
    integer, dimension(n_dim), intent(out) :: local_idx
    integer, dimension(n_dim) :: idx_extents
    integer, dimension(n_dim) :: nsub
    integer                   :: shifted_lin_idx
    integer, dimension(1) :: loc
    idx_extents = get_face_intervals(n_dim,n_cells)
    loc = findloc((lin_face_idx <= idx_extents),.true.)
    dir = loc(1)
    shifted_lin_idx = lin_face_idx - ( idx_extents(dir) - idx_extents(1) )
    nsub = n_cells
    nsub(dir) = nsub(dir) + 1
    local_idx = global2local( shifted_lin_idx,nsub)
  end subroutine global2local_face

  pure subroutine generate_faces(n_dim,n_cells,mult,n_faces,faces)
    integer,                           intent(in)  :: n_dim
    integer, dimension(n_dim),         intent(in)  :: n_cells
    integer,                           intent(in)  :: mult 
    integer,                           intent(in)  :: n_faces 
    integer, dimension(n_dim+1,n_faces), intent(out) :: faces
    integer, dimension(n_dim) :: face_sz
    integer :: d, i, cnt, sz
    cnt = 0
    do d = 1,n_dim
      face_sz = n_cells
      face_sz(d) = face_sz(d) + 1
      sz = product(face_sz)
      do i = 1,sz
        cnt = cnt + 1
        call global2local_face(n_dim,n_cells,cnt,faces(1,cnt),faces(2:n_dim,cnt))
        faces(2:n_dim,cnt) = mult*faces(2:n_dim,cnt)
        faces(1+d,cnt) = faces(1+d,cnt) - 1
      end do
    end do
  end subroutine generate_faces

  ! pure subroutine enumerate_faces(n_dim,n_cells,faces)
  !   integer,                   intent(in) :: n_dim
  !   integer, dimension(n_dim), intent(in) :: n_cells
  !   integer, dimension(2*n_dim,product(n_cells)), intent(out) :: faces
  !   integer, dimension(n_dim) :: cell_idx, face_sz, face_idx
  !   integer :: cnt, d, n
  !   do n = 1,product(n_cells)
  !     cell_idx = global2local(n,n_cells)
  !     cnt = 0
  !     do d = 1,n_dim
  !       face_sz = n_cells
  !       face_sz(d) = face_sz(d) + 1

  !       cnt = cnt + 1
  !       face_idx = cell_idx + 1
  !       face_idx(d) = face_idx(d) - 1 ! lo face
  !       faces(cnt,n) = local2global(face_idx,face_sz)

  !       cnt = cnt + 1
  !       face_idx = cell_idx
  !       face_idx(d) = face_idx(d) + 1 ! hi face
  !       faces(cnt,n) = local2global(face_idx,face_sz)
  !     end do
  !   end do
  ! end subroutine enumerate_faces

  pure subroutine enumerate_faces(n_dim,n_cells,n_faces,face_idx,faces)
    integer,                   intent(in) :: n_dim
    integer, dimension(n_dim), intent(in) :: n_cells
    integer,                     intent(in) :: n_faces
    integer, dimension(n_faces), intent(in) :: face_idx
    integer, dimension(2*n_dim,product(n_cells)), intent(out) :: faces
    integer, dimension(n_dim) :: cell_idx, face_sz, face_idx_local
    integer :: cnt, d, n, face_idx_lin
    do n = 1,product(n_cells)
      cell_idx = global2local(n,n_cells)
      cnt = 0
      do d = 1,n_dim
        face_sz = n_cells
        face_sz(d) = face_sz(d) + 1

        cnt = cnt + 1
        face_idx_local = cell_idx ! lo face
        call local2global_face(n_dim,n_cells,d,face_idx_local,face_idx_lin)
        faces(cnt,n) = face_idx( face_idx_lin )

        cnt = cnt + 1
        face_idx_local = cell_idx
        face_idx_local(d) = face_idx_local(d) + 1 ! hi face
        call local2global_face(n_dim,n_cells,d,face_idx_local,face_idx_lin)
        faces(cnt,n) = face_idx( face_idx_lin )
      end do
    end do
  end subroutine enumerate_faces

  pure subroutine get_cell_face_nbors( n_dim, n_faces, n_cells_lin, face_map_inv, cell_map_inv, n_cells, cell_idx, nbor_idx, face_idx, n_int )
    integer,                     intent(in) :: n_dim, n_faces, n_cells_lin
    integer, dimension(n_faces), intent(in) :: face_map_inv
    integer, dimension(n_cells_lin), intent(in) :: cell_map_inv
    integer, dimension(n_dim),   intent(in) :: n_cells, cell_idx
    integer, dimension(2*n_dim), intent(out) :: nbor_idx, face_idx
    integer,                     intent(out) :: n_int

    integer, dimension(n_dim) :: idx, lo, hi
    integer :: s, d, n_ext, cnt, tmp_idx
    cnt   = 0
    n_int = 0
    n_ext = 0
    lo = 1
    hi = n_cells
    do d = 1,n_dim
      do s = 0,1
        cnt = cnt + 1
        idx = cell_idx
        idx(d) = idx(d) + 2*s - 1
        if ( in_bound(n_dim,idx,lo,hi) ) then
            n_int = n_int + 1
            nbor_idx(n_int) = cell_map_inv( local2global(idx,n_cells) )
            idx(d) = idx(d) - s + 1
            call local2global_face( n_dim, n_cells, d, idx, tmp_idx )
            face_idx(n_int) = face_map_inv(tmp_idx)
        else
          nbor_idx(2*n_dim - n_ext) = cell_map_inv( local2global(cell_idx,n_cells) )
          idx(d) = idx(d) - s + 1
          call local2global_face( n_dim, n_cells, d, idx, tmp_idx )
          face_idx(2*n_dim - n_ext) = face_map_inv(tmp_idx)
          n_ext = n_ext + 1
        end if
      end do
    end do
  end subroutine get_cell_face_nbors

  pure subroutine fetch_faces(n_dim,n_cells,n_faces,face_idx_inv,faces,cell_idx,face_idxs)
    integer, intent(in) :: n_dim
    integer, dimension(n_dim), intent(in) :: n_cells
    integer, intent(in) :: n_faces
    integer, dimension(n_faces), intent(in) :: face_idx_inv
    integer, dimension(2*n_dim,product(n_cells)), intent(in) :: faces
    integer, dimension(n_dim), intent(in) :: cell_idx
    integer, dimension(n_dim+1,2*n_dim), intent(out) :: face_idxs
    integer, dimension(n_dim) :: face_sz

    integer :: cnt, dir, n, cell_idx_linear
    cell_idx_linear = local2global(cell_idx,n_cells)
    cnt = 0
    do n = 1,2*n_dim
      cnt = cnt + 1
      dir = (n-1)/2 + 1 ! direction
      face_idxs(1,cnt) = dir
      face_sz = n_cells
      face_sz(dir) = face_sz(dir) + 1
      call global2local_face(n_dim,n_cells,face_idx_inv(faces(n,cell_idx_linear)), face_idxs(1,cnt), face_idxs(2:n_dim+1,cnt) )
      ! face_idxs(2:n_dim+1,cnt) = global2local(faces(n,cell_idx_linear),face_sz)
    end do
  end subroutine fetch_faces

  pure function in_bound( dim, idx, bnd_min, bnd_max )
    integer,                 intent(in) :: dim
    integer, dimension(dim), intent(in) :: idx, bnd_min, bnd_max
    logical                             :: in_bound
    in_bound =     all(idx>=bnd_min).and.all(idx<=bnd_max)                       &
              .or. all(idx<=bnd_min).and.all(idx>=bnd_max)
  end function in_bound

  pure subroutine cell_face_nbors_sub( dim, idx, bnd_min, bnd_max, nbor_cell_idx, nbor_face_id, n_int )
    integer,                       intent(in) :: dim
    integer, dimension(dim),       intent(in) :: idx, bnd_min, bnd_max
    integer, dimension(dim,2*dim), intent(out) :: nbor_cell_idx
    integer, dimension(2*dim),     intent(out) :: nbor_face_id
    integer,                       intent(out) :: n_int
    integer, dimension(dim,2*dim) :: nbor_cell_idx_tmp
    integer, dimension(2*dim) :: nbor_face_id_tmp
    integer, dimension(dim) :: idx_tmp
    integer :: s, j, n_ext, cnt
    cnt   = 0
    n_int = 0
    n_ext = 0
    do j = 1,dim
      do s = -1,1,2
        cnt = cnt + 1
        idx_tmp = idx
        idx_tmp(j) = idx_tmp(j) + s
        if ( in_bound(dim,idx_tmp,bnd_min,bnd_max) ) then
            n_int = n_int + 1
            nbor_cell_idx(:,n_int) = idx_tmp
            nbor_face_id(n_int) = cnt
        else
          n_ext = n_ext + 1
          nbor_cell_idx_tmp(:,n_ext) = idx_tmp
          nbor_face_id_tmp(n_ext) = cnt
        end if
      end do
    end do
    do j = 1,n_ext
      nbor_cell_idx(:,n_int+j) = nbor_cell_idx_tmp(:,j)
      nbor_face_id(n_int+j) = nbor_face_id_tmp(j)
    end do
  end subroutine cell_face_nbors_sub

  pure subroutine cell_face_nbors_lin( dim, lin_idx, bnd_min, bnd_max, &
                                       nbor_cell_idx, nbor_face_id, n_int )
    integer,                       intent(in) :: dim, lin_idx
    integer, dimension(dim),       intent(in) :: bnd_min, bnd_max
    integer, dimension(2*dim), intent(out) :: nbor_cell_idx
    integer, dimension(2*dim), intent(out) :: nbor_face_id
    integer,                       intent(out) :: n_int
    integer, dimension(dim,2*dim) :: nbor_idx
    integer, dimension(dim) :: idx
    integer :: s, j, n_ext, cnt
    idx = global2local_bnd(lin_idx,bnd_min,bnd_max)
    call cell_face_nbors_sub( dim, idx, bnd_min, bnd_max, nbor_idx, nbor_face_id, n_int )
    do j = 1,2*dim
      nbor_cell_idx(j) = local2global_bnd(nbor_idx(:,j),bnd_min,bnd_max)
    end do
  end subroutine cell_face_nbors_lin

  

  pure elemental subroutine get_face_info_from_id(face_id,dir,offset)
    integer, intent(in)  :: face_id
    integer, intent(out) :: dir, offset
    dir    = (face_id-1)/2 + 1
    offset = mod(face_id+1,2)
  end subroutine get_face_info_from_id

  pure subroutine get_face_idx_from_id(idx,face_id,dir,face_idx)
    integer, dimension(:),         intent(in) :: idx
    integer,                       intent(in)  :: face_id
    integer,                       intent(out) :: dir
    integer, dimension(size(idx)), intent(out) :: face_idx

    integer, dimension(size(idx)) :: face_offset
    integer :: offset
    call get_face_info_from_id(face_id,dir,offset)
    face_offset = 0
    face_offset(dir) = offset
    face_idx = idx + face_offset
  end subroutine get_face_idx_from_id

  pure subroutine get_reshape_indices( sz_in, loc, sz_out, sz_cnt, idx_start, idx_end )
    integer, dimension(:),           intent(in)  :: sz_in
    integer, dimension(size(sz_in)), intent(in)  :: loc
    integer, dimension(size(sz_in)), intent(out) :: sz_out
    integer,                         intent(out) :: sz_cnt
    integer, dimension(size(sz_in)), intent(out) :: idx_start
    integer, dimension(size(sz_in)), intent(out) :: idx_end

    logical, dimension(size(sz_in)) :: lo, hi, varies

    lo     = (loc==0)
    hi     = (loc==1)
    varies = (loc==2)
    sz_out    = 1
    sz_cnt    = count(varies)
    sz_out(1:sz_cnt) = pack(sz_in,varies)
    idx_start = 1
    idx_end   = 1

    where ( lo .or. varies ) idx_start = 1
    where ( hi             ) idx_start = sz_in
    where ( lo             ) idx_end   = 1
    where ( hi .or. varies ) idx_end   = sz_in

  end subroutine get_reshape_indices

end module index_conversion

module reshape_array
  use set_precision, only : dp
  implicit none
  private
  public :: extract_1D_slice_from_3D_array
  public :: extract_2D_slice_from_3D_array
contains

  pure function extract_1D_slice_from_3D_array(A,lo,hi,sz) result(slice)
    use index_conversion, only : get_reshape_indices
    real(dp), dimension(:,:,:), intent(in) :: A
    integer,  dimension(3),     intent(in) :: lo, hi
    integer,                   intent(in) :: sz
    real(dp), dimension(sz) :: slice
    ! integer :: i, j, k, cnt

    slice = reshape( A(lo(1):hi(1),lo(2):hi(2),lo(3):hi(3)), [sz] )
    ! cnt = 0
    ! do k = lo(3),hi(3)
    !   do j = lo(2),hi(2)
    !     do i = lo(1),hi(1)
    !       cnt = cnt + 1
    !       slice(cnt) = A(i,j,k)
    !     end do
    !   end do
    ! end do
  end function extract_1D_slice_from_3D_array

  pure function extract_2D_slice_from_3D_array(A,lo,hi,sz) result(slice)
    use index_conversion, only : get_reshape_indices
    real(dp), dimension(:,:,:), intent(in) :: A
    integer,  dimension(3),     intent(in) :: lo, hi
    integer,  dimension(2),     intent(in) :: sz
    real(dp), dimension(sz(1),sz(2)) :: slice
    ! real(dp), dimension(product(sz)) :: slice_tmp
    ! integer :: i, j, k, cnt

    slice = reshape( A(lo(1):hi(1),lo(2):hi(2),lo(3):hi(3)), sz )
    ! cnt = 0
    ! do k = lo(3),hi(3)
    !   do j = lo(2),hi(2)
    !     do i = lo(1),hi(1)
    !       cnt = cnt + 1
    !       slice_tmp(cnt) = A(i,j,k)
    !     end do
    !   end do
    ! end do
    ! slice = reshape(slice_tmp,sz)
  end function extract_2D_slice_from_3D_array

end module reshape_array

module combinatorics
  implicit none
  private
  public :: nchoosek
  public :: get_exponents
contains

  pure function nchoosek( n, k ) result( c )
    integer, intent(in) :: n, k
    integer             :: c
    integer :: i
    c = 0
    if (k>n) return

    c = 1
    do i = 1, min(n-k,k)
      c = c * ( n - (i-1) )
      c = c / i
    end do
  end function nchoosek

  pure subroutine get_exponents(n_dim,degree,n_terms,exponents,idx)
    use index_conversion, only : global2local
    integer, intent(in) :: n_dim, degree, n_terms
    integer, dimension(n_dim,n_terms), intent(out) :: exponents
    integer, dimension(degree+1),      intent(out) :: idx
    integer :: curr_total_degree, j, cnt, N_full_terms
    integer, dimension(n_dim) :: tmp_exp, nsub
    cnt = 0
    do curr_total_degree = 0,degree
      idx(curr_total_degree+1) = cnt + 1
      N_full_terms = (curr_total_degree+1) ** n_dim
      do j = 0,N_full_terms
        nSub = curr_total_degree + 1
        tmp_exp = global2local(j+1,nsub)-1
        if ( sum(tmp_exp) == curr_total_degree ) then
          cnt = cnt + 1
          exponents(:,cnt) = tmp_exp
        end if
      end do
    end do
  end subroutine get_exponents

end module combinatorics

module math
  use set_precision, only : dp
  implicit none
  private
  public :: cross_product, det_3x3, vector_norm
  public :: LUdecomp, LUsolve, mat_inv
  public :: LegendrePolynomialAndDerivative, LegendreGaussNodesAndWeights
  public :: maximal_diameter, maximal_extents
  public :: rand_int_in_range


  interface LUsolve
    module procedure LUsolve_single_rhs
    module procedure LUsolve_multiple_rhs
  end interface
contains

  impure elemental function rand_int_in_range(lo,hi) result(num)
    integer, intent(in) :: lo, hi
    integer             :: num
    real(dp) :: harvest
    call random_number(harvest)
    num = nint( harvest*real(hi-lo,dp) + real(lo,dp) )
  end function rand_int_in_range

  pure function cross_product( vec1, vec2 )
    real(dp), dimension(3), intent(in) :: vec1, vec2
    real(dp), dimension(3)             :: cross_product
    cross_product(1) =  ( vec1(2)*vec2(3) - vec1(3)*vec2(2) )
    cross_product(2) = -( vec1(1)*vec2(3) - vec1(3)*vec2(1) )
    cross_product(3) =  ( vec1(1)*vec2(2) - vec1(2)*vec2(1) )
  end function cross_product

  pure function det_3x3( mat )
    real(dp), dimension(3,3), intent(in) :: mat
    real(dp)                             :: det_3x3
    continue
    det_3x3 = mat(1,1)*(mat(2,2)*mat(3,3)-mat(2,3)*mat(3,2)) &
            - mat(1,2)*(mat(2,1)*mat(3,3)-mat(2,3)*mat(3,1)) &
            + mat(1,3)*(mat(2,1)*mat(3,2)-mat(2,2)*mat(3,1))
  end function det_3x3

  pure function vector_norm( vector )
    use set_precision, only : dp
    use set_constants, only : zero
    real(dp), dimension(:), intent(in) :: vector
    real(dp)                           :: vector_norm
    integer :: i
    vector_norm = zero
    do i = 1, size(vector)
      vector_norm = vector_norm + vector(i)**2
    end do
    vector_norm = sqrt( vector_norm )
  end function vector_norm

  !================================== LUdecomp =================================80
!>
!! LU decomposition, with pivoting, for a regular NxN dense array
!<
!=============================================================================80
  pure subroutine LUdecomp( LU, P, A, m )
    use set_precision, only : dp
    use set_constants, only : zero, one
    real(dp), dimension(m,m), intent(out) :: LU,P
    real(dp), dimension(m,m), intent(in)  :: A
    integer,                  intent(in)  :: m
    real(dp), dimension(m) :: ctemp1, LUtemp
    integer  :: col, row, maxi, ipr
    real(dp) :: factor
    LU = A
    P = zero
    do col = 1,m
      P(col,col) = one
    end do
    do col = 1,m-1
!row pivot
      maxi=maxloc(abs(LU(col:m,col)),1)
      ipr=maxi+col-1
      if (ipr.ne.col) then
        ctemp1 = LU(ipr,:)
        LU(ipr,:) = LU(col,:)
        LU(col,:) = ctemp1
        ctemp1 = P(ipr,:)
        P(ipr,:) = P(col,:)
        P(col,:) = ctemp1
      end if
      if ( abs(LU(col,col)) > zero ) then
        do row = col+1,m
          factor = LU(row,col)/LU(col,col)
          LUtemp(col+1:m) = LU(row,col+1:m) - factor*LU(col,col+1:m)
          LU(row,col+1:m) = LUtemp(col+1:m)
          LU(row,col) = factor
        end do
      end if
    end do
  end subroutine LUdecomp

!================================== LUsolve ==================================80
!>
!! LU solve for a regular dense array
!<
!=============================================================================80
  pure subroutine LUsolve_single_rhs( x, LU, P, bin, m )
    use set_precision, only : dp
    real(dp), dimension(m),   intent(out) :: x
    real(dp), dimension(m,m), intent(in)  :: LU, P
    real(dp), dimension(m),   intent(in)  :: bin
    integer,                  intent(in)  :: m
    integer :: i, row
    real(dp), dimension(m) :: b, d
! Permute b matrix
    b = matmul(P,bin)
! Forward substitution
    d(1) = b(1)
    do row = 2,m
      d(row) = b(row) - sum( LU(row,1:row-1)*d(1:row-1) )
    end do
! Backward substitution
    x(m) = d(m)/LU(m,m)
    do i = 1,m-1
      row = m-i
      x(row) = ( d(row) - sum( LU(row,row+1:m)*x(row+1:m) ) ) / LU(row,row)
    end do
  end subroutine LUsolve_single_rhs

  pure subroutine LUsolve_multiple_rhs(x,LU,P,bin,m,n_rhs)
    real(dp), dimension(m,n_rhs), intent(out) :: x
    real(dp), dimension(m,m),     intent(in)  :: LU, P
    real(dp), dimension(m,n_rhs), intent(in)  :: bin
    integer,                      intent(in)  :: m, n_rhs
    integer :: n
    do n = 1, n_rhs
      call LUsolve_single_rhs(x(:,n),LU,P,bin(:,n),m)
    end do
  end subroutine LUsolve_multiple_rhs

!================================== mat_inv ==================================80
!>
!! Calculates the inverse of a general nxn matrix using LU decomposition
!<
!=============================================================================80
  subroutine mat_inv( mat, inv, n )
    use set_precision, only : dp
    use set_constants, only : zero, one
    integer,                  intent(in)  :: n
    real(dp), dimension(n,n), intent(in)  :: mat
    real(dp), dimension(n,n), intent(out) :: inv
    integer                  :: i
    real(dp), dimension(n)   :: b
    real(dp), dimension(n,n) :: lu, p
    call ludecomp( lu, p, mat, n )
    inv = zero
    do i = 1,n
      b = zero
      b(i) = one
      call lusolve( inv(:,i), lu, p, b, n )
    end do
  end subroutine mat_inv

  elemental subroutine LegendrePolynomialAndDerivative(N,x,LN,dLN)
    use set_constants, only : zero, one, two
    integer, intent(in) :: N
    real(dp), intent(in) :: x
    real(dp), intent(out) :: LN, dLN
    real(dp) :: LNm2, LNm1, dLNm2, dLNm1
    integer :: j
    if (N == 0) then
      LN = one
      dLN = zero
    elseif (N == 1) then
      LN = x
      dLN = one
    else
      LNm2 = one
      LNm1 = x
      dLNm2 = zero
      dLNm1 = one
      do j = 2,N
        LN = real(2*j-1,dp)/real(j,dp) * x * LNm1 &
          - real(j-1,dp)/real(j,dp) * LNm2
        dLN = dLNm2 + real(2*j-1,dp) * LNm1
        LNm2 = LNm1
        LNm1 = LN
        dLNm2 = dLNm1
        dLNm1 = dLN
      end do
    end if
  end subroutine LegendrePolynomialAndDerivative

  pure subroutine LegendreGaussNodesAndWeights(N,x,w)
    use set_constants, only : zero, one, two, four, third, pi
    integer,                  intent(in)  :: N
    real(dp), dimension(N+1), intent(out) :: x, w
    integer :: j, k
    real(dp) :: eps4, delta, LNp1, dLNp1
    integer, parameter :: quad_n_iter = 10
    eps4 = four*epsilon(one)
    x = zero
    w = zero

    if (N == 0) then
      x(1) = zero
      w(1) = two
    elseif (N == 1) then
      x(1) = -sqrt(third)
      w(1) = one
      x(2) = -x(1)
      w(2) = w(1)
    else
      do j = 0,(N+1)/2 - 1
        x(j+1) = -cos( ( real(2*j+1,dp)/real(2*N+2,dp) )*pi )
        do k = 1,quad_n_iter
          call LegendrePolynomialAndDerivative(N+1,x(j+1),LNp1,dLNp1)
          delta = -LNp1/dLNp1
          x(j+1) = x(j+1) + delta
          if ( abs(delta) <= eps4*abs(x(j+1)) ) then
            exit
          end if
        end do
        call LegendrePolynomialAndDerivative(N+1,x(j+1),LNp1,dLNp1)
        x(N+1-j) = -x(j+1)
        w(j+1) = two/( (one-x(j+1)**2)*dLNp1**2)
        w(N+1-j) = w(j+1)
      end do
      if (mod(N,2) == 0) then
        call LegendrePolynomialAndDerivative(N+1,zero,LNp1,dLNp1)
        x(N/2+1) = zero
        w(N/2+1) = two/dLNp1**2
      end if
    end if
  end subroutine LegendreGaussNodesAndWeights

  pure function maximal_diameter(dim,n_points,points) result(d)
    use set_constants, only : zero
    integer, intent(in) :: dim, n_points
    real(dp), dimension(dim,n_points), intent(in) :: points
    real(dp) :: d
    real(dp), dimension(dim) :: delta
    integer :: i, j
    d = zero
    do j = 1,n_points-1
      do i = j+1,n_points
        delta = points(:,j) - points(:,i)
        d = max(d,dot_product(delta,delta))
      end do
    end do
    d = sqrt(d)
  end function maximal_diameter

  pure function maximal_extents(dim,n_points,points) result(d)
    use set_constants, only : half
    integer, intent(in) :: dim, n_points
    real(dp), dimension(dim,n_points), intent(in) :: points
    real(dp), dimension(dim) :: d
    d = half*(maxval(points,dim=2) - minval(points,dim=2))
  end function maximal_extents

end module math





module vector_derived_type
  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: face_vec
  public :: face_vec_ptr_3D
  type face_vec
    integer :: n
    real(dp), allocatable, dimension(:,:) :: v
  contains
    private
    procedure, public, pass :: create  => allocate_face_vec
    procedure, public, pass :: destroy => deallocate_face_vec
  end type face_vec

  type face_vec_ptr_3D
    type(face_vec), dimension(:,:,:), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_face_vec_ptr_3D
  end type face_vec_ptr_3D

contains

  subroutine allocate_face_vec( this, n )
    class(face_vec), intent(inout) :: this
    integer,       intent(in)      :: n
    continue
    this%n = n
    allocate( this%v(3,n) )
    this%v = zero
  end subroutine allocate_face_vec

  pure elemental subroutine deallocate_face_vec( this )
    class(face_vec), intent(inout) :: this
    continue
    this%n = 0
    if( allocated( this%v  ) ) deallocate( this%v )
  end subroutine deallocate_face_vec

  pure elemental subroutine destroy_face_vec_ptr_3D( this )
    class(face_vec_ptr_3D), intent(inout) :: this
    this%p => null()
  end subroutine destroy_face_vec_ptr_3D
end module vector_derived_type

module pointers
  use set_precision, only : dp
  implicit none
  private
  public :: array_ptr_3D_real, array_ptr_4D_real

  type array_ptr_3D_real
    real(dp), dimension(:,:,:),     pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_real_3D
  end type array_ptr_3D_real

  type array_ptr_4D_real
    real(dp), dimension(:,:,:,:),   pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_real_4D
  end type array_ptr_4D_real

contains

  pure elemental subroutine destroy_real_3D( this )
    class(array_ptr_3D_real), intent(inout) :: this
    this%p => null()
  end subroutine destroy_real_3D

  pure elemental subroutine destroy_real_4D( this )
    class(array_ptr_4D_real), intent(inout) :: this
    this%p => null()
  end subroutine destroy_real_4D
end module pointers

module linspace_helper
  use set_precision, only : dp
  private
  public :: unit_cartesian_mesh_cat
  public :: unit_cartesian_mesh_cat_perturbed
  public :: linspace, meshgrid2, meshgrid3
contains
  pure function unit_cartesian_mesh_cat(nx,ny,nz) result(xyz)
    integer, intent(in) :: nx, ny, nz
    real(dp), dimension(3,nx,ny,nz) :: xyz
    real(dp), dimension(nx,ny,nz) :: tmp_x, tmp_y, tmp_z
    integer :: i, j, k

    call unit_cartesian_mesh(nx,ny,nz,tmp_x,tmp_y,tmp_z)

    do k = 1,nz
      do j = 1,ny
        do i = 1,nx
          xyz(1,i,j,k) = tmp_x(i,j,k)
          xyz(2,i,j,k) = tmp_y(i,j,k)
          xyz(3,i,j,k) = tmp_z(i,j,k)
        end do
      end do
    end do
  end function unit_cartesian_mesh_cat

  function unit_cartesian_mesh_cat_perturbed(nx,ny,nz,delta) result(xyz)
    use set_constants, only : zero, one, two
    integer, intent(in) :: nx, ny, nz
    real(dp), intent(in) :: delta
    real(dp), dimension(3,nx,ny,nz) :: xyz
    real(dp), dimension(nx,ny,nz) :: tmp_x, tmp_y, tmp_z
    integer :: i, j, k
    real(dp), dimension(3) :: h0
    real(dp) :: dx, dy, dz

    call unit_cartesian_mesh(nx,ny,nz,tmp_x,tmp_y,tmp_z)
    h0 = zero
    if ( nx > 2 ) h0(1) = one/real(nx-1,dp)
    if ( ny > 2 ) h0(2) = one/real(ny-1,dp)
    if ( nz > 2 ) h0(3) = one/real(nz-1,dp)
    h0 = h0 * delta

    call random_init(.true.,.false.)
    do k = 1,nz
      do j = 1,ny
        do i = 1,nx
          call random_number(dx); dx = two*dx - one; dx = dx*h0(1)
          call random_number(dy); dy = two*dy - one; dy = dy*h0(2)
          call random_number(dz); dz = two*dz - one; dz = dz*h0(3)
          xyz(1,i,j,k) = tmp_x(i,j,k) + dx
          xyz(2,i,j,k) = tmp_y(i,j,k) + dy
          xyz(3,i,j,k) = tmp_z(i,j,k) + dz
        end do
      end do
    end do
  end function unit_cartesian_mesh_cat_perturbed

  pure subroutine unit_cartesian_mesh(nx,ny,nz,x,y,z)
    use set_constants, only : zero, one
    integer, intent(in) :: nx, ny, nz
    real(dp), dimension(nx,ny,nz), intent(out) :: x, y, z

    call meshgrid3( linspace(nx,zero,one), &
                    linspace(ny,zero,one), &
                    linspace(nz,zero,one), x,y,z)
  end subroutine unit_cartesian_mesh

  pure function linspace(N,x1,x2) result(array)
    integer,  intent(in)   :: N
    real(dp), intent(in)   :: x1, x2
    real(dp), dimension(N) :: array
    real(dp) :: range_den
    integer :: i
    if (N==0) return
    if (N==1) then
      array(1) = x1
      return
    end if
    range_den = (x2-x1)/real(N-1,dp)
    do i = 1,N
      array(i) = x1 + range_den*real(i-1,dp)
    end do
  end function linspace

  pure subroutine meshgrid2(x1,x2,x1_array,x2_array)
    real(dp), dimension(:),   intent(in)  :: x1, x2
    real(dp), dimension(:,:), intent(out) :: x1_array, x2_array
    integer :: N1, N2
    N1 = size(x1)
    N2 = size(x2)
    x1_array = spread(x1,2,N2)
    x2_array = spread(x2,1,N1)
  end subroutine meshgrid2

  pure subroutine meshgrid3(x1,x2,x3,x1_array,x2_array,x3_array)
    real(dp), dimension(:),     intent(in)  :: x1, x2, x3
    real(dp), dimension(:,:,:), intent(out) :: x1_array, x2_array, x3_array
    real(dp), dimension(size(x1),size(x2)) :: x1_tmp
    real(dp), dimension(size(x2),size(x3)) :: x2_tmp
    real(dp), dimension(size(x2),size(x3),size(x1)) :: x2_tmp2
    real(dp), dimension(size(x3),size(x1)) :: x3_tmp
    real(dp), dimension(size(x3),size(x1),size(x2)) :: x3_tmp2
    integer, parameter, dimension(3) :: o2 = [2,3,1], o3 = [3,1,2]
    integer :: N1, N2, N3
    N1 = size(x1)
    N2 = size(x2)
    N3 = size(x3)

    x1_tmp   = spread(x1,2,N2)
    x2_tmp   = spread(x2,2,N3)
    x3_tmp   = spread(x3,2,N1)
    x1_array = spread(x1_tmp,3,N3)
    x2_tmp2  = spread(x2_tmp,3,N1)
    x3_tmp2  = spread(x3_tmp,3,N2)
    x2_array = reshape(x2_tmp2,shape(x2_array),order=o2)
    x3_array = reshape(x3_tmp2,shape(x3_array),order=o3)
  end subroutine meshgrid3

end module linspace_helper

module tecplot_output
  use set_precision, only : dp
  use set_constants, only : zero
  use message,       only : error_message
  implicit none
  private
  public :: write_tecplot_file_header
  character(*), dimension(6), parameter :: data_types=['DOUBLE  ','SINGLE  ',  &
                                                       'LONGINT ','SHORTINT',  &
                                                       'BYTE    ','BIT     ' ]
  character(*), dimension(6), parameter :: formats = ['(ES23.15)','(ES16.7) ', &
                                                      '(I11)    ','(I6)     ', &
                                                      '(I4)     ','(I1)     ']
contains

subroutine write_tecplot_zone_header( fid, n_dim, n_nodes, n_cells,            &
                                      n_node_vars, n_cell_vars, zone_name,     &
                                      data_packing, data_format, var_share,    &
                                      strand_id, solution_time )
    integer,                                intent(in) :: fid, n_dim, n_vars
    integer,      dimension(:),             intent(in) :: n_nodes
    character(*), dimension(:),             intent(in) :: var_names
    integer,                      optional, intent(in) :: n_cells
    integer,                      optional, intent(in) :: n_cell_vars
    character(*),                 optional, intent(in) :: zone_name
    integer,                      optional, intent(in) :: data_packing
    integer, dimension(:),        optional, intent(in) :: data_format
    integer, dimension(:),        optional, intent(in) :: var_share
    integer,                      optional, intent(in) :: strand_id
    real(dp),                     optional, intent(in) :: solution_time
    integer,                            intent(in) :: fid, n_dim
    integer,                            intent(in) :: n_nodes, n_cells
    integer,                            intent(in) :: n_node_var, n_cell_var
    character(*),                       intent(in) :: zone_name
    
    integer, dimension(:),    optional, intent(in) :: data_format
    integer,                  optional, intent(in) :: strand_id
    real(dp),                 optional, intent(in) :: solution_time
    if ( present(n_cells) ) then
      call write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes, n_cells,  &
                                                 n_node_vars, n_cell_vars,        &
                                                 zone_name, var_name,           &
                                                 data_format,                   &
                                                 strand_id, solution_time )
end subroutine write_tecplot_zone_header
subroutine write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,           &
                                                n_node_vars, n_cell_vars,      &
                                                zone_name,                     &
                                                data_packing,                  &
                                                data_format,                   &
                                                var_share,                     &
                                                strand_id,                     &
                                                solution_time)!,               &
subroutine write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes, n_cells,  &
                                                 n_node_vars, n_cell_vars,        &
                                                 zone_name, var_name,           &
                                                 data_format,                   &
                                                 strand_id, solution_time )
  subroutine write_tecplot_var_fmt( var_name, fmt )
    character(*), dimension(:),  intent(in)  :: var_name
    character(*),                intent(out) :: fmt
    integer               :: n_vars, i
    n_vars = size(var_name)
    write(fmt,'((A),I0,(A))') "('VARIABLES = ',", n_vars-1,                  &
                  "('""',(A),'""',', '),'""',(A),'""')"
    write(fmt,trim(fmt)) (trim(var_name(i)),i=1,n_vars)
  end subroutine write_tecplot_var_fmt

  subroutine write_tecplot_loc_fmt( n_node_vars, n_cell_vars, fmt )
    integer,      intent(in)  :: n_node_vars, n_cell_vars
    character(*), intent(out) :: fmt  
    integer                   :: loc_ind, i
    integer, dimension(4)     :: loc_range
    character(100)            :: loc_cell, loc_nodal
    loc_ind   = 1
    loc_range = 1
    if ( n_node_vars > 0 ) then
      write(loc_nodal,'(A)') "('[',I0,'-',I0,']=NODAL'"
      if (n_cell_vars>0) then; loc_nodal = trim(loc_nodal)//",',')"
      else;                    loc_nodal = trim(loc_nodal)//")"
      end if
      loc_range(2) = n_node_vars
      loc_ind = 2
    else
      write(loc_nodal,'(A)') ''
    end if

    if ( n_cell_vars > 0 ) then
      write(loc_cell,'(A)') "'[',I0,'-',I0,']=CELLCENTERED')"
      if (n_node_vars>0) then; loc_cell = ",("//trim(loc_cell)
      else;                    loc_cell = "("//trim(loc_cell)
      end if
      loc_range(3) = n_node_vars + 1
      loc_range(4) = n_node_vars + n_cell_vars
      loc_ind = 4
    else
      write(loc_cell,'(A)') ''
    end if
    write(fmt,'(A)') "('VARLOCATION=(',"//trim(loc_nodal)//                  &
                                          trim(loc_cell)//"')')"
    write(fmt,trim(fmt)) (loc_range(i),i=1,loc_ind)
  end subroutine write_tecplot_loc_fmt

  subroutine write_tecplot_data_type_fmt( n_vars, fmt, data_format )
    integer,                              intent(in)  :: n_vars
    character(*),                         intent(out) :: fmt
    integer, dimension(n_vars), optional, intent(in)  :: data_format
    character(*), parameter :: routine_name = 'write_tecplot_data_type_fmt'
    integer :: i
    logical :: err
    write(fmt,'((A),I0,(A))') "('DT=(',(A),", n_vars-1,"(',',(A))')')"
    if ( present(data_format) ) then
      if ( any(data_format<1).or.any(data_format>6) ) then
        err = error_message(routine_name,'data_format must be in range [1,6]')
      end if
      write(fmt,trim(fmt)) ( trim( data_types( data_format(i) ) ),i=1,n_vars)
    else
      write(fmt,trim(fmt)) (trim(data_types(1)),i=1,n_vars)
    end if
  end subroutine write_tecplot_data_type_fmt

  subroutine write_tecplot_var_share_fmt( var_info, fmt )
  ! assume donor zone is first integer in var_info
    integer, dimension(:),       intent(in)  :: var_info
    character(*),                intent(out) :: fmt
    character(*), parameter :: routine_name = 'write_tecplot_var_share_fmt'
    integer :: n_vars, zone_num, i
    logical :: err
    n_vars = size(var_info) - 1
    if (n_vars < 1) then
      err = error_message(routine_name,'No variables were specified to be shared')
    end if
    zone_num = var_info(1)
    write(fmt,'((A),I0,(A),I0,(A))') "('VARSHARELIST=([',I0,", n_vars-1,       &
                                                 "(',',I0),']=", zone_num, ")')"
    write(fmt,trim(fmt)) ( var_info(i+1),i=1,n_vars)
  end subroutine write_tecplot_var_share_fmt

  !======================== write_tecplot_file_header ==========================80
  !>
  !! Generic routine for writing header info in Tecplot ASCII file
  !<
  !=============================================================================80
  subroutine write_tecplot_file_header( fid, var_names, title, filetype )

    integer,                            intent(in) :: fid
    character(*), dimension(:),         intent(in) :: var_names
    character(*), optional,             intent(in) :: title
    integer,      optional,             intent(in) :: filetype ! 0, 1, or 2

    integer               :: n_vars
    logical               :: err
    character(1024)       :: var_fmt, title_fmt

    character(*), parameter :: routine_name = 'write_tecplot_file_header'

    call write_tecplot_var_fmt(var_names,var_fmt)

    write( fid, * )
    if ( present(title) ) then
      write(title_fmt,"('TITLE=','""',(A),'""')") trim(title)
      write( fid, '(A)' ) trim( title_fmt )
    end if
    if ( present(filetype) ) then
      select case(filetype)
      case(0)
        write( fid, '(A)' ) 'FILETYPE=FULL'
      case(1)
        write( fid, '(A)' ) 'FILETYPE=GRID'
      case(2)
        write( fid, '(A)' ) 'FILETYPE=SOLUTION'
      case default
        err = error_message(routine_name, 'unrecognized filetype! must be '//  &
                                '0 ("FULL"), 1 ("GRID"), or 2 ("SOLUTION")' )
      end select
    end if
    write( fid, '(A)' ) trim( var_fmt )

  end subroutine write_tecplot_file_header

!===================== write_tecplot_ordered_zone_header =====================80
!>
!! Generic routine for writing ordered zone header info in Tecplot ASCII file
!<
!=============================================================================80
  subroutine write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,           &
                                                n_node_vars,                   &
                                                n_cell_vars,                   &
                                                zone_name,                     &
                                                var_names,                     &
                                                var_share,                     &
                                                data_packing,                  &
                                                data_format,                   &
                                                strand_id,                     &
                                                solution_time )!,              &
                                                ! aux_data )
    use set_constants, only : max_text_line_length
    integer,                                intent(in) :: fid, n_dim
    integer, dimension(:),                  intent(in) :: n_nodes
    integer,                                intent(in) :: n_node_vars
    integer,                                intent(in) :: n_cell_vars
    character(*),                 optional, intent(in) :: zone_name
    character(*), dimension(:),   optional, intent(in) :: var_names
    integer, dimension(:),        optional, intent(in) :: var_share
    integer,                      optional, intent(in) :: data_packing
    integer, dimension(:),        optional, intent(in) :: data_format
    integer,                      optional, intent(in) :: strand_id
    real(dp),                     optional, intent(in) :: solution_time
    ! character(*), dimension(:,:), optional, intent(in) :: aux_data
    integer :: i, n_vars
    logical :: err
    character(max_text_line_length) :: loc_fmt, type_fmt
    character(max_text_line_length) :: var_fmt, var_share_fmt
    character(max_text_line_length) :: zone_fmt1, zone_fmt2
    character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_header'
    character(*), dimension(3), parameter :: IJK = ['I','J','K']

    if ( n_dim < 1 .or. n_dim > 3 ) then
      err = error_message( routine_name, 'Tecplot ordered file-format '//      &
                                         'supports only 1-3 dim.' )
    end if

    write( fid, *     )
    if ( present(zone_name) ) then
      write(zone_fmt1,"('ZONE T=','""',(A),'""')") trim(zone_name)
    else
      write(zone_fmt1,"(A)") 'ZONE'
    end if

    write( fid, '(A)' ) 'ZONETYPE=ORDERED'

    do i = 1,n_dim
      write( fid, "((A),'=',I0)") IJK(i), n_nodes(i)
    end do

    if ( present(data_packing) ) then
      select case(data_packing)
      case(0)
        if (n_cell_vars > 0) then
          err = error_message(routine_name, 'data_packing must be "BLOCK" '//  &
                                            'for cell-centered data' )
        end if
        write( fid, '(A)' ) 'DATAPACKING=POINT'
      case(1)
        write( fid, '(A)' ) 'DATAPACKING=BLOCK'
      case default
        err = error_message(routine_name, 'unrecognized data_packing! must '// &
                                          'be 0 ("POINT"), 1 ("BLOCK")' )
      end select
    end if

    call write_tecplot_loc_fmt( n_node_vars, n_cell_vars, loc_fmt )
    write( fid, '(A)' ) trim( loc_fmt   )

    n_vars = n_node_vars + n_cell_vars

    if ( present(data_format) )  then
      if (size(data_format)/=n_vars) then
        err = error_message(routine_name,"Size of optional argument "//        &
                                        "'data_format' does not match "//      &
                                        "number of variables.")
      end if
      call write_tecplot_data_type_fmt( n_vars, type_fmt,                      &
                                        data_format=data_format )
    else
      call write_tecplot_data_type_fmt( n_vars, type_fmt )
    end if
    write( fid, '(A)' ) trim( type_fmt )

    if ( present(var_share) )  then
      call write_tecplot_var_share_fmt( var_share, var_share_fmt )
      write( *, '(A)' ) trim( var_share_fmt )
      write( fid, '(A)' ) trim( var_share_fmt )
    end if

    write( fid, *     )
    write( fid, '(A)' ) trim( var_fmt   )
    write( fid, '(A)' ) trim( zone_fmt1 )
    write( fid, '(A)' ) trim( zone_fmt2 )
    write( fid, '(A)' ) trim( loc_fmt   )
    write( fid, '(A)' ) trim( type_fmt  )

    if ( present(strand_id) )  then
      write( fid, '((A),I0)' ) 'STRANDID=',strand_id
    end if

    if ( present(solution_time) )  then
      write( fid, '((A),ES23.15)' ) 'SOLUTIONTIME=',solution_time
    end if

  end subroutine write_tecplot_ordered_zone_header

  subroutine write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes,          &
                                                 n_cells,                      &
                                                 n_node_vars,                  &
                                                 n_cell_vars,                  &
                                                 zone_name,                    &
                                                 var_names,                    &
                                                 data_format,                  &
                                                 strand_id,                    &
                                                 solution_time )
    use set_constants, only : max_text_line_length
    integer,                              intent(in) :: fid, n_dim
    integer,                              intent(in) :: n_nodes, n_cells
    integer,                              intent(in) :: n_node_vars, n_cell_vars
    character(*),                         intent(in) :: zone_name
    character(*), dimension(:), optional, intent(in) :: var_names
    integer,      dimension(:), optional, intent(in) :: data_format
    integer,                    optional, intent(in) :: strand_id
    real(dp),                   optional, intent(in) :: solution_time
    integer               :: n_vars
    logical               :: err
    character(max_text_line_length) :: var_fmt, loc_fmt, type_fmt
    character(max_text_line_length) :: zone_fmt1, zone_fmt2
    character(*), parameter :: routine_name = 'write_tecplot_fe_brick_zone_header'

    if ( present(zone_name) ) then
      write(zone_fmt1,"('ZONE T=','""',(A),'""')") trim(zone_name)
    else
      write(zone_fmt1,"(A)") 'ZONE'
    end if

    if ( present(var_names) ) then
      call write_tecplot_var_fmt(var_names,var_fmt)
    end if

    select case(n_dim)
    case(2)
      write(zone_fmt2,'((A),(A))') "('NODES=',I0,', ELEMENTS=',I0,",           &
                                   "', DATAPACKING=BLOCK, "//                  &
                                   "ZONETYPE=FEQUADRILATERAL')"
    case(3)
      write(zone_fmt2,'((A),(A))') "('NODES=',I0,', ELEMENTS=',I0,",           &
                                   "', DATAPACKING=BLOCK, ZONETYPE=FEBRICK')"
    case default
      err = error_message(routine_name,"Only n_dim=2 or 3 supported")
    end select

    write(zone_fmt2,trim(zone_fmt2)) n_nodes, n_cells
    call write_tecplot_loc_fmt( n_node_vars, n_cell_vars, loc_fmt )

    n_vars = n_node_vars + n_cell_vars
    if ( present(data_format) ) then
      if (size(data_format)/=n_vars) then
        err = error_message(routine_name,"Size of optional argument "//        &
                                         "'data_format' does not match "//     &
                                         "number of variables.")
      end if
      call write_tecplot_data_type_fmt( n_vars, type_fmt,                      &
                                        data_format=data_format )
    else
      call write_tecplot_data_type_fmt( n_vars, type_fmt )
    end if

    write( fid, *     )
    if ( present(var_names) ) write( fid, '(A)' ) trim( var_fmt )
    write( fid, '(A)' ) trim( zone_fmt1 )
    write( fid, '(A)' ) trim( zone_fmt2 )
    write( fid, '(A)' ) trim( loc_fmt   )
    write( fid, '(A)' ) trim( type_fmt  )

    if ( present(strand_id) )  then
      write( fid, '((A),I0)' ) 'STRANDID=',strand_id
    end if

    if ( present(solution_time) )  then
      write( fid, '((A),ES23.15)' ) 'SOLUTIONTIME=',solution_time
    end if

  end subroutine write_tecplot_fe_brick_zone_header

  subroutine write_tecplot_ordered_zone_block_packed( fid, fmt, n_nodes,       &
                                                      n_node_vars, n_cell_vars,&
                                                      NODE_DATA, CELL_DATA )
    integer,                  intent(in) :: fid
    character(*),             intent(in) :: fmt
    integer, dimension(:),    intent(in) :: n_nodes
    integer,                  intent(in) :: n_node_vars, n_cell_vars
    real(dp), dimension(:,:), intent(in) :: NODE_DATA
    real(dp), dimension(:,:), intent(in) :: CELL_DATA
    integer :: v, i, n_n, n_c
    character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_block_packed'
    n_n = product(n_nodes)
    n_c = product(max(n_nodes-1,1))
    write(fid,fmt) ( ( NODE_DATA(v,i), i = 1,n_n ), v = 1,n_node_vars )
    write(fid,fmt) ( ( CELL_DATA(v,i), i = 1,n_c ), v = 1,n_cell_vars )

  end subroutine write_tecplot_ordered_zone_block_packed

  subroutine write_tecplot_ordered_zone_point_packed( fid, fmt, n_nodes,       &
                                                      n_vars, NODE_DATA )
    integer,                  intent(in) :: fid
    character(*),             intent(in) :: fmt
    integer, dimension(:),    intent(in) :: n_nodes
    integer,                  intent(in) :: n_vars
    real(dp), dimension(:,:), intent(in) :: NODE_DATA
    integer :: v, i, n_n
    character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_point_packed'
    n_n = product(n_nodes)
    do i = 1,n_n
      write(fid,'('//fmt//'," ")',advance='no') ( NODE_DATA(v,i), v = 1,n_vars )
    end do
  end subroutine write_tecplot_ordered_zone_point_packed

  subroutine write_tecplot_fe_brick_zone( fid, n_nodes, n_cells, n_node_var,   &
                                          n_cell_var, NODE_DATA, CELL_DATA,    &
                                          conn_idx, data_format )
    integer,                  intent(in) :: fid
    integer,                  intent(in) :: n_nodes,    n_cells
    integer,                  intent(in) :: n_node_var, n_cell_var
    real(dp), dimension(:,:), intent(in) :: NODE_DATA
    real(dp), dimension(:,:), intent(in) :: CELL_DATA
    integer,  dimension(:,:), intent(in) :: conn_idx
    integer,  dimension(:), optional, intent(in) :: data_format
    integer :: v, t, n_brick, cnt
    logical :: err
    character(len=100) :: conn_fmt
    n_brick = size(conn_idx,1)
    if ( present(data_format) ) then
      cnt = 0
      do v = 1,n_node_var
        cnt = cnt + 1
        write(fid,trim(formats(data_format(cnt)))) ( NODE_DATA(v,t), t=1,n_nodes )
      end do
      do v = 1,n_cell_var
        cnt = cnt + 1
        select case(data_format(cnt))
        case(1,2)
          write(fid,trim(formats(data_format(cnt)))) ( CELL_DATA(v,t), t=1,n_cells )
        case default
          write(fid,trim(formats(data_format(cnt)))) ( int(CELL_DATA(v,t)), t=1,n_cells )
        end select
      end do
    else
      write(fid,formats(1)) ( ( NODE_DATA(v,t), t=1,n_nodes ), v=1,n_node_var )
      write(fid,formats(1)) ( ( CELL_DATA(v,t), t=1,n_cells ), v=1,n_cell_var )
    end if

    write(conn_fmt,'((A),I0,(A))') "((I0),", n_brick-1,"(' ',(I0)))"
    do t = 1,merge(n_cells,n_nodes,n_cell_var>0)
      write(fid,trim(conn_fmt)) ( conn_idx(v,t), v=1,n_brick )
    end do

  end subroutine write_tecplot_fe_brick_zone
end module tecplot_output

module interpolant_derived_type
  use set_precision, only : dp
  use set_constants, only : zero, one, two, half
  implicit none
  private
  public :: interpolant_t

  type :: interpolant_t
    integer :: Nmax
    real(dp), dimension(:,:),     allocatable :: xb, wb
    real(dp), dimension(:,:,:,:), allocatable :: Dmat
  contains
    private
    procedure, public, nopass :: constructor
    procedure, public, pass   :: destroy       => destroy_interpolant
    procedure,         pass   :: lagbary, lagbary_wderiv, lagbary_wderiv2
    procedure,         pass   :: lagbary_2D, lagbary_2D_wgrad, lagbary_2D_whess
    procedure,         pass   :: lagbary_3D, lagbary_3D_wgrad, lagbary_3D_whess
    procedure, public, pass   :: calc_grid_metrics, calc_grid_metrics_alt
    procedure, public, pass   :: normal_vectors
    ! procedure, public, pass   :: map_point_3D
    procedure, public, pass   :: map_point_3D_curve, map_point_3D_surface, map_point_3D_volume
  end type interpolant_t

  interface interpolant_t
    procedure constructor
  end interface interpolant_t

contains

  pure elemental subroutine destroy_interpolant(this)
    class(interpolant_t), intent(inout) :: this
    if ( allocated(this%Dmat) ) deallocate(this%Dmat)
    if ( allocated(this%xb) )   deallocate(this%xb)
    if ( allocated(this%wb) )   deallocate(this%wb)
    this%Nmax = 0
  end subroutine destroy_interpolant

  pure elemental function constructor(N) result(this)
    use linspace_helper, only : linspace
    use set_constants,   only : zero, one
    integer, optional, intent(in) :: N
    type(interpolant_t)           :: this
    integer :: j
    call this%destroy()
    if ( present(N) ) this%Nmax = max(N,2)
    allocate( this%Dmat(this%Nmax,this%Nmax,this%Nmax,2) )
    allocate(   this%xb(this%Nmax,this%Nmax), this%wb(this%Nmax,this%Nmax) )
    this%Dmat = zero; this%xb = zero; this%wb = zero
    this%wb(1,1) = one
    do j = 2,this%Nmax
      this%xb(1:j,j) = linspace(j,-one,one)
      this%wb(1:j,j) = barycentric_weights( this%xb(1:j,j) )
      this%Dmat(1:j,1:j,j,:) = mth_order_polynomial_derivative_matrix( this%xb(1:j,j), this%wb(1:j,j), 2 )
    end do
  end function constructor

  pure elemental logical function almost_equal(a,b)
    real(dp), intent(in) :: a, b
    logical :: test1, test2, test3
    test1 = ( (a==zero) .or. (b==zero) )
    test2 = ( abs(a-b) <= two*epsilon(one) )
    test3 = ( ( abs(a-b) <= epsilon(abs(a)) ) .and. &
              ( abs(a-b) <= epsilon(abs(b)) ) )
    almost_equal = ( ( test1 .and. test2 ) .or. ( (.not. test1) .and. test3 ) )
  end function almost_equal

  pure function barycentric_weights(x) result(w)
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(size(x))       :: w
    integer :: j, k, N
    N = size(x)
    w = one
    do j = 2,N
      do k = 1,j-1
        w(k) = w(k) * ( x(k) - x(j) )
        w(j) = w(j) * ( x(j) - x(k) )
      end do
    end do
    w = one/w
  end function barycentric_weights

  pure function polynomial_derivative_matrix(x,w) result(D)
    real(dp), dimension(:), intent(in) :: x, w
    real(dp), dimension(size(x),size(x)) :: D
    integer :: i, j, N
    D = zero
    N = size(x)
    do i = 1,N
      do j = 1,N
        if (j/=i) then
          D(i,j) = w(j)/w(i) * one / ( x(i) - x(j) )
          D(i,i) = D(i,i) - D(i,j)
        end if
      end do
    end do
  end function polynomial_derivative_matrix

  pure function mth_order_polynomial_derivative_matrix(x,w,M) result(D)
    real(dp), dimension(:), intent(in) :: x, w
    integer,                intent(in) :: M
    real(dp), dimension(size(x),size(x),M) :: D
    integer :: i, j, k, N
    D = zero
    N = size(x)
    D(:,:,1) = polynomial_derivative_matrix(x,w)
    do k = 2,M
      do i = 1,N
        D(i,i,k) = zero
        do j = 1,N
          if (j/=i) then
            D(i,j,k) = ( real(k,dp) / (x(i) - x(j)) )        &
                     * ( w(j)/w(i)*D(i,i,k-1) - D(i,j,k-1) )
            D(i,i,k) = D(i,i,k) - D(i,j,k)
          end if
        end do
      end do
    end do
  end function mth_order_polynomial_derivative_matrix

  pure subroutine lagbary(this,x,dir,fval,Npts,val)
    class(interpolant_t),    intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val
    real(dp) :: A, F
    real(dp) :: x1, t1
    integer :: j, N
    A = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val = fval(j)
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
    end do
    val = A/F
  end subroutine lagbary

  pure subroutine lagbary_wderiv(this,x,dir,fval,Npts,val,dval)
    class(interpolant_t),    intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val, dval
    real(dp) :: A, B, C, F
    real(dp) :: x1, t1, t2, FF, AC
    integer :: j, N
    A = zero; B = zero; C = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val = fval(j)
        dval = dot_product( this%Dmat(j,1:N,N,1), fval )
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
      t2 = t1/x1
      B = B + t2 * fval(j)
      C = C + t2
    end do
    val = A/F
    FF = F*F
    AC = A*C
    dval = (B * F - AC)/FF
  end subroutine lagbary_wderiv

  pure subroutine lagbary_wderiv2(this,x,dir,fval,Npts,val,dval,d2val)
    class(interpolant_t),    intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val, dval, d2val
    real(dp) :: A, B, C, D, E, F
    real(dp) :: x1, t1, t2, t3, FF, AC
    integer :: j, N
    A = zero; B = zero; C = zero; D = zero; E = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val   = fval(j)
        dval  = dot_product( this%Dmat(j,1:N,N,1), fval )
        d2val = dot_product( this%Dmat(j,1:N,N,2), fval )
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
      t2 = t1/x1
      B = B + t2 * fval(j)
      C = C + t2
      t3 = t2/x1
      D = D + t3 * fval(j)
      E = E + t3
    end do
    val = A/F
    FF = F*F
    AC = A*C
    dval = (B * F - AC)/FF
    d2val = ( two * D      ) / F          &
          - ( two * E * A  ) / FF         &
          - ( two * B * C  ) / FF         &
          + ( two * C * AC ) / ( FF * F )
  end subroutine lagbary_wderiv2

  pure subroutine lagbary_2D(this,x,fval,Npts,val)
    class(interpolant_t),      intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(size(fval,2)) :: tmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary( x(1), 1, fval(:,j), Npts, tmp(j) )
    end do
    call this%lagbary( x(2), 2, tmp, Npts, val )
  end subroutine lagbary_2D

  pure subroutine lagbary_2D_wgrad(this,x,fval,Npts,val,grad)
    class(interpolant_t),      intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(2),   intent(out) :: grad
    real(dp), dimension(size(fval,2)) :: tmp, gtmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary_wderiv( x(1), 1, fval(:,j), Npts, tmp(j), gtmp(j) )
    end do
    call this%lagbary_wderiv( x(2), 2,  tmp, Npts, val, grad(2) )
    call this%lagbary(        x(2), 2, gtmp, Npts,      grad(1) )
  end subroutine lagbary_2D_wgrad

  pure subroutine lagbary_2D_whess(this,x,fval,Npts,val,grad,hess)
    class(interpolant_t),      intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(2),   intent(out) :: grad
    real(dp), dimension(3),   intent(out) :: hess
    real(dp), dimension(size(fval,2)) :: tmp, gtmp, htmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary_wderiv2( x(1), 1, fval(:,j), Npts, tmp(j), gtmp(j), htmp(j) )
    end do
    call this%lagbary_wderiv2( x(2), 2,  tmp, Npts, val, grad(2), hess(3) )
    call this%lagbary_wderiv(  x(2), 2, gtmp, Npts,      grad(1), hess(2) )
    call this%lagbary(         x(2), 2, htmp, Npts,               hess(1) )
  end subroutine lagbary_2D_whess

  pure subroutine lagbary_3D(this,x,fval,Npts,val)
    class(interpolant_t),        intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp
    real(dp), dimension(size(fval,3)) :: tmp2
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary( x(1), 1, fval(:,j,k), Npts, tmp(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary( x(2), 2, tmp(:,k), Npts, tmp2(k) )
    end do
    call this%lagbary( x(3), 3, tmp2, Npts, val )
  end subroutine lagbary_3D

  pure subroutine lagbary_3D_wgrad(this,x,fval,Npts,val,grad)
    class(interpolant_t),        intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(3),     intent(out) :: grad
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp, gtmp0
    real(dp), dimension(size(fval,3)) :: tmp2, gtmp1, gtmp2
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary_wderiv( x(1), 1, fval(:,j,k), Npts, tmp(j,k), gtmp0(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary_wderiv( x(2), 2,   tmp(:,k), Npts, tmp2(k), gtmp2(k) )
      call this%lagbary(        x(2), 2, gtmp0(:,k), Npts, gtmp1(k) )
    end do
    call this%lagbary_wderiv( x(3), 3,  tmp2, Npts, val, grad(3) )
    call this%lagbary(        x(3), 3, gtmp2, Npts,      grad(2) )
    call this%lagbary(        x(3), 3, gtmp1, Npts,      grad(1) )
  end subroutine lagbary_3D_wgrad

  pure subroutine lagbary_3D_whess(this,x,fval,Npts,val,grad,hess)
    class(interpolant_t),        intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(3),     intent(out) :: grad
    real(dp), dimension(6),     intent(out) :: hess
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp, gtmp, htmp
    real(dp), dimension(size(fval,3)) :: tmp1, gtmp1, gtmp2, htmp1, htmp2, htmp3
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary_wderiv2( x(1), 1, fval(:,j,k), Npts, tmp(j,k), gtmp(j,k), htmp(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary_wderiv2( x(2), 2,  tmp(:,k), Npts, tmp1(k), gtmp2(k), htmp3(k) )
      call this%lagbary_wderiv(  x(2), 2, gtmp(:,k), Npts,          gtmp1(k), htmp2(k) )
      call this%lagbary(         x(2), 2, htmp(:,k), Npts,                    htmp1(k) )
    end do
    call this%lagbary_wderiv2( x(3), 3,  tmp1, Npts, val, grad(3), hess(6) )
    call this%lagbary_wderiv(  x(3), 3, gtmp2, Npts,      grad(2), hess(5) )
    call this%lagbary(         x(3), 3, htmp3, Npts,               hess(4) )
    call this%lagbary_wderiv(  x(3), 3, gtmp1, Npts,      grad(1), hess(3) )
    call this%lagbary(         x(3), 3, htmp2, Npts,               hess(2) )
    call this%lagbary(         x(3), 3, htmp1, Npts,               hess(1) )
  end subroutine lagbary_3D_whess

  pure function calc_grid_metrics(this,point,X1,X2,X3) result(Ja)
    use set_constants, only : zero
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Ja
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3),3) :: X
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3))   :: tmp
    real(dp), dimension(3) :: dX_l, dX_m, dd1, dd2, dd3
    real(dp) :: junk
    integer, dimension(3), parameter :: ijk = [1,2,3]
    integer, dimension(3), parameter :: kij = cshift(ijk,1)
    integer, dimension(3), parameter :: jki = cshift(kij,1)
    integer, dimension(3) :: Npts
    integer :: i
    Ja = zero
    Npts = shape(X1)
    call this%lagbary_3D_wgrad( point, X3, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X2, Npts, junk, dX_m )
    tmp = X3*dX_m(1) - X2*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X3*dX_m(2) - X2*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X3*dX_m(3) - X2*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(1,1) = -half*( dd3(2) - dd2(3) );
    Ja(1,2) = -half*( dd1(3) - dd3(1) );
    Ja(1,3) = -half*( dd2(1) - dd1(2) );

    call this%lagbary_3D_wgrad( point, X1, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X3, Npts, junk, dX_m )
    tmp = X1*dX_m(1) - X3*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X1*dX_m(2) - X3*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X1*dX_m(3) - X3*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(2,1) = -half*( dd3(2) - dd2(3) );
    Ja(2,2) = -half*( dd1(3) - dd3(1) );
    Ja(2,3) = -half*( dd2(1) - dd1(2) );

    call this%lagbary_3D_wgrad( point, X2, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X1, Npts, junk, dX_m )
    tmp = X2*dX_m(1) - X1*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X2*dX_m(2) - X1*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X2*dX_m(3) - X1*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(3,1) = -half*( dd3(2) - dd2(3) );
    Ja(3,2) = -half*( dd1(3) - dd3(1) );
    Ja(3,3) = -half*( dd2(1) - dd1(2) );
  end function calc_grid_metrics

  pure function calc_grid_metrics_alt(this,point,X1,X2,X3) result(Ja)
    use set_constants, only : zero
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Ja
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3),3) :: X
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3))   :: tmp
    real(dp), dimension(3) :: dX_l, dX_m, dd1, dd2, dd3
    real(dp) :: junk
    integer, dimension(3), parameter :: ijk = [1,2,3]
    integer, dimension(3), parameter :: kij = cshift(ijk,1)
    integer, dimension(3), parameter :: jki = cshift(kij,1)
    integer, dimension(3) :: Npts
    integer :: i
    Ja = zero
    X(:,:,:,1) = X1
    X(:,:,:,2) = X2
    X(:,:,:,3) = X3
    Npts = shape(X1)
    do i = 1,3
      associate( l => kij(i), m => jki(i) )
        associate( X_l => X(:,:,:,l), X_m => X(:,:,:,m) )
          call this%lagbary_3D_wgrad( point, X_l, Npts, junk, dX_l )
          call this%lagbary_3D_wgrad( point, X_m, Npts, junk, dX_m )
          tmp = X_l*dX_m(1) - X_m*dX_l(1)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
          tmp = X_l*dX_m(2) - X_m*dX_l(2)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
          tmp = X_l*dX_m(3) - X_m*dX_l(3)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
          Ja(i,1) = -half*( dd3(2) - dd2(3) );
          Ja(i,2) = -half*( dd1(3) - dd3(1) );
          Ja(i,3) = -half*( dd2(1) - dd1(2) );
        end associate
      end associate
    end do
  end function calc_grid_metrics_alt

  pure function normal_vectors(this,point,X1,X2,X3) result(Nvec)
    use math, only : cross_product
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Nvec
    Nvec = this%calc_grid_metrics(point,X1,X2,X3)
    Nvec(:,1) = Nvec(:,1)/norm2(Nvec(:,1))
    Nvec(:,2) = Nvec(:,2)/norm2(Nvec(:,2))
    Nvec(:,3) = Nvec(:,3)/norm2(Nvec(:,3))
  end function normal_vectors

  pure subroutine map_point_3D_curve(this,point,X1,X2,X3,xyz,dS)
    class(interpolant_t),   intent(in)  :: this
    real(dp), dimension(1), intent(in)  :: point ! [t]
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3), intent(out) :: xyz
    real(dp),               intent(out) :: dS
    integer,  dimension(1) :: Npts
    real(dp), dimension(3) :: dval
    Npts = shape(X1)
    call this%lagbary_wderiv(point(1),1,X1,Npts,xyz(1),dval(1))
    call this%lagbary_wderiv(point(1),1,X2,Npts,xyz(2),dval(2))
    call this%lagbary_wderiv(point(1),1,X3,Npts,xyz(3),dval(3))
    dS = norm2(dval)
  end subroutine map_point_3D_curve

  pure subroutine map_point_3D_surface(this,point,X1,X2,X3,xyz,dS)
    use math, only : cross_product
    class(interpolant_t),     intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: point ! [u,v]
    real(dp), dimension(:,:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3),   intent(out) :: xyz
    real(dp),                 intent(out) :: dS
    integer,  dimension(2) :: Npts
    real(dp), dimension(2) :: tmp
    real(dp), dimension(3) :: drdu, drdv
    Npts = shape(X1)
    call this%lagbary_2D_wgrad(point,X1,Npts,xyz(1),tmp)
    drdu(1) = tmp(1); drdv(1) = tmp(2)
    call this%lagbary_2D_wgrad(point,X2,Npts,xyz(2),tmp)
    drdu(2) = tmp(1); drdv(2) = tmp(2)
    call this%lagbary_2D_wgrad(point,X3,Npts,xyz(3),tmp)
    drdu(3) = tmp(1); drdv(3) = tmp(2)
    dS = norm2( cross_product(drdu,drdv) )
  end subroutine map_point_3D_surface

  pure subroutine map_point_3D_volume(this,point,X1,X2,X3,xyz,dS)
    use math, only : det_3x3
    class(interpolant_t),       intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: point ! [xi,eta,zeta]
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3),     intent(out) :: xyz
    real(dp),                   intent(out) :: dS
    integer, dimension(3) :: Npts
    real(dp), dimension(3,3) :: A
    Npts = shape(X1)
    call this%lagbary_3D_wgrad(point,X1,Npts,xyz(1),A(:,1))
    call this%lagbary_3D_wgrad(point,X2,Npts,xyz(2),A(:,2))
    call this%lagbary_3D_wgrad(point,X3,Npts,xyz(3),A(:,3))
    dS = det_3x3(A)
  end subroutine map_point_3D_volume


end module interpolant_derived_type

module quadrature_derived_type

  use set_precision,       only : dp
  use set_constants,       only : zero
  implicit none
  private
  public :: quad_t
  public :: quad_ptr, quad_ptr_3D
  public :: create_quad_ref_1D, create_quad_ref_2D, create_quad_ref_3D
  public :: map_quad_ref_to_physical
  ! public :: map_quad_ref_to_physical_1D
  ! public :: map_quad_ref_to_physical_2D
  ! public :: map_quad_ref_to_physical_3D
  type quad_t
    integer :: n_quad = 0
    real(dp), allocatable, dimension(:,:) :: quad_pts
    real(dp), allocatable, dimension(:)   :: quad_wts
  contains
    private
    procedure, public, pass :: create  => allocate_quad
    procedure, public, pass :: destroy => deallocate_quad
    generic,   public :: integrate => integrate_scalar, integrate_vector
    procedure :: integrate_scalar
    procedure :: integrate_vector
  end type quad_t

  type quad_ptr
    type(quad_t), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_quad_ptr
  end type quad_ptr

  type quad_ptr_3D
    type(quad_t), dimension(:,:,:), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_quad_ptr_3D
  end type quad_ptr_3D

contains

  pure elemental subroutine destroy_quad_ptr_3D( this )
    class(quad_ptr_3D), intent(inout) :: this
    this%p => null()
  end subroutine destroy_quad_ptr_3D

  pure elemental subroutine destroy_quad_ptr( this )
    class(quad_ptr), intent(inout) :: this
    this%p => null()
  end subroutine destroy_quad_ptr

  pure elemental subroutine allocate_quad( this, n_quad )
    use set_constants, only : zero
    class(quad_t), intent(inout) :: this
    integer,       intent(in)    :: n_quad
    this%n_quad = n_quad
    allocate( this%quad_pts(3,n_quad) )
    this%quad_pts = zero
    allocate( this%quad_wts(n_quad) )
    this%quad_wts = zero
  end subroutine allocate_quad

  pure elemental subroutine deallocate_quad( this )
    class(quad_t), intent(inout) :: this
    this%n_quad = 0
    if( allocated( this%quad_wts  ) ) deallocate( this%quad_wts  )
    if( allocated( this%quad_pts  ) ) deallocate( this%quad_pts  )
  end subroutine deallocate_quad

  pure function integrate_scalar( this, f ) result( integral )
    use set_precision, only : dp
    class(quad_t),                    intent(in) :: this
    real(dp), dimension(this%n_quad), intent(in) :: f
    real(dp)                                     :: integral
    integral = dot_product(f,this%quad_wts)
  end function integrate_scalar

  pure function integrate_vector( this, neq, f ) result( integral )
    use set_precision, only : dp
    class(quad_t),                        intent(in) :: this
    integer,                              intent(in) :: neq
    real(dp), dimension(neq,this%n_quad), intent(in) :: f
    real(dp), dimension(neq)                         :: integral
    integer :: n
    do n = 1, neq
      integral(n) = dot_product(f(n,:),this%quad_wts)
    end do
  end function integrate_vector

  pure function gauss_1D_size( polynomial_order ) result( N_quad )
    use set_constants, only : half
    integer, intent(in) :: polynomial_order
    integer             :: N_quad
    N_quad = ceiling( half*(polynomial_order + 1) )
  end function gauss_1D_size

  pure subroutine gauss_1D( n_quad, pts_1D, wts_1D )
    use math, only : LegendreGaussNodesAndWeights
    integer,                       intent(in)  :: n_quad
    real(dp), dimension( n_quad ), intent(out) :: pts_1D
    real(dp), dimension( n_quad ), intent(out) :: wts_1D
    call LegendreGaussNodesAndWeights(n_quad-1, pts_1D, wts_1D)
  end subroutine gauss_1D

  pure subroutine create_quad_ref_1D( quad_order, quad_ref )
    integer,      intent(in)  :: quad_order
    type(quad_t), intent(out) :: quad_ref
    real(dp), dimension( gauss_1D_size( quad_order ) ) :: xtmp
    integer :: n_quad
    n_quad = gauss_1D_size( quad_order )
    call quad_ref%destroy()
    call quad_ref%create( n_quad )
    call gauss_1D( n_quad, xtmp, quad_ref%quad_wts )
    quad_ref%quad_pts(1,:) = xtmp
  end subroutine create_quad_ref_1D

  pure subroutine create_quad_ref_2D( quad_order, quad_ref )
    use set_constants, only : zero
    integer,      intent(in)  :: quad_order
    type(quad_t), intent(out) :: quad_ref
    integer :: n_quad
    integer :: i, j, cnt
    real(dp), dimension( gauss_1D_size( quad_order ) ) :: pts_1D
    real(dp), dimension( gauss_1D_size( quad_order ) ) :: wts_1D
    n_quad = gauss_1D_size( quad_order )
    call gauss_1D(n_quad, pts_1D, wts_1D)
    call quad_ref%destroy()
    call quad_ref%create( n_quad**2 )
    cnt = 0
    do j = 1, n_quad
      do i = 1, n_quad
        cnt = cnt + 1
        quad_ref%quad_pts(:,cnt) = [ pts_1D(i), pts_1D(j), zero ]
        quad_ref%quad_wts(cnt) = wts_1D(i)*wts_1D(j)
      end do
    end do
  end subroutine create_quad_ref_2D

  pure subroutine create_quad_ref_3D( quad_order, quad_ref )
    integer,      intent(in)  :: quad_order
    type(quad_t), intent(out) :: quad_ref
    integer :: n_quad
    integer :: i, j, k, cnt
    real(dp), dimension( gauss_1D_size( quad_order ) ) :: pts_1D
    real(dp), dimension( gauss_1D_size( quad_order ) ) :: wts_1D
    n_quad = gauss_1D_size( quad_order )
    call gauss_1D(n_quad, pts_1D, wts_1D)
    call quad_ref%destroy()
    call quad_ref%create( n_quad**3 )
    cnt = 0
    do k = 1, n_quad
      do j = 1, n_quad
        do i = 1, n_quad
          cnt = cnt + 1
          quad_ref%quad_pts(:,cnt) = [ pts_1D(i), pts_1D(j), pts_1D(k) ]
          quad_ref%quad_wts(cnt) = wts_1D(i)*wts_1D(j)*wts_1D(k)
        end do
      end do
    end do
  end subroutine create_quad_ref_3D

  pure subroutine map_quad_ref_to_physical_1D( X1, X2, X3, interpolant, quad_ref, quad_physical )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    type(interpolant_t),    intent(in)  :: interpolant
    type(quad_t),           intent(in)  :: quad_ref
    type(quad_t),           intent(out) :: quad_physical
    real(dp) :: dS
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_curve( [quad_ref%quad_pts(1,n)], X1, X2, X3, quad_physical%quad_pts(:,n), dS )
      quad_physical%quad_wts(n) = dS * quad_ref%quad_wts(n)
    end do
  end subroutine map_quad_ref_to_physical_1D

  pure subroutine map_quad_ref_to_physical_2D( X1, X2, X3, interpolant, quad_ref, quad_physical )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:), intent(in)  :: X1, X2, X3
    type(interpolant_t),      intent(in)  :: interpolant
    type(quad_t),             intent(in)  :: quad_ref
    type(quad_t),             intent(out) :: quad_physical
    real(dp) :: dA
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_surface( quad_ref%quad_pts(1:2,n), X1, X2, X3, quad_physical%quad_pts(:,n), dA )
      quad_physical%quad_wts(n) = dA * quad_ref%quad_wts(n)
    end do
  end subroutine map_quad_ref_to_physical_2D

  pure subroutine map_quad_ref_to_physical_3D( X1, X2, X3, interpolant, quad_ref, quad_physical )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    type(interpolant_t),        intent(in)  :: interpolant
    type(quad_t),               intent(in)  :: quad_ref
    type(quad_t),               intent(out) :: quad_physical
    real(dp) :: dV
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_volume( quad_ref%quad_pts(1:3,n), X1, X2, X3, quad_physical%quad_pts(:,n), dV )
      quad_physical%quad_wts(n) = dV * quad_ref%quad_wts(n)
    end do
  end subroutine map_quad_ref_to_physical_3D



  pure subroutine map_quad_ref_to_physical( X1, X2, X3, loc, interpolant, quad_ref, quad_physical, status )
    use set_constants,                        only : zero
    use index_conversion,                     only : get_reshape_indices
    use reshape_array,                        only : extract_1D_slice_from_3D_array, extract_2D_slice_from_3D_array
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    integer,  dimension(3),     intent(in)  :: loc
    type(interpolant_t),        intent(in)  :: interpolant
    type(quad_t),               intent(in)  :: quad_ref
    type(quad_t),               intent(out) :: quad_physical
    integer, optional,          intent(out) :: status
    real(dp), allocatable, dimension(:,:) :: X1_tmp, X2_tmp, X3_tmp
    integer, dimension(3) :: idx_start, idx_end, sz_in, sz_out
    integer  :: sz_cnt
    if (present(status)) status = 1
    sz_in = shape(X1)
    call get_reshape_indices(sz_in, loc, sz_out, sz_cnt, idx_start, idx_end )
    select case(sz_cnt)
    case(1)
      allocate( X1_tmp(sz_out(1),1), X2_tmp(sz_out(1),1), X3_tmp(sz_out(1),1) )
      X1_tmp(:,1) = extract_1D_slice_from_3D_array(X1,idx_start,idx_end,sz_out(1))
      X2_tmp(:,1) = extract_1D_slice_from_3D_array(X2,idx_start,idx_end,sz_out(1))
      X3_tmp(:,1) = extract_1D_slice_from_3D_array(X3,idx_start,idx_end,sz_out(1))
      call map_quad_ref_to_physical_1D(X1_tmp(:,1),X2_tmp(:,1),X3_tmp(:,1),interpolant,quad_ref,quad_physical)
    case(2)
      allocate( X1_tmp(sz_out(1),sz_out(2)), X2_tmp(sz_out(1),sz_out(2)), X3_tmp(sz_out(1),sz_out(2)) )
      X1_tmp = extract_2D_slice_from_3D_array(X1,idx_start,idx_end,sz_out(1:2))
      X2_tmp = extract_2D_slice_from_3D_array(X2,idx_start,idx_end,sz_out(1:2))
      X3_tmp = extract_2D_slice_from_3D_array(X3,idx_start,idx_end,sz_out(1:2))
      call map_quad_ref_to_physical_2D(X1_tmp,X2_tmp,X3_tmp,interpolant,quad_ref,quad_physical)
    case(3)
      call map_quad_ref_to_physical_3D(X1,X2,X3,interpolant,quad_ref,quad_physical)
    case default
      if (present(status)) status = -1
    end select

    if ( allocated(X1_tmp) ) deallocate( X1_tmp )
    if ( allocated(X2_tmp) ) deallocate( X2_tmp )
    if ( allocated(X3_tmp) ) deallocate( X3_tmp )

  end subroutine map_quad_ref_to_physical

end module quadrature_derived_type
module grid_derived_type
  use set_precision,           only : dp
  use quadrature_derived_type, only : quad_t, quad_ptr_3D
  use vector_derived_type,     only : face_vec, face_vec_ptr_3D
  use pointers,                only : array_ptr_3D_real, array_ptr_4D_real
  use interpolant_derived_type, only : interpolant_t
  implicit none
  private
  public :: derived_grid_vars
  public :: grid_block
  public :: grid_type
  public :: deallocate_grid
  public :: allocate_grid_block, deallocate_grid_block
  public :: allocate_derived_grid, deallocate_derived_grid

  public :: pack_cell_node_coords
  public :: get_face_quad_ptrs
  public :: pack_quadrature_info, pack_quadrature_info_z_order

  type derived_grid_vars
    real(dp),       allocatable, dimension(:,:,:,:) :: cell_c
    real(dp),       allocatable, dimension(:,:,:)   :: volume
    real(dp),       allocatable, dimension(:,:,:)   :: xi_area
    real(dp),       allocatable, dimension(:,:,:)   :: eta_area
    real(dp),       allocatable, dimension(:,:,:)   :: zeta_area
    type(quad_t),   allocatable, dimension(:,:,:)   :: quad
    type(quad_t),   allocatable, dimension(:,:,:)   :: xi_face_quad
    type(quad_t),   allocatable, dimension(:,:,:)   :: eta_face_quad
    type(quad_t),   allocatable, dimension(:,:,:)   :: zeta_face_quad
    type(face_vec), allocatable, dimension(:,:,:)   :: xi_nv
    type(face_vec), allocatable, dimension(:,:,:)   :: eta_nv
    type(face_vec), allocatable, dimension(:,:,:)   :: zeta_nv
    type(quad_ptr_3D),           dimension(3)       :: face_quads
    type(face_vec_ptr_3D),       dimension(3)       :: normals
    integer, dimension(:), pointer :: n_cells, n_ghost
    integer,               pointer :: n_dim
    type(interpolant_t) :: interp
  contains
    private
    procedure, public, pass :: setup   =>   allocate_derived_grid
    procedure, public, pass :: destroy => deallocate_derived_grid
  end type derived_grid_vars

  type :: grid_block
    integer, dimension(3) :: n_nodes
    integer, dimension(3) :: n_cells
    integer, dimension(3) :: n_ghost
    integer  :: n_dim
    integer  :: total_cells
    real(dp) :: total_volume
    real(dp), allocatable, dimension(:,:,:,:) ::  node_coords
    type(derived_grid_vars) :: grid_vars
  contains
    private
    procedure, public, pass :: setup   =>   allocate_grid_block
    procedure, public, pass :: destroy => deallocate_grid_block
  end type grid_block

  type grid_type
    integer  :: n_blocks
    integer  :: total_int_cells
    real(dp) :: total_int_volume
    type(grid_block), allocatable, dimension(:) :: gblock
  contains
    private
    procedure, public, pass   :: setup => init_grid_type
    procedure, public, pass :: destroy => deallocate_grid
  end type grid_type

contains

  subroutine pack_quadrature_info(gblock,n_quad,n_faces,quad_wts,quad_pts)
    use set_constants,    only : zero
    use index_conversion, only : get_number_of_faces
    type(grid_block), intent(in) :: gblock
    integer,          intent(in) :: n_quad, n_faces
    real(dp), dimension(n_quad,n_faces),   intent(out) :: quad_wts
    real(dp), dimension(3,n_quad,n_faces), intent(out) :: quad_pts
    integer :: d, i,j,k,cnt
    integer, dimension(3) :: offset
    quad_pts = zero
    quad_wts = zero
    cnt = 0
    do d = 1,gblock%n_dim
      offset = 0
      offset(d) = 1
      do k = 1,gblock%n_cells(3)+offset(3)
        do j = 1,gblock%n_cells(2)+offset(2)
          do i = 1,gblock%n_cells(1)+offset(1)
            cnt = cnt + 1
            quad_wts(:,cnt) = gblock%grid_vars%face_quads(d)%p(i,j,k)%quad_wts
            quad_pts(:,:,cnt) = gblock%grid_vars%face_quads(d)%p(i,j,k)%quad_pts
          end do
        end do
      end do
    end do
  end subroutine pack_quadrature_info

  subroutine pack_quadrature_info_z_order(gblock,n_dim,n_quad,n_faces,face_map,quad_wts,quad_pts)
    use set_constants,    only : zero
    use index_conversion, only : global2local_face
    type(grid_block), intent(in) :: gblock
    integer,          intent(in) :: n_quad, n_faces
    integer,  dimension(n_faces),          intent(in)  :: face_map
    real(dp), dimension(n_quad,n_faces),   intent(out) :: quad_wts
    real(dp), dimension(n_dim,n_quad,n_faces), intent(out) :: quad_pts
    integer :: n, d, n_dim
    integer, dimension(3) :: idx, n_cells
    quad_pts = zero
    quad_wts = zero
    n_dim = gblock%n_dim
    n_cells = gblock%n_cells
    do n = 1,n_faces
      idx = 1
      call global2local_face( n_dim, n_cells(1:n_dim), face_map(n), d, idx(1:n_dim) )
      quad_wts(:,n)   = gblock%grid_vars%face_quads(d)%p(idx(1),idx(2),idx(3))%quad_wts
      quad_pts(:,:,n) = gblock%grid_vars%face_quads(d)%p(idx(1),idx(2),idx(3))%quad_pts(1:n_dim,:)
    end do
  end subroutine pack_quadrature_info_z_order

  pure function pack_cell_node_coords(idx,bnd_min,bnd_max,coords_in) result(coords_out)
  integer, dimension(3),                            intent(in)  :: idx, bnd_min, bnd_max
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ), intent(in)  :: coords_in
    real(dp), dimension(3,8)                                     :: coords_out
    integer :: i,j,k,cnt
    cnt = 0
    do k = idx(3),idx(3)+1
      do j = idx(2),idx(2)+1
        do i = idx(1),idx(1)+1
          cnt = cnt + 1
          coords_out(:,cnt) = coords_in(:,i,j,k)
        end do
      end do
    end do
  end function pack_cell_node_coords

  pure function cell_node_coords(idx,stride,bnd_min,bnd_max,coords_in) result(coords_out)
    integer, dimension(3),                            intent(in)  :: idx, stride, bnd_min, bnd_max
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ), intent(in)  :: coords_in
    real(dp), dimension(stride(1)+1,stride(2)+1,stride(3)+1,3)   :: coords_out
    integer :: i,j,k,ii,jj,kk
    kk = 0
    do k = idx(3),idx(3)+stride(3)
      kk = kk + 1
      jj = 0
      do j = idx(2),idx(2)+stride(2)
        jj = jj + 1
        ii = 0
        do i = idx(1),idx(1)+stride(1)
          ii = ii + 1
          coords_out(ii,jj,kk,1) = coords_in(1,i,j,k)
          coords_out(ii,jj,kk,2) = coords_in(2,i,j,k)
          coords_out(ii,jj,kk,3) = coords_in(3,i,j,k)
        end do
      end do
    end do
  end function cell_node_coords
  
  subroutine get_face_quad_ptrs(gblock,cell_idx,face_ids,fquad)
    use quadrature_derived_type, only : quad_ptr
    use index_conversion,        only : get_face_idx_from_id
    type(grid_block), target,                  intent(in)  :: gblock
    integer,        dimension(3),              intent(in)  :: cell_idx
    integer,        dimension(:),              intent(in)  :: face_ids
    type(quad_ptr), dimension(size(face_ids)), intent(out) :: fquad
    integer :: i, dir
    integer, dimension(3) :: face_idx
    call fquad%destroy()
    do i = 1,size(face_ids)
      call get_face_idx_from_id(cell_idx,face_ids(i),dir,face_idx)
      fquad(i)%p => gblock%grid_vars%face_quads(3)%p(face_idx(1),face_idx(2),face_idx(3))
    end do
  end subroutine get_face_quad_ptrs

  pure subroutine init_grid_type( this, n_blocks )
    use set_constants, only : zero
    class(grid_type), intent(inout) :: this
    integer, intent(in) :: n_blocks

    this%n_blocks = n_blocks
    this%total_int_cells = 0
    this%total_int_volume = zero
    allocate( this%gblock(n_blocks) )
  end subroutine init_grid_type

  pure subroutine allocate_grid_block( this, n_dim, n_nodes, n_ghost )
    use set_constants, only : zero
    integer,               intent(in)  :: n_dim
    integer, dimension(3), intent(in)  :: n_nodes, n_ghost
    class(grid_block),     intent(inout) :: this
    integer, dimension(3) :: lo, hi
    this%n_dim   = n_dim
    this%n_nodes = n_nodes
    this%n_ghost = n_ghost
    this%n_cells = 1; this%n_cells(1:n_dim) = n_nodes(1:n_dim) - 1
    lo = 1; lo(1:n_dim) = 1 - n_ghost(1:n_dim)
    hi = 1; hi(1:n_dim) = n_nodes(1:n_dim) + n_ghost(1:n_dim)
    allocate( this%node_coords( n_dim, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    this%node_coords = zero
    this%total_volume = zero
    this%total_cells  = product(this%n_cells)
  end subroutine allocate_grid_block

  subroutine allocate_derived_grid( this, gblock )
    use set_constants,  only : zero
    use interpolant_derived_type, only : interpolant_t
    class(derived_grid_vars), target, intent(inout)   :: this
    class(grid_block),       target, intent(inout) :: gblock
    integer, dimension(3) :: lo, hi
    this%n_cells => gblock%n_cells
    this%n_ghost => gblock%n_ghost
    this%n_dim   => gblock%n_dim
    lo = 1
    lo(1:this%n_dim) = 1 - this%n_ghost(1:this%n_dim)
    hi = 1
    hi(1:this%n_dim) = this%n_cells(1:this%n_dim) + this%n_ghost(1:this%n_dim)
    allocate( this%volume(    lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%cell_c( 3, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    ! not including ghost cells
    ! allocate( this%quad(      lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    this%volume = zero
    this%cell_c = zero

    lo = 1
    hi = 1
    hi(1:this%n_dim) = this%n_cells(1:this%n_dim)
    allocate( this%quad(      lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%xi_area(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_area(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_area( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_face_quad(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_face_quad(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_face_quad( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_nv(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_nv(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_nv( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    this%xi_area   = zero
    this%eta_area  = zero
    this%zeta_area = zero

    this%interp = interpolant_t(N=10)

    call compute_quadrature_points( gblock, gblock, [1,1,1], 4 )

    this%face_quads(1)%p => this%xi_face_quad
    this%face_quads(2)%p => this%eta_face_quad
    this%face_quads(3)%p => this%zeta_face_quad
    this%normals(1)%p    => this%xi_nv
    this%normals(2)%p    => this%eta_nv
    this%normals(3)%p    => this%zeta_nv
  end subroutine allocate_derived_grid

  subroutine compute_quadrature_points( gblock1, gblock, n_skip, quad_order )
    use quadrature_derived_type, only : create_quad_ref_1D,                    &
                                        create_quad_ref_2D,                    &
                                        create_quad_ref_3D,                    &
                                        map_quad_ref_to_physical
    class(grid_block),     intent(in)    :: gblock1
    class(grid_block),     intent(inout) :: gblock
    integer, dimension(3), intent(in)    :: n_skip
    integer,               intent(in)    :: quad_order
    type(quad_t), dimension(0:3) :: ref_quads
    real(dp), dimension(n_skip(1)+1,n_skip(1)+1,n_skip(1)+1,3) :: coords_tmp
    integer :: i, j, k
    integer :: status
    integer, dimension(3) :: idx, bnd_min, bnd_max, sz, loc1, loc2

    sz = n_skip + 1
    call create_quad_ref_1D(          1, ref_quads(0) )
    call create_quad_ref_1D( quad_order, ref_quads(1) )
    call create_quad_ref_2D( quad_order, ref_quads(2) )
    call create_quad_ref_3D( quad_order, ref_quads(3) )

    bnd_min = [ lbound(gblock1%node_coords,2), lbound(gblock1%node_coords,3), lbound(gblock1%node_coords,4) ]
    bnd_max = [ ubound(gblock1%node_coords,2), ubound(gblock1%node_coords,3), ubound(gblock1%node_coords,4) ]
    ! first the volume quads
    loc1 = 2
    loc1(gblock%n_dim+1:) = 0
    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max, gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1, gv%interp, ref_quads(gblock%n_dim), gv%quad(i,j,k), status=status )
            gv%volume = sum( gv%quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
    end do
    ! now the face quads

    ! xi-faces
    loc1 = 2
    if ( gblock%n_dim /= 1) loc1(gblock%n_dim+1:) = 0
    loc1(1) = 0

    loc2 = 2
    if ( gblock%n_dim /= 1) loc2(gblock%n_dim+1:) = 0
    loc2(1) = 1

    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,1
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max, gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1, gv%interp, ref_quads(gblock%n_dim-1), gv%xi_face_quad(i,j,k), status=status )
            gv%xi_area = sum( gv%xi_face_quad(i,j,k)%quad_wts )
          end associate
        end do
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max, gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc2, gv%interp, ref_quads(gblock%n_dim-1), gv%xi_face_quad(i+1,j,k), status=status )
            gv%xi_area = sum( gv%xi_face_quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
    end do

    if ( gblock%n_dim == 1) return

    ! eta-faces
    loc1 = 2
    loc1(gblock%n_dim+1:) = 0
    loc1(2) = 0

    loc2 = 2
    loc2(gblock%n_dim+1:) = 0
    loc2(2) = 1

    do k = 1,gblock%n_cells(3)
      do j = 1,1
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max, gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1, gv%interp, ref_quads(gblock%n_dim-1), gv%eta_face_quad(i,j,k), status=status )
            gv%eta_area = sum( gv%eta_face_quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max, gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc2, gv%interp, ref_quads(gblock%n_dim-1), gv%eta_face_quad(i,j+1,k), status=status )
            gv%eta_area = sum( gv%eta_face_quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
    end do

    if ( gblock%n_dim == 2) return

    ! zeta-faces
    loc1 = 2
    loc1(3) = 0

    loc2 = 2
    loc2(3) = 1

    do k = 1,1
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max, gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1, gv%interp, ref_quads(gblock%n_dim-1), gv%zeta_face_quad(i,j,k), status=status )
            gv%zeta_area = sum( gv%zeta_face_quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
    end do
    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max, gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc2, gv%interp, ref_quads(gblock%n_dim-1), gv%zeta_face_quad(i,j,k+1), status=status )
            gv%zeta_area = sum( gv%zeta_face_quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
    end do

    call ref_quads%destroy()

  end subroutine compute_quadrature_points

  pure elemental subroutine deallocate_grid(this)
    use set_constants, only : zero
    class(grid_type), intent(inout) :: this
    if ( allocated(this%gblock) ) then
      call this%gblock%destroy()
      deallocate( this%gblock)
    end if
    this%n_blocks = 0
    this%total_int_cells = 0
    this%total_int_volume = zero
  end subroutine deallocate_grid

  pure elemental subroutine deallocate_grid_block( this )
    use set_constants, only : zero
    class(grid_block), intent(inout) :: this
    call this%grid_vars%destroy()
    if (allocated( this%node_coords ) ) deallocate( this%node_coords )
    this%total_volume = zero
    this%total_cells  = 0
    this%n_dim        = 0
    this%n_nodes      = 0
    this%n_cells      = 0
  end subroutine deallocate_grid_block

  pure elemental subroutine deallocate_derived_grid( this )
    class(derived_grid_vars), intent(inout) :: this
    call this%interp%destroy()
    call this%face_quads(1)%destroy()
    call this%face_quads(2)%destroy()
    call this%face_quads(3)%destroy()
    call this%normals(1)%destroy()
    call this%normals(2)%destroy()
    call this%normals(3)%destroy()
    if ( allocated(this%cell_c)    ) deallocate( this%cell_c    )
    if ( allocated(this%volume)    ) deallocate( this%volume    )
    if ( allocated(this%xi_area)   ) deallocate( this%xi_area   )
    if ( allocated(this%eta_area)  ) deallocate( this%eta_area  )
    if ( allocated(this%zeta_area) ) deallocate( this%zeta_area )
    if ( allocated(this%quad) ) then
      call this%quad%destroy()
      deallocate( this%quad )
    end if
    if ( allocated(this%xi_face_quad) ) then
      call this%xi_face_quad%destroy()
      deallocate( this%xi_face_quad )
    end if
    if ( allocated(this%eta_face_quad) ) then
      call this%eta_face_quad%destroy()
      deallocate( this%eta_face_quad )
    end if
    if ( allocated(this%zeta_face_quad) ) then
      call this%zeta_face_quad%destroy()
      deallocate( this%zeta_face_quad )
    end if
    if ( allocated(this%xi_nv) ) then
      call this%xi_nv%destroy()
      deallocate(this%xi_nv)
    end if
    if ( allocated(this%eta_nv) ) then
      call this%eta_nv%destroy()
      deallocate(this%eta_nv)
    end if
    if ( allocated(this%zeta_nv) ) then
      call this%zeta_nv%destroy()
      deallocate(this%zeta_nv)
    end if
  end subroutine deallocate_derived_grid

end module grid_derived_type

module monomial_basis_derived_type
  implicit none
  private
  public :: monomial_basis_t
  type :: monomial_basis_t
    private
    integer, public :: total_degree
    integer, public :: n_dim
    integer, public :: n_terms
    integer, public, allocatable, dimension(:)   :: idx
    integer, public, allocatable, dimension(:,:) :: exponents
  contains
    private
    procedure, public, pass :: eval  => evaluate_monomial
    procedure, public, pass :: deval => evaluate_monomial_derivative
    procedure, public, pass :: destroy => destroy_monomial_basis_t
  end type monomial_basis_t

  interface monomial_basis_t
    procedure constructor
  end interface monomial_basis_t

contains

  pure function constructor( total_degree, n_dim ) result(this)
    use combinatorics, only : nchoosek, get_exponents
    integer, intent(in) :: total_degree, n_dim
    type(monomial_basis_t) :: this

    call this%destroy()

    this%total_degree  = total_degree
    this%n_dim   = n_dim
    this%n_terms = nchoosek( n_dim + total_degree, total_degree )
    allocate( this%exponents( this%n_dim, this%n_terms ) )
    allocate( this%idx(this%total_degree+1) )
    call get_exponents( this%n_dim, this%total_degree, this%n_terms, this%exponents, this%idx )
  end function constructor
  
  pure subroutine destroy_monomial_basis_t(this)
    class(monomial_basis_t), intent(inout) :: this
    if ( allocated(this%exponents) ) deallocate( this%exponents )
    if ( allocated(this%idx) )       deallocate( this%idx )
  end subroutine destroy_monomial_basis_t

  pure subroutine evaluate_monomial(this,term,x,val,coef)
    use set_precision, only : dp
    use set_constants, only : one
    class(monomial_basis_t), intent(in)  :: this
    integer,                 intent(in)  :: term
    real(dp), dimension(:),  intent(in)  :: x
    real(dp),                        intent(out) :: val
    integer,                         intent(out) :: coef
    integer :: d, i
    val  = one
    coef = 1
    do d = 1,this%n_dim
      do i = this%exponents(d,term),1,-1
        val  = val * x(d)
        coef = coef * i
      end do
    end do
  end subroutine evaluate_monomial

  pure subroutine evaluate_monomial_derivative(this,term,x,order,dval,dcoef,coef)
    use set_precision, only : dp
    use set_constants, only : zero, one
    class(monomial_basis_t),         intent(in)  :: this
    integer,                         intent(in)  :: term
    real(dp), dimension(:),          intent(in)  :: x
    integer,  dimension(:),          intent(in)  :: order
    real(dp),                        intent(out) :: dval
    integer,                         intent(out) :: dcoef, coef
    integer :: d, i
    
    dcoef = 1
    coef  = 1
    dval  = zero
    if ( any( this%exponents(:,term)-order(1:this%n_dim) < 0 ) ) return

    dval  = one
    do d = 1,this%n_dim
      do i = this%exponents(d,term),this%exponents(d,term)-order(d)+1,-1
        dcoef = dcoef * i
      end do
      do i = this%exponents(d,term)-order(d),1,-1
        dval  = dval * x(d)
        coef = coef * i
      end do
    end do
  end subroutine evaluate_monomial_derivative

end module monomial_basis_derived_type

module function_holder_type
  use set_precision, only : dp
  implicit none
  private
  public :: func_h_t

  type, abstract :: func_h_t
    integer :: n_eq
    integer :: n_dim
  contains
    procedure :: initialize_super
    procedure, pass :: test_eval
    procedure(eval_i),    public, deferred :: eval
    procedure(destroy_i), public, deferred :: destroy
  end type func_h_t

  abstract interface
    pure function eval_i( this, x, t ) result(q)
      use set_precision,  only : dp
      import func_h_t
      class(func_h_t),        intent(in) :: this
      real(dp), dimension(:), intent(in) :: x
      real(dp), optional,     intent(in) :: t
      real(dp), dimension(this%n_eq)     :: q
    end function eval_i

    pure elemental subroutine destroy_i(this)
      import func_h_t
      class(func_h_t), intent(inout) :: this
    end subroutine destroy_i
  end interface

contains
  subroutine initialize_super( this, n_eq, n_dim )
    class(func_h_t),  intent(inout) :: this
    integer,      intent(in)    :: n_eq, n_dim
    this%n_eq     = n_eq
    this%n_dim    = n_dim
  end subroutine initialize_super

  pure function test_eval( this, n_dim, n_var, x ) result(val)
    use set_constants, only : zero
    class(func_h_t),        intent(in) :: this
    integer,                intent(in) :: n_dim, n_var
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(n_var)         :: val
    real(dp), dimension(this%n_eq)     :: tmp_val
    integer :: sz, i
    sz = min(n_var,this%n_eq)
    tmp_val = this%eval(x)
    val = zero
    do i = 1,sz
      val(i) = tmp_val(i)
    end do
  end function test_eval

end module function_holder_type

module cross_term_sinusoid
  use set_precision, only : dp
  use function_holder_type, only : func_h_t
  use monomial_basis_derived_type, only : monomial_basis_t
  implicit none
  private
  public :: cts_t

  type, extends(func_h_t) :: cts_t
    real(dp), dimension(:),   allocatable :: dt, t0, a, e, f, g
    real(dp), dimension(:,:), allocatable :: dx, x0, b, c, d
    type(monomial_basis_t)                :: mono
  contains
    procedure :: eval    => eval_cts
    procedure :: destroy => destroy_cts
  end type cts_t

  interface cts_t
    procedure constructor
  end interface cts_t
contains
  function constructor(n_dim,n_eq,mean,space_coefs,space_scale,space_origin,time_coefs,time_scale,time_origin,rand_coefs) result(this)
    use set_constants, only : zero, one, two, pi, near_zero
    use combinatorics, only : nchoosek

    integer,                                                       intent(in) :: n_dim, n_eq
    real(dp), dimension(n_eq),                           optional, intent(in) :: mean
    real(dp), dimension(n_eq,3*nchoosek(2*n_dim,n_dim)), optional, intent(in) :: space_coefs
    real(dp), dimension(n_dim,n_eq),                     optional, intent(in) :: space_scale
    real(dp), dimension(n_dim,n_eq),                     optional, intent(in) :: space_origin
    real(dp), dimension(n_eq,3),                         optional, intent(in) :: time_coefs
    real(dp), dimension(n_eq),                           optional, intent(in) :: time_scale
    real(dp), dimension(n_eq),                           optional, intent(in) :: time_origin
    logical,                                             optional, intent(in) :: rand_coefs
    type(cts_t)                                                               :: this
    integer :: n, cnt
    integer :: total_degree, n_terms

    call this%destroy()
    this%n_dim = n_dim
    this%n_eq  = n_eq
    total_degree = n_dim
    this%mono = monomial_basis_t(total_degree,n_dim)
    n_terms = this%mono%n_terms

    allocate( this%dt(n_eq) )
    allocate( this%t0(n_eq) )
    allocate( this%dx(n_dim,n_eq) )
    allocate( this%x0(n_dim,n_eq) )
    allocate( this%a(n_eq) )
    allocate( this%b(n_eq,n_terms) )
    allocate( this%c(n_eq,n_terms) )
    allocate( this%d(n_eq,n_terms) )
    allocate( this%e(n_eq) )
    allocate( this%f(n_eq) )
    allocate( this%g(n_eq) )
    
    this%dt = one
    this%dx = one
    this%t0 = zero
    this%x0 = zero
    this%a  = zero
    this%b  = one
    this%c  = one
    this%d  = one
    this%e  = one
    this%f  = one
    this%g  = one

    if (present(rand_coefs) ) then
      call random_init(.true.,.false.)
      call random_number(this%a); this%a = two*this%a - one
      call random_number(this%b); this%b = two*this%b - one
      call random_number(this%c); this%c = two*this%c - one
      call random_number(this%d); this%d = two*this%d - one
      call random_number(this%e); this%e = two*this%e - one
      call random_number(this%f); this%f = two*this%f - one
      call random_number(this%g); this%g = two*this%g - one
    else
      if (present(mean)) this%a = mean
      if (present(space_coefs)) then
        cnt = 0
        do n = 1,n_terms
          cnt = cnt + 1
          this%b(:,n) = space_coefs(:,cnt)
        end do
        do n = 1,n_terms
          cnt = cnt + 1
          this%c(:,n) = space_coefs(:,cnt)
        end do
        do n = 1,n_terms
          cnt = cnt + 1
          this%d(:,n) = space_coefs(:,cnt)
        end do
      end if
      if ( present(space_scale) ) then
        this%dx = sign(one,space_scale) * max(near_zero,abs(space_scale))
      end if
      if ( present(space_origin) ) this%x0 = space_origin
      if ( present(time_coefs)   ) then
        this%e  = time_coefs(:,1)
        this%f  = time_coefs(:,2)
        this%g  = time_coefs(:,3)
      end if
      if ( present(time_scale) ) then
        this%dt = sign(one,time_scale) * max(near_zero,abs(time_scale))
      end if
      if ( present(time_origin) ) this%t0 = time_origin
    end if
  end function constructor

  pure elemental subroutine destroy_cts(this)
    class(cts_t), intent(inout) :: this
    if ( allocated(this%dt)   ) deallocate( this%dt   )
    if ( allocated(this%dx)   ) deallocate( this%dx   )
    if ( allocated(this%t0)   ) deallocate( this%t0   )
    if ( allocated(this%x0)   ) deallocate( this%x0   )
    if ( allocated(this%a)    ) deallocate( this%a    )
    if ( allocated(this%b)    ) deallocate( this%b    )
    if ( allocated(this%c)    ) deallocate( this%c    )
    if ( allocated(this%d)    ) deallocate( this%d    )
    if ( allocated(this%e)    ) deallocate( this%e    )
    if ( allocated(this%f)    ) deallocate( this%f    )
    if ( allocated(this%g)    ) deallocate( this%g    )
    call this%mono%destroy()
  end subroutine destroy_cts

  pure function eval_cts( this, x, t ) result(q)
    class(cts_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    real(dp), dimension(this%n_dim,this%n_eq) :: x_bar
    real(dp), dimension(this%n_eq)            :: t_bar
    real(dp) :: tmp_val
    integer :: coef, i, n
      do i = 1,this%n_eq
        x_bar(:,i) = (x(1:this%n_dim) - this%x0(:,i)) / this%dx(:,i)
      end do
      q = this%a
      do n = 1,this%mono%n_terms
        do i = 1,this%n_eq
          call this%mono%eval(n,x_bar(:,i),tmp_val,coef)
          q(i) = q(i) + this%b(i,n) * sin( this%c(i,n) * tmp_val + this%d(i,n) )
        end do
      end do
      if ( present(t) ) then
        t_bar = ( t - this%t0 ) / this%dt
        do i = 1,this%n_eq
          q(i) = q(i) + this%e(i) * sin( this%f(i) * t_bar(i) + this%g(i) )
        end do
      end if
    end function eval_cts

end module cross_term_sinusoid

module zero_mean_basis_derived_type
  use set_precision, only : dp
  use quadrature_derived_type, only : quad_t
  use monomial_basis_derived_type, only : monomial_basis_t
  implicit none
  private
  public :: zero_mean_basis_t
  type :: zero_mean_basis_t
    private
    real(dp), public, allocatable, dimension(:)   :: moments
    real(dp), public, allocatable, dimension(:)   :: x_ref
    real(dp), public, allocatable, dimension(:)   :: h_ref
  contains
    private
    procedure, pass :: compute_grid_moments
    procedure, pass :: transform
    
    procedure, nopass       :: length_scale  => get_length_scale_vector
    procedure, public, pass :: eval  => evaluate_basis
    procedure, public, pass :: deval => evaluate_basis_derivative
    procedure, public, pass :: scaled_basis_derivative
    procedure, public, pass :: scaled_basis_derivatives
    procedure, public, pass :: destroy => destroy_zero_mean_basis_t

  end type zero_mean_basis_t

  interface zero_mean_basis_t
    procedure constructor
  end interface

contains

function constructor( p, quad, h_ref ) result(this)
  type(monomial_basis_t), intent(in) :: p
  type(quad_t),           intent(in) :: quad
  real(dp), dimension(:), intent(in) :: h_ref
  type(zero_mean_basis_t)            :: this
  integer :: n

  call this%destroy()
  allocate( this%moments( p%n_terms ) )
  allocate( this%x_ref( p%n_dim ) )
  allocate( this%h_ref( p%n_dim ) )
  this%h_ref   = h_ref(1:p%n_dim)
  this%x_ref   = quad%integrate( p%n_dim, quad%quad_pts(1:p%n_dim,:) ) / sum( quad%quad_wts )
  this%moments = this%compute_grid_moments(p,quad)
  ! this%moments = this%compute_grid_moment([(n,n=1,p%n_terms)],quad)
end function constructor

pure subroutine destroy_zero_mean_basis_t(this)
  class(zero_mean_basis_t), intent(inout) :: this
  if ( allocated(this%moments) ) deallocate( this%moments )
  if ( allocated(this%x_ref)   ) deallocate( this%x_ref   )
  if ( allocated(this%h_ref)   ) deallocate( this%h_ref   )
end subroutine destroy_zero_mean_basis_t

pure function transform(this,n_dim,x) result(x_bar)
  class(zero_mean_basis_t), intent(in) :: this
  integer,                  intent(in) :: n_dim
  real(dp), dimension(:),   intent(in) :: x
  real(dp), dimension(n_dim)           :: x_bar
  x_bar = (x(1:n_dim)-this%x_ref)/this%h_ref
end function transform

pure function compute_grid_moments(this,p,quad) result(moments)
  use set_constants, only : one
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  type(quad_t),             intent(in) :: quad
  real(dp), dimension(p%n_terms)       :: moments
  real(dp), dimension(quad%n_quad)         :: tmp
  real(dp), dimension(p%n_dim,quad%n_quad) :: xtmp
  integer :: n, q, coef
  do q = 1,quad%n_quad
    xtmp(:,q) = this%transform( p%n_dim, quad%quad_pts(:,q) )
  end do
  do n = 1,p%n_terms
    do q = 1,quad%n_quad
      call p%eval( n, xtmp(:,q), tmp(q), coef )
    end do
    moments(n) = quad%integrate(tmp)
  end do
  moments = moments / sum( quad%quad_wts )
end function compute_grid_moments

pure function evaluate_basis(this,p,term,point) result(B)
  use set_constants, only : one
  class(zero_mean_basis_t),     intent(in) :: this
  type(monomial_basis_t),       intent(in) :: p
  integer,                      intent(in) :: term
  real(dp), dimension(:),       intent(in) :: point
  real(dp) :: B
  integer :: coef
  B = one
  if (term == 1) return
  call p%eval( term, this%transform(p%n_dim,point), B, coef )
  B = B - this%moments(term)
end function evaluate_basis

pure function evaluate_basis_derivative(this,p,term,point,order) result(dB)
  use set_constants, only : zero, one
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  integer,                  intent(in) :: term
  integer,  dimension(:), intent(in) :: order
  real(dp), dimension(:), intent(in) :: point
  real(dp) :: dB
  integer :: dcoef,coef

  if (all(order==0)) then
    dB =  this%eval(p,term,point)
    return
  end if

  dB = zero
  if (term==1) return

  call p%deval( term, this%transform(p%n_dim,point), order, dB, dcoef, coef )
  dB = dB * real( dcoef, dp ) / product( this%h_ref ** order(1:p%n_dim) )
end function evaluate_basis_derivative

pure function get_length_scale_vector( order, scale ) result(L)
  use set_constants, only : one
  integer,  dimension(:),           intent(in) :: order
  real(dp), dimension(size(order)), intent(in) :: scale
  real(dp)                                     :: L
  integer :: n, d, den

  L   = one
  den = 1
  do d = 1,size(order)
    do n = order(d),1,-1
      L   = L * scale(d)
      den = den * n
    end do
  end do
  L = L / real(den,dp)

end function get_length_scale_vector

pure function scaled_basis_derivative( this, p, term_idx, diff_idx, point, scale ) result(derivative)
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  integer,                  intent(in) :: term_idx, diff_idx
  real(dp), dimension(:),   intent(in) :: point
  real(dp), dimension(:),   intent(in) :: scale
  real(dp)                             :: derivative
  real(dp) :: L
  derivative = this%deval( p, term_idx, point, p%exponents(:,diff_idx) )
  L = this%length_scale( p%exponents(:,diff_idx), scale )
  derivative = derivative * L
end function scaled_basis_derivative

pure function scaled_basis_derivatives( this, p, term_start, term_end, point, scale ) result(derivatives)
  use set_constants, only : zero
  class(zero_mean_basis_t),                 intent(in) :: this
  type(monomial_basis_t),                   intent(in) :: p
  integer,                                  intent(in) :: term_start, term_end
  real(dp), dimension(:),                   intent(in) :: point
  real(dp), dimension(:),                   intent(in) :: scale
  real(dp), dimension(term_end, term_end - term_start) :: derivatives
  integer :: i, j
  derivatives = zero
  ! outer loop over basis functions
  do j = term_start+1,term_end
    ! inner loop over derivatives
    do i = 1,j
      derivatives(i,j-term_start) = this%scaled_basis_derivative(p,j,i,point,scale);
    end do
  end do
end function scaled_basis_derivatives

end module zero_mean_basis_derived_type


module var_rec_cell_derived_type
  use set_precision,                only : dp
  use monomial_basis_derived_type,  only : monomial_basis_t
  use zero_mean_basis_derived_type, only : zero_mean_basis_t
  use quadrature_derived_type,      only : quad_t
  implicit none
  private
  public :: var_rec_cell_t
  type :: var_rec_cell_t
    ! private
    integer :: n_vars
    integer :: n_interior
    integer :: self_idx
    integer :: self_block
    integer, dimension(:), allocatable      :: nbor_block, nbor_idx, face_id
    type(zero_mean_basis_t)                 :: basis
    real(dp), dimension(:,:),   allocatable :: coefs, RHS
    real(dp), dimension(:,:),   allocatable :: A, D, LU, P
    real(dp), dimension(:,:,:), allocatable :: B, C
  contains
    private
    procedure, public, pass :: destroy => destroy_var_rec_cell_t
    procedure, public, pass :: eval => evaluate_reconstruction
    procedure, public, pass :: get_nbor_contribution
    procedure, public, pass :: get_self_RHS_contribution
    procedure, public, pass :: get_nbor_RHS_contribution
    
  end type var_rec_cell_t

  interface var_rec_cell_t
    procedure constructor
  end interface var_rec_cell_t

contains

  pure elemental subroutine destroy_var_rec_cell_t(this)
    class(var_rec_cell_t), intent(inout) :: this
    call this%basis%destroy()
    if ( allocated(this%nbor_block) ) deallocate( this%nbor_block )
    if ( allocated(this%nbor_idx  ) ) deallocate( this%nbor_idx   )
    if ( allocated(this%face_id   ) ) deallocate( this%face_id    )
    if ( allocated(this%coefs     ) ) deallocate( this%coefs      )
    if ( allocated(this%RHS       ) ) deallocate( this%RHS        )
    if ( allocated(this%A         ) ) deallocate( this%A          )
    if ( allocated(this%D         ) ) deallocate( this%D          )
    if ( allocated(this%LU        ) ) deallocate( this%LU         )
    if ( allocated(this%P         ) ) deallocate( this%P          )
    if ( allocated(this%B         ) ) deallocate( this%B          )
    if ( allocated(this%C         ) ) deallocate( this%C          )
  end subroutine destroy_var_rec_cell_t

  function constructor( p, self_block, self_idx, nbor_block, nbor_idx, face_id, n_interior, n_vars, quad, h_ref ) result(this)
    use set_constants, only : zero
    type(monomial_basis_t),        intent(in) :: p
    integer,                       intent(in) :: self_block, self_idx
    integer, dimension(2*p%n_dim), intent(in) :: nbor_block, nbor_idx, face_id
    integer,                       intent(in) :: n_interior, n_vars
    type(quad_t),                  intent(in) :: quad
    real(dp), dimension(p%n_dim),  intent(in) :: h_ref
    type(var_rec_cell_t)                      :: this
    call this%destroy()
    this%basis = zero_mean_basis_t( p, quad, h_ref )
    allocate( this%nbor_block( 2*p%n_dim ) )
    allocate( this%nbor_idx(   2*p%n_dim ) )
    allocate( this%face_id(    2*p%n_dim ) )
    allocate( this%coefs( p%n_terms, n_vars ) )
    allocate( this%RHS(p%n_terms-1, n_vars ) )
    allocate( this%A(  p%n_terms-1, p%n_terms-1 ) )
    allocate( this%D(  p%n_terms-1, p%n_terms-1 ) )
    allocate( this%LU( p%n_terms-1, p%n_terms-1 ) )
    allocate( this%P(  p%n_terms-1, p%n_terms-1 ) )
    allocate( this%B(  p%n_terms-1, p%n_terms-1, n_interior ) )
    allocate( this%C(  p%n_terms-1, p%n_terms-1, n_interior ) )
    this%coefs      = zero
    this%RHS        = zero
    this%A = zero; this%B = zero; this%C = zero; this%D = zero; this%LU = zero; this%P = zero
    this%self_block = self_block
    this%self_idx   = self_idx
    this%nbor_block = nbor_block
    this%nbor_idx   = nbor_idx
    this%face_id    = face_id
    this%n_interior = n_interior
    this%n_vars     = n_vars
  end function constructor

  pure function evaluate_reconstruction(this,p,point,n_terms,n_var,var_idx) result(val)
    use set_constants, only : zero
    class(var_rec_cell_t),      intent(in) :: this
    type(monomial_basis_t),     intent(in) :: p
    real(dp), dimension(:),     intent(in) :: point
    integer,                    intent(in) :: n_terms, n_var
    integer,  dimension(n_var), intent(in) :: var_idx
    real(dp), dimension(n_var)             :: val
    real(dp), dimension(n_terms) :: basis
    integer :: v, n
    val = zero
    do n = 1,n_terms
      basis(n) = this%basis%eval(p,n,point)
    end do
    do v = 1,n_var
      val(v) = val(v) + dot_product( this%coefs(1:n_terms,var_idx(v)), basis )
    end do
  end function evaluate_reconstruction

  pure subroutine get_nbor_contribution( this, nbor, p, fquad, term_start, term_end, A, B, D, C )
    use set_constants, only : zero, one
    class(var_rec_cell_t),  intent(in) :: this
    class(var_rec_cell_t),  intent(in) :: nbor
    type(monomial_basis_t), intent(in) :: p
    class(quad_t),          intent(in) :: fquad
    integer,                intent(in) :: term_start, term_end
    real(dp), dimension(term_end - term_start, term_end - term_start), intent(out) :: A, B
    real(dp), dimension(term_end - term_start, term_start),            intent(out) :: D, C
    real(dp), dimension(term_end,term_end) :: d_basis_i
    real(dp), dimension(term_end,term_end) :: d_basis_j
    integer :: q, l, m
    real(dp), dimension(p%n_dim) :: dij
    real(dp) :: xdij_mag

    A = zero; B = zero; C = zero; D = zero
    dij = abs( this%basis%x_ref - nbor%basis%x_ref )
    do q = 1,fquad%n_quad
      d_basis_i = this%basis%scaled_basis_derivatives( p, 0, term_end, fquad%quad_pts(:,q), dij )
      d_basis_j = nbor%basis%scaled_basis_derivatives( p, 0, term_end, fquad%quad_pts(:,q), dij )
      ! LHS
      do m = 1,term_end-term_start
        do l = 1,term_end-term_start
          A(l,m) = A(l,m) + fquad%quad_wts(q) * dot_product( d_basis_i(:,l+term_start), d_basis_i(:,m+term_start) )
          B(l,m) = B(l,m) + fquad%quad_wts(q) * dot_product( d_basis_i(:,l+term_start), d_basis_j(:,m+term_start) )
        end do
      end do

      ! RHS
      do m = 1,term_start
        do l = 1,term_end-term_start
          D(l,m) = D(l,m) + fquad%quad_wts(q) * dot_product( d_basis_i(:,l+term_start), d_basis_i(:,m) )
          C(l,m) = C(l,m) + fquad%quad_wts(q) * dot_product( d_basis_i(:,l+term_start), d_basis_j(:,m) )
        end do
      end do
    end do
    xdij_mag = one/norm2(dij)
    A = A * xdij_mag
    B = B * xdij_mag
    C = C * xdij_mag
    D = D * xdij_mag
  end subroutine get_nbor_contribution

  pure function get_self_RHS_contribution( this, term_start, term_end, n_var, var_idx ) result(b)
    use set_constants, only : zero, one
    class(var_rec_cell_t),      intent(in) :: this
    integer,                    intent(in) :: term_start, term_end, n_var
    integer,  dimension(:),     intent(in) :: var_idx
    real(dp), dimension( term_end - term_start, n_var ) :: b
    integer :: n, m, v
    m = term_end - term_start
    n = term_start
    do v = 1,n_var
      b(:,v) = matmul( this%D(1:m,1:n), this%coefs(1:n,var_idx(v)) )
    end do
  end function get_self_RHS_contribution

  pure function get_nbor_RHS_contribution( this, nbor, nbor_id, term_start, term_end, n_var, var_idx ) result(b)
    use set_constants, only : zero, one
    class(var_rec_cell_t),    intent(in) :: this
    class(var_rec_cell_t),    intent(in) :: nbor
    integer,                  intent(in) :: nbor_id, term_start, term_end, n_var
    integer,  dimension(:),   intent(in) :: var_idx
    real(dp), dimension( term_end - term_start, n_var ) :: b
    integer :: n, m, v
    m = term_end - term_start
    n = term_start
    do v = 1,n_var
      b(:,v) = matmul( this%C(1:m,1:n,nbor_id), nbor%coefs(1:n,var_idx(v)) )
    end do
  end function get_nbor_RHS_contribution

end module var_rec_cell_derived_type

module var_rec_block_derived_type
  use set_precision,               only : dp
  use monomial_basis_derived_type, only : monomial_basis_t
  use var_rec_cell_derived_type,   only : var_rec_cell_t
  implicit none
  private
  public :: var_rec_block_t
  ! public :: spatial_function
  type :: var_rec_block_t
    private
    integer, public                                 :: block_num, n_dim, degree, n_vars, n_cells_total
    integer, public,      dimension(:), allocatable :: n_cells
    type(var_rec_cell_t), dimension(:), allocatable :: cells
    type(monomial_basis_t), public :: p
  contains
    private
    procedure, public, pass :: destroy => destroy_var_rec_block_t
    procedure, public, pass :: solve   => perform_iterative_reconstruction_SOR
    procedure, public, pass :: set_cell_avgs, init_cells
    procedure, public, pass :: get_cell_error, get_error_norm
    procedure,         pass :: get_cell_LHS, get_cell_RHS
    procedure,         pass :: get_cell_update, get_cell_residual, residual_norm
    procedure,         pass :: SOR_iteration
  end type var_rec_block_t

  interface var_rec_block_t
    module procedure constructor
  end interface var_rec_block_t

  ! abstract interface
  !   pure function spatial_function(n_dim,n_var,x) result(val)
  !     use set_precision, only : dp
  !     integer,                intent(in) :: n_dim, n_var
  !     real(dp), dimension(:), intent(in) :: x
  !     real(dp), dimension(n_var)         :: val
  !   end function spatial_function
  ! end interface

contains

  pure elemental subroutine destroy_var_rec_block_t( this )
    class(var_rec_block_t), intent(inout) :: this
    if ( allocated(this%n_cells) ) deallocate( this%n_cells )
    if ( allocated(this%cells) ) then
      call this%cells%destroy()
      deallocate( this%cells )
    end if
    this%block_num     = 0
    this%n_dim         = 0
    this%degree        = 0
    this%n_vars        = 0
    this%n_cells_total = 0
  end subroutine destroy_var_rec_block_t

  function constructor( grid, block_num, n_dim, degree, n_var ) result(this)
    
    use math,              only : maximal_extents
    use index_conversion,  only : cell_face_nbors, global2local_bnd
    use grid_derived_type, only : grid_type, pack_cell_node_coords
    implicit none
    type(grid_type),   intent(in) :: grid
    integer,           intent(in) :: block_num, n_dim, degree, n_var
    type(var_rec_block_t)         :: this
    integer,  dimension(3)     :: idx_tmp, lo, hi1, hi2
    real(dp), dimension(3,8)   :: nodes
    real(dp), dimension(n_dim) :: h_ref
    integer,  dimension(2*n_dim) :: nbor_block, nbor_cell_idx, nbor_face_id
    integer :: n, n_interior
    call this%destroy()
    allocate( this%n_cells( n_dim ) )
    this%n_cells        = grid%gblock(block_num)%n_cells(1:n_dim)
    this%n_cells_total  = product(this%n_cells)
    this%block_num      = block_num
    this%n_dim          = n_dim
    this%degree         = degree
    this%n_vars         = n_var
    this%p = monomial_basis_t( this%degree, this%n_dim )
    allocate( this%cells(this%n_cells_total) )

    lo = [1,1,1]; hi1 = grid%gblock(block_num)%n_cells; hi2 = grid%gblock(block_num)%n_nodes
    idx_tmp = 1
    do n = 1,this%n_cells_total
      idx_tmp(1:n_dim) = global2local_bnd( n, lo(1:n_dim), hi1(1:n_dim) )
      call cell_face_nbors( n_dim, n, lo(1:n_dim), hi1(1:n_dim), nbor_cell_idx, nbor_face_id, n_interior )
      nbor_block = block_num
      nodes = pack_cell_node_coords( idx_tmp, lo, hi2, grid%gblock(block_num)%node_coords )
      h_ref = maximal_extents( n_dim, 8, nodes(1:n_dim,:) )
      this%cells(n) = var_rec_cell_t( this%p, block_num, n, nbor_block, nbor_cell_idx, nbor_face_id, n_interior, n_var, grid%gblock(block_num)%grid_vars%quad( idx_tmp(1), idx_tmp(2), idx_tmp(3) ), h_ref )
    end do
  end function constructor

  subroutine get_cell_LHS( this, grid, lin_idx, term_start, term_end )
    use set_constants,           only : zero
    use index_conversion,        only : get_face_idx_from_id, global2local_bnd
    use math,                    only : LUdecomp
    use grid_derived_type,       only : grid_type
    use quadrature_derived_type, only : quad_t
    use var_rec_cell_derived_type, only : var_rec_cell_t
    class(var_rec_block_t), intent(inout) :: this
    type(grid_type),        intent(in)    :: grid
    integer,                intent(in)    :: lin_idx, term_start, term_end
    real(dp), dimension(term_end - term_start, term_end - term_start ) :: dA
    real(dp), dimension(term_end - term_start,            term_start ) :: dD
    integer, dimension(3) :: lo, hi, idx, face_idx
    integer :: i, j, jj, m, n, dir, n_interior
    m = term_end - term_start
    n = term_start
    i = lin_idx
    lo = [1,1,1]; hi = grid%gblock(this%block_num)%n_cells
    idx = 1; idx(1:this%p%n_dim) = global2local_bnd(i,lo(1:this%p%n_dim),hi(1:this%p%n_dim))
    n_interior = this%cells(lin_idx)%n_interior

    this%cells(i)%A = zero
    this%cells(i)%B = zero
    this%cells(i)%C = zero
    this%cells(i)%D = zero
    this%cells(i)%LU = zero
    this%cells(i)%P = zero
      do jj = 1,n_interior
        j = this%cells(i)%nbor_idx(jj)
        call get_face_idx_from_id( idx, this%cells(i)%face_id(jj), dir, face_idx )
        call this%cells(i)%get_nbor_contribution( this%cells(j), this%p, grid%gblock(this%block_num)%grid_vars%face_quads(dir)%p(face_idx(1),face_idx(2),face_idx(3)), term_start, term_end, dA, this%cells(i)%B(1:m,1:m,jj), dD, this%cells(i)%C(1:m,1:n,jj) )
        this%cells(i)%A(1:m,1:m) = this%cells(i)%A(1:m,1:m) + dA
        this%cells(i)%D(1:m,1:n) = this%cells(i)%D(1:m,1:n) + dD
    end do
    call LUdecomp(this%cells(i)%LU(1:m,1:m), this%cells(i)%P(1:m,1:m), this%cells(i)%A(1:m,1:m), m )
  end subroutine get_cell_LHS

  pure function get_cell_RHS( this, lin_idx, term_start, term_end, n_var, var_idx ) result(b)
    use set_constants,           only : zero
    class(var_rec_block_t),     intent(in) :: this
    integer,                    intent(in) :: term_start, term_end, lin_idx, n_var
    integer,  dimension(:),     intent(in) :: var_idx
    real(dp), dimension( term_end-term_start, n_var ) :: b
    integer :: i, j, jj, n_interior
    b = zero
    i = lin_idx
    n_interior = this%cells(i)%n_interior
    do jj = 1,n_interior
      j = this%cells(i)%nbor_idx(jj)
      associate( nbor => this%cells(j) )
        b = b + this%cells(i)%get_nbor_RHS_contribution( nbor, jj, term_start, term_end, n_var, var_idx )
      end associate
    end do
    b = b - this%cells(i)%get_self_RHS_contribution( term_start, term_end, n_var, var_idx )
  end function get_cell_RHS

  pure function get_cell_update( this, lin_idx, term_start, term_end, n_var, var_idx ) result(update)
    use set_constants, only : zero, one
    use math, only : LUsolve
    class(var_rec_block_t), intent(in) :: this
    integer,                intent(in) :: lin_idx, term_start, term_end, n_var
    integer,  dimension(:), intent(in) :: var_idx
    real(dp), dimension(term_end-term_start,n_var) :: update
    real(dp), dimension(term_end-term_start,n_var) :: RHS
    integer :: i, j, m, n, jj, v
    m = term_end - term_start
    n = term_start
    i = lin_idx
    do v = 1,n_var
      RHS(:,v) = this%cells(i)%RHS(term_start:term_end-1,var_idx(v))
    end do
    associate( B  => this%cells(i)%B,  &
               LU => this%cells(i)%LU, &
               P  => this%cells(i)%P,  &
               n_interior => this%cells(i)%n_interior )
      do jj = 1,n_interior
        j = this%cells(i)%nbor_idx(jj)
        associate( nbor => this%cells(j) )
          RHS = RHS + matmul( B(1:m,1:m,jj), nbor%coefs(term_start+1:term_end,var_idx) )
        end associate
      end do
      call LUsolve(update, LU(1:m,1:m), P(1:m,1:m), RHS, m, n_var )
    end associate
    update = update - this%cells(i)%coefs(term_start+1:term_end,var_idx)
  end function get_cell_update

  pure function get_cell_residual( this, lin_idx, term_start, term_end, n_var, var_idx ) result(residual)
    use set_constants, only : zero, one
    class(var_rec_block_t), intent(in)    :: this
    integer,                intent(in)    :: lin_idx, term_start, term_end, n_var
    integer, dimension(:),  intent(in)    :: var_idx
    real(dp), dimension( term_end-term_start, n_var ) :: residual
    integer :: m, n, i, j, jj, v
    residual = zero
    m = term_end - term_start
    n = term_start
    i = lin_idx
    associate( A  => this%cells(i)%A,  &
               B  => this%cells(i)%B,  &
               n_interior => this%cells(i)%n_interior )
      do v = 1,n_var
        residual(:,v) = residual(:,v) + matmul( A(1:m,1:m), this%cells(i)%coefs(term_start+1:term_end,var_idx(v)) )
      end do
      residual = residual - this%get_cell_RHS(i,term_start,term_end,n_var,var_idx)
      do jj = 1,n_interior
        j = this%cells(i)%nbor_idx(jj)
        do v = 1,n_var
          residual(:,v) = residual(:,v) - matmul( B(1:m,1:m,jj), this%cells(j)%coefs(term_start+1:term_end,var_idx(v)) )
        end do
      end do
    end associate
  end function get_cell_residual

  pure function residual_norm(this,term_start,term_end,n_var,var_idx) result(residual)
    use set_constants, only : zero
    class(var_rec_block_t), intent(in)    :: this
    integer,                intent(in)    :: term_start, term_end, n_var
    integer,  dimension(:), intent(in)    :: var_idx
    real(dp), dimension(n_var)    :: residual
    integer :: i
    residual = zero
    do i = 1,this%n_cells_total
      residual = residual + sum( ( this%get_cell_residual(i,term_start,term_end,n_var,var_idx) )**2,dim=1 )
    end do
    residual = sqrt( residual )
  end function residual_norm

  subroutine SOR_iteration( this, term_start, term_end, n_var, var_idx, omega, residual )
    use set_constants, only : zero, one
    class(var_rec_block_t),       intent(inout) :: this
    integer,                      intent(in)    :: term_start, term_end, n_var
    integer,  dimension(:),       intent(in)    :: var_idx
    real(dp),                     intent(in)    :: omega
    real(dp), dimension(n_var),   intent(out) :: residual
    real(dp), dimension(term_end-term_start,n_var) :: update
    integer :: i, v
    residual = zero
    do i = 1,this%n_cells_total
      update = omega * this%get_cell_update(i,term_start,term_end,n_var,var_idx)
      residual = residual + sum( update**2,dim=1 )
      do v = 1,n_var
        this%cells(i)%coefs(term_start+1:term_end,var_idx(v)) = this%cells(i)%coefs(term_start+1:term_end,var_idx(v)) + update(:,v)
      end do
    end do
    residual = sqrt( residual )
  end subroutine SOR_iteration

  subroutine perform_iterative_reconstruction_SOR(this,term_start,term_end,n_var,var_idx,omega,tol,n_iter,converged,residual)
    use set_constants, only : zero, one
    class(var_rec_block_t),       intent(inout) :: this
    integer,                            intent(in)    :: term_start, term_end, n_var
    integer,  dimension(:),             intent(in)    :: var_idx
    real(dp), optional,                 intent(in)    :: omega, tol
    integer,  optional,                 intent(in)    :: n_iter
    logical,  optional,                 intent(out)   :: converged
    real(dp), optional, dimension(:,:), allocatable, intent(out) :: residual
    real(dp) :: omega_, tol_
    real(dp), dimension(n_var) :: res_tmp, res_init
    real(dp), dimension(n_var) :: res_tmp2, res_init2
    integer :: n, n_iter_
    logical :: converged_

    omega_ = 1.3_dp
    tol_   = 1.0e-8_dp
    n_iter_ = 100
    if ( present(omega)     ) omega_     = omega
    if ( present(tol)       ) tol_       = tol
    if ( present(n_iter)    ) n_iter_    = n_iter
    if ( present(converged) ) converged  = .false.
    converged_ = .false.
    if (present(residual)) then
      if (allocated(residual)) deallocate(residual)
      allocate(residual(n_var,n_iter_))
    end if
    res_init2 = this%residual_norm( term_start, term_end, n_var, var_idx )
    call this%SOR_iteration( term_start, term_end, n_var, var_idx, omega_, res_init )
    ! write(*,*) 0, res_init, res_init2
    ! write(*,*) 0, res_init2
    call iteration_line('',0,res_init2)
    where ( res_init2 < epsilon(one) ) res_init2 = one
    where ( res_init  < epsilon(one) ) res_init  = one

    do n = 1,n_iter_
      call this%SOR_iteration(term_start, term_end, n_var, var_idx, omega_, res_tmp )
      res_tmp = res_tmp / res_init
      res_tmp2  = this%residual_norm( term_start, term_end, n_var, var_idx )
      res_tmp2 = res_tmp2 / res_init2
      ! if (n==1 .or. mod(n,1)==0) write(*,*) n, res_tmp, res_tmp2
      ! if (n==1 .or. mod(n,1)==0) write(*,*) n, res_tmp2
      if (n==1 .or. mod(n,1)==0) call iteration_line('',n,res_tmp2)
      converged_ = all( res_tmp2 < tol_)
      if ( present(residual ) ) residual(:,n) = res_tmp
      if ( present(converged) ) converged     = converged_
      if ( converged_ ) then
        return
      end if
    end do
  end subroutine perform_iterative_reconstruction_SOR

  pure function get_cell_avg(quad,n_dim,n_var,var_idx,eval_fun) result(avg)
    use set_constants, only : zero
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    type(quad_t),           intent(in) :: quad
    integer,                intent(in) :: n_dim, n_var
    integer,  dimension(:), intent(in) :: var_idx
    class(func_h_t),        intent(in) :: eval_fun
    real(dp), dimension(n_var)         :: avg
    real(dp), dimension(n_var,quad%n_quad) :: tmp_val
    integer :: n
    tmp_val = zero
    do n = 1,quad%n_quad
      ! tmp_val(:,n) = eval_fun( n_dim, n_var, quad%quad_pts(:,n) )
      tmp_val(:,n) = eval_fun%test_eval( n_dim, n_var, quad%quad_pts(:,n) )
    end do
    avg = quad%integrate( n_var, tmp_val ) / sum( quad%quad_wts)
  end function get_cell_avg

  pure subroutine set_cell_avgs(this,gblock,n_var,var_idx,eval_fun)
    use grid_derived_type,       only : grid_block
    use index_conversion,        only : global2local
    use function_holder_type,    only : func_h_t
    class(var_rec_block_t), intent(inout) :: this
    type(grid_block),       intent(in)    :: gblock
    integer,                intent(in)    :: n_var
    integer,  dimension(:), intent(in)    :: var_idx
    ! procedure(spatial_function)           :: eval_fun
    class(func_h_t),        intent(in)    :: eval_fun
    real(dp), dimension(n_var) :: tmp_val
    integer,  dimension(3)     :: tmp_idx
    integer :: i, v
    tmp_idx = 1
    do i = 1, this%n_cells_total
      tmp_idx(1:this%n_dim) = global2local(i,this%n_cells)
      tmp_val = get_cell_avg( gblock%grid_vars%quad(tmp_idx(1),tmp_idx(2),tmp_idx(3)), this%p%n_dim, n_var, var_idx, eval_fun )
      do v = 1,n_var
        this%cells(i)%coefs(1,var_idx(v)) = tmp_val(v)
      end do
    end do
  end subroutine set_cell_avgs

  subroutine update_current_line(string)
    use iso_fortran_env, only : std_out => output_unit
    character(*), intent(in) :: string
    write(std_out,'(A)',advance='no') string
    flush(std_out)
  end subroutine update_current_line

  subroutine progress_line(string,n,n_total)
    character(*), intent(in) :: string
    integer,      intent(in) :: n, n_total
    character(*), parameter :: fmt = '(A,I0,A,I0,A,F5.1,A)'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: out_string
    write(out_string,fmt) string, n,'/',n_total, ' (', real(n,dp)/real(n_total,dp)*100.0_dp, '%)'
    call update_current_line(carriage_return//trim(out_string))
  end subroutine progress_line

  subroutine iteration_line(string,n,residual)
    character(*), intent(in) :: string
    integer,      intent(in) :: n
    real(dp), dimension(:), intent(in) :: residual
    character(*), parameter :: fmt1 = '("(A,I0,",I0,"("" "",ES18.12))")'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: fmt, out_string
    write(fmt,fmt1) size(residual)
    write(out_string,fmt) string, n, residual
    call update_current_line(carriage_return//trim(out_string))
  end subroutine iteration_line

  ! subroutine iteration_line(string,n,residual)
  !   character(*), intent(in) :: string
  !   integer,      intent(in) :: n
  !   real(dp), dimension(:), intent(in) :: residual
  !   character(*) :: fmt1 = '(I0(A,I0,A,I0,A,F5.1,A)'
  !   character(*), parameter :: carriage_return = achar(13)
  !   character(len=100) :: out_string
  !   write(out_string,fmt) carriage_return, string,n,'/',n_total, ' (', real(n,dp)/real(n_total,dp)*100.0_dp, '%)'
  !   call update_current_line(trim(out_string))
  ! end subroutine iteration_line

  subroutine init_cells(this,grid,term_start,term_end,n_var,var_idx)
    use grid_derived_type, only : grid_type 
    class(var_rec_block_t), intent(inout) :: this
    type(grid_type),        intent(in)    :: grid
    integer,                intent(in)    :: term_start, term_end, n_var
    integer, dimension(:),  intent(in)    :: var_idx
    integer :: n
    do n = 1,this%n_cells_total
      ! write(*,'(A,I0,A,I0,A,F5.1,A)') 'initializing cell ',n,'/',this%n_cells_total, ' (', real(n,dp)/real(this%n_cells_total,dp)*100.0_dp, '%)'
      call progress_line('initializing cell ',n,this%n_cells_total)
      call this%get_cell_LHS( grid, n, term_start, term_end )
      this%cells(n)%RHS = this%get_cell_RHS( n, term_start, term_end, n_var, var_idx )
    end do
    write(*,*)
  end subroutine init_cells

  pure function get_cell_error( this, quad, lin_idx, n_terms, norm, n_var, var_idx, eval_fun ) result(err)
    use set_constants,           only : zero, one
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(var_rec_block_t), intent(in) :: this
    type(quad_t),           intent(in) :: quad
    integer,                intent(in) :: lin_idx, n_terms, norm, n_var
    integer, dimension(:),  intent(in) :: var_idx
    class(func_h_t),        intent(in)    :: eval_fun
    ! procedure(spatial_function)        :: eval_fun
    real(dp), dimension(n_var) :: reconstructed, exact, err
    real(dp), dimension(n_var,quad%n_quad) :: tmp_val
    integer, parameter :: max_L_norm = 10
    integer :: n
    tmp_val = zero
    do n = 1,quad%n_quad
      ! exact = eval_fun( this%p%n_dim, n_var, quad%quad_pts(:,n) )
      exact = eval_fun%test_eval( this%p%n_dim, n_var, quad%quad_pts(:,n) )
      reconstructed = this%cells(lin_idx)%eval( this%p, quad%quad_pts(:,n), n_terms, n_var, var_idx )
      tmp_val(:,n) = abs( reconstructed - exact )
    end do
    if (norm>max_L_norm) then
      err = maxval(tmp_val,dim=2)
    else
      err = quad%integrate( size(var_idx), tmp_val**norm )**(one/real(norm,dp))
      err = err / sum( quad%quad_wts)**(one/real(norm,dp))
    end if
    
  end function get_cell_error

  pure function get_error_norm(this,gblock,var_idx,n_terms,norms,eval_fun) result(err_norms)
    use set_constants,           only : zero
    use index_conversion,        only : global2local
    use grid_derived_type,       only : grid_block
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(var_rec_block_t),       intent(in) :: this
    type(grid_block),             intent(in) :: gblock
    integer, dimension(:),        intent(in) :: var_idx
    integer,                      intent(in) :: n_terms
    integer, dimension(:),        intent(in) :: norms
    class(func_h_t),              intent(in) :: eval_fun
    ! procedure(spatial_function)              :: eval_fun
    real(dp), dimension(size(var_idx),size(norms)) :: err_norms
    integer, dimension(this%n_dim) :: cell_idx
    integer, dimension(3) :: tmp_idx
    integer, parameter :: max_L_norm = 10
    integer :: n, i, n_var
    n_var = size(var_idx)
    err_norms = zero
    do n = 1,size(norms)
      if (norms(n)>max_L_norm) then
        do i = 1,this%n_cells_total
          cell_idx = global2local(i,gblock%n_cells(1:this%n_dim))
          tmp_idx = 1
          tmp_idx(1:this%n_dim) = cell_idx
          associate( quad => gblock%grid_vars%quad(tmp_idx(1),tmp_idx(2),tmp_idx(3)) )
            err_norms(:,n) = max( err_norms(:,n), this%get_cell_error( quad, i,n_terms,norms(n),n_var,var_idx,eval_fun) )
          end associate
        end do
      else
        do i = 1,this%n_cells_total
          cell_idx = global2local(i,gblock%n_cells(1:this%n_dim))
          tmp_idx = 1
          tmp_idx(1:this%n_dim) = cell_idx
          associate( quad => gblock%grid_vars%quad(tmp_idx(1),tmp_idx(2),tmp_idx(3)) )
            err_norms(:,n) = err_norms(:,n) + this%get_cell_error( quad, i,n_terms,norms(n),n_var,var_idx,eval_fun)
          end associate
        end do
        err_norms(:,n) = err_norms(:,n) / real( this%n_cells_total, dp )
      end if
    end do
  end function get_error_norm

end module var_rec_block_derived_type

module test_problem
  use set_precision, only : dp
  implicit none
  private
  public :: setup_grid_and_rec
  public :: test_function_1, test_function_2
contains

  pure function test_function_1(n_dim,n_var,x) result(val)
    integer,                intent(in) :: n_dim, n_var
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(n_var)         :: val
    val(1) = 999.0_dp * x(1) - 888.0_dp * x(2) + 777.0_dp * x(3) - 666.0_dp
    val(:) = val(1)
  end function test_function_1

  pure function test_function_2(n_dim,n_var,x) result(val)
  use set_constants, only : pi
    integer,                intent(in) :: n_dim, n_var
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(n_var)         :: val
    integer :: i
    val(1) = sin(pi*x(1))
    do i = 2,n_dim
      val(1) = val(1)*sin(pi*x(i))
    end do
    val(:) = val(1)
    ! val(1) = sin(pi*x(1)) * sin(pi*x(2)) * sin(pi*x(3))
    ! val(1) = sin(pi*x(1)) * sin(pi*x(2))
  end function test_function_2

  subroutine setup_grid_and_rec( n_dim, n_vars, degree, n_nodes, n_ghost, grid, rec )
    use combinatorics, only : nchoosek
    ! use math, only : maximal_extents
    ! use grid_derived_type, only : pack_cell_node_coords
    use grid_derived_type,           only : grid_type
    use var_rec_block_derived_type,  only : var_rec_block_t
    ! use var_rec_block_derived_type,  only : spatial_function 
    use monomial_basis_derived_type, only : monomial_basis_t
    use linspace_helper,             only : unit_cartesian_mesh_cat
    use linspace_helper,             only : unit_cartesian_mesh_cat_perturbed
    use cross_term_sinusoid,         only : cts_t
    integer,                     intent(in)  :: n_dim, n_vars, degree
    integer, dimension(3),       intent(in)  :: n_nodes, n_ghost
    type(grid_type),             intent(out) :: grid
    type(var_rec_block_t),       intent(out) :: rec
    logical :: converged
    integer :: block_num, term_start, term_end
    integer :: n, i, v
    real(dp), dimension(n_vars, 3) :: error_norms
    ! procedure(spatial_function), pointer :: eval_fun => null()
    type(cts_t) :: eval_fun
    ! real(dp), dimension(3,8) :: nodes
    ! real(dp), dimension(n_dim)   :: h_ref

    ! eval_fun => test_function_2

    eval_fun = cts_t(n_dim,n_vars,rand_coefs=.true.)
    call grid%setup(1)
    call grid%gblock(1)%setup(n_dim,n_nodes,n_ghost)
    ! grid%gblock(1)%node_coords = unit_cartesian_mesh_cat(n_nodes(1),n_nodes(2),n_nodes(3))
    grid%gblock(1)%node_coords = unit_cartesian_mesh_cat_perturbed(n_nodes(1),n_nodes(2),n_nodes(3),0.3_dp)
    call grid%gblock(1)%grid_vars%setup( grid%gblock(1) )
    block_num = 1

    ! nodes = pack_cell_node_coords( [1,1,1], [1,1,1], grid%gblock(block_num)%n_nodes, grid%gblock(block_num)%node_coords )
    ! h_ref = maximal_extents( n_dim, 8, nodes(1:n_dim,:) )
    rec = var_rec_block_t( grid, block_num, n_dim, degree, n_vars )

    ! term_start = 1
    ! term_end   = rec%p%n_terms
    ! call rec%set_cell_avgs(grid%gblock(1),n_vars,[(n,n=1,n_vars)],eval_fun)
    ! call rec%init_cells(grid,term_start,term_end,n_vars,[(n,n=1,n_vars)])
    ! call rec%solve(term_start,term_end,n_vars,[(n,n=1,n_vars)],omega=1.3_dp,tol=1e-10_dp,n_iter=100,converged=converged)

    call rec%set_cell_avgs(grid%gblock(1),n_vars,[(n,n=1,n_vars)],eval_fun)
    do i = 1,rec%p%total_degree
      term_start = 1
      term_end   = nchoosek( rec%p%n_dim + i, i )
      write(*,'(A,I0)') "reconstructing: p=",i 
      call rec%init_cells(grid,term_start,term_end,n_vars,[(n,n=1,n_vars)])
      call rec%solve(term_start,term_end,n_vars,[(n,n=1,n_vars)],omega=1.3_dp,tol=1e-10_dp,n_iter=100,converged=converged)
      write(*,*) 'converged =', converged
      error_norms = rec%get_error_norm(grid%gblock(1),[(n,n=1,n_vars)],term_end,[1,2,huge(1)],eval_fun)
      write(*,*) 'Error: '
      do v = 1,n_vars
        write(*,'(I0,3(" ",ES18.12))') v, (error_norms(v,n), n = 1,3)
      end do
    end do
  end subroutine setup_grid_and_rec

end module test_problem

program main
  use set_precision, only : dp
  use set_constants, only : zero, one
  use test_problem,  only : setup_grid_and_rec
  use grid_derived_type, only : grid_type
  use var_rec_block_derived_type, only : var_rec_block_t
  use timer_derived_type, only : basic_timer_t

  implicit none

  type(grid_type) :: grid
  type(var_rec_block_t) :: rec
  type(basic_timer_t) :: timer
  integer :: degree, n_vars, n_dim
  integer, dimension(3) :: n_nodes, n_ghost

  degree  = 5
  n_vars  = 1
  n_dim   = 1
  n_nodes = [5,5,5]
  n_ghost = [0,0,0]
  call timer%tic()
  call setup_grid_and_rec( n_dim, n_vars, degree, n_nodes, n_ghost, grid, rec )
  write(*,*) 'Elapsed time: ', timer%toc()
  call rec%destroy()
  call grid%destroy()
end program main