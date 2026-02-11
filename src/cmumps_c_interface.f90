!
! This file is part of MUMPS 5.8.2
!
! Simplified C-interoperable interface for MUMPS using iso_c_binding
! This provides a Python-friendly API via ctypes
!

module mumps_c_interface
  use iso_c_binding
  use cmumps_struc_def
  implicit none

  ! Opaque handle type (pointer to Fortran MUMPS structure)
  type, bind(C) :: mumps_handle
    type(c_ptr) :: ptr
  end type mumps_handle

contains

  !> Initialize MUMPS instance
  !!
  !! @param n Matrix dimension
  !! @param sym Symmetry: 0=unsymmetric, 1=SPD, 2=general symmetric
  !! @param par Parallel mode: 1=host working, 0=host not working
  !! @param comm MPI communicator (0 for sequential)
  !! @return Opaque handle to MUMPS structure
  function mumps_initialize(n, sym, par, comm) bind(C, name="mumps_initialize") result(handle)
    integer(c_int), intent(in), value :: n, sym, par, comm
    type(mumps_handle) :: handle
    type(CMUMPS_STRUC), pointer :: mumps_par

    ! Allocate MUMPS structure
    allocate(mumps_par)

    ! Set parameters for initialization
    mumps_par%sym = sym
    mumps_par%par = par
    mumps_par%comm = comm

    ! Initialize MUMPS (job = -1) - N should be 0 here
    mumps_par%job = -1
    call cmumps(mumps_par)

    ! Now set N after initialization
    mumps_par%n = n

    ! Return opaque handle
    handle%ptr = c_loc(mumps_par)
  end function mumps_initialize

  !> Set matrix in COO format (1-indexed)
  !!
  !! @param handle MUMPS handle
  !! @param nz Number of non-zeros
  !! @param irn Row indices (1-indexed)
  !! @param jcn Column indices (1-indexed)
  !! @param a Matrix values
  subroutine mumps_set_matrix(handle, nz, irn, jcn, a) bind(C, name="mumps_set_matrix")
    type(mumps_handle), intent(in), value :: handle
    integer(c_int), intent(in), value :: nz
    integer(c_int), intent(in) :: irn(*), jcn(*)
    real(c_double), intent(in) :: a(*)
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)

    ! Set number of non-zeros
    mumps_par%nz = nz

    ! Allocate and copy matrix data
    if (associated(mumps_par%irn)) deallocate(mumps_par%irn)
    if (associated(mumps_par%jcn)) deallocate(mumps_par%jcn)
    if (associated(mumps_par%a)) deallocate(mumps_par%a)

    allocate(mumps_par%irn(nz))
    allocate(mumps_par%jcn(nz))
    allocate(mumps_par%a(nz))

    mumps_par%irn(1:nz) = irn(1:nz)
    mumps_par%jcn(1:nz) = jcn(1:nz)
    mumps_par%a(1:nz) = a(1:nz)
  end subroutine mumps_set_matrix

  !> Set right-hand side vector
  !!
  !! @param handle MUMPS handle
  !! @param rhs Right-hand side vector (length n)
  subroutine mumps_set_rhs(handle, rhs) bind(C, name="mumps_set_rhs")
    type(mumps_handle), intent(in), value :: handle
    real(c_double), intent(in) :: rhs(*)
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)

    ! Allocate RHS if not already allocated
    if (.not. associated(mumps_par%rhs)) then
      allocate(mumps_par%rhs(mumps_par%n))
    endif

    ! Copy RHS data
    mumps_par%rhs(1:mumps_par%n) = rhs(1:mumps_par%n)
  end subroutine mumps_set_rhs

  !> Get solution vector
  !!
  !! @param handle MUMPS handle
  !! @param sol Solution vector output (length n)
  subroutine mumps_get_solution(handle, sol) bind(C, name="mumps_get_solution")
    type(mumps_handle), intent(in), value :: handle
    real(c_double), intent(out) :: sol(*)
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)

    ! Copy solution (stored in RHS after solve)
    sol(1:mumps_par%n) = mumps_par%rhs(1:mumps_par%n)
  end subroutine mumps_get_solution

  !> Perform symbolic factorization (analysis phase)
  !!
  !! @param handle MUMPS handle
  subroutine mumps_analyze(handle) bind(C, name="mumps_analyze")
    type(mumps_handle), intent(in), value :: handle
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)

    ! Analysis phase (job = 1)
    mumps_par%job = 1
    call cmumps(mumps_par)
  end subroutine mumps_analyze

  !> Perform numerical factorization
  !!
  !! @param handle MUMPS handle
  subroutine mumps_factorize(handle) bind(C, name="mumps_factorize")
    type(mumps_handle), intent(in), value :: handle
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)

    ! Factorization phase (job = 2)
    mumps_par%job = 2
    call cmumps(mumps_par)
  end subroutine mumps_factorize

  !> Solve the system
  !!
  !! @param handle MUMPS handle
  subroutine mumps_solve(handle) bind(C, name="mumps_solve")
    type(mumps_handle), intent(in), value :: handle
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)

    ! Solve phase (job = 3)
    mumps_par%job = 3
    call cmumps(mumps_par)
  end subroutine mumps_solve

  !> Finalize MUMPS (cleanup)
  !!
  !! @param handle MUMPS handle
  subroutine mumps_finalize(handle) bind(C, name="mumps_finalize")
    type(mumps_handle), intent(in), value :: handle
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)

    ! Finalize MUMPS (job = -2)
    mumps_par%job = -2
    call cmumps(mumps_par)

    ! Deallocate structure
    deallocate(mumps_par)

    ! Note: Cannot nullify handle%ptr from here as it's passed by value
  end subroutine mumps_finalize

  !> Get info parameter
  !!
  !! @param handle MUMPS handle
  !! @param idx Info index (1-80)
  !! @return Info value
  function mumps_get_info(handle, idx) bind(C, name="mumps_get_info") result(info_val)
    type(mumps_handle), intent(in), value :: handle
    integer(c_int), intent(in), value :: idx
    integer(c_int) :: info_val
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)
    info_val = mumps_par%info(idx)
  end function mumps_get_info

  !> Set integer control parameter
  !!
  !! @param handle MUMPS handle
  !! @param idx ICNTL index (1-60)
  !! @param val Value to set
  subroutine mumps_set_icntl(handle, idx, val) bind(C, name="mumps_set_icntl")
    type(mumps_handle), intent(in), value :: handle
    integer(c_int), intent(in), value :: idx, val
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)
    mumps_par%icntl(idx) = val
  end subroutine mumps_set_icntl

  !> Get integer control parameter
  !!
  !! @param handle MUMPS handle
  !! @param idx ICNTL index (1-60)
  !! @return ICNTL value
  function mumps_get_icntl(handle, idx) bind(C, name="mumps_get_icntl") result(val)
    type(mumps_handle), intent(in), value :: handle
    integer(c_int), intent(in), value :: idx
    integer(c_int) :: val
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)
    val = mumps_par%icntl(idx)
  end function mumps_get_icntl

  !> Set real control parameter
  !!
  !! @param handle MUMPS handle
  !! @param idx CNTL index (1-15)
  !! @param val Value to set
  subroutine mumps_set_cntl(handle, idx, val) bind(C, name="mumps_set_cntl")
    type(mumps_handle), intent(in), value :: handle
    integer(c_int), intent(in), value :: idx
    real(c_double), intent(in), value :: val
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)
    mumps_par%cntl(idx) = val
  end subroutine mumps_set_cntl

  !> Get real info parameter
  !!
  !! @param handle MUMPS handle
  !! @param idx RINFO index (1-40)
  !! @return RINFO value
  function mumps_get_rinfo(handle, idx) bind(C, name="mumps_get_rinfo") result(val)
    type(mumps_handle), intent(in), value :: handle
    integer(c_int), intent(in), value :: idx
    real(c_double) :: val
    type(CMUMPS_STRUC), pointer :: mumps_par

    call c_f_pointer(handle%ptr, mumps_par)
    val = mumps_par%rinfo(idx)
  end function mumps_get_rinfo

end module mumps_c_interface
