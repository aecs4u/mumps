!
! Dense BLAS benchmark for ZGEMM (complex double precision).
! Prints parseable key=value output for automation scripts.
!
program zgemm_bench
  implicit none
  integer :: m, n, k, iters, i
  integer :: argc
  character(len=64) :: arg, val
  complex(8), allocatable :: a(:,:), b(:,:), c(:,:)
  complex(8) :: alpha, beta
  real(8) :: t0, t1, seconds, gflops

  m = 2048
  n = 2048
  k = 2048
  iters = 3

  argc = command_argument_count()
  i = 1
  do while (i <= argc)
    call get_command_argument(i, arg)
    if (trim(arg) == '--m') then
      if (i + 1 > argc) call die_usage()
      call get_command_argument(i + 1, val)
      read(val, *) m
      i = i + 2
    else if (trim(arg) == '--n') then
      if (i + 1 > argc) call die_usage()
      call get_command_argument(i + 1, val)
      read(val, *) n
      i = i + 2
    else if (trim(arg) == '--k') then
      if (i + 1 > argc) call die_usage()
      call get_command_argument(i + 1, val)
      read(val, *) k
      i = i + 2
    else if (trim(arg) == '--iters') then
      if (i + 1 > argc) call die_usage()
      call get_command_argument(i + 1, val)
      read(val, *) iters
      i = i + 2
    else if (trim(arg) == '--help' .or. trim(arg) == '-h') then
      call print_usage()
      stop 0
    else
      write(*, '(a,a)') 'Unknown argument: ', trim(arg)
      call die_usage()
    end if
  end do

  if (m <= 0 .or. n <= 0 .or. k <= 0 .or. iters <= 0) then
    write(*,*) 'All dimensions and --iters must be positive.'
    stop 2
  end if

  allocate(a(m, k), b(k, n), c(m, n))
  call random_number(a)
  call random_number(b)
  c = (0.0d0, 0.0d0)

  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)

  ! Warmup
  call zgemm('N', 'N', m, n, k, alpha, a, m, b, k, beta, c, m)

  call wall_time(t0)
  do i = 1, iters
    call zgemm('N', 'N', m, n, k, alpha, a, m, b, k, beta, c, m)
  end do
  call wall_time(t1)

  seconds = t1 - t0
  ! Complex GEMM has 8x FLOPs compared to real GEMM
  gflops = (8.0d0 * dble(m) * dble(n) * dble(k) * dble(iters)) / (seconds * 1.0d9)

  write(*,'(a,i0)') 'ZGEMM_BENCH_M=', m
  write(*,'(a,i0)') 'ZGEMM_BENCH_N=', n
  write(*,'(a,i0)') 'ZGEMM_BENCH_K=', k
  write(*,'(a,i0)') 'ZGEMM_BENCH_ITERS=', iters
  write(*,'(a,f0.6)') 'ZGEMM_BENCH_SECONDS=', seconds
  write(*,'(a,f0.3)') 'ZGEMM_BENCH_GFLOPS=', gflops

  deallocate(a, b, c)
end program zgemm_bench

subroutine wall_time(t)
  implicit none
  real(8), intent(out) :: t
  integer :: count, rate
  call system_clock(count, rate)
  t = dble(count) / dble(rate)
end subroutine wall_time

subroutine print_usage()
  implicit none
  write(*,'(a)') 'Usage: ./zgemm_bench [--m M] [--n N] [--k K] [--iters I]'
  write(*,'(a)') 'Defaults: M=N=K=2048, ITERS=3'
end subroutine print_usage

subroutine die_usage()
  implicit none
  call print_usage()
  stop 2
end subroutine die_usage
