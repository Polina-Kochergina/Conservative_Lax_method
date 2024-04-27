program main
use mymod
implicit none
		
integer :: n, m, k, err, j, i, idx
real(8) :: a, dx, dt, c
real(8), allocatable, dimension(:,:) :: U
real(8), allocatable, dimension(:) :: X
CHARACTER(LEN=30) :: name

! vel:
a = 1.0
! Courant number:
c = 0.01
m = 1000
! step along x coordinate:
dx = 1.0/m
! time step:
dt = c*dx/a
n = int(1.0/dt)

! write parameters:
write(*, *) " m = ", m
write(*, *) " n = ", n
write(*, *) " dt = ", dt
write(*, *) " dx = ",dx
write(*, *) " c = ",c


   
allocate(X(m), U(2,m), stat=err) 
if (err /= 0) print *, "arrays: Allocation request denied"

! The Couchy problem:
! periodic in x from 0 to 1
!       |= 0.4, x < 0.4
! U(0)  |= 0.8, 0.4 < x < 0.6
!       |= 0.4, x > 0.6
do idx = 1, m

    if ((idx < m*0.4).or.(idx > 0.6*m)) then
        X(idx) = 0.4
    else 
        X(idx) = 0.8
    endif

enddo

! found X, we use the module where the sweep method is implemented 
call Solve(X, n, m, C)

! don't forget to free up memory
deallocate(U, stat=err)
if (err /= 0) print *, "arrays: Deallocation request denied"



end program