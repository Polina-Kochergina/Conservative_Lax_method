module mymod
implicit none
    
contains 

subroutine Solve(X, n, m, C)
    real(8), dimension(2, m) :: U
    real(8), dimension(:) :: X
    real(8) dx, dt, a, C
    integer :: i, j, n, m, ios
    CHARACTER(LEN=20) :: name, cu
    CHARACTER(LEN=30) :: Format
    ! format output in fortran
    10 format(f10.8)
    100 format(5x, a)
 
    ! initial values, the Couchy problem:
    U(1, :) = X
    
    ! transform C(real) to cu(char) (curent number)
    write(cu,'(F4.2)') c
    print*, cu

    ! create name of output-file
    name = trim(".\DATA\lmeth")//trim(cu)//trim(".csv")
    print*, name

    open(unit=2, file=name, iostat=ios)
    if ( ios /= 0 ) stop "Error opening file name"
    

    ! Use conservative lax method:
    do i = 1, n
        do j = 2, m-1
            U(2, j) = 1.0/2.0*(U(1, j+1) + U(1, j-1)) - 0.5*C*(U(1, j+1) - U(1, j-1))
        enddo
        ! periodic conditions in x 
        U(2, 1) = 0.5*(U(1, 2) + U(1, m-1)) - 0.5*C*(U(1, 2) - U(1, m-2 ))
        U(2, m) = U (2, 1)

        ! rewrite U
        U(1, :) = U(2, :)

        ! write  every 10(100)th vector in output-file 
        if (MOD(i, 100) == 0) then
            write(2, *)  U(1, :)
        endif
    enddo

    close(unit=2, iostat=ios)
    if ( ios /= 0 ) stop "Error closing file unit 2"

end subroutine Solve

end module

