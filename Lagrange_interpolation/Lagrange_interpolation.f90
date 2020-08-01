program Lagr_interpol
    implicit none

    integer::i,j,k,n
    real::a,L=0.0,p=1.0
    real,dimension(3)::x

    print*, "Enter the no. of input values: "
    read*, n
    print*, "Enter the input data points: "
    do i = 1, 3
        read*, x(i)
    enddo
    print*,"Enter the no. value at which the function is to be approximated: "
    read*, a

    do j = 1, 3
        p = 1.0
        do k = 1, 3
            if(j /= k) then
                p = p * ((a - x(k))/(x(j) - x(k)))
            endif
            enddo
        L = L + p * log(real(x(j)))
    enddo

    print*, "The required value is :", L
    print*, abs(real(log(2.0) - L))/log(2.0)

end program Lagr_interpol
