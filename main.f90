
program bouncing_ball
    use iso_c_binding
    implicit none

    interface
        function GetStdHandle(n) bind(C, name="GetStdHandle") result(h)
            import
            integer(c_int32_t), value :: n
            type(c_ptr) :: h
        end function

        function GetConsoleMode(h, mode) &
            bind(C, name="GetConsoleMode") result(r)
            import
            type(c_ptr), value :: h
            integer(c_int32_t) :: mode
            integer(c_int) :: r
        end function

        function SetConsoleMode(h, mode) &
            bind(C, name="SetConsoleMode") result(r)
            import
            type(c_ptr), value :: h
            integer(c_int32_t), value :: mode
            integer(c_int) :: r
        end function

        subroutine Sleep(ms) bind(C, name="Sleep")
            import
            integer(c_int32_t), value :: ms
        end subroutine
    end interface

    integer, parameter :: W=40, H=15
    integer :: x,y,dx,dy
    integer :: i,j,frame
    integer(c_int32_t) :: oldmode
    integer(c_int) :: result
    type(c_ptr) :: hout
    character(len=W+2) :: line
    character(len=1) :: screen(H,W)

    ! Initialize Windows console
    hout = GetStdHandle(-11_c_int32_t)
    result = GetConsoleMode(hout,oldmode)

    if (result /= 0) then
        result = SetConsoleMode(hout,ior(oldmode,4_c_int32_t))
    end if

    x=5
    y=5
    dx=1
    dy=1

    write(*,'(A)',advance='no') achar(27)//'[2J'

    do frame=1,1000

        screen=' '
        screen(y,x)='O'

        ! Move cursor to home
        write(*,'(A)',advance='no') achar(27)//'[H'

        write(*,'(A)') "       B O U N C I N G   B A L L"
        write(*,'(A)') '+'//repeat('-',W)//'+'

        do i=1,H
            line=' '
            line(1:1)='|'

            do j=1,W
                line(j+1:j+1)=screen(i,j)
            end do

            line(W+2:W+2)='|'
            write(*,'(A)') line
        end do

        write(*,'(A)') '+'//repeat('-',W)//'+'
        write(*,'(A,I5)') 'Frame: ',frame

        ! Update position
        x=x+dx
        y=y+dy

        ! Wall collisions
        if (x>=W) dx=-1
        if (x<=1) dx=1

        if (y>=H) dy=-1
        if (y<=1) dy=1

        ! 60 milliseconds per frame
        call Sleep(60_c_int32_t)

    end do

    write(*,'(A)',advance='no') achar(27)//'[0m'
    write(*,'(A)',advance='no') achar(27)//'[?25h'
    print *, "Animation completed!"

end program bouncing_ball
