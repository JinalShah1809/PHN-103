program P9
implicit none

	integer :: R
	real :: S !Safe loading
	
	open (unit = 1, file = "P9output.txt", status = "replace", action = "write")
	
		write (1,*) " Value of R        Safe Loading (S)"
		
		!given Safe loading equations
		do R = 25, 250, 25
            if ( R < 120 ) then
                S = 17000.0 - 0.485 * (R ** 2.0)
            else
                S = 18000.0 / ( 1 + (( R ** 2.0) / 18000.0))
            end if
            write (1, 2) R, S
            2 format (i8, f25.4)
		end do
	close (1)
	
end program P9