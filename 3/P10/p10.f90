program P10
implicit none

	integer :: n, m
	real :: W !wavelength
	
	open (unit = 1, file = "P10output.txt", status = "replace", action = "write")
	
		write (1,*) "(m)      (n)       Wavelength(W)"
		
		do m = 2, 50, 1
            do n = 1, (m-1), 1
				W = 911.8 / ((1.0/(n ** 2)) - (1.0/(m ** 2))) !Given equation
				
				write (1,2) m, n, W
				2 format (i3, i9, f20.5)
				
			end do
		end do
	close (1)
	
end program P10