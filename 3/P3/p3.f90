program P3
implicit none

	integer :: i, Vo
	real, dimension (3) :: Tf, Tk
	real :: Id, Vd
	real, parameter :: Io = 2.0e-6 , q = 1.602e-19, k = 1.38e-23

!Saving given temperatures (F) in an array	
	Tf = (/75, 100, 125 /)
	
!Converting Temperature from fahrenheit to kelvin
	do i = 1,3
		Tk(i)= ( Tf(i) - 32 ) * ( 5.0 / 9.0 ) + 273.15
	end do

!Displaying output of Current corresponding to Voltage for each Temperature	
	open (unit = 2, file = "P3output.txt", status = "replace")
	
	do i = 1,3
		write (2,10) "At T=",Tf(i), " F"
			10 format (a, f6.1 ,a)
		write (2,*) 
		write (2,*)"Voltage (V)                   Current (A)"
		write (2,*) 
		
			do Vo = -10, 6, 1
				Vd = Vo/10.0
				Id = Io * ( exp(( q * (Vd) ) / ( k * Tk(i) )) - 1 ) !Given equation
			
				write (2, 20) Vd, "          ", Id
					20 format (f5.1, a, f30.15)
			end do
			
		write (2, *) 
	end do
	
	close(2)
	
end program P3