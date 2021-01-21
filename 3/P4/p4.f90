program P4
implicit none

	real :: d, T, d_min, T_min
 	real, parameter :: Lc = 3.0, Lp = 3.0, W = 1960.0 !W = mg, m=200, g=9.8
	
!Initializing variables
	d = 0.5
	d_min = 0.5
	T_min = (W * Lp * Lc)/(d_min * sqrt((lp ** 2) - (d_min ** 2)))
	
!Displaying required outputs	
	open ( unit = 1, file = "p4output.txt", status = "replace")
	write (1, *) "Value of d     Tension (T)"
	
	do while (d <= 2.8)
		T = (W * Lp * Lc)/(d * sqrt((Lp ** 2) -(d ** 2))) !Given equation
		
		if (T <= T_min) then !Finding minimum Tension and locating corresponding distance
			T_min = T
			d_min = d
		end if
		
		write (1, 10) d, "           ", T !Displaying respective Tension for each distance
			10 format (f5.1, a, f10.4)
			
		d = d + 0.1
		
	end do
	
	!Displaying minimum tension and corresponding distance	
	write (1,20) "The minumum value of Tension is", T_min, " N at distance of", d_min, " m." 
		20 format (a, f10.4, a, f4.1, a)
	
	close (1)
	
end program P4