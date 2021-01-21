program P6
implicit none

	integer :: theta
	real, dimension(91) :: G
	real, parameter :: pi = 4 * atan(1.0)
	
	theta = 0 !Initializing theta 
	
	open(unit=1 , file="P6output.txt")
            write (1, *) "  Antenna Gain vs Angle (deg)" !Labelling table with title
            write (1, *)
            write (1, *) "  Antenna Gain    Angle (deg)" !Column headings
            write (1, *)
			
		do while (theta <= 90)
		
		!Calculating value of antenna gain 
			if ( theta == 0 ) then
				G(theta) = 0.0
			else
				G(theta) = abs( (sin(6.0  * theta * pi / 180.0 )) / (6.0  * theta * pi / 180.0) )
				
			write (1,10) G(theta),"          ", theta !Displaying table in output
				10 format (f10.5,a,i4)
				
			end if
		
		theta = theta + 1
		end do
	close(1)
	
end program P6