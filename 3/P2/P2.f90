program P2
implicit none

	integer :: theta, i
	real :: r
 	real, parameter :: p = 1200.0, pi = 4*atan(1.0)
	integer, dimension (3):: theta_min, theta_max
	real, dimension (3):: e, r_min, r_max
	
	open (unit=1, file="P2input.dat", status="old", action="read")
		do i = 1,3
			read (1,*) e(i)
		end do
	close (1)
	
!Displaying required outputs	
	open ( unit = 2, file = "P2output1.txt", status = "replace")
	write (2, 20) "For epsilon = ", e(1)
	write (2, *) "Theta 	      r"
	open ( unit = 3, file = "P2output2.txt", status = "replace")
	write (3, 20) "For epsilon = ", e(2)
	write (3, *) "Theta 	      r"
	open ( unit = 4, file = "P2output3.txt", status = "replace")
	write (4, 20) "For epsilon = ", e(3)
	write (4, *) "Theta 	      r"
	open ( unit = 5, file = "P2output4.txt", status = "replace")
	
	do i = 1,3
	
	theta_min(i)=0
	r_min(i) = p / (1 - e(i)* cos(real(theta_min(i)) * pi/180.0))
	theta_max(i)=1
	r_max(i) = p / (1 - e(i)* cos(real(theta_max(i)) * pi/180.0))
	
		do theta = 0, 359
			r = p / (1 - e(i)* cos(real(theta) * pi/180.0)) !Given equation
			
		write ((i+1), 10) theta," 	 ", r 
			10 format (i4,a,f10.5)
			
			!Finding minimum r and locating corresponding theta			
			if (r <= r_min(i)) then 
				r_min(i) = r
				theta_min(i) = theta
			end if
		
			!Finding maximum r and locating corresponding theta
			if (r >= r_max(i)) then 
				r_max(i) = r
				theta_max(i) = theta
			end if

		end do
	
		write (5, 20) "For epsilon =", e(i)
			20 format (a,f5.2)
		write (5, 30) " The orbit is closest to Earth at distance ", r_min(i), " km, at theta = ", theta_min(i)
		write (5, 30) " The orbit is farthest from Earth at distance ", r_max(i), " km, at theta = ", theta_max(i)
			30 format (a,f9.4,a,i3)
	end do
	
	close(2)
	close(3)
	close(4)
	close(5)		
	
end program P2