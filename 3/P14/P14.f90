program P14
implicit none

	real :: x, y, z, r, theta, phi

!Reading Cartesian coordinates from input file	
	open (unit = 1, file = "P14input.dat", status = "old")
		read (1,*) x, y, z
	close(1)

!Using subroutine to find Spherical coordinates	
	call coordinates(x,y,z,r,theta,phi)
	
	open (unit = 2, file = "P14output.txt", status = "replace")
		write (2,*) "r = ", r
		write (2,*) "theta = ", theta
		write (2,*) "phi = ", phi
	close(2)

end program P14

subroutine coordinates(x,y,z,r,theta,phi)
implicit none
	real, intent(in) :: x, y, z
	real, intent(out) :: r, theta, phi
	
	r = sqrt(x**2 + y**2 + z**2)
	theta = acos(z/sqrt(x**2 + y**2 + z**2))
	phi = atan(y/x)
	
end subroutine coordinates