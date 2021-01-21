module determinant
contains

real function det(a)
implicit none
	real, dimension(2,2) :: a
	
	det = (a(1,1) * a(2,2)) - (a(1,2) * a(2,1))
	
end function det
end module determinant

program P13
use determinant
implicit none

	integer :: i,j
	real, dimension(3,3) :: m
	real, dimension(2,2) :: m11, m12, m13
	real :: d, d11, d12, d13

!Reading from input files
	open(unit = 1, file = "P13input.dat", status = "old", action = "read")
		
		!Loop to read matrix 
		do i = 1,3
			read(1, *, end=10) (m(i,j), j = 1, 3)
		end do
	10 close(1)

!Finding minor matrix corresponding to each element in first row
	m11 = m(2:3,2:3)
	m12(:,1) = m(2:3,1)
	m12(:,2) = m(2:3,3)
	m13 = m(2:3,1:2)
	
!Finding determinant of minor matrices using function	
	d11 = det(m11)
	d12 = det(m12)
	d13 = det(m13)
	
!calculating determinant of main 3x3 matrix	
	d = ((m(1,1))*d11)-((m(1,2))*d12)+((m(1,3))*d13)

!Displaying determinant in output
	open(unit = 2, file = "P13output.txt", status = "replace", action = "write")
		write (2, *) "The determinant of the matrix is ", d
	close(2)
end program P13