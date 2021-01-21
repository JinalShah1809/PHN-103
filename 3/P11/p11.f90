program P11
implicit none

	integer :: i, j, k
	real :: sum, T1, T2
	real, dimension(6,6):: m1, m2, m3
	
	sum = 0.0
	t1 = 0.0
	t2 = 0.0

!Reading from input files	
	open(unit = 1, file = "P11input1.dat", status = "old", action = "read")
		do i = 1,6
			read(1, *, end=10) (m1(i,j), j = 1, 6) !matrix 1
		end do
	10 close(1)
	
	open(unit = 2, file = "P11input2.dat", status = "old", action = "read")	
		do i = 1,6
			read(2, *, end=20) (m2(i,j), j = 1, 6) !matrix 2
		end do
	20 close(2)
	
!Calculating sum and trace  	
		do i = 1,6
			do j =1,6
			
			!sum
			sum = m1(i,j) + m2(i,j)
			m3(i,j) = sum !Sum matrix
			sum = 0
			
			!trace
			if (i == j) then
				T1 = T1 + m1(i,j) !trace of matrix 1
				T2 = T2 + m2(i,j) !trace of matrix 1
			end if
				
			end do
		end do
		
!Displaying output file	
	open(unit = 3, file = "P11output.txt", status = "replace", action = "write")
		write(3, *) "The resulting sum matrix is" 	
			do i =1, 6
				write(3,*) (m3(i,j), j = 1, 6)
			end do	
			
		write(3, 30) "Trace of Matrix 1 is", T1
		write(3, 30) "Trace of Matrix 2 is", T2
			30 format (a20, e13.5) !E format 
	close(3)
	
end program P11