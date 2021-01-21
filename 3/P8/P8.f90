program P8
implicit none 
	
integer :: row1, col1, row2, col2, i, j, k, sum
real, dimension(:,:), allocatable :: m1, m2, m3
	
!Reading from input files
	!Reading matrix A 
	open(unit = 1, file = "P8input1.dat", status = "old", action = "read")
		read (1,*) row1, col1
		allocate(m1(row1, col1)) !Matrix A
		
		do i = 1,row1
			read(1, *, end=10) (m1(i,j), j = 1, col1)
		end do
	10 close(1)
	
	!Reading matrix B
	open(unit = 2, file = "P8input2.dat", status = "old", action = "read")
		read (2,*) row2, col2
		allocate(m2(row2, col2)) !Matrix B
		
		do i = 1,row2
			read(2, *, end=20) (m2(i,j), j = 1, col2)
		end do
	20 close(2)
	
	
!Displaying output file	
	open(unit = 3, file = "P8output.txt", status = "replace", action = "write")
		if (col1 /= row2) then
			write (3,*) "Invalid input : Matrices incompatible." !Error message
		else
			allocate(m3(row1,col2)) !Product Matrix
			sum = 0
				do i =1,row1
					do j = 1,col2
						do k = 1,col1
							sum = sum + (m1(i,k)*m2(k,j))
						end do
						m3(i,j) = sum 
						sum = 0
					end do
				end do
	
	write(3, *) "The resulting product matrix is" 	
			do i = 1, row1
				write(3,*) (m3(i,j), j = 1, col2)			
			end do	
		end if
	close (3)
	
	deallocate(m1)
	deallocate(m2)
			
end program P8