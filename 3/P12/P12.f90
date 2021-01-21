module multiplication
contains

subroutine multiply(a,b,r1,c1,r2,c2,t1) 
implicit none
	integer :: i, j, k
	real :: sum
	integer :: r1, c1, r2, c2
	real, dimension(:,:) :: a, b
	real, dimension(:,:) :: t1
	
	sum = 0
	do i =1,r1
		do j = 1,c2
			do k = 1,c1
				sum = sum + (a(i,k) * b(k,j))
			end do
			t1(i,j) = sum 
			sum = 0
		end do
	end do
end subroutine multiply
end module multiplication

program P12
use multiplication
implicit none 
			
	integer :: row1, col1, row2, col2, row3, col3, i, j, k
	real, dimension(:,:), allocatable :: m1, m2, m3
	real, dimension(:,:), allocatable :: m4, m5
			
	!Reading from input files
		!Matrix A
		open(unit = 1, file = "P12input1.dat", status = "old", action = "read")
			read (1,*) row1, col1
			allocate(m1(row1, col1)) !Matrix A
				
			do i = 1,row1
				read(1, *, end=10) (m1(i,j), j = 1, col1)
			end do
		10 close(1)
			
		!Matrix B
		open(unit = 2, file = "P12input2.dat", status = "old", action = "read")
			read (2,*) row2, col2
			allocate(m2(row2, col2)) !Matrix B
			
			do i = 1,row2
				read(2, *, end=20) (m2(i,j), j = 1, col2)
			end do
		20 close(2)
			
		!Matrix C
			open(unit = 3, file = "P12input3.dat", status = "old", action = "read")
			read (3,*) row3, col3
			allocate(m3(row3, col3)) !Matrix C
			
			do i = 1,row3
				read(3, *, end=30) (m3(i,j), j = 1, col3)
			end do
		30 close(3)			
			
	!Displaying output file	
		open(unit = 4, file = "P12output.txt", status = "replace", action = "write")
			if (col1 /= row2 .or. col2 /= row3 ) then
				write (4,*) "Invalid input : Matrices incompatible."
			else
				allocate(m4(row1, col2)) !Matrix A*B
				call multiply(m1,m2,row1,col1,row2,col2,m4)
				
				allocate(m5(row1, col3)) !Matrix D
				call multiply(m4,m3,row1,col2,row3,col3,m5)				
			
			!Printing elements of Matrix D
				write(4, *) "The resulting product matrix D is" 	
				do i = 1, row1
					write(4,*) (m5(i,j), j = 1,col3)					
				end do	
			end if
		close(4)
		
		deallocate(m1)
		deallocate(m2)
		deallocate(m3)
		deallocate(m4)
		deallocate(m5)
			
end program P12