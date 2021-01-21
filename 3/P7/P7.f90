program P7
implicit none

	integer :: i, j
	integer, dimension(1:3) :: n
	real :: term
	real, dimension(1:3) :: sum
	
!Initializing variables
	term = 1
	sum = 0

!Reading from input file
	open(unit=1, file= "P7input.dat", status= "old", action="read")
		read(1,*) n
	close (1)
	
	do i= 1, 3
		do j= 1,(n(i)+1)
			term = ((-1)**(real(j)+1))/((2*real(j)-1)) !General term of series
			sum(i)=sum(i)+term !sum of series
		end do
	end do 

!Displaying output
	open(unit=2, file= "P7output.txt", status= "replace", action="write")
		do i = 1, 3
			write (2, 3) "Sum of series up to", n(i), "terms is", sum(i)  
				3 format (a20, i5, a10, f13.9)
		end do
	close (2)
	
end program P7