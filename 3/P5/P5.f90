program P5
implicit none

        integer :: i, j, n, flag
		real :: sum, product, am, gm
		real, dimension(:), allocatable :: a
		
        sum = 0
        product = 1
        
!Reading from input file		
        open(unit = 1, file = "P5input.dat", status = 'old', action = "read")
            
			read (1,*) n !Reading number of inputs 
			allocate(a(n))
			
			i = 1
			do while (i <= n) !Reading elements from file into array 
				read (1,*) a(i)
				i=i+1
			end do
        close(1)
		
!Checking inputs 
		i = 1
		flag = 0 
		do while (i <= n)
			if (a(i) < 0) then
				flag = 1 !Used to indicate negative input
			exit !Terminating loop on negative user input
			
			else
				i = i+1
				
			end if
		end do

!Displaying output		
		open(unit=2 , file="P5output.txt", status = "replace", action = "write")
		
		if (flag == 1) then
			write (2, *) "You cannot input negative number."
		
		else
			j = 1
			do while (j <= n) !loop to find sum and product
				sum = sum + a(j)
				product = product*(a(j))
				j = j+1
			end do
					
			am = sum / n !Arithmetic Mean
			gm = (product) ** (1.0/ n) !Geometric Mean
		
			write (2, *) "The arithmetic mean is", am
			write (2, *) "The geometric mean is", gm
			
        end if
		close (2)	
		
			deallocate(a)
			
end program P5