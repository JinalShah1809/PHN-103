program P1A
implicit none 

real :: x1, y1, x2, y2, d 

!Reading values from the input file
	open (unit=1, file="P1Ainput.dat", status="old", action="read")
		read (1,*) x1, y1
		read (1,*) x2, y2
	close (1)

!Applying distance formula 
	d = sqrt((x1-x2)**2 + (y1-y2)**2)

!Displaying output
	open (unit=2, file="P1Aoutput.txt", status="replace", action="write")
		write (2,*) "The distance between the two given points is", d, "units."
	close (2)
	
end program P1A
