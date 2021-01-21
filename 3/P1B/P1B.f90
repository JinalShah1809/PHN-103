program P1B
implicit none 

real :: x1, y1, x2, y2, z1, z2, d, l, m, n

!Reading values from the input file
	open (unit=1, file="P1Binput.dat", status="old", action="read")
		read (1,*) x1, y1, z1
		read (1,*) x2, y2, z2
	close (1)

!Applying standard equations
	d = sqrt((x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2 ) !d = distance between points S and T
	l = (x2-x1)/d !l = direction cosine along X axis
	m = (y2-y1)/d !m = direction cosine along Y axis
	n = (z2-z1)/d !n = direction cosine along Z axis

!Displaying output
	open (unit=2, file="P1Boutput.txt", status="replace", action="write")
		write (2,3) "The distance between S (", x1, ",", y1, ",", z1, ") and T (", x2, ",",y2,",",z2, ") is ", d
			3 format (a, f4.2, a, f4.2, a, f4.2, a, f4.2, a, f4.2, a, f4.2, a, f10.7)
		write (2,4) "The direction cosine of ST along X axis is", l
		write (2,4) "The direction cosine of ST along Y axis is", m
		write (2,4) "The direction cosine of ST along Z axis is", n
			4 format (a, f9.6)
	close (2)
	
end program P1B
