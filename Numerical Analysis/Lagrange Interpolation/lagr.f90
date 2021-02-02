program lagr
use numericalanalysis
implicit none


real :: xx, lagr
integer :: i,j,n
real, dimension (:), allocatable :: x, y
open(unit=1,file='lagrin.dat',status='old')
read(1,*)n,xx
print *, n, xx
allocate (x(n), y(n))
read(1,*)(x(i),y(i),i=1,n)

lagr = lagrange(x,y,n,xx)
open(unit=2,file='lagrout.txt',status='replace')
write (2,*) lagr
end program lagr