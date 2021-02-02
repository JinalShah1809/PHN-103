module numericalanalysis
contains

function lagrange(x,y,n,xx) result(lagr)
	implicit none
	real, dimension (:), allocatable :: x, y
	integer :: i,j,n
	real :: sum, product, lagr, xx
	sum = 0
		do i = 1,n
		product = y(i)
			do j = 1,n
				if (i/=j) then
				product = product*(xx-x(j))/(x(i)-x(j))
				end if
			end do
		sum = sum + product
		end do
	lagr = sum
end function lagrange

function trapezoid(h,n,f) result(trap)
	implicit none
	real, dimension (:), allocatable :: f
	integer :: i,n
	real :: sum, trap, h
	sum = f(1)
		do i = 2,n-1
		sum = sum + 2*f(i)
		end do
	sum = sum + f(n)
	trap = h * sum/2
end function trapezoid

function simp13(h,n,f)
	implicit none
	real, dimension (:), allocatable :: f
	integer :: i,n
	real :: sum, simp13, h
	sum = f(1)
		do i = 2,n-2,2
		sum = sum + 4*f(i) + 2*f(i+1)
		end do
	sum = sum + 4*f(n-1) + f(n)
	simp13 = h * sum/3
end function simp13

end module numericalanalysis