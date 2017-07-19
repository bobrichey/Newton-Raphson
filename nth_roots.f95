! NAME: Bob Richey
!
! CLASS: CSC 540
!
! ASSIGNMENT: Nth Roots
!
! FILE NAME: nth_roots.f95
!
! DATE: March 14th, 2016
!
! DESCRIPTION: implements the Newton-Raphson method to approximate the
! various roots of an array of values
!
program nth_roots
	implicit none
	
	integer, parameter :: values_length = 4
	integer, parameter :: roots_length = 4
	real, dimension(values_length) :: values
	integer, dimension(roots_length) :: roots
	integer :: i
	
 	values = (/33.0, 2255.0, 148.0, 5231.23/)
	roots = (/3, 4, 5, 6/)
		
	do i = 1, values_length
		call find_nth_root(values(i), roots(i))
	end do
end program nth_roots

!Finds the approximate nth root of a given value
subroutine find_nth_root(value, root)
	implicit none
	
	real, parameter :: limit = 0.0005
	real :: value, guess, next_guess, delta
	integer :: root, count
		
	guess = 1.0
	next_guess = 0
	count = 0

	do while (ABS(guess - next_guess) > limit)
		if (count > 0) then
			guess = next_guess
		end if
		next_guess = guess - delta(guess, value, root)
		count = count + 1
	end do
	
	print *, root,'root',value,'=', next_guess
	print *, 'Number of guesses made:', count
	print *, ''
end subroutine find_nth_root

!Calculates the approximate distance between a guess and the nth root of a value
function delta(guess, value, root)
	implicit none

	real :: guess, value, delta
	integer :: root

	delta = 1.0 / root * (guess - (value / guess**(root - 1)));
end function delta
