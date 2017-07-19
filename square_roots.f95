! NAME: Bob Richey
!
! CLASS: CSC 540
!
! ASSIGNMENT: Nth Roots
!
! FILE NAME: square_roots.f95
!
! DATE: March 14th, 2016
!
! DESCRIPTION: implements the Newton-Raphson method to approximate the
! square roots of an array of values
!
program square_roots
	implicit none
	
	integer, parameter :: values_length = 6
	real, dimension(values_length) :: values
	integer :: i
	
	values = (/0.5, 1.0, 100.0, 150.00, 5000.0, 62632.52/)
	
	do i = 1, values_length
		call find_square_root(values(i))
	end do
end program square_roots

!Finds the approximate square root of a given value
subroutine find_square_root(value)
	implicit none
	
	real, parameter :: limit = 0.0005
	real :: value, guess, next_guess, delta
	integer :: count
		
	guess = 1.0
	next_guess = 0
	count = 0

	do while (ABS(guess - next_guess) > limit)
		if (count > 0) then
			guess = next_guess
		end if
		next_guess = guess - delta(guess, value)		
		count = count + 1
	end do
	
	print *, 'The square root of',value,'=', next_guess
	print *, 'Number of guesses made:', count
	print *, ''
end subroutine find_square_root

!Calculates the approximate distance between a guess and the square root of a value
function delta(guess, value)
	implicit none

	integer, parameter :: root = 2
	real :: guess, value, delta

	delta = 1.0 / root * (guess - (value / guess**(root - 1)));
end function delta
