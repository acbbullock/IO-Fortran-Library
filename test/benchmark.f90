program main
	use, intrinsic :: iso_fortran_env, only: int64, rk=>real64, dp=>real64, compiler_version, compiler_options
	use io_fortran_lib, only: String, str, LF, operator(+)
	implicit none (type,external)

	type(String) :: csv
	type(String), allocatable, dimension(:,:) :: cells

	integer(int64) :: t1, t2
	real(dp) :: wall_time, rate

	integer, parameter :: n = 10000
	real(rk), allocatable, dimension(:,:) :: x, y

	allocate( x(n,n) ); call random_gauss(x,0.0_rk,1.0_rk)
	write(*,'(a)')	'Compiler version: ' + compiler_version()
	write(*,'(a)')	'Compiler options: ' + compiler_options() + LF

	call system_clock(t1)
	cells = String(x, fmt='e')
	call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

	write(*,'(a)')	'Wall time for String: ' + str(wall_time, fmt='f', decimals=3) + ' s'
	write(*,'(a)')	'Number of string conversions/second: ' + str(nint(size(x)/wall_time)) + LF

	call system_clock(t1)
	call csv%write_file(cells, file_name='bigx.csv')
	call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

	write(*,'(a)')	'Wall time for write_file: ' + str(wall_time, fmt='f', decimals=3) + ' s'
	write(*,'(a)')	'Estimated file size: ' + str(csv%len64()/1e9, fmt='f', decimals=6) + ' GB' + LF

	call system_clock(t1)
	call csv%read_file('bigx.csv', cell_array=cells)
	call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

	write(*,'(a)')	'Wall time for read_file: ' + str(wall_time, fmt='f', decimals=3) + ' s' + LF

	call csv%empty(); allocate( y(n,n) )

	call system_clock(t1)
	call cells%cast(into=y, fmt='e')
	call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

	write(*,'(a)')	'Wall time for cast: ' + str(wall_time, fmt='f', decimals=3) + ' s'
	write(*,'(a)')	'Number of string casts/second: ' + str(nint(size(x)/wall_time))
	write(*,'(a,l)')'Data is exact match: ', all(x == y)

	contains
	impure real(rk) function gauss(mu, sig) result(gauss_res)
		!!-------------------------------------------------------------------------------------------------------------
		!! Samples random numbers from the standard Normal (Gaussian) Distribution with the given mean and sigma.
		!! Uses the Acceptance-complement ratio from W. Hoermann and G. Derflinger.
		!! This is one of the fastest existing methods for generating normal random variables.
		!!
		!! REFERENCE:  - W. Hoermann and G. Derflinger (1990):
		!!              The ACR Method for generating normal random variables,
		!!              OR Spektrum 12 (1990), 181-185.
		!!
		!! Implementation taken from <https://root.cern.ch/doc/master/TRandom_8cxx_source.html#l00274>
		!! UNURAN (c) 2000  W. Hoermann & J. Leydold, Institut f. Statistik, WU Wien
		!!-------------------------------------------------------------------------------------------------------------
		real(rk), intent(in) :: mu, sig

		real(rk) :: kC1, kC2, kC3, kD1, kD2, kD3, kHm, kZm, kHp, kZp, kPhln, kHm1
		real(rk) :: kHp1, kHzm, kHzmp, kAs, kBs, kCs, kB, kX0, kYm, kS, kT
		real(rk) :: rn, x, y, z, res
		integer :: i, j

		kC1   = 1.448242853_rk
		kC2   = 3.307147487_rk
		kC3   = 1.46754004_rk
		kD1   = 1.036467755_rk
		kD2   = 5.295844968_rk
		kD3   = 3.631288474_rk
		kHm   = 0.483941449_rk
		kZm   = 0.107981933_rk
		kHp   = 4.132731354_rk
		kZp   = 18.52161694_rk
		kPhln = 0.4515827053_rk
		kHm1  = 0.516058551_rk
		kHp1  = 3.132731354_rk
		kHzm  = 0.375959516_rk
		kHzmp = 0.591923442_rk
		
		kAs	= 0.8853395638_rk
		kBs	= 0.2452635696_rk
		kCs	= 0.2770276848_rk
		kB	= 0.5029324303_rk
		kX0	= 0.4571828819_rk
		kYm	= 0.187308492_rk
		kS	= 0.7270572718_rk
		kT	= 0.03895759111_rk

		outer: do
			call random_number(y)

			if ( y > kHm1 ) then
				res = kHp*y - kHp1; exit outer
			else if ( y < kZm ) then
				rn = kZp*y - 1.0_rk

				if ( rn > 0.0_rk ) then
					res = 1.0_rk + rn; exit outer
				else
					res = -1.0_rk + rn; exit outer
				end if
			else if ( y < kHm ) then
				call random_number(rn)
				rn = rn - 1.0_rk + rn

				if ( rn > 0.0_rk ) then
					z = 2.0_rk - rn
				else
					z = -2.0_rk - rn
				end if

				if ( ((kC1-y)*(kC3+abs(z))) < kC2 ) then
					res = z; exit outer
				else
					x = rn*rn
					if ( ((y+kD1)*(kD3+x)) < kD2 ) then
						res = rn; exit outer
					else if ( (kHzmp-y) < exp(-(z*z+kPhln)/2) ) then
						res = z; exit outer
					else if ( (y+kHzm) < exp(-(x+kPhln)/2) ) then
						res = rn; exit outer
					end if
				end if
			end if

			inner: do
				call random_number(x)
				call random_number(y)
				y = kYm*y
				z = kX0 - kS*x - y

				if ( z > 0.0_rk ) then
					rn = 2.0_rk + y/x
				else
					x = 1.0_rk - x
					y = kYm - y
					rn = -(2.0_rk + y/x)
				end if

				if ( ((y-kAs+x)*(kCs+x)+kBs) < 0.0_rk ) then
					res = rn; exit inner
				else if ( y < (x+kT) ) then
					if ( (rn*rn) < (4.0_rk*(kB-log(x))) ) then
						res = rn; exit inner
					end if
				end if
			end do inner
		end do outer

		gauss_res = res*sig + mu
	end function gauss

	impure elemental subroutine random_gauss(x, mu, sig)
		real(rk), intent(inout) :: x
		real(rk), intent(in) :: mu, sig
		x = gauss(mu, sig)
	end subroutine random_gauss
end program main
