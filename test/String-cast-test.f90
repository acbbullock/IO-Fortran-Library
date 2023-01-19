program main
	use, intrinsic :: iso_fortran_env, only: ik=>int64, rk=>real32, compiler_version, compiler_options
	use io_fortran_lib
	implicit none (type,external)

	real(rk), parameter :: tol = epsilon(1.0_rk)
	integer, parameter :: n = 2000

	type(String) :: logmsg
	character(len=:), allocatable :: logfile
	character(len=10) :: date, time
	logical :: all_passing

	call random_init(repeatable=.false., image_distinct=.true.)

	logfile = './test/tests.log'
	call date_and_time(date=date, time=time)

	logmsg = String('RUNNING TESTS (String|cast) - date: ' + trim(adjustl(date)) + ' | time: ' + time + LF)
	call logmsg%push('-'**logmsg%len() + LF)

	all_passing = .true.
	write(*,'(a)') logmsg%as_str()

	test_int: block
		real(rk), dimension(n) :: x
		integer(ik), dimension(n) :: i, j

		call random_gauss(x,0.0_rk,1.0_rk); i = floor(2147483647*x, ik) + 1_ik
		call cast(String(i, fmt='i'), into=j, fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 1: SUCCESS'
		else
			write(*,*) 'int 1: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); i = floor(2147483647*x, ik) + 1_ik
		call cast(String(i, fmt='z'), into=j, fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 2: SUCCESS'
		else
			write(*,*) 'int 2: FAILURE'
			all_passing = .false.
		end if
	end block test_int

	test_real: block
		real(rk), dimension(n) :: x, y

		call random_gauss(x,0.0_rk,1.0_rk)
		call cast(String(x, locale='US', fmt='e'), into=y, locale='US', fmt='e')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 1: SUCCESS'
		else
			write(*,*) 'real 1: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call cast(String(x, locale='US', fmt='f'), into=y, locale='US', fmt='f')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 2: SUCCESS'
		else
			write(*,*) 'real 2: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call cast(String(x, locale='US', fmt='z'), into=y, locale='US', fmt='z')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 3: SUCCESS'
		else
			write(*,*) 'real 3: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call cast(String(x, locale='EU', fmt='e'), into=y, locale='EU', fmt='e')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 4: SUCCESS'
		else
			write(*,*) 'real 4: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call cast(String(x, locale='EU', fmt='f'), into=y, locale='EU', fmt='f')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 5: SUCCESS'
		else
			write(*,*) 'real 5: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call cast(String(x, locale='EU', fmt='z'), into=y, locale='EU', fmt='z')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 6: SUCCESS'
		else
			write(*,*) 'real 6: FAILURE'
			all_passing = .false.
		end if
	end block test_real

	test_complex: block
		real(rk), dimension(n) :: x, y
		complex(rk), dimension(n) :: z1, z2

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='e', im=''), into=z2, locale='US', fmt='e', im='')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 1: SUCCESS'
		else
			write(*,*) 'complex 1: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='f', im=''), into=z2, locale='US', fmt='f', im='')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 2: SUCCESS'
		else
			write(*,*) 'complex 2: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='z', im=''), into=z2, locale='US', fmt='z', im='')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 3: SUCCESS'
		else
			write(*,*) 'complex 3: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='e', im='j'), into=z2, locale='US', fmt='e', im='j')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 4: SUCCESS'
		else
			write(*,*) 'complex 4: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='f', im='j'), into=z2, locale='US', fmt='f', im='j')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 5: SUCCESS'
		else
			write(*,*) 'complex 5: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='z', im='j'), into=z2, locale='US', fmt='z', im='j')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 6: SUCCESS'
		else
			write(*,*) 'complex 6: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='e', im='*1i'), into=z2, locale='US', fmt='e', im='*1i')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 7: SUCCESS'
		else
			write(*,*) 'complex 7: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='f', im='*1i'), into=z2, locale='US', fmt='f', im='*1i')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 8: SUCCESS'
		else
			write(*,*) 'complex 8: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='US', fmt='z', im='*1i'), into=z2, locale='US', fmt='z', im='*1i')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 9: SUCCESS'
		else
			write(*,*) 'complex 9: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='e', im=''), into=z2, locale='EU', fmt='e', im='')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 10: SUCCESS'
		else
			write(*,*) 'complex 10: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='f', im=''), into=z2, locale='EU', fmt='f', im='')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 11: SUCCESS'
		else
			write(*,*) 'complex 11: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='z', im=''), into=z2, locale='EU', fmt='z', im='')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 12: SUCCESS'
		else
			write(*,*) 'complex 12: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='e', im='j'), into=z2, locale='EU', fmt='e', im='j')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 13: SUCCESS'
		else
			write(*,*) 'complex 13: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='f', im='j'), into=z2, locale='EU', fmt='f', im='j')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 14: SUCCESS'
		else
			write(*,*) 'complex 14: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='z', im='j'), into=z2, locale='EU', fmt='z', im='j')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 15: SUCCESS'
		else
			write(*,*) 'complex 15: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='e', im='*1i'), into=z2, locale='EU', fmt='e', im='*1i')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 16: SUCCESS'
		else
			write(*,*) 'complex 16: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='f', im='*1i'), into=z2, locale='EU', fmt='f', im='*1i')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 17: SUCCESS'
		else
			write(*,*) 'complex 17: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
		call cast(String(z1, locale='EU', fmt='z', im='*1i'), into=z2, locale='EU', fmt='z', im='*1i')
		if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
			write(*,*) 'complex 18: SUCCESS'
		else
			write(*,*) 'complex 18: FAILURE'
			all_passing = .false.
		end if
	end block test_complex

	if ( all_passing ) then
		call logmsg%push('All tests are "PASSING" with compiler "' + compiler_version() + '" ' +  &
						 'using compiler options "' + compiler_options() + '".' + LF)
	else
		call logmsg%push('Some tests are "FAILING" with compiler "' + compiler_version() + '" ' +  &
						 'using compiler options "' + compiler_options() + '".' + LF)
	end if

	call logmsg%echo(logfile)

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
