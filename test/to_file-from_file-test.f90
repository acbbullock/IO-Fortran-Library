program main
	use, intrinsic :: iso_fortran_env, only: ik=>int64, rk=>real128, compiler_version, compiler_options
	use io_fortran_lib
	implicit none (type,external)

	real(rk), parameter :: tol = epsilon(1.0_rk)
	integer, parameter :: rows = 100
	integer, parameter :: cols = 20

	type(String) :: logmsg
	character(len=:), allocatable :: logfile
	character(len=10) :: date, time
	logical :: all_passing

	call random_init(repeatable=.false., image_distinct=.true.)

	logfile = './test/tests.log'
	call date_and_time(date=date, time=time)

	logmsg = String('RUNNING TESTS (to_file|from_file) - date: ' + trim(adjustl(date)) + ' | time: ' + time + LF)
	call logmsg%push('-'**logmsg%len() + LF)

	all_passing = .true.
	write(*,'(a)') logmsg%as_str()

	test_int: block
		real(rk), allocatable :: u(:), x(:,:)
		integer(ik), allocatable :: i(:), k(:,:)
		integer(ik), allocatable :: j(:), l(:,:)

		allocate( u(rows), i(rows) )

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=[''], dim=1, delim=',', fmt='i')
		call from_file('./data/i.csv', into=j, header=.false., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 1: SUCCESS'
		else
			write(*,*) 'int 1: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=[''], dim=1, delim=',', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.false., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 2: SUCCESS'
		else
			write(*,*) 'int 2: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=[''], dim=1, delim=';', fmt='i')
		call from_file('./data/i.csv', into=j, header=.false., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 3: SUCCESS'
		else
			write(*,*) 'int 3: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=[''], dim=1, delim=';', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.false., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 4: SUCCESS'
		else
			write(*,*) 'int 4: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=[''], dim=2, delim=',', fmt='i')
		call from_file('./data/i.csv', into=j, header=.false., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 5: SUCCESS'
		else
			write(*,*) 'int 5: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=[''], dim=2, delim=',', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.false., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 6: SUCCESS'
		else
			write(*,*) 'int 6: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=[''], dim=2, delim=';', fmt='i')
		call from_file('./data/i.csv', into=j, header=.false., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 7: SUCCESS'
		else
			write(*,*) 'int 7: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=[''], dim=2, delim=';', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.false., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 8: SUCCESS'
		else
			write(*,*) 'int 8: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=['i'], dim=1, delim=',', fmt='i')
		call from_file('./data/i.csv', into=j, header=.true., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 9: SUCCESS'
		else
			write(*,*) 'int 9: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=['i'], dim=1, delim=',', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.true., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 10: SUCCESS'
		else
			write(*,*) 'int 10: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=['i'], dim=1, delim=';', fmt='i')
		call from_file('./data/i.csv', into=j, header=.true., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 11: SUCCESS'
		else
			write(*,*) 'int 11: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=['i'], dim=1, delim=';', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.true., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 12: SUCCESS'
		else
			write(*,*) 'int 12: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=['i'], dim=2, delim=',', fmt='i')
		call from_file('./data/i.csv', into=j, header=.true., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 13: SUCCESS'
		else
			write(*,*) 'int 13: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=['i'], dim=2, delim=',', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.true., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 14: SUCCESS'
		else
			write(*,*) 'int 14: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i.csv', header=['i'], dim=2, delim=';', fmt='i')
		call from_file('./data/i.csv', into=j, header=.true., fmt='i')
		if ( all(i == j) ) then
			write(*,*) 'int 15: SUCCESS'
		else
			write(*,*) 'int 15: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); i = floor(2147483647*u, ik) + 1_ik
		call to_file(i, file_name='./data/i_z.csv', header=['i'], dim=2, delim=';', fmt='z')
		call from_file('./data/i_z.csv', into=j, header=.true., fmt='z')
		if ( all(i == j) ) then
			write(*,*) 'int 16: SUCCESS'
		else
			write(*,*) 'int 16: FAILURE'
			all_passing = .false.
		end if

		deallocate(u,i,j); allocate( x(rows,cols), k(rows,cols) )

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k.csv', header=[''], delim=',', fmt='i')
		call from_file('./data/k.csv', into=l, header=.false., fmt='i')
		if ( all(k == l) ) then
			write(*,*) 'int 17: SUCCESS'
		else
			write(*,*) 'int 17: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k_z.csv', header=[''], delim=',', fmt='z')
		call from_file('./data/k_z.csv', into=l, header=.false., fmt='z')
		if ( all(k == l) ) then
			write(*,*) 'int 18: SUCCESS'
		else
			write(*,*) 'int 18: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k.csv', header=[''], delim=';', fmt='i')
		call from_file('./data/k.csv', into=l, header=.false., fmt='i')
		if ( all(k == l) ) then
			write(*,*) 'int 19: SUCCESS'
		else
			write(*,*) 'int 19: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k_z.csv', header=[''], delim=';', fmt='z')
		call from_file('./data/k_z.csv', into=l, header=.false., fmt='z')
		if ( all(k == l) ) then
			write(*,*) 'int 20: SUCCESS'
		else
			write(*,*) 'int 20: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k.csv', header=['k'], delim=',', fmt='i')
		call from_file('./data/k.csv', into=l, header=.true., fmt='i')
		if ( all(k == l) ) then
			write(*,*) 'int 21: SUCCESS'
		else
			write(*,*) 'int 21: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k_z.csv', header=['k'], delim=',', fmt='z')
		call from_file('./data/k_z.csv', into=l, header=.true., fmt='z')
		if ( all(k == l) ) then
			write(*,*) 'int 22: SUCCESS'
		else
			write(*,*) 'int 22: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k.csv', header=['k'], delim=';', fmt='i')
		call from_file('./data/k.csv', into=l, header=.true., fmt='i')
		if ( all(k == l) ) then
			write(*,*) 'int 23: SUCCESS'
		else
			write(*,*) 'int 23: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); k = floor(2147483647*x, ik) + 1_ik
		call to_file(k, file_name='./data/k_z.csv', header=['k'], delim=';', fmt='z')
		call from_file('./data/k_z.csv', into=l, header=.true., fmt='z')
		if ( all(k == l) ) then
			write(*,*) 'int 24: SUCCESS'
		else
			write(*,*) 'int 24: FAILURE'
			all_passing = .false.
		end if
	end block test_int

	test_real: block
		real(rk), allocatable :: u(:), x(:,:)
		real(rk), allocatable :: v(:), y(:,:)

		allocate( u(rows) )

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=[''], dim=1, locale='EU', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.false., locale='EU', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 1: SUCCESS'
		else
			write(*,*) 'real 1: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=[''], dim=1, locale='EU', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.false., locale='EU', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 2: SUCCESS'
		else
			write(*,*) 'real 2: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=[''], dim=1, locale='EU', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.false., locale='EU', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 3: SUCCESS'
		else
			write(*,*) 'real 3: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=[''], dim=1, locale='US', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.false., locale='US', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 4: SUCCESS'
		else
			write(*,*) 'real 4: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=[''], dim=1, locale='US', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.false., locale='US', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 5: SUCCESS'
		else
			write(*,*) 'real 5: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=[''], dim=1, locale='US', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.false., locale='US', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 6: SUCCESS'
		else
			write(*,*) 'real 6: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=[''], dim=2, locale='EU', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.false., locale='EU', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 7: SUCCESS'
		else
			write(*,*) 'real 7: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=[''], dim=2, locale='EU', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.false., locale='EU', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 8: SUCCESS'
		else
			write(*,*) 'real 8: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=[''], dim=2, locale='EU', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.false., locale='EU', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 9: SUCCESS'
		else
			write(*,*) 'real 9: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=[''], dim=2, locale='US', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.false., locale='US', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 10: SUCCESS'
		else
			write(*,*) 'real 10: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=[''], dim=2, locale='US', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.false., locale='US', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 11: SUCCESS'
		else
			write(*,*) 'real 11: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=[''], dim=2, locale='US', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.false., locale='US', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 12: SUCCESS'
		else
			write(*,*) 'real 12: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=['u'], dim=1, locale='EU', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.true., locale='EU', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 13: SUCCESS'
		else
			write(*,*) 'real 13: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=['u'], dim=1, locale='EU', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.true., locale='EU', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 14: SUCCESS'
		else
			write(*,*) 'real 14: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=['u'], dim=1, locale='EU', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.true., locale='EU', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 15: SUCCESS'
		else
			write(*,*) 'real 15: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=['u'], dim=1, locale='US', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.true., locale='US', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 16: SUCCESS'
		else
			write(*,*) 'real 16: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=['u'], dim=1, locale='US', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.true., locale='US', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 17: SUCCESS'
		else
			write(*,*) 'real 17: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=['u'], dim=1, locale='US', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.true., locale='US', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 18: SUCCESS'
		else
			write(*,*) 'real 18: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=['u'], dim=2, locale='EU', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.true., locale='EU', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 19: SUCCESS'
		else
			write(*,*) 'real 19: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=['u'], dim=2, locale='EU', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.true., locale='EU', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 20: SUCCESS'
		else
			write(*,*) 'real 20: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=['u'], dim=2, locale='EU', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.true., locale='EU', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 21: SUCCESS'
		else
			write(*,*) 'real 21: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_e.csv', header=['u'], dim=2, locale='US', fmt='e')
		call from_file('./data/u_e.csv', into=v, header=.true., locale='US', fmt='e')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 22: SUCCESS'
		else
			write(*,*) 'real 22: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_f.csv', header=['u'], dim=2, locale='US', fmt='f')
		call from_file('./data/u_f.csv', into=v, header=.true., locale='US', fmt='f')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 23: SUCCESS'
		else
			write(*,*) 'real 23: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk)
		call to_file(u, file_name='./data/u_z.csv', header=['u'], dim=2, locale='US', fmt='z')
		call from_file('./data/u_z.csv', into=v, header=.true., locale='US', fmt='z')
		if ( maxval( abs(u-v)/abs(u) ) < tol ) then
			write(*,*) 'real 24: SUCCESS'
		else
			write(*,*) 'real 24: FAILURE'
			all_passing = .false.
		end if

		deallocate(u,v); allocate( x(rows,cols) )

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_e.csv', header=[''], locale='EU', fmt='e')
		call from_file('./data/x_e.csv', into=y, header=.false., locale='EU', fmt='e')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 25: SUCCESS'
		else
			write(*,*) 'real 25: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_f.csv', header=[''], locale='EU', fmt='f')
		call from_file('./data/x_f.csv', into=y, header=.false., locale='EU', fmt='f')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 26: SUCCESS'
		else
			write(*,*) 'real 26: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_z.csv', header=[''], locale='EU', fmt='z')
		call from_file('./data/x_z.csv', into=y, header=.false., locale='EU', fmt='z')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 27: SUCCESS'
		else
			write(*,*) 'real 27: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_e.csv', header=[''], locale='US', fmt='e')
		call from_file('./data/x_e.csv', into=y, header=.false., locale='US', fmt='e')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 28: SUCCESS'
		else
			write(*,*) 'real 28: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_f.csv', header=[''], locale='US', fmt='f')
		call from_file('./data/x_f.csv', into=y, header=.false., locale='US', fmt='f')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 29: SUCCESS'
		else
			write(*,*) 'real 29: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_z.csv', header=[''], locale='US', fmt='z')
		call from_file('./data/x_z.csv', into=y, header=.false., locale='US', fmt='z')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 30: SUCCESS'
		else
			write(*,*) 'real 30: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_e.csv', header=['x'], locale='EU', fmt='e')
		call from_file('./data/x_e.csv', into=y, header=.true., locale='EU', fmt='e')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 31: SUCCESS'
		else
			write(*,*) 'real 31: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_f.csv', header=['x'], locale='EU', fmt='f')
		call from_file('./data/x_f.csv', into=y, header=.true., locale='EU', fmt='f')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 32: SUCCESS'
		else
			write(*,*) 'real 32: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_z.csv', header=['x'], locale='EU', fmt='z')
		call from_file('./data/x_z.csv', into=y, header=.true., locale='EU', fmt='z')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 33: SUCCESS'
		else
			write(*,*) 'real 33: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_e.csv', header=['x'], locale='US', fmt='e')
		call from_file('./data/x_e.csv', into=y, header=.true., locale='US', fmt='e')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 34: SUCCESS'
		else
			write(*,*) 'real 34: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_f.csv', header=['x'], locale='US', fmt='f')
		call from_file('./data/x_f.csv', into=y, header=.true., locale='US', fmt='f')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 35: SUCCESS'
		else
			write(*,*) 'real 35: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk)
		call to_file(x, file_name='./data/x_z.csv', header=['x'], locale='US', fmt='z')
		call from_file('./data/x_z.csv', into=y, header=.true., locale='US', fmt='z')
		if ( maxval( abs(x-y)/abs(x) ) < tol ) then
			write(*,*) 'real 36: SUCCESS'
		else
			write(*,*) 'real 36: FAILURE'
			all_passing = .false.
		end if
	end block test_real

	test_complex: block
		real(rk), allocatable :: u(:), v(:), x(:,:), y(:,:)
		complex(rk), allocatable :: a(:), b(:), c(:,:), d(:,:)

		allocate( u(rows), v(rows) )

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=1, locale='EU', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='EU', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 1: SUCCESS'
		else
			write(*,*) 'complex 1: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=1, locale='EU', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='EU', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 2: SUCCESS'
		else
			write(*,*) 'complex 2: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=1, locale='EU', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='EU', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 3: SUCCESS'
		else
			write(*,*) 'complex 3: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=1, locale='US', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='US', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 4: SUCCESS'
		else
			write(*,*) 'complex 4: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=1, locale='US', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='US', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 5: SUCCESS'
		else
			write(*,*) 'complex 5: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=1, locale='US', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='US', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 6: SUCCESS'
		else
			write(*,*) 'complex 6: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=2, locale='EU', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='EU', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 7: SUCCESS'
		else
			write(*,*) 'complex 7: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=2, locale='EU', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='EU', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 8: SUCCESS'
		else
			write(*,*) 'complex 8: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=2, locale='EU', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='EU', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 9: SUCCESS'
		else
			write(*,*) 'complex 9: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=2, locale='US', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='US', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 10: SUCCESS'
		else
			write(*,*) 'complex 10: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=2, locale='US', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='US', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 11: SUCCESS'
		else
			write(*,*) 'complex 11: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=2, locale='US', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='US', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 12: SUCCESS'
		else
			write(*,*) 'complex 12: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=1, locale='EU', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='EU', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 13: SUCCESS'
		else
			write(*,*) 'complex 13: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=1, locale='EU', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='EU', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 14: SUCCESS'
		else
			write(*,*) 'complex 14: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=1, locale='EU', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='EU', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 15: SUCCESS'
		else
			write(*,*) 'complex 15: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=1, locale='US', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='US', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 16: SUCCESS'
		else
			write(*,*) 'complex 16: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=1, locale='US', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='US', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 17: SUCCESS'
		else
			write(*,*) 'complex 17: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=1, locale='US', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='US', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 18: SUCCESS'
		else
			write(*,*) 'complex 18: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=2, locale='EU', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='EU', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 19: SUCCESS'
		else
			write(*,*) 'complex 19: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=2, locale='EU', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='EU', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 20: SUCCESS'
		else
			write(*,*) 'complex 20: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=2, locale='EU', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='EU', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 21: SUCCESS'
		else
			write(*,*) 'complex 21: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=2, locale='US', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='US', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 22: SUCCESS'
		else
			write(*,*) 'complex 22: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=2, locale='US', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='US', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 23: SUCCESS'
		else
			write(*,*) 'complex 23: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=2, locale='US', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='US', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 24: SUCCESS'
		else
			write(*,*) 'complex 24: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=1, locale='EU', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='EU', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 25: SUCCESS'
		else
			write(*,*) 'complex 25: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=1, locale='EU', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='EU', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 26: SUCCESS'
		else
			write(*,*) 'complex 26: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=1, locale='EU', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='EU', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 27: SUCCESS'
		else
			write(*,*) 'complex 27: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=1, locale='US', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='US', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 28: SUCCESS'
		else
			write(*,*) 'complex 28: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=1, locale='US', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='US', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 29: SUCCESS'
		else
			write(*,*) 'complex 29: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=1, locale='US', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='US', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 30: SUCCESS'
		else
			write(*,*) 'complex 30: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=2, locale='EU', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='EU', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 31: SUCCESS'
		else
			write(*,*) 'complex 31: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=2, locale='EU', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='EU', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 32: SUCCESS'
		else
			write(*,*) 'complex 32: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=2, locale='EU', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='EU', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 33: SUCCESS'
		else
			write(*,*) 'complex 33: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=[''], dim=2, locale='US', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.false., locale='US', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 34: SUCCESS'
		else
			write(*,*) 'complex 34: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=[''], dim=2, locale='US', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.false., locale='US', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 35: SUCCESS'
		else
			write(*,*) 'complex 35: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=[''], dim=2, locale='US', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.false., locale='US', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 36: SUCCESS'
		else
			write(*,*) 'complex 36: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=1, locale='EU', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='EU', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 37: SUCCESS'
		else
			write(*,*) 'complex 37: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=1, locale='EU', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='EU', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 38: SUCCESS'
		else
			write(*,*) 'complex 38: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=1, locale='EU', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='EU', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 39: SUCCESS'
		else
			write(*,*) 'complex 39: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=1, locale='US', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='US', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 40: SUCCESS'
		else
			write(*,*) 'complex 40: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=1, locale='US', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='US', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 41: SUCCESS'
		else
			write(*,*) 'complex 41: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=1, locale='US', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='US', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 42: SUCCESS'
		else
			write(*,*) 'complex 42: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=2, locale='EU', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='EU', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 43: SUCCESS'
		else
			write(*,*) 'complex 43: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=2, locale='EU', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='EU', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 44: SUCCESS'
		else
			write(*,*) 'complex 44: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=2, locale='EU', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='EU', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 45: SUCCESS'
		else
			write(*,*) 'complex 45: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=2, locale='US', fmt='e', im='')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='US', fmt='e', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 46: SUCCESS'
		else
			write(*,*) 'complex 46: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=2, locale='US', fmt='f', im='')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='US', fmt='f', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 47: SUCCESS'
		else
			write(*,*) 'complex 47: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=2, locale='US', fmt='z', im='')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='US', fmt='z', im='')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 48: SUCCESS'
		else
			write(*,*) 'complex 48: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=1, locale='EU', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='EU', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 49: SUCCESS'
		else
			write(*,*) 'complex 49: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=1, locale='EU', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='EU', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 50: SUCCESS'
		else
			write(*,*) 'complex 50: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=1, locale='EU', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='EU', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 51: SUCCESS'
		else
			write(*,*) 'complex 51: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=1, locale='US', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='US', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 52: SUCCESS'
		else
			write(*,*) 'complex 52: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=1, locale='US', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='US', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 53: SUCCESS'
		else
			write(*,*) 'complex 53: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=1, locale='US', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='US', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 54: SUCCESS'
		else
			write(*,*) 'complex 54: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=2, locale='EU', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='EU', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 55: SUCCESS'
		else
			write(*,*) 'complex 55: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=2, locale='EU', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='EU', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 56: SUCCESS'
		else
			write(*,*) 'complex 56: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=2, locale='EU', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='EU', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 57: SUCCESS'
		else
			write(*,*) 'complex 57: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=2, locale='US', fmt='e', im='*1i')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='US', fmt='e', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 58: SUCCESS'
		else
			write(*,*) 'complex 58: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=2, locale='US', fmt='f', im='*1i')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='US', fmt='f', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 59: SUCCESS'
		else
			write(*,*) 'complex 59: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=2, locale='US', fmt='z', im='*1i')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='US', fmt='z', im='*1i')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 60: SUCCESS'
		else
			write(*,*) 'complex 60: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=1, locale='EU', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='EU', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 61: SUCCESS'
		else
			write(*,*) 'complex 61: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=1, locale='EU', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='EU', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 62: SUCCESS'
		else
			write(*,*) 'complex 62: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=1, locale='EU', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='EU', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 63: SUCCESS'
		else
			write(*,*) 'complex 63: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=1, locale='US', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='US', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 64: SUCCESS'
		else
			write(*,*) 'complex 64: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=1, locale='US', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='US', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 65: SUCCESS'
		else
			write(*,*) 'complex 65: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=1, locale='US', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='US', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 66: SUCCESS'
		else
			write(*,*) 'complex 66: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=2, locale='EU', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='EU', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 67: SUCCESS'
		else
			write(*,*) 'complex 67: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=2, locale='EU', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='EU', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 68: SUCCESS'
		else
			write(*,*) 'complex 68: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=2, locale='EU', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='EU', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 69: SUCCESS'
		else
			write(*,*) 'complex 69: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_e.csv', header=['a'], dim=2, locale='US', fmt='e', im='j')
		call from_file('./data/a_e.csv', into=b, header=.true., locale='US', fmt='e', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 70: SUCCESS'
		else
			write(*,*) 'complex 70: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_f.csv', header=['a'], dim=2, locale='US', fmt='f', im='j')
		call from_file('./data/a_f.csv', into=b, header=.true., locale='US', fmt='f', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 71: SUCCESS'
		else
			write(*,*) 'complex 71: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(u,0.0_rk,1.0_rk); call random_gauss(v,0.0_rk,1.0_rk); a = cmplx(u,v,rk)
		call to_file(a, file_name='./data/a_z.csv', header=['a'], dim=2, locale='US', fmt='z', im='j')
		call from_file('./data/a_z.csv', into=b, header=.true., locale='US', fmt='z', im='j')
		if ( maxval( abs(a-b)/abs(a) ) < tol ) then
			write(*,*) 'complex 72: SUCCESS'
		else
			write(*,*) 'complex 72: FAILURE'
			all_passing = .false.
		end if

		deallocate(u,v,a,b); allocate( x(rows,cols), y(rows,cols) )

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=[''], locale='EU', fmt='e', im='')
		call from_file('./data/c_e.csv', into=d, header=.false., locale='EU', fmt='e', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 73: SUCCESS'
		else
			write(*,*) 'complex 73: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=[''], locale='EU', fmt='f', im='')
		call from_file('./data/c_f.csv', into=d, header=.false., locale='EU', fmt='f', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 74: SUCCESS'
		else
			write(*,*) 'complex 74: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=[''], locale='EU', fmt='z', im='')
		call from_file('./data/c_z.csv', into=d, header=.false., locale='EU', fmt='z', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 75: SUCCESS'
		else
			write(*,*) 'complex 75: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=[''], locale='US', fmt='e', im='')
		call from_file('./data/c_e.csv', into=d, header=.false., locale='US', fmt='e', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 76: SUCCESS'
		else
			write(*,*) 'complex 76: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=[''], locale='US', fmt='f', im='')
		call from_file('./data/c_f.csv', into=d, header=.false., locale='US', fmt='f', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 77: SUCCESS'
		else
			write(*,*) 'complex 77: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=[''], locale='US', fmt='z', im='')
		call from_file('./data/c_z.csv', into=d, header=.false., locale='US', fmt='z', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 78: SUCCESS'
		else
			write(*,*) 'complex 78: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=[''], locale='EU', fmt='e', im='*1i')
		call from_file('./data/c_e.csv', into=d, header=.false., locale='EU', fmt='e', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 79: SUCCESS'
		else
			write(*,*) 'complex 79: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=[''], locale='EU', fmt='f', im='*1i')
		call from_file('./data/c_f.csv', into=d, header=.false., locale='EU', fmt='f', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 80: SUCCESS'
		else
			write(*,*) 'complex 80: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=[''], locale='EU', fmt='z', im='*1i')
		call from_file('./data/c_z.csv', into=d, header=.false., locale='EU', fmt='z', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 81: SUCCESS'
		else
			write(*,*) 'complex 81: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=[''], locale='US', fmt='e', im='*1i')
		call from_file('./data/c_e.csv', into=d, header=.false., locale='US', fmt='e', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 82: SUCCESS'
		else
			write(*,*) 'complex 82: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=[''], locale='US', fmt='f', im='*1i')
		call from_file('./data/c_f.csv', into=d, header=.false., locale='US', fmt='f', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 83: SUCCESS'
		else
			write(*,*) 'complex 83: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=[''], locale='US', fmt='z', im='*1i')
		call from_file('./data/c_z.csv', into=d, header=.false., locale='US', fmt='z', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 84: SUCCESS'
		else
			write(*,*) 'complex 84: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=[''], locale='EU', fmt='e', im='j')
		call from_file('./data/c_e.csv', into=d, header=.false., locale='EU', fmt='e', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 85: SUCCESS'
		else
			write(*,*) 'complex 85: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=[''], locale='EU', fmt='f', im='j')
		call from_file('./data/c_f.csv', into=d, header=.false., locale='EU', fmt='f', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 86: SUCCESS'
		else
			write(*,*) 'complex 86: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=[''], locale='EU', fmt='z', im='j')
		call from_file('./data/c_z.csv', into=d, header=.false., locale='EU', fmt='z', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 87: SUCCESS'
		else
			write(*,*) 'complex 87: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=[''], locale='US', fmt='e', im='j')
		call from_file('./data/c_e.csv', into=d, header=.false., locale='US', fmt='e', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 88: SUCCESS'
		else
			write(*,*) 'complex 88: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=[''], locale='US', fmt='f', im='j')
		call from_file('./data/c_f.csv', into=d, header=.false., locale='US', fmt='f', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 89: SUCCESS'
		else
			write(*,*) 'complex 89: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=[''], locale='US', fmt='z', im='j')
		call from_file('./data/c_z.csv', into=d, header=.false., locale='US', fmt='z', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 90: SUCCESS'
		else
			write(*,*) 'complex 90: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=['c'], locale='EU', fmt='e', im='')
		call from_file('./data/c_e.csv', into=d, header=.true., locale='EU', fmt='e', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 91: SUCCESS'
		else
			write(*,*) 'complex 91: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=['c'], locale='EU', fmt='f', im='')
		call from_file('./data/c_f.csv', into=d, header=.true., locale='EU', fmt='f', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 92: SUCCESS'
		else
			write(*,*) 'complex 92: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=['c'], locale='EU', fmt='z', im='')
		call from_file('./data/c_z.csv', into=d, header=.true., locale='EU', fmt='z', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 93: SUCCESS'
		else
			write(*,*) 'complex 93: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=['c'], locale='US', fmt='e', im='')
		call from_file('./data/c_e.csv', into=d, header=.true., locale='US', fmt='e', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 94: SUCCESS'
		else
			write(*,*) 'complex 94: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=['c'], locale='US', fmt='f', im='')
		call from_file('./data/c_f.csv', into=d, header=.true., locale='US', fmt='f', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 95: SUCCESS'
		else
			write(*,*) 'complex 95: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=['c'], locale='US', fmt='z', im='')
		call from_file('./data/c_z.csv', into=d, header=.true., locale='US', fmt='z', im='')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 96: SUCCESS'
		else
			write(*,*) 'complex 96: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=['c'], locale='EU', fmt='e', im='*1i')
		call from_file('./data/c_e.csv', into=d, header=.true., locale='EU', fmt='e', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 97: SUCCESS'
		else
			write(*,*) 'complex 97: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=['c'], locale='EU', fmt='f', im='*1i')
		call from_file('./data/c_f.csv', into=d, header=.true., locale='EU', fmt='f', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 98: SUCCESS'
		else
			write(*,*) 'complex 98: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=['c'], locale='EU', fmt='z', im='*1i')
		call from_file('./data/c_z.csv', into=d, header=.true., locale='EU', fmt='z', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 99: SUCCESS'
		else
			write(*,*) 'complex 99: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=['c'], locale='US', fmt='e', im='*1i')
		call from_file('./data/c_e.csv', into=d, header=.true., locale='US', fmt='e', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 100: SUCCESS'
		else
			write(*,*) 'complex 100: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=['c'], locale='US', fmt='f', im='*1i')
		call from_file('./data/c_f.csv', into=d, header=.true., locale='US', fmt='f', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 101: SUCCESS'
		else
			write(*,*) 'complex 101: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=['c'], locale='US', fmt='z', im='*1i')
		call from_file('./data/c_z.csv', into=d, header=.true., locale='US', fmt='z', im='*1i')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 102: SUCCESS'
		else
			write(*,*) 'complex 102: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=['c'], locale='EU', fmt='e', im='j')
		call from_file('./data/c_e.csv', into=d, header=.true., locale='EU', fmt='e', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 103: SUCCESS'
		else
			write(*,*) 'complex 103: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=['c'], locale='EU', fmt='f', im='j')
		call from_file('./data/c_f.csv', into=d, header=.true., locale='EU', fmt='f', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 104: SUCCESS'
		else
			write(*,*) 'complex 104: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=['c'], locale='EU', fmt='z', im='j')
		call from_file('./data/c_z.csv', into=d, header=.true., locale='EU', fmt='z', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 105: SUCCESS'
		else
			write(*,*) 'complex 105: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_e.csv', header=['c'], locale='US', fmt='e', im='j')
		call from_file('./data/c_e.csv', into=d, header=.true., locale='US', fmt='e', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 106: SUCCESS'
		else
			write(*,*) 'complex 106: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_f.csv', header=['c'], locale='US', fmt='f', im='j')
		call from_file('./data/c_f.csv', into=d, header=.true., locale='US', fmt='f', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 107: SUCCESS'
		else
			write(*,*) 'complex 107: FAILURE'
			all_passing = .false.
		end if

		call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); c = cmplx(x,y,rk)
		call to_file(c, file_name='./data/c_z.csv', header=['c'], locale='US', fmt='z', im='j')
		call from_file('./data/c_z.csv', into=d, header=.true., locale='US', fmt='z', im='j')
		if ( maxval( abs(c-d)/abs(c) ) < tol ) then
			write(*,*) 'complex 108: SUCCESS'
		else
			write(*,*) 'complex 108: FAILURE'
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
