fpm test --compiler gfortran --flag "-mtune=generic -march=x86-64 -g -Wall -Wextra -Werror=implicit-interface -fPIC -fmax-errors=1 -fbounds-check -fcheck=array-temps -fbacktrace -fcoarray=single -fimplicit-none -ffree-form"
fpm test --compiler gfortran --flag "-O3"

fpm test --compiler ifx --flag "-stand f18 -warn all -check all -check nouninit -error-limit 1 -O0 -g -traceback -assume byterecl"
fpm test --compiler ifx --flag "-stand f18 -O3"

fpm test --compiler ifort --flag "-stand f18 -warn all -check all -error-limit 1 -O0 -g -traceback -assume byterecl"
fpm test --compiler ifort --flag "-stand f18 -O3"
