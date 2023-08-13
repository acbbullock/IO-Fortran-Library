fpm test --compiler gfortran
fpm test --compiler gfortran --flag "-O3"

fpm test --compiler ifx
fpm test --compiler ifx --flag "/O3 /stand:f18"

fpm test --compiler ifort
fpm test --compiler ifort --flag "/O3 /stand:f18"
