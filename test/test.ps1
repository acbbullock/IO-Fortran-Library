fpm clean --all
fpm test --compiler gfortran --flag "-std=f2018 -mtune=generic -march=x86-64 -g -Wall -Wextra -Werror=implicit-interface -fPIC -fmax-errors=1 -fbounds-check -fcheck=array-temps -fbacktrace -fcoarray=single -fimplicit-none -ffree-form -cpp -D R4 -D I1"
fpm test --compiler gfortran --flag "-std=f2018 -mtune=generic -march=x86-64 -g -Wall -Wextra -Werror=implicit-interface -fPIC -fmax-errors=1 -fbounds-check -fcheck=array-temps -fbacktrace -fcoarray=single -fimplicit-none -ffree-form -cpp -D R4 -D I2"
fpm test --compiler gfortran --flag "-std=f2018 -mtune=generic -march=x86-64 -g -Wall -Wextra -Werror=implicit-interface -fPIC -fmax-errors=1 -fbounds-check -fcheck=array-temps -fbacktrace -fcoarray=single -fimplicit-none -ffree-form -cpp -D R8 -D I4"
fpm test --compiler gfortran --flag "-std=f2018 -mtune=generic -march=x86-64 -g -Wall -Wextra -Werror=implicit-interface -fPIC -fmax-errors=1 -fbounds-check -fcheck=array-temps -fbacktrace -fcoarray=single -fimplicit-none -ffree-form -cpp -D R16 -D I8"

fpm clean --all
fpm test --compiler gfortran --flag "-std=f2018 -O3 -cpp -D R4 -D I1"
fpm test --compiler gfortran --flag "-std=f2018 -O3 -cpp -D R4 -D I2"
fpm test --compiler gfortran --flag "-std=f2018 -O3 -cpp -D R8 -D I4"
fpm test --compiler gfortran --flag "-std=f2018 -O3 -cpp -D R16 -D I8"

fpm clean --all
fpm test --compiler ifx --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R4 /define:I1"
fpm test --compiler ifx --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R4 /define:I2"
fpm test --compiler ifx --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R8 /define:I4"
fpm test --compiler ifx --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R16 /define:I8"

fpm clean --all
fpm test --compiler ifx --flag "/stand:f18 /O3 /fpp /define:R4 /define:I1"
fpm test --compiler ifx --flag "/stand:f18 /O3 /fpp /define:R4 /define:I2"
fpm test --compiler ifx --flag "/stand:f18 /O3 /fpp /define:R8 /define:I4"
fpm test --compiler ifx --flag "/stand:f18 /O3 /fpp /define:R16 /define:I8"

fpm clean --all
fpm test --compiler ifort --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R4 /define:I1"
fpm test --compiler ifort --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R4 /define:I2"
fpm test --compiler ifort --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R8 /define:I4"
fpm test --compiler ifort --flag "/stand:f18 /warn:all /check:all /error-limit:1 /Od /Z7 /traceback /assume:byterecl /fpp /define:R16 /define:I8"

fpm clean --all
fpm test --compiler ifort --flag "/stand:f18 /O3 /fpp /define:R4 /define:I1"
fpm test --compiler ifort --flag "/stand:f18 /O3 /fpp /define:R4 /define:I2"
fpm test --compiler ifort --flag "/stand:f18 /O3 /fpp /define:R8 /define:I4"
fpm test --compiler ifort --flag "/stand:f18 /O3 /fpp /define:R16 /define:I8"

fpm clean --all
