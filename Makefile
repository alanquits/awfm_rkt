MAKE = mingw32-make

all: libawfm awfm

libawfm:
	cd c; mingw32-make; cd ..

awfm:
	cd rkt; mingw-make; cd ..
