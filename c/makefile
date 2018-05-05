F90 = gfortran
CC = gcc
FLAGS = -Wall -Werror -fpic
COBJECTS = awfm.o
FOBJECTS = slatec.o
OBJECTS = $(COBJECTS) $(FOBJECTS)

libawfm.dll: $(OBJECTS)
	$(CC) -shared -o libawfm.dll $(OBJECTS) -lgfortran
	copy libawfm.dll ..\bin\libawfm.dll

awfm.o: awfm.c awfm.h slatec.o
	$(CC) -c $(FLAGS) awfm.c

slatec.o: slatec.f
	$(F90) -c -fpic slatec.f

clean:
	del /S *.o
