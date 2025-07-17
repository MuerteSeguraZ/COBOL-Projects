# Makefile for MicroDB Fortran program

FC = gfortran
FFLAGS = -Wall -O2
TARGET = microdb.exe
SRC = microdb.f90

all: $(TARGET)

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET) *.mod *.o
