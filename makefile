
ifneq ($(CUSTOM),yes)
FC = ifort
XFLAGS = -O -xHost
INC = -I $(JASPER_ROOT)/include -I $(G2LIB_ROOT) -I $(NETCDF_ROOT)/include
LIBS = -L $(G2LIB_ROOT) -L $(JASPER_ROOT)/lib -L $(NETCDF_ROOT)/lib -lg2 -lnetcdf -lnetcdff -ljasper -lpng
PPFLAG90 = -fpp
endif

ifeq ($(GFORTRAN),yes)
FC = gfortran
XFLAGS = -O2 -mtune=native -march=native
PPFLAG90 = -x f95-cpp-input
endif


OBJ = g2n.o readswitch.o ncwrite.o grib2read.o grib2meta.o splice.o misc.o \
      netcdf_m.o

g2n : $(OBJ)
	$(FC) $(XFLAGS) $(OBJ) $(LIBS) -o g2n

clean:
	rm -f *.o core *.mod


.SUFFIXES:.f90

.f90.o:
	$(FC) -c $(XFLAGS) $(INC) $(PPFLAG90) $<

g2n.o : netcdf_m.o
ncwrite.o : netcdf_m.o
