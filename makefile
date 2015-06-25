CMP = ifort
XFLAGS = -O -xHost -fpp
INC = -I /apps/jasper/1.900.1/include -I $(NETCDF_ROOT)/include
LIBS = -L /cs/datastore/csdar/tha051/src/g2nlib/g2lib-1.4.0 -L /apps/jasper/1.900.1/lib -L $(NETCDF_ROOT)/lib -lg2 -lnetcdf -lnetcdff -ljasper -lpng

OBJ = g2n.o readswitch.o ncwrite.o grib2read.o grib2meta.o splice.o misc.o \
      netcdf_m.o

g2n : $(OBJ)
	$(CMP) $(XFLAGS) $(OBJ) $(LIBS) -o g2n

clean:
	rm -f *.o core


.SUFFIXES:.f90

.f90.o:
	$(CMP) -c $(XFLAGS) $(INC) $<

g2n.o : netcdf_m.o
ncwrite.o : netcdf_m.o
