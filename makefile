CMP = ifort
XFLAGS = -O
INC = -I/cs/datastore/u/csdar/tha051/lib -I /tools/netcdf/3.6.0-p1/include
LIBS = -L/cs/datastore/u/csdar/tha051/lib -L /tools/netcdf/3.6.0-p1/lib -lg2 -lnetcdf -ljasper -lpng

OBJ = g2n.o readswitch.o ncwrite.o grib2read.o grib2meta.o splice.o misc.o

g2n : $(OBJ)
	$(CMP) $(XFLAGS) $(OBJ) $(LIBS) -o g2n

clean:
	rm -f *.o core


.SUFFIXES:.f90

.f90.o:
	$(CMP) -c $(XFLAGS) $(INC) $<
grib2meta.o: grib2meta.f90
	$(CMP) -c -O -override-limits $(INC) $<
