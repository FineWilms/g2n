Program g2n

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This code reads a GRIB file (using the g2 library)
! and converts it to NetCDF (using the netcdf library).

Implicit None

Integer :: nopts
Character*80, dimension(:,:), allocatable :: options
Integer, parameter :: maxsplicenum = 100
Character*80, dimension(1:maxsplicenum) :: splicename,spliceunit,newname
Character*80, dimension(:,:), allocatable :: splicestore
Character*80, dimension(1:2) :: outputunit
Character*80 typename,returnoption
Integer splicenum,ensnum
Logical levinvert,badecmwf
Namelist /splice/ outputunit,splicename,spliceunit,newname,levinvert,badecmwf,ensnum

Write(6,*) "g2n - GRIB1/2 to NetCDF converter (APR-11)"
Write(6,*) "Currently a work in progress.  There may be problems..."

! Read switches
nopts=3
Allocate (options(nopts,2))
options(:,1) = (/ '-i', '-o', '-f' /)
options(:,2) = ''

Call readswitch(options,nopts)
Call defaults(options,nopts)

typename=returnoption('-f',options,nopts)

outputunit(1)='pres'
outputunit(2)='hPa'
splicename=''
spliceunit=''
newname=splicename
levinvert=.TRUE.
badecmwf=.FALSE.
ensnum=0

! Read namelist

If (typename.NE.'none') Then
  Write(6,*) "Input &splice namelist"
  Read(5,NML=splice)
  Write(6,*) "Namelist accepted"
  Call procnml(splicenum,splicename,maxsplicenum)
Else
  splicenum=1
End If

Allocate(splicestore(1:splicenum,1:3))
splicestore(1:splicenum,1)=splicename(1:splicenum)
splicestore(1:splicenum,2)=spliceunit(1:splicenum)
splicestore(1:splicenum,3)=newname(1:splicenum)

! Convert
Call convert(options,nopts,outputunit,splicestore,splicenum,levinvert,badecmwf,ensnum)

! Clean-up
Deallocate (options,splicestore)

Stop
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine displays the help message
!

Subroutine help()

Implicit None

Write(6,*)
Write(6,*) "Usage:"
Write(6,*) "  g2n [-f FORMAT] -i inputfile [-o outputfile] [< g2n.nml]"
Write(6,*)
Write(6,*) "Options:"
Write(6,*) "  -f FORMAT Output file type"
Write(6,*) "              FORMAT = none (View contents only, default)"
Write(6,*) "                     = nc   (NetCDF)"
Write(6,*) "  -i NAME   Input filename"
Write(6,*) "  -o NAME   Output filename (default = inputfile.FORMAT)"
Write(6,*) "  g2n.nml   Namelist file (see below)"
Write(6,*)
Write(6,*) "Namelist:"
Write(6,*) "  The namelist g2n.nml is a required for FORMATs other than"
Write(6,*) "  'none'.  The purpose of the namelist is to specify which"
Write(6,*) "  fields are to be stored in the output file.  The following"
Write(6,*) "  example illustrates the namelist syntax:"
Write(6,*)
Write(6,*) "  &splice"
Write(6,*) '    outputunit  = "pres", "hPa"'
Write(6,*) '    levinvert   = .TRUE.'
Write(6,*) '    badecmwf    = .FALSE.'
Write(6,*) '    ensnum      = 0'
Write(6,*) '    splicename  = "HGT",  "TMP",  "UGRD", "VGRD", "TMP"'
Write(6,*) '    spliceunit  = "ISBL", "ISBL", "ISBL", "ISBL", "0.1 DBLL"'
Write(6,*) '    newname     = "hgt",  "temp", "u",    "v",    "tgg1"'
Write(6,*) "    /"
Write(6,*)
Write(6,*) "  where:"
Write(6,*) "    outputunit  = name and units for levels in the output file"
Write(6,*) "    levinvert   = order of output levels (FALSE = small to large)"
Write(6,*) "    badecmwf    = Ignore forecast time (used for ERA-40 reanalyses)"
Write(6,*) "    ensnum      = Ensemble number to extract (use 0 for no ensemble)"
Write(6,*) "    splicename  = an array of field names to be stored"
Write(6,*) "    spliceunit  = an array of unit names for splicename"
Write(6,*) "    newname     = an array of output names for splicename"
Write(6,*)
Write(6,*) "Notes:"
Write(6,*) "  Normal usage starts with viewing the contents of a GRIB file,"
Write(6,*) "  e.g.,"
Write(6,*)
Write(6,*) "  g2n -i filename"
Write(6,*)
Write(6,*) "  Once the contents of the GRIB file are known, a namelist can be"
Write(6,*) "  specified to extract the relevant data, e.g.,"
Write(6,*)
Write(6,*) "  g2n -f nc -i filename < g2n.nml"
Write(6,*)
Write(6,*) "  The output data would then be stored as filename.nc"
Write(6,*)
Stop

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determins the default values for the switches
!

Subroutine defaults(options,nopts)

Implicit None

Integer nopts
Character(len=*), dimension(nopts,2), intent(inout) :: options
Integer infile,outfile,ftype
Integer locate

infile=locate('-i',options(:,1),nopts)
outfile=locate('-o',options(:,1),nopts)
ftype=locate('-f',options(:,1),nopts)

If (options(ftype,2).EQ.'') then
  options(ftype,2)='none'
End if

If (options(infile,2).EQ.'') then
  Write(6,*) "ERROR: No input filename specified"
  Stop
End if

If (options(outfile,2).EQ.'') then
  ! Default output filename
    options(outfile,2)=trim(options(infile,2))//'.'//options(ftype,2)
End if

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads a message from the GRIB file and writes it
! to the output file
!

Subroutine convert(options,nopts,outputunit,splicename,splicenum,levinvert,badecmwf,ensnum)

Implicit None

Integer, intent(in) :: nopts,splicenum,ensnum
Character(len=*), dimension(nopts,2), intent(in) :: options
Character(len=*), dimension(1:splicenum,1:3), intent(in) :: splicename
Character(len=*), dimension(1:2), intent(in) :: outputunit
character*80, dimension(1:3) :: dumb
Logical, intent(in) :: levinvert,badecmwf
Character*80 infile,outfile,typename,lunit
Character*16 dateout
Character*80 returnoption
Integer, dimension(:,:), allocatable :: elemlist,tempelemlist
Integer, dimension(:,:), allocatable :: splicelist
Integer, dimension(1:6) :: adate
Integer, dimension(:,:), allocatable :: datelist,tempdatelist
Integer, dimension(0:4) :: ncidarr
Integer, dimension(:), allocatable :: varid,fieldlist,tempfieldlist
Integer, dimension(1:4) :: dimnum
Integer, dimension(1:4,1:2) :: dimnumtmp
integer, dimension(1:8) :: duma
Integer multidate,msgnum,elemnum,maxelemnum,gribpos
Integer gribunit,ierr,maxlvlnum
Integer maxdatenum,datenum
Integer i,t,gridout,maxens
Real, dimension(:,:,:), allocatable :: dataout
Real, dimension(:), allocatable :: alvl,tmpalvl,tmplvl
Real, dimension(1:2,1:3) :: alonlat


! Read switches
infile=returnoption('-i',options,nopts)
outfile=returnoption('-o',options,nopts)
typename=returnoption('-f',options,nopts)


! Open GRIB file
gribunit = 1
Open(UNIT=gribunit,FILE=infile,STATUS='OLD',FORM='BINARY',IOSTAT=ierr)
If (ierr.NE.0) Then
  Write(6,*) "ERROR: Cannot open input file. ierr=",ierr
  Stop
End If


! Get the number of messages
! Record data from each grid
! GRIB SCAN 1
Write(6,*) "Reading metadata..."
maxelemnum=999
elemnum=maxelemnum
Allocate(tempelemlist(1:elemnum,0:49))
Call getgriddata(gribunit,msgnum,tempelemlist,elemnum,badecmwf)
Allocate(elemlist(1:elemnum,0:49))
If (elemnum.GT.maxelemnum) Then
  Write(6,*) "WARN: Metadata buffer size exceeded - Allocating more memory"
  Call getgriddata(gribunit,msgnum,elemlist,elemnum,badecmwf)
Else
  elemlist(1:elemnum,:)=tempelemlist(1:elemnum,:)
End If
Deallocate(tempelemlist)

gridout=minval(elemlist(:,14))
  
If (typename.NE.'none') Then
  ! Determine start locations of spliced elements
  Write(6,*) "Processing metadata..."

  maxdatenum=1
  datenum=maxdatenum
  Allocate(tempdatelist(1:datenum,1:8))
  Call mapdate(tempdatelist,datenum,elemlist,elemnum)
  Allocate(datelist(1:datenum,1:8))
  If (datenum.GT.maxdatenum) Then
    Write(6,*) "WARN: Date data buffer size exceeded - Allocating more memory"
    Call mapdate(datelist,datenum,elemlist,elemnum)
  Else
    datelist(1:datenum,:)=tempdatelist(1:datenum,:)
  End If
  Deallocate(tempdatelist)
  adate(:)=datelist(1,1:6)
 
  Allocate(splicelist(1:splicenum,1:4),varid(1:splicenum))
  Call mapsplice(splicelist,splicename,splicenum,datelist,datenum,elemlist,elemnum,ensnum)

  ! Get date, lon, lat and lvl data
  Write(6,*) 'Calculating output dimensions...'
  maxlvlnum=99
  dimnum=1
  dimnum(3)=maxlvlnum
  Allocate(tmpalvl(1:dimnum(3)))
  Call getsplicedim(alonlat,tmpalvl,dimnum(1:3),splicelist,splicenum,datelist,datenum,elemlist,elemnum,outputunit(2),levinvert, &
                    maxlvlnum,splicename,ensnum)
  If (dimnum(3).EQ.0) then
    Write(6,*) "WARN: Output unit not found in GRIB file"
    dimnum(3)=1
  End if
  Allocate(alvl(1:dimnum(3)))
  If (dimnum(3).GT.maxlvlnum) then
    Write(6,*) "WARN: Level data buffer size exceeded - Allocating more memory"
    Call getsplicedim(alonlat,alvl,dimnum,splicelist,splicenum,datelist,datenum,elemlist,elemnum,outputunit(2),levinvert, &
                      maxlvlnum,splicename,ensnum)
  Else
    alvl(1:dimnum(3))=tmpalvl(1:dimnum(3))
  End if
  Deallocate(tmpalvl)
  
  gridout=minval(elemlist(splicelist(:,1),14),splicelist(:,1).gt.0)
End If


! Display GRIB data
Call displaygrib(elemlist,elemnum)
If (typename.NE.'none') Then
  Call displaysplice(splicelist,splicenum,elemlist,elemnum)
End If
! Display general file info  
Write(6,*) "General GRIB file information:"
Write(6,*) "Number of messages              = ",msgnum
Write(6,*) "Number of grids                 = ",elemnum
Write(6,*) "Min/Max ensemble number         = ",minval(elemlist(:,49)),maxval(elemlist(:,49))
If (typename.NE.'none') Then
  Write(6,*) "General splice information:"
  Write(6,'(" Start date                      = ",I4.4,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2,":",I2.2)') adate
  Write(6,*) "Number of elements in splice    = ",splicenum
  Write(6,*) "Number of time intervals        = ",datenum
  Write(6,'(" Lon (start,end,step)            = ",F10.3,F10.3,F8.3)') alonlat(1,:)
  Write(6,'(" Lat (start,end,step)            = ",F10.3,F10.3,F8.3)') alonlat(2,:)
  Write(6,'(" Lvl (start,end,lvlnum)          = ",F7.1,F7.1,I4)') alvl(1),alvl(dimnum(3)),dimnum(3)
End If


! Initialise output file
Select Case(typename)
  Case('nc')
    Call nccreatefile(ncidarr,outfile,dimnum(1:3),outputunit,adate,splicename,splicelist,varid, &
                      splicenum,datelist,datenum,elemlist,elemnum,alonlat,alvl,gridout)
  Case('none')
  Case DEFAULT
    Write(6,*) 'ERROR: Unreconised output type'
    Close(gribunit)
    Stop
End Select  

! Write each spliced element to output file
If (typename.NE.'none') Then
  Rewind(gribunit)
  gribpos=0
  Allocate(dataout(1:dimnum(1),1:dimnum(2),1:dimnum(3)))

  Do i=1,splicenum
    Do t=1,datenum
      dimnumtmp(:,1)=1
      dimnumtmp(4,1)=t
  
      ! GRIB SCAN 3+N (N=0,1,2...) of the GRIB file
      If (splicelist(i,1).GT.0) then
      
        Write(dateout,'(I4.4,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2)') datelist(t,1:5)
        Write(6,*) "Reading ",trim(splicename(i,3))," for ",dateout

	! Get list of level positions
        dimnumtmp(:,2)=dimnum(:)
        If (splicelist(i,4).EQ.1) Then
          dimnumtmp(3,2)=1
          dimnumtmp(3,1)=t
        End if
  
        ! Get spliced element data (also interpolate to lat/lon/lvl grid)
        duma=datelist(t,:)
        dumb=splicename(i,:)
        Call unpackgribdata(gribunit,gribpos,dataout(1:dimnumtmp(1,2),1:dimnumtmp(2,2),1:dimnumtmp(3,2)), &
                            alonlat,alvl,dimnumtmp(1:3,2),duma,elemlist,elemnum, &
                            outputunit(2),dumb,gridout,ensnum)

        ! Write spliced element data to output file    
        Select Case(typename)
          Case('nc')
            Write(6,*) "Writing ",trim(splicename(i,3))
            Call ncwritedat4(ncidarr,dataout(1:dimnumtmp(1,2),1:dimnumtmp(2,2),1:dimnumtmp(3,2)),dimnumtmp,varid(i))
          Case DEFAULT
            Write(6,*) 'ERROR: Unreconised output type'
            Close(gribunit)
            Stop
        End Select
      
      End If
    End Do
  End Do
  Deallocate(dataout)
  Deallocate(splicelist,varid,alvl,datelist)
End if

Deallocate(elemlist)

! Clean-up
Close(gribunit)

Select Case(typename)
  Case('nc')
    Call ncclose(ncidarr)
  Case('none')
  Case DEFAULT
    Write(6,*) 'ERROR: Unreconised output type'
    Close(gribunit)
    Stop
End Select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine gets data from the grib file
!

Subroutine unpackgribdata(gribunit,gribpos,dataout,alonlat,alvl,dimnum, &
                          datelist,elemlist,elemnum,ounit,splicename,gridout,ensnum)

Implicit None

Integer, intent(in) :: gribunit,gribpos,elemnum,gridout,ensnum
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
Integer, dimension(1:3), intent(in) :: dimnum
Integer, dimension(1:8), intent(in) :: datelist
Integer, dimension(:), allocatable :: fieldlist,tempfieldlist
Real, dimension(1:dimnum(1),1:dimnum(2),1:dimnum(3)), intent(out) :: dataout
Real, dimension(1:dimnum(3)), intent(in) :: alvl
Real, dimension(1:2,1:3), intent(in) :: alonlat
Real, dimension(:), allocatable :: tmplvl,tmpalvl
Character(len=*), intent(in) :: ounit
Character(len=*), dimension(1:3), intent(in) :: splicename
Integer, dimension(1:3) :: fielddimnum
Integer, dimension(1:2) :: oldfielddimnum
integer, dimension(0:49) :: duma
Integer locposa(1),locposb(1),locpos(1)
Integer unpacksize,indx,skipnum
Integer i,j,k
Integer countmultigrid
Real, dimension(:), allocatable :: dataunpack
Real, dimension(:,:,:), allocatable :: rawfield
Real, dimension(:,:), allocatable :: rawslice
Real, dimension(1:2,1:3) :: fieldlonlat
Double Precision surfvalue,sndvalue
Character*80, dimension(1:3) :: surtxt
Character*80 gridtype,lunit

fielddimnum(3)=dimnum(3)
Allocate(tempfieldlist(1:fielddimnum(3)),tmpalvl(1:fielddimnum(3)))
Call getfieldlvl(tmpalvl,tempfieldlist,fielddimnum(3),lunit,datelist,elemlist,elemnum,splicename,ensnum)

If (fielddimnum(3).eq.0) then
  Write(6,*) "WARN: Missinig data"
  Deallocate(tempfieldlist,tmpalvl)
  dataout=0.
Else

  Allocate(fieldlist(1:fielddimnum(3)),tmplvl(1:fielddimnum(3)))
  If (fielddimnum(3).GT.dimnum(3)) then
    Call getfieldlvl(tmplvl,fieldlist,fielddimnum(3),lunit,datelist,elemlist,elemnum,splicename)
  Else
    fieldlist(1:fielddimnum(3))=tempfieldlist(1:fielddimnum(3))
    tmplvl(1:fielddimnum(3))=tmpalvl(1:fielddimnum(3))
  End if
  Deallocate(tempfieldlist,tmpalvl)

  If ((fielddimnum(3).gt.1).and.(dimnum(3).eq.1)) then
    Write(6,*) "ERROR: Found 3D data when expecting 2D"
    Stop
  End if

  ! Allocate arrays (ready for lon/lat)
  oldfielddimnum(1:2)=dimnum(1:2)
  unpacksize=oldfielddimnum(1)*oldfielddimnum(2)
  Allocate(rawfield(1:dimnum(1),1:dimnum(2),1:fielddimnum(3)))
  Allocate(dataunpack(1:unpacksize),rawslice(1:oldfielddimnum(1),1:oldfielddimnum(2)))

  Do i=1,fielddimnum(3)
    duma=elemlist(fieldlist(i),:)
    
    ! Get lon,lat size
    Call getelemlonlat(duma,fieldlonlat,gridtype)
    Do j=1,2
      If (fieldlonlat(j,3).EQ.0.) then
        fielddimnum(j)=1
      Else
        fielddimnum(j)=nInt(Abs(fieldlonlat(j,2)-fieldlonlat(j,1))/abs(fieldlonlat(j,3)))+1
      End If
    End Do

    ! Check for multiple grids in the message
    skipnum=Count(elemlist(1:fieldlist(i),0).EQ.elemlist(fieldlist(i),0))

    ! Get GRIB grid data
    If (Any(oldfielddimnum(1:2).NE.fielddimnum(1:2))) then
      Deallocate(dataunpack,rawslice)
      oldfielddimnum(1:2)=fielddimnum(1:2)
      unpacksize=fielddimnum(1)*fielddimnum(2)
      Allocate(dataunpack(1:unpacksize),rawslice(1:fielddimnum(1),1:fielddimnum(2)))
    End if
    
    Call unpackgrid(gribunit,gribpos,elemlist(fieldlist(i),0),skipnum,dataunpack,unpacksize)
    
    Do j=1,fielddimnum(1)
      Do k=1,fielddimnum(2)
        rawslice(j,k)=dataunpack((k-1)*fielddimnum(1)+j)
      End Do
    End Do
  
    ! Insert data into 2D/3D field
    Call getelemlvl(duma,indx,surfvalue,sndvalue,surtxt)
    locposa=Maxloc(tmplvl,tmplvl.LE.surfvalue)
    locposb=Minloc(tmplvl,tmplvl.GE.surfvalue)
    locposa(1)=min(fielddimnum(3),max(locposa(1),1))
    locposb(1)=min(fielddimnum(3),max(locposb(1),1))  
    If (abs(tmplvl(locposa(1))-surfvalue).LT.abs(tmplvl(locposb(1))-surfvalue)) Then
      locpos=locposa
    Else
      locpos=locposb  
    End If

    ! Need to interpolate 2D grid so that the 3D field is self-consistant
    ! Cannot use 3D interpolation since grid may not be regulat lat/lon
    Call interpolate2dgrid(rawslice,fieldlonlat,fielddimnum,rawfield(:,:,locpos(1)),alonlat,dimnum, &
                           elemlist(fieldlist(i),14),gridout)
  
  End Do

  ! Interpolate unpacked data between levels
  Call getelemlvl(duma,indx,surfvalue,sndvalue,surtxt)
  Call interpolate3dgrid(rawfield,tmplvl,fielddimnum,dataout,alvl,dimnum,surtxt(1),ounit)

  Deallocate(fieldlist,tmplvl)			    
  Deallocate(rawfield,dataunpack,rawslice)

End If

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine creates a NetCDF file
!

Subroutine nccreatefile(ncidarr,outfile,dimnum,outputunit,adate,splicename,splicelist,varid, &
                        splicenum,datelist,datenum,elemlist,elemnum,alonlat,alvl,gridout)

Implicit None

Include "netcdf.inc"

Integer, intent(in) :: splicenum,elemnum,datenum,gridout
Integer, dimension(0:4), intent(inout) :: ncidarr
Integer, dimension(1:3), intent(in) :: dimnum
Integer, dimension(1:6), intent(in) :: adate
Integer, dimension(1:splicenum,1:4), intent(in) :: splicelist
Integer, dimension(1:datenum,1:8), intent(in) :: datelist
Integer, dimension(1:splicenum), intent(out) :: varid
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
integer, dimension(0:49) :: duma
Real, dimension(1:2,1:3), intent(in) :: alonlat
Real, dimension(1:dimnum(3)), intent(in) :: alvl
Real, dimension(1:3,1:2) :: alonlattmp
Real, dimension(1:datenum) :: atime
real, dimension(1:dimnum(2)) :: latarr
Real smin,sadd
Character(len=*), intent(in) :: outfile
Character(len=*), dimension(1:2), intent(in) :: outputunit
Character(len=*), dimension(1:splicenum,1:3), intent(in) :: splicename
Integer, dimension(1:4) :: dimid,dimout
Integer, dimension(1:6) :: sdate
Integer i,j,nsize
Character*80, dimension(1:3) :: elemdesc,elemtxt
Logical test

Do i=1,2
  alonlattmp(:,i)=alonlat(i,:)
End Do

select case(gridout)
  case(0)
    Call ncinit(ncidarr,outfile,dimnum,dimid,outputunit,adate)
  case(10,40)
    Call ncinitgen(ncidarr,outfile,dimnum,dimid,outputunit,adate,'uneven')  
  case DEFAULT
    write(6,*) "ERROR: Unsupported output grid type ",gridout
    write(6,*) "       Please contact MJT and get him to fix this"
    stop
end select
  
Do i=1,splicenum
  If (splicelist(i,1).gt.0) then
    duma=elemlist(splicelist(i,1),:)
    Call getelemdesc(duma,elemtxt)
    elemdesc(1)=splicename(i,3)
    elemdesc(2)=elemtxt(2)
    elemdesc(3)=elemtxt(3)

    If (splicelist(i,4).EQ.1) Then
      If ((datenum.EQ.1).or.(splicelist(i,3).gt.0)) then
        Call ncaddvargen(ncidarr,elemdesc,nf_float,3,varid(i),1.,0.)
      Else
        Call ncaddvargen(ncidarr,elemdesc,nf_float,2,varid(i),1.,0.)
      End if
    Else
      Call ncaddvargen(ncidarr,elemdesc,nf_float,4,varid(i),1.,0.)
    End If
  End if
EndDo
Call ncenddef(ncidarr)

atime=0.
sdate=adate
smin=0.
sadd=1.
if (All(datelist(:,5:6).eq.0).and.All(sdate(5:6).eq.0)) sadd=60.
do i=1,datenum
  test=.true.
  do while (test)
    test=.false.
    j=1
    do while (j.le.6)
      if (sdate(j).lt.datelist(i,j)) then
        test=.true.
        j=9
      else if (sdate(j).gt.datelist(i,j)) then
        j=9
      else
        j=j+1
      end if
    end do
    if (test) then
      smin=smin+sadd
      call advdate(adate,sdate,nint(smin))
    end if
  end do
  atime(i)=smin/60.
end do
dimout(1:3)=dimnum(1:3)
dimout(4)=datenum

select case(gridout)
  case(0) ! Reg
    Call nclonlatgen(ncidarr,dimid,alonlattmp,alvl,atime,dimout)
  case(10) ! Merc
    nsize=1+nint((alonlat(2,2)-alonlat(2,1))/(alonlat(2,3)))
    if (nsize.ne.dimnum(2)) then
      write(6,*) "ERROR: Mismatch in gauss dimension"
      write(6,*) "nsize,indim(2) ",nsize,dimnum(2)
      stop
    end if    
    call getmerc(alonlat,latarr,nsize)
    call nclonlatarr(ncidarr,dimid,alonlat(1,:),latarr,alvl,atime,dimout)
        
  case(40) ! Gauss
    nsize=2*nint((alonlat(2,1)-alonlat(2,2))/(2.*alonlat(2,3)))
    if (nsize.ne.dimnum(2)) then
      write(6,*) "ERROR: Mismatch in gauss dimension"
      write(6,*) "nsize,indim(2) ",nsize,dimnum(2)
      stop
    end if    
    call getgauss(alonlat,latarr,nsize)
    call nclonlatarr(ncidarr,dimid,alonlat(1,:),latarr,alvl,atime,dimout)
  
  case DEFAULT
    write(6,*) "ERROR: Unsupported output grid type ",gridout
    write(6,*) "       Please contact MJT and get him to fix this"
    stop
end select

Return
End

