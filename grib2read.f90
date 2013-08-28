!
! SUBROUTINES FOR READING GRIB2 FILES.
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine records data (e.g., name, comment, units, etc) of
! each grid in a GRIB file
!

Subroutine getgriddata(gribunit,msgnum,elemlist,elemnum,badecmwf)

Implicit None

Integer, intent(in) :: gribunit
Integer, intent(inout) :: elemnum
Integer, intent(out) :: msgnum
Integer, dimension(1:elemnum,0:49), intent(out) :: elemlist
Integer, dimension(0:49) :: alist
Integer gribend,multigrid,msgcount,elemcount
Logical, intent(in) :: badecmwf

! alist(0) = message number
! alist(1) = Centre
! alist(2) = Subcentre
! alist(3) = Prodtype
! alist(4) = Template
! alist(5) = Category
! alist(6) = Subcategory
! alist(7) = LenTime
! alist(8) = fstsurftype
! alist(9) = fstsurfactor
! alist(10) = fstsurdata
! alist(11) = sndsurfactor
! alist(12) = sndsurfdata
! alist(13) = f_sndvalue
! alist(14) = Map projection
! alist(15:41) = Lon and Lat coord data.
! alist(42) = year
! alist(43) = month
! alist(44) = day
! alist(45) = hour
! alist(46) = min
! alist(47) = sec
! alist(48) = GRIB edition number
! alist(49) = ensemble number

elemlist=0
alist=0
gribend=0
multigrid=0
msgcount=0
elemcount=0
Rewind(gribunit)
Do While (gribend.EQ.0)

  Call scangribhead(gribunit,gribend,multigrid,alist,badecmwf)
  If (gribend.EQ.0) Then
    If (multigrid.EQ.0) msgcount=msgcount+1
    elemcount=elemcount+1

    If (msgcount.EQ.0) Then
      Write(6,*) "ERROR: Cannot read GRIB file"
      Close(gribunit)
      Stop
    End If
  
    If (elemcount.LE.elemnum) Then
      elemlist(elemcount,:)=alist(:)
    End If
  End If
  
End Do

elemnum=elemcount
msgnum=msgcount

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine scans a single grid header
!

Subroutine scangribhead(gribunit,gribend,multigrid,alist,badecmwf)

Implicit None

Integer, intent(in) :: gribunit
Integer, dimension(0:49), intent(inout) :: alist
Integer, intent(out) :: gribend,multigrid
Integer firstsec
Logical, intent(in) :: badecmwf

! Check for multigrid or section 0 (i.e., next message)
! Note: prodtype is unchanged in a multigrid message

! Assume it is a multigrid until proven otherwise
Call gribsec2340(gribunit,gribend,firstsec,alist,.true.,badecmwf)
If (firstsec.GT.0) Then
  multigrid=1
Else
  multigrid=0
  alist(0)=alist(0)+1
End If

If (gribend.EQ.0) Then
  Select Case(alist(48))
    Case(1)
      Call gribsec1234(gribunit,alist,badecmwf)
    Case DEFAULT !GRIB2
      ! Note: Section 1 is unchanged in a multigrid message
      If (multigrid.EQ.0) Then
        Call gribsec1(gribunit,alist)
        ! This time get sections 2, 3 and 4
        Call gribsec2340(gribunit,gribend,firstsec,alist,.false.,badecmwf)
      End If
      Call gribskiphead(gribunit,5)
      Call gribskiphead(gribunit,6)
      Call gribskiphead(gribunit,7)
  End Select
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads sections 2, 3, 4 and 0.  Which sections are
! read depends on whether there are multiple grids in a single message
! or not.
!

Subroutine gribsec2340(gribunit,gribend,firstsec,alist,mtest,badecmwf)

Implicit None

Integer, intent(in) :: gribunit
Integer, dimension(0:49), intent(inout) :: alist
Integer, intent(out) :: gribend,firstsec
Logical, intent(in) :: mtest,badecmwf
Character*1 txt(1:4)
Integer*1, dimension(:), allocatable :: values
Integer*1 dat(1:16)
Integer, dimension(1:6) :: adate
Integer i,seclen,fstsurfdata,fstsurffactor,sndsurfdata,sndsurffactor,ierr
Integer tunit1,tunit2,tp1
Integer arr2int

firstsec=-1
gribend=0

If (alist(48).EQ.2) then
  !---------------------------------------------------------------------
  ! Search for section 2
  Read(UNIT=gribunit,IOSTAT=ierr) dat(1:5)

  If (ierr.NE.0) then
    ! Reached end of GRIB file
    gribend=1
    Rewind(gribunit)
    Return
  End If
  
  seclen=arr2int(dat(1:4),4)
  
  If (dat(5).EQ.2) Then
    firstsec=2
  
    Write(6,*) "  WARN: Non-trivial section 2 detected. Skipping."
    
    Allocate(values(1:seclen))
    Read(UNIT=gribunit) values(6:seclen)
    values(1:5)=dat(1:5)
    Deallocate(values)  

    Read(UNIT=gribunit) dat(1:5)
    seclen=arr2int(dat(1:4),4)
  End If

  !---------------------------------------------------------------------
  ! Search for section 3
  If (dat(5).EQ.3) Then
    If (firstsec.EQ.-1) firstsec=3

    Read(UNIT=gribunit) dat(6:14)

    Allocate(values(1:seclen))
    Read(UNIT=gribunit) values(15:seclen)
    values(1:14)=dat(1:14)

    ! Map projection
    alist(14)=arr2int(dat(13),1)
    ! alist(15:41) = lat/lon data
    alist(15:41)=0
  
    Select Case(alist(14))
  
      Case(0)
        If (seclen.LT.71) Then
          Write(6,*) "ERROR: Missing data from section 4"
          Close(gribunit)
          Stop
        End If
      
        Do i=1,4
          alist(14+i)=arr2int(values(35+i*4:38+i*4),4)
        End Do
        alist(19)=arr2int(values(55),1)
        Do i=1,4
          alist(19+i)=arr2int(values(52+i*4:55+i*4),4)
        End Do
    
      Case(10)
        If (seclen.LT.72) Then
          Write(6,*) "ERROR: Missing data from section 4"
          Close(gribunit)
          Stop
        End If
    
        Do i=1,2
      	  alist(14+i)=arr2int(values(35+i*4:38+i*4),4)
        End Do
          alist(17)=arr2int(values(47),1)
        Do i=1,3
          alist(17+i)=arr2int(values(44+i*4:47+i*4),4)
        End Do
          alist(21)=arr2int(values(60),1)
        Do i=1,3
          alist(21+i)=arr2int(values(57+i*4:60+i*4),4)
        End Do
    
      Case(20)
        If (seclen.LT.64) Then
          Write(6,*) "ERROR: Missing data from section 4"
	      Close(gribunit)
	      Stop
        End If

        Do i=1,2
	      alist(14+i)=arr2int(values(35+i*4:38+i*4),4)
        End Do
          alist(17)=arr2int(values(47),1)
        Do i=1,4
	      alist(17+i)=arr2int(values(44+i*4:47+i*4),4)
        End Do
          alist(22)=arr2int(values(64),1)
    
      Case(30)
        If (seclen.LT.81) Then
          Write(6,*) "ERROR: Missing data from section 4"
	      Close(gribunit)
	      Stop
        End If

        Do i=1,2
	      alist(14+i)=arr2int(values(35+i*4:38+i*4),4)
        End Do
        alist(17)=arr2int(values(47),1)
        Do i=1,4
	      alist(17+i)=arr2int(values(44+i*4:47+i*4),4)
        End Do
        alist(22)=arr2int(values(64),1)
        alist(23)=arr2int(values(65),1)
        Do i=1,4
	      alist(23+i)=arr2int(values(62+i*4:65+i*4),4)
        End Do
    
      Case DEFAULT
        Write(6,*) "ERROR: Un-supported map projection ",alist(14)
        Close(gribunit)
        Stop
  
    End Select
  
    Deallocate(values)  

    Read(UNIT=gribunit) dat(1:5)
    seclen=arr2int(dat(1:4),4)
  Else  
    If (firstsec.NE.-1) Then
      Write(6,*) "ERROR: Failed to read section 3"
      Close(gribunit)
      Stop
    End If
  End If

  !---------------------------------------------------------------------
  ! Search for section 4
  If (dat(5).EQ.4) Then
    If (firstsec.EQ.-1) firstsec=4
  
    Read(UNIT=gribunit) dat(6:9)

    Allocate(values(1:seclen))
    Read(UNIT=gribunit) values(10:seclen)
    values(1:9)=dat(1:9)

    ! template
    alist(4)=arr2int(values(8:9),2)
    ! category
    alist(5)=arr2int(values(10),1)
    ! subcategory
    alist(6)=arr2int(values(11),1)
    ! LenTime
    adate=alist(42:47)
    tp1=arr2int(values(19:22),4)
    tunit1=arr2int(values(18),1)
    if (alist(4).eq.8.and.seclen.ge.53) then ! use end of time range for date
      tunit2=arr2int(values(49),1)
      if (tunit1.eq.tunit2) then
        tp1=tp1+arr2int(values(50:53),4)
      else
        tp1=arr2int(values(50:53),4)
      end if
      tunit1=tunit2      
    end if
    Call g1date(alist(42:47),alist(7),tunit1,tp1,0,0)
    if (badecmwf.or.mtest) then
      alist(42:47)=adate ! clobber fcst time in ECMWF analyses or when multigrid
    end if
    ! ensemble
    ! (values(12) holds the generating process=4 for ensemble)
    alist(49)=arr2int(values(36),1)
    ! (values(37) holds the total number of ensemble members)
      
    ! lvl info
    If ((alist(4).EQ.20).OR.(alist(4).EQ.30).OR.(alist(4).EQ.254).OR.(alist(4).EQ.1000).OR.(alist(4).EQ.1001).OR.(alist(4).EQ.1002)) Then
      ! Undefined
      alist(8:13)=-1
    Else
      If (seclen.GE.28) Then
        ! fstsurftype
        alist(8)=arr2int(values(23),1)
        ! fstsurfactor
        alist(9)=arr2int(values(24),1)
        if (alist(9).gt.127) alist(9)=128-alist(9)
        ! fstsurfdata
        alist(10)=arr2int(values(25:28),4)
      
        If (seclen.GE.34) Then
          ! sndsurffactor
          alist(11)=arr2int(values(30),1)
          if (alist(11).gt.127) alist(11)=128-alist(11)
          ! sndsurfdata
          alist(12)=arr2int(values(31:34),4)
          ! f_sndvalue
          alist(13)=1
        Else
          alist(11:13)=0
        End If
      
      Else
        Write(6,*) "ERROR: Level data in section 4 is missing"
        Close(gribunit)
        Stop
      End If
    End If
    
    Deallocate(values)
  
  Else  
    If (firstsec.NE.-1) Then
      Write(6,*) "ERROR: Failed to read section 4"
      Close(gribunit)
      Stop
    End If
  End If
End If

!---------------------------------------------------------------------
! Search for section 0 (if a section has not already been found)
If (firstsec.EQ.-1) Then

  Allocate(values(1:16))
  Call gribsec0(gribunit,dat(1:5),values(1:16),gribend)
  If (gribend.NE.1) Then
    firstsec=0
    ! prodtype
    alist(3)=arr2int(values(7),1)
    ! edition number
    alist(48)=arr2int(values(8),1)
  End if
  
  Deallocate(values)
  
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads section 0 of the grib file
!

Subroutine gribsec0(gribunit,indat,dat,gribend)

Implicit None

Integer, intent(in) :: gribunit
Integer, intent(out) :: gribend
Integer*1, dimension(1:5), intent(in) :: indat
Integer*1, dimension(1:16), intent(out) :: dat
Integer*1 tmp
Integer i,txtchk,ierr
Integer chkgrib

dat=0
dat(1:5)=indat(1:5)

txtchk=chkgrib(dat(1:4),'GRIB')
Do While (txtchk.NE.4)
  Read(UNIT=gribunit,IOSTAT=ierr) tmp
  
  If (ierr.NE.0) then
    ! Reached end of GRIB file
    gribend=1
    Rewind(gribunit)
    Return
  End if

  Do i=1,4
    dat(i)=dat(i+1)
  EndDo
  dat(5)=tmp
  txtchk=chkgrib(dat(1:4),'GRIB')
End Do

! Once located, read from section 0
Read(UNIT=gribunit) dat(6:8)
Select Case(dat(8))
  Case(1)
    dat(9:13)=0
    dat(14:16)=dat(5:7)
    dat(5:7)=0
  Case DEFAULT !GRIB2
    Read(UNIT=gribunit) dat(9:16)
End Select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads section 1 of the grib2 file
!

Subroutine gribsec1(gribunit,alist)

Implicit None

Integer, intent(in) :: gribunit
Integer, dimension(0:49), intent(inout) :: alist
Integer*1, dimension(1:21) :: dat
Integer i
Integer arr2int

! Read Section 1
Read(UNIT=gribunit) dat(1:21)
If (dat(5).NE.1) Then
  Write(6,*) "ERROR: Failed to read section 1"
  Close(gribunit)
  Stop
End If
! Centre
alist(1)=arr2int(dat(6:7),2)
! Subcentre
alist(2)=arr2int(dat(8:9),2)
! Year
alist(42)=arr2int(dat(13:14),2)
! Month
alist(43)=arr2int(dat(15),1)
! Day
alist(44)=arr2int(dat(16),1)
! Hour
alist(45)=arr2int(dat(17),1)
! Min
alist(46)=arr2int(dat(18),1)
! Sec
alist(47)=arr2int(dat(19),1)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads GRIB1 PDS and GDS
!

Subroutine gribsec1234(gribunit,alist,badecmwf)

Implicit None

Integer, intent(in) :: gribunit
Integer, dimension(0:49), intent(inout) :: alist
Integer*1, dimension(:), allocatable :: dat
Integer*1, dimension(1:3) :: tmp
Integer, dimension(1:6) :: adate
Integer i,seclen
Integer arr2int,signarr2int
Integer trng,tunit,tp1,tp2
Logical foundgds,foundbms
Logical, intent(in) :: badecmwf

Read(UNIT=gribunit) tmp(1:3)
seclen=arr2int(tmp,3)
Allocate(dat(1:seclen))
dat(1:3)=tmp(1:3)
Read(UNIT=gribunit) dat(4:seclen)
! Centre
alist(1)=arr2int(dat(5),1)
! Subcentre
alist(2)=arr2int(dat(26),1)
! prodtype
alist(3)=arr2int(dat(4),1)
! template
alist(4)=0 ! not used
! category
alist(5)=arr2int(dat(9),1)
! subcategory
alist(6)=0
! LenTime
alist(7)=0
! fstsurftype
alist(8)=arr2int(dat(10),1)
! fstsurfactor
alist(9)=arr2int(dat(11:12),2)
! fstsurfdata
alist(10)=0
! sndsurffactor
alist(11)=0
! sndsurfdata
alist(12)=0
! f_sndvalue
alist(13)=0 ! defined in grib2meta
! alist(15:41) = lat/lon data
alist(41)=arr2int(dat(7),1)
! Year
alist(42)=arr2int(dat(13),1)+100*(arr2int(dat(25),1)-1)
! Month
alist(43)=arr2int(dat(14),1)
! Day
alist(44)=arr2int(dat(15),1)
! Hour
alist(45)=arr2int(dat(16),1)
! Min
alist(46)=arr2int(dat(17),1)
! Sec
alist(47)=0
! ensemble
select case(alist(1))
  case(98)     ! ECMWF
    alist(49)=arr2int(dat(50),1) ! This should be dat(51).  However, the actual number is stored at dat(50) for some reason.
  case default ! NCEP
    alist(49)=arr2int(dat(43),1)
end select

tunit=arr2int(dat(18),1)
tp1=arr2int(dat(19),1)
tp2=arr2int(dat(20),1)
trng=arr2int(dat(21),1)
adate=alist(42:47)
Call g1date(alist(42:47),alist(7),tunit,tp1,tp2,trng)
if (badecmwf) then
  alist(42:47)=adate ! clobber fcst time in ECMWF analyses 
end if

foundgds=(128.AND.dat(8)).NE.0
foundbms=(64.AND.dat(8)).NE.0
Deallocate(dat)

! alist(15:41) = lat/lon data
alist(15:41)=0

! Search for GDS
If (foundgds) Then
  Read(UNIT=gribunit) tmp(1:3)
  seclen=arr2int(tmp,3)
  Allocate(dat(1:seclen))
  dat(1:3)=tmp(1:3)
  Read(UNIT=gribunit) dat(4:seclen)  
  ! Map projection
  alist(14)=arr2int(dat(6),1)
  
  Select Case(alist(14))
    Case(0) !Latlon
      alist(15)=arr2int(dat(7:8),2)
      alist(16)=arr2int(dat(9:10),2)
      alist(17)=signarr2int(dat(11:13),3)
      alist(18)=signarr2int(dat(14:16),3)
      !If (alist(18).lt.0) alist(18)=alist(18)+360000
      alist(19)=arr2int(dat(17),1)
      alist(20)=signarr2int(dat(18:20),3)
      alist(21)=signarr2int(dat(21:23),3)
      !If (alist(21).lt.0) alist(21)=alist(21)+360000
      alist(22)=arr2int(dat(24:25),2)
      alist(23)=arr2int(dat(26:27),2)
    Case(1)
      alist(14)=10 ! for GRIB2 convention
      alist(15)=arr2int(dat(7:8),2)
      alist(16)=arr2int(dat(9:10),2)
      alist(17)=signarr2int(dat(11:13),3)
      alist(18)=signarr2int(dat(14:16),3)
      !If (alist(18).lt.0) alist(18)=alist(18)+360000
      alist(19)=arr2int(dat(17),1)
      alist(20)=signarr2int(dat(18:20),3)
      alist(21)=signarr2int(dat(21:23),3)
      !If (alist(21).lt.0) alist(21)=alist(21)+360000
      alist(22)=arr2int(dat(24:25),2)
      alist(23)=arr2int(dat(26:27),2)      
    Case(3)
      alist(14)=30 ! for GRIB2 convention
      alist(15)=arr2int(dat(7:8),2)
      alist(16)=arr2int(dat(9:10),2)
      alist(17)=signarr2int(dat(11:13),3)
      alist(18)=signarr2int(dat(14:16),3)
      !If (alist(18).lt.0) alist(18)=alist(18)+360000
      alist(19)=arr2int(dat(17),1)
      alist(20)=signarr2int(dat(18:20),3)
      alist(21)=signarr2int(dat(21:23),3)
      !If (alist(21).lt.0) alist(21)=alist(21)+360000
      alist(22)=arr2int(dat(24:25),2)
      alist(23)=arr2int(dat(26:27),2)      
    Case(4) !Gaussian
      alist(14)=40 ! for GRIB2 convention
      alist(15)=arr2int(dat(7:8),2)
      alist(16)=arr2int(dat(9:10),2)
      alist(17)=signarr2int(dat(11:13),3)
      alist(18)=signarr2int(dat(14:16),3)
      !If (alist(18).lt.0) alist(18)=alist(18)+360000
      alist(19)=arr2int(dat(17),1)
      alist(20)=signarr2int(dat(18:20),3)
      alist(21)=signarr2int(dat(21:23),3)
      !If (alist(21).lt.0) alist(21)=alist(21)+360000
      alist(22)=arr2int(dat(24:25),2)
      alist(23)=arr2int(dat(26:27),2)
    Case(5)
      alist(14)=20 ! for GRIB2 convention
    Case DEFAULT
      Write(6,*) "ERROR: Un-supported map projection ",alist(14)
      Write(6,*) "       Please contact MJT and get him to fix this"
      Close(gribunit)
      Stop
  End Select

  Deallocate(dat)
End If

! Search for BMS
If (foundbms) Then
  Read(UNIT=gribunit) tmp(1:3)
  seclen=arr2int(tmp,3)
  Allocate(dat(1:seclen))
  dat(1:3)=tmp(1:3)
  Read(UNIT=gribunit) dat(4:seclen)  
  Deallocate(dat)
End If

! Search for BDS
Read(UNIT=gribunit) tmp(1:3)
seclen=arr2int(tmp,3)
Allocate(dat(1:seclen))
dat(1:3)=tmp(1:3)
Read(UNIT=gribunit) dat(4:seclen)  
  
Deallocate(dat)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine skips a specified header of the GRIB file
!

Subroutine gribskiphead(gribunit,skip)

Implicit None

Integer, intent(in) :: gribunit,skip
Integer*1 dat(1:5)
Integer*1 tmp
Integer seclen
Integer i
Integer arr2int

Read(UNIT=gribunit) dat(1:5)
seclen=arr2int(dat(1:4),4)

If (arr2int(dat(5),1).NE.skip) then
  Write(6,'("ERROR: Failed to read section ",I1)') skip
  Close(gribunit)
  Stop
End If
Read(UNIT=gribunit) (tmp, i=6,seclen)
Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine unpacks a single grid of data
!

Subroutine unpackgrid(gribunit,gribpos,msgnum,spliceskip,dataunpack,unpacksize)

Use grib_mod

Implicit None

Integer, intent(in) :: gribunit,msgnum,unpacksize,spliceskip
Integer, intent(inout) :: gribpos
Real, dimension(1:unpacksize), intent(out) :: dataunpack
Type (gribfield) :: gfld
Integer*1, dimension(:), allocatable :: datapack
Integer, dimension(0:49) :: alist
Integer*1, dimension(1:5) :: indat
Integer*1, dimension(1:16) :: dat
Integer*1 tmp
Integer gribend,multigrid,packsize,ierr
Integer i,minstart,oldpacksize
Integer getmsgsize

oldpacksize=0

! Rewind if we have passed the message
If (gribpos.GE.msgnum) Then
  Rewind(gribunit)
  gribpos=0
End If

! Skip messages
Do While (gribpos.LT.msgnum)
  ! Get size of unpacked data
  Call gribsec0(gribunit,indat,dat,gribend)
  packsize=getmsgsize(dat)
  
  Select Case(dat(8))
    Case(1)
      dat(5:7)=dat(14:16)
      minstart=9
    Case DEFAULT !GRIB2
      minstart=17
  End Select

  ! Allocate arrays
  if (packsize.gt.oldpacksize) then
    if (allocated(datapack)) then
      deallocate(datapack)
    end if
    Allocate(datapack(1:packsize))
    oldpacksize=packsize
  end if

  datapack(1:minstart-1)=dat(1:minstart-1)

  ! Read remaining packed data (assume disk cache for speed...)
  Read(UNIT=gribunit,IOSTAT=ierr) datapack(minstart:packsize)

  If (ierr.NE.0) then
    Write(6,*) "ERROR: Cannot read GRIB message."
    Stop
  End if

  gribpos=gribpos+1
End Do

! Call unpack lib
Write(6,'(T2A,T31I6,T37A,T39I6,T45A)') "Unpacking message number:",msgnum," (",spliceskip,")"
Select Case(dat(8))
  Case(1)
    Call unpackgrib1(datapack(1:packsize),packsize,dataunpack,unpacksize) 
  Case DEFAULT !GRIB2
    Call gf_getfld(Char(datapack(1:packsize)),packsize,spliceskip,.TRUE.,1,gfld,ierr)
    If (ierr.NE.0) Then
      Write(6,*) "ERROR: Error in unpack lib (",ierr,")"
      Stop
    End If
    ! Copy data to array and free memory
    dataunpack=gfld%fld
    Call gf_free(gfld)
End Select

Deallocate(datapack)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine unpacks GRIB1 data
!

Subroutine unpackgrib1(dat,packsize,dataunpack,unpacksize)

Implicit None

Integer, intent(in) :: packsize,unpacksize
Integer*1, dimension(packsize), intent(in) :: dat
Real, dimension(unpacksize), intent(out) :: dataunpack
Integer gdsstart,bdsstart,bmsstart,nbits,scale,bdsend,pos,decscale,ds,ups
Integer i,pwr,sbits,cbits,bs
Integer arr2int,signarr2int
Real ref,rds,rs,store
Real arr2flt
Logical harmonic,udf

dataunpack=0.
ups=unpacksize

pos=9 ! start of section 1
pos=arr2int(dat(pos:pos+2),3)+pos ! start of section 2
If ((128.AND.dat(16)).EQ.0) then
  gdsstart=0
Else
  gdsstart=pos
  pos=arr2int(dat(pos:pos+2),3)+pos  
End If

If ((64.AND.dat(16)).EQ.0) Then
  bmsstart=0
Else
  bmsstart=pos
  pos=arr2int(dat(pos:pos+2),3)+pos    
End If

bdsstart=pos
bdsend=arr2int(dat(bdsstart:bdsstart+2),3)+bdsstart-1
decscale=signarr2int(dat(35:36),2)
rds=10.**(-decscale)
nbits=arr2int(dat(bdsstart+10),1)
scale=signarr2int(dat(bdsstart+4:bdsstart+5),2)
rs=rds*2.**scale
ref=rds*arr2flt(dat(bdsstart+6:bdsstart+9))

harmonic=((dat(bdsstart+3).AND.128).NE.0)
If (harmonic) Then
  dataunpack=arr2flt(dat(bdsstart+11:bdsstart+14))
  ds=bdsstart+15
  ups=ups-1
Else
  ds=bdsstart+11  
End If

If (bmsstart.GT.0) Then
  bs=bmsstart+6
Else
  bs=0
End If

cbits=8
Do i=1,ups
  udf=.FALSE.
  If (bmsstart.GT.0) then
    pos=8-Mod(i,8)
    If (pos.EQ.8) pos=0
    pwr=2**pos
    udf=(dat(bs).AND.pwr).EQ.0
    If (pos.EQ.0) bs=bs+1
  End If

  If (udf) Then
    dataunpack(i)=9.999e20
  Else  
    store=0.
    sbits=nbits
    Do While (cbits.LE.sbits)
      pwr=2**cbits
      store=store*real(pwr)+real(arr2int(dat(ds).AND.(pwr-1),1))
      ds=ds+1
      sbits=sbits-cbits
      cbits=8
    End Do
    If (sbits.GT.0) Then
      cbits=cbits-sbits
      pwr=2**sbits
      store=store*real(pwr)+real(rshift(dat(ds),8-sbits))
    End If
    dataunpack(i)=dataunpack(i)+store    
  End If
End Do

Where (dataunpack.NE.9.999e20)
  dataunpack=ref+rs*dataunpack
End Where

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine displays contents of the GRIB file
!

Subroutine displaygrib(elemlist,elemnum)

Implicit None

Integer, intent(in) :: elemnum
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
integer, dimension(0:49) :: duma
Character*80 elemtxt(1:3),lvltxt(1:2)
Character*80 gridtype
Real alonlat(1:3,1:2)
Integer i

! Display all grids in GRIB file
Write(6,*) "Grids located in GRIB file:"
Write(6,*) "Msg   Element  Grd Level            Date       En Comment"
Do i=1,elemnum
  duma=elemlist(i,:)
  Call getelemdesc(duma,elemtxt)
  Call getelemlonlat(duma,alonlat,gridtype)
  Call getlvltxt(duma,lvltxt)
  Write(6,'(T1I5,T8A,T17A,T21A,T38I4.4,T42I2.2,T44I2.2,T46I2.2,T49I2,T52A)') elemlist(i,0),elemtxt(1)(1:8),gridtype(1:3), &
    lvltxt(1)(1:16),elemlist(i,42),elemlist(i,43),elemlist(i,44),elemlist(i,45),elemlist(i,49),elemtxt(2)(1:27)
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function converts an array of 1byte integers into a single 4
! byte integer.  The result is probably best displayed as HEX.
!

Integer*4 Function arr2int(arr,n)

Implicit None

Integer, intent(in) :: n
Integer*1, dimension(1:n), intent(in) :: arr
Integer mul,i,tmp

arr2int=0
mul=1
Do i=n,1,-1
  tmp=arr(i)
  tmp=mod(tmp+256,256)
  arr2int=arr2int+tmp*mul
  mul=mul*256
End Do

If (arr2int.LT.0) arr2int=-2147483647-arr2int
Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function converts an array of 1byte integers into a single
! signed 4 byte integer.
!

Integer*4 Function signarr2int(arr,n)

Implicit None

Integer, intent(in) :: n
Integer*1, dimension(1:n), intent(in) :: arr
Integer lim
Integer arr2int

signarr2int=arr2int(arr,n)
lim=2**(8*n-1)
If (signarr2int.GT.lim) signarr2int=lim-signarr2int

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function converts an array of 1byte integers into a single
! float
!

Real Function arr2flt(arr)

Implicit None

Integer*1, dimension(1:4), intent(in) :: arr
Double precision low,pwr
Integer i,flg,x,st
Integer arr2int

pwr=1.
low=real(arr2int(arr(2:4),3))

st=abs((arr(1).and.127)-64)

Do i=1,6 ! should be 6 but too large for double precision
  flg=2.**(i-1)
  If ((st.AND.flg).NE.0) Then
    pwr=pwr*2.**(2**(i+1))
  End If
End Do
If ((arr(1).AND.64).EQ.0.) pwr=1./pwr
If ((arr(1).AND.128).NE.0) pwr=-pwr

arr2flt=pwr*low/16777216.0

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function determines if an array matches the text input.  The
! function returns the number of correct letters in the correct positions.
!

Integer Function chkgrib(dat,txt)

Implicit None

Integer*1, dimension(1:4), intent(in) :: dat
Character*4, intent(in) :: txt
Character*1 chars(1:4)
Integer i,pos
  
chars(:)=Char(dat(:))
chkgrib=0
Do i=1,4
  pos=Scan(txt,chars(i))
  If (pos.EQ.i) chkgrib=chkgrib+1
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function determines the size of the message (with error
! checking).
!

Integer function getmsgsize(dat)

Implicit None

Integer*1, dimension(1:16), intent(in) :: dat
Integer, dimension(1:2) :: msgsize
Integer arr2int

msgsize(1)=arr2int(dat(9:12),4)
msgsize(2)=arr2int(dat(13:16),4)

If (msgsize(1).NE.0) Then
  Write(6,*) "ERROR: Message size is too large for a 4byte integer"
  Stop
End If

getmsgsize=msgsize(2)

Return
End
