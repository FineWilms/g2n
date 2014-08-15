!
! SUBROUTINES FOR JOINING GRIB DATA INTO 3D FIELDS.
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine splices multiple grids into a single NetCDF file
! The subroutine checks to make sure that the multiple grids have
! compatable units, date, lon, lat, lvls, etc.
!

Subroutine mapsplice(splicelist,splicename,splicenum,datelist,datenum,elemlist,elemnum,ensnum)

Implicit None

Integer, intent(in) :: splicenum,elemnum,datenum,ensnum
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
Integer, dimension(1:splicenum,1:4), intent(out) :: splicelist
Integer, dimension(1:datenum,1:8), intent(in) :: datelist
Integer, dimension(1:4) :: tempsplicelist
integer, dimension(0:49) :: duma
integer, dimension(1:8) :: dumb
Integer i,j,t,pos(1),np(1)
Character(len=*), dimension(1:splicenum,1:3), intent(inout) :: splicename
Character*80, dimension(1:3) :: tempsplicename
Logical found,tfnd
Logical validsplice

splicelist=0

do i=1,splicenum
  tfnd=.false.
  t=0
  do while(.not.tfnd)
    t=t+1
    j=datelist(t,7)-1
    found=.false.
    do while (.not.found)
      j=j+1
      duma=elemlist(j,:)
      dumb=datelist(t,:)
      if (validsplice(duma,splicename(i,1),splicename(i,2),dumb,ensnum)) then
        if (splicelist(i,1).eq.0) then
          splicelist(i,1)=j
          splicelist(i,2)=t
        else
          splicelist(i,3)=1
        end if
        found=.true.
      End If
      found=found.or.(j.ge.datelist(t,8))
    end do
    tfnd=(splicelist(i,3).gt.0).or.(t.ge.datenum)
  end do
  If (splicelist(i,1).EQ.0) Write(6,*) "WARN:  Missing ",trim(splicename(i,1))
end do


! Sort splice list to improve speed...
Do i=1,splicenum-1
  pos=Minloc(splicelist(i:splicenum,1))+i-1
  If (pos(1).NE.i) then
    tempsplicename=splicename(pos(1),:)
    tempsplicelist=splicelist(pos(1),:)
    splicename(pos(1),:)=splicename(i,:)
    splicelist(pos(1),:)=splicelist(i,:)
    splicename(i,:)=tempsplicename
    splicelist(i,:)=tempsplicelist
  End if
End do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines the date, lon, lat and lvl for the output
! file.
!

Subroutine getsplicedim(alonlat,alvl,dimnum,splicelist,splicenum,datelist,datenum, &
                        elemlist,elemnum,ounit,levinvert,maxlvlnum,splicename,ensnum)

Implicit None

Integer, intent(in) :: splicenum,elemnum,maxlvlnum,datenum,ensnum
Integer, dimension(1:3), intent(inout) :: dimnum
Integer, dimension(1:splicenum,1:4), intent(inout) :: splicelist
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
Integer, dimension(1:datenum,1:8), intent(in) :: datelist
Integer, dimension(:), allocatable :: tmpfield
integer, dimension(1:8) :: duma
integer, dimension(0:49) :: dumc
Character(len=*), intent(in) :: ounit
Character(len=*), dimension(1:splicenum,1:3), intent(in) :: splicename
character*80, dimension(1:3) :: dumb
Real, dimension(1:2,1:3), intent(out) :: alonlat
Real, dimension(1:dimnum(3)), intent(out) :: alvl
Logical, intent(in) :: levinvert
Character*80 lunit,gridtype
Real, dimension(:), allocatable :: tmplvl,tmpblvl
Real newlonlat(1:2,1:3)
Real rnglvl,newrnglvl
Integer tmplvlnum,gridnum,origdimnum
Integer i,j,ta,pos
Logical istart

origdimnum=dimnum(3)
dimnum(3)=0 ! Reset level number (0 = no levels found)
alonlat=0.
alvl=0.
rnglvl=-1.

istart=.TRUE.
Do i=1,splicenum
  If (splicelist(i,1).gt.0) then
    pos=splicelist(i,2)

    ! Determine the number of levels and range of levels (max-min)
    tmplvlnum=maxlvlnum
    Allocate(tmpblvl(1:tmplvlnum),tmpfield(1:tmplvlnum))
    duma=datelist(pos,:)
    dumb=splicename(i,:)
    Call getfieldlvl(tmpblvl,tmpfield,tmplvlnum,lunit,duma,elemlist,elemnum,dumb,ensnum)
    Deallocate(tmpfield)
    Allocate(tmplvl(1:tmplvlnum),tmpfield(1:tmplvlnum))
    If (tmplvlnum.GT.maxlvlnum) then
      Write(6,*) "WARN: Level data buffer size exceeded - Allocating more memory"
      Call getfieldlvl(tmplvl,tmpfield,tmplvlnum,lunit,duma,elemlist,elemnum,dumb,ensnum)
    Else
      tmplvl(1:tmplvlnum)=tmpblvl(1:tmplvlnum)
    End if
    Deallocate(tmpblvl,tmpfield)    
    splicelist(i,4)=tmplvlnum
     
    If (tmplvlnum.GT.1) Call rescalelvl(tmplvl,tmplvlnum,lunit,ounit)
    newrnglvl=Maxval(tmplvl)-Minval(tmplvl)

    ! Assign level data
    If (newrnglvl.GT.rnglvl.or.(newrnglvl.eq.rnglvl.and.tmplvlnum.gt.dimnum(3))) then
      rnglvl=newrnglvl
      dimnum(3)=tmplvlnum

      ta=Min(tmplvlnum,origdimnum)
      If (levinvert) Then
        Do j=1,ta
          alvl(j)=tmplvl(tmplvlnum+1-j)
        End Do
      Else
        alvl(1:ta)=tmplvl(1:ta)	    
      End If
    End If

    ! Check lat/lon
    dumc=elemlist(splicelist(i,1),:)
    Call getelemlonlat(dumc,newlonlat,gridtype)    
  
    If (istart) then
      alonlat=newlonlat
      istart=.FALSE.
    End If

    If (Any(alonlat.NE.newlonlat)) Then
      Write(6,*) "WARN: Multiple coordinate formats in 3D field"
      Call replacelonlat(alonlat,newlonlat)
    End If
    
    Deallocate(tmplvl)
    
  End If
End Do

If ((alonlat(1,3).EQ.0.).OR.(alonlat(2,3).EQ.0.)) Then
  Write(6,*) "ERROR: Lat/Lon step equals zero"
  Stop
Else
  dimnum(1)=nInt(Abs(alonlat(1,2)-alonlat(1,1))/abs(alonlat(1,3)))+1
  dimnum(2)=nInt(Abs(alonlat(2,2)-alonlat(2,1))/abs(alonlat(2,3)))+1
  if (alonlat(1,1).eq.alonlat(1,2)) then
    dimnum(1)=int(360./alonlat(1,3))+1
  end if
End If

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine returns the levels of a specified field
!

Subroutine getfieldlvl(lvllist,fieldlist,lvlnum,lunit,datelist,elemlist,elemnum,splicename,ensnum)

Implicit None

Integer, intent(in) :: elemnum,ensnum
Integer, intent(inout) :: lvlnum
Character*80, intent(out) :: lunit
Character(len=*), dimension(1:3), intent(in) :: splicename
Real, dimension(1:lvlnum), intent(out) :: lvllist
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
Integer, dimension(1:8), intent(in) :: datelist
Integer, dimension(1:lvlnum), intent(out) :: fieldlist
integer, dimension(0:49) :: duma
Integer minpos(1)
Integer indx,origlvlnum
Integer i,ta
Character*80 surtxt(1:3)
Double Precision surfvalue, sndvalue
Real tmp

origlvlnum=lvlnum
lvllist=0.

! Determine number of levels
Call getfieldlist(fieldlist,lvlnum,datelist,elemlist,elemnum,splicename,ensnum)

! Create a list of levels
ta=Min(lvlnum,origlvlnum)
Do i=1,ta
  duma=elemlist(fieldlist(i),:)
  Call getelemlvl(duma,indx,surfvalue,sndvalue,surtxt)
  lvllist(i)=surfvalue
End Do

! Sort level list
! (very crude.  Need to fix this)
Do i=1,ta
  minpos=Minloc(lvllist(i:ta))
  tmp=lvllist(minpos(1)+i-1)
  lvllist(minpos(1)+i-1)=lvllist(i)
  lvllist(i)=tmp
End Do

lunit=surtxt(1)
  
Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine returns a list of grids corresponding to different
! levels of a specified element
!

Subroutine getfieldlist(fieldlist,fieldnum,datelist,elemlist,elemnum,splicename,ensnum)

Implicit None

Integer, intent(in) :: elemnum,ensnum
Integer, intent(inout) :: fieldnum
Integer, dimension(1:fieldnum), intent(out) :: fieldlist
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
Integer, dimension(1:8), intent(in) :: datelist
integer, dimension(0:49) :: duma
Character(len=*), dimension(1:3), intent(in) :: splicename
Integer fieldcount
Integer i
Logical validsplice

fieldlist=0
fieldcount=0

Do i=datelist(7),datelist(8)
  duma=elemlist(i,:)
  If (validsplice(duma,splicename(1),splicename(2),datelist,ensnum)) Then
    fieldcount=fieldcount+1
    If (fieldcount.LE.fieldnum) fieldlist(fieldcount)=i
  End If
End Do

fieldnum=fieldcount

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine displays contents of the splice
!

Subroutine displaysplice(splicelist,splicenum,elemlist,elemnum)

Implicit None

Integer, intent(in) :: splicenum,elemnum
Integer, dimension(1:splicenum,1:4), intent(in) :: splicelist
Integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
integer, dimension(0:49) :: duma
Character*80 elemtxt(1:3),surtxt(1:3)
Integer indx
Integer i
Double Precision surfvalue,sndvalue

! Display elements after splice
Write(6,*) "Grids remaining after splice:"
Write(6,*) "Num  Element  Unit     Level        Comment"
Do i=1,splicenum
  If (splicelist(i,1).gt.0) then
    duma=elemlist(splicelist(i,1),:)
    Call getelemdesc(duma,elemtxt)    
    Call getelemlvl(duma,indx,surfvalue,sndvalue,surtxt)
    Write(6,'(T1I4,T7A,T16A,T25A,T38A)') i,elemtxt(1)(1:8),elemtxt(3)(1:8),surtxt(1)(1:16),elemtxt(2)(1:41)
  Else
    Write(6,'(T1I4,T7A)') i,"Element not found"
  End If
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine interpolates 2D data to lat/lon grid
!

Subroutine interpolate2dgrid(rawslice,inlonlat,indimnum,rawfield,outlonlat,outdimnum,ingridnum,gridout)

Implicit None

Integer, intent(in) :: ingridnum,gridout
Integer, dimension(1:3), intent(in) :: indimnum,outdimnum
Real, dimension(1:indimnum(1),1:indimnum(2)), intent(in) :: rawslice
Real, dimension(1:outdimnum(1),1:outdimnum(2)), intent(out) :: rawfield
Real, dimension(1:2,1:3), intent(in) :: inlonlat,outlonlat
real, dimension(:), allocatable :: latarr
Integer i,j,a,b,adva,advb,jj,nsize
Real, dimension(1:2,1:2)  :: stp,rawdata
Real x,y,rawsum
Real ipol

! No conversion needed
If (All(inlonlat.EQ.outlonlat).AND.All(indimnum(1:2).EQ.outdimnum(1:2)) &
    .and.gridout.eq.ingridnum) Then
  rawfield=rawslice
  Return
End If

! select output grid
select case(gridout)

  case(0) ! reg
    
    ! Select coordinate systems to convert between
    Select Case(ingridnum)

      Case(0)

        ! Determine 'direction' of grid    
        Do i=1,2
          Do j=1,2
            If (j.EQ.1) Then
              If (inlonlat(i,1).GT.inlonlat(i,2)) Then
                stp(i,j)=-Abs(inlonlat(i,3))
              Else
                stp(i,j)=Abs(inlonlat(i,3))
              End If
            Else
              If (outlonlat(i,1).GT.outlonlat(i,2)) Then
                stp(i,j)=-Abs(outlonlat(i,3))
              Else
                stp(i,j)=Abs(outlonlat(i,3))
              End If
            End If
          End Do
        End Do
    
        Do i=1,outdimnum(1)
          Do j=1,outdimnum(2)
      
            x=(stp(1,2)*Real(i-1)+outlonlat(1,1)-inlonlat(1,1))/stp(1,1)+1.
            y=(stp(2,2)*Real(j-1)+outlonlat(2,1)-inlonlat(2,1))/stp(2,1)+1.
      
            a=Int(x)
            b=Int(y)
            x=x-Real(a)
            y=y-Real(b)
	
            adva=1
            advb=1
	
            If (a.GE.indimnum(1)) Then
              adva=0
              x=0.
            End If
	
            If (b.GE.indimnum(2)) Then
              advb=0
              y=0.
            End If

            rawdata=0.
            rawdata(1:1+adva,1:1+advb)=rawslice(a:a+adva,b:b+advb)
            if (any(rawdata.ne.0.)) then
              rawsum=sum(rawdata,rawdata.ne.0.)/real(count(rawdata.ne.0.))
              where (rawdata.eq.0.)
                rawdata=rawsum
              end where
            end if

            rawfield(i,j)=ipol(rawdata,x,y)
	
          End Do
        End Do  

      Case(10)
        Write(6,*) "ERROR: Conversion from Mercator to Lat/Lon is unsupported"
        Write(6,*) "       Please contact MJT and get him to fix this"
        Stop
      
      Case(20)
        Write(6,*) "ERROR: Conversion from Polar to Lat/Lon is unsupported"
        Write(6,*) "       Please contact MJT and get him to fix this"
        Stop
  
      Case(30)
        Write(6,*) "ERROR: Conversion from Lambert to Lat/Lon is unsupported"
        Write(6,*) "       Please contact MJT and get him to fix this"    
        Stop

      Case(40)
        ! Calculate gaussian coordinates
        nsize=2*nint((inlonlat(2,1)-inlonlat(2,2))/(2.*inlonlat(2,3)))
        if (nsize.ne.indimnum(2)) then
          write(6,*) "ERROR: Mismatch in gauss dimension"
          write(6,*) "nsize,indim(2) ",nsize,indimnum(2)
          stop
        end if
        allocate(latarr(nsize))
        call getgauss(inlonlat,latarr,nsize)

        ! Determine 'direction' of grid    
        Do i=1,2
          Do j=1,2
            If (j.EQ.1) Then
              If (inlonlat(i,1).GT.inlonlat(i,2)) Then
                stp(i,j)=-Abs(inlonlat(i,3))
              Else
                stp(i,j)=Abs(inlonlat(i,3))
              End If
            Else
              If (outlonlat(i,1).GT.outlonlat(i,2)) Then
                stp(i,j)=-Abs(outlonlat(i,3))
              Else
                stp(i,j)=Abs(outlonlat(i,3))
              End If
            End If
          End Do
        End Do
    
        Do i=1,outdimnum(1)
          Do j=1,outdimnum(2)
      
            x=(stp(1,2)*Real(i-1)+outlonlat(1,1)-inlonlat(1,1))/stp(1,1)+1.
	
            do jj=1,nsize-1
              y=stp(2,2)*Real(j-1)+outlonlat(2,1)-latarr(jj)
              y=y/(latarr(jj+1)-latarr(jj))
              if (y.ge.0..and.y.le.1.) then
                y=y+real(jj)
                exit
              end if
            end do
      
            a=Int(x)
            b=Int(y)
            x=x-Real(a)
            y=y-Real(b)
	
            adva=1
            advb=1
	
            If (a.GE.indimnum(1)) Then
              adva=0
              x=0.
            End If
	
            If (b.GE.indimnum(2)) Then
              advb=0
              y=0.
            End If
	
            rawdata=0.
            rawdata(1:1+adva,1:1+advb)=rawslice(a:a+adva,b:b+advb)
            if (any(rawdata.ne.0.)) then
              rawsum=sum(rawdata,rawdata.ne.0.)/real(count(rawdata.ne.0.))
              where (rawdata.eq.0.)
                rawdata=rawsum
              end where
            end if

            rawfield(i,j)=ipol(rawdata,x,y)
	
          End Do
        End Do
        deallocate(latarr)

      Case DEFAULT
        Write(6,*) "ERROR: 2D coordinate conversion is unsupported"
        Stop

    End Select

  case(10)
    Write(6,*) "ERROR: Conversion to Mercator is unsupported"
    Write(6,*) "       Please contact MJT and get him to fix this"
    Stop

  case(20)
    Write(6,*) "ERROR: Conversion to Polar is unsupported"
    Write(6,*) "       Please contact MJT and get him to fix this"
    Stop

  case(30)
    Write(6,*) "ERROR: Conversion to Lambert is unsupported"
    Write(6,*) "       Please contact MJT and get him to fix this"
    Stop

  case(40)
    Write(6,*) "ERROR: Conversion to Gaussian is unsupported"
    Write(6,*) "       Please contact MJT and get him to fix this"
    Stop  
    
  case DEFAULT
    Write(6,*) "ERROR: 2D coordinate conversion is unsupported"
    Stop

end select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine interpolates 3D data between two coordinate
! systems
!

Subroutine interpolate3dgrid(indata,inlvl,indimnum,outdata,outlvl,outdimnum,inputunit,outputunit)

Implicit None

Integer, dimension(1:3), intent(in) :: indimnum,outdimnum
Real, dimension(1:indimnum(1),1:indimnum(2),1:indimnum(3)), intent(in) :: indata
Real, dimension(1:indimnum(3)), intent(inout) :: inlvl
Real, dimension(1:outdimnum(1),1:outdimnum(2),1:outdimnum(3)), intent(out) :: outdata
Real, dimension(1:outdimnum(3)), intent(in) :: outlvl
Character(len=*), intent(in) :: inputunit
Character(len=*), intent(in) :: outputunit
Integer minlvlpos(1),maxlvlpos(1)
Integer i
Real x

outdata=0.

If (indimnum(3).EQ.1) then
  ! Surface data?
  outdata(:,:,1)=indata(:,:,1)
  Return
End if

If (All(indimnum.EQ.outdimnum)) Then
  If (All(inlvl.EQ.outlvl).AND.(inputunit.EQ.outputunit)) Then
    outdata=indata
    Return
  End If
End If

Call rescalelvl(inlvl,indimnum(3),inputunit,outputunit)

Do i=1,outdimnum(3)

  ! Interpolate between levels
  ! Find adjacent levels
  minlvlpos=Maxloc(inlvl,inlvl.LE.outlvl(i))
  maxlvlpos=Minloc(inlvl,inlvl.GE.outlvl(i))
  
  If ((minlvlpos(1).EQ.0).AND.(maxlvlpos(1).EQ.0)) Then
    Write(6,*) "ERROR: Cannot interpolate level"
    Stop
  End If

  ! Bottom level
  If (minlvlpos(1).EQ.0) minlvlpos(1)=maxlvlpos(1)
  
  ! Top level
  If (maxlvlpos(1).EQ.0) maxlvlpos(1)=minlvlpos(1)
  
  ! Calculate interpolated value (use linear interpolation for now)
  If (minlvlpos(1).EQ.maxlvlpos(1)) Then
    outdata(:,:,i)=indata(:,:,minlvlpos(1))
  Else
    x=(outlvl(i)-inlvl(minlvlpos(1)))/(inlvl(maxlvlpos(1))-inlvl(minlvlpos(1)))
    outdata(:,:,i)=(indata(:,:,maxlvlpos(1))-indata(:,:,minlvlpos(1)))*x+indata(:,:,minlvlpos(1))
  End If

End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine rescales levels
!

Subroutine rescalelvl(alvl,lvlnum,inputunit,ounit)

Implicit None

Integer, intent(in) :: lvlnum
Real, dimension(1:lvlnum), intent(inout) :: alvl
Character(len=*), intent(in) :: inputunit,ounit

If (lvlnum.EQ.1) Then
  ! Single level.  No need to convert
Else
  If (inputunit.EQ.ounit) Then
    ! No change
  Else
  
    Select Case(ounit)
      Case('hPa')
        Select Case(inputunit)
          Case('ISBL')
            alvl=alvl/100.
          Case DEFAULT
            Write(6,*) "ERROR: Level unit conversions are unsupported"
            Write(6,*) "       Please contact MJT and get him to fix this"
            Write(6,*) "ounit = ",ounit
            Write(6,*) "iunit = ",inputunit
            Stop
        End Select
      Case('m','meters')
        Select Case(inputunit)
          Case('DBLL')
            ! no change
          Case('HTGL')
            ! no change
          Case DEFAULT
            Write(6,*) "ERROR: Level unit conversions are unsupported"
            Write(6,*) "       Please contact MJT and get him to fix this"
            Stop
        End Select	    
      Case DEFAULT
        Write(6,*) "ERROR: Level unit conversions are unsupported"
        Write(6,*) "       Please contact MJT and get him to fix this"
        Stop
    End Select
      
  End If
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function determines if the element is part of a splice
!

Logical Function validsplice(alist,splicename,spliceunit,datelist,ensnum)

Implicit None

integer, intent(in) :: ensnum
Integer, dimension(0:49), intent(in) :: alist
Integer, dimension(1:8), intent(in) :: datelist
Character(len=*), intent(in) :: splicename
Character(len=*), intent(in) :: spliceunit
Character*80 elemtxt(1:3),surtxt(1:3)
Character*7 valtxt
Integer indx,pa,pc,pd,pe,px
Double Precision surfvalue,sndvalue
Logical tsta

if (ensnum.ne.0) then
  validsplice=(ensnum.eq.alist(49))
  if (.not.validsplice) return
end if

Call getelemdesc(alist,elemtxt)
validsplice=(elemtxt(1).EQ.splicename).and.All(datelist(1:6).eq.alist(42:47))
if (.not.validsplice) return

Call getelemlvl(alist,indx,surfvalue,sndvalue,surtxt)
validsplice=(surtxt(1).EQ.spliceunit)
if (validsplice) return

px=len(spliceunit)
pd=scan(spliceunit,'1234567890.')
if (pd.gt.0) then
  pe=verify(spliceunit(pd:px),'1234567890.')+pd-2
  if (pe.lt.pd) pe=px

  if (int(surfvalue).eq.surfvalue) then
    write(valtxt,'(I7)') nint(surfvalue)
  else
    write(valtxt,'(F7.1)') surfvalue
  end if
  pc=scan(valtxt,'1234567890.')
  pa=index(spliceunit,trim(surtxt(1)))
  if ((pe-pd).eq.(7-pc)) validsplice=(pa.gt.0).and.(spliceunit(pd:pe).eq.valtxt(pc:7))
End if

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine replaces the lon/lat with the maximum common area
!

Subroutine replacelonlat(alonlat,blonlat)

Implicit None

Real, dimension(2,3), intent(inout) :: alonlat
Real, dimension(2,3), intent(in) :: blonlat
Real mnva,mnvb,mxva,mxvb
Integer i

Do i=1,2
  mnva=Minval(alonlat(i,1:2))
  mxva=Maxval(alonlat(i,1:2))
  mnvb=Minval(blonlat(i,1:2))
  mxvb=Maxval(blonlat(i,1:2))
  
  alonlat(i,1)=Max(mnva,mnvb)
  alonlat(i,2)=Min(mxva,mxvb)
  alonlat(i,3)=Max(alonlat(i,3),blonlat(i,3))
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine groups dates in the GRIB file
!

subroutine mapdate(datelist,datenum,elemlist,elemnum)

implicit none

integer, intent(in) :: elemnum
integer, intent(inout) :: datenum
integer i,j,k,datenumin,pos
integer, dimension(1:datenum,1:8), intent(out) :: datelist
integer, dimension(1:elemnum,0:49), intent(in) :: elemlist
integer, dimension(1:elemnum,1:8) :: worklist
integer, dimension(1:8) :: tempdatelist
logical tflag

datenumin=datenum
datenum=0
datelist=0
worklist=0
do i=1,elemnum
  j=1
  tflag=.true.
  do while ((j.le.datenum).and.tflag)
    tflag=.not.All(elemlist(i,42:47).eq.worklist(j,1:6))
    if (tflag) j=j+1
  end do
  
  if (tflag) then
    datenum=j
    worklist(j,1:6)=elemlist(i,42:47)
  end if
end do

if (datenum.gt.1) then
  do k=1,datenum
    worklist(k,7)=elemnum+1
    worklist(k,8)=0
    do i=1,elemnum
      if (All(worklist(k,1:6).eq.elemlist(i,42:47))) then
        worklist(k,7)=min(i,worklist(k,7))
        worklist(k,8)=max(i,worklist(k,8))
      end if
    end do
  end do
else
  worklist(1,7)=1
  worklist(1,8)=elemnum
end if

! sort
do i=1,datenum-1
  pos=i
  do j=i+1,datenum
    k=1
    do while (k.le.6)
      if (worklist(j,k).lt.worklist(pos,k)) then
        pos=j
        k=9
      else if (worklist(j,k).gt.worklist(pos,k)) then
        k=9
      else
        k=k+1
      end if
    end do
  end do
  If (pos.NE.i) then
    tempdatelist(:)=worklist(pos,:)
    worklist(pos,:)=worklist(i,:)
    worklist(i,:)=tempdatelist(:)
  End if
End do

datenumin=min(datenumin,elemnum)
datelist(1:datenumin,:)=worklist(1:datenumin,:)

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine calculates the gaussian coordinates
! based on wgrib2
!

subroutine getgauss(latlon,latarr,nlat)

implicit none

integer, intent(in) :: nlat
real, dimension(2,3), intent(in) :: latlon
real, dimension(nlat), intent(out) :: latarr
integer nzero,i
real, dimension(nlat) :: cosc,colat,sinc
real, parameter :: pi = 3.1415927
real, parameter :: xlim = 1.E-7
real g,gm,gp,gt,delta,a,b,np1,d
real gord

np1=nlat+1
a=real(nlat)*real(np1)/sqrt(4.*real(np1)*real(np1)-1.)
b=real(np1)*real(nlat)/sqrt(4.*real(nlat)*real(nlat)-1.)

nzero=nlat/2

! The following code is based on geo.c from wgrib2
do i=1,nzero
  cosc(i)=sin((i-0.5)*pi/real(nlat)+pi*0.5)
end do
do i=1,nzero
  g=gord(nlat,cosc(i))
  gm=gord(nlat-1,cosc(i))
  gp=gord(nlat+1,cosc(i))
  gt=(cosc(i)*cosc(i)-1.)/(a*gp-b*gm)
  delta=g*gt
  cosc(i)=cosc(i)-delta
  do while (abs(delta).gt.xlim)
    g=gord(nlat,cosc(i))
    gm=gord(nlat-1,cosc(i))
    gp=gord(nlat+1,cosc(i))
    gt=(cosc(i)*cosc(i)-1.)/(a*gp-b*gm)
    delta=g*gt
    cosc(i)=cosc(i)-delta
  end do
end do
do i=1,nzero
  colat(i)=acos(cosc(i))
  sinc(i)=sin(colat(i))
end do
if (mod(nlat,2).ne.0) then
  i=nzero+1
  cosc(i)=0.
  d=gord(nlat-1,cosc(i))
  d=d*d*real(nlat)*real(nlat)
  colat(i)=pi*0.5
  sinc(i)=1.
end if
do i=nlat-nzero+1,nlat
  cosc(i)=-cosc(nlat+1-i)
  colat(i)=pi-colat(nlat+1-i)
  sinc(i)=sinc(nlat+1-i)
end do
do i=1,nlat
  latarr(i)=acos(sinc(i))*180./pi
  if (i.gt.nlat/2) latarr(i)=-latarr(i)
end do

return
end

real function gord(n,x)

implicit none

integer, intent(in) :: n
real, intent(in) :: x
real colat,c1,fn,ang,s1,c4,a,b,fi
integer i

colat=acos(x)
c1=sqrt(2.)
fn=real(n)
ang=fn*colat
s1=0.
c4=1.
a=-1.
b=0.

do i=1,n
  fi=real(i)
  c1=c1*sqrt(1.-1./(4.*fi*fi))
end do
do i=0,n,2
  if (i.eq.n) c4=0.5*c4
  s1=s1+c4*cos(ang)
  a=a+2.
  b=b+1.
  fi=real(i)
  ang=colat*(fn-fi-2.)
  c4=(a*(fn-b+1.)/(b*(fn+fn-a)))*c4
end do

gord=s1*c1

return
end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine calculates the mercator coordinates
! based on wgrib2
!

subroutine getmerc(latlon,latarr,nlat)

implicit none

integer, intent(in) :: nlat
real, dimension(2,3), intent(in) :: latlon
real, dimension(nlat), intent(out) :: latarr
real, parameter :: pi = 3.1415927
real n,s,dy,tmp
integer j

s=latlon(2,1)
n=latlon(2,2)

s=log(tan((45.+0.5*s)*pi/180.))
n=log(tan((45.+0.5*n)*pi/180.))
dy=(n-s)/real(nlat-1)

do j=1,nlat
  latarr(j)=(atan(exp(s+real(j-1)*dy))*180./pi-45.)*2.
end do

return
end
