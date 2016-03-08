!
! SUBROUTINES FOR DESCRIBING GRIB1/2 METADATA
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine finds GRIB1 or GRIB2 element names, comments and
! units
!

Subroutine getelemdesc(alist,elemtxt)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Character*80, dimension(1:3), intent(out) :: elemtxt

Select Case(alist(48))
  Case(1)
    Call getelemdesc1(alist,elemtxt)
  Case DEFAULT !GRIB2
    Call getelemdesc2(alist,elemtxt)
End Select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine finds the element name, comment and units for a
! GRIB1 file.  It also contains definitions for all GRIB1 elements
!

Subroutine getelemdesc1(alist,elemtxt)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80 elemerr(1:3)

elemerr='???'

Select Case(alist(1))
  Case(7) !NCEP
    Select Case(alist(2))
      Case(2) ! assume reanalysis
        elemtxt=elemerr
      Case DEFAULT ! assume opn
        call gncepopn(alist(5),elemtxt)
    End Select
  Case(46) !CPTEC
    elemtxt=elemerr      
  Case(78) !DWD
    elemtxt=elemerr      
  Case(98) !ECMWF
    Select Case(alist(3))
      Case (128)
        call gecmwf128(alist(5),elemtxt)
      Case (140)
        call gecmwf140(alist(5),elemtxt)
      Case (210)
        call gecmwf210(alist(5),elemtxt)
      Case DEFAULT
        elemtxt(1)='      '
        Write(elemtxt(1)(1:2),'(Z2.2)') alist(1)
        Write(elemtxt(1)(3:4),'(Z2.2)') alist(3)
        Write(elemtxt(1)(5:6),'(Z2.2)') alist(5)
        elemtxt(2:3)='?'
    End Select
  Case DEFAULT
    call gncepopn(alist(5),elemtxt)
End Select

! Check for errors
If ((elemtxt(1)==elemerr(1)).OR.(elemtxt(1)=='')) Then
  elemtxt(1)='      '
  Write(elemtxt(1)(1:2),'(Z2.2)') alist(1)
  Write(elemtxt(1)(3:4),'(Z2.2)') alist(2)
  Write(elemtxt(1)(5:6),'(Z2.2)') alist(5)
  elemtxt(2:3)='?'
End If


Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NCEP Metadata
!

subroutine gncepopn(ain,elemtxt)

implicit none

Integer, intent(in) :: ain
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80, save :: ncepopn(0:255,1:3)
logical, save :: first=.true.

if (first) then

  ncepopn=""
  ncepopn(1,1)="PRES"
  ncepopn(1,2)="Pressure"
  ncepopn(1,3)="Pa"
  ncepopn(2,1)="PRMSL"
  ncepopn(2,2)="Pressure reduced to MSL"
  ncepopn(2,3)="Pa"
  ncepopn(3,1)="PTEND"
  ncepopn(3,2)="Pressure tendency"
  ncepopn(3,3)="Pa/s"
  ncepopn(4,1)="PVORT"
  ncepopn(4,2)="Pot. vorticity"
  ncepopn(4,3)="km^2/kg/s"
  ncepopn(5,1)="ICAHT"
  ncepopn(5,2)="ICAO Standard Atmosphere Reference Height"
  ncepopn(5,3)="M"
  ncepopn(6,1)="GP"
  ncepopn(6,2)="Geopotential"
  ncepopn(6,3)="m^2/s^2"
  ncepopn(7,1)="HGT"
  ncepopn(7,2)="Geopotential height"
  ncepopn(7,3)="gpm"
  ncepopn(8,1)="DIST"
  ncepopn(8,2)="Geometric height"
  ncepopn(8,3)="m"
  ncepopn(9,1)="HSTDV"
  ncepopn(9,2)="Std dev of height"
  ncepopn(9,3)="m"
  ncepopn(10,1)="TOZNE"
  ncepopn(10,2)="Total ozone"
  ncepopn(10,3)="Dobson"
  ncepopn(11,1)="TMP"
  ncepopn(11,2)="Temp."
  ncepopn(11,3)="K"
  ncepopn(12,1)="VTMP"
  ncepopn(12,2)="Virtual temp."
  ncepopn(12,3)="K"
  ncepopn(13,1)="POT"
  ncepopn(13,2)="Potential temp."
  ncepopn(13,3)="K"
  ncepopn(14,1)="EPOT"
  ncepopn(14,2)="Pseudo-adiabatic pot. temp."
  ncepopn(14,3)="K"
  ncepopn(15,1)="TMAX"
  ncepopn(15,2)="Max. temp."
  ncepopn(15,3)="K"
  ncepopn(16,1)="TMIN"
  ncepopn(16,2)="Min. temp."
  ncepopn(16,3)="K"
  ncepopn(17,1)="DPT"
  ncepopn(17,2)="Dew point temp."
  ncepopn(17,3)="K"
  ncepopn(18,1)="DEPR"
  ncepopn(18,2)="Dew point depression"
  ncepopn(18,3)="K"
  ncepopn(19,1)="LAPR"
  ncepopn(19,2)="Lapse rate"
  ncepopn(19,3)="K/m"
  ncepopn(20,1)="VIS"
  ncepopn(20,2)="Visibility"
  ncepopn(20,3)="m"
  ncepopn(21,1)="RDSP1"
  ncepopn(21,2)="Radar spectra (1)"
  ncepopn(21,3)="non-dim"
  ncepopn(22,1)="RDSP2"
  ncepopn(22,2)="Radar spectra (2)"
  ncepopn(22,3)="non-dim"
  ncepopn(23,1)="RDSP3"
  ncepopn(23,2)="Radar spectra (3)"
  ncepopn(23,3)="non-dim"
  ncepopn(24,1)="PLI"
  ncepopn(24,2)="Parcel lifted index (to 500 hPa)"
  ncepopn(24,3)="K"
  ncepopn(25,1)="TMPA"
  ncepopn(25,2)="Temp. anomaly"
  ncepopn(25,3)="K"
  ncepopn(26,1)="PRESA"
  ncepopn(26,2)="Pressure anomaly"
  ncepopn(26,3)="Pa"
  ncepopn(27,1)="GPA"
  ncepopn(27,2)="Geopotential height anomaly"
  ncepopn(27,3)="gpm"
  ncepopn(28,1)="WVSP1"
  ncepopn(28,2)="Wave spectra (1)"
  ncepopn(28,3)="non-dim"
  ncepopn(29,1)="WVSP2"
  ncepopn(29,2)="Wave spectra (2)"
  ncepopn(29,3)="non-dim"
  ncepopn(30,1)="WVSP3"
  ncepopn(30,2)="Wave spectra (3)"
  ncepopn(30,3)="non-dim"
  ncepopn(31,1)="WDIR"
  ncepopn(31,2)="Wind direction"
  ncepopn(31,3)="deg"
  ncepopn(32,1)="WIND"
  ncepopn(32,2)="Wind speed"
  ncepopn(32,3)="m/s"
  ncepopn(33,1)="UGRD"
  ncepopn(33,2)="u wind"
  ncepopn(33,3)="m/s"
  ncepopn(34,1)="VGRD"
  ncepopn(34,2)="v wind"
  ncepopn(34,3)="m/s"
  ncepopn(35,1)="STRM"
  ncepopn(35,2)="Stream function"
  ncepopn(35,3)="m^2/s"
  ncepopn(36,1)="VPOT"
  ncepopn(36,2)="Velocity potential"
  ncepopn(36,3)="m^2/s"
  ncepopn(37,1)="MNTSF"
  ncepopn(37,2)="Montgomery stream function"
  ncepopn(37,3)="m^2/s^2"
  ncepopn(38,1)="SGCVV"
  ncepopn(38,2)="Sigma coord. vertical velocity"
  ncepopn(38,3)="/s"
  ncepopn(39,1)="VVEL"
  ncepopn(39,2)="Pressure vertical velocity"
  ncepopn(39,3)="Pa/s"
  ncepopn(40,1)="DZDT"
  ncepopn(40,2)="Geometric vertical velocity"
  ncepopn(40,3)="m/s"
  ncepopn(41,1)="ABSV"
  ncepopn(41,2)="Absolute vorticity "
  ncepopn(41,3)="/s"
  ncepopn(42,1)="ABSD"
  ncepopn(42,2)="Absolute divergence"
  ncepopn(42,3)="/s"
  ncepopn(43,1)="RELV"
  ncepopn(43,2)="Relative vorticity"
  ncepopn(43,3)="/s"
  ncepopn(44,1)="RELD"
  ncepopn(44,2)="Relative divergence"
  ncepopn(44,3)="/s"
  ncepopn(45,1)="VUCSH"
  ncepopn(45,2)="Vertical u shear"
  ncepopn(45,3)="/s"
  ncepopn(46,1)="VVCSH"
  ncepopn(46,2)="Vertical v shear"
  ncepopn(46,3)="/s"
  ncepopn(47,1)="DIRC"
  ncepopn(47,2)="Direction of current"
  ncepopn(47,3)="deg"
  ncepopn(48,1)="SPC"
  ncepopn(48,2)="Speed of current"
  ncepopn(48,3)="m/s"
  ncepopn(49,1)="UOGRD"
  ncepopn(49,2)="u of current"
  ncepopn(49,3)="m/s"
  ncepopn(50,1)="VOGRD"
  ncepopn(50,2)="v of current"
  ncepopn(50,3)="m/s"
  ncepopn(51,1)="SPFH"
  ncepopn(51,2)="Specific humidity"
  ncepopn(51,3)="kg/kg"
  ncepopn(52,1)="RH"
  ncepopn(52,2)="Relative humidity"
  ncepopn(52,3)="%"
  ncepopn(53,1)="MIXR"
  ncepopn(53,2)="Humidity mixing ratio"
  ncepopn(53,3)="kg/kg"
  ncepopn(54,1)="PWAT"
  ncepopn(54,2)="Precipitable water"
  ncepopn(54,3)="kg/m^2"
  ncepopn(55,1)="VAPP"
  ncepopn(55,2)="Vapor pressure"
  ncepopn(55,3)="Pa"
  ncepopn(56,1)="SATD"
  ncepopn(56,2)="Saturation deficit"
  ncepopn(56,3)="Pa"
  ncepopn(57,1)="EVP"
  ncepopn(57,2)="Evaporation"
  ncepopn(57,3)="kg/m^2"
  ncepopn(58,1)="CICE"
  ncepopn(58,2)="Cloud Ice"
  ncepopn(58,3)="kg/m^2"
  ncepopn(59,1)="PRATE"
  ncepopn(59,2)="Precipitation rate"
  ncepopn(59,3)="kg/m^2/s"
  ncepopn(60,1)="TSTM"
  ncepopn(60,2)="Thunderstorm probability"
  ncepopn(60,3)="%"
  ncepopn(61,1)="APCP"
  ncepopn(61,2)="Total precipitation"
  ncepopn(61,3)="kg/m^2"
  ncepopn(62,1)="NCPCP"
  ncepopn(62,2)="Large scale precipitation"
  ncepopn(62,3)="kg/m^2"
  ncepopn(63,1)="ACPCP"
  ncepopn(63,2)="Convective precipitation"
  ncepopn(63,3)="kg/m^2"
  ncepopn(64,1)="SRWEQ"
  ncepopn(64,2)="Snowfall rate water equiv."
  ncepopn(64,3)="kg/m^2/s"
  ncepopn(65,1)="WEASD"
  ncepopn(65,2)="Accum. snow"
  ncepopn(65,3)="kg/m^2"
  ncepopn(66,1)="SNOD"
  ncepopn(66,2)="Snow depth"
  ncepopn(66,3)="m"
  ncepopn(67,1)="MIXHT"
  ncepopn(67,2)="Mixed layer depth"
  ncepopn(67,3)="m"
  ncepopn(68,1)="TTHDP"
  ncepopn(68,2)="Transient thermocline depth"
  ncepopn(68,3)="m"
  ncepopn(69,1)="MTHD"
  ncepopn(69,2)="Main thermocline depth"
  ncepopn(69,3)="m"
  ncepopn(70,1)="MTHA"
  ncepopn(70,2)="Main thermocline anomaly"
  ncepopn(70,3)="m"
  ncepopn(71,1)="TCDC"
  ncepopn(71,2)="Total cloud cover"
  ncepopn(71,3)="%"
  ncepopn(72,1)="CDCON"
  ncepopn(72,2)="Convective cloud cover"
  ncepopn(72,3)="%"
  ncepopn(73,1)="LCDC"
  ncepopn(73,2)="Low level cloud cover"
  ncepopn(73,3)="%"
  ncepopn(74,1)="MCDC"
  ncepopn(74,2)="Mid level cloud cover"
  ncepopn(74,3)="%"
  ncepopn(75,1)="HCDC"
  ncepopn(75,2)="High level cloud cover"
  ncepopn(75,3)="%"
  ncepopn(76,1)="CWAT"
  ncepopn(76,2)="Cloud water"
  ncepopn(76,3)="kg/m^2"
  ncepopn(77,1)="BLI"
  ncepopn(77,2)="Best lifted index (to 500 hPa)"
  ncepopn(77,3)="K"
  ncepopn(78,1)="SNOC"
  ncepopn(78,2)="Convective snow"
  ncepopn(78,3)="kg/m^2"
  ncepopn(79,1)="SNOL"
  ncepopn(79,2)="Large scale snow"
  ncepopn(79,3)="kg/m^2"
  ncepopn(80,1)="WTMP"
  ncepopn(80,2)="Water temp."
  ncepopn(80,3)="K"
  ncepopn(81,1)="LAND"
  ncepopn(81,2)="Land cover (land=1;sea=0)"
  ncepopn(81,3)="fraction"
  ncepopn(82,1)="DSLM"
  ncepopn(82,2)="Deviation of sea level from mean"
  ncepopn(82,3)="m"
  ncepopn(83,1)="SFCR"
  ncepopn(83,2)="Surface roughness"
  ncepopn(83,3)="m"
  ncepopn(84,1)="ALBDO"
  ncepopn(84,2)="Albedo"
  ncepopn(84,3)="%"
  ncepopn(85,1)="TSOIL"
  ncepopn(85,2)="Soil temp."
  ncepopn(85,3)="K"
  ncepopn(86,1)="SOILM"
  ncepopn(86,2)="Soil moisture content"
  ncepopn(86,3)="kg/m^2"
  ncepopn(87,1)="VEG"
  ncepopn(87,2)="Vegetation"
  ncepopn(87,3)="%"
  ncepopn(88,1)="SALTY"
  ncepopn(88,2)="Salinity"
  ncepopn(88,3)="kg/kg"
  ncepopn(89,1)="DEN"
  ncepopn(89,2)="Density"
  ncepopn(89,3)="kg/m^3"
  ncepopn(90,1)="WATR"
  ncepopn(90,2)="Water runoff"
  ncepopn(90,3)="kg/m^2"
  ncepopn(91,1)="ICEC"
  ncepopn(91,2)="Ice concentration (ice=1;no ice=0)"
  ncepopn(91,3)="fraction"
  ncepopn(92,1)="ICETK"
  ncepopn(92,2)="Ice thickness"
  ncepopn(92,3)="m"
  ncepopn(93,1)="DICED"
  ncepopn(93,2)="Direction of ice drift"
  ncepopn(93,3)="deg"
  ncepopn(94,1)="SICED"
  ncepopn(94,2)="Speed of ice drift"
  ncepopn(94,3)="m/s"
  ncepopn(95,1)="UICE"
  ncepopn(95,2)="u of ice drift"
  ncepopn(95,3)="m/s"
  ncepopn(96,1)="VICE"
  ncepopn(96,2)="v of ice drift"
  ncepopn(96,3)="m/s"
  ncepopn(97,1)="ICEG"
  ncepopn(97,2)="Ice growth rate"
  ncepopn(97,3)="m/s"
  ncepopn(98,1)="ICED"
  ncepopn(98,2)="Ice divergence"
  ncepopn(98,3)="/s"
  ncepopn(99,1)="SNOM"
  ncepopn(99,2)="Snow melt"
  ncepopn(99,3)="kg/m^2"
  ncepopn(100,1)="HTSGW"
  ncepopn(100,2)="Sig height of wind waves and swell"
  ncepopn(100,3)="m"
  ncepopn(101,1)="WVDIR"
  ncepopn(101,2)="Direction of wind waves"
  ncepopn(101,3)="deg"
  ncepopn(102,1)="WVHGT"
  ncepopn(102,2)="Sig height of wind waves"
  ncepopn(102,3)="m"
  ncepopn(103,1)="WVPER"
  ncepopn(103,2)="Mean period of wind waves"
  ncepopn(103,3)="s"
  ncepopn(104,1)="SWDIR"
  ncepopn(104,2)="Direction of swell waves"
  ncepopn(104,3)="deg"
  ncepopn(105,1)="SWELL"
  ncepopn(105,2)="Sig height of swell waves"
  ncepopn(105,3)="m"
  ncepopn(106,1)="SWPER"
  ncepopn(106,2)="Mean period of swell waves"
  ncepopn(106,3)="s"
  ncepopn(107,1)="DIRPW"
  ncepopn(107,2)="Primary wave direction"
  ncepopn(107,3)="deg"
  ncepopn(108,1)="PERPW"
  ncepopn(108,2)="Primary wave mean period"
  ncepopn(108,3)="s"
  ncepopn(109,1)="DIRSW"
  ncepopn(109,2)="Secondary wave direction"
  ncepopn(109,3)="deg"
  ncepopn(110,1)="PERSW"
  ncepopn(110,2)="Secondary wave mean period"
  ncepopn(110,3)="s"
  ncepopn(111,1)="NSWRS"
  ncepopn(111,2)="Net short wave (surface)"
  ncepopn(111,3)="W/m^2"
  ncepopn(112,1)="NLWRS"
  ncepopn(112,2)="Net long wave (surface)"
  ncepopn(112,3)="W/m^2"
  ncepopn(113,1)="NSWRT"
  ncepopn(113,2)="Net short wave (top)"
  ncepopn(113,3)="W/m^2"
  ncepopn(114,1)="NLWRT"
  ncepopn(114,2)="Net long wave (top)"
  ncepopn(114,3)="W/m^2"
  ncepopn(115,1)="LWAVR"
  ncepopn(115,2)="Long wave"
  ncepopn(115,3)="W/m^2"
  ncepopn(116,1)="SWAVR"
  ncepopn(116,2)="Short wave"
  ncepopn(116,3)="W/m^2"
  ncepopn(117,1)="GRAD"
  ncepopn(117,2)="Global radiation"
  ncepopn(117,3)="W/m^2"
  ncepopn(118,1)="BRTMP"
  ncepopn(118,2)="Brightness temperature"
  ncepopn(118,3)="K"
  ncepopn(119,1)="LWRAD"
  ncepopn(119,2)="Radiance with respect to wave no."
  ncepopn(119,3)="W/m/sr"
  ncepopn(120,1)="SWRAD"
  ncepopn(120,2)="Radiance with respect ot wave len."
  ncepopn(120,3)="W/m^3/sr"
  ncepopn(121,1)="LHTFL"
  ncepopn(121,2)="Latent heat flux"
  ncepopn(121,3)="W/m^2"
  ncepopn(122,1)="SHTFL"
  ncepopn(122,2)="Sensible heat flux"
  ncepopn(122,3)="W/m^2"
  ncepopn(123,1)="BLYDP"
  ncepopn(123,2)="Boundary layer dissipation"
  ncepopn(123,3)="W/m^2"
  ncepopn(124,1)="UFLX"
  ncepopn(124,2)="Zonal momentum flux"
  ncepopn(124,3)="N/m^2"
  ncepopn(125,1)="VFLX"
  ncepopn(125,2)="Meridional momentum flux"
  ncepopn(125,3)="N/m^2"
  ncepopn(126,1)="WMIXE"
  ncepopn(126,2)="Wind mixing energy"
  ncepopn(126,3)="J"
  ncepopn(127,1)="IMGD"
  ncepopn(127,2)="Image data"
  ncepopn(127,3)=""
  ncepopn(128,1)="MSLSA"
  ncepopn(128,2)="Mean sea level pressure (Std Atm)"
  ncepopn(128,3)="Pa"
  ncepopn(129,1)="MSLMA"
  ncepopn(129,2)="Mean sea level pressure (MAPS)"
  ncepopn(129,3)="Pa"
  ncepopn(130,1)="MSLET"
  ncepopn(130,2)="Mean sea level pressure (ETA model)"
  ncepopn(130,3)="Pa"
  ncepopn(131,1)="LFTX"
  ncepopn(131,2)="Surface lifted index"
  ncepopn(131,3)="K"
  ncepopn(132,1)="4LFTX"
  ncepopn(132,2)="Best (4-layer) lifted index"
  ncepopn(132,3)="K"
  ncepopn(133,1)="KX"
  ncepopn(133,2)="K index"
  ncepopn(133,3)="K"
  ncepopn(134,1)="SX"
  ncepopn(134,2)="Sweat index"
  ncepopn(134,3)="K"
  ncepopn(135,1)="MCONV"
  ncepopn(135,2)="Horizontal moisture divergence"
  ncepopn(135,3)="kg/kg/s"
  ncepopn(136,1)="VWSH"
  ncepopn(136,2)="Vertical speed shear"
  ncepopn(136,3)="1/s"
  ncepopn(137,1)="TSLSA"
  ncepopn(137,2)="3-hr pressure tendency (Std Atmos Red)"
  ncepopn(137,3)="Pa/s"
  ncepopn(138,1)="BVF2"
  ncepopn(138,2)="Brunt-Vaisala frequency^2"
  ncepopn(138,3)="1/s^2"
  ncepopn(139,1)="PVMW"
  ncepopn(139,2)="Potential vorticity (mass-weighted)"
  ncepopn(139,3)="1/s/m"
  ncepopn(140,1)="CRAIN"
  ncepopn(140,2)="Categorical rain"
  ncepopn(140,3)="yes=1;no=0"
  ncepopn(141,1)="CFRZR"
  ncepopn(141,2)="Categorical freezing rain"
  ncepopn(141,3)="yes=1;no=0"
  ncepopn(142,1)="CICEP"
  ncepopn(142,2)="Categorical ice pellets"
  ncepopn(142,3)="yes=1;no=0"
  ncepopn(143,1)="CSNOW"
  ncepopn(143,2)="Categorical snow"
  ncepopn(143,3)="yes=1;no=0"
  ncepopn(144,1)="SOILW"
  ncepopn(144,2)="Volumetric soil moisture"
  ncepopn(144,3)="fraction"
  ncepopn(145,1)="PEVPR"
  ncepopn(145,2)="Potential evaporation rate"
  ncepopn(145,3)="W/m^2"
  ncepopn(146,1)="CWORK"
  ncepopn(146,2)="Cloud work function"
  ncepopn(146,3)="J/kg"
  ncepopn(147,1)="U-GWD"
  ncepopn(147,2)="Zonal gravity wave stress"
  ncepopn(147,3)="N/m^2"
  ncepopn(148,1)="V-GWD"
  ncepopn(148,2)="Meridional gravity wave stress"
  ncepopn(148,3)="N/m^2"
  ncepopn(149,1)="PV"
  ncepopn(149,2)="Potential vorticity"
  ncepopn(149,3)="m^2/s/kg"
  ncepopn(150,1)="COVMZ"
  ncepopn(150,2)="Covariance between u and v"
  ncepopn(150,3)="m^2/s^2"
  ncepopn(151,1)="COVTZ"
  ncepopn(151,2)="Covariance between u and T"
  ncepopn(151,3)="K*m/s"
  ncepopn(152,1)="COVTM"
  ncepopn(152,2)="Covariance between v and T"
  ncepopn(152,3)="K*m/s"
  ncepopn(153,1)="CLWMR"
  ncepopn(153,2)="Cloud water"
  ncepopn(153,3)="kg/kg"
  ncepopn(154,1)="O3MR"
  ncepopn(154,2)="Ozone mixing ratio"
  ncepopn(154,3)="kg/kg"
  ncepopn(155,1)="GFLUX"
  ncepopn(155,2)="Ground heat flux"
  ncepopn(155,3)="W/m^2"
  ncepopn(156,1)="CIN"
  ncepopn(156,2)="Convective inhibition"
  ncepopn(156,3)="J/kg"
  ncepopn(157,1)="CAPE"
  ncepopn(157,2)="Convective Avail. Pot. Energy"
  ncepopn(157,3)="J/kg"
  ncepopn(158,1)="TKE"
  ncepopn(158,2)="Turbulent kinetic energy"
  ncepopn(158,3)="J/kg"
  ncepopn(159,1)="CONDP"
  ncepopn(159,2)="Lifted parcel condensation pressure"
  ncepopn(159,3)="Pa"
  ncepopn(160,1)="CSUSF"
  ncepopn(160,2)="Clear sky upward solar flux"
  ncepopn(160,3)="W/m^2"
  ncepopn(161,1)="CSDSF"
  ncepopn(161,2)="Clear sky downward solar flux"
  ncepopn(161,3)="W/m^2"
  ncepopn(162,1)="CSULF"
  ncepopn(162,2)="Clear sky upward long wave flux"
  ncepopn(162,3)="W/m^2"
  ncepopn(163,1)="CSDLF"
  ncepopn(163,2)="Clear sky downward long wave flux"
  ncepopn(163,3)="W/m^2"
  ncepopn(164,1)="CFNSF"
  ncepopn(164,2)="Cloud forcing net solar flux"
  ncepopn(164,3)="W/m^2"
  ncepopn(165,1)="CFNLF"
  ncepopn(165,2)="Cloud forcing net long wave flux"
  ncepopn(165,3)="W/m^2"
  ncepopn(166,1)="VBDSF"
  ncepopn(166,2)="Visible beam downward solar flux"
  ncepopn(166,3)="W/m^2"
  ncepopn(167,1)="VDDSF"
  ncepopn(167,2)="Visible diffuse downward solar flux"
  ncepopn(167,3)="W/m^2"
  ncepopn(168,1)="NBDSF"
  ncepopn(168,2)="Near IR beam downward solar flux"
  ncepopn(168,3)="W/m^2"
  ncepopn(169,1)="NDDSF"
  ncepopn(169,2)="Near IR diffuse downward solar flux"
  ncepopn(169,3)="W/m^2"
  ncepopn(170,1)="RWMR"
  ncepopn(170,2)="Rain water mixing ratio"
  ncepopn(170,3)="kg/kg" 
  ncepopn(171,1)="SNMR"
  ncepopn(171,2)="Snow mixing ratio"
  ncepopn(171,3)="kg/kg"
  ncepopn(172,1)="MFLX"
  ncepopn(172,2)="Momentum flux"
  ncepopn(172,3)="N/m^2"
  ncepopn(173,1)="LMH"
  ncepopn(173,2)="Mass point model surface"
  ncepopn(173,3)="non-dim"
  ncepopn(174,1)="LMV"
  ncepopn(174,2)="Velocity point model surface"
  ncepopn(174,3)="non-dim"
  ncepopn(175,1)="MLYNO"
  ncepopn(175,2)="Model layer number (from bottom up)"
  ncepopn(175,3)="non-dim"
  ncepopn(176,1)="NLAT"
  ncepopn(176,2)="Latitude (-90 to +90)"
  ncepopn(176,3)="deg"
  ncepopn(177,1)="ELON"
  ncepopn(177,2)="East longitude (0-360)"
  ncepopn(177,3)="deg"
  ncepopn(178,1)="ICMR"
  ncepopn(178,2)="Ice mixing ratio"
  ncepopn(178,3)="kg/kg"
  ncepopn(179,1)="GRMR"
  ncepopn(179,2)="Graupel mixing ratio"
  ncepopn(179,3)="kg/kg"
  ncepopn(180,1)="GUST"
  ncepopn(180,2)="Surface wind gust"
  ncepopn(180,3)="m/s"
  ncepopn(181,1)="LPSX"
  ncepopn(181,2)="x-gradient of log pressure"
  ncepopn(181,3)="1/m"
  ncepopn(182,1)="LPSY"
  ncepopn(182,2)="y-gradient of log pressure"
  ncepopn(182,3)="1/m"
  ncepopn(183,1)="HGTX"
  ncepopn(183,2)="x-gradient of height"
  ncepopn(183,3)="m/m"
  ncepopn(184,1)="HGTY"
  ncepopn(184,2)="y-gradient of height"
  ncepopn(184,3)="m/m"
  ncepopn(185,1)="TURB"
  ncepopn(185,2)="Turbulence SIGMET/AIRMET"
  ncepopn(185,3)="non-dim"
  ncepopn(186,1)="ICNG"
  ncepopn(186,2)="Icing SIGMET/AIRMET"
  ncepopn(186,3)="non-dim"
  ncepopn(187,1)="LTNG"
  ncepopn(187,2)="Lightning"
  ncepopn(187,3)="non-dim"
  ncepopn(188,1)="DRIP"
  ncepopn(188,2)="Rate of water dropping from canopy to gnd"
  ncepopn(188,3)="kg/m^2"
  ncepopn(189,1)="VPTMP"
  ncepopn(189,2)="Virtual pot. temp."
  ncepopn(189,3)="K"
  ncepopn(190,1)="HLCY"
  ncepopn(190,2)="Storm relative helicity"
  ncepopn(190,3)="m^2/s^2"
  ncepopn(191,1)="PROB"
  ncepopn(191,2)="Prob. from ensemble"
  ncepopn(191,3)="non-dim"
  ncepopn(192,1)="PROBN"
  ncepopn(192,2)="Prob. from ensemble norm. to clim. expect."
  ncepopn(192,3)="non-dim"
  ncepopn(193,1)="POP"
  ncepopn(193,2)="Prob. of precipitation"
  ncepopn(193,3)="%"
  ncepopn(194,1)="CPOFP"
  ncepopn(194,2)="Prob. of frozen precipitation"
  ncepopn(194,3)="%"
  ncepopn(195,1)="CPOZP"
  ncepopn(195,2)="Prob. of freezing precipitation"
  ncepopn(195,3)="%"
  ncepopn(196,1)="USTM"
  ncepopn(196,2)="u-component of storm motion"
  ncepopn(196,3)="m/s"
  ncepopn(197,1)="VSTM"
  ncepopn(197,2)="v-component of storm motion"
  ncepopn(197,3)="m/s"
  ncepopn(198,1)="NCIP"
  ncepopn(198,2)="No. concen. ice particles"
  ncepopn(198,3)=""
  ncepopn(199,1)="EVBS"
  ncepopn(199,2)="Direct evaporation from bare soil"
  ncepopn(199,3)="W/m^2"
  ncepopn(200,1)="EVCW"
  ncepopn(200,2)="Canopy water evaporation"
  ncepopn(200,3)="W/m^2"
  ncepopn(201,1)="ICWAT"
  ncepopn(201,2)="Ice-free water surface"
  ncepopn(201,3)="%"
  ncepopn(202,1)="CWDI"
  ncepopn(202,2)="Convective weather detection index"
  ncepopn(202,3)=""
  ncepopn(203,1)="VAFTAD"
  ncepopn(203,2)="VAFTAD??"
  ncepopn(203,3)="??"
  ncepopn(204,1)="DSWRF"
  ncepopn(204,2)="Downward short wave flux"
  ncepopn(204,3)="W/m^2"
  ncepopn(205,1)="DLWRF"
  ncepopn(205,2)="Downward long wave flux"
  ncepopn(205,3)="W/m^2"
  ncepopn(206,1)="UVI"
  ncepopn(206,2)="Ultra violet index (1 hour centered at solar noon)"
  ncepopn(206,3)="J/m^2"
  ncepopn(207,1)="MSTAV"
  ncepopn(207,2)="Moisture availability"
  ncepopn(207,3)="%"
  ncepopn(208,1)="SFEXC"
  ncepopn(208,2)="Exchange coefficient"
  ncepopn(208,3)="(kg/m^3)(m/s)"
  ncepopn(209,1)="MIXLY"
  ncepopn(209,2)="No. of mixed layers next to surface"
  ncepopn(209,3)="integer"
  ncepopn(210,1)="TRANS"
  ncepopn(210,2)="Transpiration"
  ncepopn(210,3)="W/m^2"
  ncepopn(211,1)="USWRF"
  ncepopn(211,2)="Upward short wave flux"
  ncepopn(211,3)="W/m^2"
  ncepopn(212,1)="ULWRF"
  ncepopn(212,2)="Upward long wave flux"
  ncepopn(212,3)="W/m^2"
  ncepopn(213,1)="CDLYR"
  ncepopn(213,2)="Non-convective cloud"
  ncepopn(213,3)="%"
  ncepopn(214,1)="CPRAT"
  ncepopn(214,2)="Convective precip. rate"
  ncepopn(214,3)="kg/m^2/s"
  ncepopn(215,1)="TTDIA"
  ncepopn(215,2)="Temp. tendency by all physics"
  ncepopn(215,3)="K/s"
  ncepopn(216,1)="TTRAD"
  ncepopn(216,2)="Temp. tendency by all radiation"
  ncepopn(216,3)="K/s"
  ncepopn(217,1)="TTPHY"
  ncepopn(217,2)="Temp. tendency by non-radiation physics"
  ncepopn(217,3)="K/s"
  ncepopn(218,1)="PREIX"
  ncepopn(218,2)="Precip index (0.0-1.00)"
  ncepopn(218,3)="fraction"
  ncepopn(219,1)="TSD1D"
  ncepopn(219,2)="Std. dev. of IR T over 1x1 deg area"
  ncepopn(219,3)="K"
  ncepopn(220,1)="NLGSP"
  ncepopn(220,2)="Natural log of surface pressure"
  ncepopn(220,3)="ln(kPa)"
  ncepopn(221,1)="HPBL"
  ncepopn(221,2)="Planetary boundary layer height"
  ncepopn(221,3)="m"
  ncepopn(222,1)="5WAVH"
  ncepopn(222,2)="5-wave geopotential height"
  ncepopn(222,3)="gpm"
  ncepopn(223,1)="CNWAT"
  ncepopn(223,2)="Plant canopy surface water"
  ncepopn(223,3)="kg/m^2"
  ncepopn(224,1)="SOTYP"
  ncepopn(224,2)="Soil type (Zobler)"
  ncepopn(224,3)="0..9"
  ncepopn(225,1)="VGTYP"
  ncepopn(225,2)="Vegetation type (as in SiB)"
  ncepopn(225,3)="0..13"
  ncepopn(226,1)="BMIXL"
  ncepopn(226,2)="Blackadar's mixing length scale"
  ncepopn(226,3)="m"
  ncepopn(227,1)="AMIXL"
  ncepopn(227,2)="Asymptotic mixing length scale"
  ncepopn(227,3)="m"
  ncepopn(228,1)="PEVAP"
  ncepopn(228,2)="Pot. evaporation"
  ncepopn(228,3)="kg/m^2"
  ncepopn(229,1)="SNOHF"
  ncepopn(229,2)="Snow phase-change heat flux"
  ncepopn(229,3)="W/m^2"
  ncepopn(230,1)="5WAVA"
  ncepopn(230,2)="5-wave geopot. height anomaly"
  ncepopn(230,3)="gpm"
  ncepopn(231,1)="MFLUX"
  ncepopn(231,2)="Convective cloud mass flux"
  ncepopn(231,3)="Pa/s"
  ncepopn(232,1)="DTRF"
  ncepopn(232,2)="Downward total radiation flux"
  ncepopn(232,3)="W/m^2"
  ncepopn(233,1)="UTRF"
  ncepopn(233,2)="Upward total radiation flux"
  ncepopn(233,3)="W/m^2"
  ncepopn(234,1)="BGRUN"
  ncepopn(234,2)="Baseflow-groundwater runoff"
  ncepopn(234,3)="kg/m^2"
  ncepopn(235,1)="SSRUN"
  ncepopn(235,2)="Storm surface runoff"
  ncepopn(235,3)="kg/m^2"
  ncepopn(236,1)="SIPD"
  ncepopn(236,2)="Supercooled large droplet (SLD) icing pot. diagn."
  ncepopn(236,3)=""
  ncepopn(237,1)="O3TOT"
  ncepopn(237,2)="Total ozone"
  ncepopn(237,3)="kg/m^2"
  ncepopn(238,1)="SNOWC"
  ncepopn(238,2)="Snow cover"
  ncepopn(238,3)="%"
  ncepopn(239,1)="SNOT"
  ncepopn(239,2)="Snow temp."
  ncepopn(239,3)="K"
  ncepopn(240,1)="COVTW"
  ncepopn(240,2)="Covariance T and w"
  ncepopn(240,3)="K*m/s"
  ncepopn(241,1)="LRGHR"
  ncepopn(241,2)="Large scale condensation heating"
  ncepopn(241,3)="K/s"
  ncepopn(242,1)="CNVHR"
  ncepopn(242,2)="Deep convective heating"
  ncepopn(242,3)="K/s"
  ncepopn(243,1)="CNVMR"
  ncepopn(243,2)="Deep convective moistening"
  ncepopn(243,3)="kg/kg/s"
  ncepopn(244,1)="SHAHR"
  ncepopn(244,2)="Shallow convective heating"
  ncepopn(244,3)="K/s"
  ncepopn(245,1)="SHAMR"
  ncepopn(245,2)="Shallow convective moistening"
  ncepopn(245,3)="kg/kg/s"
  ncepopn(246,1)="VDFHR"
  ncepopn(246,2)="Vertical diffusion heating"
  ncepopn(246,3)="K/s"
  ncepopn(247,1)="VDFUA"
  ncepopn(247,2)="Vertical diffusion zonal accel"
  ncepopn(247,3)="m/s^2"
  ncepopn(248,1)="VDFVA"
  ncepopn(248,2)="Vertical diffusion meridional accel"
  ncepopn(248,3)="m/s^2"
  ncepopn(249,1)="VDFMR"
  ncepopn(249,2)="Vertical diffusion moistening"
  ncepopn(249,3)="kg/kg/s"
  ncepopn(250,1)="SWHR"
  ncepopn(250,2)="Solar radiative heating"
  ncepopn(250,3)="K/s"
  ncepopn(251,1)="LWHR"
  ncepopn(251,2)="Longwave radiative heating"
  ncepopn(251,3)="K/s"
  ncepopn(252,1)="CD"
  ncepopn(252,2)="Drag coefficient"
  ncepopn(252,3)="non-dim"
  ncepopn(253,1)="FRICV"
  ncepopn(253,2)="Friction velocity"
  ncepopn(253,3)="m/s"
  ncepopn(254,1)="RI"
  ncepopn(254,2)="Richardson number"
  ncepopn(254,3)="non-dim"

  first=.false.
end if

elemtxt=ncepopn(ain,:)

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ECMWF Metadata 128
!

subroutine gecmwf128(ain,elemtxt)

implicit none

Integer, intent(in) :: ain
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80, save :: ecmwf128(0:255,1:3)
logical, save :: first=.true.

if (first) then

  ecmwf128=""
  ecmwf128(1,1)="STRF"
  ecmwf128(1,2)="Stream function"
  ecmwf128(1,3)="m^2/s"
  ecmwf128(2,1)="VPOT"
  ecmwf128(2,2)="Velocity potential"
  ecmwf128(2,3)="m^2/s"
  ecmwf128(3,1)="PT"
  ecmwf128(3,2)="Potential temperature"
  ecmwf128(3,3)="K"
  ecmwf128(4,1)="EQPT"
  ecmwf128(4,2)="Equivalent potential temperature"
  ecmwf128(4,3)="K"
  ecmwf128(5,1)="SEPT"
  ecmwf128(5,2)="Saturated equivalent potential temperature"
  ecmwf128(5,3)="K"
  ecmwf128(6,1)="var6"
  ecmwf128(6,2)="reserved"
  ecmwf128(6,3)=""
  ecmwf128(7,1)="var7"
  ecmwf128(7,2)="reserved"
  ecmwf128(7,3)=""
  ecmwf128(8,1)="var8"
  ecmwf128(8,2)="reserved"
  ecmwf128(8,3)=""
  ecmwf128(9,1)="var9"
  ecmwf128(9,2)="reserved"
  ecmwf128(9,3)=""
  ecmwf128(10,1)="var10"
  ecmwf128(10,2)="reserved"
  ecmwf128(10,3)=""
  ecmwf128(11,1)="UDVW"
  ecmwf128(11,2)="U component of divergent wind"
  ecmwf128(11,3)="m/s"
  ecmwf128(12,1)="VDVW"
  ecmwf128(12,2)="V component of divergent wind"
  ecmwf128(12,3)="m/s"
  ecmwf128(13,1)="URTW"
  ecmwf128(13,2)="U component of rotational wind"
  ecmwf128(13,3)="m/s"
  ecmwf128(14,1)="VRTW"
  ecmwf128(14,2)="V component of rotational wind"
  ecmwf128(14,3)="m/s"
  ecmwf128(15,1)="var15"
  ecmwf128(15,2)="reserved"
  ecmwf128(15,3)=""
  ecmwf128(16,1)="var16"
  ecmwf128(16,2)="reserved"
  ecmwf128(16,3)=""
  ecmwf128(17,1)="var17"
  ecmwf128(17,2)="reserved"
  ecmwf128(17,3)=""
  ecmwf128(18,1)="var18"
  ecmwf128(18,2)="reserved"
  ecmwf128(18,3)=""
  ecmwf128(19,1)="var19"
  ecmwf128(19,2)="reserved"
  ecmwf128(19,3)=""
  ecmwf128(20,1)="var20"
  ecmwf128(20,2)="reserved"
  ecmwf128(20,3)=""
  ecmwf128(21,1)="UCTP"
  ecmwf128(21,2)="Unbalanced component of temperature"
  ecmwf128(21,3)="K"
  ecmwf128(22,1)="UCLN"
  ecmwf128(22,2)="Unbalanced component of logarithm of surface pressure"
  ecmwf128(22,3)=""
  ecmwf128(23,1)="UCDV"
  ecmwf128(23,2)="Unbalanced component of divergence"
  ecmwf128(23,3)="1/s"
  ecmwf128(24,1)="var24"
  ecmwf128(24,2)="reserved"
  ecmwf128(24,3)=""
  ecmwf128(25,1)="var25"
  ecmwf128(25,2)="reserved"
  ecmwf128(25,3)=""
  ecmwf128(26,1)="CL"
  ecmwf128(26,2)="Lake cover"
  ecmwf128(26,3)="%"
  ecmwf128(27,1)="CVL"
  ecmwf128(27,2)="Low vegetation cover"
  ecmwf128(27,3)="%"
  ecmwf128(28,1)="CVH"
  ecmwf128(28,2)="High vegetation cover"
  ecmwf128(28,3)="%"
  ecmwf128(29,1)="TVL"
  ecmwf128(29,2)="Type of low vegetation"
  ecmwf128(29,3)=""
  ecmwf128(30,1)="TVH"
  ecmwf128(30,2)="Type of high vegetation"
  ecmwf128(30,3)=""
  ecmwf128(31,1)="CI"
  ecmwf128(31,2)="Sea-ice cover"
  ecmwf128(31,3)="%"
  ecmwf128(32,1)="ASN"
  ecmwf128(32,2)="Snow albedo"
  ecmwf128(32,3)="%"
  ecmwf128(33,1)="RSN"
  ecmwf128(33,2)="Snow density"
  ecmwf128(33,3)="kg/m^3"
  ecmwf128(34,1)="SSTK"
  ecmwf128(34,2)="Sea surface temperature"
  ecmwf128(34,3)="K"
  ecmwf128(35,1)="ISTL1"
  ecmwf128(35,2)="Ice surface temperature layer 1"
  ecmwf128(35,3)="K"
  ecmwf128(36,1)="ISTL2"
  ecmwf128(36,2)="Ice surface temperature layer 2"
  ecmwf128(36,3)="K"
  ecmwf128(37,1)="ISTL3"
  ecmwf128(37,2)="Ice surface temperature layer 3"
  ecmwf128(37,3)="K"
  ecmwf128(38,1)="ISTL4"
  ecmwf128(38,2)="Ice surface temperature layer 4"
  ecmwf128(38,3)="K"
  ecmwf128(39,1)="SWVL1"
  ecmwf128(39,2)="Volumetric soil water layer 1"
  ecmwf128(39,3)="m^3/m^3"
  ecmwf128(40,1)="SWVL2"
  ecmwf128(40,2)="Volumetric soil water layer 2"
  ecmwf128(40,3)="m^3/m^3"
  ecmwf128(41,1)="SWVL3"
  ecmwf128(41,2)="Volumetric soil water layer 3"
  ecmwf128(41,3)="m^3/m^3"
  ecmwf128(42,1)="SWVL4"
  ecmwf128(42,2)="Volumetric soil water layer 4"
  ecmwf128(42,3)="m^3/m^3"
  ecmwf128(43,1)="SLT"
  ecmwf128(43,2)="Soil type"
  ecmwf128(43,3)=""
  ecmwf128(44,1)="ES"
  ecmwf128(44,2)="Snow evaporation"
  ecmwf128(44,3)="m of water"
  ecmwf128(45,1)="SMLT"
  ecmwf128(45,2)="Snowmelt"
  ecmwf128(45,3)="m of water"
  ecmwf128(46,1)="SDUR"
  ecmwf128(46,2)="Solar duration"
  ecmwf128(46,3)="s"
  ecmwf128(47,1)="DSRP"
  ecmwf128(47,2)="Direct solar radiation"
  ecmwf128(47,3)="W/m^2"
  ecmwf128(48,1)="MAGSS"
  ecmwf128(48,2)="Magnitude of surface stress"
  ecmwf128(48,3)="N s/m^2"
  ecmwf128(49,1)="10FG"
  ecmwf128(49,2)="Wind gust at 10 metres"
  ecmwf128(49,3)="m/s"
  ecmwf128(50,1)="LSPF"
  ecmwf128(50,2)="Large-scale precipitation fraction"
  ecmwf128(50,3)="s"
  ecmwf128(51,1)="MX2T24"
  ecmwf128(51,2)="Maximum 2 metre temperature"
  ecmwf128(51,3)="K"
  ecmwf128(52,1)="MN2T24"
  ecmwf128(52,2)="Minimum 2 metre temperature"
  ecmwf128(52,3)="K"
  ecmwf128(53,1)="MONT"
  ecmwf128(53,2)="Montgomery potential"
  ecmwf128(53,3)="m^2/s^2"
  ecmwf128(54,1)="PRES"
  ecmwf128(54,2)="Pressure"
  ecmwf128(54,3)="Pa"
  ecmwf128(57,1)="UVB"
  ecmwf128(57,2)="Downward UV radiation at the surface (Ultra-violet band B)"
  ecmwf128(57,3)="W/m^2"
  ecmwf128(58,1)="PAR"
  ecmwf128(58,2)="Photosynthetically active radiation at the surface"
  ecmwf128(58,3)="W/m^2"
  ecmwf128(59,1)="CAPE"
  ecmwf128(59,2)="Convective available potential energy"
  ecmwf128(59,3)="J/kg"
  ecmwf128(60,1)="PV"
  ecmwf128(60,2)="Potential vorticity"
  ecmwf128(60,3)="K m^2 / (kg s)"
  ecmwf128(127,1)="AT"
  ecmwf128(127,2)="Atmospheric tide"
  ecmwf128(127,3)=""
  ecmwf128(128,1)="BV"
  ecmwf128(128,2)="Budget values"
  ecmwf128(128,3)=""
  ecmwf128(129,1)="Z"
  ecmwf128(129,2)="Geopotential"
  ecmwf128(129,3)="m^2/s^2"
  ecmwf128(130,1)="T"
  ecmwf128(130,2)="Temperature"
  ecmwf128(130,3)="K"
  ecmwf128(131,1)="U"
  ecmwf128(131,2)="U velocity"
  ecmwf128(131,3)="m/s"
  ecmwf128(132,1)="V"
  ecmwf128(132,2)="V velocity"
  ecmwf128(132,3)="m/s"
  ecmwf128(133,1)="Q"
  ecmwf128(133,2)="Specific humidity"
  ecmwf128(133,3)="kg/kg"
  ecmwf128(134,1)="SP"
  ecmwf128(134,2)="Surface pressure"
  ecmwf128(134,3)="Pa"
  ecmwf128(135,1)="W"
  ecmwf128(135,2)="Vertical velocity"
  ecmwf128(135,3)="Pa/s"
  ecmwf128(136,1)="TCW"
  ecmwf128(136,2)="Total column water"
  ecmwf128(136,3)="kg/m^2"
  ecmwf128(137,1)="TCWV"
  ecmwf128(137,2)="Total column water vapour"
  ecmwf128(137,3)="kg/m^2"
  ecmwf128(138,1)="VO"
  ecmwf128(138,2)="Vorticity (relative)"
  ecmwf128(138,3)="1/s"
  ecmwf128(139,1)="STL1"
  ecmwf128(139,2)="Soil temperature level 1"
  ecmwf128(139,3)="K"
  ecmwf128(140,1)="SWL1"
  ecmwf128(140,2)="Soil wetness level 1"
  ecmwf128(140,3)="m of water"
  ecmwf128(141,1)="SD"
  ecmwf128(141,2)="Snow depth"
  ecmwf128(141,3)="m of water"
  ecmwf128(142,1)="LSP"
  ecmwf128(142,2)="Stratiform precipitation"
  ecmwf128(142,3)="m"
  ecmwf128(143,1)="CP"
  ecmwf128(143,2)="Convective precipitation"
  ecmwf128(143,3)="m"
  ecmwf128(144,1)="SF"
  ecmwf128(144,2)="Snowfall (convective + stratiform)"
  ecmwf128(144,3)="m of water"
  ecmwf128(145,1)="BLD"
  ecmwf128(145,2)="Boundary layer dissipation"
  ecmwf128(145,3)="W s/m^2"
  ecmwf128(146,1)="SSHF"
  ecmwf128(146,2)="Surface sensible heat flux"
  ecmwf128(146,3)="W s/m^2"
  ecmwf128(147,1)="SLHF"
  ecmwf128(147,2)="Surface latent heat flux"
  ecmwf128(147,3)="W s/m^2"
  ecmwf128(148,1)="CHNK"
  ecmwf128(148,2)="Charnock"
  ecmwf128(148,3)=""
  ecmwf128(149,1)="SNR"
  ecmwf128(149,2)="Surface net radiation"
  ecmwf128(149,3)="W s/m^2"
  ecmwf128(150,1)="TNR"
  ecmwf128(150,2)="Top net radiation"
  ecmwf128(150,3)=""
  ecmwf128(151,1)="MSL"
  ecmwf128(151,2)="Mean sea-level pressure"
  ecmwf128(151,3)="Pa"
  ecmwf128(152,1)="LNSP"
  ecmwf128(152,2)="Logarithm of surface pressure"
  ecmwf128(152,3)=""
  ecmwf128(153,1)="SWHR"
  ecmwf128(153,2)="Short-wave heating rate"
  ecmwf128(153,3)="K"
  ecmwf128(154,1)="LWHR"
  ecmwf128(154,2)="Long-wave heating rate"
  ecmwf128(154,3)="K"
  ecmwf128(155,1)="D"
  ecmwf128(155,2)="Divergence"
  ecmwf128(155,3)="1/s"
  ecmwf128(156,1)="GH"
  ecmwf128(156,2)="Height"
  ecmwf128(156,3)="m"
  ecmwf128(157,1)="R"
  ecmwf128(157,2)="Relative humidity"
  ecmwf128(157,3)="%"
  ecmwf128(158,1)="TSP"
  ecmwf128(158,2)="Tendency of surface pressure"
  ecmwf128(158,3)="Pa/s"
  ecmwf128(159,1)="BLH"
  ecmwf128(159,2)="Boundary layer height"
  ecmwf128(159,3)="m"
  ecmwf128(160,1)="SDOR"
  ecmwf128(160,2)="Standard deviation of orography"
  ecmwf128(160,3)=""
  ecmwf128(161,1)="ISOR"
  ecmwf128(161,2)="Anisotropy of sub-gridscale orography"
  ecmwf128(161,3)=""
  ecmwf128(162,1)="ANOR"
  ecmwf128(162,2)="Angle of sub-gridscale orography"
  ecmwf128(162,3)="rad"
  ecmwf128(163,1)="SLOR"
  ecmwf128(163,2)="Slope of sub-gridscale orography"
  ecmwf128(163,3)=""
  ecmwf128(164,1)="TCC"
  ecmwf128(164,2)="Total cloud cover"
  ecmwf128(164,3)="%"
  ecmwf128(165,1)="10U"
  ecmwf128(165,2)="10 metre U wind component"
  ecmwf128(165,3)="m/s"
  ecmwf128(166,1)="10V"
  ecmwf128(166,2)="10 metre V wind component"
  ecmwf128(166,3)="m/s"
  ecmwf128(167,1)="2T"
  ecmwf128(167,2)="2 metre temperature"
  ecmwf128(167,3)="K"
  ecmwf128(168,1)="2D"
  ecmwf128(168,2)="2 metre dewpoint temperature"
  ecmwf128(168,3)="K"
  ecmwf128(169,1)="SSRD"
  ecmwf128(169,2)="Surface solar radiation downwards"
  ecmwf128(169,3)="W s/m^2"
  ecmwf128(170,1)="STL2"
  ecmwf128(170,2)="Soil temperature level 2"
  ecmwf128(170,3)="K"
  ecmwf128(171,1)="SWL2"
  ecmwf128(171,2)="Soil wetness level 2"
  ecmwf128(171,3)="m of water"
  ecmwf128(172,1)="LSM"
  ecmwf128(172,2)="Land/sea mask"
  ecmwf128(172,3)="%"
  ecmwf128(173,1)="SR"
  ecmwf128(173,2)="Surface roughness"
  ecmwf128(173,3)="m"
  ecmwf128(174,1)="AL"
  ecmwf128(174,2)="Albedo"
  ecmwf128(174,3)="%"
  ecmwf128(175,1)="STRD"
  ecmwf128(175,2)="Surface thermal radiation downwards"
  ecmwf128(175,3)="W s/m^2"
  ecmwf128(176,1)="SSR"
  ecmwf128(176,2)="Surface solar radiation"
  ecmwf128(176,3)="W s/m^2"
  ecmwf128(177,1)="STR"
  ecmwf128(177,2)="Surface thermal radiation"
  ecmwf128(177,3)="W s/m^2"
  ecmwf128(178,1)="TSR"
  ecmwf128(178,2)="Top solar radiation"
  ecmwf128(178,3)="W s/m^2"
  ecmwf128(179,1)="TTR"
  ecmwf128(179,2)="Top thermal radiation"
  ecmwf128(179,3)="W s/m^2"
  ecmwf128(180,1)="EWSS"
  ecmwf128(180,2)="East/West surface stress"
  ecmwf128(180,3)="N s/m^2"
  ecmwf128(181,1)="NSSS"
  ecmwf128(181,2)="North/South surface stress"
  ecmwf128(181,3)="N s/m^2"
  ecmwf128(182,1)="E"
  ecmwf128(182,2)="Evaporation"
  ecmwf128(182,3)="m of water"
  ecmwf128(183,1)="STL3"
  ecmwf128(183,2)="Soil temperature level 3"
  ecmwf128(183,3)="K"
  ecmwf128(184,1)="SWL3"
  ecmwf128(184,2)="Soil wetness level 3"
  ecmwf128(184,3)="m of water"
  ecmwf128(185,1)="CCC"
  ecmwf128(185,2)="Convective cloud cover"
  ecmwf128(185,3)="%"
  ecmwf128(186,1)="LCC"
  ecmwf128(186,2)="Low cloud cover"
  ecmwf128(186,3)="%"
  ecmwf128(187,1)="MCC"
  ecmwf128(187,2)="Medium cloud cover"
  ecmwf128(187,3)="%"
  ecmwf128(188,1)="HCC"
  ecmwf128(188,2)="High cloud cover"
  ecmwf128(188,3)="%"
  ecmwf128(189,1)="SUND"
  ecmwf128(189,2)="Sunshine duration"
  ecmwf128(189,3)="s"
  ecmwf128(190,1)="EWOV"
  ecmwf128(190,2)="EW component of subgrid orographic variance"
  ecmwf128(190,3)="m^2"
  ecmwf128(191,1)="NSOV"
  ecmwf128(191,2)="NS component of subgrid orographic variance"
  ecmwf128(191,3)="m^2"
  ecmwf128(192,1)="NWOV"
  ecmwf128(192,2)="NWSE component of subgrid orographic variance"
  ecmwf128(192,3)="m^2"
  ecmwf128(193,1)="NEOV"
  ecmwf128(193,2)="NESW component of subgrid orographic variance"
  ecmwf128(193,3)="m^2"
  ecmwf128(194,1)="BTMP"
  ecmwf128(194,2)="Brightness temperature"
  ecmwf128(194,3)="K"
  ecmwf128(195,1)="LGWS"
  ecmwf128(195,2)="Lat. component of gravity wave stress"
  ecmwf128(195,3)="N s/m^2"
  ecmwf128(196,1)="MGWS"
  ecmwf128(196,2)="Meridional component of gravity wave stress"
  ecmwf128(196,3)="N s/m^2"
  ecmwf128(197,1)="GWD"
  ecmwf128(197,2)="Gravity wave dissipation"
  ecmwf128(197,3)="W s/m^2"
  ecmwf128(198,1)="SRC"
  ecmwf128(198,2)="Skin reservoir content"
  ecmwf128(198,3)="m of water"
  ecmwf128(199,1)="VEG"
  ecmwf128(199,2)="Vegetation fraction"
  ecmwf128(199,3)="%"
  ecmwf128(200,1)="VSO"
  ecmwf128(200,2)="Variance of sub-gridscale orography"
  ecmwf128(200,3)="m^2"
  ecmwf128(201,1)="MX2T"
  ecmwf128(201,2)="Maximum 2 metre temperature since previous post-processing"
  ecmwf128(201,3)="K"
  ecmwf128(202,1)="MN2T"
  ecmwf128(202,2)="Minimum 2 metre temperature since previous post-processing"
  ecmwf128(202,3)="K"
  ecmwf128(203,1)="O3"
  ecmwf128(203,2)="Ozone mass mixing ratio"
  ecmwf128(203,3)="kg/kg"
  ecmwf128(204,1)="PAW"
  ecmwf128(204,2)="Precipiation analysis weights"
  ecmwf128(204,3)=""
  ecmwf128(205,1)="RO"
  ecmwf128(205,2)="Runoff"
  ecmwf128(205,3)="m"
  ecmwf128(206,1)="TCO3"
  ecmwf128(206,2)="Total column ozone"
  ecmwf128(206,3)="Dobson"
  ecmwf128(207,1)="10SI"
  ecmwf128(207,2)="10 meter windspeed"
  ecmwf128(207,3)="m/s"
  ecmwf128(208,1)="TSRC"
  ecmwf128(208,2)="Top net solar radiation, clear sky"
  ecmwf128(208,3)="W/m^2"
  ecmwf128(209,1)="TTRC"
  ecmwf128(209,2)="Top net thermal radiation, clear sky"
  ecmwf128(209,3)="W/m^2"
  ecmwf128(210,1)="SSRC"
  ecmwf128(210,2)="Surface net solar radiation, clear sky"
  ecmwf128(210,3)="W/m^2"
  ecmwf128(211,1)="STRC"
  ecmwf128(211,2)="Surface net thermal radiation, clear sky"
  ecmwf128(211,3)="W/m^2"
  ecmwf128(212,1)="SI"
  ecmwf128(212,2)="Solar insolation"
  ecmwf128(212,3)="W/m^2"
  ecmwf128(214,1)="DHR"
  ecmwf128(214,2)="Diabatic heating by radiation"
  ecmwf128(214,3)="K"
  ecmwf128(215,1)="DHVD"
  ecmwf128(215,2)="Diabatic heating by vertical diffusion"
  ecmwf128(215,3)="K"
  ecmwf128(216,1)="DHCC"
  ecmwf128(216,2)="Diabatic heating by cumulus convection"
  ecmwf128(216,3)="K"
  ecmwf128(217,1)="DHLC"
  ecmwf128(217,2)="Diabatic heating large-scale condensation"
  ecmwf128(217,3)="K"
  ecmwf128(218,1)="VDZW"
  ecmwf128(218,2)="Vertical diffusion of zonal wind"
  ecmwf128(218,3)="m/s"
  ecmwf128(219,1)="VDMW"
  ecmwf128(219,2)="Vertical diffusion of meridional wind"
  ecmwf128(219,3)="m/s"
  ecmwf128(220,1)="EWGD"
  ecmwf128(220,2)="EW gravity wave drag tendency"
  ecmwf128(220,3)="m/s"
  ecmwf128(221,1)="NSGD"
  ecmwf128(221,2)="NS gravity wave drag tendency"
  ecmwf128(221,3)="m/s"
  ecmwf128(222,1)="CTZW"
  ecmwf128(222,2)="Convective tendency of zonal wind"
  ecmwf128(222,3)="m/s"
  ecmwf128(223,1)="CTMW"
  ecmwf128(223,2)="Convective tendency of meridional wind"
  ecmwf128(223,3)="m/s"
  ecmwf128(224,1)="VDH"
  ecmwf128(224,2)="Vertical diffusion of humidity"
  ecmwf128(224,3)="kg/kg"
  ecmwf128(225,1)="HTCC"
  ecmwf128(225,2)="Humidity tendency by cumulus convection"
  ecmwf128(225,3)="kg/kg"
  ecmwf128(226,1)="HTLC"
  ecmwf128(226,2)="Humidity tendency large-scale condensation"
  ecmwf128(226,3)="kg/kg"
  ecmwf128(227,1)="CRNH"
  ecmwf128(227,2)="Change from removing negative humidity"
  ecmwf128(227,3)="kg/kg"
  ecmwf128(228,1)="TP"
  ecmwf128(228,2)="Total precipitation"
  ecmwf128(228,3)="m"
  ecmwf128(229,1)="IEWS"
  ecmwf128(229,2)="Instantaneous X surface stress"
  ecmwf128(229,3)="N/m^2"
  ecmwf128(230,1)="INSS"
  ecmwf128(230,2)="Instantaneous Y surface stress"
  ecmwf128(230,3)="N/m^2"
  ecmwf128(231,1)="ISHF"
  ecmwf128(231,2)="Instantaneous surface heat flux"
  ecmwf128(231,3)="W/m^2"
  ecmwf128(232,1)="IE"
  ecmwf128(232,2)="Instantaneous moisture flux"
  ecmwf128(232,3)="kg s/m^2"
  ecmwf128(233,1)="ASQ"
  ecmwf128(233,2)="Apparent surface humidity"
  ecmwf128(233,3)="kg/kg"
  ecmwf128(234,1)="LSRH"
  ecmwf128(234,2)="Logarithm of surface roughness length for heat"
  ecmwf128(234,3)=""
  ecmwf128(235,1)="SKT"
  ecmwf128(235,2)="Skin temperature"
  ecmwf128(235,3)="K"
  ecmwf128(236,1)="STL4"
  ecmwf128(236,2)="Soil temperature level 4"
  ecmwf128(236,3)="K"
  ecmwf128(237,1)="SWL4"
  ecmwf128(237,2)="Soil wetness level 4"
  ecmwf128(237,3)="m"
  ecmwf128(238,1)="TSN"
  ecmwf128(238,2)="Temperature of snow layer"
  ecmwf128(238,3)="K"
  ecmwf128(239,1)="CSF"
  ecmwf128(239,2)="Convective snowfall"
  ecmwf128(239,3)="m of water"
  ecmwf128(240,1)="LSF"
  ecmwf128(240,2)="Large-scale snowfall"
  ecmwf128(240,3)="m of water"
  ecmwf128(241,1)="ACF"
  ecmwf128(241,2)="Accumulated cloud fraction tendency"
  ecmwf128(241,3)="-1 to 1"
  ecmwf128(242,1)="ALW"
  ecmwf128(242,2)="Accumulated liquid water tendency"
  ecmwf128(242,3)="-1 to 1"
  ecmwf128(243,1)="FAL"
  ecmwf128(243,2)="Forecast albedo"
  ecmwf128(243,3)="%"
  ecmwf128(244,1)="FSR"
  ecmwf128(244,2)="Forecast surface roughness"
  ecmwf128(244,3)="m"
  ecmwf128(245,1)="FLSR"
  ecmwf128(245,2)="Forecast log of surface roughness for heat"
  ecmwf128(245,3)=""
  ecmwf128(246,1)="CLWC"
  ecmwf128(246,2)="Cloud liquid water content"
  ecmwf128(246,3)="kg/kg"
  ecmwf128(247,1)="CIWC"
  ecmwf128(247,2)="Cloud ice water content"
  ecmwf128(247,3)="kg/kg"
  ecmwf128(248,1)="CC"
  ecmwf128(248,2)="Cloud cover"
  ecmwf128(248,3)="%"
  ecmwf128(249,1)="AIW"
  ecmwf128(249,2)="Accumulated ice water tendency"
  ecmwf128(249,3)="-1 to 1"
  ecmwf128(250,1)="ICE"
  ecmwf128(250,2)="Ice age"
  ecmwf128(250,3)="[1,0]"
  ecmwf128(251,1)="ATTE"
  ecmwf128(251,2)="Adiabatic tendency of temperature"
  ecmwf128(251,3)="K"
  ecmwf128(252,1)="ATHE"
  ecmwf128(252,2)="Adiabatic tendency of humidity"
  ecmwf128(252,3)="kg/kg"
  ecmwf128(253,1)="ATZE"
  ecmwf128(253,2)="Adiabatic tendency of zonal wind"
  ecmwf128(253,3)="m/s"
  ecmwf128(254,1)="ATMW"
  ecmwf128(254,2)="Adiabatic tendency of meridional wind"
  ecmwf128(254,3)="m/s"
  ecmwf128(255,1)="var255"
  ecmwf128(255,2)="missing value"
  ecmwf128(255,3)=""

  first=.false.
end if

elemtxt=ecmwf128(ain,:)

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ECMWF Metadata 140
!

subroutine gecmwf140(ain,elemtxt)

implicit none

Integer, intent(in) :: ain
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80, save :: ecmwf140(0:255,1:3)
logical, save :: first=.true.

if (first) then

  ecmwf140=""
  ecmwf140(220,1)="MP1"
  ecmwf140(220,2)="Mean wave period based on first moment"
  ecmwf140(220,3)="s"
  ecmwf140(221,1)="MP2"
  ecmwf140(221,2)="Mean wave period based on second moment"
  ecmwf140(221,3)="s"
  ecmwf140(222,1)="WDW"
  ecmwf140(222,2)="Wave spectral directional width"
  ecmwf140(222,3)=""
  ecmwf140(223,1)="P1WW"
  ecmwf140(223,2)="Mean wave period based on first moment for wind waves"
  ecmwf140(223,3)="s"
  ecmwf140(224,1)="P2WW"
  ecmwf140(224,2)="Mean wave period based on second moment for wind waves"
  ecmwf140(224,3)="s"
  ecmwf140(225,1)="DWWW"
  ecmwf140(225,2)="Wave spectral directional width for wind waves"
  ecmwf140(225,3)=""
  ecmwf140(226,1)="P1PS"
  ecmwf140(226,2)="Mean wave period based on first moment for swell"
  ecmwf140(226,3)="s"
  ecmwf140(227,1)="P2PS"
  ecmwf140(227,2)="Mean wave period based on second moment for swell"
  ecmwf140(227,3)="s"
  ecmwf140(228,1)="DWPS"
  ecmwf140(228,2)="Wave spectral directional width for swell"
  ecmwf140(228,3)=""
  ecmwf140(229,1)="SWH"
  ecmwf140(229,2)="Significant wave height"
  ecmwf140(229,3)="m"
  ecmwf140(230,1)="MWD"
  ecmwf140(230,2)="Mean wave direction"
  ecmwf140(230,3)="deg"
  ecmwf140(231,1)="PP1D"
  ecmwf140(231,2)="Peak period of 1D spectra"
  ecmwf140(231,3)="s"
  ecmwf140(232,1)="MWP"
  ecmwf140(232,2)="Mean wave period"
  ecmwf140(232,3)="s"
  ecmwf140(233,1)="CDWW"
  ecmwf140(233,2)="Coefficient of drag with waves"
  ecmwf140(233,3)=""
  ecmwf140(234,1)="SHWW"
  ecmwf140(234,2)="Significant height of wind waves"
  ecmwf140(234,3)="m"
  ecmwf140(235,1)="MDWW"
  ecmwf140(235,2)="Mean direction of wind waves"
  ecmwf140(235,3)="deg"
  ecmwf140(236,1)="MPWW"
  ecmwf140(236,2)="Mean period of wind waves"
  ecmwf140(236,3)="s"
  ecmwf140(237,1)="SHPS"
  ecmwf140(237,2)="Significant height of primary swell"
  ecmwf140(237,3)="m"
  ecmwf140(238,1)="MDPS"
  ecmwf140(238,2)="Mean direction of primary swell"
  ecmwf140(238,3)="deg"
  ecmwf140(239,1)="MPPS"
  ecmwf140(239,2)="Mean period of primary swell"
  ecmwf140(239,3)="s"
  ecmwf140(240,1)="SDHS"
  ecmwf140(240,2)="Standard deviation wave height"
  ecmwf140(240,3)="m"
  ecmwf140(241,1)="MU10"
  ecmwf140(241,2)="Mean of 10 metre windspeed"
  ecmwf140(241,3)="m/s"
  ecmwf140(242,1)="MDWI"
  ecmwf140(242,2)="Mean wind direction"
  ecmwf140(242,3)="deg"
  ecmwf140(243,1)="SDU"
  ecmwf140(243,2)="Standard deviation of 10 metre wind speed"
  ecmwf140(243,3)="m/s"
  ecmwf140(244,1)="MSQS"
  ecmwf140(244,2)="Mean square slope of waves"
  ecmwf140(244,3)="none"
  ecmwf140(245,1)="WIND"
  ecmwf140(245,2)="10 metre wind speed"
  ecmwf140(245,3)="m/s"
  ecmwf140(246,1)="AWH"
  ecmwf140(246,2)="Altimeter wave height"
  ecmwf140(246,3)="m"
  ecmwf140(247,1)="ACWH"
  ecmwf140(247,2)="Altimeter corrected wave height"
  ecmwf140(247,3)="m"
  ecmwf140(248,1)="ARRC"
  ecmwf140(248,2)="Altimeter range relative correction"
  ecmwf140(248,3)=""
  ecmwf140(249,1)="DWI"
  ecmwf140(249,2)="10 metre wind direction"
  ecmwf140(249,3)="deg"
  ecmwf140(250,1)="2DSP"
  ecmwf140(250,2)="2D wave spectra (multiple)"
  ecmwf140(250,3)="m^2 s"
  ecmwf140(251,1)="2DFD"
  ecmwf140(251,2)="2D wave spectra (single)"
  ecmwf140(251,3)="m^2 s"

  first=.false.
end if

elemtxt=ecmwf140(ain,:)

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ECMWF Metadata 128
!

subroutine gecmwf210(ain,elemtxt)

implicit none

Integer, intent(in) :: ain
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80, save :: ecmwf210(0:255,1:3)
logical, save :: first=.true.

if (first) then

  ecmwf210=""
  ecmwf210(1,1)="AERMR01"
  ecmwf210(1,2)="Sea Salt Aerosol (0.03-0.5 um) Mixing Ratio"
  ecmwf210(1,3)="kg/kg"
  ecmwf210(2,1)="AERMR02"
  ecmwf210(2,2)="Sea Salt Aerosol (0.5-5 um) Mixing Ratio"
  ecmwf210(2,3)="kg/kg"
  ecmwf210(3,1)="AERMR03"
  ecmwf210(3,2)="Sea Salt Aerosol (5-20 um) Mixing Ratio"
  ecmwf210(3,3)="kg/kg"
  ecmwf210(4,1)="AERMR04"
  ecmwf210(4,2)="Dust Aerosol (0.03-0.55 um) Mixing Ratio"
  ecmwf210(4,3)="kg/kg"
  ecmwf210(5,1)="AERMR05"
  ecmwf210(5,2)="Dust Aerosol (0.55-0.9 um) Mixing Ratio"
  ecmwf210(5,3)="kg/kg"
  ecmwf210(6,1)="AERMR06"
  ecmwf210(6,2)="Dust Aerosol (0.9-20 um) Mixing Ratio"
  ecmwf210(6,3)="kg/kg"
  ecmwf210(7,1)="AERMR07"
  ecmwf210(7,2)="Hydrophobic Organic Matter Aerosol Mixing Ratio"
  ecmwf210(7,3)="kg/kg"
  ecmwf210(8,1)="AERMR08"
  ecmwf210(8,2)="Hydrophilic Organic Matter Aerosol Mixing Ratio"
  ecmwf210(8,3)="kg/kg"
  ecmwf210(9,1)="AERMR09"
  ecmwf210(9,2)="Hydrophobic Black Carbon Aerosol Mixing Ratio"
  ecmwf210(9,3)="kg/kg"
  ecmwf210(10,1)="AERMR10"
  ecmwf210(10,2)="Hydrophilic Black Carbon Aerosol Mixing Ratio"
  ecmwf210(10,3)="kg/kg"
  ecmwf210(11,1)="AERMR11"
  ecmwf210(11,2)="Sulphate Aerosol Mixing Ratio"
  ecmwf210(11,3)="kg/kg"
  ecmwf210(12,1)="AERMR12"
  ecmwf210(12,2)="Aerosol Type 12 Mixing Ratio"
  ecmwf210(12,3)="kg/kg"
  ecmwf210(16,1)="AERGN01"
  ecmwf210(16,2)="Aerosol type 1 source/gain accumulated"
  ecmwf210(16,3)="kg/m^2"
  ecmwf210(17,1)="AERGN02"
  ecmwf210(17,2)="Aerosol type 2 source/gain accumulated"
  ecmwf210(17,3)="kg/m^2"
  ecmwf210(18,1)="AERGN03"
  ecmwf210(18,2)="Aerosol type 3 source/gain accumulated"
  ecmwf210(18,3)="kg/m^2"
  ecmwf210(19,1)="AERGN04"
  ecmwf210(19,2)="Aerosol type 4 source/gain accumulated"
  ecmwf210(19,3)="kg/m^2"
  ecmwf210(20,1)="AERGN05"
  ecmwf210(20,2)="Aerosol type 5 source/gain accumulated"
  ecmwf210(20,3)="kg/m^2"
  ecmwf210(21,1)="AERGN06"
  ecmwf210(21,2)="Aerosol type 6 source/gain accumulated"
  ecmwf210(21,3)="kg/m^2"
  ecmwf210(22,1)="AERGN07"
  ecmwf210(22,2)="Aerosol type 7 source/gain accumulated"
  ecmwf210(22,3)="kg/m^2"
  ecmwf210(23,1)="AERGN08"
  ecmwf210(23,2)="Aerosol type 8 source/gain accumulated"
  ecmwf210(23,3)="kg/m^2"
  ecmwf210(24,1)="AERGN09"
  ecmwf210(24,2)="Aerosol type 9 source/gain accumulated"
  ecmwf210(24,3)="kg/m^2"
  ecmwf210(25,1)="AERGN10"
  ecmwf210(25,2)="Aerosol type 10 source/gain accumulated"
  ecmwf210(25,3)="kg/m^2"
  ecmwf210(26,1)="AERGN11"
  ecmwf210(26,2)="Aerosol type 11 source/gain accumulated"
  ecmwf210(26,3)="kg/m^2"
  ecmwf210(27,1)="AERGN12"
  ecmwf210(27,2)="Aerosol type 12 source/gain accumulated"
  ecmwf210(27,3)="kg/m^2"
  ecmwf210(31,1)="AERLS01"
  ecmwf210(31,2)="Aerosol type 1 sink/loss accumulated"
  ecmwf210(31,3)="kg/m^2"
  ecmwf210(32,1)="AERLS02"
  ecmwf210(32,2)="Aerosol type 2 sink/loss accumulated"
  ecmwf210(32,3)="kg/m^2"
  ecmwf210(33,1)="AERLS03"
  ecmwf210(33,2)="Aerosol type 3 sink/loss accumulated"
  ecmwf210(33,3)="kg/m^2"
  ecmwf210(34,1)="AERLS04"
  ecmwf210(34,2)="Aerosol type 4 sink/loss accumulated"
  ecmwf210(34,3)="kg/m^2"
  ecmwf210(35,1)="AERLS05"
  ecmwf210(35,2)="Aerosol type 5 sink/loss accumulated"
  ecmwf210(35,3)="kg/m^2"
  ecmwf210(36,1)="AERLS06"
  ecmwf210(36,2)="Aerosol type 6 sink/loss accumulated"
  ecmwf210(36,3)="kg/m^2"
  ecmwf210(37,1)="AERLS07"
  ecmwf210(37,2)="Aerosol type 7 sink/loss accumulated"
  ecmwf210(37,3)="kg/m^2"
  ecmwf210(38,1)="AERLS08"
  ecmwf210(38,2)="Aerosol type 8 sink/loss accumulated"
  ecmwf210(38,3)="kg/m^2"
  ecmwf210(39,1)="AERLS09"
  ecmwf210(39,2)="Aerosol type 9 sink/loss accumulated"
  ecmwf210(39,3)="kg/m^2"
  ecmwf210(40,1)="AERLS10"
  ecmwf210(40,2)="Aerosol type 10 sink/loss accumulated"
  ecmwf210(40,3)="kg/m^2"
  ecmwf210(41,1)="AERLS11"
  ecmwf210(41,2)="Aerosol type 11 sink/loss accumulated"
  ecmwf210(41,3)="kg/m^2"
  ecmwf210(42,1)="AERLS12"
  ecmwf210(42,2)="Aerosol type 12 sink/loss accumulated"
  ecmwf210(42,3)="kg/m^2"
  ecmwf210(46,1)="AERPR"
  ecmwf210(46,2)="Aerosol precursor mixing ratio"
  ecmwf210(46,3)="kg/kg"
  ecmwf210(47,1)="AERSM"
  ecmwf210(47,2)="Aerosol small mode mixing ratio"
  ecmwf210(47,3)="kg/kg"
  ecmwf210(48,1)="AERLG"
  ecmwf210(48,2)="Aerosol large mode mixing ratio"
  ecmwf210(48,3)="kg/kg"
  ecmwf210(49,1)="AODPR"
  ecmwf210(49,2)="Aerosol precursor optical depth"
  ecmwf210(49,3)="none"
  ecmwf210(50,1)="AODSM"
  ecmwf210(50,2)="Aerosol small mode optical depth"
  ecmwf210(50,3)="none"
  ecmwf210(51,1)="AODLG"
  ecmwf210(51,2)="Aerosol large mode optical depth"
  ecmwf210(51,3)="none"
  ecmwf210(52,1)="AERDEP"
  ecmwf210(52,2)="Dust emission potential"
  ecmwf210(52,3)="kg s^2/m^5"
  ecmwf210(53,1)="AERLTS"
  ecmwf210(53,2)="Lifting threshold speed"
  ecmwf210(53,3)="m/s"
  ecmwf210(54,1)="AERSCC"
  ecmwf210(54,2)="Soil clay content"
  ecmwf210(54,3)="%"
  ecmwf210(61,1)="CO2"
  ecmwf210(61,2)="Carbon Dioxide"
  ecmwf210(61,3)="kg/kg"
  ecmwf210(62,1)="CH4"
  ecmwf210(62,2)="Methane"
  ecmwf210(62,3)="kg/kg"
  ecmwf210(63,1)="N20"
  ecmwf210(63,2)="Nitrous oxide"
  ecmwf210(63,3)="kg/kg"
  ecmwf210(64,1)="TCCO2"
  ecmwf210(64,2)="Total column Carbon Dioxide"
  ecmwf210(64,3)="kg/m^2"
  ecmwf210(65,1)="TCCH4"
  ecmwf210(65,2)="Total column Methane"
  ecmwf210(65,3)="kg/m^2"
  ecmwf210(66,1)="TCN2O"
  ecmwf210(66,2)="Total column Nitrous oxide"
  ecmwf210(66,3)="kg/m^2"
  ecmwf210(67,1)="CO2OF"
  ecmwf210(67,2)="Ocean flux of Carbon Dioxide"
  ecmwf210(67,3)="kg/m^2/s"
  ecmwf210(68,1)="CO2NBF"
  ecmwf210(68,2)="Natural biosphere flux of Carbon Dioxide"
  ecmwf210(68,3)="kg/m^2/s"
  ecmwf210(69,1)="CO2APF"
  ecmwf210(69,2)="Anthropogenic emissions of Carbon Dioxide"
  ecmwf210(69,3)="kg/m^2/s"
  ecmwf210(70,1)="CH4F"
  ecmwf210(70,2)="Methane suface fluxes"
  ecmwf210(70,3)="kg/m^2/s"
  ecmwf210(71,1)="kCH4"
  ecmwf210(71,2)="Methane loss rate due to radical hydroxyl(OH)"
  ecmwf210(71,3)="1/s"
  ecmwf210(80,1)="CO2FIRE"
  ecmwf210(80,2)="Wildfire flux of Carbon Dioxide"
  ecmwf210(80,3)="kg/m^2/s"
  ecmwf210(81,1)="COFIRE"
  ecmwf210(81,2)="Wildfire flux of Carbon Monoxide"
  ecmwf210(81,3)="kg/m^2/s"
  ecmwf210(82,1)="CH4FIRE"
  ecmwf210(82,2)="Wildfire flux of Methane"
  ecmwf210(82,3)="kg/m^2/s"
  ecmwf210(83,1)="NMHCFIRE"
  ecmwf210(83,2)="Wildfire flux of Non-Methane Hydro-Carbons"
  ecmwf210(83,3)="kg/m^2/s"
  ecmwf210(84,1)="H2FIRE"
  ecmwf210(84,2)="Wildfire flux of Hydrogen"
  ecmwf210(84,3)="kg/m^2/s"
  ecmwf210(85,1)="NOXFIRE"
  ecmwf210(85,2)="Wildfire flux of Nitrogen Oxides NOx"
  ecmwf210(85,3)="kg/m^2/s"
  ecmwf210(86,1)="N2OFIRE"
  ecmwf210(86,2)="Wildfire flux of Nitrous Oxide"
  ecmwf210(86,3)="kg/m^2/s"
  ecmwf210(87,1)="PM2P5FIRE"
  ecmwf210(87,2)="Wildfire flux of Particulate Matter PM2.5"
  ecmwf210(87,3)="kg/m^2/s"
  ecmwf210(88,1)="TPMFIRE"
  ecmwf210(88,2)="Wildfire flux of Total Particulate Matter"
  ecmwf210(88,3)="kg/m^2/s"
  ecmwf210(89,1)="TCFIRE"
  ecmwf210(89,2)="Wildfire flux of Total Carbon in Aerosols"
  ecmwf210(89,3)="kg/m^2/s"
  ecmwf210(90,1)="OCFIRE"
  ecmwf210(90,2)="Wildfire flux of Organic Carbon"
  ecmwf210(90,3)="kg/m^2/s"
  ecmwf210(91,1)="BCFIRE"
  ecmwf210(91,2)="Wildfire flux of Black Carbon"
  ecmwf210(91,3)="kg/m^2/s"
  ecmwf210(92,1)="CFIRE"
  ecmwf210(92,2)="Wildfire overall flux of burnt Carbon"
  ecmwf210(92,3)="kg/m^2/s"
  ecmwf210(93,1)="C4FFIRE"
  ecmwf210(93,2)="Wildfire fraction of C4 plants"
  ecmwf210(93,3)="none"
  ecmwf210(94,1)="VEGFIRE"
  ecmwf210(94,2)="Wildfire vegetation map index"
  ecmwf210(94,3)="none"
  ecmwf210(95,1)="CCFIRE"
  ecmwf210(95,2)="Wildfire Combustion Completeness"
  ecmwf210(95,3)="none"
  ecmwf210(96,1)="FLFIRE"
  ecmwf210(96,2)="Wildfire fuel load: Carbon per unit area"
  ecmwf210(96,3)="kg/m^2"
  ecmwf210(97,1)="BFFIRE"
  ecmwf210(97,2)="Wildfire fraction of area burnt"
  ecmwf210(97,3)="none"
  ecmwf210(98,1)="OAFIRE"
  ecmwf210(98,2)="Wildfire observed area"
  ecmwf210(98,3)="m^2"
  ecmwf210(99,1)="FRPFIRE"
  ecmwf210(99,2)="Wildfire radiative power"
  ecmwf210(99,3)="W/m^2"
  ecmwf210(100,1)="CRFIRE"
  ecmwf210(100,2)="Wildfire combusion rate"
  ecmwf210(100,3)="kg/m^2/s"
  ecmwf210(121,1)="NO2"
  ecmwf210(121,2)="Nitrogen dioxide"
  ecmwf210(121,3)="kg/kg"
  ecmwf210(122,1)="SO2"
  ecmwf210(122,2)="Sulphur dioxide"
  ecmwf210(122,3)="kg/kg"
  ecmwf210(123,1)="CO"
  ecmwf210(123,2)="Carbon monoxide"
  ecmwf210(123,3)="kg/kg"
  ecmwf210(124,1)="HCHO"
  ecmwf210(124,2)="Formaldehyde"
  ecmwf210(124,3)="kg/kg"
  ecmwf210(125,1)="TCNO2"
  ecmwf210(125,2)="Total column Nitrogen dioxide"
  ecmwf210(125,3)="kg/m^2"
  ecmwf210(126,1)="TCSO2"
  ecmwf210(126,2)="Total column Sulphur dioxide"
  ecmwf210(126,3)="kg/m^2"
  ecmwf210(127,1)="TCCO"
  ecmwf210(127,2)="Total column Carbon monoxide"
  ecmwf210(127,3)="kg/m^2"
  ecmwf210(128,1)="TCHCHO"
  ecmwf210(128,2)="Total column Formaldehyde"
  ecmwf210(128,3)="kg/m^2"
  ecmwf210(129,1)="NOX"
  ecmwf210(129,2)="Nitrogen Oxides"
  ecmwf210(129,3)="kg/kg"
  ecmwf210(130,1)="TCNOX"
  ecmwf210(130,2)="Total column Nitrogen Oxides"
  ecmwf210(130,3)="kg/m^2"
  ecmwf210(131,1)="GRG1"
  ecmwf210(131,2)="Reactive tracer 1 mass mixing ratio"
  ecmwf210(131,3)="kg/kg"
  ecmwf210(132,1)="TCGRG1"
  ecmwf210(132,2)="Total column GRG tracer 1"
  ecmwf210(132,3)="kg/m^2"
  ecmwf210(133,1)="GRG2"
  ecmwf210(133,2)="Reactive tracer 2 mass mixing ratio"
  ecmwf210(133,3)="kg/kg"
  ecmwf210(134,1)="TCGRG2"
  ecmwf210(134,2)="Total column GRG tracer 2"
  ecmwf210(134,3)="kg/m^2"
  ecmwf210(135,1)="GRG3"
  ecmwf210(135,2)="Reactive tracer 3 mass mixing ratio"
  ecmwf210(135,3)="kg/kg"
  ecmwf210(136,1)="TCGRG3"
  ecmwf210(136,2)="Total column GRG tracer 3"
  ecmwf210(136,3)="kg/m^2"
  ecmwf210(137,1)="GRG4"
  ecmwf210(137,2)="Reactive tracer 4 mass mixing ratio"
  ecmwf210(137,3)="kg/kg"
  ecmwf210(138,1)="TCGRG4"
  ecmwf210(138,2)="Total column GRG tracer 4"
  ecmwf210(138,3)="kg/m^2"
  ecmwf210(139,1)="GRG5"
  ecmwf210(139,2)="Reactive tracer 5 mass mixing ratio"
  ecmwf210(139,3)="kg/kg"
  ecmwf210(140,1)="TCGRG5"
  ecmwf210(140,2)="Total column GRG tracer 5"
  ecmwf210(140,3)="kg/m^2"
  ecmwf210(141,1)="GRG6"
  ecmwf210(141,2)="Reactive tracer 6 mass mixing ratio"
  ecmwf210(141,3)="kg/kg"
  ecmwf210(142,1)="TCGRG6"
  ecmwf210(142,2)="Total column GRG tracer 6"
  ecmwf210(142,3)="kg/m^2"
  ecmwf210(143,1)="GRG7"
  ecmwf210(143,2)="Reactive tracer 7 mass mixing ratio"
  ecmwf210(143,3)="kg/kg"
  ecmwf210(144,1)="TCGRG7"
  ecmwf210(144,2)="Total column GRG tracer 7"
  ecmwf210(144,3)="kg/m^2"
  ecmwf210(145,1)="GRG8"
  ecmwf210(145,2)="Reactive tracer 8 mass mixing ratio"
  ecmwf210(145,3)="kg/kg"
  ecmwf210(146,1)="TCGRG8"
  ecmwf210(146,2)="Total column GRG tracer 8"
  ecmwf210(146,3)="kg/m^2"
  ecmwf210(147,1)="GRG9"
  ecmwf210(147,2)="Reactive tracer 9 mass mixing ratio"
  ecmwf210(147,3)="kg/kg"
  ecmwf210(148,1)="TCGRG9"
  ecmwf210(148,2)="Total column GRG tracer 9"
  ecmwf210(148,3)="kg/m^2"
  ecmwf210(149,1)="GRG10"
  ecmwf210(149,2)="Reactive tracer 10 mass mixing ratio"
  ecmwf210(149,3)="kg/kg"
  ecmwf210(150,1)="TCGRG10"
  ecmwf210(150,2)="Total column GRG tracer 10"
  ecmwf210(150,3)="kg/m^2"
  ecmwf210(151,1)="SFNOX"
  ecmwf210(151,2)="Surface flux Nitrogen oxides"
  ecmwf210(151,3)="kg/m^2/s"
  ecmwf210(152,1)="SFNO2"
  ecmwf210(152,2)="Surface flux Nitrogen dioxide"
  ecmwf210(152,3)="kg/m^2/s"
  ecmwf210(153,1)="SFSO2"
  ecmwf210(153,2)="Surface flux Sulphur dioxide"
  ecmwf210(153,3)="kg/m^2/s"
  ecmwf210(154,1)="SFCO2"
  ecmwf210(154,2)="Surface flux Carbon monoxide"
  ecmwf210(154,3)="kg/m^2/s"
  ecmwf210(155,1)="SFHCHO"
  ecmwf210(155,2)="Surface flux Formaldehyde"
  ecmwf210(155,3)="kg/m^2/s"
  ecmwf210(156,1)="SFGO3"
  ecmwf210(156,2)="Surface flux GEMS ozone"
  ecmwf210(156,3)="kg/m^2/s"
  ecmwf210(157,1)="SFGR1"
  ecmwf210(157,2)="Surface flux reactive tracer 1"
  ecmwf210(157,3)="kg/m^2/s"
  ecmwf210(158,1)="SFGR2"
  ecmwf210(158,2)="Surface flux reactive tracer 2"
  ecmwf210(158,3)="kg/m^2/s"
  ecmwf210(159,1)="SFGR3"
  ecmwf210(159,2)="Surface flux reactive tracer 3"
  ecmwf210(159,3)="kg/m^2/s"
  ecmwf210(160,1)="SFGR4"
  ecmwf210(160,2)="Surface flux reactive tracer 4"
  ecmwf210(160,3)="kg/m^2/s"
  ecmwf210(161,1)="SFGR5"
  ecmwf210(161,2)="Surface flux reactive tracer 5"
  ecmwf210(161,3)="kg/m^2/s"
  ecmwf210(162,1)="SFGR6"
  ecmwf210(162,2)="Surface flux reactive tracer 6"
  ecmwf210(162,3)="kg/m^2/s"
  ecmwf210(163,1)="SFGR7"
  ecmwf210(163,2)="Surface flux reactive tracer 7"
  ecmwf210(163,3)="kg/m^2/s"
  ecmwf210(164,1)="SFGR8"
  ecmwf210(164,2)="Surface flux reactive tracer 8"
  ecmwf210(164,3)="kg/m^2/s"
  ecmwf210(165,1)="SFGR9"
  ecmwf210(165,2)="Surface flux reactive tracer 9"
  ecmwf210(165,3)="kg/m^2/s"
  ecmwf210(166,1)="SFGR10"
  ecmwf210(166,2)="Surface flux reactive tracer 10"
  ecmwf210(166,3)="kg/m^2/s"
  ecmwf210(181,1)="Ra"
  ecmwf210(181,2)="Radon"
  ecmwf210(181,3)="kg/kg"
  ecmwf210(182,1)="SF6"
  ecmwf210(182,2)="Sulphur Hexafluoride"
  ecmwf210(182,3)="kg/kg"
  ecmwf210(183,1)="TCRa"
  ecmwf210(183,2)="Total column of Radon"
  ecmwf210(183,3)="kg/m^2"
  ecmwf210(184,1)="TCSF6"
  ecmwf210(184,2)="Total column of Sulphur Hexafluoride"
  ecmwf210(184,3)="kg/m^2"
  ecmwf210(185,1)="SF6APF"
  ecmwf210(185,2)="Anthropogenic Emissions of Sulphur Hexafluoride"
  ecmwf210(185,3)="kg/m^2/s"
  ecmwf210(203,1)="GO3"
  ecmwf210(203,2)="GEMS ozone"
  ecmwf210(203,3)="kg/kg"
  ecmwf210(206,1)="GTCO3"
  ecmwf210(206,2)="GEMS Total column ozone"
  ecmwf210(206,3)="kg/m^2"
  ecmwf210(207,1)="AOD550"
  ecmwf210(207,2)="Total Aerosol Optical Depth at 550nm"
  ecmwf210(207,3)="none"
  ecmwf210(208,1)="SSAOD550"
  ecmwf210(208,2)="Sea Salt Aerosol Optical Depth at 550nm"
  ecmwf210(208,3)="none"
  ecmwf210(209,1)="DUAOD550"
  ecmwf210(209,2)="Dust Aerosol Optical Depth at 550nm"
  ecmwf210(209,3)="none"
  ecmwf210(210,1)="OMAOD550"
  ecmwf210(210,2)="Organic Matter Aerosol Optical Depth at 550nm"
  ecmwf210(210,3)="none"
  ecmwf210(211,1)="BCAOD550"
  ecmwf210(211,2)="Black Carbon Aerosol Optical Depth at 550nm"
  ecmwf210(211,3)="none"
  ecmwf210(212,1)="SUAOD550"
  ecmwf210(212,2)="Sulphate Aerosol Optical Depth at 550nm"
  ecmwf210(212,3)="none"
  ecmwf210(213,1)="AOD469"
  ecmwf210(213,2)="Total Aerosol Optical Depth at 469nm"
  ecmwf210(213,3)="none"
  ecmwf210(214,1)="AOD670"
  ecmwf210(214,2)="Total Aerosol Optical Depth at 670nm"
  ecmwf210(214,3)="none"
  ecmwf210(215,1)="AOD865"
  ecmwf210(215,2)="Total Aerosol Optical Depth at 865nm"
  ecmwf210(215,3)="none"
  ecmwf210(216,1)="AOD1240"
  ecmwf210(216,2)="Total Aerosol Optical Depth at 1240nm"
  ecmwf210(216,3)="none"

  first=.false.
end if

elemtxt=ecmwf210(ain,:)

return
end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine finds the element name, comment and units for a
! GRIB2 file.  It also contains definitions for all GRIB2 elements
!

Subroutine getelemdesc2(alist,elemtxt)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80, elemerr(1:3)
Character*80, save :: meteotemp(0:21,1:3)
Character*80, save :: meteomoist(0:107,1:3)
Character*80, save :: meteomoment(0:38,1:3)
Character*80, save :: meteomass(0:26,1:3)
Character*80, save :: meteoshortradiate(0:6,1:3)
Character*80, save :: meteolongradiate(0:2,1:3)
Character*80, save :: meteocloud(0:201,1:3)
Character*80, save :: meteostability(0:9,1:3)
Character*80, save :: meteoaerosols(0:0,1:3)
Character*80, save :: meteogases(0:0,1:3)
Character*80, save :: meteoradar(0:8,1:3)
Character*80, save :: meteonuclear(0:8,1:3)
Character*80, save :: meteoatmos(0:16,1:3)
Character*80, save :: meteotext(0:0,1:3)
Character*80, save :: hydrobasic(0:4,1:3)
Character*80, save :: hydroprob(0:2,1:3)
Character*80, save :: landveg(0:8,1:3)
Character*80, save :: landsoil(0:203,1:3)
Character*80, save :: landfire(0:4,1:3)
Character*80, save :: spaceimage(0:7,1:3)
Character*80, save :: spacequantitative(0:0,1:3)
Character*80, save :: oceanwaves(0:13,1:3)
Character*80, save :: oceancurrents(0:3,1:3)
Character*80, save :: oceanice(0:7,1:3)
Character*80, save :: oceansurface(0:1,1:3)
Character*80, save :: oceansubsurface(0:3,1:3)
Character*80, save :: nceplcltable000000(192:204,1:3)
Character*80, save :: nceplcltable000001(192:205,1:3)
Character*80, save :: nceplcltable000002(192:224,1:3)
Character*80, save :: nceplcltable000003(192:221,1:3)
Character*80, save :: nceplcltable000004(192:193,1:3)
Character*80, save :: nceplcltable000005(192:193,1:3)
Character*80, save :: nceplcltable000006(192:199,1:3)
Character*80, save :: nceplcltable000007(192:194,1:3)
Character*80, save :: nceplcltable000014(192:194,1:3)
Character*80, save :: nceplcltable000019(192:193,1:3)
Character*80, save :: nceplcltable000191(192:194,1:3)
Character*80, save :: nceplcltable001000(192:193,1:3)
Character*80, save :: nceplcltable002000(192:205,1:3)
Character*80, save :: nceplcltable002003(192:197,1:3)
Character*80, save :: nceplcltable003001(192:193,1:3)
logical, save :: first=.true.

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

elemerr='???'

if (first) then

  meteotemp(0,1) =  'TMP'
  meteotemp(0,2) =  'Temperature'
  meteotemp(0,3) =  'K'
  meteotemp(1,1) =  'VTMP'
  meteotemp(1,2) =  'Virtual temperature'
  meteotemp(1,3) =  'K'
  meteotemp(2,1) =  'POT'
  meteotemp(2,2) =  'Potential temperature'
  meteotemp(2,3) =  'K'
  meteotemp(3,1) =  'EPOT'
  meteotemp(3,2) =  'Pseudo-adiabatic potential temperature'
  meteotemp(3,3) =  'K'
  meteotemp(4,1) =  'TMAX'
  meteotemp(4,2) =  'Maximum temperature'
  meteotemp(4,3) =  'K'
  meteotemp(5,1) =  'TMIN'
  meteotemp(5,2) =  'Minimum temperature'
  meteotemp(5,3) =  'K'
  meteotemp(6,1) =  'DPT'
  meteotemp(6,2) =  'Dew point temperature'
  meteotemp(6,3) =  'K'
  meteotemp(7,1) =  'DEPR'
  meteotemp(7,2) =  'Dew point depression'
  meteotemp(7,3) =  'K'
  meteotemp(8,1) =  'LAPR'
  meteotemp(8,2) =  'Lapse rate'
  meteotemp(8,3) =  'K'
  meteotemp(9,1) =  'TMPA'
  meteotemp(9,2) =  'Temperature anomaly'
  meteotemp(9,3) =  'K'
  meteotemp(10,1) = 'LHTFL'
  meteotemp(10,2) = 'Latent heat net flux'
  meteotemp(10,3) = 'W/(m^2)'
  meteotemp(11,1) = 'SHTFL'
  meteotemp(11,2) = 'Sensible heat net flux'
  meteotemp(11,3) = 'K'
  meteotemp(12,1) = 'HeatIndex'
  meteotemp(12,2) = 'Heat index'
  meteotemp(12,3) = 'K'
  meteotemp(13,1) = 'WCI'
  meteotemp(13,2) = 'Wind chill factor'
  meteotemp(13,3) = 'K'
  meteotemp(14,1) = ''
  meteotemp(14,2) = 'Minimum dew point depression'
  meteotemp(14,3) = 'K'
  meteotemp(15,1) = 'VPTMP'
  meteotemp(15,2) = 'Virtual potential temperature'
  meteotemp(15,3) = 'K'
  meteotemp(16,1) = 'SNOHF'
  meteotemp(16,2) = 'Snow phase change heat flux'
  meteotemp(16,3) = 'W/(m^2)'
  meteotemp(17,1) = 'SKINT'
  meteotemp(17,2) = 'Skin temperature'
  meteotemp(17,3) = 'K'
  meteotemp(18,1) = 'SNOT'
  meteotemp(18,2) = 'Snow temperature (top)'
  meteotemp(18,3) = 'K'
  meteotemp(19,1) = 'TTCHT'
  meteotemp(19,2) = 'Turbulent tranfer coefficient for heat'
  meteotemp(19,3) = 'Numeric'
  meteotemp(20,1) = 'TDCHT'
  meteotemp(20,2) = 'Turbulent diffusion coefficient for heat'
  meteotemp(20,3) = '(m^2)/s'
  meteotemp(21,1) = 'APTMP'
  meteotemp(21,2) = 'Apparent temperature'
  meteotemp(21,3) = 'K'

  meteomoist=''
  meteomoist(0,1) =  'SPFH'
  meteomoist(0,2) =  'Specific humidity'
  meteomoist(0,3) =  'kg/kg'
  meteomoist(1,1) =  'RH'
  meteomoist(1,2) =  'Relative humidity'
  meteomoist(1,3) =  '%'
  meteomoist(2,1) =  'MIXR'
  meteomoist(2,2) =  'Humidity mixing ratio'
  meteomoist(2,3) =  'kg/kg'
  meteomoist(3,1) =  'PWAT'
  meteomoist(3,2) =  'Precipitable water'
  meteomoist(3,3) =  'kg/(m^2)'
  meteomoist(4,1) =  'VAPP'
  meteomoist(4,2) =  'Vapor pressure'
  meteomoist(4,3) =  'Pa'
  meteomoist(5,1) =  'SATD'
  meteomoist(5,2) =  'Saturation deficit'
  meteomoist(5,3) =  'Pa'
  meteomoist(6,1) =  'EVP'
  meteomoist(6,2) =  'Evaporation'
  meteomoist(6,3) =  'kg/(m^2)'
  meteomoist(7,1) =  'PRATE'
  meteomoist(7,2) =  'Precipitation rate'
  meteomoist(7,3) =  'kg/(m^2 s)'
  meteomoist(8,1) =  'APCP'
  meteomoist(8,2) =  'Total precipitation'
  meteomoist(8,3) =  'kg/(m^2)'
  meteomoist(9,1) =  'NCPCP'
  meteomoist(9,2) =  'Large scale precipitation'
  meteomoist(9,3) =  'kg/(m^2)'
  meteomoist(10,1) = 'ACPCP'
  meteomoist(10,2) = 'Convective precipitation'
  meteomoist(10,3) = 'kg/(m^2)'
  meteomoist(11,1) = 'SNOD'
  meteomoist(11,2) = 'Snow depth'
  meteomoist(11,3) = 'm'
  meteomoist(12,1) = 'SRWEQ'
  meteomoist(12,2) = 'Snowfall rare water equivalent'
  meteomoist(12,3) = 'kg/(m^2)'
  meteomoist(13,1) = 'WEASD'
  meteomoist(13,2) = 'Water equivalent of accumulated snow depth'
  meteomoist(13,3) = 'kg/(m^2)'
  meteomoist(14,1) = 'SNOC'
  meteomoist(14,2) = 'Convective snow'
  meteomoist(14,3) = 'kg/(m^2)'
  meteomoist(15,1) = 'SNOL'
  meteomoist(15,2) = 'Large scale snow'
  meteomoist(15,3) = 'kg/(m^2)'
  meteomoist(16,1) = 'SNOM'
  meteomoist(16,2) = 'Snow melt'
  meteomoist(16,3) = 'kg/(m^2)'
  meteomoist(17,1) = 'SNOAG'
  meteomoist(17,2) = 'Snow age'
  meteomoist(17,3) = 'day'
  meteomoist(18,1) = 'ABSH'
  meteomoist(18,2) = 'Absolute humidity'
  meteomoist(18,3) = 'kg/(m^3)'
  meteomoist(19,1) = 'PTYPE'
  meteomoist(19,2) = 'Precipitation type'
  meteomoist(19,3) = '(1 Rain, 2 Thunderstorm, 3 Freezing rain, 4 Mixed/ice, 5 Snow, 255 Missing)'
  meteomoist(20,1) = 'ILIQW'
  meteomoist(20,2) = 'Integrated liquid water'
  meteomoist(20,3) = 'km/(m^2)'
  meteomoist(21,1) = 'TCOND'
  meteomoist(21,2) = 'Condensate'
  meteomoist(21,3) = 'kg/kg'
  meteomoist(22,1) = 'CLWMR'
  meteomoist(22,2) = 'Cloud water mixing ratio'
  meteomoist(22,3) = 'kg/kg'
  meteomoist(23,1) = 'ICMR'
  meteomoist(23,2) = 'Ice water mixing ratio'
  meteomoist(23,3) = 'kg/kg'
  meteomoist(24,1) = 'RWMR'
  meteomoist(24,2) = 'Rain water mixing ratio'
  meteomoist(24,3) = 'kg/kg'
  meteomoist(25,1) = 'SNMR'
  meteomoist(25,2) = 'Snow water mixing ratio'
  meteomoist(25,3) = 'kg/kg'
  meteomoist(26,1) = 'MCONV'
  meteomoist(26,2) = 'Horizontal moisture convergence'
  meteomoist(26,3) = 'kg/(kg s)'
  meteomoist(27,1) = 'MAXRH'
  meteomoist(27,2) = 'Maximum relative humidity'
  meteomoist(27,3) = '%'
  meteomoist(28,1) = 'MAXAH'
  meteomoist(28,2) = 'Maximum absolute humidity'
  meteomoist(28,3) = 'kg/(m^3)'
  meteomoist(29,1) = 'ASNOW'
  meteomoist(29,2) = 'Total snowfall'
  meteomoist(29,3) = 'm'
  meteomoist(30,1) = 'PWCAT'
  meteomoist(30,2) = 'Precipitable water category'
  meteomoist(30,3) = '(undefined)'
  meteomoist(31,1) = 'HAIL'
  meteomoist(31,2) = 'Hail'
  meteomoist(31,3) = 'm'
  meteomoist(32,1) = 'GRLE'
  meteomoist(32,2) = 'Graupel (snow pellets)'
  meteomoist(32,3) = 'kg/kg'
  meteomoist(33,1) = 'CRAIN'
  meteomoist(33,2) = 'Categorical rain'
  meteomoist(33,3) = ''
  meteomoist(34,1) = 'CFRZR'
  meteomoist(34,2) = 'Categorical freezing rain'
  meteomoist(34,3) = ''
  meteomoist(35,1) = 'CICEP'
  meteomoist(35,2) = 'Categorical ice pellets'
  meteomoist(35,3) = ''
  meteomoist(36,1) = 'CSNOW'
  meteomoist(36,2) = 'Categorical snow'
  meteomoist(36,3) = ''
  meteomoist(37,1) = 'CPRAT'
  meteomoist(37,2) = 'Convective precipitation rate'
  meteomoist(37,3) = 'kg/(m^2 s)'
  meteomoist(38,1) = 'MDIVER'
  meteomoist(38,2) = 'Horizontal moisture divergence'
  meteomoist(38,3) = 'kg/(kg s)'
  meteomoist(39,1) = 'CPOFP'
  meteomoist(39,2) = 'Percent frozen precipitation'
  meteomoist(39,3) = '%'
  meteomoist(40,1) = 'PEVAP'
  meteomoist(40,2) = 'Potential evaporation'
  meteomoist(40,3) = 'kg/(m^2)'
  meteomoist(41,1) = 'PEVPR'
  meteomoist(41,2) = 'Potential evaporation rate'
  meteomoist(41,3) = 'W/(m^2)'
  meteomoist(42,1) = 'SNOWC'
  meteomoist(42,2) = 'Snow cover'
  meteomoist(42,3) = '%'
  meteomoist(43,1) = 'FRAIN'
  meteomoist(43,2) = 'Rain fraction of total cloud water'
  meteomoist(43,3) = 'Proportion'
  meteomoist(44,1) = 'RIME'
  meteomoist(44,2) = 'Rime factor'
  meteomoist(44,3) = 'Numeric'
  meteomoist(45,1) = 'TCOLR'
  meteomoist(45,2) = 'Total column integrated rain'
  meteomoist(45,3) = 'kg/(m^2)'
  meteomoist(46,1) = 'TCOLS'
  meteomoist(46,2) = 'Total column integrated snow'
  meteomoist(46,3) = 'kg/(m^2)'
  meteomoist(47,1) = 'LSWP'
  meteomoist(47,2) = 'Large scale water precipitation'
  meteomoist(47,3) = 'kg/(m^2)'
  meteomoist(48,1) = 'CWP'
  meteomoist(48,2) = 'Convective water precipitation'
  meteomoist(48,3) = 'kg/(m^2)'
  meteomoist(49,1) = 'TWAPTP'
  meteomoist(49,2) = 'Total water precipitation'
  meteomoist(49,3) = 'kg/(m^2)'
  meteomoist(50,1) = 'TSNOWP'
  meteomoist(50,2) = 'Total snow precipitation'
  meteomoist(50,3) = 'kg/(m^2)'
  meteomoist(51,1) = 'TCWAT'
  meteomoist(51,2) = 'Total column water'
  meteomoist(51,3) = 'kg/(m^2)'
  meteomoist(52,1) = 'TPRATE'
  meteomoist(52,2) = 'Total precipitation rate'
  meteomoist(52,3) = 'kg/(m^2 s)'
  meteomoist(53,1) = 'TSRWE'
  meteomoist(53,2) = 'Total snowfall rate water equiv'
  meteomoist(53,3) = 'kg/(m^2 s)'
  meteomoist(54,1) = 'LSPRATE'
  meteomoist(54,2) = 'Large scale precipitation rate'
  meteomoist(54,3) = 'kg/(m^2 s)'
  meteomoist(55,1) = 'CSRWE'
  meteomoist(55,2) = 'Convective snowfall rate water equiv'
  meteomoist(55,3) = 'kg/(m^2 s)'
  meteomoist(56,1) = 'LSSRWE'
  meteomoist(56,2) = 'Large scale snowfall rate water equiv'
  meteomoist(56,3) = 'kg/(m^2 s)'
  meteomoist(57,1) = 'TSRATE'
  meteomoist(57,2) = 'Total snowfall rate'
  meteomoist(57,3) = 'm/s'
  meteomoist(58,1) = 'CSRATE'
  meteomoist(58,2) = 'Convective snowfall rate'
  meteomoist(58,3) = 'm/s'
  meteomoist(59,1) = 'LSSRATE'
  meteomoist(59,2) = 'Large scale snowfall rate'
  meteomoist(59,3) = 'm/s'  
  meteomoist(60,1) = 'SDWE'
  meteomoist(60,2) = 'Snow depth water equiv'
  meteomoist(60,3) = 'kg/(m^2)'
  meteomoist(61,1) = 'SDEN'
  meteomoist(61,2) = 'Snow density'
  meteomoist(61,3) = 'kg/(m^2)'
  meteomoist(62,1) = 'SEVAP'
  meteomoist(62,2) = 'Snow evaporation'
  meteomoist(62,3) = 'kg/(m^2)'
  meteomoist(64,1) = 'TCIWV'
  meteomoist(64,2) = 'Total column integrated water vapour'
  meteomoist(64,3) = 'kg/(m^2)'
  meteomoist(65,1) = 'PRATE'
  meteomoist(65,2) = 'Rain precipitation rate'
  meteomoist(65,3) = 'kg/(m^2 s)'
  meteomoist(66,1) = 'SPRATE'
  meteomoist(66,2) = 'Snow precipitation rate'
  meteomoist(66,3) = 'kg/(m^2 s)'
  meteomoist(67,1) = 'FPRATE'
  meteomoist(67,2) = 'Freezing rain precipitation rate'
  meteomoist(67,3) = 'kg/(m^2 s)'
  meteomoist(68,1) = 'IPRATE'
  meteomoist(68,2) = 'Ice pellets precipitation rate'
  meteomoist(68,3) = 'kg/(m^2 s)'
  meteomoist(69,1) = 'TCOLW'
  meteomoist(69,2) = 'Total column integrate cloud water'
  meteomoist(69,3) = 'kg/(m^2)'
  meteomoist(70,1) = 'TCOLI'
  meteomoist(70,2) = 'Total column integrate cloud ice'
  meteomoist(70,3) = 'kg/(m^2)'
  meteomoist(71,1) = 'HAILMXR'
  meteomoist(71,2) = 'Hail mixing ratio'
  meteomoist(71,3) = 'kg/kg'
  meteomoist(72,1) = 'TCOLH'
  meteomoist(72,2) = 'Total column integrate hail'
  meteomoist(72,3) = 'kg/(m^2)'
  meteomoist(73,1) = 'HAILPR'
  meteomoist(73,2) = 'Hail precipitation rate'
  meteomoist(73,3) = 'kg/(m^2 s)'
  meteomoist(74,1) = 'TCOLG'
  meteomoist(74,2) = 'Total column integrate graupel'
  meteomoist(74,3) = 'kg/(m^2 s)'
  meteomoist(75,1) = 'GPRATE'
  meteomoist(75,2) = 'Graupel precipitation rate'
  meteomoist(75,3) = 'kg/(m^2 s)'
  meteomoist(76,1) = 'CRRATE'
  meteomoist(76,2) = 'Convective rain rate'
  meteomoist(76,3) = 'kg/(m^2 s)'
  meteomoist(77,1) = 'LSRRATE'
  meteomoist(77,2) = 'Large scale rain rate'
  meteomoist(77,3) = 'kg/(m^2 s)'
  meteomoist(78,1) = 'TCOLWA'
  meteomoist(78,2) = 'Total column integrate water'
  meteomoist(78,3) = 'kg/(m^2)'
  meteomoist(79,1) = 'EVARATE'
  meteomoist(79,2) = 'Evaporation rate'
  meteomoist(79,3) = 'kg/(m^2 s)'  
  meteomoist(80,1) = 'TOTCON'
  meteomoist(80,2) = 'Total condensate'
  meteomoist(80,3) = 'kg/kg'
  meteomoist(81,1) = 'TCICON'
  meteomoist(81,2) = 'Total column integrate condensate'
  meteomoist(81,3) = 'kg/(m^2)'
  meteomoist(82,1) = 'CIMIXR'
  meteomoist(82,2) = 'Cloud ice mixing ratio'
  meteomoist(82,3) = 'kg/kg'
  meteomoist(83,1) = 'SCLLWC'
  meteomoist(83,2) = 'Specific cloud liquid water content'
  meteomoist(83,3) = 'kg/kg'
  meteomoist(84,1) = 'SCLIWC'
  meteomoist(84,2) = 'Specific cloud ice water content'
  meteomoist(84,3) = 'kg/kg'
  meteomoist(85,1) = 'SRAINW'
  meteomoist(85,2) = 'Specific rain water content'
  meteomoist(85,3) = 'kg/kg'
  meteomoist(86,1) = 'SSNOWW'
  meteomoist(86,2) = 'Specific snow water content'
  meteomoist(86,3) = 'kg/kg'
  meteomoist(90,1) = 'TKMFLX'
  meteomoist(90,2) = 'Total kinematic moisture flux'
  meteomoist(90,3) = '(kg m)/(kg s)'
  meteomoist(91,1) = 'UKMFLX'
  meteomoist(91,2) = 'U-component kinematic moisture flux'
  meteomoist(91,3) = '(kg m)/(kg s)'
  meteomoist(92,1) = 'VKMFLX'
  meteomoist(92,2) = 'V-component kinematic moisture flux'
  meteomoist(92,3) = '(kg m)/(kg s)'
  meteomoist(93,1) = 'RHWATER'
  meteomoist(93,2) = 'Relative humidity with respect to water'
  meteomoist(93,3) = '%'
  meteomoist(94,1) = 'RHICE'
  meteomoist(94,2) = 'Relative humidity with respect to ice'
  meteomoist(94,3) = '%'
  meteomoist(95,1) = 'FZPRATE'
  meteomoist(95,2) = 'Freezing precipitation rate'
  meteomoist(95,3) = 'kg/(m^2 s)'
  meteomoist(96,1) = 'MASSDR'
  meteomoist(96,2) = 'Mass density of rain'
  meteomoist(96,3) = 'kg/(m^3)'
  meteomoist(97,1) = 'MASSDS'
  meteomoist(97,2) = 'Mass density of snow'
  meteomoist(97,3) = 'kg/(m^3)'
  meteomoist(98,1) = 'MASSDG'
  meteomoist(98,2) = 'Mass density of graupel'
  meteomoist(98,3) = 'kg/(m^3)'
  meteomoist(99,1) = 'MASSDH'
  meteomoist(99,2) = 'Mass density of hail'
  meteomoist(99,3) = 'kg/(m^3)'  
  meteomoist(100,1) = 'SPNCR'
  meteomoist(100,2) = 'Specific number concentation of rain'
  meteomoist(100,3) = '1/kg'
  meteomoist(101,1) = 'SPNCS'
  meteomoist(101,2) = 'Specific number concentration of snow'
  meteomoist(101,3) = '1/kg'
  meteomoist(102,1) = 'SPNCG'
  meteomoist(102,2) = 'Specific number concentration of graupel'
  meteomoist(102,3) = '1/kg'
  meteomoist(103,1) = 'SPNCH'
  meteomoist(103,2) = 'Specific number concentration of hail'
  meteomoist(103,3) = '1/kg'
  meteomoist(104,1) = 'NUMDR'
  meteomoist(104,2) = 'Number density of rain'
  meteomoist(104,3) = '1/(m^3)'
  meteomoist(105,1) = 'NUMDS'
  meteomoist(105,2) = 'Number density of snow'
  meteomoist(105,3) = '1/(m^3)'
  meteomoist(106,1) = 'NUMDG'
  meteomoist(106,2) = 'Number density of graupel'
  meteomoist(106,3) = '1/(m^3)'
  meteomoist(107,1) = 'NUMDH'
  meteomoist(107,2) = 'Number density of hail'
  meteomoist(107,3) = '1/(m^3)'
  
  meteomoment(0,1) =  'WDIR'
  meteomoment(0,2) =  'Wind direction (from which blowing)'
  meteomoment(0,3) =  'deg true'
  meteomoment(1,1) =  'WIND'
  meteomoment(1,2) =  'Wind speed'
  meteomoment(1,3) =  'm/s'
  meteomoment(2,1) =  'UGRD'
  meteomoment(2,2) =  'u-component of wind'
  meteomoment(2,3) =  'm/s'
  meteomoment(3,1) =  'VGRD'
  meteomoment(3,2) =  'v-component of wind'
  meteomoment(3,3) =  'm/s'
  meteomoment(4,1) =  'STRM'
  meteomoment(4,2) =  'Stream function'
  meteomoment(4,3) =  '(m^2)/s'
  meteomoment(5,1) =  'VPOT'
  meteomoment(5,2) =  'Velocity potential'
  meteomoment(5,3) =  '(m^2)/s'
  meteomoment(6,1) =  'MNTSF'
  meteomoment(6,2) =  'Montgomery stream function'
  meteomoment(6,3) =  '(m^2)/(s^2)'
  meteomoment(7,1) =  'SGCVV'
  meteomoment(7,2) =  'Sigma coordinate vertical velocity'
  meteomoment(7,3) =  '1/s'
  meteomoment(8,1) =  'VVEL'
  meteomoment(8,2) =  'Vertical velocity (pressure)'
  meteomoment(8,3) =  'Pa/s'
  meteomoment(9,1) =  'DZDT'
  meteomoment(9,2) =  'Vertical velocity (geometric)'
  meteomoment(9,3) =  'm/s'
  meteomoment(10,1) = 'ABSV'
  meteomoment(10,2) = 'Absolute vorticity'
  meteomoment(10,3) = '1/s'
  meteomoment(11,1) = 'ABSD'
  meteomoment(11,2) = 'Absolute divergence'
  meteomoment(11,3) = '1/s'
  meteomoment(12,1) = 'RELV'
  meteomoment(12,2) = 'Relative vorticity'
  meteomoment(12,3) = '1/s'
  meteomoment(13,1) = 'RELD'
  meteomoment(13,2) = 'Relative divergence'
  meteomoment(13,3) = '1/s'
  meteomoment(14,1) = 'PVORT'
  meteomoment(14,2) = 'Potential vorticity'
  meteomoment(14,3) = 'K(m^2)/(kg s)'
  meteomoment(15,1) = 'VUCSH'
  meteomoment(15,2) = 'Vertical u-component shear'
  meteomoment(15,3) = '1/s'
  meteomoment(16,1) = 'VVCSH'
  meteomoment(16,2) = 'Vertical v-component shear'
  meteomoment(16,3) = '1/s'
  meteomoment(17,1) = 'UFLX'
  meteomoment(17,2) = 'Momentum flux, u component'
  meteomoment(17,3) = 'N/(m^2)'
  meteomoment(18,1) = 'VFLX'
  meteomoment(18,2) = 'Momentum flux, v component'
  meteomoment(18,3) = 'N/(m^2)'
  meteomoment(19,1) = 'WMIXE'
  meteomoment(19,2) = 'Wind mixing energy'
  meteomoment(19,3) = 'J'
  meteomoment(20,1) = 'BLYDP'
  meteomoment(20,2) = 'Boundary layer dissipation'
  meteomoment(20,3) = 'W/(m^2)'
  meteomoment(21,1) = 'MAXGUST'
  meteomoment(21,2) = 'Maximum wind speed'
  meteomoment(21,3) = 'm/s'
  meteomoment(22,1) = 'GUST'
  meteomoment(22,2) = 'Wind speed (gust)'
  meteomoment(22,3) = 'm/s'
  meteomoment(23,1) = 'UGUST'
  meteomoment(23,2) = 'u-component of wind (gust)'
  meteomoment(23,3) = 'm/s'
  meteomoment(24,1) = 'VGUST'
  meteomoment(24,2) = 'v-component of wind (gust)'
  meteomoment(24,3) = 'm/s'
  meteomoment(25,1) = 'VWSH'
  meteomoment(25,2) = 'Vertical speed shear'
  meteomoment(25,3) = '1/s'
  meteomoment(26,1) = 'MFLX'
  meteomoment(26,2) = 'Horizontal momentum flux'
  meteomoment(26,3) = 'N/(m^2)'
  meteomoment(27,1) = 'USTM'
  meteomoment(27,2) = 'U-component storm motion'
  meteomoment(27,3) = 'm/s'
  meteomoment(28,1) = 'VSTM'
  meteomoment(28,2) = 'V-component storm motion'
  meteomoment(28,3) = 'm/s'
  meteomoment(29,1) = 'CD'
  meteomoment(29,2) = 'Drag coefficient'
  meteomoment(29,3) = 'Numeric'
  meteomoment(30,1) = 'FRICV'
  meteomoment(30,2) = 'Frictional velocity'
  meteomoment(30,3) = 'm/2'
  meteomoment(31,1) = 'TDCMOM'
  meteomoment(31,2) = 'Turbulent diffusion coefficient for momentum'
  meteomoment(31,3) = '(m^2)/s'
  meteomoment(32,1) = 'ETACVV'
  meteomoment(32,2) = 'Eta coordinate vertical velocity'
  meteomoment(32,3) = '1/s'
  meteomoment(33,1) = 'WINDF'
  meteomoment(33,2) = 'Wind fetch'
  meteomoment(33,3) = 'm'
  meteomoment(34,1) = 'NWIND'
  meteomoment(34,2) = 'Normal wind component'
  meteomoment(34,3) = 'm/s'
  meteomoment(35,1) = 'TWIND'
  meteomoment(35,2) = 'Tengential wind component'
  meteomoment(35,3) = 'm/s'
  meteomoment(36,1) = 'AFRWE'
  meteomoment(36,2) = 'Amplitude function for Rossby wave'
  meteomoment(36,3) = 'm/s'
  meteomoment(37,1) = 'NTSS'
  meteomoment(37,2) = 'Northward turbulent surface stress'
  meteomoment(37,3) = 'N s/(m^2)'
  meteomoment(38,1) = 'ETSS'
  meteomoment(38,2) = 'Eastward turbulent surface stress'
  meteomoment(38,3) = 'N s/(m^2)'

  meteomass(0,1) =  'PRES'
  meteomass(0,2) =  'Pressure'
  meteomass(0,3) =  'Pa'
  meteomass(1,1) =  'PRMSL'
  meteomass(1,2) =  'Pressure reduced to MSL'
  meteomass(1,3) =  'Pa'
  meteomass(2,1) =  'PTEND'
  meteomass(2,2) =  'Pressure tendency'
  meteomass(2,3) =  'Pa/s'
  meteomass(3,1) =  'ICAHT'
  meteomass(3,2) =  'ICA0 standard atmosphere reference height'
  meteomass(3,3) =  'm'
  meteomass(4,1) =  'GP'
  meteomass(4,2) =  'Geopotential'
  meteomass(4,3) =  '(m^2)/(s^2)'
  meteomass(5,1) =  'HGT'
  meteomass(5,2) =  'Geopotential height'
  meteomass(5,3) =  'gpm'
  meteomass(6,1) =  'DIST'
  meteomass(6,2) =  'Geometric height'
  meteomass(6,3) =  'm'
  meteomass(7,1) =  'HSTDV'
  meteomass(7,2) =  'Standard deviation of height'
  meteomass(7,3) =  'm'
  meteomass(8,1) =  'PRESA'
  meteomass(8,2) =  'Pressure anomaly'
  meteomass(8,3) =  'Pa'
  meteomass(9,1) =  'GPA'
  meteomass(9,2) =  'Geopotential height anomally'
  meteomass(9,3) =  'gpm'
  meteomass(10,1) = 'DEN'
  meteomass(10,2) = 'Density'
  meteomass(10,3) = 'kg/(m^3)'
  meteomass(11,1) = 'ALTS'
  meteomass(11,2) = 'Altimeter setting'
  meteomass(11,3) = 'Pa'
  meteomass(12,1) = 'THICK'
  meteomass(12,2) = 'Thickness'
  meteomass(12,3) = 'm'
  meteomass(13,1) = 'PRESALT'
  meteomass(13,2) = 'Pressure altitude'
  meteomass(13,3) = 'm'
  meteomass(14,1) = 'DENALT'
  meteomass(14,2) = 'Density altitude'
  meteomass(14,3) = 'm'
  meteomass(15,1) = '5WAVH'
  meteomass(15,2) = '5-wave geopotential height'
  meteomass(15,3) = 'gpm'
  meteomass(16,1) = 'U-GWD'
  meteomass(16,2) = 'Zonal flux of gravity wave stress'
  meteomass(16,3) = 'N/(m^2)'
  meteomass(17,1) = 'V-GWD'
  meteomass(17,2) = 'Meridional flux of gravity wave stress'
  meteomass(17,3) = 'N/(m^2)'
  meteomass(18,1) = 'HPBL'
  meteomass(18,2) = 'Planetary boundary layer height'
  meteomass(18,3) = 'm'
  meteomass(19,1) = '5WAVA'
  meteomass(19,2) = '5-Wave geopotential height anomaly'
  meteomass(19,3) = 'gpm'
  meteomass(20,1) = 'SDSGSO'
  meteomass(20,2) = 'Standard deviation of sub-grid scale orography'
  meteomass(20,3) = 'm'
  meteomass(21,1) = 'AOSGSO'
  meteomass(21,2) = 'Angle of sub-grid scale orography'
  meteomass(21,3) = 'rad'
  meteomass(22,1) = 'SSGSO'
  meteomass(22,2) = 'Slope of sub-grid scale orography'
  meteomass(22,3) = 'Numeric'
  meteomass(23,1) = 'GWD'
  meteomass(23,2) = 'Gravity wave dissipation'
  meteomass(23,3) = 'W/(m^2)'
  meteomass(24,1) = 'ASGSO'
  meteomass(24,2) = 'Anisotropy of sub-grid scale orography'
  meteomass(24,3) = 'Numeric'
  meteomass(25,1) = 'NLPRES'
  meteomass(25,2) = 'Natural logarithm of pressure'
  meteomass(25,3) = 'Numeric'
  meteomass(26,1) = 'EXPRES'
  meteomass(26,2) = 'Exner pressure'
  meteomass(26,3) = 'Numeric'
  
  meteoshortradiate(0,1)='NSWRS'
  meteoshortradiate(0,2)='Net short-wave radiation flux (surface)'
  meteoshortradiate(0,3)='W/(m^2)'
  meteoshortradiate(1,1)='NSWRT'
  meteoshortradiate(1,2)='Net short-wave radiation flux (top of atmosphere)'
  meteoshortradiate(1,3)='W/(m^2)'
  meteoshortradiate(2,1)='SWAVR'
  meteoshortradiate(2,2)='Short wave radiation flux'
  meteoshortradiate(2,3)='W/(m^2)'
  meteoshortradiate(3,1)='GRAD'
  meteoshortradiate(3,2)='Global radiation flux'
  meteoshortradiate(3,3)='W/(m^2)'
  meteoshortradiate(4,1)='BRTMP'
  meteoshortradiate(4,2)='Brightness temperature'
  meteoshortradiate(4,3)='K'
  meteoshortradiate(5,1)='LWRAD'
  meteoshortradiate(5,2)='Radiance (with respect to wave number)'
  meteoshortradiate(5,3)='W/(m sr)'
  meteoshortradiate(6,1)='SWARD'
  meteoshortradiate(6,2)='Radiance (with respect to wave length'
  meteoshortradiate(6,3)='W/(m^3 sr)'

  meteolongradiate(0,1)='NLWRS'
  meteolongradiate(0,2)='Net long wave radiation flux (surface)'
  meteolongradiate(0,3)='W/(m^2)'
  meteolongradiate(1,1)='NLWRT'
  meteolongradiate(1,2)='Net long wave radiation flux (top of atmosphere)'
  meteolongradiate(1,3)='W/(m^2)'
  meteolongradiate(2,1)='LWAVR'
  meteolongradiate(2,2)='Long wave radiation flux'
  meteolongradiate(2,3)='W/(m^2)'

  meteocloud=''
  meteocloud(0,1)='CICE'
  meteocloud(0,2)='Cloud Ice'
  meteocloud(0,3)='kg/(m^2)'
  meteocloud(1,1)='TCDC'
  meteocloud(1,2)='Total cloud cover'
  meteocloud(1,3)='%'
  meteocloud(2,1)='CDCON'
  meteocloud(2,2)='Convective cloud cover'
  meteocloud(2,3)='%'
  meteocloud(3,1)='LCDC'
  meteocloud(3,2)='Low cloud cover'
  meteocloud(3,3)='%'
  meteocloud(4,1)='MCDC'
  meteocloud(4,2)='Medium cloud cover'
  meteocloud(4,3)='%'
  meteocloud(5,1)='HCDC'
  meteocloud(5,2)='High cloud cover'
  meteocloud(5,3)='%'
  meteocloud(6,1)='CWAT'
  meteocloud(6,2)='Could water'
  meteocloud(6,3)='kg/(m^2)'
  meteocloud(7,1)='CDCA'
  meteocloud(7,2)='Cloud amount'
  meteocloud(7,3)='%'
  meteocloud(8,1)='CDCT'
  meteocloud(8,2)='Cloud type'
  meteocloud(8,3)="index"
!      //"(0 clear, 1 Cumulonimbus, 2 Stratus, 3 Stratocumulus, 4 Cumulus, 5 Altostratus, 6 Nimbostratus, " &
!      //"7 Altocumulus, 8 Cirrostratus, 9 Cirrocumulus, 10 Cirrus, 11 Cumulonimbus (fog), 12 Stratus (fog), "        &
!      //"13 Stratocumulus (fog), 14 Cumulus (fog), Altostratus (fog), 16 Nimbostratus (fog), 17 Altocumulus (fog), " &
!      //"18 Cirrostratus (fog), 19 Cirrocumulus (fog), 20 Cirrus (fog), 191 unknown, 255 missing)"
  meteocloud(9,1)='TMAXT'
  meteocloud(9,2)='Thunderstorm maximum tops'
  meteocloud(9,3)='m'
  meteocloud(10,1)='THUNC'
  meteocloud(10,2)='Thunderstorm coverage'
  meteocloud(10,3)='index'
!      //'(0 none, 1 isolated (1%-2%), 2 few (3%-15%), 3 scattered (16%-45%), 4 numerous (>45%), 255 missing)'
  meteocloud(11,1)='CDCB'
  meteocloud(11,2)='Cloud base'
  meteocloud(11,3)='m'
  meteocloud(12,1)='CDCT'
  meteocloud(12,2)='Cloud top'
  meteocloud(12,3)='m'
  meteocloud(13,1)='CEIL'
  meteocloud(13,2)='Ceiling'
  meteocloud(13,3)='m'
  meteocloud(14,1)='CDLYR'
  meteocloud(14,2)='Non-convective cloud cover'
  meteocloud(14,3)='%'
  meteocloud(15,1)='CWORK'
  meteocloud(15,2)='Cloud work function'
  meteocloud(15,3)='J/kg'
  meteocloud(16,1)='CUEFI'
  meteocloud(16,2)='Convective cloud efficiency'
  meteocloud(16,3)='-'
  meteocloud(17,1)='TCOND'
  meteocloud(17,2)='Total condensate'
  meteocloud(17,3)='kg/kg'
  meteocloud(18,1)='TCOLW'
  meteocloud(18,2)='Total column-integrated cloud water'
  meteocloud(18,3)='kg/(m^2)'
  meteocloud(19,1)='TCOLI'
  meteocloud(19,2)='Total column-integrated cloud ice'
  meteocloud(19,3)='kg/(m^2)'
  meteocloud(20,1)='TCOLC'
  meteocloud(20,2)='Total column-integrated cloud condensate'
  meteocloud(20,3)='kg/(m^2)'
  meteocloud(21,1)='FICE'
  meteocloud(21,2)='Ice fraction of total condensate'
  meteocloud(21,3)='-'
  meteocloud(22,1)='CDCC'
  meteocloud(22,2)='Cloud cover'
  meteocloud(22,3)='%'
  meteocloud(23,1)='CDCIMR'
  meteocloud(23,2)='Cloud ice mixing ratio'
  meteocloud(23,3)='kg/kg'
  meteocloud(24,1)='SUNS'
  meteocloud(24,2)='Sunshine'
  meteocloud(24,3)='-'
  meteocloud(25,1)='CBHE'
  meteocloud(25,2)='Horizontal extent of cumulonibus'
  meteocloud(25,3)='%'
  meteocloud(33,1)='SUNSD'
  meteocloud(33,2)='Sunshine duration'
  meteocloud(33,3)='s'
  meteocloud(192,1)='CDLYR'
  meteocloud(192,2)='Non-convective cloud cover'
  meteocloud(192,3)='%'
  meteocloud(193,1)='CWORK'
  meteocloud(193,2)='Cloud work function'
  meteocloud(193,3)='J/kg'
  meteocloud(194,1)='CUEFI'
  meteocloud(194,2)='Convective cloud efficiency'
  meteocloud(194,3)='-'
  meteocloud(195,1)='TCOND'
  meteocloud(195,2)='Total condensate'
  meteocloud(195,3)='kg/kg'
  meteocloud(196,1)='TCOLW'
  meteocloud(196,2)='Total column-integrated cloud water'
  meteocloud(196,3)='kg/(m^2)'
  meteocloud(197,1)='TCOLI'
  meteocloud(197,2)='Total column-integrated cloud ice'
  meteocloud(197,3)='kg/(m^2)'
  meteocloud(198,1)='TCOLC'
  meteocloud(198,2)='Total column-integrated cloud condensate'
  meteocloud(198,3)='kg/(m^2)'
  meteocloud(199,1)='FICE'
  meteocloud(199,2)='Ice fraction of total condensate'
  meteocloud(199,3)='-'
  meteocloud(200,1)='MFLUX'
  meteocloud(200,2)='Convective cloud mass flux'
  meteocloud(200,3)='Pa/s'
  meteocloud(201,1)='SUNSD'
  meteocloud(201,2)='Sunshine duration'
  meteocloud(201,3)='s'

  meteostability(0,1)='PLI'
  meteostability(0,2)='Parcel lifted index (to 500 hPa)'
  meteostability(0,3)='K'
  meteostability(1,1)='BLI'
  meteostability(1,2)='Best lifted index (to 500 hPa)'
  meteostability(1,3)='K'
  meteostability(2,1)='KX'
  meteostability(2,2)='K index'
  meteostability(2,3)='K'
  meteostability(3,1)=''
  meteostability(3,2)='K0 index'
  meteostability(3,3)='K'
  meteostability(4,1)=''
  meteostability(4,2)='Total totals index'
  meteostability(4,3)='K'
  meteostability(5,1)='SX'
  meteostability(5,2)='Sweat index'
  meteostability(5,3)='numeric'
  meteostability(6,1)='CAPE'
  meteostability(6,2)='Convective available potential energy'
  meteostability(6,3)='J/kg'
  meteostability(7,1)='CIN'
  meteostability(7,2)='Convective inhibition'
  meteostability(7,3)='J/kg'
  meteostability(8,1)='HLCY'
  meteostability(8,2)='Storm relative helicity'
  meteostability(8,3)='J/kg'
  meteostability(9,1)=''
  meteostability(9,2)='Energy helicity index'
  meteostability(9,3)='numeric'

  meteoaerosols(0,1)=''
  meteoaerosols(0,2)='Aerosol type'
  meteoaerosols(0,3)='(0 Aerosol not present, 1 Aerosol present)'

  meteogases(0,1)='TOZNE'
  meteogases(0,2)='Total ozone'
  meteogases(0,3)='Dobson'

  meteoradar(0,1)=''
  meteoradar(0,2)='Base spectrum width'
  meteoradar(0,3)='m/s'
  meteoradar(1,1)=''
  meteoradar(1,2)='Base reflectivity'
  meteoradar(1,3)='dB'
  meteoradar(2,1)=''
  meteoradar(2,2)='Base radial velocity'
  meteoradar(2,3)='m/s'
  meteoradar(3,1)=''
  meteoradar(3,2)='Vertically-integrated liquid'
  meteoradar(3,3)='kg/m'
  meteoradar(4,1)=''
  meteoradar(4,2)='Layer-maximum base reflectivity'
  meteoradar(4,3)='dB'
  meteoradar(5,1)=''
  meteoradar(5,2)='Precipitation'
  meteoradar(5,3)='kg/(m^2)'
  meteoradar(6,1)='RDSP1'
  meteoradar(6,2)='Radar spectra (1)'
  meteoradar(6,3)='-'
  meteoradar(7,1)='RDSP2'
  meteoradar(7,2)='Radar spectra (2)'
  meteoradar(7,3)='-'
  meteoradar(8,1)='RDSP3'
  meteoradar(8,2)='Radar spectra (3)'
  meteoradar(8,3)='-'

  meteonuclear(0,1)=''
  meteonuclear(0,2)='Air concentration of Caesium 137'
  meteonuclear(0,3)='Bq/(m^3)'
  meteonuclear(1,1)=''
  meteonuclear(1,2)='Air concentration of Iodine 131'
  meteonuclear(1,3)='Bq/(m^3)'
  meteonuclear(2,1)=''
  meteonuclear(2,2)='Air concentration of radioactive pollutant'
  meteonuclear(2,3)='Bq/(m^3)'
  meteonuclear(3,1)=''
  meteonuclear(3,2)='Ground deposition of Caesium 137'
  meteonuclear(3,3)='Bq/(m^2)'
  meteonuclear(4,1)=''
  meteonuclear(4,2)='Ground deposition of Iodine 131'
  meteonuclear(4,3)='Bq/(m^2)'
  meteonuclear(5,1)=''
  meteonuclear(5,2)='Ground deposition of radioactive pollutant'
  meteonuclear(5,3)='Bq/(m^2)'
  meteonuclear(6,1)=''
  meteonuclear(6,2)='Time-integrated air concentration of caesium pollutant'
  meteonuclear(6,3)='(Bq s)/(m^3)'
  meteonuclear(7,1)=''
  meteonuclear(7,2)='Time-integrated air concentration of iodine pollutant'
  meteonuclear(7,3)='(Bq s)/(m^3)'
  meteonuclear(8,1)=''
  meteonuclear(8,2)='Time-integrated air concentration of radioactive pollutant'
  meteonuclear(8,3)='(Bq s)/(m^3)'

  meteoatmos(0,1)='VIS'
  meteoatmos(0,2)='Visibility'
  meteoatmos(0,3)='m'
  meteoatmos(1,1)='ALBDO'
  meteoatmos(1,2)='Albedo'
  meteoatmos(1,3)='%'
  meteoatmos(2,1)='TSTM'
  meteoatmos(2,2)='Thunderstorm probability'
  meteoatmos(2,3)='%'
  meteoatmos(3,1)='MIXHT'
  meteoatmos(3,2)='Mixed layer depth'
  meteoatmos(3,3)='m'
  meteoatmos(4,1)=''
  meteoatmos(4,2)='Volcanic ash'
  meteoatmos(4,3)='(0 not present, 1 present, 255 missing)'
  meteoatmos(5,1)=''
  meteoatmos(5,2)='Icing top'
  meteoatmos(5,3)='m'
  meteoatmos(6,1)=''
  meteoatmos(6,2)='Icing base'
  meteoatmos(6,3)='m'
  meteoatmos(7,1)=''
  meteoatmos(7,2)='Icing'
  meteoatmos(7,3)='(0 None, 1 light, 2 moderate, 3 severe, 255 missing)'
  meteoatmos(8,1)=''
  meteoatmos(8,2)='Turbulance top'
  meteoatmos(8,3)='m'
  meteoatmos(9,1)=''
  meteoatmos(9,2)='Turbulence base'
  meteoatmos(9,3)='m'
  meteoatmos(10,1)=''
  meteoatmos(10,2)='Turbulence'
  meteoatmos(10,3)='(0 None(smooth), 1 light, 2 moderate, 3 severe, 4 extreme, 255 missing)'
  meteoatmos(11,1)='TKE'
  meteoatmos(11,2)='Turbulent kinetic energy'
  meteoatmos(11,3)='J/kg'
  meteoatmos(12,1)=''
  meteoatmos(12,2)='Planetary boundary layer regime'
  meteoatmos(12,3)='(0 reserved, 1 stable, 2 mechanically driven turbulence, 3 forced convection, 4 free convection, 255 missing)'
  meteoatmos(13,1)=''
  meteoatmos(13,2)='Contrail intensity'
  meteoatmos(13,3)='(0 Contrail not present, 1 contrail present, 255 missing)'
  meteoatmos(14,1)=''
  meteoatmos(14,2)='Contrail engine type'
  meteoatmos(14,3)='(0 Low bypass, 1 high bypass, 2 non bypass, 255 missing)'
  meteoatmos(15,1)=''
  meteoatmos(15,2)='Contrail top'
  meteoatmos(15,3)='m'
  meteoatmos(16,1)=''
  meteoatmos(16,2)='Contrail base'
  meteoatmos(16,3)='m'

  meteotext(0,1)=''
  meteotext(0,2)='Arbitrary test string'
  meteotext(0,3)='CCITTIA5'

  hydrobasic(0,1)=''
  hydrobasic(0,2)='Flash flood guidance'
  hydrobasic(0,3)='kg/(m^2)'
  hydrobasic(1,1)=''
  hydrobasic(1,2)='Flash flood runoff'
  hydrobasic(1,3)='kg/(m^2)'
  hydrobasic(2,1)=''
  hydrobasic(2,2)='Remotely sensed snow cover'
  hydrobasic(2,3)='(50 no-snow/no-cloud, 100 clouds, 250 snow, 255 missing)'
  hydrobasic(3,1)=''
  hydrobasic(3,2)='Elevation of snow covered terrain'
  hydrobasic(3,3)='(0-90 elevation in increments of 100m, 254 clouds, 255 missing)'
  hydrobasic(4,1)=''
  hydrobasic(4,2)='Snow water equivalent precent of normal'
  hydrobasic(4,3)='%'

  hydroprob(0,1)=''
  hydroprob(0,2)='Conditional percent precipitation amount factile for an overall period'
  hydroprob(0,3)='kg/(m^2)'
  hydroprob(1,1)=''
  hydroprob(1,2)='Percent precipitation in a sub-period of an overall period'
  hydroprob(1,3)='%'
  hydroprob(2,1)=''
  hydroprob(2,2)='Probability of 0.01 inch of precipitation (POP)'
  hydroprob(2,3)='%'

  landveg(0,1)='LAND'
  landveg(0,2)='Landcover (0=sea 1=land)'
  landveg(0,3)='Proportion'
  landveg(1,1)='SFCR'
  landveg(1,2)='Surface roughness'
  landveg(1,3)='m'
  landveg(2,1)='TSOIL'
  landveg(2,2)='Soil temperature'
  landveg(2,3)='K'
  landveg(3,1)='SOILM'
  landveg(3,2)='Soil moisture content'
  landveg(3,3)='kg/(m^2)'
  landveg(4,1)='VEG'
  landveg(4,2)='Vegetation'
  landveg(4,3)='%'
  landveg(5,1)='WATR'
  landveg(5,2)='Water runoff'
  landveg(5,3)='kg/(m^2)'
  landveg(6,1)=''
  landveg(6,2)='Evapotranspiration'
  landveg(6,3)='1/(kg^2 s)'
  landveg(7,1)=''
  landveg(7,2)='Model terrain height'
  landveg(7,3)='m'
  landveg(8,1)=''
  landveg(8,2)='Land use'
  landveg(8,3)='(1 Urban land, 2 agriculture, 3 range land, 4 deciduous forest, 5 coniferous forest, 6 forest/wetland, ' &
      //'7 water, 8 wetlands, 9 desert, 10 tundra, 11 ice, 12 tropical forest, 13 savannah'

  landsoil=''
  landsoil(0,1)='SOTYP'
  landsoil(0,2)='Soil type'
  landsoil(0,3)='(1 Sand, 2 loamy sand, 3 sandy loam, 4 silt loam, 5 orgainc (redefined), 6 sandy clay loam, ' &
      //'7 silt clay loam, 8 clay loam, 9 sandy clay, 10 silty clay, 11 clay)'
  landsoil(1,1)='UPLST'
  landsoil(1,2)='Upper layer soil temperature'
  landsoil(1,3)='K'
  landsoil(2,1)='UPLSM'
  landsoil(2,2)='Upper layer soil moisture'
  landsoil(2,3)='kg/(m^3)'
  landsoil(3,1)='LOWLSM'
  landsoil(3,2)='Lower layer soil moisture'
  landsoil(3,3)='kg/(m^3)'
  landsoil(4,1)='BOTLST'
  landsoil(4,2)='Bottom layer soil temperature'
  landsoil(4,3)='K'
  landsoil(5,1)='SOILL'
  landsoil(5,2)='Liquid volumetric soil moisture'
  landsoil(5,3)='-'
  landsoil(6,1)='RLYRS'
  landsoil(6,2)='Number of soil layers in root zone'
  landsoil(6,3)='-'
  landsoil(7,1)='SMREF'
  landsoil(7,2)='Transpiration stress-onset'
  landsoil(7,3)='-'
  landsoil(8,1)='SMDRY'
  landsoil(8,2)='Direct evaporation cease'
  landsoil(8,3)='-'
  landsoil(9,1)='POROS'
  landsoil(9,2)='Soil porosity'
  landsoil(9,3)='-'
  landsoil(10,1)='LIQVSM'
  landsoil(10,2)='Liquid volumetric soil moisture'
  landsoil(10,3)='m^3/(m^3)'
  landsoil(11,1)='VOLTSO'
  landsoil(11,2)='Volumetric transpiration stree-onset'
  landsoil(11,3)='m^3/(m^3)'
  landsoil(12,1)='TRANSO'
  landsoil(12,2)='Transpiration stree-onset'
  landsoil(12,3)='kg/(m^3)'
  landsoil(13,1)='VOLDEC'
  landsoil(13,2)='Volumetric direct evaporation cease'
  landsoil(13,3)='m^3/(m^3)'
  landsoil(14,1)='DIREC'
  landsoil(14,2)='Direct evaporation cease'
  landsoil(14,3)='kg/(m^3)'
  landsoil(15,1)='SOILP'
  landsoil(15,2)='Soil porosity'
  landsoil(15,3)='m^3/(m^3)'
  landsoil(16,1)='VSOSM'
  landsoil(16,2)='Volumetric saturation of soil moisture'
  landsoil(16,3)='m^3/(m^3)'
  landsoil(17,1)='SATOSM'
  landsoil(17,2)='Saturation of soil moisture'
  landsoil(17,3)='kg/(m^3)'
  landsoil(192,1)='SOILL'
  landsoil(192,2)='Liquid volumetric soil moisture'
  landsoil(192,3)='-'
  landsoil(193,1)='RLYRS'
  landsoil(193,2)='Number of soil layers in root zone'
  landsoil(193,3)='-'
  landsoil(194,1)='SLTYP'
  landsoil(194,2)='Surface slope type'
  landsoil(194,3)='-'
  landsoil(195,1)='SMREF'
  landsoil(195,2)='Transpiration stress-onset'
  landsoil(195,3)='-'
  landsoil(196,1)='SMDRY'
  landsoil(196,2)='Direct evaporation cease'
  landsoil(196,3)='-'
  landsoil(197,1)='POROS'
  landsoil(197,2)='Soil porosity'
  landsoil(197,3)='-'
  landsoil(198,1)='EVBS'
  landsoil(198,2)='Direct evaporation from bare soil'
  landsoil(198,3)='W/(m^2)'
  landsoil(199,1)='LSPA'
  landsoil(199,2)='Land surface precipitation accumulation'
  landsoil(199,3)='kg/(m^2)'
  landsoil(200,1)='BARET'
  landsoil(200,2)='Bare soil surface skin temperature'
  landsoil(200,3)='K'
  landsoil(201,1)='AVSFT'
  landsoil(201,2)='Average surface skin temperature'
  landsoil(201,3)='K'
  landsoil(202,1)='RADT'
  landsoil(202,2)='Effective radiative skin temperature'
  landsoil(202,3)='K'
  landsoil(203,1)='FLDCP'
  landsoil(203,2)='Field capacity'
  landsoil(203,3)='-'

  landfire=''
  landfire(0,1) = 'FIREOLK'
  landfire(0,2) = 'Fire outlook'
  landfire(0,3) = ''
  landfire(1,1) = 'FIREODT'
  landfire(1,2) = 'Fire outlook due to dry thunderstorms'
  landfire(1,3) = ''
  landfire(2,1) = 'HINDEX'
  landfire(2,2) = 'Haines index'
  landfire(2,3) = 'numeric'
  landfire(3,1) = 'FBAREA'
  landfire(3,2) = 'Fire burned area'
  landfire(3,3) = '%'
  landfire(4,1) = 'FOSINDX'
  landfire(4,2) = 'Fosberg Index'
  landfire(4,3) = 'Numeric'

  spaceimage(0,1)=''
  spaceimage(0,2)='Scaled radiance'
  spaceimage(0,3)='numeric'
  spaceimage(1,1)=''
  spaceimage(1,2)='Scaled albedo'
  spaceimage(1,3)='numeric'
  spaceimage(2,1)=''
  spaceimage(2,2)='Scaled brightness temperature'
  spaceimage(2,3)='numeric'
  spaceimage(3,1)=''
  spaceimage(3,2)='Scaled precipitable water'
  spaceimage(3,3)='numeric'
  spaceimage(4,1)=''
  spaceimage(4,2)='Scaled lifted index'
  spaceimage(4,3)='numeric'
  spaceimage(5,1)=''
  spaceimage(5,2)='Scaled cloud top pressure'
  spaceimage(5,3)='numeric'
  spaceimage(6,1)=''
  spaceimage(6,2)='Scaled skin temperature'
  spaceimage(6,3)='numeric'
  spaceimage(7,1)=''
  spaceimage(7,2)='Cloud mask'
  spaceimage(7,3)='(0 Clear over water, 1 clear over land, 2 cloud)'

  spacequantitative(0,1)=''
  spacequantitative(0,2)='Estimated precipitation'
  spacequantitative(0,3)='kg/(m^2)'

  oceanwaves(0,1)='WVSP1'
  oceanwaves(0,2)='Wave spectra (1)'
  oceanwaves(0,3)='-'
  oceanwaves(1,1)='WVSP2'
  oceanwaves(1,2)='Wave spectra (2)'
  oceanwaves(1,3)='-'
  oceanwaves(2,1)='WVSP3'
  oceanwaves(2,2)='Wave spectra (3)'
  oceanwaves(2,3)='-'
  oceanwaves(3,1)='HTSGW'
  oceanwaves(3,2)='Significant height of combined wind waves and swell'
  oceanwaves(3,3)='m'
  oceanwaves(4,1)='WVDIR'
  oceanwaves(4,2)='Direction of wind waves'
  oceanwaves(4,3)='Degree true'
  oceanwaves(5,1)='WVHGT'
  oceanwaves(5,2)='Significant height of wind waves'
  oceanwaves(5,3)='m'
  oceanwaves(6,1)='WVPER'
  oceanwaves(6,2)='Mean period of wind waves'
  oceanwaves(6,3)='s'
  oceanwaves(7,1)='SWDIR'
  oceanwaves(7,2)='Direction of swell waves'
  oceanwaves(7,3)='Degree true'
  oceanwaves(8,1)='SWELL'
  oceanwaves(8,2)='Significant height of swell waves'
  oceanwaves(8,3)='m'
  oceanwaves(9,1)='SWPER'
  oceanwaves(9,2)='Mean period of swell waves'
  oceanwaves(9,3)='s'
  oceanwaves(10,1)='DIRPW'
  oceanwaves(10,2)='Primary wave direction'
  oceanwaves(10,3)='Degree true'
  oceanwaves(11,1)='PERPW'
  oceanwaves(11,2)='Primary wave mean period'
  oceanwaves(11,3)='s'
  oceanwaves(12,1)='DIRSW'
  oceanwaves(12,2)='Secondary wave direction'
  oceanwaves(12,3)='Degree true'
  oceanwaves(13,1)='PERSW'
  oceanwaves(13,2)='Secondary wave mean period'
  oceanwaves(13,3)='s'

  oceancurrents(0,1)='DIRC'
  oceancurrents(0,2)='Current direction'
  oceancurrents(0,3)='Degree true'
  oceancurrents(1,1)='SPC'
  oceancurrents(1,2)='Current speed'
  oceancurrents(1,3)='m/s'
  oceancurrents(2,1)='U0GRD'
  oceancurrents(2,2)='u-component of current'
  oceancurrents(2,3)='m/s'
  oceancurrents(3,1)='v0GRD'
  oceancurrents(3,2)='v-component of current'
  oceancurrents(3,3)='m/s'

  oceanice(0,1)='ICEC'
  oceanice(0,2)='Ice cover'
  oceanice(0,3)='Proportion'
  oceanice(1,1)='ICETK'
  oceanice(1,2)='Ice thickness'
  oceanice(1,3)='m'
  oceanice(2,1)='DICED'
  oceanice(2,2)='Direction of ice drift'
  oceanice(2,3)='Degree true'
  oceanice(3,1)='SICED'
  oceanice(3,2)='Speed of ice drift'
  oceanice(3,3)='m/s'
  oceanice(4,1)='UICE'
  oceanice(4,2)='u-component of ice drift'
  oceanice(4,3)='m/s'
  oceanice(5,1)='VICE'
  oceanice(5,2)='v-component of ice drift'
  oceanice(5,3)='m/s'
  oceanice(6,1)='ICEG'
  oceanice(6,2)='Ice growth rate'
  oceanice(6,3)='m/s'
  oceanice(7,1)='ICED'
  oceanice(7,2)='Ice divergence'
  oceanice(7,3)='1/s'

  oceansurface(0,1)='WTMP'
  oceansurface(0,2)='Water temperature'
  oceansurface(0,3)='K'
  oceansurface(1,1)='DSLM'
  oceansurface(1,2)='Deviation of sea level from mean'
  oceansurface(1,3)='m'

  oceansubsurface(0,1)='MTHD'
  oceansubsurface(0,2)='Main thermocline depth'
  oceansubsurface(0,3)='m'
  oceansubsurface(1,1)='MTHA'
  oceansubsurface(1,2)='Main thermocline anomaly'
  oceansubsurface(1,3)='m'
  oceansubsurface(2,1)='TTHDP'
  oceansubsurface(2,2)='Transient thermocline depth'
  oceansubsurface(2,3)='m'
  oceansubsurface(3,1)='SALTY'
  oceansubsurface(3,2)='Salinity'
  oceansubsurface(3,3)='kg/kg'

  nceplcltable000000(192,1)='SNOHF'
  nceplcltable000000(192,2)='Snow phase change heat flux'
  nceplcltable000000(192,3)='W/(m^2)'
  nceplcltable000000(193,1) = 'TTRAD'
  nceplcltable000000(193,2) = 'Temperature tendency by all radiation'
  nceplcltable000000(193,3) = 'K/s'
  nceplcltable000000(194,1) = 'REV'
  nceplcltable000000(194,2) = 'Relative error variance'
  nceplcltable000000(194,3) = ''
  nceplcltable000000(195,1) = 'LRGHR'
  nceplcltable000000(195,2) = 'Large scale condensate heating rate'
  nceplcltable000000(195,3) = 'K/s'
  nceplcltable000000(196,1) = 'CNVHR'
  nceplcltable000000(196,2) = 'Deep convective heating rate'
  nceplcltable000000(196,3) = 'K/s'
  nceplcltable000000(197,1) = 'THFLX'
  nceplcltable000000(197,2) = 'Total downward heat flux at surface'
  nceplcltable000000(197,3) = 'W/(m^2)'
  nceplcltable000000(198,1) = 'TTDIA'
  nceplcltable000000(198,2) = 'Temperature tendency by all physics'
  nceplcltable000000(198,3) = 'K/s'
  nceplcltable000000(199,1) = 'TTPHY'
  nceplcltable000000(199,2) = 'Temperature tendency by non-radiation physics'
  nceplcltable000000(199,3) = 'K/s'
  nceplcltable000000(200,1) = 'TSD1D'
  nceplcltable000000(200,2) = 'Standard dev of IR temp 1x1deg'
  nceplcltable000000(200,3) = 'K'
  nceplcltable000000(201,1) = 'SHAHR'
  nceplcltable000000(201,2) = 'Shallow convective heating rate'
  nceplcltable000000(201,3) = 'K/s'
  nceplcltable000000(202,1) = 'VDFHR'
  nceplcltable000000(202,2) = 'Vertical diffusion heating rate'
  nceplcltable000000(202,3) = 'K/s'
  nceplcltable000000(203,1) = 'THZ0'
  nceplcltable000000(203,2) = 'Potential temperature at top of viscous sublayer'
  nceplcltable000000(203,3) = 'K'
  nceplcltable000000(204,1) = 'TCHP'
  nceplcltable000000(204,2) = 'Tropical cyclone heat potential'
  nceplcltable000000(204,3) = '(J K)/(m^2)'

  nceplcltable000001=''
  nceplcltable000001(192,1)='CRAIN'
  nceplcltable000001(192,2)='Categorical rain'
  nceplcltable000001(192,3)='(0 no, 1 yes)'
  nceplcltable000001(193,1)='CFRZR'
  nceplcltable000001(193,2)='Categorical freezing rain'
  nceplcltable000001(193,3)='(0 no, 1 yes)'
  nceplcltable000001(194,1)='CICEP'
  nceplcltable000001(194,2)='Categorical ice pellets'
  nceplcltable000001(194,3)='(0 no, 1 yes)'
  nceplcltable000001(195,1)='CSNOW'
  nceplcltable000001(195,2)='Categorical snow'
  nceplcltable000001(195,3)='(0 no, 1 yes)'
  nceplcltable000001(196,1)='CPRAT'
  nceplcltable000001(196,2)='Convective precipitation rate'
  nceplcltable000001(196,3)='(kg s)/(m^2)'
  nceplcltable000001(197,1)='MCONV'
  nceplcltable000001(197,2)='Horizontal moisture divergence'
  nceplcltable000001(197,3)='(kg s)/(m^2)'
  nceplcltable000001(198,1)=''
  nceplcltable000001(198,2)='Percent frozen precipitation'
  nceplcltable000001(198,3)='-'
  nceplcltable000001(199,1)='PEVAP'
  nceplcltable000001(199,2)='Potential evaporation'
  nceplcltable000001(199,3)='kg/(m^2)'
  nceplcltable000001(200,1)='PEVPR'
  nceplcltable000001(200,2)='Potential evaporation rate'
  nceplcltable000001(200,3)='W/(m^2)'
  nceplcltable000001(201,1)='SNOWC'
  nceplcltable000001(201,2)='Snow cover'
  nceplcltable000001(201,3)='%'
  nceplcltable000001(202,1)='FRAIN'
  nceplcltable000001(202,2)='Rain fraction of total liquid water'
  nceplcltable000001(202,3)=''
  nceplcltable000001(203,1)='FRIME'
  nceplcltable000001(203,2)='Rime factor'
  nceplcltable000001(203,3)='-'
  nceplcltable000001(204,1)='TCOLR'
  nceplcltable000001(204,2)='Total column integrated rain'
  nceplcltable000001(204,3)='kg/(m/m)'
  nceplcltable000001(205,1)='TCOLS'
  nceplcltable000001(205,2)='Total column integrated snow'
  nceplcltable000001(205,3)='kg/(m/m)'
  nceplcltable000001(206,1)='TIPD'
  nceplcltable000001(206,2)='Total icing potential diagnostic'
  nceplcltable000001(206,3)='non-dim'
  nceplcltable000001(207,1)='NCIP'
  nceplcltable000001(207,2)='Number concentration for ice particles'
  nceplcltable000001(207,3)='non-dim'
  nceplcltable000001(208,1)='SNOT'
  nceplcltable000001(208,2)='Snow temperature'
  nceplcltable000001(208,3)='K'
  nceplcltable000001(209,1)='TCLSW'
  nceplcltable000001(209,2)='Total column integrated supercooled liquid water'
  nceplcltable000001(209,3)='kg/(m^2)'
  nceplcltable000001(210,1)='TCOLM'
  nceplcltable000001(210,2)='Total column integrated melting ice'
  nceplcltable000001(210,3)='kg/(m^2)'
  nceplcltable000001(211,1)='EMNP'
  nceplcltable000001(211,2)='Evaporation'
  nceplcltable000001(211,3)='cm/day'
  nceplcltable000001(212,1)='SBSNO'
  nceplcltable000001(212,2)='Sublimation'
  nceplcltable000001(212,3)='W/(m^2)'
  nceplcltable000001(213,1)='CNVMR'
  nceplcltable000001(213,2)='Deep convective moistening rate'
  nceplcltable000001(213,3)='kg/(kg s)'
  nceplcltable000001(214,1)='SHAMR'
  nceplcltable000001(214,2)='Shallow convective moistening rate'
  nceplcltable000001(214,3)='kg/(kg s)'
  nceplcltable000001(215,1)='VDFMR'
  nceplcltable000001(215,2)='Vertical diffusion moistening rate'
  nceplcltable000001(215,3)='kg/(kg s)'
  nceplcltable000001(216,1)='CONDP'
  nceplcltable000001(216,2)='Condensation pressure of parcali lifted'
  nceplcltable000001(216,3)='Pa'
  nceplcltable000001(217,1)='LRGMR'
  nceplcltable000001(217,2)='Large scale moistening rate'
  nceplcltable000001(217,3)='kg/(kg s)'
  nceplcltable000001(218,1)='QZ0'
  nceplcltable000001(218,2)='Specific humidity at top of viscous sublayer'
  nceplcltable000001(218,3)='kg/kg'
  nceplcltable000001(219,1)='QMAX'
  nceplcltable000001(219,2)='Maximum specific humidity at 2m'
  nceplcltable000001(219,3)='kg/kg'
  nceplcltable000001(220,1)='QMIN'
  nceplcltable000001(220,2)='Minimum specific humdity at 2m'
  nceplcltable000001(220,3)='kg/kg'
  nceplcltable000001(221,1)='ARAIN'
  nceplcltable000001(221,2)='Liquid precipitation'
  nceplcltable000001(221,3)='kg/(m^2)'
  nceplcltable000001(222,1)='SNOWT'
  nceplcltable000001(222,2)='Snow temperature'
  nceplcltable000001(222,3)='K'
  nceplcltable000001(223,1)='APCPN'
  nceplcltable000001(223,2)='Total precipitation'
  nceplcltable000001(223,3)='kg/(m^2)'
  nceplcltable000001(224,1)='ACPCPN'
  nceplcltable000001(224,2)='Convective precipitation'
  nceplcltable000001(224,3)='kg/(m^2)'
  nceplcltable000001(225,1)='FRZR'
  nceplcltable000001(225,2)='Freezing rain'
  nceplcltable000001(225,3)='kg/(m^2)'
  nceplcltable000001(226,1)='PWTHER'
  nceplcltable000001(226,2)='Pblackominant weather'
  nceplcltable000001(226,3)='Numeric'
  nceplcltable000001(227,1)='FROZR'
  nceplcltable000001(227,2)='Frozen rain'
  nceplcltable000001(227,3)='kg/(m^2)'
  nceplcltable000001(241,1)='TSNOW'
  nceplcltable000001(241,2)='Total snow'
  nceplcltable000001(241,3)='kg/(m^2)'
  nceplcltable000001(242,1)='RHPW'
  nceplcltable000001(242,2)='Relative humidity with respect to precipitable water'
  nceplcltable000001(242,3)='%'
  
  nceplcltable000002(192,1)='VWSH'
  nceplcltable000002(192,2)='Vertical speed shear'
  nceplcltable000002(192,3)='1/s'
  nceplcltable000002(193,1)='MFLX'
  nceplcltable000002(193,2)='Horizontal momentum flux'
  nceplcltable000002(193,3)='N/(m^2)'
  nceplcltable000002(194,1)='USTM'
  nceplcltable000002(194,2)='u-component storm motion'
  nceplcltable000002(194,3)='m/s'
  nceplcltable000002(195,1)='VSTM'
  nceplcltable000002(195,2)='v-component storm motion'
  nceplcltable000002(195,3)='m/s'
  nceplcltable000002(196,1)='CD'
  nceplcltable000002(196,2)='Drag coefficient'
  nceplcltable000002(196,3)='-'
  nceplcltable000002(197,1)='FRICV'
  nceplcltable000002(197,2)='Frictional velocity'
  nceplcltable000002(197,3)='m/s'
  nceplcltable000002(198,1) = 'LAUV'
  nceplcltable000002(198,2) = 'Latitude of U wind component'
  nceplcltable000002(198,3) = 'deg'  
  nceplcltable000002(199,1) = 'LOUV'
  nceplcltable000002(199,2) = 'Longitude of U wind component'
  nceplcltable000002(199,3) = 'deg'  
  nceplcltable000002(200,1) = 'LAVV'
  nceplcltable000002(200,2) = 'Latitude of V wind component'
  nceplcltable000002(200,3) = 'deg'  
  nceplcltable000002(201,1) = 'LOVV'
  nceplcltable000002(201,2) = 'Longitude of V wind component'
  nceplcltable000002(201,3) = 'deg'  
  nceplcltable000002(202,1) = 'LAPP'
  nceplcltable000002(202,2) = 'Latitude of pressure point'
  nceplcltable000002(202,3) = 'deg'  
  nceplcltable000002(203,1) = 'LOPP'
  nceplcltable000002(203,2) = 'Longitude of pressure point'
  nceplcltable000002(203,3) = 'deg'  
  nceplcltable000002(204,1) = 'VEDH'
  nceplcltable000002(204,2) = 'Vertical eddy diffusivity heat exchange'
  nceplcltable000002(204,3) = '(m^2)/s'  
  nceplcltable000002(205,1) = 'CONVMZ'
  nceplcltable000002(205,2) = 'Covariance between meridional and zonal wind'
  nceplcltable000002(205,3) = '(m^2)/(s^2)'  
  nceplcltable000002(206,1) = 'CONVTZ'
  nceplcltable000002(206,2) = 'Covariance between temperature and zonal wind'
  nceplcltable000002(206,3) = '(K m)/s'  
  nceplcltable000002(207,1) = 'COVTM'
  nceplcltable000002(207,2) = 'Covariance between temperature and meridional wind'
  nceplcltable000002(207,3) = '(K m)/s'  
  nceplcltable000002(208,1) = 'VDFUA'
  nceplcltable000002(208,2) = 'Vertical diffusion zonal acceleration'
  nceplcltable000002(208,3) = 'm/(s^2)'  
  nceplcltable000002(209,1) = 'VDFVA'
  nceplcltable000002(209,2) = 'Vertical diffusion meridional acceleration'
  nceplcltable000002(209,3) = 'm/(s^2)'  
  nceplcltable000002(210,1) = 'GWDU'
  nceplcltable000002(210,2) = 'Gravity wave drag zonal acceleration'
  nceplcltable000002(210,3) = 'm/(s^2)'  
  nceplcltable000002(211,1) = 'GWDV'
  nceplcltable000002(211,2) = 'Gravity wave drag meridional acceleration'
  nceplcltable000002(211,3) = 'm/(s^2)'  
  nceplcltable000002(212,1) = 'CNVU'
  nceplcltable000002(212,2) = 'Convective zonal momentum mixing acceleration'
  nceplcltable000002(212,3) = 'm/(s^2)'  
  nceplcltable000002(213,1) = 'CNVV'
  nceplcltable000002(213,2) = 'Convective meridional momentum mixing acceleration'
  nceplcltable000002(213,3) = 'm/(s^2)'  
  nceplcltable000002(214,1) = 'WTEND'
  nceplcltable000002(214,2) = 'Tendency of vertical velocity'
  nceplcltable000002(214,3) = 'm/(s^2)'  
  nceplcltable000002(215,1) = 'OMGALF'
  nceplcltable000002(215,2) = 'Omega divide by density'
  nceplcltable000002(215,3) = 'K'  
  nceplcltable000002(216,1) = 'CNGWDU'
  nceplcltable000002(216,2) = 'Convective gravity wave drag zonal acceleration'
  nceplcltable000002(216,3) = 'm/(s^2)'  
  nceplcltable000002(217,1) = 'CNGWDV'
  nceplcltable000002(217,2) = 'Convective gravity wave drag meridional acceleration'
  nceplcltable000002(217,3) = 'm/(s^2)'  
  nceplcltable000002(218,1) = 'LMV'
  nceplcltable000002(218,2) = 'Velocity point model surface'
  nceplcltable000002(218,3) = ''  
  nceplcltable000002(219,1) = 'PVMWW'
  nceplcltable000002(219,2) = 'Potential vorticity'
  nceplcltable000002(219,3) = '1/(s m)'  
  nceplcltable000002(220,1) = 'MAXUVV'
  nceplcltable000002(220,2) = 'Hourly maximum upward vertical velocity'
  nceplcltable000002(220,3) = 'm/s'  
  nceplcltable000002(221,1) = 'MAXDVV'
  nceplcltable000002(221,2) = 'Hourly maximum downward vertical velocity'
  nceplcltable000002(221,3) = 'm/s'  
  nceplcltable000002(222,1) = 'MAXUW'
  nceplcltable000002(222,2) = 'U-component of hourly maximum 10m wind speed'
  nceplcltable000002(222,3) = 'm/s'  
  nceplcltable000002(223,1) = 'MAXVW'
  nceplcltable000002(223,2) = 'V-component of hourly maximum 10m wind speed'
  nceplcltable000002(223,3) = 'm.s'  
  nceplcltable000002(224,1) = 'VRATE'
  nceplcltable000002(224,2) = 'Ventilation rate'
  nceplcltable000002(224,3) = '(m^2)/s'  

  nceplcltable000003(192,1) = 'MSLET'
  nceplcltable000003(192,2) = 'Mean Sea Level Pressure (Eta reduction)'
  nceplcltable000003(192,3) = 'Pa'
  nceplcltable000003(193,1) = '5WAVH'
  nceplcltable000003(193,2) = '5-wave geopotential height'
  nceplcltable000003(193,3) = 'gpm'
  nceplcltable000003(194,1) = 'U-GWD'
  nceplcltable000003(194,2) = 'Zonal flux of gravity wave stress'
  nceplcltable000003(194,3) = 'N/(m^2)'
  nceplcltable000003(195,1) = 'V-GWD'
  nceplcltable000003(195,2) = 'Meridional flux of gravity wave stress'
  nceplcltable000003(195,3) = 'N/(m^2)'
  nceplcltable000003(196,1) = 'HPBL'
  nceplcltable000003(196,2) = 'Planetary boundary layer height'
  nceplcltable000003(196,3) = 'm'
  nceplcltable000003(197,1) = '5WAVA'
  nceplcltable000003(197,2) = '5-wave geopotential height anomaly'
  nceplcltable000003(197,3) = 'gpm'
  nceplcltable000003(198,1) = 'MSLMA'
  nceplcltable000003(198,2) = 'MSLP'
  nceplcltable000003(198,3) = 'Pa'
  nceplcltable000003(199,1) = 'TSLSA'
  nceplcltable000003(199,2) = '3-hr pressure tendency'
  nceplcltable000003(199,3) = 'Pa/s'
  nceplcltable000003(200,1) = 'PLPL'
  nceplcltable000003(200,2) = 'Pressure level from which parcel was lifted'
  nceplcltable000003(200,3) = 'Pa'
  nceplcltable000003(201,1) = 'LPSX'
  nceplcltable000003(201,2) = 'X-gradient of log pressure'
  nceplcltable000003(201,3) = '1/m'
  nceplcltable000003(202,1) = 'LPSY'
  nceplcltable000003(202,2) = 'Y-gradient of log pressure'
  nceplcltable000003(202,3) = '1/m'
  nceplcltable000003(203,1) = 'HGTX'
  nceplcltable000003(203,2) = 'X-gradient of height'
  nceplcltable000003(203,3) = '1/m'
  nceplcltable000003(204,1) = 'HGTY'
  nceplcltable000003(204,2) = 'Y-gradient of height'
  nceplcltable000003(204,3) = '1/m'
  nceplcltable000003(205,1) = 'LAYTH'
  nceplcltable000003(205,2) = 'Layer thickness'
  nceplcltable000003(205,3) = 'm'
  nceplcltable000003(206,1) = 'NLGSP'
  nceplcltable000003(206,2) = 'Natural log of surface pressure'
  nceplcltable000003(206,3) = 'ln(kPa)'
  nceplcltable000003(207,1) = 'CNVUMF'
  nceplcltable000003(207,2) = 'Convective updraft mass flux'
  nceplcltable000003(207,3) = 'kg/(m^2 s)'
  nceplcltable000003(208,1) = 'CNVDMF'
  nceplcltable000003(208,2) = 'Convective downdraft mass flux'
  nceplcltable000003(208,3) = 'kg/(m^2 s)'
  nceplcltable000003(209,1) = 'CNVDEMF'
  nceplcltable000003(209,2) = 'Convective detrainment mass flux'
  nceplcltable000003(209,3) = 'kg/(m^2 s)'
  nceplcltable000003(210,1) = 'LMH'
  nceplcltable000003(210,2) = 'Mass point model surface'
  nceplcltable000003(210,3) = ''
  nceplcltable000003(211,1) = 'HGTN'
  nceplcltable000003(211,2) = 'Geopotential height'
  nceplcltable000003(211,3) = 'gpm'
  nceplcltable000003(212,1) = 'PRESN'
  nceplcltable000003(212,2) = 'Pressure'
  nceplcltable000003(212,3) = 'Pa'
  nceplcltable000003(213,1) = 'ORCONV'
  nceplcltable000003(213,2) = 'Orography convexity'
  nceplcltable000003(213,3) = ''
  nceplcltable000003(214,1) = 'ORASW'
  nceplcltable000003(214,2) = 'Orographic asymmetry W component'
  nceplcltable000003(214,3) = ''
  nceplcltable000003(215,1) = 'ORASS'
  nceplcltable000003(215,2) = 'Orographic asymmetry S component'
  nceplcltable000003(215,3) = ''
  nceplcltable000003(216,1) = 'ORASSW'
  nceplcltable000003(216,2) = 'Orographic asymmetry SW component'
  nceplcltable000003(216,3) = ''
  nceplcltable000003(217,1) = 'ORASNW'
  nceplcltable000003(217,2) = 'Orographic asymmetry NW component'
  nceplcltable000003(217,3) = ''
  nceplcltable000003(218,1) = 'ORLSW'
  nceplcltable000003(218,2) = 'Orographic length scale W component'
  nceplcltable000003(218,3) = ''
  nceplcltable000003(219,1) = 'ORLSS'
  nceplcltable000003(219,2) = 'Orographic length scale S component'
  nceplcltable000003(219,3) = ''
  nceplcltable000003(220,1) = 'ORLSSW'
  nceplcltable000003(220,2) = 'Orographic length scale SW component'
  nceplcltable000003(220,3) = ''
  nceplcltable000003(221,1) = 'ORLSNW'
  nceplcltable000003(221,2) = 'Orographic length scale NW component'
  nceplcltable000003(221,3) = ''
  
  nceplcltable000004(192,1)='DSWRF'
  nceplcltable000004(192,2)='Downward short-wave rad. flux'
  nceplcltable000004(192,3)='W/(m^2)'
  nceplcltable000004(193,1)='USWRF'
  nceplcltable000004(193,2)='Upward short-wave rad. flux'
  nceplcltable000004(193,3)='W/(m^2)'

  nceplcltable000005(192,1)='DLWRF'
  nceplcltable000005(192,2)='Downward long-wave rad. flux'
  nceplcltable000005(192,3)='W/(m^2)'
  nceplcltable000005(193,1)='ULWRF'
  nceplcltable000005(193,2)='Upward long-wave rad. flux'
  nceplcltable000005(193,3)='W/(m^2)'

  nceplcltable000006(192,1)='CDLYR'
  nceplcltable000006(192,2)='Non-convective cloud cover'
  nceplcltable000006(192,3)='%'
  nceplcltable000006(193,1)='CWORK'
  nceplcltable000006(193,2)='Cloud work function'
  nceplcltable000006(193,3)='J/kg'
  nceplcltable000006(194,1)='CUEFI'
  nceplcltable000006(194,2)='Convective cloud efficiency'
  nceplcltable000006(194,3)='-'
  nceplcltable000006(195,1)='TCOND'
  nceplcltable000006(195,2)='Total condenstate'
  nceplcltable000006(195,3)='kg/kg'
  nceplcltable000006(196,1)='TCOLW'
  nceplcltable000006(196,2)='Total column-integrated cloud water'
  nceplcltable000006(196,3)='kg/(m/m)'
  nceplcltable000006(197,1)='TCOLI'
  nceplcltable000006(197,2)='Total column-integrated cloud ice'
  nceplcltable000006(197,3)='kg/(m/m)'
  nceplcltable000006(198,1)='TCOLC'
  nceplcltable000006(198,2)='Total column-integrated cloud condensate'
  nceplcltable000006(198,3)='kg/(m/m)'
  nceplcltable000006(199,1)='FICE'
  nceplcltable000006(199,2)='Ice fraction of total condensate'
  nceplcltable000006(199,3)='kg/(m/m)'

  nceplcltable000007(192,1)='LFTX'
  nceplcltable000007(192,2)='Surface lifted index'
  nceplcltable000007(192,3)='K'
  nceplcltable000007(193,1)='4LFTX'
  nceplcltable000007(193,2)='Best (4 layer) lifted index'
  nceplcltable000007(193,3)='K'
  nceplcltable000007(194,1)='RI'
  nceplcltable000007(194,2)='Richardson number'
  nceplcltable000007(194,3)='-'

  nceplcltable000014(192,1)='03MR'
  nceplcltable000014(192,2)='Ozone mixing ratio'
  nceplcltable000014(192,3)='kg/kg'
  nceplcltable000014(193,1)='OZCON'
  nceplcltable000014(193,2)='Ozone concentration'
  nceplcltable000014(193,3)='PPB'
  nceplcltable000014(194,1)='OZCAT'
  nceplcltable000014(194,2)='Categorical ozone concentration'
  nceplcltable000014(194,3)='unknown'

  nceplcltable000019(192,1)='MXSALB'
  nceplcltable000019(192,2)='Maxmium snow albedo'
  nceplcltable000019(192,3)='%'
  nceplcltable000019(193,1)='SNFALB'
  nceplcltable000019(193,2)='Snow-free albedo'
  nceplcltable000019(193,3)='%'

  nceplcltable000191(192,1)='NLAT'
  nceplcltable000191(192,2)='Latitude (-90 to 90)'
  nceplcltable000191(192,3)='deg'
  nceplcltable000191(193,1)='ELON'
  nceplcltable000191(193,2)='East logitude (0 to 360)'
  nceplcltable000191(193,3)='deg'
  nceplcltable000191(194,1)='secPriorInitRef'
  nceplcltable000191(194,2)='Seconds prior to initial reference time'
  nceplcltable000191(194,3)='sec'

  nceplcltable001000(192,1)='BGRUN'
  nceplcltable001000(192,2)='Baseflow-groundwater runoff'
  nceplcltable001000(192,3)='kg/(m^2)'
  nceplcltable001000(193,1)='SSRUN'
  nceplcltable001000(193,2)='Storm surface runoff'
  nceplcltable001000(193,3)='kg/(m^2)'

  nceplcltable002000(192,1)='SOILW'
  nceplcltable002000(192,2)='Volumetric soil moisture content'
  nceplcltable002000(192,3)='fraction'
  nceplcltable002000(193,1)='GFLUX'
  nceplcltable002000(193,2)='Ground heat flux'
  nceplcltable002000(193,3)='W/(m^2)'
  nceplcltable002000(194,1)='MSTAV'
  nceplcltable002000(194,2)='Moisture avaliability'
  nceplcltable002000(194,3)='%'
  nceplcltable002000(195,1)='SFEXC'
  nceplcltable002000(195,2)='Exchange coefficient'
  nceplcltable002000(195,3)='(kg/(m^3))(m/s)'
  nceplcltable002000(196,1)='CNWAT'
  nceplcltable002000(196,2)='Plant canopy surface water'
  nceplcltable002000(196,3)='kg/(m^2)'
  nceplcltable002000(197,1)='BMIXL'
  nceplcltable002000(197,2)='Blackadars mixing length scale'
  nceplcltable002000(197,3)='m'
  nceplcltable002000(198,1)='VGTYP'
  nceplcltable002000(198,2)='Vegetation type'
  nceplcltable002000(198,3)='0..13'
  nceplcltable002000(199,1)='CCOND'
  nceplcltable002000(199,2)='Canopy conductance'
  nceplcltable002000(199,3)='m/s'
  nceplcltable002000(200,1)='RSMIN'
  nceplcltable002000(200,2)='Minimal stomatal resistance'
  nceplcltable002000(200,3)='s/m'
  nceplcltable002000(201,1)='WILT'
  nceplcltable002000(201,2)='Wilting point'
  nceplcltable002000(201,3)='fraction'
  nceplcltable002000(202,1)='RCS'
  nceplcltable002000(202,2)='Solar parameter in canopy conductance'
  nceplcltable002000(202,3)='fraction'
  nceplcltable002000(203,1)='RCT'
  nceplcltable002000(203,2)='Temperature parameter in canopy conductance'
  nceplcltable002000(203,3)='fraction'
  nceplcltable002000(204,1)='RCQ'
  nceplcltable002000(204,2)='Humidity parameter in canopy conductance'
  nceplcltable002000(204,3)='fraction'
  nceplcltable002000(205,1)='RCSOIL'
  nceplcltable002000(205,2)='Soil moisture parameter in canopy conductance'
  nceplcltable002000(205,3)='fraction'

  nceplcltable002003(192,1)='SOILL'
  nceplcltable002003(192,2)='Liquid volumetic soil moisture'
  nceplcltable002003(192,3)='fraction'
  nceplcltable002003(193,1)='RLYRS'
  nceplcltable002003(193,2)='Number of soil layers in root zone'
  nceplcltable002003(193,3)='-'
  nceplcltable002003(194,1)='SLTYP'
  nceplcltable002003(194,2)='Surface slope type'
  nceplcltable002003(194,3)='Index'
  nceplcltable002003(195,1)='SMREF'
  nceplcltable002003(195,2)='Transpiration stress-onset (soil moisture)'
  nceplcltable002003(195,3)='fraction'
  nceplcltable002003(196,1)='SMDRY'
  nceplcltable002003(196,2)='Direct evaporation cease (soil moisture)'
  nceplcltable002003(196,3)='fraction'
  nceplcltable002003(197,1)='POROS'
  nceplcltable002003(197,2)='Soil porosity'
  nceplcltable002003(197,3)='fraction'

  nceplcltable003001(192,1)='ScatEstUWind'
  nceplcltable003001(192,2)='Scatterometer estimated u wind'
  nceplcltable003001(192,3)='unknown'
  nceplcltable003001(193,1)='ScatEstVWind'
  nceplcltable003001(193,2)='Scatterometer estimated v wind'
  nceplcltable003001(193,3)='unknown'

  first=.false.
end if

! Check standard meta data
Select Case(alist(3))
  Case(0)
    Select Case(alist(5))
      Case(0)
        if (alist(6)>=0.and.alist(6)<=21) then
          elemtxt=meteotemp(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(1)
        if (alist(6)>=0.and.alist(6)<=107) then
          elemtxt=meteomoist(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(2)
        if (alist(6)>=0.and.alist(6)<=38) then
          elemtxt=meteomoment(alist(6),:)
        else
          elemtxt=elemerr
        end if          
      Case(3)
        if (alist(6)>=0.and.alist(6)<=26) then
          elemtxt=meteomass(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(4)
        if (alist(6)>=0.and.alist(6)<=6) then
          elemtxt=meteoshortradiate(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(5)
        if (alist(6)>=0.and.alist(6)<=2) then
          elemtxt=meteolongradiate(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(6)
        if (alist(6)>=0.and.alist(6)<=201) then
          elemtxt=meteocloud(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(7)
        if (alist(6)>=0.and.alist(6)<=9) then          
          elemtxt=meteostability(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(13)
        if (alist(6)>=0.and.alist(6)<=0) then
          elemtxt=meteoaerosols(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(14)
        if (alist(6)>=0.and.alist(6)<=0) then
          elemtxt=meteogases(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(15)
        if (alist(6)>=0.and.alist(6)<=6) then
          elemtxt=meteoradar(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(18)
        if (alist(6)>=0.and.alist(6)<=8) then
          elemtxt=meteonuclear(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(19)
        if (alist(6)>=0.and.alist(6)<=16) then
          elemtxt=meteoatmos(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(190)
        if (alist(6)>=0.and.alist(6)<=0) then
          elemtxt=meteotext(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(253)
        if (alist(6)>=0.and.alist(6)<=0) then
          elemtxt=meteotext(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case DEFAULT
        elemtxt=elemerr
    End Select
  Case(1)
    Select Case(alist(5))
      Case(0)
        if (alist(6)>=0.and.alist(6)<=4) then
          elemtxt=hydrobasic(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(1)
        if (alist(6)>=0.and.alist(6)<=2) then
          elemtxt=hydroprob(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case DEFAULT
        elemtxt=elemerr
    End Select
  Case(2)
    Select Case(alist(5))
      Case(0)
        if (alist(6)>=0.and.alist(6)<=8) then
          elemtxt=landveg(alist(6),:)
        else
          elemtxt=elemerr
        end if        
      Case(3)
        if (alist(6)>=0.and.alist(6)<=203) then
          elemtxt=landsoil(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(4)
        if (alist(6)>=0.and.alist(6)<=4) then
          elemtxt=landfire(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case DEFAULT
        elemtxt=elemerr
    End Select
  Case(3)
    Select Case(alist(5))
      Case(0)
        if (alist(6)>=0.and.alist(6)<=7) then
          elemtxt=spaceimage(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(1)
        if (alist(6)>=0.and.alist(6)<=0) then
          elemtxt=spacequantitative(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case DEFAULT
        elemtxt=elemerr
    End Select
  Case(10)
    Select Case(alist(5))
      Case(0)
        if (alist(6)>=0.and.alist(6)<=13) then
          elemtxt=oceanwaves(alist(6),:)
        else
          elemtxt=elemerr
        end if        
      Case(1)
        if (alist(6)>=0.and.alist(6)<=3) then
          elemtxt=oceancurrents(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(2)
        if (alist(6)>=0.and.alist(6)<=7) then
          elemtxt=oceanice(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(3)
        if (alist(6)>=0.and.alist(6)<=1) then
          elemtxt=oceansurface(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case(4)
        if (alist(6)>=0.and.alist(6)<=3) then
          elemtxt=oceansubsurface(alist(6),:)
        else
          elemtxt=elemerr
        end if
      Case DEFAULT
        elemtxt=elemerr
    End Select
  Case DEFAULT
    elemtxt=elemerr
End Select

! Check local meta data
If (elemtxt(1)=='???') Then
  Select Case(alist(3))
    Case(0)
      Select Case(alist(5))
        Case(0)
          if (alist(6)>=192.and.alist(6)<=204) then
            elemtxt=nceplcltable000000(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(1)
          if (alist(6)>=192.and.alist(6)<=242) then
            elemtxt=nceplcltable000001(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(2)
          if (alist(6)>=192.and.alist(6)<=224) then
            elemtxt=nceplcltable000002(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(3)
          if (alist(6)>=192.and.alist(6)<=221) then
            elemtxt=nceplcltable000003(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(4)
          if (alist(6)>=192.and.alist(6)<=193) then
            elemtxt=nceplcltable000004(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(5)
          if (alist(6)>=192.and.alist(6)<=193) then
            elemtxt=nceplcltable000005(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(6)
          if (alist(6)>=192.and.alist(6)<=199) then
            elemtxt=nceplcltable000006(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(7)
          if (alist(6)>=192.and.alist(6)<=194) then
            elemtxt=nceplcltable000007(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(14)
          if (alist(6)>=192.and.alist(6)<=194) then
            elemtxt=nceplcltable000014(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(19)
          if (alist(6)>=192.and.alist(6)<=193) then
            elemtxt=nceplcltable000019(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(191)
          if (alist(6)>=192.and.alist(6)<=194) then
            elemtxt=nceplcltable000191(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case DEFAULT
          elemtxt=elemerr
      End Select
    Case(1)
      Select Case(alist(5))
        Case(0)
          if (alist(6)>=192.and.alist(6)<=193) then
            elemtxt=nceplcltable001000(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case DEFAULT
          elemtxt=elemerr
      End Select
    Case(2)
      Select Case(alist(5))
        Case(0)
          if (alist(6)>=192.and.alist(6)<=205) then
            elemtxt=nceplcltable002000(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case(3)
          if (alist(6)>=192.and.alist(6)<=197) then
            elemtxt=nceplcltable002003(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case DEFAULT
          elemtxt=elemerr
      End Select
    Case(3)
      Select Case(alist(5))
        Case(1)
          if (alist(6)>=192.and.alist(6)<=193) then
            elemtxt=nceplcltable003001(alist(6),:)
          else
            elemtxt=elemerr
          end if
        Case DEFAULT
          elemtxt=elemerr
      End Select
    Case DEFAULT
      elemtxt=elemerr
  End Select
End If

! Check for errors
If ((elemtxt(1)=='???').OR.(elemtxt(1)=='')) Then
  elemtxt(1)='      '
  Write(elemtxt(1)(1:2),'(Z2.2)') alist(3)
  Write(elemtxt(1)(3:4),'(Z2.2)') alist(5)
  Write(elemtxt(1)(5:6),'(Z2.2)') alist(6)
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine gets the level description for the specified element
!

Subroutine getelemlvl(alist,indx,surfvalue,sndvalue,surtxt)

Implicit None

Integer, dimension(0:49), intent(inout) :: alist
Integer, intent(out) :: indx
Double Precision, intent(out) :: surfvalue,sndvalue
Character*80, dimension(1:3), intent(out) :: surtxt

Select Case(alist(48))
  Case(1)
    Call getelemlvl1(alist,indx,surfvalue,sndvalue,surtxt)
  Case DEFAULT !GRIB2
    Call getelemlvl2(alist,indx,surfvalue,sndvalue,surtxt)
End Select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine gets the GRIB1 level description for the specified
! element
!

Subroutine getelemlvl1(alist,indx,surfvalue,sndvalue,surtxt)

Implicit None

Integer, dimension(0:49), intent(inout) :: alist
Integer, intent(out) :: indx
Double Precision, intent(out) :: surfvalue,sndvalue
Double Precision ozero,o11,o12
Character*80, dimension(1:3), intent(out) :: surtxt

indx=alist(8)
o11=real(int(alist(9)/256))
o12=real(mod(alist(9),256))
ozero=real(alist(9))

Select Case(alist(8))
  Case(1)
    surtxt(1)="SFC"
    surtxt(2)="sfc"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(2)
    surtxt(1)="CBL"
    surtxt(2)="cld base"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(3) 
    surtxt(1)="CTL"
    surtxt(2)="cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(4)
    surtxt(1)="0DEG"
    surtxt(2)="0C isotherm"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(5)
    surtxt(1)="ADCL"
    surtxt(2)="cond lev"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(6)
    surtxt(1)="MWSL"
    surtxt(2)="max wind lev"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(7)
    surtxt(1)="TRO"
    surtxt(2)="tropopause"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(8)
    surtxt(1)="NTAT"
    surtxt(2)="nom. top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(9)
    surtxt(1)="SEAB"
    surtxt(2)="sea bottom"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(10,200)
    surtxt(1)="reserved"
    surtxt(2)="atmos col"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(12,212)
    surtxt(1)="reserved"
    surtxt(2)="low cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(13,213)
    surtxt(1)="reserved"
    surtxt(2)="low cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(14,214)
    surtxt(1)="reserved"
    surtxt(2)="low cld lay"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(22,222)
    surtxt(1)="reserved"
    surtxt(2)="mid cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(23,223)
    surtxt(1)="reserved"
    surtxt(2)="mid cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(24,224)
    surtxt(1)="reserved"
    surtxt(2)="mid cld lay"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(32,232)
    surtxt(1)="reserved"
    surtxt(2)="high cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(33,233)
    surtxt(1)="reserved"
    surtxt(2)="high cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(34,234)
    surtxt(1)="reserved"
    surtxt(2)="high cld lay"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(201)
    surtxt(1)="reserved"
    surtxt(2)="ocean column"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(204)
    surtxt(1)="reserved"
    surtxt(2)="high trop freezing lvl"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(206)
    surtxt(1)="reserved"
    surtxt(2)="grid-scale cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(207)
    surtxt(1)="reserved"
    surtxt(2)="grid-scale cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(209)
    surtxt(1)="reserved"
    surtxt(2)="bndary-layer cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(210)
    surtxt(1)="reserved"
    surtxt(2)="bndary-layer cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(211)
    surtxt(1)="reserved"
    surtxt(2)="bndary-layer cld layer"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(242)
    surtxt(1)="reserved"
    surtxt(2)="convect-cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(243)
    surtxt(1)="reserved"
    surtxt(2)="convect-cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(244)
    surtxt(1)="reserved"
    surtxt(2)="convect-cld layer"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(246)
    surtxt(1)="reserved"
    surtxt(2)="max e-pot-temp lvl"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(247)
    surtxt(1)="reserved"
    surtxt(2)="equilibrium lvl"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(248)
    surtxt(1)="reserved"
    surtxt(2)="shallow convect-cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(249)
    surtxt(1)="reserved"
    surtxt(2)="shallow convect-cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(251)
    surtxt(1)="reserved"
    surtxt(2)="deep convect-cld bot"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(252)
    surtxt(1)="reserved"
    surtxt(2)="deep convect-cld top"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(100)
    surtxt(1)="ISBL"
    surtxt(2)="Pa"
    surtxt(3)="Pa"
    alist(13)=0
    surfvalue=ozero*100.
    sndvalue=0.
  Case(101)
    surtxt(1)="ISBL"
    surtxt(2)="Pa"
    surtxt(3)="Pa"
    alist(13)=1
    surfvalue=o11*1000.
    sndvalue=o12*1000.
  Case(102)
    surtxt(1)="MSL"
    surtxt(2)="MSL"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(103)
    surtxt(1)="GPML"
    surtxt(2)="m above MSL"
    surtxt(3)="m"
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(104)
    surtxt(1)="GPML"
    surtxt(2)="m above MSL"
    surtxt(3)="m"
    alist(13)=1
    surfvalue=o11*100.
    sndvalue=o12*100.
  Case(105)
    surtxt(1)="HTGL"
    surtxt(2)="m above gnd"
    surtxt(3)="m"
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(106)
    surtxt(1)="HTGL"
    surtxt(2)="m above gnd"
    surtxt(3)="m"
    alist(13)=1
    surfvalue=o11*100.
    sndvalue=o12*100.
  Case(107)
    surtxt(1)="SIGL"
    surtxt(2)="sigma"
    surtxt(3)="sigma value"
    alist(13)=0
    surfvalue=ozero/10000.
    sndvalue=0.
  Case(108)
    surtxt(1)="SIGL"
    surtxt(2)="sigma"
    surtxt(3)="sigma value"
    alist(13)=1
    surfvalue=o11/100.
    sndvalue=o12/100.
  Case(109)
    surtxt(1)="HYBL"
    surtxt(2)="hybrid lev"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(110)
    surtxt(1)="HYBL"
    surtxt(2)="hybrid lev"
    surtxt(3)="-"
    alist(13)=1
    surfvalue=o11
    sndvalue=o12
  Case(111)
    surtxt(1)="DBLL"
    surtxt(2)="m down"
    surtxt(3)="m"
    alist(13)=0
    surfvalue=ozero*0.01
    sndvalue=0.
  Case(112)
    surtxt(1)="DBLL"
    surtxt(2)="m down"
    surtxt(3)="m"
    alist(13)=1
    surfvalue=o11*0.01
    sndvalue=o12*0.01
  Case(113)
    surtxt(1)="THEL"
    surtxt(2)="K"
    surtxt(3)="K"
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(114)
    surtxt(1)="THEL"
    surtxt(2)="K"
    surtxt(3)="K"
    alist(13)=1
    surfvalue=475.-o11
    sndvalue=475.-o12
  Case(115)
    surtxt(1)="SPDL"
    surtxt(2)="Pa above gnd"
    surtxt(3)="Pa"
    alist(13)=0
    surfvalue=ozero*100.
    sndvalue=0.
  Case(116)
    surtxt(1)="SPDL"
    surtxt(2)="Pa above gnd"
    surtxt(3)="Pa"
    alist(13)=1
    surfvalue=o11*100.
    sndvalue=o12*100.
  Case(117)
    surtxt(1)="PVL"
    surtxt(2)="pv units"
    surtxt(3)="(K M^2)/(kg s)"
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(119)
    surtxt(1)="EtaL"
    surtxt(2)="(ETA level)"
    surtxt(3)="-"
    alist(13)=0
    surfvalue=ozero/10000.
    sndvalue=0.
  Case(120)
    surtxt(1)="EtaL"
    surtxt(2)="(ETA levels)"
    surtxt(3)="-"
    alist(13)=1
    surfvalue=o11/100.
    sndvalue=o12/100.
  Case(121)
    surtxt(1)="ISBL"
    surtxt(2)="Pa"
    surtxt(3)="Pa"
    alist(13)=1
    surfvalue=(1100.-o11)*100.
    sndvalue=(1100.-o12)*100.
  Case(125)
    surtxt(1)="HTGL"
    surtxt(2)="m above gnd"
    surtxt(3)="m"
    alist(13)=0
    surfvalue=ozero*100.
    sndvalue=0.
  Case(126)
    surtxt(1)="ISBL"
    surtxt(2)="Pa"
    surtxt(3)="Pa"
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(128)
    surtxt(1)="SIGL"
    surtxt(2)="(sigma)"
    surtxt(3)="sigma value"
    alist(13)=1
    surfvalue=1.1-o11/1000.
    sndvalue=1.1-o12/1000.
  Case(141)
    surtxt(1)="ISBL"
    surtxt(2)="Pa"
    surtxt(3)="Pa"
    alist(13)=1
    surfvalue=o11*1000.
    sndvalue=(1100.-o12)*100.
  Case(160)
    surtxt(1)="DBSL"
    surtxt(2)="m below sea level"
    surtxt(3)="m"
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case DEFAULT
  ! undefined
    surtxt(:)="???"
End Select

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine gets the GRIB2 level description for the specified
! element
!

Subroutine getelemlvl2(alist,indx,surfvalue,sndvalue,surtxt)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Integer, intent(out) :: indx
Double Precision, intent(out) :: surfvalue,sndvalue
Character*80, dimension(1:3), intent(out) :: surtxt
Character*80, dimension(1:160,1:3), save :: surface
logical, save :: first = .true.

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

if (first) then

  surface='???'

  ! Define surface table

  surface(1,1) = 'SFC'
  surface(1,2) = 'Ground or water surface'
  surface(1,3) = '-'
  surface(2,1) = 'CBL'
  surface(2,2) = 'Cloud base level'
  surface(2,3) = '-'
  surface(3,1) = 'CTL'
  surface(3,2) = 'Level of cloud tops'
  surface(3,3) = '-'
  surface(4,1) = '0DEG'
  surface(4,2) = 'Level of 0 degreee C isotherm'
  surface(4,3) = '-'
  surface(5,1) = 'ADCL'
  surface(5,2) = 'Level of adiabatic condensation lifted from the surface'
  surface(5,3) = '-'
  surface(6,1) = 'MWSL'
  surface(6,2) = 'Maximum wind level'
  surface(6,3) = '-'
  surface(7,1) = 'TRO'
  surface(7,2) = 'Tropopause'
  surface(7,3) = '-'
  surface(8,1) = 'NTAT'
  surface(8,2) = 'Nominal top of atmosphere'
  surface(8,3) = '-'
  surface(9,1) = 'SEAB'
  surface(9,2) = 'Sea bottom'
  surface(9,3) = '-'
  surface(20,1) = 'TMPL'
  surface(20,2) = 'Isotherm level'
  surface(20,3) = 'K'
  surface(100,1) = 'ISBL'
  surface(100,2) = 'Isobaric surface'
  surface(100,3) = 'Pa'
  surface(101,1) = 'MSL'
  surface(101,2) = 'Mean sea level'
  surface(101,3) = '-'
  surface(102,1) = 'GPML'
  surface(102,2) = 'Specific altitude above mean sea level'
  surface(102,3) = 'm'
  surface(103,1) = 'HTGL'
  surface(103,2) = 'Specific height level above ground'
  surface(103,3) = 'm'
  surface(104,1) = 'SIGL'
  surface(104,2) = 'Sigma level'
  surface(104,3) = 'sigma value'
  surface(105,1) = 'HYBL'
  surface(105,2) = 'Hybrid level'
  surface(105,3) = '-'
  surface(106,1) = 'DBLL'
  surface(106,2) = 'Depth below land surface'
  surface(106,3) = 'm'
  surface(107,1) = 'THEL'
  surface(107,2) = 'Isentropic (theta) level'
  surface(107,3) = 'K'
  surface(108,1) = 'SPDL'
  surface(108,2) = 'Level at specified pressure difference from ground to level'
  surface(108,3) = 'Pa'
  surface(109,1) = 'PVL'
  surface(109,2) = 'Potential vorticity surface'
  surface(109,3) = '(K M^2)/(kg s)'
  surface(111,1) = 'EtaL'
  surface(111,2) = 'Eta* level'
  surface(111,3) = '-'
  surface(117,1) = 'unknown'
  surface(117,2) = 'Mixed layer depth'
  surface(117,3) = 'm'
  surface(160,1) = 'DBSL'
  surface(160,2) = 'Depth below sea level'
  surface(160,3) = 'm'

  first=.false.
end if

! Determine level data

surtxt='Reserved (index=191)'

If (alist(8)<1) Then
  ! Undefined
  surfvalue=0
  sndvalue=0
  indx=0
  Return
End If

if (alist(9)>=0.) then
  surfvalue=Real(alist(10))/Real(10**alist(9))
else
  surfvalue=real(alist(10))*real(10**abs(alist(9)))
end if

If (alist(8)>160) then
  indx=191
  Write(surtxt(1),'("Reserved (",I4,")")') alist(8)
else if (surface(alist(8),1)=='???') Then
  indx=191
  Write(surtxt(1),'("Reserved (",I4,")")') alist(8)   
Else
  indx=alist(8)
  surtxt(:)=surface(indx,:)
End If

! snd data    
If (alist(13)==1) Then
  if (alist(12)==-2147483646) then
    sndvalue=0.
  else
    sndvalue=Real(alist(12))/Real(10**alist(11))
  end if
Else
  sndvalue=0.
End If

! MSL and SFC special case
if (indx==1.or.indx==101) then
   surfvalue=0.
   sndvalue=0.
end if

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine formats the level data for text output
!

Subroutine getlvltxt(alist,lvltxt)

Implicit None

Integer, dimension(0:49), intent(inout) :: alist
Character*80, intent(out) :: lvltxt

Select Case(alist(48))
  Case(1)
    Call getlvltxt1(alist,lvltxt)
  Case DEFAULT !GRIB2
    Call getlvltxt2(alist,lvltxt)
End Select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine formats the GRIB1 level data for text output
!

Subroutine getlvltxt1(alist,lvltxt)

Implicit None

Integer, dimension(0:49), intent(inout) :: alist
Character*80, intent(out) :: lvltxt
Character*80, dimension(1:3) :: surtxt
Double Precision surfvalue,sndvalue
Integer indx

lvltxt=''

Call getelemlvl1(alist,indx,surfvalue,sndvalue,surtxt)

If (alist(13)==1) Then    
  ! snd data
  If (dble(Int(surfvalue))==surfvalue) Then
    Write(lvltxt,'(I7,"-",I3,"-",A4)') Int(surfvalue),Int(sndvalue),surtxt(1)(1:10)
  Else
    Write(lvltxt,'(F7.1,"-",F3.1,"-",A4)') surfvalue,sndvalue,surtxt(1)(1:10)
  End If
Else
  ! no snd data
  If (dble(Int(surfvalue))==surfvalue) Then
    Write(lvltxt,'(I7,"-",A4)') Int(surfvalue),surtxt(1)(1:10)
  Else
    Write(lvltxt,'(F7.1,"-",A4)') surfvalue,surtxt(1)(1:10)
  End If
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine formats the GRIB2 level data for text output
!

Subroutine getlvltxt2(alist,lvltxt)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Character*80, intent(out) :: lvltxt
Character*80, dimension(1:3) :: surtxt
Double Precision surfvalue,sndvalue
Integer indx

lvltxt=''

If (alist(8)<1) Then
  lvltxt='0 undefined'
  Return
End If

Call getelemlvl2(alist,indx,surfvalue,sndvalue,surtxt)

If (alist(13)==1) Then    
  ! snd data
  If (indx>=191) Then
    If (dble(Int(surfvalue))==surfvalue) Then
      Write(lvltxt,'(I7,"-",I3,"-reserved(",I3,")")') Int(surfvalue),Int(sndvalue),alist(8)
    Else
      Write(lvltxt,'(F7.1,"-",F3.1,"-reserved(",I3,")")') surfvalue,sndvalue,alist(8)
    End If
  Else
    If (dble(Int(surfvalue))==surfvalue) Then
      Write(lvltxt,'(I7,"-",I3,"-",A4)') Int(surfvalue),Int(sndvalue),surtxt(1)(1:10)
    Else
      Write(lvltxt,'(F7.1,"-",F3.1,"-",A4)') surfvalue,sndvalue,surtxt(1)(1:10)
    End If
  End If
Else
  ! no snd data
  If (indx>=191) Then
    If (dble(Int(surfvalue))==surfvalue) Then
      Write(lvltxt,'(I7,"-reserved(",I3,")")') Int(surfvalue),alist(8)
    Else
      Write(lvltxt,'(F7.1,"-reserved(",I3,")")') surfvalue,alist(8)
    End If
  Else
    If (dble(Int(surfvalue))==surfvalue) Then
      Write(lvltxt,'(I7,"-",A4)') Int(surfvalue),surtxt(1)(1:10)
    Else
      Write(lvltxt,'(F7.1,"-",A4)') surfvalue,surtxt(1)(1:10)
    End If
  End If
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines the lon and lat of a grid
!

Subroutine getelemlonlat(alist,alonlat,gridtype)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Real, dimension(1:2,1:3), intent(out) :: alonlat
Character*80, intent(out) :: gridtype

Select Case(alist(48))
  Case(1)
    Call getelemlonlat1(alist,alonlat,gridtype)
  Case DEFAULT
    Call getelemlonlat2(alist,alonlat,gridtype)
End Select

if (alonlat(1,1)==alonlat(1,2)) then
  if (alonlat(1,3)>0.) then
    alonlat(1,1)=alonlat(1,1)-360.
  else
    alonlat(1,2)=alonlat(1,2)-360.
  end if
end if

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines the lon and lat of a grid for GRIB1
!

Subroutine getelemlonlat1(alist,alonlat,gridtype)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Real, dimension(1:2,1:3), intent(out) :: alonlat
Character*80, intent(out) :: gridtype
real midval

Select Case(alist(14))
  Case(0)
    gridtype="Regular"
    alonlat(2,1)=real(alist(17))/1000.
    alonlat(1,1)=real(alist(18))/1000.
    alonlat(2,2)=real(alist(20))/1000.
    alonlat(1,2)=real(alist(21))/1000.
    If (alist(19)>=128) Then
      alonlat(2,3)=abs(real(alist(22))/1000.)
      if (alonlat(2,1)>alonlat(2,2)) alonlat(2,3)=-alonlat(2,3)
      alonlat(1,3)=abs(real(alist(23))/1000.)
      if (alonlat(1,1)>alonlat(1,2)) alonlat(1,3)=-alonlat(1,3)
      if (alist(16)>1) then
        if (abs((alonlat(2,2)-alonlat(2,1))/real(alist(16)-1)) &
            <abs(0.99*alonlat(2,3))) then
          alonlat(2,3)=real(alist(22))/1000.
          alonlat(2,2)=alonlat(2,1)+alonlat(2,3)*real(alist(16)-1)
        end if
      end if
      if (alist(15)>1) then
        if (abs((alonlat(1,2)-alonlat(1,1))/real(alist(15)-1)) &
            <abs(0.99*alonlat(1,3))) then
          alonlat(1,3)=real(alist(23))/1000.
          alonlat(1,2)=alonlat(1,1)+alonlat(1,3)*real(alist(15)-1)
        end if
      end if
    Else
      If ((alist(15)>1).AND.(alist(16)>1)) Then
        alonlat(2,3)=(alonlat(2,2)-alonlat(2,1))/real(alist(15)-1)
        alonlat(1,3)=(alonlat(1,2)-alonlat(1,1))/real(alist(16)-1)
      Else
        Write(6,*) "ERROR: Cannot determine grid spacing"
        Stop
      End If
    End If
  Case(10)
    gridtype="Mercator"
    alonlat(2,1)=real(alist(17))/1000.
    alonlat(1,1)=real(alist(18))/1000.
    alonlat(2,2)=real(alist(20))/1000.
    alonlat(1,2)=real(alist(21))/1000.
    alonlat(2,3)=(alonlat(2,2)-alonlat(2,1))/real(alist(16)-1)
    alonlat(1,3)=(alonlat(1,2)-alonlat(1,1))/real(alist(15)-1)
  Case(20)
    ! Un-supported
    gridtype="Polar"
    alonlat=0.
  Case(30)
    ! Un-supported
    gridtype="Lambert"
    alonlat=0.
  Case(40)
    gridtype="Gaussian"
    alonlat(2,1)=real(alist(17))/1000.
    alonlat(1,1)=real(alist(18))/1000.
    alonlat(2,2)=real(alist(20))/1000.
    alonlat(1,2)=real(alist(21))/1000.
    If (alist(19)>=128) Then
      alonlat(2,3)=(alonlat(2,1)-alonlat(2,2))/real(2*alist(23)-1)
      alonlat(2,3)=real(int(alonlat(2,3)*1000.))/1000.
      midval=0.5*(alonlat(2,1)+alonlat(2,2))
      alonlat(2,2)=0.5*(2.*midval-real(2*alist(23)-1)*alonlat(2,3))
      alonlat(2,2)=real(int(alonlat(2,2)*1000.))/1000.
      alonlat(2,1)=alonlat(2,2)+real(2*alist(23)-1)*alonlat(2,3)
      alonlat(1,3)=real(alist(22))/1000.
    Else
      Write(6,*) "ERROR: Cannot determine grid spacing"
      Stop
    End If
  Case DEFAULT
    ! Un-supported
    gridtype="???"
    alonlat=0.
End Select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines the lon and lat of a grid for GRIB2
!

Subroutine getelemlonlat2(alist,alonlat,gridtype)

Implicit None

Integer, dimension(0:49), intent(in) :: alist
Real, dimension(1:2,1:3), intent(out) :: alonlat
Character*80, intent(out) :: gridtype
Integer resflag
Real unt

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

Select Case(alist(14))
  Case(0)
    gridtype="Regular"
    If ((alist(15)/=0).AND.(alist(16)==0)) Then
      Write(6,*) "ERROR: Cannot read regular grid"
      Write(6,*) alist(15),alist(16)
      alonlat=0.
    Else
      If (alist(15)==0) Then
        unt=1.E-6
      Else
        unt=Real(alist(15))/Real(alist(16))
      End If
      alonlat(2,1)=Real(alist(17))*unt
      alonlat(1,1)=Real(alist(18))*unt
      resflag=alist(19)
      alonlat(2,2)=Real(alist(20))*unt
      alonlat(1,2)=Real(alist(21))*unt
      alonlat(2,3)=Real(alist(23))*unt
      alonlat(1,3)=Real(alist(22))*unt
      If ((iand(resflag,32)==32).AND.(.NOT.(iand(resflag,16)==16))) Then
        alonlat(2,3)=alonlat(1,3)
      End If
      If ((.NOT.(iand(resflag,32)==32)).AND.(iand(resflag,16)==16)) Then
        alonlat(1,3)=alonlat(2,3)
      End If
    End If
    ! CMC bug fix
    if (alonlat(1,3)>0. .and. alonlat(1,1)>alonlat(1,2) ) then
      if ( alonlat(1,1)>0. ) then
        alonlat(1,1) = alonlat(1,1) - 360.
      else
        alonlat(1,2) = alonlat(1,2) + 360.
      end if
    else if (alonlat(1,3)<0. .and. alonlat(1,1)<alonlat(1,2) ) then
      if ( alonlat(1,1)<0. ) then
        alonlat(1,1) = alonlat(1,1) + 360.
      else
        alonlat(1,2) = alonlat(1,2) - 360.
      end if
    end if
  Case(10)
    ! Un-supported
    gridtype="Mercator"
    alonlat=0.
  Case(20)
    ! Un-supported
    gridtype="Polar"
    alonlat=0.
  Case(30)
    ! Un-supported
    gridtype="Lambert"
    alonlat=0.
  Case DEFAULT
    ! Un-supported
    gridtype="???"
    alonlat=0.
End Select

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine adds the forecast time to the date for GRIB1/2

Subroutine g1date(adate,fcstlen,tunit,tp1,tp2,trng)

Implicit None

Integer, dimension(1:6), intent(inout) :: adate
Integer, intent(in) :: tunit,tp1,tp2,trng
Integer, intent(out) :: fcstlen
Integer, dimension(1:6) :: ndate

Select Case(trng)
  case(0,1,113,114,118,123,124)
    fcstlen=tp1
  case(2,3,4,5)
    fcstlen=tp2
  case(10)
    fcstlen=tp1*256+tp2
  case default
    fcstlen=0
End Select

if (fcstlen.eq.0) return

Select Case(tunit)
  Case(0)
    call advdate(adate,ndate,fcstlen)
  Case(1)
    call advdate(adate,ndate,fcstlen*60)
  Case(2)
    call advdate(adate,ndate,fcstlen*1440)
  Case(3)
    ndate=adate
    ndate(2)=adate(2)+fcstlen
    do while (ndate(2).gt.12)
      ndate(2)=ndate(2)-12
      ndate(1)=ndate(1)+1
    end do
  Case(4)
    ndate=adate
    ndate(1)=ndate(1)+fcstlen
  Case(5)
    ndate=adate
    ndate(1)=ndate(1)+fcstlen*10
  Case(6)
    ndate=adate
    ndate(1)=ndate(1)+fcstlen*30
  Case(7)
    ndate=adate
    ndate(1)=ndate(1)+fcstlen*100
  Case(10)
    call advdate(adate,ndate,fcstlen*180)
  Case(11)
    call advdate(adate,ndate,fcstlen*360)
  Case(12)
    call advdate(adate,ndate,fcstlen*720)
  Case(13,254)
    adate(6)=adate(6)+mod(fcstlen,60)
    call advdate(adate,ndate,fcstlen/60)
  Case default
    write(6,*) "ERROR: Unknown time unit in GRIB file"
    stop
End Select

adate=ndate

Return
End

