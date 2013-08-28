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
If ((elemtxt(1).EQ.elemerr(1)).OR.(elemtxt(1).EQ.'')) Then
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
Character*80 elemerr(1:3)
Character*80, save :: ncepopn(0:255,1:3)
logical, save :: first=.true.

elemerr='???'

if (first) then

ncepopn=""
ncepopn(1,:)=(/ "PRES", "Pressure", "Pa" /)
ncepopn(2,:)=(/ "PRMSL", "Pressure reduced to MSL","Pa" /)
ncepopn(3,:)=(/ "PTEND", "Pressure tendency","Pa/s" /)
ncepopn(4,:)=(/ "PVORT", "Pot. vorticity","km^2/kg/s" /)
ncepopn(5,:)=(/ "ICAHT", "ICAO Standard Atmosphere Reference Height","M" /)
ncepopn(6,:)=(/ "GP", "Geopotential","m^2/s^2" /)
ncepopn(7,:)=(/ "HGT", "Geopotential height","gpm" /)
ncepopn(8,:)=(/ "DIST", "Geometric height","m" /)
ncepopn(9,:)=(/ "HSTDV", "Std dev of height","m" /)
ncepopn(10,:)=(/ "TOZNE", "Total ozone","Dobson" /)
ncepopn(11,:)=(/ "TMP", "Temp.","K" /)
ncepopn(12,:)=(/ "VTMP", "Virtual temp.","K" /)
ncepopn(13,:)=(/ "POT", "Potential temp.","K" /)
ncepopn(14,:)=(/ "EPOT", "Pseudo-adiabatic pot. temp.","K" /)
ncepopn(15,:)=(/ "TMAX", "Max. temp.","K" /)
ncepopn(16,:)=(/ "TMIN", "Min. temp.","K" /)
ncepopn(17,:)=(/ "DPT", "Dew point temp.","K" /)
ncepopn(18,:)=(/ "DEPR", "Dew point depression","K" /)
ncepopn(19,:)=(/ "LAPR", "Lapse rate","K/m" /)
ncepopn(20,:)=(/ "VIS", "Visibility","m" /)
ncepopn(21,:)=(/ "RDSP1", "Radar spectra (1)","non-dim" /)
ncepopn(22,:)=(/ "RDSP2", "Radar spectra (2)","non-dim" /)
ncepopn(23,:)=(/ "RDSP3", "Radar spectra (3)","non-dim" /)
ncepopn(24,:)=(/ "PLI", "Parcel lifted index (to 500 hPa)","K" /)
ncepopn(25,:)=(/ "TMPA", "Temp. anomaly","K" /)
ncepopn(26,:)=(/ "PRESA", "Pressure anomaly","Pa" /)
ncepopn(27,:)=(/ "GPA", "Geopotential height anomaly","gpm" /)
ncepopn(28,:)=(/ "WVSP1", "Wave spectra (1)","non-dim" /)
ncepopn(29,:)=(/ "WVSP2", "Wave spectra (2)","non-dim" /)
ncepopn(30,:)=(/ "WVSP3", "Wave spectra (3)","non-dim" /)
ncepopn(31,:)=(/ "WDIR", "Wind direction","deg" /)
ncepopn(32,:)=(/ "WIND", "Wind speed","m/s" /)
ncepopn(33,:)=(/ "UGRD", "u wind","m/s" /)
ncepopn(34,:)=(/ "VGRD", "v wind","m/s" /)
ncepopn(35,:)=(/ "STRM", "Stream function","m^2/s" /)
ncepopn(36,:)=(/ "VPOT", "Velocity potential","m^2/s" /)
ncepopn(37,:)=(/ "MNTSF", "Montgomery stream function","m^2/s^2" /)
ncepopn(38,:)=(/ "SGCVV", "Sigma coord. vertical velocity","/s" /)
ncepopn(39,:)=(/ "VVEL", "Pressure vertical velocity","Pa/s" /)
ncepopn(40,:)=(/ "DZDT", "Geometric vertical velocity","m/s" /)
ncepopn(41,:)=(/ "ABSV", "Absolute vorticity ","/s" /)
ncepopn(42,:)=(/ "ABSD", "Absolute divergence","/s" /)
ncepopn(43,:)=(/ "RELV", "Relative vorticity","/s" /)
ncepopn(44,:)=(/ "RELD", "Relative divergence","/s" /)
ncepopn(45,:)=(/ "VUCSH", "Vertical u shear","/s" /)
ncepopn(46,:)=(/ "VVCSH", "Vertical v shear","/s" /)
ncepopn(47,:)=(/ "DIRC", "Direction of current","deg" /)
ncepopn(48,:)=(/ "SPC", "Speed of current","m/s" /)
ncepopn(49,:)=(/ "UOGRD", "u of current","m/s" /)
ncepopn(50,:)=(/ "VOGRD", "v of current","m/s" /)
ncepopn(51,:)=(/ "SPFH", "Specific humidity","kg/kg" /)
ncepopn(52,:)=(/ "RH", "Relative humidity","%" /)
ncepopn(53,:)=(/ "MIXR", "Humidity mixing ratio","kg/kg" /)
ncepopn(54,:)=(/ "PWAT", "Precipitable water","kg/m^2" /)
ncepopn(55,:)=(/ "VAPP", "Vapor pressure","Pa" /)
ncepopn(56,:)=(/ "SATD", "Saturation deficit","Pa" /)
ncepopn(57,:)=(/ "EVP", "Evaporation","kg/m^2" /)
ncepopn(58,:)=(/ "CICE", "Cloud Ice","kg/m^2" /)
ncepopn(59,:)=(/ "PRATE", "Precipitation rate","kg/m^2/s" /)
ncepopn(60,:)=(/ "TSTM", "Thunderstorm probability","%" /)
ncepopn(61,:)=(/ "APCP", "Total precipitation","kg/m^2" /)
ncepopn(62,:)=(/ "NCPCP", "Large scale precipitation","kg/m^2" /)
ncepopn(63,:)=(/ "ACPCP", "Convective precipitation","kg/m^2" /)
ncepopn(64,:)=(/ "SRWEQ", "Snowfall rate water equiv.","kg/m^2/s" /)
ncepopn(65,:)=(/ "WEASD", "Accum. snow","kg/m^2" /)
ncepopn(66,:)=(/ "SNOD", "Snow depth","m" /)
ncepopn(67,:)=(/ "MIXHT", "Mixed layer depth","m" /)
ncepopn(68,:)=(/ "TTHDP", "Transient thermocline depth","m" /)
ncepopn(69,:)=(/ "MTHD", "Main thermocline depth","m" /)
ncepopn(70,:)=(/ "MTHA", "Main thermocline anomaly","m" /)
ncepopn(71,:)=(/ "TCDC", "Total cloud cover","%" /)
ncepopn(72,:)=(/ "CDCON", "Convective cloud cover","%" /)
ncepopn(73,:)=(/ "LCDC", "Low level cloud cover","%" /)
ncepopn(74,:)=(/ "MCDC", "Mid level cloud cover","%" /)
ncepopn(75,:)=(/ "HCDC", "High level cloud cover","%" /)
ncepopn(76,:)=(/ "CWAT", "Cloud water","kg/m^2" /)
ncepopn(77,:)=(/ "BLI", "Best lifted index (to 500 hPa)","K" /)
ncepopn(78,:)=(/ "SNOC", "Convective snow","kg/m^2" /)
ncepopn(79,:)=(/ "SNOL", "Large scale snow","kg/m^2" /)
ncepopn(80,:)=(/ "WTMP", "Water temp.","K" /)
ncepopn(81,:)=(/ "LAND", "Land cover (land=1;sea=0)","fraction" /)
ncepopn(82,:)=(/ "DSLM", "Deviation of sea level from mean","m" /)
ncepopn(83,:)=(/ "SFCR", "Surface roughness","m" /)
ncepopn(84,:)=(/ "ALBDO", "Albedo","%" /)
ncepopn(85,:)=(/ "TSOIL", "Soil temp.","K" /)
ncepopn(86,:)=(/ "SOILM", "Soil moisture content","kg/m^2" /)
ncepopn(87,:)=(/ "VEG", "Vegetation","%" /)
ncepopn(88,:)=(/ "SALTY", "Salinity","kg/kg" /)
ncepopn(89,:)=(/ "DEN", "Density","kg/m^3" /)
ncepopn(90,:)=(/ "WATR", "Water runoff","kg/m^2" /)
ncepopn(91,:)=(/ "ICEC", "Ice concentration (ice=1;no ice=0)","fraction" /)
ncepopn(92,:)=(/ "ICETK", "Ice thickness","m" /)
ncepopn(93,:)=(/ "DICED", "Direction of ice drift","deg" /)
ncepopn(94,:)=(/ "SICED", "Speed of ice drift","m/s" /)
ncepopn(95,:)=(/ "UICE", "u of ice drift","m/s" /)
ncepopn(96,:)=(/ "VICE", "v of ice drift","m/s" /)
ncepopn(97,:)=(/ "ICEG", "Ice growth rate","m/s" /)
ncepopn(98,:)=(/ "ICED", "Ice divergence","/s" /)
ncepopn(99,:)=(/ "SNOM", "Snow melt","kg/m^2" /)
ncepopn(100,:)=(/ "HTSGW", "Sig height of wind waves and swell","m" /)
ncepopn(101,:)=(/ "WVDIR", "Direction of wind waves","deg" /)
ncepopn(102,:)=(/ "WVHGT", "Sig height of wind waves","m" /)
ncepopn(103,:)=(/ "WVPER", "Mean period of wind waves","s" /)
ncepopn(104,:)=(/ "SWDIR", "Direction of swell waves","deg" /)
ncepopn(105,:)=(/ "SWELL", "Sig height of swell waves","m" /)
ncepopn(106,:)=(/ "SWPER", "Mean period of swell waves","s" /)
ncepopn(107,:)=(/ "DIRPW", "Primary wave direction","deg" /)
ncepopn(108,:)=(/ "PERPW", "Primary wave mean period","s" /)
ncepopn(109,:)=(/ "DIRSW", "Secondary wave direction","deg" /)
ncepopn(110,:)=(/ "PERSW", "Secondary wave mean period","s" /)
ncepopn(111,:)=(/ "NSWRS", "Net short wave (surface)","W/m^2" /)
ncepopn(112,:)=(/ "NLWRS", "Net long wave (surface)","W/m^2" /)
ncepopn(113,:)=(/ "NSWRT", "Net short wave (top)","W/m^2" /)
ncepopn(114,:)=(/ "NLWRT", "Net long wave (top)","W/m^2" /)
ncepopn(115,:)=(/ "LWAVR", "Long wave","W/m^2" /)
ncepopn(116,:)=(/ "SWAVR", "Short wave","W/m^2" /)
ncepopn(117,:)=(/ "GRAD", "Global radiation","W/m^2" /)
ncepopn(118,:)=(/ "BRTMP", "Brightness temperature","K" /)
ncepopn(119,:)=(/ "LWRAD", "Radiance with respect to wave no.","W/m/sr" /)
ncepopn(120,:)=(/ "SWRAD", "Radiance with respect ot wave len.","W/m^3/sr" /)
ncepopn(121,:)=(/ "LHTFL", "Latent heat flux","W/m^2" /)
ncepopn(122,:)=(/ "SHTFL", "Sensible heat flux","W/m^2" /)
ncepopn(123,:)=(/ "BLYDP", "Boundary layer dissipation","W/m^2" /)
ncepopn(124,:)=(/ "UFLX", "Zonal momentum flux","N/m^2" /)
ncepopn(125,:)=(/ "VFLX", "Meridional momentum flux","N/m^2" /)
ncepopn(126,:)=(/ "WMIXE", "Wind mixing energy","J" /)
ncepopn(127,:)=(/ "IMGD", "Image data","" /)
ncepopn(128,:)=(/ "MSLSA", "Mean sea level pressure (Std Atm)","Pa" /)
ncepopn(129,:)=(/ "MSLMA", "Mean sea level pressure (MAPS)","Pa" /)
ncepopn(130,:)=(/ "MSLET", "Mean sea level pressure (ETA model)","Pa" /)
ncepopn(131,:)=(/ "LFTX", "Surface lifted index","K" /)
ncepopn(132,:)=(/ "4LFTX", "Best (4-layer) lifted index","K" /)
ncepopn(133,:)=(/ "KX", "K index","K" /)
ncepopn(134,:)=(/ "SX", "Sweat index","K" /)
ncepopn(135,:)=(/ "MCONV", "Horizontal moisture divergence","kg/kg/s" /)
ncepopn(136,:)=(/ "VWSH", "Vertical speed shear","1/s" /)
ncepopn(137,:)=(/ "TSLSA", "3-hr pressure tendency (Std Atmos Red)","Pa/s" /)
ncepopn(138,:)=(/ "BVF2", "Brunt-Vaisala frequency^2","1/s^2" /)
ncepopn(139,:)=(/ "PVMW", "Potential vorticity (mass-weighted)","1/s/m" /)
ncepopn(140,:)=(/ "CRAIN", "Categorical rain","yes=1;no=0" /)
ncepopn(141,:)=(/ "CFRZR", "Categorical freezing rain","yes=1;no=0" /)
ncepopn(142,:)=(/ "CICEP", "Categorical ice pellets","yes=1;no=0" /)
ncepopn(143,:)=(/ "CSNOW", "Categorical snow","yes=1;no=0" /)
ncepopn(144,:)=(/ "SOILW", "Volumetric soil moisture","fraction" /)
ncepopn(145,:)=(/ "PEVPR", "Potential evaporation rate","W/m^2" /)
ncepopn(146,:)=(/ "CWORK", "Cloud work function","J/kg" /)
ncepopn(147,:)=(/ "U-GWD", "Zonal gravity wave stress","N/m^2" /)
ncepopn(148,:)=(/ "V-GWD", "Meridional gravity wave stress","N/m^2" /)
ncepopn(149,:)=(/ "PV", "Potential vorticity","m^2/s/kg" /)
ncepopn(150,:)=(/ "COVMZ", "Covariance between u and v","m^2/s^2" /)
ncepopn(151,:)=(/ "COVTZ", "Covariance between u and T","K*m/s" /)
ncepopn(152,:)=(/ "COVTM", "Covariance between v and T","K*m/s" /)
ncepopn(153,:)=(/ "CLWMR", "Cloud water","kg/kg" /)
ncepopn(154,:)=(/ "O3MR", "Ozone mixing ratio","kg/kg" /)
ncepopn(155,:)=(/ "GFLUX", "Ground heat flux","W/m^2" /)
ncepopn(156,:)=(/ "CIN", "Convective inhibition","J/kg" /)
ncepopn(157,:)=(/ "CAPE", "Convective Avail. Pot. Energy","J/kg" /)
ncepopn(158,:)=(/ "TKE", "Turbulent kinetic energy","J/kg" /)
ncepopn(159,:)=(/ "CONDP", "Lifted parcel condensation pressure","Pa" /)
ncepopn(160,:)=(/ "CSUSF", "Clear sky upward solar flux","W/m^2" /)
ncepopn(161,:)=(/ "CSDSF", "Clear sky downward solar flux","W/m^2" /)
ncepopn(162,:)=(/ "CSULF", "Clear sky upward long wave flux","W/m^2" /)
ncepopn(163,:)=(/ "CSDLF", "Clear sky downward long wave flux","W/m^2" /)
ncepopn(164,:)=(/ "CFNSF", "Cloud forcing net solar flux","W/m^2" /)
ncepopn(165,:)=(/ "CFNLF", "Cloud forcing net long wave flux","W/m^2" /)
ncepopn(166,:)=(/ "VBDSF", "Visible beam downward solar flux","W/m^2" /)
ncepopn(167,:)=(/ "VDDSF", "Visible diffuse downward solar flux","W/m^2" /)
ncepopn(168,:)=(/ "NBDSF", "Near IR beam downward solar flux","W/m^2" /)
ncepopn(169,:)=(/ "NDDSF", "Near IR diffuse downward solar flux","W/m^2" /)
ncepopn(170,:)=(/ "RWMR", "Rain water mixing ratio","kg/kg" /)
ncepopn(171,:)=(/ "SNMR", "Snow mixing ratio","kg/kg" /)
ncepopn(172,:)=(/ "MFLX", "Momentum flux","N/m^2" /)
ncepopn(173,:)=(/ "LMH", "Mass point model surface","non-dim" /)
ncepopn(174,:)=(/ "LMV", "Velocity point model surface","non-dim" /)
ncepopn(175,:)=(/ "MLYNO", "Model layer number (from bottom up)","non-dim" /)
ncepopn(176,:)=(/ "NLAT", "Latitude (-90 to +90)","deg" /)
ncepopn(177,:)=(/ "ELON", "East longitude (0-360)","deg" /)
ncepopn(178,:)=(/ "ICMR", "Ice mixing ratio","kg/kg" /)
ncepopn(179,:)=(/ "GRMR", "Graupel mixing ratio","kg/kg" /)
ncepopn(180,:)=(/ "GUST", "Surface wind gust","m/s" /)
ncepopn(181,:)=(/ "LPSX", "x-gradient of log pressure","1/m" /)
ncepopn(182,:)=(/ "LPSY", "y-gradient of log pressure","1/m" /)
ncepopn(183,:)=(/ "HGTX", "x-gradient of height","m/m" /)
ncepopn(184,:)=(/ "HGTY", "y-gradient of height","m/m" /)
ncepopn(185,:)=(/ "TURB", "Turbulence SIGMET/AIRMET","non-dim" /)
ncepopn(186,:)=(/ "ICNG", "Icing SIGMET/AIRMET","non-dim" /)
ncepopn(187,:)=(/ "LTNG", "Lightning","non-dim" /)
ncepopn(188,:)=(/ "DRIP", "Rate of water dropping from canopy to gnd","kg/m^2" /)
ncepopn(189,:)=(/ "VPTMP", "Virtual pot. temp.","K" /)
ncepopn(190,:)=(/ "HLCY", "Storm relative helicity","m^2/s^2" /)
ncepopn(191,:)=(/ "PROB", "Prob. from ensemble","non-dim" /)
ncepopn(192,:)=(/ "PROBN", "Prob. from ensemble norm. to clim. expect.","non-dim" /)
ncepopn(193,:)=(/ "POP", "Prob. of precipitation","%" /)
ncepopn(194,:)=(/ "CPOFP", "Prob. of frozen precipitation","%" /)
ncepopn(195,:)=(/ "CPOZP", "Prob. of freezing precipitation","%" /)
ncepopn(196,:)=(/ "USTM", "u-component of storm motion","m/s" /)
ncepopn(197,:)=(/ "VSTM", "v-component of storm motion","m/s" /)
ncepopn(198,:)=(/ "NCIP", "No. concen. ice particles","" /)
ncepopn(199,:)=(/ "EVBS", "Direct evaporation from bare soil","W/m^2" /)
ncepopn(200,:)=(/ "EVCW", "Canopy water evaporation","W/m^2" /)
ncepopn(201,:)=(/ "ICWAT", "Ice-free water surface","%" /)
ncepopn(202,:)=(/ "CWDI", "Convective weather detection index","" /)
ncepopn(203,:)=(/ "VAFTAD", "VAFTAD??","??" /)
ncepopn(204,:)=(/ "DSWRF", "Downward short wave flux","W/m^2" /)
ncepopn(205,:)=(/ "DLWRF", "Downward long wave flux","W/m^2" /)
ncepopn(206,:)=(/ "UVI", "Ultra violet index (1 hour centered at solar noon)","J/m^2" /)
ncepopn(207,:)=(/ "MSTAV", "Moisture availability","%" /)
ncepopn(208,:)=(/ "SFEXC", "Exchange coefficient","(kg/m^3)(m/s)" /)
ncepopn(209,:)=(/ "MIXLY", "No. of mixed layers next to surface","integer" /)
ncepopn(210,:)=(/ "TRANS", "Transpiration","W/m^2" /)
ncepopn(211,:)=(/ "USWRF", "Upward short wave flux","W/m^2" /)
ncepopn(212,:)=(/ "ULWRF", "Upward long wave flux","W/m^2" /)
ncepopn(213,:)=(/ "CDLYR", "Non-convective cloud","%" /)
ncepopn(214,:)=(/ "CPRAT", "Convective precip. rate","kg/m^2/s" /)
ncepopn(215,:)=(/ "TTDIA", "Temp. tendency by all physics","K/s" /)
ncepopn(216,:)=(/ "TTRAD", "Temp. tendency by all radiation","K/s" /)
ncepopn(217,:)=(/ "TTPHY", "Temp. tendency by non-radiation physics","K/s" /)
ncepopn(218,:)=(/ "PREIX", "Precip index (0.0-1.00)","fraction" /)
ncepopn(219,:)=(/ "TSD1D", "Std. dev. of IR T over 1x1 deg area","K" /)
ncepopn(220,:)=(/ "NLGSP", "Natural log of surface pressure","ln(kPa)" /)
ncepopn(221,:)=(/ "HPBL", "Planetary boundary layer height","m" /)
ncepopn(222,:)=(/ "5WAVH", "5-wave geopotential height","gpm" /)
ncepopn(223,:)=(/ "CNWAT", "Plant canopy surface water","kg/m^2" /)
ncepopn(224,:)=(/ "SOTYP", "Soil type (Zobler)","0..9" /)
ncepopn(225,:)=(/ "VGTYP", "Vegetation type (as in SiB)","0..13" /)
ncepopn(226,:)=(/ "BMIXL", "Blackadar's mixing length scale","m" /)
ncepopn(227,:)=(/ "AMIXL", "Asymptotic mixing length scale","m" /)
ncepopn(228,:)=(/ "PEVAP", "Pot. evaporation","kg/m^2" /)
ncepopn(229,:)=(/ "SNOHF", "Snow phase-change heat flux","W/m^2" /)
ncepopn(230,:)=(/ "5WAVA", "5-wave geopot. height anomaly","gpm" /)
ncepopn(231,:)=(/ "MFLUX", "Convective cloud mass flux","Pa/s" /)
ncepopn(232,:)=(/ "DTRF", "Downward total radiation flux","W/m^2" /)
ncepopn(233,:)=(/ "UTRF", "Upward total radiation flux","W/m^2" /)
ncepopn(234,:)=(/ "BGRUN", "Baseflow-groundwater runoff","kg/m^2" /)
ncepopn(235,:)=(/ "SSRUN", "Storm surface runoff","kg/m^2" /)
ncepopn(236,:)=(/ "SIPD", "Supercooled large droplet (SLD) icing pot. diagn.","" /)
ncepopn(237,:)=(/ "O3TOT", "Total ozone","kg/m^2" /)
ncepopn(238,:)=(/ "SNOWC", "Snow cover","%" /)
ncepopn(239,:)=(/ "SNOT", "Snow temp.","K" /)
ncepopn(240,:)=(/ "COVTW", "Covariance T and w","K*m/s" /)
ncepopn(241,:)=(/ "LRGHR", "Large scale condensation heating","K/s" /)
ncepopn(242,:)=(/ "CNVHR", "Deep convective heating","K/s" /)
ncepopn(243,:)=(/ "CNVMR", "Deep convective moistening","kg/kg/s" /)
ncepopn(244,:)=(/ "SHAHR", "Shallow convective heating","K/s" /)
ncepopn(245,:)=(/ "SHAMR", "Shallow convective moistening","kg/kg/s" /)
ncepopn(246,:)=(/ "VDFHR", "Vertical diffusion heating","K/s" /)
ncepopn(247,:)=(/ "VDFUA", "Vertical diffusion zonal accel","m/s^2" /)
ncepopn(248,:)=(/ "VDFVA", "Vertical diffusion meridional accel","m/s^2" /)
ncepopn(249,:)=(/ "VDFMR", "Vertical diffusion moistening","kg/kg/s" /)
ncepopn(250,:)=(/ "SWHR", "Solar radiative heating","K/s" /)
ncepopn(251,:)=(/ "LWHR", "Longwave radiative heating","K/s" /)
ncepopn(252,:)=(/ "CD", "Drag coefficient","non-dim" /)
ncepopn(253,:)=(/ "FRICV", "Friction velocity","m/s" /)
ncepopn(254,:)=(/ "RI", "Richardson number","non-dim" /)

first=.false.
end if

if (ain.GT.size(ncepopn,DIM=1)) Then
  elemtxt=elemerr
else
  elemtxt=ncepopn(ain,:)
end If

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ECMWF Metadata 128
!

subroutine gecmwf128(ain,elemtxt)

implicit none

Integer, intent(in) :: ain
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80 elemerr(1:3)
Character*80, save :: ecmwf128(0:255,1:3)
logical, save :: first=.true.

elemerr='???'

if (first) then

ecmwf128=""
ecmwf128(1,:)=(/ "STRF", "Stream function", "m^2/s" /)
ecmwf128(2,:)=(/ "VPOT", "Velocity potential", "m^2/s" /)
ecmwf128(3,:)=(/ "PT", "Potential temperature", "K" /)
ecmwf128(4,:)=(/ "EQPT", "Equivalent potential temperature", "K" /)
ecmwf128(5,:)=(/ "SEPT", "Saturated equivalent potential temperature", "K" /)
ecmwf128(6,:)=(/ "var6", "reserved", "" /)
ecmwf128(7,:)=(/ "var7", "reserved", "" /)
ecmwf128(8,:)=(/ "var8", "reserved", "" /)
ecmwf128(9,:)=(/ "var9", "reserved", "" /)
ecmwf128(10,:)=(/ "var10", "reserved", "" /)
ecmwf128(11,:)=(/ "UDVW", "U component of divergent wind", "m/s" /)
ecmwf128(12,:)=(/ "VDVW", "V component of divergent wind", "m/s" /)
ecmwf128(13,:)=(/ "URTW", "U component of rotational wind", "m/s" /)
ecmwf128(14,:)=(/ "VRTW", "V component of rotational wind", "m/s" /)
ecmwf128(15,:)=(/ "var15", "reserved", "" /)
ecmwf128(16,:)=(/ "var16", "reserved", "" /)
ecmwf128(17,:)=(/ "var17", "reserved", "" /)
ecmwf128(18,:)=(/ "var18", "reserved", "" /)
ecmwf128(19,:)=(/ "var19", "reserved", "" /)
ecmwf128(20,:)=(/ "var20", "reserved", "" /)
ecmwf128(21,:)=(/ "UCTP", "Unbalanced component of temperature", "K" /)
ecmwf128(22,:)=(/ "UCLN", "Unbalanced component of logarithm of surface pressure", "" /)
ecmwf128(23,:)=(/ "UCDV", "Unbalanced component of divergence", "1/s" /)
ecmwf128(24,:)=(/ "var24", "reserved", "" /)
ecmwf128(25,:)=(/ "var25", "reserved", "" /)
ecmwf128(26,:)=(/ "CL", "Lake cover", "%" /)
ecmwf128(27,:)=(/ "CVL", "Low vegetation cover", "%" /)
ecmwf128(28,:)=(/ "CVH", "High vegetation cover", "%" /)
ecmwf128(29,:)=(/ "TVL", "Type of low vegetation", "" /)
ecmwf128(30,:)=(/ "TVH", "Type of high vegetation", "" /)
ecmwf128(31,:)=(/ "CI", "Sea-ice cover", "%" /)
ecmwf128(32,:)=(/ "ASN", "Snow albedo", "%" /)
ecmwf128(33,:)=(/ "RSN", "Snow density", "kg/m^3" /)
ecmwf128(34,:)=(/ "SSTK", "Sea surface temperature", "K" /)
ecmwf128(35,:)=(/ "ISTL1", "Ice surface temperature layer 1", "K" /)
ecmwf128(36,:)=(/ "ISTL2", "Ice surface temperature layer 2", "K" /)
ecmwf128(37,:)=(/ "ISTL3", "Ice surface temperature layer 3", "K" /)
ecmwf128(38,:)=(/ "ISTL4", "Ice surface temperature layer 4", "K" /)
ecmwf128(39,:)=(/ "SWVL1", "Volumetric soil water layer 1", "m^3/m^3" /)
ecmwf128(40,:)=(/ "SWVL2", "Volumetric soil water layer 2", "m^3/m^3" /)
ecmwf128(41,:)=(/ "SWVL3", "Volumetric soil water layer 3", "m^3/m^3" /)
ecmwf128(42,:)=(/ "SWVL4", "Volumetric soil water layer 4", "m^3/m^3" /)
ecmwf128(43,:)=(/ "SLT", "Soil type", "" /)
ecmwf128(44,:)=(/ "ES", "Snow evaporation", "m of water" /)
ecmwf128(45,:)=(/ "SMLT", "Snowmelt", "m of water" /)
ecmwf128(46,:)=(/ "SDUR", "Solar duration", "s" /)
ecmwf128(47,:)=(/ "DSRP", "Direct solar radiation", "W/m^2" /)
ecmwf128(48,:)=(/ "MAGSS", "Magnitude of surface stress", "N s/m^2" /)
ecmwf128(49,:)=(/ "10FG", "Wind gust at 10 metres", "m/s" /)
ecmwf128(50,:)=(/ "LSPF", "Large-scale precipitation fraction", "s" /)
ecmwf128(51,:)=(/ "MX2T24", "Maximum 2 metre temperature", "K" /)
ecmwf128(52,:)=(/ "MN2T24", "Minimum 2 metre temperature", "K" /)
ecmwf128(53,:)=(/ "MONT", "Montgomery potential", "m^2/s^2" /)
ecmwf128(54,:)=(/ "PRES", "Pressure", "Pa" /)
ecmwf128(57,:)=(/ "UVB", "Downward UV radiation at the surface (Ultra-violet band B)", "W/m^2" /)
ecmwf128(58,:)=(/ "PAR", "Photosynthetically active radiation at the surface", "W/m^2" /)
ecmwf128(59,:)=(/ "CAPE", "Convective available potential energy", "J/kg" /)
ecmwf128(60,:)=(/ "PV", "Potential vorticity", "K m^2 / (kg s)" /)
ecmwf128(127,:)=(/ "AT", "Atmospheric tide", "" /)
ecmwf128(128,:)=(/ "BV", "Budget values", "" /)
ecmwf128(129,:)=(/ "Z", "Geopotential", "m^2/s^2" /)
ecmwf128(130,:)=(/ "T", "Temperature", "K" /)
ecmwf128(131,:)=(/ "U", "U velocity", "m/s" /)
ecmwf128(132,:)=(/ "V", "V velocity", "m/s" /)
ecmwf128(133,:)=(/ "Q", "Specific humidity", "kg/kg" /)
ecmwf128(134,:)=(/ "SP", "Surface pressure", "Pa" /)
ecmwf128(135,:)=(/ "W", "Vertical velocity", "Pa/s" /)
ecmwf128(136,:)=(/ "TCW", "Total column water", "kg/m^2" /)
ecmwf128(137,:)=(/ "TCWV", "Total column water vapour", "kg/m^2" /)
ecmwf128(138,:)=(/ "VO", "Vorticity (relative)", "1/s" /)
ecmwf128(139,:)=(/ "STL1", "Soil temperature level 1", "K" /)
ecmwf128(140,:)=(/ "SWL1", "Soil wetness level 1", "m of water" /)
ecmwf128(141,:)=(/ "SD", "Snow depth", "m of water" /)
ecmwf128(142,:)=(/ "LSP", "Stratiform precipitation", "m" /)
ecmwf128(143,:)=(/ "CP", "Convective precipitation", "m" /)
ecmwf128(144,:)=(/ "SF", "Snowfall (convective + stratiform)", "m of water" /)
ecmwf128(145,:)=(/ "BLD", "Boundary layer dissipation", "W s/m^2" /)
ecmwf128(146,:)=(/ "SSHF", "Surface sensible heat flux", "W s/m^2" /)
ecmwf128(147,:)=(/ "SLHF", "Surface latent heat flux", "W s/m^2" /)
ecmwf128(148,:)=(/ "CHNK", "Charnock", "" /)
ecmwf128(149,:)=(/ "SNR", "Surface net radiation", "W s/m^2" /)
ecmwf128(150,:)=(/ "TNR", "Top net radiation", "" /)
ecmwf128(151,:)=(/ "MSL", "Mean sea-level pressure", "Pa" /)
ecmwf128(152,:)=(/ "LNSP", "Logarithm of surface pressure", "" /)
ecmwf128(153,:)=(/ "SWHR", "Short-wave heating rate", "K" /)
ecmwf128(154,:)=(/ "LWHR", "Long-wave heating rate", "K" /)
ecmwf128(155,:)=(/ "D", "Divergence", "1/s" /)
ecmwf128(156,:)=(/ "GH", "Height", "m" /)
ecmwf128(157,:)=(/ "R", "Relative humidity", "%" /)
ecmwf128(158,:)=(/ "TSP", "Tendency of surface pressure", "Pa/s" /)
ecmwf128(159,:)=(/ "BLH", "Boundary layer height", "m" /)
ecmwf128(160,:)=(/ "SDOR", "Standard deviation of orography", "" /)
ecmwf128(161,:)=(/ "ISOR", "Anisotropy of sub-gridscale orography", "" /)
ecmwf128(162,:)=(/ "ANOR", "Angle of sub-gridscale orography", "rad" /)
ecmwf128(163,:)=(/ "SLOR", "Slope of sub-gridscale orography", "" /)
ecmwf128(164,:)=(/ "TCC", "Total cloud cover", "%" /)
ecmwf128(165,:)=(/ "10U", "10 metre U wind component", "m/s" /)
ecmwf128(166,:)=(/ "10V", "10 metre V wind component", "m/s" /)
ecmwf128(167,:)=(/ "2T", "2 metre temperature", "K" /)
ecmwf128(168,:)=(/ "2D", "2 metre dewpoint temperature", "K" /)
ecmwf128(169,:)=(/ "SSRD", "Surface solar radiation downwards", "W s/m^2" /)
ecmwf128(170,:)=(/ "STL2", "Soil temperature level 2", "K" /)
ecmwf128(171,:)=(/ "SWL2", "Soil wetness level 2", "m of water" /)
ecmwf128(172,:)=(/ "LSM", "Land/sea mask", "%" /)
ecmwf128(173,:)=(/ "SR", "Surface roughness", "m" /)
ecmwf128(174,:)=(/ "AL", "Albedo", "%" /)
ecmwf128(175,:)=(/ "STRD", "Surface thermal radiation downwards", "W s/m^2" /)
ecmwf128(176,:)=(/ "SSR", "Surface solar radiation", "W s/m^2" /)
ecmwf128(177,:)=(/ "STR", "Surface thermal radiation", "W s/m^2" /)
ecmwf128(178,:)=(/ "TSR", "Top solar radiation", "W s/m^2" /)
ecmwf128(179,:)=(/ "TTR", "Top thermal radiation", "W s/m^2" /)
ecmwf128(180,:)=(/ "EWSS", "East/West surface stress", "N s/m^2" /)
ecmwf128(181,:)=(/ "NSSS", "North/South surface stress", "N s/m^2" /)
ecmwf128(182,:)=(/ "E", "Evaporation", "m of water" /)
ecmwf128(183,:)=(/ "STL3", "Soil temperature level 3", "K" /)
ecmwf128(184,:)=(/ "SWL3", "Soil wetness level 3", "m of water" /)
ecmwf128(185,:)=(/ "CCC", "Convective cloud cover", "%" /)
ecmwf128(186,:)=(/ "LCC", "Low cloud cover", "%" /)
ecmwf128(187,:)=(/ "MCC", "Medium cloud cover", "%" /)
ecmwf128(188,:)=(/ "HCC", "High cloud cover", "%" /)
ecmwf128(189,:)=(/ "SUND", "Sunshine duration", "s" /)
ecmwf128(190,:)=(/ "EWOV", "EW component of subgrid orographic variance", "m^2" /)
ecmwf128(191,:)=(/ "NSOV", "NS component of subgrid orographic variance", "m^2" /)
ecmwf128(192,:)=(/ "NWOV", "NWSE component of subgrid orographic variance", "m^2" /)
ecmwf128(193,:)=(/ "NEOV", "NESW component of subgrid orographic variance", "m^2" /)
ecmwf128(194,:)=(/ "BTMP", "Brightness temperature", "K" /)
ecmwf128(195,:)=(/ "LGWS", "Lat. component of gravity wave stress", "N s/m^2" /)
ecmwf128(196,:)=(/ "MGWS", "Meridional component of gravity wave stress", "N s/m^2" /)
ecmwf128(197,:)=(/ "GWD", "Gravity wave dissipation", "W s/m^2" /)
ecmwf128(198,:)=(/ "SRC", "Skin reservoir content", "m of water" /)
ecmwf128(199,:)=(/ "VEG", "Vegetation fraction", "%" /)
ecmwf128(200,:)=(/ "VSO", "Variance of sub-gridscale orography", "m^2" /)
ecmwf128(201,:)=(/ "MX2T", "Maximum 2 metre temperature since previous post-processing", "K" /)
ecmwf128(202,:)=(/ "MN2T", "Minimum 2 metre temperature since previous post-processing", "K" /)
ecmwf128(203,:)=(/ "O3", "Ozone mass mixing ratio", "kg/kg" /)
ecmwf128(204,:)=(/ "PAW", "Precipiation analysis weights", "" /)
ecmwf128(205,:)=(/ "RO", "Runoff", "m" /)
ecmwf128(206,:)=(/ "TCO3", "Total column ozone", "Dobson" /)
ecmwf128(207,:)=(/ "10SI", "10 meter windspeed", "m/s" /)
ecmwf128(208,:)=(/ "TSRC", "Top net solar radiation, clear sky", "W/m^2" /)
ecmwf128(209,:)=(/ "TTRC", "Top net thermal radiation, clear sky", "W/m^2" /)
ecmwf128(210,:)=(/ "SSRC", "Surface net solar radiation, clear sky", "W/m^2" /)
ecmwf128(211,:)=(/ "STRC", "Surface net thermal radiation, clear sky", "W/m^2" /)
ecmwf128(212,:)=(/ "SI", "Solar insolation", "W/m^2" /)
ecmwf128(214,:)=(/ "DHR", "Diabatic heating by radiation", "K" /)
ecmwf128(215,:)=(/ "DHVD", "Diabatic heating by vertical diffusion", "K" /)
ecmwf128(216,:)=(/ "DHCC", "Diabatic heating by cumulus convection", "K" /)
ecmwf128(217,:)=(/ "DHLC", "Diabatic heating large-scale condensation", "K" /)
ecmwf128(218,:)=(/ "VDZW", "Vertical diffusion of zonal wind", "m/s" /)
ecmwf128(219,:)=(/ "VDMW", "Vertical diffusion of meridional wind", "m/s" /)
ecmwf128(220,:)=(/ "EWGD", "EW gravity wave drag tendency", "m/s" /)
ecmwf128(221,:)=(/ "NSGD", "NS gravity wave drag tendency", "m/s" /)
ecmwf128(222,:)=(/ "CTZW", "Convective tendency of zonal wind", "m/s" /)
ecmwf128(223,:)=(/ "CTMW", "Convective tendency of meridional wind", "m/s" /)
ecmwf128(224,:)=(/ "VDH", "Vertical diffusion of humidity", "kg/kg" /)
ecmwf128(225,:)=(/ "HTCC", "Humidity tendency by cumulus convection", "kg/kg" /)
ecmwf128(226,:)=(/ "HTLC", "Humidity tendency large-scale condensation", "kg/kg" /)
ecmwf128(227,:)=(/ "CRNH", "Change from removing negative humidity", "kg/kg" /)
ecmwf128(228,:)=(/ "TP", "Total precipitation", "m" /)
ecmwf128(229,:)=(/ "IEWS", "Instantaneous X surface stress", "N/m^2" /)
ecmwf128(230,:)=(/ "INSS", "Instantaneous Y surface stress", "N/m^2" /)
ecmwf128(231,:)=(/ "ISHF", "Instantaneous surface heat flux", "W/m^2" /)
ecmwf128(232,:)=(/ "IE", "Instantaneous moisture flux", "kg s/m^2" /)
ecmwf128(233,:)=(/ "ASQ", "Apparent surface humidity", "kg/kg" /)
ecmwf128(234,:)=(/ "LSRH", "Logarithm of surface roughness length for heat", "" /)
ecmwf128(235,:)=(/ "SKT", "Skin temperature", "K" /)
ecmwf128(236,:)=(/ "STL4", "Soil temperature level 4", "K" /)
ecmwf128(237,:)=(/ "SWL4", "Soil wetness level 4", "m" /)
ecmwf128(238,:)=(/ "TSN", "Temperature of snow layer", "K" /)
ecmwf128(239,:)=(/ "CSF", "Convective snowfall", "m of water" /)
ecmwf128(240,:)=(/ "LSF", "Large-scale snowfall", "m of water" /)
ecmwf128(241,:)=(/ "ACF", "Accumulated cloud fraction tendency", "-1 to 1" /)
ecmwf128(242,:)=(/ "ALW", "Accumulated liquid water tendency", "-1 to 1" /)
ecmwf128(243,:)=(/ "FAL", "Forecast albedo", "%" /)
ecmwf128(244,:)=(/ "FSR", "Forecast surface roughness", "m" /)
ecmwf128(245,:)=(/ "FLSR", "Forecast log of surface roughness for heat", "" /)
ecmwf128(246,:)=(/ "CLWC", "Cloud liquid water content", "kg/kg" /)
ecmwf128(247,:)=(/ "CIWC", "Cloud ice water content", "kg/kg" /)
ecmwf128(248,:)=(/ "CC", "Cloud cover", "%" /)
ecmwf128(249,:)=(/ "AIW", "Accumulated ice water tendency", "-1 to 1" /)
ecmwf128(250,:)=(/ "ICE", "Ice age", "[1,0]" /)
ecmwf128(251,:)=(/ "ATTE", "Adiabatic tendency of temperature", "K" /)
ecmwf128(252,:)=(/ "ATHE", "Adiabatic tendency of humidity", "kg/kg" /)
ecmwf128(253,:)=(/ "ATZE", "Adiabatic tendency of zonal wind", "m/s" /)
ecmwf128(254,:)=(/ "ATMW", "Adiabatic tendency of meridional wind", "m/s" /)
ecmwf128(255,:)=(/ "var255", "missing value", "" /)

first=.false.
end if

if (ain.GT.size(ecmwf128,DIM=1)) Then
  elemtxt=elemerr
else
  elemtxt=ecmwf128(ain,:)
end If

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ECMWF Metadata 140
!

subroutine gecmwf140(ain,elemtxt)

implicit none

Integer, intent(in) :: ain
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80 elemerr(1:3)
Character*80, save :: ecmwf140(0:255,1:3)
logical, save :: first=.true.

elemerr='???'

if (first) then

ecmwf140=""
ecmwf140(220,:)=(/ "MP1", "Mean wave period based on first moment", "s" /)
ecmwf140(221,:)=(/ "MP2", "Mean wave period based on second moment", "s" /)
ecmwf140(222,:)=(/ "WDW", "Wave spectral directional width", "" /)
ecmwf140(223,:)=(/ "P1WW", "Mean wave period based on first moment for wind waves", "s" /)
ecmwf140(224,:)=(/ "P2WW", "Mean wave period based on second moment for wind waves", "s" /)
ecmwf140(225,:)=(/ "DWWW", "Wave spectral directional width for wind waves", "" /)
ecmwf140(226,:)=(/ "P1PS", "Mean wave period based on first moment for swell", "s" /)
ecmwf140(227,:)=(/ "P2PS", "Mean wave period based on second moment for swell", "s" /)
ecmwf140(228,:)=(/ "DWPS", "Wave spectral directional width for swell", "" /)
ecmwf140(229,:)=(/ "SWH", "Significant wave height", "m" /)
ecmwf140(230,:)=(/ "MWD", "Mean wave direction", "deg" /)
ecmwf140(231,:)=(/ "PP1D", "Peak period of 1D spectra", "s" /)
ecmwf140(232,:)=(/ "MWP", "Mean wave period", "s" /)
ecmwf140(233,:)=(/ "CDWW", "Coefficient of drag with waves", "" /)
ecmwf140(234,:)=(/ "SHWW", "Significant height of wind waves", "m" /)
ecmwf140(235,:)=(/ "MDWW", "Mean direction of wind waves", "deg" /)
ecmwf140(236,:)=(/ "MPWW", "Mean period of wind waves", "s" /)
ecmwf140(237,:)=(/ "SHPS", "Significant height of primary swell", "m" /)
ecmwf140(238,:)=(/ "MDPS", "Mean direction of primary swell", "deg" /)
ecmwf140(239,:)=(/ "MPPS", "Mean period of primary swell", "s" /)
ecmwf140(240,:)=(/ "SDHS", "Standard deviation wave height", "m" /)
ecmwf140(241,:)=(/ "MU10", "Mean of 10 metre windspeed", "m/s" /)
ecmwf140(242,:)=(/ "MDWI", "Mean wind direction", "deg" /)
ecmwf140(243,:)=(/ "SDU", "Standard deviation of 10 metre wind speed", "m/s" /)
ecmwf140(244,:)=(/ "MSQS", "Mean square slope of waves", "none" /)
ecmwf140(245,:)=(/ "WIND", "10 metre wind speed", "m/s" /)
ecmwf140(246,:)=(/ "AWH", "Altimeter wave height", "m" /)
ecmwf140(247,:)=(/ "ACWH", "Altimeter corrected wave height", "m" /)
ecmwf140(248,:)=(/ "ARRC", "Altimeter range relative correction", "" /)
ecmwf140(249,:)=(/ "DWI", "10 metre wind direction", "deg" /)
ecmwf140(250,:)=(/ "2DSP", "2D wave spectra (multiple)", "m^2 s" /)
ecmwf140(251,:)=(/ "2DFD", "2D wave spectra (single)", "m^2 s" /)

first=.false.
end if

if (ain.GT.size(ecmwf140,DIM=1)) Then
  elemtxt=elemerr
else
  elemtxt=ecmwf140(ain,:)
end If

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ECMWF Metadata 128
!

subroutine gecmwf210(ain,elemtxt)

implicit none

Integer, intent(in) :: ain
Character*80, dimension(1:3), intent(out) :: elemtxt
Character*80 elemerr(1:3)
Character*80, save :: ecmwf210(0:255,1:3)
logical, save :: first=.true.

elemerr='???'

if (first) then

ecmwf210=""
ecmwf210(1,:)=(/ "AERMR01", "Sea Salt Aerosol (0.03-0.5 um) Mixing Ratio", "kg/kg" /)
ecmwf210(2,:)=(/ "AERMR02", "Sea Salt Aerosol (0.5-5 um) Mixing Ratio", "kg/kg" /)
ecmwf210(3,:)=(/ "AERMR03", "Sea Salt Aerosol (5-20 um) Mixing Ratio", "kg/kg" /)
ecmwf210(4,:)=(/ "AERMR04", "Dust Aerosol (0.03-0.55 um) Mixing Ratio", "kg/kg" /)
ecmwf210(5,:)=(/ "AERMR05", "Dust Aerosol (0.55-0.9 um) Mixing Ratio", "kg/kg" /)
ecmwf210(6,:)=(/ "AERMR06", "Dust Aerosol (0.9-20 um) Mixing Ratio", "kg/kg" /)
ecmwf210(7,:)=(/ "AERMR07", "Hydrophobic Organic Matter Aerosol Mixing Ratio", "kg/kg" /)
ecmwf210(8,:)=(/ "AERMR08", "Hydrophilic Organic Matter Aerosol Mixing Ratio", "kg/kg" /)
ecmwf210(9,:)=(/ "AERMR09", "Hydrophobic Black Carbon Aerosol Mixing Ratio", "kg/kg" /)
ecmwf210(10,:)=(/ "AERMR10", "Hydrophilic Black Carbon Aerosol Mixing Ratio", "kg/kg" /)
ecmwf210(11,:)=(/ "AERMR11", "Sulphate Aerosol Mixing Ratio", "kg/kg" /)
ecmwf210(12,:)=(/ "AERMR12", "Aerosol Type 12 Mixing Ratio", "kg/kg" /)
ecmwf210(16,:)=(/ "AERGN01", "Aerosol type 1 source/gain accumulated", "kg/m^2" /)
ecmwf210(17,:)=(/ "AERGN02", "Aerosol type 2 source/gain accumulated", "kg/m^2" /)
ecmwf210(18,:)=(/ "AERGN03", "Aerosol type 3 source/gain accumulated", "kg/m^2" /)
ecmwf210(19,:)=(/ "AERGN04", "Aerosol type 4 source/gain accumulated", "kg/m^2" /)
ecmwf210(20,:)=(/ "AERGN05", "Aerosol type 5 source/gain accumulated", "kg/m^2" /)
ecmwf210(21,:)=(/ "AERGN06", "Aerosol type 6 source/gain accumulated", "kg/m^2" /)
ecmwf210(22,:)=(/ "AERGN07", "Aerosol type 7 source/gain accumulated", "kg/m^2" /)
ecmwf210(23,:)=(/ "AERGN08", "Aerosol type 8 source/gain accumulated", "kg/m^2" /)
ecmwf210(24,:)=(/ "AERGN09", "Aerosol type 9 source/gain accumulated", "kg/m^2" /)
ecmwf210(25,:)=(/ "AERGN10", "Aerosol type 10 source/gain accumulated", "kg/m^2" /)
ecmwf210(26,:)=(/ "AERGN11", "Aerosol type 11 source/gain accumulated", "kg/m^2" /)
ecmwf210(27,:)=(/ "AERGN12", "Aerosol type 12 source/gain accumulated", "kg/m^2" /)
ecmwf210(31,:)=(/ "AERLS01", "Aerosol type 1 sink/loss accumulated", "kg/m^2" /)
ecmwf210(32,:)=(/ "AERLS02", "Aerosol type 2 sink/loss accumulated", "kg/m^2" /)
ecmwf210(33,:)=(/ "AERLS03", "Aerosol type 3 sink/loss accumulated", "kg/m^2" /)
ecmwf210(34,:)=(/ "AERLS04", "Aerosol type 4 sink/loss accumulated", "kg/m^2" /)
ecmwf210(35,:)=(/ "AERLS05", "Aerosol type 5 sink/loss accumulated", "kg/m^2" /)
ecmwf210(36,:)=(/ "AERLS06", "Aerosol type 6 sink/loss accumulated", "kg/m^2" /)
ecmwf210(37,:)=(/ "AERLS07", "Aerosol type 7 sink/loss accumulated", "kg/m^2" /)
ecmwf210(38,:)=(/ "AERLS08", "Aerosol type 8 sink/loss accumulated", "kg/m^2" /)
ecmwf210(39,:)=(/ "AERLS09", "Aerosol type 9 sink/loss accumulated", "kg/m^2" /)
ecmwf210(40,:)=(/ "AERLS10", "Aerosol type 10 sink/loss accumulated", "kg/m^2" /)
ecmwf210(41,:)=(/ "AERLS11", "Aerosol type 11 sink/loss accumulated", "kg/m^2" /)
ecmwf210(42,:)=(/ "AERLS12", "Aerosol type 12 sink/loss accumulated", "kg/m^2" /)
ecmwf210(46,:)=(/ "AERPR", "Aerosol precursor mixing ratio", "kg/kg" /)
ecmwf210(47,:)=(/ "AERSM", "Aerosol small mode mixing ratio", "kg/kg" /)
ecmwf210(48,:)=(/ "AERLG", "Aerosol large mode mixing ratio", "kg/kg" /)
ecmwf210(49,:)=(/ "AODPR", "Aerosol precursor optical depth", "none" /)
ecmwf210(50,:)=(/ "AODSM", "Aerosol small mode optical depth", "none" /)
ecmwf210(51,:)=(/ "AODLG", "Aerosol large mode optical depth", "none" /)
ecmwf210(52,:)=(/ "AERDEP", "Dust emission potential", "kg s^2/m^5" /)
ecmwf210(53,:)=(/ "AERLTS", "Lifting threshold speed", "m/s" /)
ecmwf210(54,:)=(/ "AERSCC", "Soil clay content", "%" /)
ecmwf210(61,:)=(/ "CO2", "Carbon Dioxide", "kg/kg" /)
ecmwf210(62,:)=(/ "CH4", "Methane", "kg/kg" /)
ecmwf210(63,:)=(/ "N20", "Nitrous oxide", "kg/kg" /)
ecmwf210(64,:)=(/ "TCCO2", "Total column Carbon Dioxide", "kg/m^2" /)
ecmwf210(65,:)=(/ "TCCH4", "Total column Methane", "kg/m^2" /)
ecmwf210(66,:)=(/ "TCN2O", "Total column Nitrous oxide", "kg/m^2" /)
ecmwf210(67,:)=(/ "CO2OF", "Ocean flux of Carbon Dioxide", "kg/m^2/s" /)
ecmwf210(68,:)=(/ "CO2NBF", "Natural biosphere flux of Carbon Dioxide", "kg/m^2/s" /)
ecmwf210(69,:)=(/ "CO2APF", "Anthropogenic emissions of Carbon Dioxide", "kg/m^2/s" /)
ecmwf210(70,:)=(/ "CH4F", "Methane suface fluxes", "kg/m^2/s" /)
ecmwf210(71,:)=(/ "kCH4", "Methane loss rate due to radical hydroxyl(OH)", "1/s" /)
ecmwf210(80,:)=(/ "CO2FIRE", "Wildfire flux of Carbon Dioxide", "kg/m^2/s" /)
ecmwf210(81,:)=(/ "COFIRE", "Wildfire flux of Carbon Monoxide", "kg/m^2/s" /)
ecmwf210(82,:)=(/ "CH4FIRE", "Wildfire flux of Methane", "kg/m^2/s" /)
ecmwf210(83,:)=(/ "NMHCFIRE", "Wildfire flux of Non-Methane Hydro-Carbons", "kg/m^2/s" /)
ecmwf210(84,:)=(/ "H2FIRE", "Wildfire flux of Hydrogen", "kg/m^2/s" /)
ecmwf210(85,:)=(/ "NOXFIRE", "Wildfire flux of Nitrogen Oxides NOx", "kg/m^2/s" /)
ecmwf210(86,:)=(/ "N2OFIRE", "Wildfire flux of Nitrous Oxide", "kg/m^2/s" /)
ecmwf210(87,:)=(/ "PM2P5FIRE", "Wildfire flux of Particulate Matter PM2.5", "kg/m^2/s" /)
ecmwf210(88,:)=(/ "TPMFIRE", "Wildfire flux of Total Particulate Matter", "kg/m^2/s" /)
ecmwf210(89,:)=(/ "TCFIRE", "Wildfire flux of Total Carbon in Aerosols", "kg/m^2/s" /)
ecmwf210(90,:)=(/ "OCFIRE", "Wildfire flux of Organic Carbon", "kg/m^2/s" /)
ecmwf210(91,:)=(/ "BCFIRE", "Wildfire flux of Black Carbon", "kg/m^2/s" /)
ecmwf210(92,:)=(/ "CFIRE", "Wildfire overall flux of burnt Carbon", "kg/m^2/s" /)
ecmwf210(93,:)=(/ "C4FFIRE", "Wildfire fraction of C4 plants", "none" /)
ecmwf210(94,:)=(/ "VEGFIRE", "Wildfire vegetation map index", "none" /)
ecmwf210(95,:)=(/ "CCFIRE", "Wildfire Combustion Completeness", "none" /)
ecmwf210(96,:)=(/ "FLFIRE", "Wildfire fuel load: Carbon per unit area", "kg/m^2" /)
ecmwf210(97,:)=(/ "BFFIRE", "Wildfire fraction of area burnt", "none" /)
ecmwf210(98,:)=(/ "OAFIRE", "Wildfire observed area", "m^2" /)
ecmwf210(99,:)=(/ "FRPFIRE", "Wildfire radiative power", "W/m^2" /)
ecmwf210(100,:)=(/ "CRFIRE", "Wildfire combusion rate", "kg/m^2/s" /)
ecmwf210(121,:)=(/ "NO2", "Nitrogen dioxide", "kg/kg" /)
ecmwf210(122,:)=(/ "SO2", "Sulphur dioxide", "kg/kg" /)
ecmwf210(123,:)=(/ "CO", "Carbon monoxide", "kg/kg" /)
ecmwf210(124,:)=(/ "HCHO", "Formaldehyde", "kg/kg" /)
ecmwf210(125,:)=(/ "TCNO2", "Total column Nitrogen dioxide", "kg/m^2" /)
ecmwf210(126,:)=(/ "TCSO2", "Total column Sulphur dioxide", "kg/m^2" /)
ecmwf210(127,:)=(/ "TCCO", "Total column Carbon monoxide", "kg/m^2" /)
ecmwf210(128,:)=(/ "TCHCHO", "Total column Formaldehyde", "kg/m^2" /)
ecmwf210(129,:)=(/ "NOX", "Nitrogen Oxides", "kg/kg" /)
ecmwf210(130,:)=(/ "TCNOX", "Total column Nitrogen Oxides", "kg/m^2" /)
ecmwf210(131,:)=(/ "GRG1", "Reactive tracer 1 mass mixing ratio", "kg/kg" /)
ecmwf210(132,:)=(/ "TCGRG1", "Total column GRG tracer 1", "kg/m^2" /)
ecmwf210(133,:)=(/ "GRG2", "Reactive tracer 2 mass mixing ratio", "kg/kg" /)
ecmwf210(134,:)=(/ "TCGRG2", "Total column GRG tracer 2", "kg/m^2" /)
ecmwf210(135,:)=(/ "GRG3", "Reactive tracer 3 mass mixing ratio", "kg/kg" /)
ecmwf210(136,:)=(/ "TCGRG3", "Total column GRG tracer 3", "kg/m^2" /)
ecmwf210(137,:)=(/ "GRG4", "Reactive tracer 4 mass mixing ratio", "kg/kg" /)
ecmwf210(138,:)=(/ "TCGRG4", "Total column GRG tracer 4", "kg/m^2" /)
ecmwf210(139,:)=(/ "GRG5", "Reactive tracer 5 mass mixing ratio", "kg/kg" /)
ecmwf210(140,:)=(/ "TCGRG5", "Total column GRG tracer 5", "kg/m^2" /)
ecmwf210(141,:)=(/ "GRG6", "Reactive tracer 6 mass mixing ratio", "kg/kg" /)
ecmwf210(142,:)=(/ "TCGRG6", "Total column GRG tracer 6", "kg/m^2" /)
ecmwf210(143,:)=(/ "GRG7", "Reactive tracer 7 mass mixing ratio", "kg/kg" /)
ecmwf210(144,:)=(/ "TCGRG7", "Total column GRG tracer 7", "kg/m^2" /)
ecmwf210(145,:)=(/ "GRG8", "Reactive tracer 8 mass mixing ratio", "kg/kg" /)
ecmwf210(146,:)=(/ "TCGRG8", "Total column GRG tracer 8", "kg/m^2" /)
ecmwf210(147,:)=(/ "GRG9", "Reactive tracer 9 mass mixing ratio", "kg/kg" /)
ecmwf210(148,:)=(/ "TCGRG9", "Total column GRG tracer 9", "kg/m^2" /)
ecmwf210(149,:)=(/ "GRG10", "Reactive tracer 10 mass mixing ratio", "kg/kg" /)
ecmwf210(150,:)=(/ "TCGRG10", "Total column GRG tracer 10", "kg/m^2" /)
ecmwf210(151,:)=(/ "SFNOX", "Surface flux Nitrogen oxides", "kg/m^2/s" /)
ecmwf210(152,:)=(/ "SFNO2", "Surface flux Nitrogen dioxide", "kg/m^2/s" /)
ecmwf210(153,:)=(/ "SFSO2", "Surface flux Sulphur dioxide", "kg/m^2/s" /)
ecmwf210(154,:)=(/ "SFCO2", "Surface flux Carbon monoxide", "kg/m^2/s" /)
ecmwf210(155,:)=(/ "SFHCHO", "Surface flux Formaldehyde", "kg/m^2/s" /)
ecmwf210(156,:)=(/ "SFGO3", "Surface flux GEMS ozone", "kg/m^2/s" /)
ecmwf210(157,:)=(/ "SFGR1", "Surface flux reactive tracer 1", "kg/m^2/s" /)
ecmwf210(158,:)=(/ "SFGR2", "Surface flux reactive tracer 2", "kg/m^2/s" /)
ecmwf210(159,:)=(/ "SFGR3", "Surface flux reactive tracer 3", "kg/m^2/s" /)
ecmwf210(160,:)=(/ "SFGR4", "Surface flux reactive tracer 4", "kg/m^2/s" /)
ecmwf210(161,:)=(/ "SFGR5", "Surface flux reactive tracer 5", "kg/m^2/s" /)
ecmwf210(162,:)=(/ "SFGR6", "Surface flux reactive tracer 6", "kg/m^2/s" /)
ecmwf210(163,:)=(/ "SFGR7", "Surface flux reactive tracer 7", "kg/m^2/s" /)
ecmwf210(164,:)=(/ "SFGR8", "Surface flux reactive tracer 8", "kg/m^2/s" /)
ecmwf210(165,:)=(/ "SFGR9", "Surface flux reactive tracer 9", "kg/m^2/s" /)
ecmwf210(166,:)=(/ "SFGR10", "Surface flux reactive tracer 10", "kg/m^2/s" /)
ecmwf210(181,:)=(/ "Ra", "Radon", "kg/kg" /)
ecmwf210(182,:)=(/ "SF6", "Sulphur Hexafluoride", "kg/kg" /)
ecmwf210(183,:)=(/ "TCRa", "Total column of Radon", "kg/m^2" /)
ecmwf210(184,:)=(/ "TCSF6", "Total column of Sulphur Hexafluoride", "kg/m^2" /)
ecmwf210(185,:)=(/ "SF6APF", "Anthropogenic Emissions of Sulphur Hexafluoride", "kg/m^2/s" /)
ecmwf210(203,:)=(/ "GO3", "GEMS ozone", "kg/kg" /)
ecmwf210(206,:)=(/ "GTCO3", "GEMS Total column ozone", "kg/m^2" /)
ecmwf210(207,:)=(/ "AOD550", "Total Aerosol Optical Depth at 550nm", "none" /)
ecmwf210(208,:)=(/ "SSAOD550", "Sea Salt Aerosol Optical Depth at 550nm", "none" /)
ecmwf210(209,:)=(/ "DUAOD550", "Dust Aerosol Optical Depth at 550nm", "none" /)
ecmwf210(210,:)=(/ "OMAOD550", "Organic Matter Aerosol Optical Depth at 550nm", "none" /)
ecmwf210(211,:)=(/ "BCAOD550", "Black Carbon Aerosol Optical Depth at 550nm", "none" /)
ecmwf210(212,:)=(/ "SUAOD550", "Sulphate Aerosol Optical Depth at 550nm", "none" /)
ecmwf210(213,:)=(/ "AOD469", "Total Aerosol Optical Depth at 469nm", "none" /)
ecmwf210(214,:)=(/ "AOD670", "Total Aerosol Optical Depth at 670nm", "none" /)
ecmwf210(215,:)=(/ "AOD865", "Total Aerosol Optical Depth at 865nm", "none" /)
ecmwf210(216,:)=(/ "AOD1240", "Total Aerosol Optical Depth at 1240nm", "none" /)

first=.false.
end if

if (ain.GT.size(ecmwf210,DIM=1)) Then
  elemtxt=elemerr
else
  elemtxt=ecmwf210(ain,:)
end If

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
Character*80, save :: meteotemp(0:15,1:3)
Character*80, save :: meteomoist(0:32,1:3)
Character*80, save :: meteomoment(0:24,1:3)
Character*80, save :: meteomass(0:14,1:3)
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
Character*80, save :: spaceimage(0:7,1:3)
Character*80, save :: spacequantitative(0:0,1:3)
Character*80, save :: oceanwaves(0:13,1:3)
Character*80, save :: oceancurrents(0:3,1:3)
Character*80, save :: oceanice(0:7,1:3)
Character*80, save :: oceansurface(0:1,1:3)
Character*80, save :: oceansubsurface(0:3,1:3)
Character*80, save :: nceplcltable000000(192:192,1:3)
Character*80, save :: nceplcltable000001(192:205,1:3)
Character*80, save :: nceplcltable000002(192:197,1:3)
Character*80, save :: nceplcltable000003(192:197,1:3)
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

meteotemp(0,:) =  (/ 'TMP',       'Temperature',                            'K' /)
meteotemp(1,:) =  (/ 'VTMP',      'Virtual temperature',                    'K' /)
meteotemp(2,:) =  (/ 'POT',       'Potential temperature',                  'K' /)
meteotemp(3,:) =  (/ 'EPOT',      'Pseudo-adiabatic potential temperature', 'K' /)
meteotemp(4,:) =  (/ 'TMAX',      'Maximum temperature',                    'K' /)
meteotemp(5,:) =  (/ 'TMIN',      'Minimum temperature',                    'K' /)
meteotemp(6,:) =  (/ 'DPT',       'Dew point temperature',                  'K' /)
meteotemp(7,:) =  (/ 'DEPR',      'Dew point depression',                   'K' /)
meteotemp(8,:) =  (/ 'LAPR',      'Lapse rate',                             'K' /)
meteotemp(9,:) =  (/ 'TMPA',      'Temperature anomaly',                    'K' /)
meteotemp(10,:) = (/ 'LHTFL',     'Latent heat net flux',                   'W/(m^2)' /)
meteotemp(11,:) = (/ 'SHTFL',     'Sensible heat net flux',                 'K' /)
meteotemp(12,:) = (/ 'HeatIndex', 'Heat index',                             'K' /)
meteotemp(13,:) = (/ 'WCI',       'Wind chill factor',                      'K' /)
meteotemp(14,:) = (/ '',          'Minimum dew point depression',           'K' /)
meteotemp(15,:) = (/ 'VPTMP',     'Virtual potential temperature',          'K' /)

meteomoist(0,:) =  (/ 'SPFH',  'Specific humidity',               'kg/kg' /)
meteomoist(1,:) =  (/ 'RH',    'Relative humidity',               '%' /)
meteomoist(2,:) =  (/ 'MIXR',  'Humidity mixing ratio',           'kg/kg' /)
meteomoist(3,:) =  (/ 'PWAT',  'Precipitable water',              'kg/(m^2)' /)
meteomoist(4,:) =  (/ 'VAPP',  'Vapor pressure',                  'Pa' /)
meteomoist(5,:) =  (/ 'SATD',  'Saturation deficit',              'Pa' /)
meteomoist(6,:) =  (/ 'EVP',   'Evaporation',                     'kg/(m^2)' /)
meteomoist(7,:) =  (/ 'PRATE', 'Precipitation rate',              'kg/(m^2 s)' /)
meteomoist(8,:) =  (/ 'APCP',  'Total precipitation',             'kg/(m^2)' /)
meteomoist(9,:) =  (/ 'NCPCP', 'Large scale precipitation',       'kg/(m^2)' /)
meteomoist(10,:) = (/ 'ACPCP', 'Convective precipitation',        'kg/(m^2)' /)
meteomoist(11,:) = (/ 'SNOD',  'Snow depth',                      'm' /)
meteomoist(12,:) = (/ 'SRWEQ', 'Snowfall rare water equivalent',  'kg/(m^2)' /)
meteomoist(13,:) = (/ 'WEASD', 'Water equivalent of accumulated snow depth', 'kg/(m^2)' /)
meteomoist(14,:) = (/ 'SNOC',  'Convective snow',                 'kg/(m^2)' /)
meteomoist(15,:) = (/ 'SNOL',  'Large scale snow',                'kg/(m^2)' /)
meteomoist(16,:) = (/ 'SNOM',  'Snow melt',                       'kg/(m^2)' /)
meteomoist(17,:) = (/ '',      'Snow age',                        'day' /)
meteomoist(18,:) = (/ '',      'Absolute humidity',               'kg/(m^3)' /)
meteomoist(19,:) = (/ '',      'Precipitation type',              '(1 Rain, 2 Thunderstorm, 3 Freezing rain, 4 Mixed/ice, 5 Snow, 255 Missing)' /)
meteomoist(20,:) = (/ '',      'Integrated liquid water',         'km/(m^2)' /)
meteomoist(21,:) = (/ '',      'Condensate',                      'kg/kg' /)
meteomoist(22,:) = (/ 'CLWMR', 'Cloud water mixing ratio',        'kg/kg' /)
meteomoist(23,:) = (/ '',      'Ice water mixing ratio',          'kg/kg' /)
meteomoist(24,:) = (/ 'RWMR',  'Rain water mixing ratio',         'kg/kg' /)
meteomoist(25,:) = (/ 'SNMR',  'Snow water mixing ratio',         'kg/kg' /)
meteomoist(26,:) = (/ '',      'Horizontal moisture convergence', 'kg/(kg s)' /)
meteomoist(27,:) = (/ '',      'Maximum relative humidity',       '%' /)
meteomoist(28,:) = (/ '',      'Maximum absolute humidity',       'kg/(m^3)' /)
meteomoist(29,:) = (/ 'SnowAmt', 'Total snowfall',                'm' /)
meteomoist(30,:) = (/ '',      'Precipitable water category',     '(undefined)' /)
meteomoist(31,:) = (/ '',      'Hail',                            'm' /)
meteomoist(32,:) = (/ '',      'Graupel (snow pellets)',          'kg/kg' /)

meteomoment(0,:) =  (/ 'WDIR',    'Wind direction (from which blowing)', 'deg true' /)
meteomoment(1,:) =  (/ 'WIND',    'Wind speed',                          'm/s' /)
meteomoment(2,:) =  (/ 'UGRD',    'u-component of wind',                 'm/s' /)
meteomoment(3,:) =  (/ 'VGRD',    'v-component of wind',                 'm/s' /)
meteomoment(4,:) =  (/ 'STRM',    'Stream function',                     '(m^2)/s' /)
meteomoment(5,:) =  (/ 'VPOT',    'Velocity potential',                  '(m^2)/s' /)
meteomoment(6,:) =  (/ 'MNTSF',   'Montgomery stream function',          '(m^2)/(s^2)' /)
meteomoment(7,:) =  (/ 'SGCVV',   'Sigma coordinate vertical velocity',  '1/s' /)
meteomoment(8,:) =  (/ 'VVEL',    'Vertical velocity (pressure)',        'Pa/s' /)
meteomoment(9,:) =  (/ 'DZDT',    'Vertical velocity (geometric)',       'm/s' /)
meteomoment(10,:) = (/ 'ABSV',    'Absolute vorticity',                  '1/s' /)
meteomoment(11,:) = (/ 'ABSD',    'Absolute duvergence',                 '1/s' /)
meteomoment(12,:) = (/ 'RELV',    'Relative vorticity',                  '1/s' /)
meteomoment(13,:) = (/ 'RELD',    'Relative divergence',                 '1/s' /)
meteomoment(14,:) = (/ 'PVORT',   'Potential vorticity',                 'K(m^2)/(kg s)' /)
meteomoment(15,:) = (/ 'VUCSH',   'Vertical u-component shear',          '1/s' /)
meteomoment(16,:) = (/ 'VVCSH',   'Vertical v-component shear',          '1/s' /)
meteomoment(17,:) = (/ 'UFLX',    'Momentum flux, u component',          'N/(m^2)' /)
meteomoment(18,:) = (/ 'VFLX',    'Momentum flux, v component',          'N/(m^2)' /)
meteomoment(19,:) = (/ 'WMIXE',   'Wind mixing energy',                  'J' /)
meteomoment(20,:) = (/ 'BLYDP',   'Boundary layer dissipation',          'W/(m^2)' /)
meteomoment(21,:) = (/ 'MAXGUST', 'Maximum wind speed',                  'm/s' /)
meteomoment(22,:) = (/ 'GUST',    'Wind speed (gust)',                   'm/s' /)
meteomoment(23,:) = (/ 'UGUST',   'u-component of wind (gust)',          'm/s' /)
meteomoment(24,:) = (/ 'VGUST',   'v-component of wind (gust)',          'm/s' /)

meteomass(0,:) =  (/ 'PRES',  'Pressure',                                  'Pa' /)
meteomass(1,:) =  (/ 'PRMSL', 'Pressure reduced to MSL',                   'Pa' /)
meteomass(2,:) =  (/ 'PTEND', 'Pressure tendency',                         'Pa/s' /)
meteomass(3,:) =  (/ 'ICAHT', 'ICA0 standard atmosphere reference height', 'm' /)
meteomass(4,:) =  (/ 'GP',    'Geopotential',                              '(m^2)/(s^2)' /)
meteomass(5,:) =  (/ 'HGT',   'Geopotential height',                       'gpm' /)
meteomass(6,:) =  (/ 'DIST',  'Geometric height',                          'm' /)
meteomass(7,:) =  (/ 'HSTDV', 'Standard deviation of height',              'm' /)
meteomass(8,:) =  (/ 'PRESA', 'Pressure anomaly',                          'Pa' /)
meteomass(9,:) =  (/ 'GPA',   'Geopotential height anomally',              'gpm' /)
meteomass(10,:) = (/ 'DEN',   'Density',                                   'kg/(m^3)' /)
meteomass(11,:) = (/ '',      'Altimeter setting',                         'Pa' /)
meteomass(12,:) = (/ '',      'Thickness',                                 'm' /)
meteomass(13,:) = (/ '',      'Pressure altitude',                         'm' /)
meteomass(14,:) = (/ '',      'Density altitude',                          'm' /)

meteoshortradiate(0,:)=(/ 'NSWRS', 'Net short-wave radiation flux (surface)', 'W/(m^2)' /)
meteoshortradiate(1,:)=(/ 'NSWRT', 'Net short-wave radiation flux (top of atmosphere)', 'W/(m^2)' /)
meteoshortradiate(2,:)=(/ 'SWAVR', 'Short wave radiation flux', 'W/(m^2)' /)
meteoshortradiate(3,:)=(/ 'GRAD', 'Global radiation flux', 'W/(m^2)' /)
meteoshortradiate(4,:)=(/ 'BRTMP', 'Brightness temperature', 'K' /)
meteoshortradiate(5,:)=(/ 'LWRAD', 'Radiance (with respect to wave number)', 'W/(m sr)' /)
meteoshortradiate(6,:)=(/ 'SWARD', 'Radiance (with respect to wave length', 'W/(m^3 sr)' /)

meteolongradiate(0,:)=(/ 'NLWRS', 'Net long wave radiation flux (surface)', 'W/(m^2)' /)
meteolongradiate(1,:)=(/ 'NLWRT', 'Net long wave radiation flux (top of atmosphere)', 'W/(m^2)' /)
meteolongradiate(2,:)=(/ 'LWAVR', 'Long wave radiation flux', 'W/(m^2)' /)

meteocloud=''
meteocloud(0,:)=(/ 'CICE', 'Cloud Ice', 'kg/(m^2)' /)
meteocloud(1,:)=(/ 'TCDC', 'Total could cover', '%' /)
meteocloud(2,:)=(/ 'CDCON', 'Convective cloud cover', '%' /)
meteocloud(3,:)=(/ 'LCDC', 'Low cloud cover', '%' /)
meteocloud(4,:)=(/ 'MCDC', 'Medium cloud cover', '%' /)
meteocloud(5,:)=(/ 'HCDC', 'High cloud cover', '%' /)
meteocloud(6,:)=(/ 'CWAT', 'Could water', 'kg/(m^2)' /)
meteocloud(7,:)=(/ 'CDCA', 'Cloud amount', '%' /)
meteocloud(8,:)=(/ 'CDCT', 'Cloud type', '(0 clear, 1 Cumulonimbus, 2 Stratus, 3 Stratocumulus, 4 Cumulus, 5 Altostratus, 6 Nimbostratus, 7 Altocumulus, 8 Cirrostratus, 9 Cirrocumulus, 10 Cirrus, 11 Cumulonimbus (fog), 12 Stratus (fog), 13 Stratocumulus (fog), 14 Cumulus (fog), Altostratus (fog), 16 Nimbostratus (fog), 17 Altocumulus (fog), 18 Cirrostratus (fog), 19 Cirrocumulus (fog), 20 Cirrus (fog), 191 unknown, 255 missing)' /)
meteocloud(9,:)=(/ 'TMAXT', 'Thunderstorm maximum tops', 'm' /)
meteocloud(10,:)=(/ 'THUNC', 'Thunderstorm coverage', '(0 none, 1 isolated (1%-2%), 2 few (3%-15%), 3 scattered (16%-45%), 4 numerous (>45%), 255 missing)' /)
meteocloud(11,:)=(/ 'CDCB', 'Cloud base', 'm' /)
meteocloud(12,:)=(/ 'CDCT', 'Cloud top', 'm' /)
meteocloud(13,:)=(/ 'CEIL', 'Ceiling', 'm' /)
meteocloud(14,:)=(/ 'CDLYR', 'Non-convective cloud cover', '%' /)
meteocloud(15,:)=(/ 'CWORK', 'Cloud work function', 'J/kg' /)
meteocloud(16,:)=(/ 'CUEFI', 'Convective cloud efficiency', '-' /)
meteocloud(17,:)=(/ 'TCOND', 'Total condensate', 'kg/kg' /)
meteocloud(18,:)=(/ 'TCOLW', 'Total column-integrated cloud water', 'kg/(m^2)' /)
meteocloud(19,:)=(/ 'TCOLI', 'Total column-integrated cloud ice', 'kg/(m^2)' /)
meteocloud(20,:)=(/ 'TCOLC', 'Total column-integrated cloud condensate' ,'kg/(m^2)' /)
meteocloud(21,:)=(/ 'FICE', 'Ice fraction of total condensate', '-' /)
meteocloud(22,:)=(/ 'CDCC', 'Cloud cover', '%' /)
meteocloud(23,:)=(/ 'CDCIMR', 'Cloud ice mixing ratio', 'kg/kg' /)
meteocloud(24,:)=(/ 'SUNS', 'Sunshine', '-' /)
meteocloud(25,:)=(/ 'CBHE', 'Horizontal extent of cumulonibus', '%' /)
meteocloud(33,:)=(/ 'SUNSD', 'Sunshine duration', 's' /)
meteocloud(192,:)=(/ 'CDLYR', 'Non-convective cloud cover', '%' /)
meteocloud(193,:)=(/ 'CWORK', 'Cloud work function', 'J/kg' /)
meteocloud(194,:)=(/ 'CUEFI', 'Convective cloud efficiency', '-' /)
meteocloud(195,:)=(/ 'TCOND', 'Total condensate', 'kg/kg' /)
meteocloud(196,:)=(/ 'TCOLW', 'Total column-integrated cloud water', 'kg/(m^2)' /)
meteocloud(197,:)=(/ 'TCOLI', 'Total column-integrated cloud ice', 'kg/(m^2)' /)
meteocloud(198,:)=(/ 'TCOLC', 'Total column-integrated cloud condensate' ,'kg/(m^2)' /)
meteocloud(199,:)=(/ 'FICE', 'Ice fraction of total condensate', '-' /)
meteocloud(200,:)=(/ 'MFLUX', 'Convective cloud mass flux', 'Pa/s' /)
meteocloud(201,:)=(/ 'SUNSD', 'Sunshine duration', 's' /)

meteostability(0,:)=(/ 'PLI', 'Parcel lifted index (to 500 hPa)', 'K' /)
meteostability(1,:)=(/ 'BLI', 'Best lifted index (to 500 hPa)', 'K' /)
meteostability(2,:)=(/ 'KX', 'K index', 'K' /)
meteostability(3,:)=(/ '', 'K0 index', 'K' /)
meteostability(4,:)=(/ '', 'Total totals index', 'K' /)
meteostability(5,:)=(/ 'SX', 'Sweat index', 'numeric' /)
meteostability(6,:)=(/ 'CAPE', 'Convective available potential energy', 'J/kg' /)
meteostability(7,:)=(/ 'CIN', 'Convective inhibition', 'J/kg' /)
meteostability(8,:)=(/ 'HLCY', 'Storm relative helicity', 'J/kg' /)
meteostability(9,:)=(/ '', 'Energy helicity index', 'numeric' /)

meteoaerosols(0,:)=(/ '', 'Aerosol type', '(0 Aerosol not present, 1 Aerosol present)' /)

meteogases(0,:)=(/ 'TOZNE', 'Total ozone', 'Dobson' /)

meteoradar(0,:)=(/ '', 'Base spectrum width', 'm/s' /)
meteoradar(1,:)=(/ '', 'Base reflectivity', 'dB' /)
meteoradar(2,:)=(/ '', 'Base radial velocity', 'm/s' /)
meteoradar(3,:)=(/ '', 'Vertically-integrated liquid', 'kg/m' /)
meteoradar(4,:)=(/ '', 'Layer-maximum base reflectivity', 'dB' /)
meteoradar(5,:)=(/ '', 'Precipitation', 'kg/(m^2)' /)
meteoradar(6,:)=(/ 'RDSP1', 'Radar spectra (1)', '-' /)
meteoradar(7,:)=(/ 'RDSP2', 'Radar spectra (2)', '-' /)
meteoradar(8,:)=(/ 'RDSP3', 'Radar spectra (3)', '-' /)

meteonuclear(0,:)=(/ '', 'Air concentration of Caesium 137', 'Bq/(m^3)' /)
meteonuclear(1,:)=(/ '', 'Air concentration of Iodine 131', 'Bq/(m^3)' /)
meteonuclear(2,:)=(/ '', 'Air concentration of radioactive pollutant', 'Bq/(m^3)' /)
meteonuclear(3,:)=(/ '', 'Ground deposition of Caesium 137', 'Bq/(m^2)' /)
meteonuclear(4,:)=(/ '', 'Ground deposition of Iodine 131', 'Bq/(m^2)' /)
meteonuclear(5,:)=(/ '', 'Ground deposition of radioactive pollutant', 'Bq/(m^2)' /)
meteonuclear(6,:)=(/ '', 'Time-integrated air concentration of caesium pollutant', '(Bq s)/(m^3)' /)
meteonuclear(7,:)=(/ '', 'Time-integrated air concentration of iodine pollutant', '(Bq s)/(m^3)' /)
meteonuclear(8,:)=(/ '', 'Time-integrated air concentration of radioactive pollutant', '(Bq s)/(m^3)' /)

meteoatmos(0,:)=(/ 'VIS', 'Visibility', 'm' /)
meteoatmos(1,:)=(/ 'ALBDO', 'Albedo', '%' /)
meteoatmos(2,:)=(/ 'TSTM', 'Thunderstorm probability', '%' /)
meteoatmos(3,:)=(/ 'MIXHT', 'Mixed layer depth', 'm' /)
meteoatmos(4,:)=(/ '', 'Volcanic ash', '(0 not present, 1 present, 255 missing)' /)
meteoatmos(5,:)=(/ '', 'Icing top', 'm' /)
meteoatmos(6,:)=(/ '', 'Icing base', 'm' /)
meteoatmos(7,:)=(/ '', 'Icing', '(0 None, 1 light, 2 moderate, 3 severe, 255 missing)' /)
meteoatmos(8,:)=(/ '', 'Turbulance top', 'm' /)
meteoatmos(9,:)=(/ '', 'Turbulence base', 'm' /)
meteoatmos(10,:)=(/ '', 'Turbulence', '(0 None(smooth), 1 light, 2 moderate, 3 severe, 4 extreme, 255 missing)' /)
meteoatmos(11,:)=(/ 'TKE', 'Turbulent kinetic energy', 'J/kg' /)
meteoatmos(12,:)=(/ '', 'Planetary boundary layer regime', '(0 reserved, 1 stable, 2 mechanically driven turbulence, 3 forced convection, 4 free convection, 255 missing)' /)
meteoatmos(13,:)=(/ '', 'Contrail intensity', '(0 Contrail not present, 1 contrail present, 255 missing)' /)
meteoatmos(14,:)=(/ '', 'Contrail engine type', '(0 Low bypass, 1 high bypass, 2 non bypass, 255 missing)' /)
meteoatmos(15,:)=(/ '', 'Contrail top', 'm' /)
meteoatmos(16,:)=(/ '', 'Contrail base', 'm' /)

meteotext(0,:)=(/ '', 'Arbitrary test string', 'CCITTIA5' /)

hydrobasic(0,:)=(/ '', 'Flash flood guidance', 'kg/(m^2)' /)
hydrobasic(1,:)=(/ '', 'Flash flood runoff', 'kg/(m^2)' /)
hydrobasic(2,:)=(/ '', 'Remotely sensed snow cover', '(50 no-snow/no-cloud, 100 clouds, 250 snow, 255 missing)' /)
hydrobasic(3,:)=(/ '', 'Elevation of snow covered terrain', '(0-90 elevation in increments of 100m, 254 clouds, 255 missing)' /)
hydrobasic(4,:)=(/ '', 'Snow water equivalent precent of normal', '%' /)

hydroprob(0,:)=(/ '', 'Conditional percent precipitation amount factile for an overall period', 'kg/(m^2)' /)
hydroprob(1,:)=(/ '', 'Percent precipitation in a sub-period of an overall period', '%' /)
hydroprob(2,:)=(/ '', 'Probability of 0.01 inch of precipitation (POP)', '%' /)

landveg(0,:)=(/ 'LAND', 'Landcover (0=sea 1=land)', 'Proportion' /)
landveg(1,:)=(/ 'SFCR', 'Surface roughness', 'm' /)
landveg(2,:)=(/ 'TSOIL', 'Soil temperature', 'K' /)
landveg(3,:)=(/ 'SOILM', 'Soil moisture content', 'kg/(m^2)' /)
landveg(4,:)=(/ 'VEG', 'Vegetation', '%' /)
landveg(5,:)=(/ 'WATR', 'Water runoff', 'kg/(m^2)' /)
landveg(6,:)=(/ '', 'Evapotranspiration', '1/(kg^2 s)' /)
landveg(7,:)=(/ '', 'Model terrain height', 'm' /)
landveg(8,:)=(/ '', 'Land use', '(1 Urban land, 2 agriculture, 3 range land, 4 deciduous forest, 5 coniferous forest, 6 forest/wetland, 7 water, 8 wetlands, 9 desert, 10 tundra, 11 ice, 12 tropical forest, 13 savannah' /)

landsoil=''
landsoil(0,:)=(/ 'SOTYP', 'Soil type', '(1 Sand, 2 loamy sand, 3 sandy loam, 4 silt loam, 5 orgainc (redefined), 6 sandy clay loam, 7 silt clay loam, 8 clay loam, 9 sandy clay, 10 silty clay, 11 clay)' /)
landsoil(1,:)=(/ 'UPLST', 'Upper layer soil temperature', 'K' /)
landsoil(2,:)=(/ 'UPLSM', 'Upper layer soil moisture', 'kg/(m^3)' /)
landsoil(3,:)=(/ 'LOWLSM', 'Lower layer soil moisture', 'kg/(m^3)' /)
landsoil(4,:)=(/ 'BOTLST', 'Bottom layer soil temperature', 'K' /)
landsoil(5,:)=(/ 'SOILL', 'Liquid volumetric soil moisture', '-' /)
landsoil(6,:)=(/ 'RLYRS', 'Number of soil layers in root zone', '-' /)
landsoil(7,:)=(/ 'SMREF', 'Transpiration stress-onset', '-' /)
landsoil(8,:)=(/ 'SMDRY', 'Direct evaporation cease', '-' /)
landsoil(9,:)=(/ 'POROS', 'Soil porosity' ,'-' /)
landsoil(10,:)=(/ 'LIQVSM', 'Liquid volumetric soil moisture', 'm^3/(m^3)' /)
landsoil(11,:)=(/ 'VOLTSO', 'Volumetric transpiration stree-onset', 'm^3/(m^3)' /)
landsoil(12,:)=(/ 'TRANSO', 'Transpiration stree-onset', 'kg/(m^3)' /)
landsoil(13,:)=(/ 'VOLDEC', 'Volumetric direct evaporation cease', 'm^3/(m^3)' /)
landsoil(14,:)=(/ 'DIREC', 'Direct evaporation cease', 'kg/(m^3)' /)
landsoil(15,:)=(/ 'SOILP', 'Soil porosity', 'm^3/(m^3)' /)
landsoil(16,:)=(/ 'VSOSM', 'Volumetric saturation of soil moisture', 'm^3/(m^3)' /)
landsoil(17,:)=(/ 'SATOSM', 'Saturation of soil moisture', 'kg/(m^3)' /)
landsoil(192,:)=(/ 'SOILL', 'Liquid volumetric soil moisture', '-' /)
landsoil(193,:)=(/ 'RLYRS', 'Number of soil layers in root zone', '-' /)
landsoil(194,:)=(/ 'SLTYP', 'Surface slope type', '-' /)
landsoil(195,:)=(/ 'SMREF', 'Transpiration stress-onset', '-' /)
landsoil(196,:)=(/ 'SMDRY', 'Direct evaporation cease', '-' /)
landsoil(197,:)=(/ 'POROS', 'Soil porosity' ,'-' /)
landsoil(198,:)=(/ 'EVBS', 'Direct evaporation from bare soil', 'W/(m^2)' /)
landsoil(199,:)=(/ 'LSPA', 'Land surface precipitation accumulation', 'kg/(m^2)' /)
landsoil(200,:)=(/ 'BARET', 'Bare soil surface skin temperature', 'K' /)
landsoil(201,:)=(/ 'AVSFT', 'Average surface skin temperature', 'K' /)
landsoil(202,:)=(/ 'RADT', 'Effective radiative skin temperature', 'K' /)
landsoil(203,:)=(/ 'FLDCP', 'Field capacity', '-' /)

spaceimage(0,:)=(/ '', 'Scaled radiance', 'numeric' /)
spaceimage(1,:)=(/ '', 'Scaled albedo', 'numeric' /)
spaceimage(2,:)=(/ '', 'Scaled brightness temperature', 'numeric' /)
spaceimage(3,:)=(/ '', 'Scaled precipitable water', 'numeric' /)
spaceimage(4,:)=(/ '', 'Scaled lifted index', 'numeric' /)
spaceimage(5,:)=(/ '', 'Scaled cloud top pressure', 'numeric' /)
spaceimage(6,:)=(/ '', 'Scaled skin temperature', 'numeric' /)
spaceimage(7,:)=(/ '', 'Cloud mask', '(0 Clear over water, 1 clear over land, 2 cloud)' /)

spacequantitative(0,:)=(/ '', 'Estimated precipitation', 'kg/(m^2)' /)

oceanwaves(0,:)=(/ 'WVSP1', 'Wave spectra (1)', '-' /)
oceanwaves(1,:)=(/ 'WVSP2', 'Wave spectra (2)', '-' /)
oceanwaves(2,:)=(/ 'WVSP3', 'Wave spectra (3)', '-' /)
oceanwaves(3,:)=(/ 'HTSGW', 'Significant height of combined wind waves and swell', 'm' /)
oceanwaves(4,:)=(/ 'WVDIR', 'Direction of wind waves', 'Degree true' /)
oceanwaves(5,:)=(/ 'WVHGT', 'Significant height of wind waves', 'm' /)
oceanwaves(6,:)=(/ 'WVPER', 'Mean period of wind waves', 's' /)
oceanwaves(7,:)=(/ 'SWDIR', 'Direction of swell waves', 'Degree true' /)
oceanwaves(8,:)=(/ 'SWELL', 'Significant height of swell waves', 'm' /)
oceanwaves(9,:)=(/ 'SWPER', 'Mean period of swell waves', 's' /)
oceanwaves(10,:)=(/ 'DIRPW', 'Primary wave direction', 'Degree true' /)
oceanwaves(11,:)=(/ 'PERPW', 'Primary wave mean period', 's' /)
oceanwaves(12,:)=(/ 'DIRSW', 'Secondary wave direction', 'Degree true' /)
oceanwaves(13,:)=(/ 'PERSW', 'Secondary wave mean period', 's' /)

oceancurrents(0,:)=(/ 'DIRC', 'Current direction', 'Degree true' /)
oceancurrents(1,:)=(/ 'SPC', 'Current speed', 'm/s' /)
oceancurrents(2,:)=(/ 'U0GRD', 'u-component of current', 'm/s' /)
oceancurrents(3,:)=(/ 'v0GRD', 'v-component of current', 'm/s' /)

oceanice(0,:)=(/ 'ICEC', 'Ice cover', 'Proportion' /)
oceanice(1,:)=(/ 'ICETK', 'Ice thickness', 'm' /)
oceanice(2,:)=(/ 'DICED', 'Direction of ice drift', 'Degree true' /)
oceanice(3,:)=(/ 'SICED', 'Speed of ice drift', 'm/s' /)
oceanice(4,:)=(/ 'UICE', 'u-component of ice drift', 'm/s' /)
oceanice(5,:)=(/ 'VICE', 'v-component of ice drift', 'm/s' /)
oceanice(6,:)=(/ 'ICEG', 'Ice growth rate', 'm/s' /)
oceanice(7,:)=(/ 'ICED', 'Ice divergence', '1/s' /)

oceansurface(0,:)=(/ 'WTMP', 'Water temperature', 'K' /)
oceansurface(1,:)=(/ 'DSLM', 'Deviation of sea level from mean', 'm' /)

oceansubsurface(0,:)=(/ 'MTHD', 'Main thermocline depth', 'm' /)
oceansubsurface(1,:)=(/ 'MTHA', 'Main thermocline anomaly', 'm' /)
oceansubsurface(2,:)=(/ 'TTHDP', 'Transient thermocline depth', 'm' /)
oceansubsurface(3,:)=(/ 'SALTY', 'Salinity', 'kg/kg' /)

nceplcltable000000(192,:)=(/ 'SNOHF', 'Snow phase change heat flux', 'W/(m^2)' /)

nceplcltable000001(192,:)=(/ 'CRAIN', 'Categorical rain', '(0 no, 1 yes)' /)
nceplcltable000001(193,:)=(/ 'CFRZR', 'Categorical freezing rain', '(0 no, 1 yes)' /)
nceplcltable000001(194,:)=(/ 'CICEP', 'Categorical ice pellets', '(0 no, 1 yes)' /)
nceplcltable000001(195,:)=(/ 'CSNOW', 'Categorical snow', '(0 no, 1 yes)' /)
nceplcltable000001(196,:)=(/ 'CPRAT', 'Convective precipitation rate', '(kg s)/(m^2)' /)
nceplcltable000001(197,:)=(/ 'MCONV', 'Horizontal moisture divergence', '(kg s)/(m^2)' /)
nceplcltable000001(198,:)=(/ '', 'Percent frozen precipitation', '-' /)
nceplcltable000001(199,:)=(/ 'PEVAP', 'Potential evaporation', 'kg/(m^2)' /)
nceplcltable000001(200,:)=(/ 'PEVPR', 'Potential evaporation rate', 'W/(m^2)' /)
nceplcltable000001(201,:)=(/ 'SNOWC', 'Snow cover', '%' /)
nceplcltable000001(202,:)=(/ 'FRAIN', 'Rain fraction of total liquid water', '' /)
nceplcltable000001(203,:)=(/ 'FRIME', 'Rime factor', '-' /)
nceplcltable000001(204,:)=(/ 'TCOLR', 'Total column integrated rain', 'kg/(m/m)' /)
nceplcltable000001(205,:)=(/ 'TCOLS', 'Total column integrated snow', 'kg/(m/m)' /)

nceplcltable000002(192,:)=(/ 'VWSH', 'Vertical speed shear', '1/s' /)
nceplcltable000002(193,:)=(/ 'MFLX', 'Horizontal momentum flux', 'N/(m^2)' /)
nceplcltable000002(194,:)=(/ 'USTM', 'u-component storm motion', 'm/s' /)
nceplcltable000002(195,:)=(/ 'VSTM', 'v-component storm motion', 'm/s' /)
nceplcltable000002(196,:)=(/ 'CD', 'Drag coefficient', '-' /)
nceplcltable000002(197,:)=(/ 'FRICV', 'Frictional velocity', 'm/s' /)

nceplcltable000003(192,:) = (/ 'MSLET', 'Mean Sea Level Pressure (Eta reduction)', 'Pa' /)
nceplcltable000003(193,:) = (/ '5WAVH', '5-wave geopotential height',              'gpm' /)
nceplcltable000003(194,:) = (/ 'U-GWD', 'Zonal flux of gravity wave stress',       'N/(m^2)' /)
nceplcltable000003(195,:) = (/ 'V-GWD', 'Meridional flux of gravity wave stress',  'N/(m^2)' /)
nceplcltable000003(196,:) = (/ 'HPBL',  'Planetary boundary layer height',         'm' /)
nceplcltable000003(197,:) = (/ '5WAVA', '5-wave geopotential height anomaly',      'gpm' /)

nceplcltable000004(192,:)=(/ 'DSWRF', 'Downward short-wave rad. flux', 'W/(m^2)' /)
nceplcltable000004(193,:)=(/ 'USWRF', 'Upward short-wave rad. flux', 'W/(m^2)' /)

nceplcltable000005(192,:)=(/ 'DLWRF', 'Downward long-wave rad. flux', 'W/(m^2)' /)
nceplcltable000005(193,:)=(/ 'ULWRF', 'Upward long-wave rad. flux', 'W/(m^2)' /)

nceplcltable000006(192,:)=(/ 'CDLYR', 'Non-convective cloud cover', '%' /)
nceplcltable000006(193,:)=(/ 'CWORK', 'Cloud work function', 'J/kg' /)
nceplcltable000006(194,:)=(/ 'CUEFI', 'Convective cloud efficiency', '-' /)
nceplcltable000006(195,:)=(/ 'TCOND', 'Total condenstate', 'kg/kg' /)
nceplcltable000006(196,:)=(/ 'TCOLW', 'Total column-integrated cloud water', 'kg/(m/m)' /)
nceplcltable000006(197,:)=(/ 'TCOLI', 'Total column-integrated cloud ice', 'kg/(m/m)' /)
nceplcltable000006(198,:)=(/ 'TCOLC', 'Total column-integrated cloud condensate', 'kg/(m/m)' /)
nceplcltable000006(199,:)=(/ 'FICE', 'Ice fraction of total condensate', 'kg/(m/m)' /)

nceplcltable000007(192,:)=(/ 'LFTX', 'Surface lifted index', 'K' /)
nceplcltable000007(193,:)=(/ '4LFTX', 'Best (4 layer) lifted index', 'K' /)
nceplcltable000007(194,:)=(/ 'RI', 'Richardson number', '-' /)

nceplcltable000014(192,:)=(/ '03MR', 'Ozone mixing ratio', 'kg/kg' /)
nceplcltable000014(193,:)=(/ 'OZCON', 'Ozone concentration', 'PPB' /)
nceplcltable000014(194,:)=(/ 'OZCAT', 'Categorical ozone concentration', 'unknown' /)

nceplcltable000019(192,:)=(/ 'MXSALB', 'Maxmium snow albedo', '%' /)
nceplcltable000019(193,:)=(/ 'SNFALB', 'Snow-free albedo', '%' /)

nceplcltable000191(192,:)=(/ 'NLAT', 'Latitude (-90 to 90)', 'deg' /)
nceplcltable000191(193,:)=(/ 'ELON', 'East logitude (0 to 360)', 'deg' /)
nceplcltable000191(194,:)=(/ 'secPriorInitRef', 'Seconds prior to initial reference time', 'sec' /)

nceplcltable001000(192,:)=(/ 'BGRUN', 'Baseflow-groundwater runoff', 'kg/(m^2)' /)
nceplcltable001000(193,:)=(/ 'SSRUN', 'Storm surface runoff', 'kg/(m^2)' /)

nceplcltable002000(192,:)=(/ 'SOILW', 'Volumetric soil moisture content', 'fraction' /)
nceplcltable002000(193,:)=(/ 'GFLUX', 'Ground heat flux', 'W/(m^2)' /)
nceplcltable002000(194,:)=(/ 'MSTAV', 'Moisture avaliability', '%' /)
nceplcltable002000(195,:)=(/ 'SFEXC', 'Exchange coefficient', '(kg/(m^3))(m/s)' /)
nceplcltable002000(196,:)=(/ 'CNWAT', 'Plant canopy surface water', 'kg/(m^2)' /)
nceplcltable002000(197,:)=(/ 'BMIXL', 'Blackadars mixing length scale', 'm' /)
nceplcltable002000(198,:)=(/ 'VGTYP', 'Vegetation type', '0..13' /)
nceplcltable002000(199,:)=(/ 'CCOND', 'Canopy conductance', 'm/s' /)
nceplcltable002000(200,:)=(/ 'RSMIN', 'Minimal stomatal resistance', 's/m' /)
nceplcltable002000(201,:)=(/ 'WILT', 'Wilting point', 'fraction' /)
nceplcltable002000(202,:)=(/ 'RCS', 'Solar parameter in canopy conductance', 'fraction' /)
nceplcltable002000(203,:)=(/ 'RCT', 'Temperature parameter in canopy conductance', 'fraction' /)
nceplcltable002000(204,:)=(/ 'RCQ', 'Humidity parameter in canopy conductance', 'fraction' /)
nceplcltable002000(205,:)=(/ 'RCSOIL', 'Soil moisture parameter in canopy conductance', 'fraction' /)

nceplcltable002003(192,:)=(/ 'SOILL', 'Liquid volumetic soil moisture', 'fraction' /)
nceplcltable002003(193,:)=(/ 'RLYRS', 'Number of soil layers in root zone', '-' /)
nceplcltable002003(194,:)=(/ 'SLTYP', 'Surface slope type', 'Index' /)
nceplcltable002003(195,:)=(/ 'SMREF', 'Transpiration stress-onset (soil moisture)', 'fraction' /)
nceplcltable002003(196,:)=(/ 'SMDRY', 'Direct evaporation cease (soil moisture)', 'fraction' /)
nceplcltable002003(197,:)=(/ 'POROS', 'Soil porosity', 'fraction' /)

nceplcltable003001(192,:)=(/ 'ScatEstUWind', 'Scatterometer estimated u wind', 'unknown' /)
nceplcltable003001(193,:)=(/ 'ScatEstVWind', 'Scatterometer estimated v wind', 'unknown' /)

first=.false.
end if

! Check standard meta data
Select Case(alist(3))

  Case(0)
    Select Case(alist(5))
    
      Case(0)
        If (alist(6).GT.Size(meteotemp,DIM=1)) Then
          elemtxt=elemerr
        Else
          elemtxt=meteotemp(alist(6),:)
        End If
	
      Case(1)
        If (alist(6).GT.Size(meteomoist,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=meteomoist(alist(6),:)
	    End If
	
      Case(2)
        If (alist(6).GT.Size(meteomoment,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=meteomoment(alist(6),:)
	    End If
	
      Case(3)
        If (alist(6).GT.Size(meteomass,DIM=1)) Then
          elemtxt=elemerr
        Else
          elemtxt=meteomass(alist(6),:)
        End If

      Case(4)
        If (alist(6).GT.Size(meteoshortradiate,DIM=1)) Then
          elemtxt=elemerr
        Else
          elemtxt=meteoshortradiate(alist(6),:)
        End If

      Case(5)
        If (alist(6).GT.Size(meteolongradiate,DIM=1)) Then
          elemtxt=elemerr
        Else
          elemtxt=meteolongradiate(alist(6),:)
        End If

      Case(6)
        If (alist(6).GT.Size(meteocloud,DIM=1)) Then
          elemtxt=elemerr
        Else
          elemtxt=meteocloud(alist(6),:)
        End If

      Case(7)
        If (alist(6).GT.Size(meteostability,DIM=1)) Then
          elemtxt=elemerr
        Else
          elemtxt=meteostability(alist(6),:)
        End If

      Case(13)
        If (alist(6).GT.Size(meteoaerosols,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=meteoaerosols(alist(6),:)
	End If

      Case(14)
        If (alist(6).GT.Size(meteogases,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=meteogases(alist(6),:)
	End If

      Case(15)
        If (alist(6).GT.Size(meteoradar,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=meteoradar(alist(6),:)
	End If

      Case(18)
        If (alist(6).GT.Size(meteonuclear,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=meteonuclear(alist(6),:)
	End If

      Case(19)
        If (alist(6).GT.Size(meteoatmos,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=meteoatmos(alist(6),:)
	End If

      Case(190)
        If (alist(6).GT.Size(meteotext,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=meteotext(alist(6),:)
	End If

      Case(253)
        If (alist(6).GT.Size(meteotext,DIM=1)) Then
    	  elemtxt=elemerr
	    Else
          elemtxt=meteotext(alist(6),:)
	    End If
           
      Case DEFAULT
        elemtxt=elemerr
      
    End Select
    
  Case(1)
    Select Case(alist(5))

        Case(0)
        If (alist(6).GT.Size(hydrobasic,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=hydrobasic(alist(6),:)
	    End If

        Case(1)
        If (alist(6).GT.Size(hydroprob,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=hydroprob(alist(6),:)
	    End If
	
    Case DEFAULT
      elemtxt=elemerr
      
    End Select

  Case(2)
    Select Case(alist(5))

        Case(0)
        If (alist(6).GT.Size(landveg,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=landveg(alist(6),:)
	    End If

        Case(3)
        If (alist(6).GT.Size(landsoil,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=landsoil(alist(6),:)
	    End If
	
    Case DEFAULT
      elemtxt=elemerr
      
    End Select

  Case(3)
    Select Case(alist(5))

        Case(0)
        If (alist(6).GT.Size(spaceimage,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=spaceimage(alist(6),:)
	    End If

        Case(1)
        If (alist(6).GT.Size(spacequantitative,DIM=1)) Then
	      elemtxt=elemerr
	    Else
          elemtxt=spacequantitative(alist(6),:)
	    End If
	
    Case DEFAULT
      elemtxt=elemerr
      
    End Select

  Case(10)
    Select Case(alist(5))

        Case(0)
        If (alist(6).GT.Size(oceanwaves,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=oceanwaves(alist(6),:)
	End If

        Case(1)
        If (alist(6).GT.Size(oceancurrents,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=oceancurrents(alist(6),:)
	End If

        Case(2)
        If (alist(6).GT.Size(oceanice,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=oceanice(alist(6),:)
	End If

        Case(3)
        If (alist(6).GT.Size(oceansurface,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=oceansurface(alist(6),:)
	End If

        Case(4)
        If (alist(6).GT.Size(oceansubsurface,DIM=1)) Then
	  elemtxt=elemerr
	Else
          elemtxt=oceansubsurface(alist(6),:)
	End If

	
    Case DEFAULT
      elemtxt=elemerr
      
    End Select
  
  Case DEFAULT
    elemtxt=elemerr

End Select

! Check local meta data
If (elemtxt(1).EQ.'???') Then
  Select Case(alist(3))

    Case(0)
      Select Case(alist(5))

        Case(0)
          If ((alist(6)-192).GT.Size(nceplcltable000000,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000000(alist(6),:)
          End If

        Case(1)
          If ((alist(6)-192).GT.Size(nceplcltable000001,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000001(alist(6),:)
          End If

        Case(2)
          If ((alist(6)-192).GT.Size(nceplcltable000002,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000002(alist(6),:)
          End If
    
        Case(3)
          If ((alist(6)-192).GT.Size(nceplcltable000003,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000003(alist(6),:)
          End If

        Case(4)
          If ((alist(6)-192).GT.Size(nceplcltable000004,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000004(alist(6),:)
          End If

        Case(5)
          If ((alist(6)-192).GT.Size(nceplcltable000005,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000005(alist(6),:)
          End If

        Case(6)
          If ((alist(6)-192).GT.Size(nceplcltable000006,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000006(alist(6),:)
          End If

        Case(7)
          If ((alist(6)-192).GT.Size(nceplcltable000007,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000007(alist(6),:)
          End If

        Case(14)
          If ((alist(6)-192).GT.Size(nceplcltable000014,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000014(alist(6),:)
          End If

        Case(19)
          If ((alist(6)-192).GT.Size(nceplcltable000019,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000019(alist(6),:)
          End If

        Case(191)
          If ((alist(6)-192).GT.Size(nceplcltable000191,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable000191(alist(6),:)
          End If
            
        Case DEFAULT
          elemtxt=elemerr
      
      End Select
      
      Case(1)
      Select Case(alist(5))

        Case(0)
          If ((alist(6)-192).GT.Size(nceplcltable001000,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable001000(alist(6),:)
          End If

        Case DEFAULT
          elemtxt=elemerr
      
      End Select

      Case(2)
      Select Case(alist(5))

        Case(0)
          If ((alist(6)-192).GT.Size(nceplcltable002000,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable002000(alist(6),:)
          End If
	  
        Case(3)
          If ((alist(6)-192).GT.Size(nceplcltable002003,DIM=1)) Then
	        elemtxt=elemerr
          Else
            elemtxt=nceplcltable002003(alist(6),:)
          End If

        Case DEFAULT
          elemtxt=elemerr
      
      End Select

      Case(3)
      Select Case(alist(5))

        Case(1)
          If ((alist(6)-192).GT.Size(nceplcltable003001,DIM=1)) Then
	    elemtxt=elemerr
          Else
            elemtxt=nceplcltable003001(alist(6),:)
          End If

        Case DEFAULT
          elemtxt=elemerr
      
      End Select
    
    Case DEFAULT
      elemtxt=elemerr

  End Select
End If

! Check for errors
If ((elemtxt(1).EQ.'???').OR.(elemtxt(1).EQ.'')) Then
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

Integer, dimension(0:49), intent(in) :: alist
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
    surtxt(:)=(/ "SFC", "sfc", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(2)
    surtxt(:)=(/ "CBL", "cld base", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(3) 
    surtxt(:)=(/ "CTL", "cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(4)
    surtxt(:)=(/ "0DEG", "0C isotherm", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(5)
    surtxt(:)=(/ "ADCL", "cond lev", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(6)
    surtxt(:)=(/ "MWSL", "max wind lev", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(7)
    surtxt(:)=(/ "TRO", "tropopause", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(8)
    surtxt(:)=(/ "NTAT", "nom. top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(9)
    surtxt(:)=(/ "SEAB", "sea bottom", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(10,200)
    surtxt(:)=(/ "reserved", "atmos col", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(12,212)
    surtxt(:)=(/ "reserved", "low cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(13,213)
    surtxt(:)=(/ "reserved", "low cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(14,214)
    surtxt(:)=(/ "reserved", "low cld lay", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(22,222)
    surtxt(:)=(/ "reserved", "mid cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(23,223)
    surtxt(:)=(/ "reserved", "mid cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(24,224)
    surtxt(:)=(/ "reserved", "mid cld lay", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(32,232)
    surtxt(:)=(/ "reserved", "high cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(33,233)
    surtxt(:)=(/ "reserved", "high cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(34,234)
    surtxt(:)=(/ "reserved", "high cld lay", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(201)
    surtxt(:)=(/ "reserved", "ocean column", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(204)
    surtxt(:)=(/ "reserved", "high trop freezing lvl", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(206)
    surtxt(:)=(/ "reserved", "grid-scale cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(207)
    surtxt(:)=(/ "reserved", "grid-scale cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(209)
    surtxt(:)=(/ "reserved", "bndary-layer cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(210)
    surtxt(:)=(/ "reserved", "bndary-layer cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(211)
    surtxt(:)=(/ "reserved", "bndary-layer cld layer", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(242)
    surtxt(:)=(/ "reserved", "convect-cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(243)
    surtxt(:)=(/ "reserved", "convect-cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(244)
    surtxt(:)=(/ "reserved", "convect-cld layer", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(246)
    surtxt(:)=(/ "reserved", "max e-pot-temp lvl", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(247)
    surtxt(:)=(/ "reserved", "equilibrium lvl", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(248)
    surtxt(:)=(/ "reserved", "shallow convect-cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(249)
    surtxt(:)=(/ "reserved", "shallow convect-cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(251)
    surtxt(:)=(/ "reserved", "deep convect-cld bot", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(252)
    surtxt(:)=(/ "reserved", "deep convect-cld top", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(100)
    surtxt(:)=(/ "ISBL", "Pa", "Pa" /)
    alist(13)=0
    surfvalue=ozero*100.
    sndvalue=0.
  Case(101)
    surtxt(:)=(/ "ISBL", "Pa", "Pa" /)
    alist(13)=1
    surfvalue=o11*1000.
    sndvalue=o12*1000.
  Case(102)
    surtxt(:)=(/ "MSL", "MSL", "-" /)
    alist(13)=0
    surfvalue=0.
    sndvalue=0.
  Case(103)
    surtxt(:)=(/ "GPML", "m above MSL", "m" /)
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(104)
    surtxt(:)=(/ "GPML", "m above MSL", "m" /)
    alist(13)=1
    surfvalue=o11*100.
    sndvalue=o12*100.
  Case(105)
    surtxt(:)=(/ "HTGL", "m above gnd", "m" /)
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(106)
    surtxt(:)=(/ "HTGL", "m above gnd", "m" /)
    alist(13)=1
    surfvalue=o11*100.
    sndvalue=o12*100.
  Case(107)
    surtxt(:)=(/ "SIGL", "sigma", "sigma value" /)
    alist(13)=0
    surfvalue=ozero/10000.
    sndvalue=0.
  Case(108)
    surtxt(:)=(/ "SIGL", "sigma", "sigma value" /)
    alist(13)=1
    surfvalue=o11/100.
    sndvalue=o12/100.
  Case(109)
    surtxt(:)=(/ "HYBL", "hybrid lev", "-" /)
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(110)
    surtxt(:)=(/ "HYBL", "hybrid lev", "-" /)
    alist(13)=1
    surfvalue=o11
    sndvalue=o12
  Case(111)
    surtxt(:)=(/ "DBLL", "m down", "m" /)
    alist(13)=0
    surfvalue=ozero*0.01
    sndvalue=0.
  Case(112)
    surtxt(:)=(/ "DBLL", "m down", "m" /)
    alist(13)=1
    surfvalue=o11*0.01
    sndvalue=o12*0.01
  Case(113)
    surtxt(:)=(/ "THEL", "K", "K" /)
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(114)
    surtxt(:)=(/ "THEL", "K", "K" /)
    alist(13)=1
    surfvalue=475.-o11
    sndvalue=475.-o12
  Case(115)
    surtxt(:)=(/ "SPDL", "Pa above gnd", "Pa" /)
    alist(13)=0
    surfvalue=ozero*100.
    sndvalue=0.
  Case(116)
    surtxt(:)=(/ "SPDL", "Pa above gnd", "Pa" /)
    alist(13)=1
    surfvalue=o11*100.
    sndvalue=o12*100.
  Case(117)
    surtxt(:)=(/ "PVL", "pv units", "(K M^2)/(kg s)" /)
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(119)
    surtxt(:)=(/ "EtaL", "(ETA level)", "-" /)
    alist(13)=0
    surfvalue=ozero/10000.
    sndvalue=0.
  Case(120)
    surtxt(:)=(/ "EtaL", "(ETA levels)", "-" /)
    alist(13)=1
    surfvalue=o11/100.
    sndvalue=o12/100.
  Case(121)
    surtxt(:)=(/ "ISBL", "Pa", "Pa" /)
    alist(13)=1
    surfvalue=(1100.-o11)*100.
    sndvalue=(1100.-o12)*100.
  Case(125)
    surtxt(:)=(/ "HTGL", "m above gnd", "m" /)
    alist(13)=0
    surfvalue=ozero*100.
    sndvalue=0.
  Case(126)
    surtxt(:)=(/ "ISBL", "Pa", "Pa" /)
    alist(13)=0
    surfvalue=ozero
    sndvalue=0.
  Case(128)
    surtxt(:)=(/ "SIGL", "(sigma)", "sigma value" /)
    alist(13)=1
    surfvalue=1.1-o11/1000.
    sndvalue=1.1-o12/1000.
  Case(141)
    surtxt(:)=(/ "ISBL", "Pa", "Pa" /)
    alist(13)=1
    surfvalue=o11*1000.
    sndvalue=(1100.-o12)*100.
  Case(160)
    surtxt(:)=(/ "DBSL", "m below sea level", "m" /)
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

surface(1,:) = (/   'SFC',   'Ground or water surface',       '-' /)
surface(2,:) = (/   'CBL',   'Cloud base level',              '-' /)  
surface(3,:) = (/   'CTL',   'Level of cloud tops',           '-' /)
surface(4,:) = (/   '0DEG',  'Level of 0 degreee C isotherm', '-' /)
surface(5,:) = (/   'ADCL',  'Level of adiabatic condensation lifted from the surface', '-' /)
surface(6,:) = (/   'MWSL',  'Maximum wind level',            '-' /)
surface(7,:) = (/   'TRO',   'Tropopause',                    '-' /)
surface(8,:) = (/   'NTAT',  'Nominal top of atmosphere',     '-' /)
surface(9,:) = (/   'SEAB',  'Sea bottom',                    '-' /)
surface(20,:) = (/  'TMPL',  'Isotherm level',                'K' /)
surface(100,:) = (/ 'ISBL',  'Isobaric surface',              'Pa' /)
surface(101,:) = (/ 'MSL',   'Mean sea level',                '-' /)
surface(102,:) = (/ 'GPML',  'Specific altitude above mean sea level', 'm' /)
surface(103,:) = (/ 'HTGL',  'Specific height level above ground', 'm' /)
surface(104,:) = (/ 'SIGL',  'Sigma level',                   'sigma value' /)
surface(105,:) = (/ 'HYBL',  'Hybrid level',                  '-' /)
surface(106,:) = (/ 'DBLL',  'Depth below land surface',      'm' /)
surface(107,:) = (/ 'THEL',  'Isentropic (theta) level',      'K' /)
surface(108,:) = (/ 'SPDL',  'Level at specified pressure difference from ground to level', 'Pa' /)
surface(109,:) = (/ 'PVL',   'Potential vorticity surface',   '(K M^2)/(kg s)' /)
surface(111,:) = (/ 'EtaL',  'Eta* level',                    '-' /)
surface(117,:) = (/ 'unknown', 'Mixed layer depth',           'm' /)
surface(160,:) = (/ 'DBSL',  'Depth below sea level',         'm' /)

first=.false.
end if

! Determine level data

surtxt='Reserved (index=191)'

If (alist(8).LT.1) Then
  ! Undefined
  surfvalue=0
  sndvalue=0
  indx=0
  Return
End If

if (alist(9).ge.0.) then
  surfvalue=Real(alist(10))/Real(10**alist(9))
else
  surfvalue=real(alist(10))*real(10**abs(alist(9)))
end if

If ((alist(8).GT.160).OR.(surface(alist(8),1).EQ.'???')) Then
  indx=191
  Write(surtxt(1),'("Reserved (",I4,")")') alist(8)
Else
  indx=alist(8)
  surtxt(:)=surface(indx,:)
End If

! snd data    
If (alist(13).EQ.1) Then
  if (alist(12).eq.-2147483646) then
    sndvalue=0.
  else
    sndvalue=Real(alist(12))/Real(10**alist(11))
  end if
Else
  sndvalue=0.
End If

! MSL and SFC special case
if (indx.eq.1.or.indx.eq.101) then
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

Integer, dimension(0:49), intent(in) :: alist
Character*80, dimension(1:2), intent(out) :: lvltxt

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

Integer, dimension(0:49), intent(in) :: alist
Character*80, dimension(1:2), intent(out) :: lvltxt
Character*80, dimension(1:3) :: surtxt
Double Precision surfvalue,sndvalue
Integer indx

lvltxt=''

Call getelemlvl1(alist,indx,surfvalue,sndvalue,surtxt)

If (alist(13).EQ.1) Then    
  ! snd data
  If (Int(surfvalue).EQ.surfvalue) Then
    Write(lvltxt(1),'(I7,"-",I3,"-",A4)') Int(surfvalue),Int(sndvalue),surtxt(1)(1:10)
    Write(lvltxt(2),'(I7,"-",I3,"[",A,"] ",A4,"=""",A,"""")') Int(surfvalue),Int(sndvalue),surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
  Else
    Write(lvltxt(1),'(F7.1,"-",F3.1,"-",A4)') surfvalue,sndvalue,surtxt(1)(1:10)
    Write(lvltxt(2),'(F7.1,"-",F3.1,"[",A,"] ",A4,"=""",A,"""")') surfvalue,sndvalue,surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
  End If
Else
  ! no snd data
  If (Int(surfvalue).EQ.surfvalue) Then
    Write(lvltxt(1),'(I7,"-",A4)') Int(surfvalue),surtxt(1)(1:10)
    Write(lvltxt(2),'(I7,"[",A,"] ",A4,"=""",A,"""")') Int(surfvalue),surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
  Else
    Write(lvltxt(1),'(F7.1,"-",A4)') surfvalue,surtxt(1)(1:10)
    Write(lvltxt(2),'(F7.1,"[",A,"] ",A4,"=""",A,"""")') surfvalue,surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
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
Character*80, dimension(1:2), intent(out) :: lvltxt
Character*80, dimension(1:3) :: surtxt
Double Precision surfvalue,sndvalue
Integer indx

lvltxt=''

If (alist(8).LT.1) Then
  lvltxt(1)='0 undefined'
  lvltxt(2)='0.000[-] undefined ()'
  Return
End If

Call getelemlvl2(alist,indx,surfvalue,sndvalue,surtxt)

If (alist(13).EQ.1) Then    
  ! snd data
  If (indx.GE.191) Then
    If (Int(surfvalue).EQ.surfvalue) Then
      Write(lvltxt(1),'(I7,"-",I3,"-reserved(",I3,")")') Int(surfvalue),Int(sndvalue),alist(8)
      Write(lvltxt(2),'(I7,"-",I3,"[-] reserved(",I3,") ()")') Int(surfvalue),Int(sndvalue),indx
    Else
      Write(lvltxt(1),'(F7.1,"-",F3.1,"-reserved(",I3,")")') surfvalue,sndvalue,alist(8)
      Write(lvltxt(2),'(F7.1,"-",F3.1,"[-] reserved(",I3,") ()")') surfvalue,sndvalue,indx
    End If
  Else
    If (Int(surfvalue).EQ.surfvalue) Then
      Write(lvltxt(1),'(I7,"-",I3,"-",A4)') Int(surfvalue),Int(sndvalue),surtxt(1)(1:10)
      Write(lvltxt(2),'(I7,"-",I3,"[",A,"] ",A4,"=""",A,"""")') Int(surfvalue),Int(sndvalue),surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
    Else
      Write(lvltxt(1),'(F7.1,"-",F3.1,"-",A4)') surfvalue,sndvalue,surtxt(1)(1:10)
      Write(lvltxt(2),'(F7.1,"-",F3.1,"[",A,"] ",A4,"=""",A,"""")') surfvalue,sndvalue,surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
    End If
  End If
Else
  ! no snd data
  If (indx.GE.191) Then
    If (Int(surfvalue).EQ.surfvalue) Then
      Write(lvltxt(1),'(I7,"-reserved(",I3,")")') Int(surfvalue),alist(8)
      Write(lvltxt(2),'(I7,"[-] reserved(",I3,") ()")') Int(surfvalue),alist(8)
    Else
      Write(lvltxt(1),'(F7.1,"-reserved(",I3,")")') surfvalue,alist(8)
      Write(lvltxt(2),'(F7.1,"[-] reserved(",I3,") ()")') surfvalue,alist(8)
    End If
  Else
    If (Int(surfvalue).EQ.surfvalue) Then
      Write(lvltxt(1),'(I7,"-",A4)') Int(surfvalue),surtxt(1)(1:10)
      Write(lvltxt(2),'(I7,"[",A,"] ",A4,"=""",A,"""")') Int(surfvalue),surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
    Else
      Write(lvltxt(1),'(F7.1,"-",A4)') surfvalue,surtxt(1)(1:10)
      Write(lvltxt(2),'(F7.1,"[",A,"] ",A4,"=""",A,"""")') surfvalue,surtxt(3)(1:10),surtxt(1)(1:10),surtxt(2)(1:30)
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

if (alonlat(1,1).eq.alonlat(1,2)) then
  if (alonlat(1,3).gt.0.) then
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
    If (alist(19).GE.128) Then
      alonlat(2,3)=abs(real(alist(22))/1000.)
      if (alonlat(2,1).gt.alonlat(2,2)) alonlat(2,3)=-alonlat(2,3)
      alonlat(1,3)=abs(real(alist(23))/1000.)
      if (alonlat(1,1).gt.alonlat(1,2)) alonlat(1,3)=-alonlat(1,3)
      if (alist(16).gt.1) then
        if (abs((alonlat(2,2)-alonlat(2,1))/real(alist(16)-1)) &
            .lt.abs(0.99*alonlat(2,3))) then
          alonlat(2,3)=real(alist(22))/1000.
          alonlat(2,2)=alonlat(2,1)+alonlat(2,3)*real(alist(16)-1)
        end if
      end if
      if (alist(15).gt.1) then
        if (abs((alonlat(1,2)-alonlat(1,1))/real(alist(15)-1)) &
            .lt.abs(0.99*alonlat(1,3))) then
          alonlat(1,3)=real(alist(23))/1000.
          alonlat(1,2)=alonlat(1,1)+alonlat(1,3)*real(alist(15)-1)
        end if
      end if
    Else
      If ((alist(15).GT.1).AND.(alist(16).GT.1)) Then
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
    If (alist(19).GE.128) Then
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
    
    If ((alist(15).NE.0).AND.(alist(16).EQ.0)) Then
      Write(6,*) "ERROR: Cannot read regular grid"
      Write(6,*) alist(15),alist(16)
      alonlat=0.
    Else
      If (alist(15).EQ.0) Then
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
      
      If (((resflag.AND.32).EQ.32).AND.(.NOT.((resflag.AND.16).EQ.16))) Then
        alonlat(2,3)=alonlat(1,3)
      End If
      If ((.NOT.((resflag.AND.32).EQ.32)).AND.((resflag.AND.16).EQ.16)) Then
        alonlat(1,3)=alonlat(2,3)
      End If
            
    End If
  
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

