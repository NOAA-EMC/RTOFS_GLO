c
c  NOAA Coupled Ocean Data Assimilation (NCODA) Data Types
c     ..variational analysis version
c
c  CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c     ..observation data types, measurement errors, resolution
c
c     Name                   Description
c   ---------         ---------------------------
c   MX_TYPES          maximum number data types
c   data_lbl          data type labels
c   inst_err          obs instrumentation error
c   rerr_scl          obs representative scale (km)
c
      integer    MX_TYPES
      parameter (MX_TYPES = 209)
c
      character data_lbl (0:MX_TYPES) * 20
      real      inst_err (0:MX_TYPES)
      real      rerr_scl (0:MX_TYPES)
c
c   0 = All Data Combined
      data      data_lbl(0)  / '   All Data Combined' /
      data      inst_err(0)  / 1.0  /
      data      rerr_scl(0)  /   1. /
c   1 = Bathy Temperatures (C)
      data      data_lbl(1)  / '       eXpendable BT' /
      data      inst_err(1)  / 0.24 /
      data      rerr_scl(1)  /   1. /
c   2 = NOAA-14 Day SST (C)
      data      data_lbl(2)  / '      NOAA14 Day SST' /
      data      inst_err(2)  / 0.31 /
      data      rerr_scl(2)  /   2. /
c   3 = SHIP Engine Room Intake (C)
      data      data_lbl(3)  / '       ERI SHIP Temp' /
      data      inst_err(3)  / 1.30 /
      data      rerr_scl(3)  /   3. /
c   4 = Fixed BUOY Temperature (C)
      data      data_lbl(4)  / '     Fixed BUOY Temp' /
      data      inst_err(4)  / 0.05 /
      data      rerr_scl(4)  /   1. /
c   5 = Drifting BUOY (C)
      data      data_lbl(5)  / '  Drifting BUOY Temp' /
      data      inst_err(5)  / 0.12 /
      data      rerr_scl(5)  /   1. /
c   6 = NOAA-14 Night SST (C)
      data      data_lbl(6)  / '    NOAA14 Night SST' /
      data      inst_err(6)  / 0.26 /
      data      rerr_scl(6)  /   8. /
c   7 = NOAA-14 Twilight SST (C)
      data      data_lbl(7)  / ' NOAA14 Twilight SST' /
      data      inst_err(7)  / 0.49 /
      data      rerr_scl(7)  /   2. /
c   8 = SSM/I F11 Ice (%)
      data      data_lbl(8)  / '       SSM/I F11 Ice' /
      data      inst_err(8)  /  5.0 /
      data      rerr_scl(8)  /  25. /
c   9 = SSM/I F13 Ice (%)
      data      data_lbl(9)  / '       SSM/I F13 Ice' /
      data      inst_err(9)  /  5.0 /
      data      rerr_scl(9)  /  25. /
c  10 = SSM/I F14 Ice (%)
      data      data_lbl(10) / '       SSM/I F14 Ice' /
      data      inst_err(10) /  5.0 /
      data      rerr_scl(10) /  25. /
c  11 = Supplemental Ice (%)
      data      data_lbl(11) / '    Supplemental Ice' /
      data      inst_err(11) / 30.0 /
      data      rerr_scl(11) /  55. /
c  12 = GOES-17 Day SST (C)
      data      data_lbl(12) / '      GOES17 Day SST' /
      data      inst_err(12) / 0.49 /
      data      rerr_scl(12) /  4.  /
c  13 = GOES-17 Night SST (C)
      data      data_lbl(13) / '    GOES17 Night SST' /
      data      inst_err(13) / 0.49 /
      data      rerr_scl(13) /  4.  /
c  14 = GOES-17 Twilight SST (C)
      data      data_lbl(14) / ' GOES17 Twilight SST' /
      data      inst_err(14) / 0.49 /
      data      rerr_scl(14) /  4.  /
c  15 = MODAS Temperature (C)
      data      data_lbl(15) / '   MODAS Temperature' /
      data      inst_err(15) / 1.0  /
      data      rerr_scl(15) /  14. /
c  16 = GDEM 3D Climatology (C)
      data      data_lbl(16) / '    GDEM Climate SST' /
      data      inst_err(16) /  1.0 /
      data      rerr_scl(16) /  27. /
c  17 = GOES-8 Day SST (C)
      data      data_lbl(17) / '       GOES8 Day SST' /
      data      inst_err(17) / 0.49 /
      data      rerr_scl(17) /  12. /
c  18 = GOES-8 Night SST (C)
      data      data_lbl(18) / '     GOES8 Night SST' /
      data      inst_err(18) / 0.31 /
      data      rerr_scl(18) /  12. /
c  19 = Direct Method Temperature (C)
      data      data_lbl(19) / '  Direct Temperature' /
      data      inst_err(19) / 1.0  /
      data      rerr_scl(19) /   1. /
c  20 = TESAC Temperature (C)
      data      data_lbl(20) / '   TESAC Temperature' /
      data      inst_err(20) / 0.01 /
      data      rerr_scl(20) /   1. /
c  21 = SHIP Bucket (C)
      data      data_lbl(21) / '         Bucket SHIP' /
      data      inst_err(21) / 1.21 /
      data      rerr_scl(21) /   1. /
c  22 = SHIP Hull Sensor (C)
      data      data_lbl(22) / '    Hull Sensor SHIP' /
      data      inst_err(22) / 0.64 /
      data      rerr_scl(22) /   1. /
c  23 = CMAN SST (C)
      data      data_lbl(23) / '            CMAN SST' /
      data      inst_err(23) / 1.10 /
      data      rerr_scl(23) /   1. /
c  24 = NOAA-15 Day SST (C)
      data      data_lbl(24) / '      NOAA15 Day SST' /
      data      inst_err(24) / 0.31 /
      data      rerr_scl(24) /   2. /
c  25 = NOAA-15 Night SST (C)
      data      data_lbl(25) / '    NOAA15 Night SST' /
      data      inst_err(25) / 0.26 /
      data      rerr_scl(25) /   2. /
c  26 = NOAA-15 Twilight SST (C)
      data      data_lbl(26) / ' NOAA15 Twilight SST' /
      data      inst_err(26) / 0.49 /
      data      rerr_scl(26) /   2. /
c  27 = Mechanical BT (C)
      data      data_lbl(27) / '       Mechanical BT' /
      data      inst_err(27) / 0.55 /
      data      rerr_scl(27) /   1. /
c  28 = Hydrocast Temperature (C)
      data      data_lbl(28) / '      Hydrocast Temp' /
      data      inst_err(28) / 0.05 /
      data      rerr_scl(28) /   1. /
c  29 = SSM/I F15 Ice (%)
      data      data_lbl(29) / '       SSM/I F15 Ice' /
      data      inst_err(29) /  5.0 /
      data      rerr_scl(29) /  25. /
c  30 = In Situ Sea Surface Height Anomaly (M)
      data      data_lbl(30) / '         In Situ SSH' /
      data      inst_err(30) / 0.01 /
      data      rerr_scl(30) /   1. /
c  31 = SSM/I Ice Shelf
      data      data_lbl(31) / '     SSM/I Shelf Ice' /
      data      inst_err(31) / 30.0 /
      data      rerr_scl(31) /  25. /
c  32 = TESAC Salinity (PSU)
      data      data_lbl(32) / '          TESAC Salt' /
      data      inst_err(32) /  .01 /
      data      rerr_scl(32) /   1. /
c  33 = MODAS Salinity (PSU)
      data      data_lbl(33) / '          MODAS Salt' /
      data      inst_err(33) /  0.5 /
      data      rerr_scl(33) /  14. /
c  34 = TRACK OB Temperature (C)
      data      data_lbl(34) / '       TRACK OB Temp' /
      data      inst_err(34) / 0.30 /
      data      rerr_scl(34) /   1. /
c  35 = TRACK OB Salinty (PSU)
      data      data_lbl(35) / '       TRACK OB Salt' /
      data      inst_err(35) / 1.0  /
      data      rerr_scl(35) /   1. /
c  36 = Argo Float Temperature (C)
      data      data_lbl(36) / '     Argo Float Temp' /
      data      inst_err(36) / 0.02 /
      data      rerr_scl(36) /   1. /
c  37 = Argo Float Salinity (PSU)
      data      data_lbl(37) / '     Argo Float Salt' /
      data      inst_err(37) / 0.01 /
      data      rerr_scl(37) /   1. /
c  38 = Supplemental MODAS Temperature (C)
      data      data_lbl(38) / '    MODAS Suppl Temp' /
      data      inst_err(38) / 1.0  /
      data      rerr_scl(38) /  14. /
c  39 = Supplemental MODAS Salinity (PSU)
      data      data_lbl(39) / '    MODAS Suppl Salt' /
      data      inst_err(39) /  0.5 /
      data      rerr_scl(39) /  14. /
c  40 = Supplemental Sea Surface Height (M)
      data      data_lbl(40) / '    Supplemental SSH' /
      data      inst_err(40) /  1.0 /
      data      rerr_scl(40) /   7. /
c  41 = Freezing Sea Water SST (C)
      data      data_lbl(41) / '         Sea Ice SST' /
      data      inst_err(41) / 30.0 /
      data      rerr_scl(41) /  25. /
c  42 = SST super ob (C)
      data      data_lbl(42) / '                 SST' /
      data      inst_err(42) / 0.30 /
      data      rerr_scl(42) /  12. /
c  43 = NOAA-16 Day SST (C)
      data      data_lbl(43) / '      NOAA16 Day SST' /
      data      inst_err(43) / 0.31 /
      data      rerr_scl(43) /   2. /
c  44 = NOAA-16 Night SST (C)
      data      data_lbl(44) / '    NOAA16 Night SST' /
      data      inst_err(44) / 0.26 /
      data      rerr_scl(44) /   2. /
c  45 = NOAA-16 Twilight SST (C)
      data      data_lbl(45) / ' NOAA16 Twilight SST' /
      data      inst_err(45) / 0.49 /
      data      rerr_scl(45) /   2. /
c  46 = SST Derived Surface Salinity (PSU)
      data      data_lbl(46) / '        SST Sfc Salt' /
      data      inst_err(46) /  2.5 /
      data      rerr_scl(46) /   2. /
c  47 = GOES-10 Day SST (C)
      data      data_lbl(47) / '      GOES10 Day SST' /
      data      inst_err(47) / 0.49 /
      data      rerr_scl(47) /  12. /
c  48 = GOES-10 Night SST (C)
      data      data_lbl(48) / '    GOES10 Night SST' /
      data      inst_err(48) / 0.31 /
      data      rerr_scl(48) /  12. /
c  49 = Direct Method Salinity (PSU)
      data      data_lbl(49) / '         Direct Salt' /
      data      inst_err(49) /  1.0 /
      data      rerr_scl(49) /   1. /
c  50 = Extended Temperatures (C)
      data      data_lbl(50) / '       Extended Temp' /
      data      inst_err(50) / 0.5  /
      data      rerr_scl(50) /  12. /
c  51 = Extended Salinity (PSU)
      data      data_lbl(51) / '       Extended Salt' /
      data      inst_err(51) /  0.5 /
      data      rerr_scl(51) /  12. /
c  52 = Fixed BUOY Salinity (PSU)
      data      data_lbl(52) / '     Fixed BUOY Salt' /
      data      inst_err(52) / 0.01 /
      data      rerr_scl(52) /  1.  /
c  53 = CMAN SSS (C)
      data      data_lbl(53) / '           CMAN Salt' /
      data      inst_err(53) / 0.50 /
      data      rerr_scl(53) /   1. /
c  54 = Drifting BUOY Salinity (PSU)
      data      data_lbl(54) / '  Drifting BUOY Salt' /
      data      inst_err(54) / 0.02  /
      data      rerr_scl(54) /  1.   /
c  55 = Unknown Surface Salinity (PSU)
      data      data_lbl(55) / '         Unknown SSS' /
      data      inst_err(55) /  1.0  /
      data      rerr_scl(55) /   1.  /
c  56 = NOAA-17 Day SST (C)
      data      data_lbl(56) / '      NOAA17 Day SST' /
      data      inst_err(56) / 0.31 /
      data      rerr_scl(56) /  2.  /
c  57 = NOAA-17 Night SST (C)
      data      data_lbl(57) / '    NOAA17 Night SST' /
      data      inst_err(57) / 0.26 /
      data      rerr_scl(57) /  2.  /
c  58 = NOAA-17 Twilight SST (C)
      data      data_lbl(58) / ' NOAA17 Twilight SST' /
      data      inst_err(58) / 0.49 /
      data      rerr_scl(58) /  2.  /
c  59 = GOES-16 Day SST (C)
      data      data_lbl(59) / '      GOES16 Day SST' /
      data      inst_err(59) / 0.36 /
      data      rerr_scl(59) /  2.  /
c  60 = GOES-16 Night SST (C)
      data      data_lbl(60) / '    GOES16 Night SST' /
      data      inst_err(60) / 0.23 /
      data      rerr_scl(60) /  2.  /
c  61 = Saildrone CTD Temperature (C)
      data      data_lbl(61) / '  Saildrone CTD Temp' /
      data      inst_err(61) / 0.03 /
      data      rerr_scl(61) /   1. /
c  62 = Saildrone CTD Salinity (PSU)
      data      data_lbl(62) / '  Saildrone CTD Salt' /
      data      inst_err(62) / 0.01 /
      data      rerr_scl(62) /   1. /
c  63 = Sentinel-6A SSH (M)
      data      data_lbl(63) / '     Sentinel-6A SSH' /
      data      inst_err(63) / 0.02 /
      data      rerr_scl(63) /  7.  /
c  64 = Sentinel-6B SSH (M)
      data      data_lbl(64) / '     Sentinel-6B SSH' /
      data      inst_err(64) / 0.02 /
      data      rerr_scl(64) /  7.  /
c  65 = Model Salinity (PSU)
      data      data_lbl(65) / '          Model Salt' /
      data      inst_err(65) /  0.5 /
      data      rerr_scl(65) /   8. /
c  66 = Jason-3 Sea Surface Height - Interleaved Orbit (M)
      data      data_lbl(66) / '     JS-3 Intrlv SSH' /
      data      inst_err(66) / 0.035 /
      data      rerr_scl(66) /  7.  /
c  67 = GOES-18 Day SST (C)
      data      data_lbl(67) / '      GOES18 Day SST' /
      data      inst_err(67) / 0.49 /
      data      rerr_scl(67) /  4.  /
c  68 = GOES-18 Night SST (C)
      data      data_lbl(68) / '    GOES18 Night SST' /
      data      inst_err(68) / 0.49 /
      data      rerr_scl(68) /  4.  /
c  69 = AMSR-E Day Microwave SST (C)
      data      data_lbl(69) / '       AMSRE Day SST' /
      data      inst_err(69) / 0.71 /
      data      rerr_scl(69) /  25. /
c  70 = GOES-12 Day SST (C)
      data      data_lbl(70) / '      GOES12 Day SST' /
      data      inst_err(70) / 0.49 /
      data      rerr_scl(70) /  12. /
c  71 = GOES-12 Night SST (C)
      data      data_lbl(71) / '    GOES12 Night SST' /
      data      inst_err(71) / 0.31 /
      data      rerr_scl(71) /  12. /
c  72 = AMSR-E Night Microwave SST (C)
      data      data_lbl(72) / '     AMSRE Night SST' /
      data      inst_err(72) / 0.71 /
      data      rerr_scl(72) /  25. /
c  73 = TRMM microwave SST (C)
      data      data_lbl(73) / '         TRMM MW SST' /
      data      inst_err(73) / 0.71 /
      data      rerr_scl(73) /  50. /
c  74 = AATSR (Envisat) Day SST (C)
      data      data_lbl(74) / '       AATSR Day SST' /
      data      inst_err(74) / 0.5 /
      data      rerr_scl(74) /  1. /
c  75 = AATSR (Envisat) Night SST (C)
      data      data_lbl(75) / '     AATSR Night SST' /
      data      inst_err(75) / 0.3 /
      data      rerr_scl(75) /  1. /
c  76 = GOES-18 Twilight SST (C)
      data      data_lbl(76) / ' GOES18 Twilight SST' /
      data      inst_err(76) / 0.49 /
      data      rerr_scl(76) /  4.  /
c  77 = AMSR-2 Night Microwave SST (C)
      data      data_lbl(77) / '     AMSR2 Night SST' /
      data      inst_err(77) / 0.71 /
      data      rerr_scl(77) / 12.5 /
c  78 = Sea Ice Super Observations
      data      data_lbl(78) / '             Sea Ice' /
      data      inst_err(78) /  5.0 /
      data      rerr_scl(78) /  25. /
c  79 = SST Super Observations
      data      data_lbl(79) / '                 SST' /
      data      inst_err(79) / 1.00 /
      data      rerr_scl(79) /   1. /
c  80 = SSMIS F17 Ice (%)
      data      data_lbl(80) / '       SSMIS F17 Ice' /
      data      inst_err(80) /  5.0 /
      data      rerr_scl(80) /  25. /
c  81 = Altimeter SSH Super Observations
      data      data_lbl(81) / '                 SSH' /
      data      inst_err(81) / 0.03 /
      data      rerr_scl(81) /   7. /
c  82 = Hydrocast Salinity (C)
      data      data_lbl(82) / '      Hydrocast Salt' /
      data      inst_err(82) / 0.03 /
      data      rerr_scl(82) /   1. /
c  83 = Near Shore Ice (%)
      data      data_lbl(83) / '      Near Shore Ice' /
      data      inst_err(83) / 30.0 /
      data      rerr_scl(83) /  25. /
c  84 = Aircraft Sea Surface Temperature (C)
      data      data_lbl(84) / '        Aircraft SST' /
      data      inst_err(84) / 0.30 /
      data      rerr_scl(84) /   1. /
c  85 = HF Radar U Velocity Component
      data      data_lbl(85) / '      HF Radar U Vel' /
      data      inst_err(85) /  0.1 /
      data      rerr_scl(85) /   6. /
c  86 = HF Radar V Velocity Component
      data      data_lbl(86) / '      HF Radar V Vel' /
      data      inst_err(86) /  0.1 /
      data      rerr_scl(86) /   6. /
c  87 = U Velocity
      data      data_lbl(87) / '          U Velocity' /
      data      inst_err(87) /  0.1 /
      data      rerr_scl(87) /   6. /
c  88 = V Velocity
      data      data_lbl(88) / '          V Velocity' /
      data      inst_err(88) /  0.1 /
      data      rerr_scl(88) /   6. /
c  89 = Glider Absolute U Velocity Component
      data      data_lbl(89) / '        Glider U Vel' /
      data      inst_err(89) /  0.1 /
      data      rerr_scl(89) /   1. /
c  90 = SSMIS F18 Ice (%)
      data      data_lbl(90) / '       SSMIS F18 Ice' /
      data      inst_err(90) /  5.0 /
      data      rerr_scl(90) /  25. /
c  91 = Jason-2 Sea Surface Height (M)
      data      data_lbl(91) / '         Jason-2 SSH' /
      data      inst_err(91) / 0.02 /
      data      rerr_scl(91) /  7. /
c  92 = Glider Absolute V Velocity Component
      data      data_lbl(92) / '        Glider V Vel' /
      data      inst_err(92) /  0.1 /
      data      rerr_scl(92) /   1. /
c  93 = Surface Drifter U Velocity Component
      data      data_lbl(93) / '       Drifter U Vel' /
      data      inst_err(93) / 1.00 /
      data      rerr_scl(93) /   1. /
c  94 = NOAA-18 Day SST (C)
      data      data_lbl(94) / '      NOAA18 Day SST' /
      data      inst_err(94) / 0.31 /
      data      rerr_scl(94) /  2.  /
c  95 = NOAA-18 Night SST (C)
      data      data_lbl(95) / '    NOAA18 Night SST' /
      data      inst_err(95) / 0.26 /
      data      rerr_scl(95) /  2.  /
c  96 = NOAA-18 Twilight SST (C)
      data      data_lbl(96) / ' NOAA18 Twilight SST' /
      data      inst_err(96) / 0.49 /
      data      rerr_scl(96) /  2.  /
c  97 = SailDrone Temperature (C)
      data      data_lbl(97) / '      SailDrone Temp' /
      data      inst_err(97) /  0.05 /
      data      rerr_scl(97) /   1. /
c  98 = SailDrone Salinty (PSU)
      data      data_lbl(98) / '      SailDrone Salt' /
      data      inst_err(98) /  0.01 /
      data      rerr_scl(98) /   1. /
c  99 = Meteosat Second Generation Day SST (C)
      data      data_lbl(99) / '       MSG02 Day SST' /
      data      inst_err(99) / 0.49 /
      data      rerr_scl(99) /  4. /
c 100 = Meteosat Second Generation Night SST (C)
      data      data_lbl(100) / '     MSG02 Night SST' /
      data      inst_err(100) / 0.31 /
      data      rerr_scl(100) /  4. /
c 101 = Ice Tethered Temperature
      data      data_lbl(101) / '   Ice Tethered Temp' /
      data      inst_err(101) /  0.1 /
      data      rerr_scl(101) /   1. /
c 102 = Glider Temperature Profiles (C)
      data      data_lbl(102) / '         Glider Temp' /
      data      inst_err(102) / 0.05 /
      data      rerr_scl(102) /  1.  /
c 103 = Glider Salinity Profiles (PSU)
      data      data_lbl(103) / '         Glider Salt' /
      data      inst_err(103) / 0.01 /
      data      rerr_scl(103) /   1. /
c 104 = Surface Drifter V Velocity Component
      data      data_lbl(104) / '       Drifter V Vel' /
      data      inst_err(104) / 1.00 /
      data      rerr_scl(104) /  1.  /
c 105 = ADCP U Velocity Component
      data      data_lbl(105) / '          ADCP U Vel' /
      data      inst_err(105) / 0.02 /
      data      rerr_scl(105) /  1.  /
c 106 = HYCOM Layer Pressure (db)
      data      data_lbl(106) / '      Layer Pressure' /
      data      inst_err(106) / 5. /
      data      rerr_scl(106) /   1. /
c 107 = GOES-11 Day SST (C)
      data      data_lbl(107) / '      GOES11 Day SST' /
      data      inst_err(107) / 0.49 /
      data      rerr_scl(107) /  12. /
c 108 = GOES-11 Night SST (C)
      data      data_lbl(108) / '    GOES11 Night SST' /
      data      inst_err(108) / 0.31 /
      data      rerr_scl(108) /  12. /
c 109 = SSMIS F16 Ice (%)
      data      data_lbl(109) / '       SSMIS F16 Ice' /
      data      inst_err(109) /  5.0 /
      data      rerr_scl(109) /  25. /
c 110 = METOP-A Day SST (C)
      data      data_lbl(110) / '     METOP-A Day SST' /
      data      inst_err(110) / 0.31 /
      data      rerr_scl(110) /  1.  /
c 111 = METOP-A Night SST (C)
      data      data_lbl(111) / '   METOP-A Night SST' /
      data      inst_err(111) / 0.26 /
      data      rerr_scl(111) /  1.  /
c 112 = METOP-A Twilight SST (C)
      data      data_lbl(112) / 'METOP-A Twilight SST' /
      data      inst_err(112) / 0.49 /
      data      rerr_scl(112) /  1.  /
c 113 = Ice Tethered Salinity
      data      data_lbl(113) / '   Ice Tethered Salt' /
      data      inst_err(113) /  0.05 /
      data      rerr_scl(113) /   1. /
c 114 = Sea Ice Temperature Super Observations
      data      data_lbl(114) / '        Sea Ice Temp' /
      data      inst_err(114) /  1.0 /
      data      rerr_scl(114) /  25. /
c 115 = METOP-B Day SST (C)
      data      data_lbl(115) / '     METOP-B Day SST' /
      data      inst_err(115) / 0.31 /
      data      rerr_scl(115) /  1.  /
c 116 = METOP-B Night SST (C)
      data      data_lbl(116) / '   METOP-B Night SST' /
      data      inst_err(116) / 0.26 /
      data      rerr_scl(116) /  1.  /
c 117 = METOP-B Twilight SST (C)
      data      data_lbl(117) / 'METOP-B Twilight SST' /
      data      inst_err(117) / 0.49 /
      data      rerr_scl(117) /  2.  /
c 118 = Sea Ice Thickness Super Observations
      data      data_lbl(118) / '   Sea Ice Thickness' /
      data      inst_err(118) /  1.0 /
      data      rerr_scl(118) /  25. /
c 119 = NOAA-20 VIIRS Twilight SST (C)
      data      data_lbl(119) / ' NOAA20 Twilight SST' /
      data      inst_err(119) / 0.49 /
      data      rerr_scl(119) / 2. /
c 120 = METOP-C Day SST (C)
      data      data_lbl(120) / '     METOP-C Day SST' /
      data      inst_err(120) / 0.31 /
      data      rerr_scl(120) /  1.  /
c 121 = METOP-C Night SST (C)
      data      data_lbl(121) / '   METOP-C Night SST' /
      data      inst_err(121) / 0.26 /
      data      rerr_scl(121) /  1.  /
c 122 = METOP-C Twilight SST (C)
      data      data_lbl(122) / 'METOP-C Twilight SST' /
      data      inst_err(122) / 0.49 /
      data      rerr_scl(122) /  1.  /
c 123 = NOAA-20 VIIRS Day SST (C)
      data      data_lbl(123) / '      NOAA20 Day SST' /
      data      inst_err(123) / 0.31 /
      data      rerr_scl(123) /  2.  /
c 124 = NOAA-20 VIIRS Night SST (C)
      data      data_lbl(124) / '    NOAA20 Night SST' /
      data      inst_err(124) / 0.26 /
      data      rerr_scl(124) /  2.  /
c 125 = SWOT Sea Surface Height (M)
      data      data_lbl(125) / '            SWOT SSH' /
      data      inst_err(125) / 0.02 /
      data      rerr_scl(125) /  7. /
c 126 = NASA Surface Moisture Active Passive Salinity
      data      data_lbl(126) / '            SMAP SSS' /
      data      inst_err(126) / 0.03 /
      data      rerr_scl(126) /  50. /
c 127 = Freezing Sea Water SSS (C)
      data      data_lbl(127) / '         Sea Ice SSS' /
      data      inst_err(127) / 3.0  /
      data      rerr_scl(127) /  25. /
c 128 = NOAA-19 Day SST (C)
      data      data_lbl(128) / '      NOAA19 Day SST' /
      data      inst_err(128) / 0.31 /
      data      rerr_scl(128) /  1.  /
c 129 = NOAA-19 Night SST (C)
      data      data_lbl(129) / '    NOAA19 Night SST' /
      data      inst_err(129) / 0.26 /
      data      rerr_scl(129) /  1.  /
c 130 = NOAA-19 Twilight SST (C)
      data      data_lbl(130) / ' NOAA19 Twilight SST' /
      data      inst_err(130) / 0.49 /
      data      rerr_scl(130) /  1.  /
c 131 = HIMAWARI-9 Day SST (C)
      data      data_lbl(131) / '   HIMAWARI9 Day SST' /
      data      inst_err(131) / 0.49 /
      data      rerr_scl(131) /  4.  /
c 132 = HIMAWARI-9 Night SST (C)
      data      data_lbl(132) / ' HIMAWARI9 Night SST' /
      data      inst_err(132) / 0.31 /
      data      rerr_scl(132) /  4.  /
c 133 = Anmimal Borne Temperature (C)
      data      data_lbl(133) / '   Animal Borne Temp' /
      data      inst_err(133) / 0.04 /
      data      rerr_scl(133) /   1. /
c 134 = Animal Borne Salinity (PSU)
      data      data_lbl(134) / '   Animal Borne Salt' /
      data      inst_err(134) / 0.02 /
      data      rerr_scl(134) /   1. /
c 135 = GOES-13 Day SST (C)
      data      data_lbl(135) / '      GOES13 Day SST' /
      data      inst_err(135) / 0.49 /
      data      rerr_scl(135) /   8. /
c 136 = GOES-13 Night SST (C)
      data      data_lbl(136) / '    GOES13 Night SST' /
      data      inst_err(136) / 0.31 /
      data      rerr_scl(136) /   8. /
c 137 = ADCP V Velocity Component
      data      data_lbl(137) / '          ADCP V Vel' /
      data      inst_err(137) / 0.02 /
      data      rerr_scl(137) /  1.  /
c 138 = AMSR-2 Sea Ice (%)
      data      data_lbl(138) / '       AMSR2 Sea Ice' /
      data      inst_err(138) /  8.0 /
      data      rerr_scl(138) / 12.5 /
c 139 = MTSAT-2 Day SST (C)
      data      data_lbl(139) / '     MTSAT-2 Day SST' /
      data      inst_err(139) / 0.81 /
      data      rerr_scl(139) /  12. /
c 140 = MTSAT-2 Night SST (C)
      data      data_lbl(140) / '   MTSAT-2 Night SST' /
      data      inst_err(140) / 0.81 /
      data      rerr_scl(140) /  12. /
c 141 = Expanded Real Profile Temperature (C)
      data      data_lbl(141) / '  Expanded Real Temp' /
      data      inst_err(141) / 0.5  /
      data      rerr_scl(141) /  12. /
c 142 = Expanded Real Profile Salinity (PSU)
      data      data_lbl(142) / '  Expanded Real Salt' /
      data      inst_err(142) / 0.5  /
      data      rerr_scl(142) /  12. /
c 143 = Expanded Synthetic Profile Temperature (C)
      data      data_lbl(143) / '   Expanded Syn Temp' /
      data      inst_err(143) / 0.5  /
      data      rerr_scl(143) /  12. /
c 144 = Expanded Synthetic Profile Salinity (PSU)
      data      data_lbl(144) / '   Expanded Syn Salt' /
      data      inst_err(144) /  0.5 /
      data      rerr_scl(144) /  12. /
c 145 = NOAA-21 VIIRS Day SST (C)
      data      data_lbl(145) / '      NOAA21 Day SST' /
      data      inst_err(145) / 0.31 /
      data      rerr_scl(145) /  2.  /
c 146 = NPP VIIRS Ice Surface Temperature (C)
      data      data_lbl(146) / '  NPP VIIRS Ice Temp' /
      data      inst_err(146) /   0.5 /
      data      rerr_scl(146) /  0.37 /
c 147 = NOAA-20 VIIRS Ice Surface Temperature (C)
      data      data_lbl(147) / '     NOAA20 Ice Temp' /
      data      inst_err(147) /  0.5 /
      data      rerr_scl(147) / 0.37 /
c 148 = METOP-B Ice Surface Temperature (C)
      data      data_lbl(148) / '    METOP-B Ice Temp' /
      data      inst_err(148) /  0.5 /
      data      rerr_scl(148) /  1.  /
c 149 = Ice Surface Temp Super Observations
      data      data_lbl(149) / '            Ice Temp' /
      data      inst_err(149) / 3.00 /
      data      rerr_scl(149) /   1. /
c 150 = METOP-A Ice Surface Temperature (C)
      data      data_lbl(150) / '    METOP-A Ice Temp' /
      data      inst_err(150) / 0.5 /
      data      rerr_scl(150) /  1. /
c 151 = Sentinel-3A SLSTR Day SST (C)
      data      data_lbl(151) / '    SLSTR-3A Day SST' /
      data      inst_err(151) / 0.10 /
      data      rerr_scl(151) /  1.  /
c 152 = Sentinel-3A SLSTR Night SST (C)
      data      data_lbl(152) / '  SLSTR-3A Night SST' /
      data      inst_err(152) / 0.10 /
      data      rerr_scl(152) /  1.  /
c 153 = NPP VIIRS Day SST (C)
      data      data_lbl(153) / '         NPP Day SST' /
      data      inst_err(153) / 0.31 /
      data      rerr_scl(153) / 0.37 /
c 154 = NPP VIIRS Night SST (C)
      data      data_lbl(154) / '       NPP Night SST' /
      data      inst_err(154) / 0.26 /
      data      rerr_scl(154) / 0.37  /
c 155 = NPP VIIRS Twilight SST (C)
      data      data_lbl(155) / '    NPP Twilight SST' /
      data      inst_err(155) / 0.49 /
      data      rerr_scl(155) / 0.37 /
c 156 = METOP-C Ice Surface Temperature (C)
      data      data_lbl(156) / '    METOP-C Ice Temp' /
      data      inst_err(156) / 0.5 /
      data      rerr_scl(156) /  1. /
c 157 = Argo Trajectory U Velocity (m/s)
      data      data_lbl(157) / '  Argo Traject U Vel' /
      data      inst_err(157) /  0.1 /
      data      rerr_scl(157) /   6. /
c 158 = Argo Trajectory V Velocity (m/s)
      data      data_lbl(158) / '  Argo Traject V Vel' /
      data      inst_err(158) /  0.1 /
      data      rerr_scl(158) /   6. /
c 159 = GOES-15 Day SST (C)
      data      data_lbl(159) / '      GOES15 Day SST' /
      data      inst_err(159) / 0.49 /
      data      rerr_scl(159) /   8. /
c 160 = GOES-15 Night SST (C)
      data      data_lbl(160) / '    GOES15 Night SST' /
      data      inst_err(160) / 0.31 /
      data      rerr_scl(160) /   8. /
c 161 = SSH Cross-Track Geostrophic U Velocity (m/s)
      data      data_lbl(161) / '           SSH U Vel' /
      data      inst_err(161) /  0.1 /
      data      rerr_scl(161) /   7. /
c 162 = SSH Cross-Track Geostrophic V Velocity (m/s)
      data      data_lbl(162) / '           SSH V Vel' /
      data      inst_err(162) /  0.1 /
      data      rerr_scl(162) /   7. /
c 163 = Wave Glider Temperature (C)
      data      data_lbl(163) / '    Wave Glider Temp' /
      data      inst_err(163) /  0.02 /
      data      rerr_scl(163) /   1. /
c 164 = Wave Glider Salinity (PSU)
      data      data_lbl(164) / '    Wave Glider Salt' /
      data      inst_err(164) /  0.01 /
      data      rerr_scl(164) /   1. /
c 165 = Sentinel-3B SLSTR Day SST (C)
      data      data_lbl(165) / '    SLSTR-3B Day SST' /
      data      inst_err(165) / 0.10 /
      data      rerr_scl(165) /  1.  /
c 166 = NOAA-21 VIIRS Night SST (C)
      data      data_lbl(166) / '    NOAA21 Night SST' /
      data      inst_err(166) / 0.26 /
      data      rerr_scl(166) /  2.  /
c 167 = Cryosat-2 Sea Surface Height (M)
      data      data_lbl(167) / '       CryoSat-2 SSH' /
      data      inst_err(167) / 0.073 /
      data      rerr_scl(167) /  7. /
c 168 = NOAA-21 VIIRS Twilight SST (C)
      data      data_lbl(168) / ' NOAA21 Twilight SST' /
      data      inst_err(168) / 0.49 /
      data      rerr_scl(168) / 2. /
c 169 = AMSR-2 Day Microwave SST (C)
      data      data_lbl(169) / '       AMSR2 Day SST' /
      data      inst_err(169) / 0.71 /
      data      rerr_scl(169) / 12.5 /
c 170 = Aquarius Salinity (PSU)
      data      data_lbl(170) / '        Aquarius SSS' /
      data      inst_err(170) /  0.03 /
      data      rerr_scl(170) /  50.  /
c 171 = Meteosat Third Generation Day SST (C)
      data      data_lbl(171) / '       MSG03 Day SST' /
      data      inst_err(171) / 0.49 /
      data      rerr_scl(171) /  4. /
c 172 = Meteosat Third Generation Night SST (C)
      data      data_lbl(172) / '     MSG03 Night SST' /
      data      inst_err(172) / 0.31 /
      data      rerr_scl(172) /  4. /
c 173 = SSS Super Observations
      data      data_lbl(173) / '                 SSS' /
      data      inst_err(173) / 0.03 /
      data      rerr_scl(173) /  50. /
c 174 = SMOS Salinity (PSU)
      data      data_lbl(174) / '            SMOS SSS' /
      data      inst_err(174) /  0.03 /
      data      rerr_scl(174) /  50. /
c 175 = GOES-14 Day SST (C)
      data      data_lbl(175) / '      GOES14 Day SST' /
      data      inst_err(175) / 0.49 /
      data      rerr_scl(175) /  12. /
c 176 = GOES-14 Night SST (C)
      data      data_lbl(176) / '    GOES14 Night SST' /
      data      inst_err(176) / 0.31 /
      data      rerr_scl(176) /  12. /
c 177 = Expanded U Velocity Profile (m/s)
      data      data_lbl(177) / ' Expanded U Velocity' /
      data      inst_err(177) / 0.02 /
      data      rerr_scl(177) /  1.  /
c 178 = Expanded V Velocity Profile (m/s)
      data      data_lbl(178) / ' Expanded V Velocity' /
      data      inst_err(178) / 0.02 /
      data      rerr_scl(178) /  1.  /
c 179 = Altika Sea Surface Height (M)
      data      data_lbl(179) / '          Altika SSH' /
      data      inst_err(179) / 0.05 /
      data      rerr_scl(179) /  7. /
c 180 = Meteosat Third Generation Twilight SST (C)
      data      data_lbl(180) / '  MSG03 Twilight SST' /
      data      inst_err(180) / 0.49 /
      data      rerr_scl(180) /   4. /
c 181 = HIMAWARI-8 Day SST (C)
      data      data_lbl(181) / '   HIMAWARI8 Day SST' /
      data      inst_err(181) / 0.49 /
      data      rerr_scl(181) /   4. /
c 182 = HIMAWARI-8 Night SST (C)
      data      data_lbl(182) / ' HIMAWARI8 Night SST' /
      data      inst_err(182) / 0.31 /
      data      rerr_scl(182) /   4. /
c 183 = GOES-19 Day SST (C)
      data      data_lbl(183) / '      GOES19 Day SST' /
      data      inst_err(183) / 0.49 /
      data      rerr_scl(183) /  4.  /
c 184 = GOES-19 Night SST (C)
      data      data_lbl(184) / '    GOES19 Night SST' /
      data      inst_err(184) / 0.49 /
      data      rerr_scl(184) /  4.  /
c 185 = GOES-19 Twilight SST (C)
      data      data_lbl(185) / ' GOES19 Twilight SST' /
      data      inst_err(185) / 0.49 /
      data      rerr_scl(185) /  4.  /
c 186 = Expendable Conductivity Temperature (C)
      data      data_lbl(186) / ' eXpendable CTD Temp' /
      data      inst_err(186) / 0.12 /
      data      rerr_scl(186) /   1. /
c 187 = Expendable Conductivity Salinity (PSU)
      data      data_lbl(187) / ' eXpendable CTD Salt' /
      data      inst_err(187) / 0.02 /
      data      rerr_scl(187) /   1. /
c 188 = ALAMO Float Temperature (C)
      data      data_lbl(188) / '    ALAMO Float Temp' /
      data      inst_err(188) / 0.02 /
      data      rerr_scl(188) /   1. /
c 189 = ALAMO Float Salinity (PSU)
      data      data_lbl(189) / '    ALAMO Float Salt' /
      data      inst_err(189) / 0.01 /
      data      rerr_scl(189) /   1. /
c 190 = Sentinel-3A SLSTR Ice Temperature (C)
      data      data_lbl(190) / '   SLSTR-3A Ice Temp' /
      data      inst_err(190) / 0.5 /
      data      rerr_scl(190) /  1. /
c 191 = Sentinel-3B SLSTR Ice Temperature (C)
      data      data_lbl(191) / '   SLSTR-3B Ice Temp' /
      data      inst_err(191) / 0.5 /
      data      rerr_scl(191) /  1. /
c 192 = VIIRS Sea Ice (%)
      data      data_lbl(192) / '           VIIRS Ice' /
      data      inst_err(192) /  5.0 /
      data      rerr_scl(192) /   1. /
c 193 = Jason-3 Sea Surface Height - Tandem Orbit (M)
      data      data_lbl(193) / '     JS-3 Tandom SSH' /
      data      inst_err(193) / 0.02 /
      data      rerr_scl(193) /  7.  /
c 194 = Sentinel-3B SLSTR Night SST (C)
      data      data_lbl(194) / '  SLSTR-3B Night SST' /
      data      inst_err(194) / 0.10 /
      data      rerr_scl(194) /  1.  /
c 195 = Jason-3 Sea Surface Height (M)
      data      data_lbl(195) / '         Jason-3 SSH' /
      data      inst_err(195) / 0.02 /
      data      rerr_scl(195) /  7.  /
c 196 = Not Used
      data      data_lbl(196) / '            Not Used' /
      data      inst_err(196) /  0.0 /
      data      rerr_scl(196) /  1.  /
c 197 = Sentinel-3A SSH (M)
      data      data_lbl(197) / '     Sentinel-3A SSH' /
      data      inst_err(197) / 0.035 /
      data      rerr_scl(197) /  7.  /
c 198 = Not Used
      data      data_lbl(198) / '           Not Used' /
      data      inst_err(198) / 0.0  /
      data      rerr_scl(198) /  1.  /
c 199 = Sentinel-3B SSH (M)
      data      data_lbl(199) / '     Sentinel-3B SSH' /
      data      inst_err(199) / 0.035 /
      data      rerr_scl(199) /  7.  /
c 200 = Not Used
      data      data_lbl(200) / '           Not Used' /
      data      inst_err(200) / 0.0  /
      data      rerr_scl(200) /  1.  /
c 201 = Not Used
      data      data_lbl(201) / '            Not Used' /
      data      inst_err(201) / 0.00 /
      data      rerr_scl(201) /  0.  /
c 202 = Not Used
      data      data_lbl(202) / '            Not Used' /
      data      inst_err(202) /  0.0 /
      data      rerr_scl(202) /  1.  /
c 203 = Not Used
      data      data_lbl(203) / '            Not Used' /
      data      inst_err(203) / 0.00 /
      data      rerr_scl(203) /  0.  /
c 204 = HIMAWARI-8 Twilight SST (C)
      data      data_lbl(204) / 'HIMAWARI8 Twilgt SST' /
      data      inst_err(204) / 0.49 /
      data      rerr_scl(204) /  4.  /
c 205 = HIMAWARI-9 Twilight SST (C)
      data      data_lbl(205) / 'HIMAWARI9 Twilgt SST' /
      data      inst_err(205) / 0.49 /
      data      rerr_scl(205) /  4.  /
c 206 = GOES-16 Twilight SST (C)
      data      data_lbl(206) / ' GOES16 Twilight SST' /
      data      inst_err(206) / 0.49 /
      data      rerr_scl(206) /  4.  /
c 207 = Meteosat Second Generation Twilight SST (C)
      data      data_lbl(207) / '  MSG02 Twilight SST' /
      data      inst_err(207) / 0.49 /
      data      rerr_scl(207) /  4.  /
c 208 = Ocean Site Mooring Temperature (C)
      data      data_lbl(208) / '     Ocean Site Temp' /
      data      inst_err(208) / 0.02 /
      data      rerr_scl(208) /   1. /
c 209 = Ocean Site Mooring Salinity (PSU)
      data      data_lbl(209) / '     Ocean Site Salt' /
      data      inst_err(209) / 0.01 /
      data      rerr_scl(209) /   1. /
c
c..End CODA Types
c
