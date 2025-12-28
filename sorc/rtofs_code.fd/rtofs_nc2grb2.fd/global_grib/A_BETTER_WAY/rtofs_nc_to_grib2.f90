program rtofs_nc_to_grib2
    use netcdf
    use grib_mod
    implicit none

    ! GRIB2 Template lengths
    integer(4), parameter :: igdstmplen=19, ipdstmplen=15, idrstmplen=5

    ! HARDCODED DIMENSIONS for RTOFS Global
    integer, parameter :: imax = 4320
    integer, parameter :: jmax = 3298
    integer, parameter :: ngrdpts = imax * jmax

    ! NetCDF variables
    integer :: ncid, varid, iret
    character(len=256) :: nc_file, var_name_nc, out_file

    ! GRIB2 encoding variables
    integer(4) :: listsec0(2), listsec1(13), igds(5)
    integer(4) :: igdstmpl(igdstmplen), ipdstmpl(ipdstmplen), idrstmpl(idrstmplen)
    integer(4) :: ipdsnum, idrsnum, ierr, lugb, lengrib
    real, allocatable, dimension(:,:) :: var_data
    logical(1), allocatable, dimension(:,:) :: bmap
    character, allocatable, dimension(:) :: cgrib
    
    ! Metadata variables
    integer :: iyr, imo, iday, icycle, fcsthr, parm, p_cat, gen_pro
    real    :: lat0, lon0, lat1, lon1, dlat, dlon, depth

    ! -------------------------------------------------------------------
    ! 1. INPUT PARAMETERS
    ! -------------------------------------------------------------------
    read(*,'(A)') nc_file
    read(*,'(A)') var_name_nc
    read(*,'(A)') out_file
    read(*,*) iyr, imo, iday, icycle, fcsthr
    read(*,*) parm, p_cat, lon0, lat0, dlat, dlon, depth, gen_pro

    nc_file = trim(adjustl(nc_file))
    var_name_nc = trim(adjustl(var_name_nc))
    out_file = trim(adjustl(out_file))

    print *, "RTOFS Converter (Hardcoded Grid: 4320x3298)"
    print *, "Opening: ", trim(nc_file)

    ! -------------------------------------------------------------------
    ! 2. READ NETCDF DATA
    ! -------------------------------------------------------------------
    allocate(var_data(imax, jmax), bmap(imax, jmax))

    iret = nf90_open(trim(nc_file), NF90_NOWRITE, ncid)
    if (iret /= nf90_noerr) stop "Error: Cannot open NetCDF"

    iret = nf90_inq_varid(ncid, trim(var_name_nc), varid)
    if (iret /= nf90_noerr) stop "Error: Variable not found"

    ! Direct read into 2D array
    iret = nf90_get_var(ncid, varid, var_data)
    if (iret /= nf90_noerr) stop "Error: Failed to read data"
    
    iret = nf90_close(ncid)

    ! -------------------------------------------------------------------
    ! 3. UNIT CONVERSION & MASKING
    ! -------------------------------------------------------------------
    if (trim(var_name_nc) == "sst" .or. trim(var_name_nc) == "temp") then
        var_data = var_data + 273.15
        where (var_data > 372.15 .or. var_data < 200.0)
            bmap = .false.
        elsewhere
            bmap = .true.
        endwhere
    else
        where (var_data > 999.0)
            bmap = .false.
        elsewhere
            bmap = .true.
        endwhere
    endif

    ! -------------------------------------------------------------------
    ! 4. INITIALIZE GRIB2 MESSAGE
    ! -------------------------------------------------------------------
    allocate(cgrib(ngrdpts*4)) 
    lugb = 50
    call baopenw(lugb, trim(out_file), ierr)

    listsec0 = (/ 10, 2 /) 
    listsec1 = (/ 7, 0, 2, 1, 1, iyr, imo, iday, icycle, 0, 0, 0, 1 /)
    call gribcreate(cgrib, size(cgrib), listsec0, listsec1, ierr)

    ! -------------------------------------------------------------------
    ! 5. DEFINE GRID
    ! -------------------------------------------------------------------
    lat1 = lat0 + dlat * (jmax - 1)
    lon1 = lon0 + dlon * (imax - 1)
    
    igds = (/ 0, ngrdpts, 0, 0, 0 /)
    igdstmpl = 0
    igdstmpl(1)  = 6 
    igdstmpl(8)  = imax
    igdstmpl(9)  = jmax
    igdstmpl(12) = nint(lat0 * 1000000)
    igdstmpl(13) = nint(lon0 * 1000000)
    igdstmpl(14) = 48
    igdstmpl(15) = nint(lat1 * 1000000)
    igdstmpl(16) = nint(lon1 * 1000000)
    igdstmpl(17) = nint(dlat * 1000000)
    igdstmpl(18) = nint(dlon * 1000000)
    igdstmpl(19) = 64
    
    call addgrid(cgrib, size(cgrib), igds, igdstmpl, igdstmplen, 0, 0, ierr)

    ! -------------------------------------------------------------------
    ! 6. DEFINE PRODUCT & PACK
    ! -------------------------------------------------------------------
    ipdsnum = 0 
    ipdstmpl = 0
    ipdstmpl(1)  = p_cat   
    ipdstmpl(2)  = parm    
    ipdstmpl(3)  = gen_pro 
    ipdstmpl(9)  = fcsthr
    ipdstmpl(10) = 160 
    ipdstmpl(12) = depth

    idrsnum = 0 
    idrstmpl = (/ 0, 0, 2, 0, 0 /) 

    call addfield(cgrib, size(cgrib), ipdsnum, ipdstmpl, ipdstmplen, &
                  0, 0, idrsnum, idrstmpl, idrstmplen, &
                  var_data, ngrdpts, 0, bmap, ierr)

    ! -------------------------------------------------------------------
    ! 7. FINALIZE
    ! -------------------------------------------------------------------
    call gribend(cgrib, size(cgrib), lengrib, ierr)
    if (ierr == 0) then
        call wryte(lugb, lengrib, cgrib)
        print *, "Successfully packed GRIB2 message."
    endif

    call baclose(lugb, ierr)
    print *, "DONE."

end program rtofs_nc_to_grib2
