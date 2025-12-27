1. Prepare input(s) to: `rtofs_nc2grib2`:

- Use `rtofs_glo_2ds_f000_prog.nc` on variable: `sst`
- If you need a sample netcdf file, grab from prod:
  `cp /lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20251227/rtofs_glo_2ds_f000_prog.nc .`
- Then run the py script that converts to ascii: `fort.20` that the `rtofs_nc2grib2` will read.

```
./nc_to_fort20.py data/rtofs_glo_2ds_f000_prog.nc sst -o fort.20
```

Python modules: use [https://github.com/NOAA-EMC/RTOFS_GLO/blob/develop/scripts/UFS_helpers/set_py_modules.sh.]()

2. Since RTOFS/sorc will be built, no need for to build: `rtofs_nc2grib2` explicitly.
3. Run: `run_nc2grib.sh`. Got: `rtofs_output.grb2` - then all done!
4. (optional) you can check validity of .grb2 file using "wgrib2 -v "
