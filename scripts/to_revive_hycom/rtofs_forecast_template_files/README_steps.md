# Steps to set up a forecast (experiment):

1. `make_exp_dir.sh yyyymmdd`.
    If needed change following in top portion:
    - ptmp_path

2. Change `limits`: Need start/end dates of forecast:
   `grep 'dtime' newrestart_withIce.b`: start time.
   for Forecast 1: end time = start time +0.5 (days).

3. Restarts:
   - Ocean:
     - [Forecast 1: 0.5-day]:
       ```
       cd ${exp_dir}
       ln -s /lfs/h2/emc/couple/noscrub/santha.akella/tmp_11Apr2025/newrestart_withIce.a restart_in.a
       ln -s /lfs/h2/emc/couple/noscrub/santha.akella/tmp_11Apr2025/newrestart_withIce.b restart_in.b
       ```
   - Sea ice:
     - [Forecast 1]:
       - `get_ice_restart.sh`
       - Edit cice.restart_file so the name matches.

4. Forcing: `fix_forcings.sh yyyymmdd`

5. cd ${exp_dir}
   ```
   mkdir ../archive
   mkdir ../restart

   qsub job_card
   ```

6. Once job finishes ok, 
   ```
   mv archive archive.${run_date}
   mv restart restart.${run_date}
   ```

7. For next day(s), repeat above steps.
   - Exceptions to above in Steps 2 and 3:
     - Step 2/Limits
       Same as in (above step 2), but instead of using `newrestart_withIce.b`, use restart from below.
     - Step 3/Restart
       - ocean: link to the latest (figure out from dtime of the b file) restart_${run_date}/restart_out.[a,b] as in 3. 
       - sea ice: Put the name of the latest sea ice restart that's in: restart_${run_date}/ into cice.restart_file
