PROGRAM atmforcing_correct
  !
  ! Correct GFS near-surface air temperature from forcing.airtmp.[ab] file
  ! GFS surface temperature is read from forcing.surtmp.[ab] and air
  ! temperature is corrected where surface temperature is below freezing. 
  ! The result is written to forcing.airtem.[ab]
  !
  ! use mod_xc  ! HYCOM communication interface
  USE mod_za,ONLY : xcspmd,zaiost,zaiopf,zaiowr,zaiocl,zaiord,idm,jdm
  USE mod_hycomio1,ONLY : mask_hycom_1
  !
  IMPLICIT NONE
  REAL, PARAMETER :: T_f=-1.8 ! Freezing temperature
  REAL :: fldmin,fldmax,time
  REAL, DIMENSION(:,:), ALLOCATABLE :: T_a_star,T_a,T_s
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: imsk,ip
  INTEGER, PARAMETER :: lua=77,lub=lua+1001,luaa=15,luas=16
  INTEGER :: m,n,ihead,num_records,level1,num,iheada,iheads
  CHARACTER(LEN=79), DIMENSION(:), ALLOCATABLE :: preambl
  CHARACTER(LEN=20) :: labela,labels
  CHARACTER(LEN=16) :: airt='forcing.airtmp.b',surt='forcing.surtmp.b'
  !
  ! Read input parameters
  READ(5,*) ihead
  READ(5,*) num_records
  READ(5,'(a)') labela
  READ(5,'(a)') labels
  WRITE(*,*) 'ihead,num_records=',ihead,num_records
  !
  ! Get grid parameters and allocate arrays.
  CALL xcspmd  !input idm,jdm by use association
  m=idm ; n=jdm
  ALLOCATE ( T_a_star(idm,jdm),imsk(idm,jdm),ip(idm,jdm) &
       ,T_a(idm,jdm),T_s(idm,jdm) ,preambl(ihead) )
  !
  ! Initialize I/O uints.
  CALL zaiost ! initialize units
  !
  ! Read HYCOM mask from regional.depth.[ab]
  CALL mask_hycom_1(imsk)
  WRITE(*,*) 'imsk min, max = ',MINVAL(imsk),MAXVAL(imsk)
  !
  ! Open output files and write header in 'forcing.airtmp.b file.
  CALL zaiopf('forcing.airtem.a','new' , lua)
  OPEN (unit=lub,file='forcing.airtem.b',status='new' ,action='write')
  OPEN (unit=10,file=TRIM(airt),status='old' ,action='read')
  READ (10,'(a79)') preambl
  CLOSE(10)
  ! NOTE: header will have the date of cration of the original file
  ! no mention that the field was corrected
  WRITE(lub,'(a79)') preambl
  ! write(*,'(A79)') preambl
  level1=0
  iheada=ihead ; iheads=ihead
  !
  ! Loop over records
  records: DO num=1,num_records
     !
     ! Read air and surface temperatures 
     CALL get_abfld1(luaa,imsk,T_a,m,n,labela, &
          level1,airt,iheada,time) 
     PRINT *,'surtmp num=',num
     CALL get_abfld1(luas,imsk,T_s,m,n,labels, &
          level1,surt,iheads,time)  
     IF (num==num_records) THEN 
        CALL zaiocl(luaa)
        CALL zaiocl(luas)
     ENDIF
     !
     ! Compute temperature correction.
     WHERE(T_s > T_f)
        T_a_star=T_a
     ELSEWHERE
        T_a_star=T_f + T_a-T_s 
     END WHERE
     !
     ! Write corrected air temperature.
     CALL zaiowr(T_a_star,imsk,.TRUE., fldmin,fldmax, lua,.FALSE.)
     WRITE(lub,'(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
          &       '   '//'airtem',time,fldmin,fldmax
     !
  ENDDO records
  !
  CLOSE(lub)
  CALL zaiocl(lua)
  !
  DEALLOCATE (T_a)
  DEALLOCATE (T_s)
  DEALLOCATE (T_a_star)
  DEALLOCATE(imsk)
  DEALLOCATE(ip)
  DEALLOCATE(preambl)
  !
END PROGRAM atmforcing_correct
