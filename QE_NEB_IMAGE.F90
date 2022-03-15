PROGRAM neb_image_coord
!------------------------------------------------------!
! THIS FORTRAN PROGRAM CREATES COORDINATES DESCRIPTION
! OF NEB CALCULATION FOR QUANTUM ESPRESSO PACKAGE
! Requirements :
! Relax atoms cartesian file [relax.xyz]
! Fix atoms cartesian file   [fix.xyz]
! Written by -- Rahul Verma
!------------------------------------------------------!
IMPLICIT NONE
INTEGER :: i,j,n,m,t
INTEGER :: f_steps, r_steps,nn,mm,ios
REAL*8,ALLOCATABLE  :: x(:),y(:),z(:)
INTEGER,ALLOCATABLE :: fix(:,:)
CHARACTER*5,ALLOCATABLE :: sym(:)
CHARACTER*10 :: filename
LOGICAL :: file_exist

! Relax coorinates xyz file
OPEN(11, FILE='relax.xyz', STATUS='old', IOSTAT=ios)
IF (ios .ne. 0) STOP "RELAX ATOMS, relax.xyz file not found"
! Fix atoms coordinates files
OPEN(12, FILE='fix.xyz', STATUS='old', IOSTAT=ios)
IF (ios .ne. 0) STOP "FIX ATOMS, fix.xyz file not found"

INQUIRE(FILE='traj.xyz', EXIST=file_exist)
IF (file_exist .eqv. .TRUE.) STOP "!! traj.xyz FILE EXIST"
OPEN(13, FILE='traj.xyz', STATUS='NEW', ACCESS='APPEND')

! Read filename to write coordinates of neb images
PRINT*,'ENTER THE OUTPUT FILENAME'
READ(5,*)filename
INQUIRE(FILE=TRIM(filename), EXIST=file_exist)
IF (file_exist .eqv. .TRUE.) STOP "!! OUTPUT FILE EXIST"
OPEN(14, FILE=TRIM(filename), STATUS='NEW')

READ(11,*)n
READ(12,*)m
101 FORMAT (A,10X,3F10.6,2X,A)
102 FORMAT (A,10X,3F10.6,2X,3I2)
t = n+m
ALLOCATE (sym(t))
ALLOCATE (fix(t,3))
ALLOCATE(x(t),y(t),z(t))

CALL get_steps(11,f_steps)
CALL get_steps(12,r_steps)
nn = f_steps/(n+2)
mm = r_steps/(m+2)


DO i = 1,nn
READ(11,*)
READ(11,*)
WRITE(13,'(I3)')t
WRITE(13,'(A)')'Generated Using FORTRAN Program'
  DO j = 1,n
    READ(11,*) sym(j),x(j),y(j),z(j)
   WRITE(13,101) sym(j),x(j),y(j),z(j),'1 1 1' ! 1 --> relax coordinates along x y z
  ENDDO
READ(12,*)
READ(12,*)
  DO j = 1,m
    READ(12,*) sym(j),x(j),y(j),z(j)
   WRITE(13,101) sym(j),x(j),y(j),z(j),'0 0 0' ! 0 --> fix coordinates along x y z
  ENDDO
ENDDO
CLOSE(13)

INQUIRE(FILE='traj.xyz', EXIST=file_exist)
IF (file_exist .eqv. .FALSE.) STOP "!! traj.xyz FILE NOT EXIST"
OPEN(13, FILE='traj.xyz', STATUS='OLD')
CALL get_steps(13,f_steps)
n = f_steps/(t+2)
DO i = 1,n
READ(13,*)
READ(13,*)
 DO j = 1,t
  READ(13,*) sym(j),x(j),y(j),z(j),fix(j,1:3)
 ENDDO
  IF(i .eq. 1) THEN
     WRITE(14,'(A)')'BEGIN_POSITIONS'
     WRITE(14,'(A)')'FIRST_IMAGE'
     WRITE(14,'(A)')'ATOMIC_POSITIONS {angstrom}'
    DO j = 1,t
      WRITE(14,102) sym(j),x(j),y(j),z(j),fix(j,1:3)
    ENDDO
  ELSEIF(i .gt. 1 .and. i .lt. n) THEN
     WRITE(14,'(A)')'INTERMEDIATE_IMAGE'
     WRITE(14,'(A)')'ATOMIC_POSITIONS {angstrom}'
    DO j = 1,t
      WRITE(14,102) sym(j),x(j),y(j),z(j),fix(j,1:3)
    ENDDO
  ELSEIF(i .eq. n) THEN
     WRITE(14,'(A)')'LAST_IMAGE'
     WRITE(14,'(A)')'ATOMIC_POSITIONS {angstrom}'
    DO j = 1,t
      WRITE(14,102) sym(j),x(j),y(j),z(j),fix(j,1:3)
    ENDDO
  ENDIF
ENDDO
WRITE(14,'(A)')'END_POSITIONS'
WRITE(14,'(A)')'END_ENGINE_INPUT'
WRITE(14,'(A)')'END'
CLOSE(13)
DEALLOCATE(sym,x,y,z)
WRITE(6,'(A)')'COMBINED TRAJECTORY WRITTEN IN traj.xyz FILE'
WRITE(6,'(A,5X,A,1X,A)')'NEB COORDINATES DESCRIPTION WRITTEN IN',filename,'FILE'
END PROGRAM neb_image_coord
!==============================================
SUBROUTINE get_steps(iunit,nsteps)
IMPLICIT NONE
INTEGER iunit, nsteps
INTEGER ios
nsteps=0
REWIND(iunit)
Read_Loop: DO
   READ(iunit,*,IOSTAT=ios)
   IF(ios.ne.0)EXIT Read_Loop
   nsteps=nsteps+1
END DO Read_Loop
REWIND(iunit)
END SUBROUTINE
!==============================================
