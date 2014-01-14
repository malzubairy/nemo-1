

MODULE mpp_map
   !!======================================================================
   !!                       ***  MODULE mpp_mpa  ***
   !! NEMOVAR: MPP global grid point mapping to processors
   !!======================================================================
   !! History :  2.0  ! 2007-08  (K. Mogensen)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  mppmap_init : Initialize mppmap.
   !!----------------------------------------------------------------------
   USE par_kind, ONLY :   wp            ! Precision variables
   USE par_oce , ONLY :   jpi, jpj      ! Ocean parameters
   USE dom_oce , ONLY :   mig, mjg, nldi, nlei, nldj, nlej, narea   ! Ocean space and time domain variables



   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC ::   mppmap_init, mppmap   !: ???

   INTEGER, DIMENSION(:,:), ALLOCATABLE ::   mppmap   ! ???

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: mpp_map.F90 2363 2010-11-05 13:11:29Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE mppmap_init
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE mppmap_init ***
      !!          
      !! ** Purpose : Setup a global map of processor rank for all gridpoints
      !!
      !! ** Method  : MPI all reduce.
      !!
      !! ** Action  : This does only work for MPI. 
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::   imppmap   !





      !!----------------------------------------------------------------------

      ALLOCATE( &
         & mppmap(jpiglo,jpjglo) &
         & )

      ! Initialize local imppmap

      ALLOCATE( &
         & imppmap(jpiglo,jpjglo) &
         & )
      imppmap(:,:) = 0

      ! Setup local grid points
      imppmap(mig(nldi):mig(nlei),mjg(nldj):mjg(nlej)) = narea 
      
      ! Get global data







      
      ! No MPP: Just copy the data
      mppmap(:,:) = imppmap(:,:)

      !
   END SUBROUTINE mppmap_init

   !!======================================================================
END MODULE mpp_map
