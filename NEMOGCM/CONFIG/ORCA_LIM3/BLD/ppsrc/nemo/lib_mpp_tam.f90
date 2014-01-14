

MODULE lib_mpp_tam
   !!======================================================================
   !!                       ***  MODULE  lib_mpp_tam  ***
   !! Ocean numerics:  massively parallel processing library
   !!=====================================================================
   !! History :  OPA  !  1994  (M. Guyon, J. Escobar, M. Imbard)  Original code
   !!            7.0  !  1997  (A.M. Treguier)  SHMEM additions
   !!            8.0  !  1998  (M. Imbard, J. Escobar, L. Colombet ) SHMEM and MPI
   !!                 !  1998  (J.M. Molines) Open boundary conditions
   !!   NEMO     1.0  !  2003  (J.-M. Molines, G. Madec)  F90, free form
   !!                 !  2003  (J.M. Molines) add mpp_ini_north(_3d,_2d)
   !!             -   !  2004  (R. Bourdalle Badie)  isend option in mpi
   !!                 !  2004  (J.M. Molines) minloc, maxloc
   !!             -   !  2005  (G. Madec, S. Masson)  npolj=5,6 F-point & ice cases
   !!             -   !  2005  (R. Redler) Replacement of MPI_COMM_WORLD except for MPI_Abort
   !!             -   !  2005  (R. Benshila, G. Madec)  add extra halo case
   !!             -   !  2008  (R. Benshila) add mpp_ini_ice
   !!            3.2  !  2009  (R. Benshila) SHMEM suppression, north fold in lbc_nfd
   !!            3.2  !  2009  (O. Marti)    add mpp_ini_znl
   !!            4.0  !  2011  (G. Madec)  move ctl_ routines from in_out_manager
   !!   NEMOTAM  2.?  !  2007  (K. Mogensen) Original code (lib_mppadj)
   !!            3.0  !  2009  (A. Vidard) nemo v3 update
   !!            3.2  !  2010  (A. Vidard) 3.2 version, complete rewrite
   !!            3.4  !  2012  (P.-A. Bouttier) v3.4 update
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   ctl_stop   : update momentum and tracer Kz from a tke scheme
   !!   ctl_warn   : initialization, namelist read, and parameters control
   !!   ctl_opn    : Open file and check if required file is available.
   !!   get_unit    : give the index of an unused logical unit
   !!----------------------------------------------------------------------
   USE in_out_manager
   PUBLIC lib_mpp_alloc_adj
CONTAINS
    INTEGER FUNCTION lib_mpp_alloc_adj(kumout)          ! Dummy function
      INTEGER, INTENT(in) ::   kumout
      lib_mpp_alloc_adj = 0
   END FUNCTION lib_mpp_alloc_adj
   FUNCTION mpp_sum_nfd_4d( pval, kn1, kn2, kn3, kn4, kcom )
      !! * Function return
      REAL(wp), DIMENSION(kn1, kn2, kn3, kn4) :: mpp_sum_nfd_4d
      !! * Arguments
      INTEGER, INTENT(IN) :: kn1, kn2, kn3, kn4
      REAL(wp), DIMENSION(kn1, kn2, kn3, kn4), INTENT(IN) :: pval
      INTEGER , INTENT( in ), OPTIONAL        :: kcom
      mpp_sum_nfd_4d = 0.0_wp
      WRITE(*,*) 'mpp_sum_nfd_4d: You should not have seen this print! error?'
   END FUNCTION mpp_sum_nfd_4d
   FUNCTION mpp_sum_nfd_3d( pval, kn1, kn2, kn3, kcom )
      !! * Function return
      REAL(wp), DIMENSION(kn1, kn2, kn3) :: mpp_sum_nfd_3d
      !! * Arguments
      INTEGER, INTENT(IN) :: kn1, kn2, kn3
      REAL(wp), DIMENSION(kn1, kn2, kn3), INTENT(IN) :: pval
      INTEGER , INTENT( in ), OPTIONAL        :: kcom
      mpp_sum_nfd_3d = 0.0_wp
      WRITE(*,*) 'mpp_sum_nfd_3d: You should not have seen this print! error?'
   END FUNCTION mpp_sum_nfd_3d
   !!----------------------------------------------------------------------
END MODULE lib_mpp_tam
