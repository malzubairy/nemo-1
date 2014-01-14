

MODULE istate
   !!======================================================================
   !!                     ***  MODULE  istate  ***
   !! Ocean state   :  initial state setting, off-line case
   !!=====================================================================
   !! History :  3.3  ! 2010-10  (C. Ethe)  original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   istate_init   : initial state set to zero
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers 
   USE dom_oce         ! ocean space and time domain 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   istate_init   ! routine called by step.F90

   !! * Substitutions
   !!----------------------------------------------------------------------
   !!                    ***  domzgr_substitute.h90   ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute fsdep. and fse.., the vert. depth and scale
   !!      factors depending on the vertical coord. used, using CPP macro.
   !!----------------------------------------------------------------------
   !! History :  1.0  !  2005-10  (A. Beckmann, G. Madec) generalisation to all coord.
   !!            3.1  !  2009-02  (G. Madec, M. Leclair)  pure z* coordinate
   !!----------------------------------------------------------------------
! reference for s- or zps-coordinate (3D no time dependency)
! z- or s-coordinate (1D or 3D + no time dependency) use reference in all cases




   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: domzgr_substitute.h90 2528 2010-12-27 17:33:53Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!                   ***  vectopt_loop_substitute  ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute the inner loop starting and inding indices 
   !!      to allow unrolling of do-loop using CPP macro.
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: vectopt_loop_substitute.h90 2528 2010-12-27 17:33:53Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OFF 3.3 , NEMO Consortium (2010)
   !! $Id: istate.F90 2528 2010-12-27 17:33:53Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!---------------------------------------------------------------------
CONTAINS

   SUBROUTINE istate_init
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_init  ***
      !! 
      !! ** Purpose :   Initialization to zero of the dynamics and tracers.
      !!----------------------------------------------------------------------
      !
      !     now fields         !     after fields      !
      un   (:,:,:)   = 0._wp   ;   ua(:,:,:) = 0._wp   !
      vn   (:,:,:)   = 0._wp   ;   va(:,:,:) = 0._wp   !
      wn   (:,:,:)   = 0._wp   !                       !
      hdivn(:,:,:)   = 0._wp   !                       !
      tsn  (:,:,:,:) = 0._wp   !                       !
      !
      rhd  (:,:,:) = 0.e0
      rhop (:,:,:) = 0.e0
      rn2  (:,:,:) = 0.e0 
      !
   END SUBROUTINE istate_init

   !!=====================================================================
END MODULE istate
