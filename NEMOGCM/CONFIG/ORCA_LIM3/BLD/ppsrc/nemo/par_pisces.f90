

MODULE par_pisces
   !!======================================================================
   !!                        ***  par_pisces  ***
   !! TOP :   set the PISCES parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_pisces.F90 3295 2012-01-30 15:49:07Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_lobster, ONLY : jp_lobster      !: number of tracers in LOBSTER
   USE par_lobster, ONLY : jp_lobster_2d   !: number of 2D diag in LOBSTER
   USE par_lobster, ONLY : jp_lobster_3d   !: number of 3D diag in LOBSTER
   USE par_lobster, ONLY : jp_lobster_trd  !: number of biological diag in LOBSTER

   IMPLICIT NONE

   INTEGER, PUBLIC, PARAMETER ::   jp_lp      = jp_lobster      !: cumulative number of already defined TRC
   INTEGER, PUBLIC, PARAMETER ::   jp_lp_2d   = jp_lobster_2d   !:
   INTEGER, PUBLIC, PARAMETER ::   jp_lp_3d   = jp_lobster_3d   !:
   INTEGER, PUBLIC, PARAMETER ::   jp_lp_trd  = jp_lobster_trd  !:

   !!---------------------------------------------------------------------
   !!   Default                                   No CFC geochemical model
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_pisces     = .FALSE.  !: CFC flag 
   LOGICAL, PUBLIC, PARAMETER ::   lk_kriest     = .FALSE.  !: Kriest flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces     =  0       !: No CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_2d  =  0       !: No CFC additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_3d  =  0       !: No CFC additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_trd =  0       !: number of sms trends for PISCES

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0     = jp_lp + 1                  !: First index of PISCES tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1     = jp_lp + jp_pisces          !: Last  index of PISCES tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_2d  = jp_lp_2d + 1               !: First index of 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_2d  = jp_lp_2d + jp_pisces_2d    !: Last  index of 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_3d  = jp_lp_3d + 1               !: First index of 3D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_3d  = jp_lp_3d + jp_pisces_3d    !: Last  index of 3d diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_trd = jp_lp_trd + 1              !: First index of bio diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_trd = jp_lp_trd + jp_pisces_trd  !: Last  index of bio diag


   !!======================================================================
END MODULE par_pisces
