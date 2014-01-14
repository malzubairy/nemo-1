

MODULE par_lobster
   !!======================================================================
   !!                        ***  par_lobster  ***
   !! TOP :   set the LOBSTER parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_lobster.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   IMPLICIT NONE

   !!---------------------------------------------------------------------
   !!   Default                                           No LOBSTER model
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_lobster     = .FALSE.   !: LOBSTER flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_lobster     =  0        !: No LOBSTER tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_lobster_2d  =  0        !: No LOBSTER additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_lobster_3d  =  0        !: No LOBSTER additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_lobster_trd =  0        !: number of sms trends for LOBSTER

   ! Starting/ending LOBSTER do-loop indices (N.B. no LOBSTER : jpl_lob < jpf_lob the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_lob0     =          1       !: First index of LOBSTER tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_lob1     = jp_lobster       !: Last  index of LOBSTER tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_lob0_2d  =          1       !: First index of LOBSTER 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_lob1_2d  = jp_lobster_2d    !: Last  index of LOBSTER 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_lob0_3d  =          1       !: First index of LOBSTER 3D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_lob1_3d  = jp_lobster_3d    !: Last  index of LOBSTER 3D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_lob0_trd =          1       !: First index of LOBSTER bio. diag
   INTEGER, PUBLIC, PARAMETER ::   jp_lob1_trd = jp_lobster_trd   !: Last  index of LOBSTER bio. diag

   !!======================================================================
END MODULE par_lobster
