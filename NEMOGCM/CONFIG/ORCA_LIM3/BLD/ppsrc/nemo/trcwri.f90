

MODULE trcwri
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    TOP :   Output of passive tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
   PUBLIC trc_wri
CONTAINS
   SUBROUTINE trc_wri( kt )                     ! Empty routine   
   INTEGER, INTENT(in) :: kt
   END SUBROUTINE trc_wri

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcwri.F90 3295 2012-01-30 15:49:07Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trcwri
