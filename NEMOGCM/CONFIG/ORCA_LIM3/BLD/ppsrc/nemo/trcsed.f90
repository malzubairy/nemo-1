

MODULE trcsed
   !!======================================================================
   !!                         ***  MODULE p4sed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :    -   !  1995-06 (M. Levy)  original code
   !!              -   !  2000-12 (E. Kestenare)  clean up
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90 + simplifications
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_sed( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_sed: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sed

   !!======================================================================
END MODULE  trcsed
