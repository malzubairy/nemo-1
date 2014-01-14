

MODULE trcbio
   !!======================================================================
   !!                         ***  MODULE trcbio  ***
   !! TOP :   LOBSTER
   !!======================================================================
   !! History :    -   !  1999-07  (M. Levy) Original code
   !!              -   !  2000-12  (E. Kestenare) assign a parameter to name individual tracers
   !!              -   !  2001-03  (M. Levy)  LNO3 + dia2d 
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_bio( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_bio: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bio

   !!======================================================================
END MODULE  trcbio
