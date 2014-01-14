

MODULE trcexp
   !!======================================================================
   !!                         ***  MODULE p4sed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :    -   !  1999    (O. Aumont, C. Le Quere)  original code
   !!              -   !  2001-05 (O. Aumont, E. Kestenare) add sediment computations
   !!             1.0  !  2005-06 (A.-S. Kremeur) new temporal integration for sedpoc
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_exp( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_exp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_exp

   !!======================================================================
END MODULE  trcexp
