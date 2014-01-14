

MODULE trcopt
   !!======================================================================
   !!                         ***  MODULE trcopt  ***
   !! TOP :   LOBSTER Compute the light availability in the water column
   !!======================================================================
   !! History :    -   !  1995-05  (M. Levy) Original code
   !!              -   !  1999-09  (J.-M. Andre, M. Levy) 
   !!              -   !  1999-11  (C. Menkes, M.-A. Foujols) itabe initial
   !!              -   !  2000-02  (M.A. Foujols) change x**y par exp(y*log(x))
   !!   NEMO      2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!             3.2  !  2009-04  (C. Ethe, G. Madec)  minor optimisation + style
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_opt( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_opt: You should not have seen this print! error?', kt
   END SUBROUTINE trc_opt

   !!======================================================================
END MODULE  trcopt
