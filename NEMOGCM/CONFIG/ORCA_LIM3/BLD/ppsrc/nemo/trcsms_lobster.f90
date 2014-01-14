

MODULE trcsms_lobster
   !!======================================================================
   !!                         ***  MODULE trcsms_lobster  ***
   !! TOP :   Time loop of LOBSTER model
   !!======================================================================
   !! History :   1.0  !            M. Levy
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                     No passive tracer
   !!======================================================================
CONTAINS
   SUBROUTINE trc_sms_lobster( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_sms_lobster: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sms_lobster

   !!======================================================================
END MODULE  trcsms_lobster
