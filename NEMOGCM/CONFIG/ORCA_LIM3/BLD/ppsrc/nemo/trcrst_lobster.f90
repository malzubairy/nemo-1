

MODULE trcrst_lobster
   !!======================================================================
   !!                       ***  MODULE trcrst_lobster  ***
   !! TOP :   create, write, read the restart files of LOBSTER tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe) Original
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_lobster( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_wri_lobster: You should not have seen this print! error?',knum
   END SUBROUTINE trc_rst_read_lobster

   SUBROUTINE trc_rst_wri_lobster( kt, kitrst, knum )
     INTEGER, INTENT(in)  :: kt, kitrst, knum
     WRITE(*,*) 'trc_rst_wri_lobster: You should not have seen this print! error?', kt, kitrst, knum
   END SUBROUTINE trc_rst_wri_lobster

   !!======================================================================
END MODULE trcrst_lobster
