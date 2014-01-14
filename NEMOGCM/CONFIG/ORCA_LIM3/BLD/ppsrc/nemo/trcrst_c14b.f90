

MODULE trcrst_c14b
   !!======================================================================
   !!                       ***  MODULE trcrst_c14b  ***
   !! TOP :   create, write, read the restart files of c14b tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe) Original
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_c14b( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_wri_c14b: You should not have seen this print! error?', knum
   END SUBROUTINE trc_rst_read_c14b

   SUBROUTINE trc_rst_wri_c14b( kt, kitrst, knum )
     INTEGER, INTENT(in)  :: kt, kitrst, knum
     WRITE(*,*) 'trc_rst_wri_c14b: You should not have seen this print! error?', kt, kitrst, knum
   END SUBROUTINE trc_rst_wri_c14b

   !!======================================================================
END MODULE trcrst_c14b
