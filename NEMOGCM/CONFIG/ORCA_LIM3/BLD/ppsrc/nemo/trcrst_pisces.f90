

MODULE trcrst_pisces
   !!======================================================================
   !!                       ***  MODULE trcrst_pisces  ***
   !! TOP :   create, write, read the restart files of PISCES tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe) Original
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_pisces( knum )
      INTEGER, INTENT(in)  :: knum
      WRITE(*,*) 'trc_rst_read_pisces: You should not have seen this print! error?', knum
   END SUBROUTINE trc_rst_read_pisces

   SUBROUTINE trc_rst_wri_pisces( kt, kitrst, knum )
     INTEGER, INTENT(in)  :: kt, kitrst, knum
     WRITE(*,*) 'trc_rst_wri_pisces: You should not have seen this print! error?', kt, kitrst, knum
   END SUBROUTINE trc_rst_wri_pisces

   !!======================================================================
END MODULE trcrst_pisces
