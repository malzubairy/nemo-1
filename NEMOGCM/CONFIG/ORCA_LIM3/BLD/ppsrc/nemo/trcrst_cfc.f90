

MODULE trcrst_cfc
   !!======================================================================
   !!                       ***  MODULE trcrst_cfc  ***
   !! TOP :   create, write, read the restart files of CFC tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe) Original
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_cfc( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_wri_cfc: You should not have seen this print! error?', knum
   END SUBROUTINE trc_rst_read_cfc

   SUBROUTINE trc_rst_wri_cfc( kt, kitrst, knum )
     INTEGER, INTENT(in)  :: kt, kitrst, knum
     WRITE(*,*) 'trc_rst_wri_cfc: You should not have seen this print! error?', kt, kitrst, knum
   END SUBROUTINE trc_rst_wri_cfc

   !!======================================================================
END MODULE trcrst_cfc
