

MODULE trcrst_my_trc
   !!======================================================================
   !!                       ***  MODULE trcrst_my_trc  ***
   !! TOP :   create, write, read the restart files of MY_TRC tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe)  Original
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_my_trc( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_read_my_trc: You should not have seen this print! error?', knum
   END SUBROUTINE trc_rst_read_my_trc

   SUBROUTINE trc_rst_wri_my_trc( kt, kirst, knum )
     INTEGER, INTENT(in)  :: kt, kirst, knum
     WRITE(*,*) 'trc_rst_wri_my_trc: You should not have seen this print! error?', kt, kirst, knum
   END SUBROUTINE trc_rst_wri_my_trc

   !!======================================================================
END MODULE trcrst_my_trc
