

MODULE trdmld_trc
   !!======================================================================
   !!                       ***  MODULE  trdmld_trc  ***
   !! Ocean diagnostics:  mixed layer passive tracer trends 
   !!======================================================================
   !! History :  9.0  !  06-08  (C. Deltel)  Original code (from trdmld.F90)
   !!                 !  07-04  (C. Deltel)  Bug fix : add trcrad trends
   !!                 !  07-06  (C. Deltel)  key_gyre : do not call lbc_lnk
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default option :                                       Empty module
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trd_mld_trc( kt )                                   ! Empty routine
      INTEGER, INTENT( in) ::   kt
      WRITE(*,*) 'trd_mld_trc: You should not have seen this print! error?', kt
   END SUBROUTINE trd_mld_trc
   SUBROUTINE trd_mld_bio( kt )
      INTEGER, INTENT( in) ::   kt
      WRITE(*,*) 'trd_mld_bio: You should not have seen this print! error?', kt
   END SUBROUTINE trd_mld_bio
   SUBROUTINE trd_mld_trc_zint( ptrc_trdmld, ktrd, ctype, kjn )
      INTEGER               , INTENT( in ) ::  ktrd, kjn              ! ocean trend index and passive tracer rank
      CHARACTER(len=2)      , INTENT( in ) ::  ctype                  ! surface/bottom (2D) or interior (3D) physics
      REAL, DIMENSION(:,:,:), INTENT( in ) ::  ptrc_trdmld            ! passive trc trend
      WRITE(*,*) 'trd_mld_trc_zint: You should not have seen this print! error?', ptrc_trdmld(1,1,1)
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', ctype
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', ktrd
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kjn
   END SUBROUTINE trd_mld_trc_zint
   SUBROUTINE trd_mld_trc_init                                    ! Empty routine
      WRITE(*,*) 'trd_mld_trc_init: You should not have seen this print! error?'
   END SUBROUTINE trd_mld_trc_init

   !!======================================================================
END MODULE trdmld_trc
