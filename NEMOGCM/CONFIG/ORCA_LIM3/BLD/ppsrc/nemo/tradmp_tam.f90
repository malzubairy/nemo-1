

MODULE tradmp_tam
   !!----------------------------------------------------------------------
   !!   Default key                                     NO internal damping
   !!----------------------------------------------------------------------
   LOGICAL , PUBLIC, PARAMETER ::   lk_tradmp = .FALSE.    !: internal damping flag
CONTAINS
   SUBROUTINE tra_dmp_tan( kt )        ! Empty routine
      WRITE(*,*) 'tra_dmp_tan: You should not have seen this print! error?', kt
   END SUBROUTINE tra_dmp_tan
   SUBROUTINE tra_dmp_adj( kt )        ! Empty routine
      WRITE(*,*) 'tra_dmp_adj: You should not have seen this print! error?', kt
   END SUBROUTINE tra_dmp_adj
   SUBROUTINE tra_dmp_adj_tst( kt )        ! Empty routine
      WRITE(*,*) 'tra_dmp_adj_tst: You should not have seen this print! error?', kt
   END SUBROUTINE tra_dmp_adj_tst

   !!======================================================================
END MODULE tradmp_tam
