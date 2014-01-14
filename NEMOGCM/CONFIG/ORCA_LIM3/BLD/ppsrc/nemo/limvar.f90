

MODULE limvar
   !!======================================================================
   !!                       ***  MODULE limvar ***
   !!                 Different sets of ice model variables 
   !!                   how to switch from one to another
   !!
   !!                 There are three sets of variables
   !!                 VGLO : global variables of the model
   !!                        - v_i (jpi,jpj,jpl)
   !!                        - v_s (jpi,jpj,jpl)
   !!                        - a_i (jpi,jpj,jpl)
   !!                        - t_s (jpi,jpj,jpl)
   !!                        - e_i (jpi,jpj,nlay_i,jpl)
   !!                        - smv_i(jpi,jpj,jpl)
   !!                        - oa_i (jpi,jpj,jpl)
   !!                 VEQV : equivalent variables sometimes used in the model
   !!                        - ht_i(jpi,jpj,jpl)
   !!                        - ht_s(jpi,jpj,jpl)
   !!                        - t_i (jpi,jpj,nlay_i,jpl)
   !!                        ...
   !!                 VAGG : aggregate variables, averaged/summed over all
   !!                        thickness categories
   !!                        - vt_i(jpi,jpj)
   !!                        - vt_s(jpi,jpj)
   !!                        - at_i(jpi,jpj)
   !!                        - et_s(jpi,jpj)  !total snow heat content
   !!                        - et_i(jpi,jpj)  !total ice thermal content 
   !!                        - smt_i(jpi,jpj) !mean ice salinity
   !!                        - ot_i(jpi,jpj)  !average ice age
   !!======================================================================
   !! History :   -   ! 2006-01 (M. Vancoppenolle) Original code
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default option         Dummy module          NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_var_agg          ! Empty routines
   END SUBROUTINE lim_var_agg
   SUBROUTINE lim_var_glo2eqv      ! Empty routines
   END SUBROUTINE lim_var_glo2eqv
   SUBROUTINE lim_var_eqv2glo      ! Empty routines
   END SUBROUTINE lim_var_eqv2glo
   SUBROUTINE lim_var_salprof      ! Empty routines
   END SUBROUTINE lim_var_salprof
   SUBROUTINE lim_var_bv           ! Emtpy routines
   END SUBROUTINE lim_var_bv
   SUBROUTINE lim_var_salprof1d    ! Emtpy routines
   END SUBROUTINE lim_var_salprof1d

   !!======================================================================
END MODULE limvar
