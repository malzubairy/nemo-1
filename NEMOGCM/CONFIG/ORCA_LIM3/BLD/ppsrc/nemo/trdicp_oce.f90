

MODULE trdicp_oce
   !!======================================================================
   !!                   ***  MODULE trdicp_oce  ***
   !! Ocean trends :   set tracer and momentum trend variables
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   'key_trdtra'   or                         tracer trends diagnostics
   !!   'key_trddyn'                            momentum trends diagnostics
   !!----------------------------------------------------------------------
   USE par_oce                 ! ocean parameters

   IMPLICIT NONE
   PUBLIC

   LOGICAL, PARAMETER ::   lk_trdtra = .FALSE.   !: tracers  trend flag
   LOGICAL, PARAMETER ::   lk_trddyn = .FALSE.   !: momentum trend flag

   !                                        !!! => tracer trends indexes <=
   INTEGER, PARAMETER ::   jpicpt_xad =  1   !: x- horizontal advection
   INTEGER, PARAMETER ::   jpicpt_yad =  2   !: y- horizontal advection
   INTEGER, PARAMETER ::   jpicpt_zad =  3   !: z- vertical   advection
   INTEGER, PARAMETER ::   jpicpt_ldf =  4   !: lateral       diffusion
   INTEGER, PARAMETER ::   jpicpt_zdf =  5   !: vertical diffusion (Kz)
   INTEGER, PARAMETER ::   jpicpt_bbc =  6   !: Bottom Boundary Condition (geoth. flux) 
   INTEGER, PARAMETER ::   jpicpt_bbl =  7   !: Bottom Boundary Layer (diffusive/convective)
   INTEGER, PARAMETER ::   jpicpt_npc =  8   !: static instability mixing
   INTEGER, PARAMETER ::   jpicpt_dmp =  9   !: damping
   INTEGER, PARAMETER ::   jpicpt_qsr = 10   !: penetrative solar radiation
   INTEGER, PARAMETER ::   jpicpt_nsr = 11   !: non solar radiation
   INTEGER, PARAMETER ::   jpicpt_zl1 = 12   !: first level vertical flux

   !                                        !!! => Total tracer trends indexes <=
   INTEGER, PARAMETER ::   jptot_tra  = 12   !: change it when adding/removing one indice above
   
   !                                        !!! => dynamic trends indexes <=
   INTEGER, PARAMETER ::   jpicpd_hpg =  1   !: hydrostatic pressure gradient 
   INTEGER, PARAMETER ::   jpicpd_keg =  2   !: kinetic energy gradient
   INTEGER, PARAMETER ::   jpicpd_rvo =  3   !: relative vorticity
   INTEGER, PARAMETER ::   jpicpd_pvo =  4   !: planetary vorticity
   INTEGER, PARAMETER ::   jpicpd_ldf =  5   !: lateral diffusion
   INTEGER, PARAMETER ::   jpicpd_had =  6   !: horizontal advection
   INTEGER, PARAMETER ::   jpicpd_zad =  7   !: vertical advection
   INTEGER, PARAMETER ::   jpicpd_zdf =  8   !: vertical diffusion
   INTEGER, PARAMETER ::   jpicpd_spg =  9   !: surface pressure gradient
   INTEGER, PARAMETER ::   jpicpd_dat = 10   !: damping term
   INTEGER, PARAMETER ::   jpicpd_swf = 11   !: surface wind forcing
   INTEGER, PARAMETER ::   jpicpd_bfr = 12   !: bottom friction 

   !                                        !!! => Total dynamic trends indexes <=
   INTEGER, PARAMETER ::   jptot_dyn  = 12   !: change it when adding/removing one indice above
   
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdicp_oce.F90 2528 2010-12-27 17:33:53Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trdicp_oce
