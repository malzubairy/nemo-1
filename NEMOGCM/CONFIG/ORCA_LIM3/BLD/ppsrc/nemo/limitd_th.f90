

MODULE limitd_th
   !!======================================================================
   !!                       ***  MODULE limitd_th ***
   !!              Thermodynamics of ice thickness distribution
   !!                   computation of changes in g(h)      
   !!======================================================================
   !! History :   -   !          (W. H. Lipscomb and E.C. Hunke) CICE (c) original code
   !!            3.0  ! 2005-12  (M. Vancoppenolle) adaptation to LIM-3
   !!             -   ! 2006-06  (M. Vancoppenolle) adaptation to include salt, age and types
   !!             -   ! 2007-04  (M. Vancoppenolle) Mass conservation checked
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default option            Dummy module         NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_itd_th           ! Empty routines
   END SUBROUTINE lim_itd_th
   SUBROUTINE lim_itd_th_ini
   END SUBROUTINE lim_itd_th_ini
   SUBROUTINE lim_itd_th_rem
   END SUBROUTINE lim_itd_th_rem
   SUBROUTINE lim_itd_fitline
   END SUBROUTINE lim_itd_fitline
   SUBROUTINE lim_itd_shiftice
   END SUBROUTINE lim_itd_shiftice
   SUBROUTINE lim_itd_th_reb
   END SUBROUTINE lim_itd_th_reb
   !!======================================================================
END MODULE limitd_th
