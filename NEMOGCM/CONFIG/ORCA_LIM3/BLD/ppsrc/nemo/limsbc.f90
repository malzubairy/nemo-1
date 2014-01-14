

MODULE limsbc
   !!======================================================================
   !!                       ***  MODULE limsbc   ***
   !!           computation of the flux at the sea ice/ocean interface
   !!======================================================================
   !! History :   -   ! 2006-07 (M. Vancoppelle)  LIM3 original code
   !!            3.0  ! 2008-03 (C. Tallandier)  surface module
   !!             -   ! 2008-04 (C. Tallandier)  split in 2 + new ice-ocean coupling
   !!            3.3  ! 2010-05 (G. Madec) decrease ocean & ice reference salinities in the Baltic sea
   !!                 !                  + simplification of the ice-ocean stress calculation
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default option :        Dummy module       NO LIM 3.0 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_sbc           ! Dummy routine
   END SUBROUTINE lim_sbc

   !!======================================================================
END MODULE limsbc
