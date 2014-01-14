

MODULE lbclnk
   !!======================================================================
   !!                       ***  MODULE  lbclnk  ***
   !! Ocean        : lateral boundary conditions
   !!=====================================================================
   !! History :  OPA  ! 1997-06  (G. Madec)     Original code
   !!   NEMO     1.0  ! 2002-09  (G. Madec)     F90: Free form and module
   !!            3.2  ! 2009-03  (R. Benshila)  External north fold treatment  
   !!            3.4  ! 2012-12  (R. Bourdalle-Badie and G. Reffray)  add a C1D case  
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default option                              shared memory computing
   !!----------------------------------------------------------------------
   !!   lbc_lnk      : generic interface for lbc_lnk_3d and lbc_lnk_2d
   !!   lbc_lnk_3d   : set the lateral boundary condition on a 3D variable on ocean mesh
   !!   lbc_lnk_2d   : set the lateral boundary condition on a 2D variable on ocean mesh
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers   
   USE dom_oce         ! ocean space and time domain 
   USE in_out_manager  ! I/O manager
   USE lbcnfd          ! north fold

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_lnk
      MODULE PROCEDURE lbc_lnk_3d_gather, lbc_lnk_3d, lbc_lnk_2d
   END INTERFACE

   INTERFACE lbc_lnk_e
      MODULE PROCEDURE lbc_lnk_2d
   END INTERFACE

   PUBLIC   lbc_lnk       ! ocean/ice  lateral boundary conditions
   PUBLIC   lbc_lnk_e 
   
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: lbclnk.F90 3720 2012-12-04 10:10:08Z cbricaud $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!   Default option                           3D shared memory computing
   !!----------------------------------------------------------------------

   SUBROUTINE lbc_lnk_3d_gather( pt3d1, cd_type1, pt3d2, cd_type2, psgn )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE lbc_lnk_3d_gather  ***
      !!
      !! ** Purpose :   set lateral boundary conditions on two 3D arrays (non mpp case)
      !!
      !! ** Method  :   psign = -1 :    change the sign across the north fold
      !!                      =  1 : no change of the sign across the north fold
      !!                      =  0 : no change of the sign across the north fold and
      !!                             strict positivity preserved: use inner row/column
      !!                             for closed boundaries.
      !!----------------------------------------------------------------------
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type1, cd_type2   ! nature of pt3d grid-points
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pt3d1   , pt3d2      ! 3D array on which the lbc is applied
      REAL(wp)                        , INTENT(in   ) ::   psgn                 ! control of the sign 
      !!----------------------------------------------------------------------
      !
      CALL lbc_lnk_3d( pt3d1, cd_type1, psgn)
      CALL lbc_lnk_3d( pt3d2, cd_type2, psgn)
      !
   END SUBROUTINE lbc_lnk_3d_gather


   SUBROUTINE lbc_lnk_3d( pt3d, cd_type, psgn, cd_mpp, pval )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE lbc_lnk_3d  ***
      !!
      !! ** Purpose :   set lateral boundary conditions on a 3D array (non mpp case)
      !!
      !! ** Method  :   psign = -1 :    change the sign across the north fold
      !!                      =  1 : no change of the sign across the north fold
      !!                      =  0 : no change of the sign across the north fold and
      !!                             strict positivity preserved: use inner row/column
      !!                             for closed boundaries.
      !!----------------------------------------------------------------------
      CHARACTER(len=1)                , INTENT(in   )           ::   cd_type   ! nature of pt3d grid-points
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout)           ::   pt3d      ! 3D array on which the lbc is applied
      REAL(wp)                        , INTENT(in   )           ::   psgn      ! control of the sign 
      CHARACTER(len=3)                , INTENT(in   ), OPTIONAL ::   cd_mpp    ! MPP only (here do nothing)
      REAL(wp)                        , INTENT(in   ), OPTIONAL ::   pval      ! background value (for closed boundaries)
      !!
      REAL(wp) ::   zland
      !!----------------------------------------------------------------------

      IF( PRESENT( pval ) ) THEN   ;   zland = pval      ! set land value (zero by default)
      ELSE                         ;   zland = 0._wp
      ENDIF


      IF( PRESENT( cd_mpp ) ) THEN
         ! only fill the overlap area and extra allows 
         ! this is in mpp case. In this module, just do nothing
      ELSE
         !
         !                                     !  East-West boundaries
         !                                     ! ======================
         SELECT CASE ( nperio )
         !
         CASE ( 1 , 4 , 6 )                       !**  cyclic east-west
            pt3d( 1 ,:,:) = pt3d(jpim1,:,:)            ! all points
            pt3d(jpi,:,:) = pt3d(  2  ,:,:)
            !
         CASE DEFAULT                             !**  East closed  --  West closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt3d( 1 ,:,:) = zland
               pt3d(jpi,:,:) = zland
            CASE ( 'F' )                               ! F-point
               pt3d(jpi,:,:) = zland
            END SELECT
            !
         END SELECT
         !
         !                                     ! North-South boundaries
         !                                     ! ======================
         SELECT CASE ( nperio )
         !
         CASE ( 2 )                               !**  South symmetric  --  North closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'W' )                   ! T-, U-, W-points
               pt3d(:, 1 ,:) = pt3d(:,3,:)
               pt3d(:,jpj,:) = zland
            CASE ( 'V' , 'F' )                         ! V-, F-points
               pt3d(:, 1 ,:) = psgn * pt3d(:,2,:)
               pt3d(:,jpj,:) = zland
            END SELECT
            !
         CASE ( 3 , 4 , 5 , 6 )                   !**  North fold  T or F-point pivot  --  South closed
            SELECT CASE ( cd_type )                    ! South : closed
            CASE ( 'T' , 'U' , 'V' , 'W' , 'I' )             ! all points except F-point
               pt3d(:, 1 ,:) = zland
            END SELECT
            !                                          ! North fold
            pt3d( 1 ,jpj,:) = zland
            pt3d(jpi,jpj,:) = zland
            CALL lbc_nfd( pt3d(:,:,:), cd_type, psgn )
            !
         CASE DEFAULT                             !**  North closed  --  South closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt3d(:, 1 ,:) = zland
               pt3d(:,jpj,:) = zland
            CASE ( 'F' )                               ! F-point
               pt3d(:,jpj,:) = zland
            END SELECT
            !
         END SELECT
         !
      ENDIF
      !
   END SUBROUTINE lbc_lnk_3d


   SUBROUTINE lbc_lnk_2d( pt2d, cd_type, psgn, cd_mpp, pval )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE lbc_lnk_2d  ***
      !!
      !! ** Purpose :   set lateral boundary conditions on a 2D array (non mpp case)
      !!
      !! ** Method  :   psign = -1 :    change the sign across the north fold
      !!                      =  1 : no change of the sign across the north fold
      !!                      =  0 : no change of the sign across the north fold and
      !!                             strict positivity preserved: use inner row/column
      !!                             for closed boundaries.
      !!----------------------------------------------------------------------
      CHARACTER(len=1)            , INTENT(in   )           ::   cd_type   ! nature of pt3d grid-points
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout)           ::   pt2d      ! 2D array on which the lbc is applied
      REAL(wp)                    , INTENT(in   )           ::   psgn      ! control of the sign 
      CHARACTER(len=3)            , INTENT(in   ), OPTIONAL ::   cd_mpp    ! MPP only (here do nothing)
      REAL(wp)                    , INTENT(in   ), OPTIONAL ::   pval      ! background value (for closed boundaries)
      !!
      REAL(wp) ::   zland
      !!----------------------------------------------------------------------

      IF( PRESENT( pval ) ) THEN   ;   zland = pval      ! set land value (zero by default)
      ELSE                         ;   zland = 0._wp
      ENDIF

      IF (PRESENT(cd_mpp)) THEN
         ! only fill the overlap area and extra allows 
         ! this is in mpp case. In this module, just do nothing
      ELSE      
         !
         !                                     ! East-West boundaries
         !                                     ! ====================
         SELECT CASE ( nperio )
         !
         CASE ( 1 , 4 , 6 )                       !** cyclic east-west
            pt2d( 1 ,:) = pt2d(jpim1,:)               ! all points
            pt2d(jpi,:) = pt2d(  2  ,:)
            !
         CASE DEFAULT                             !** East closed  --  West closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )            ! T-, U-, V-, W-points
               pt2d( 1 ,:) = zland
               pt2d(jpi,:) = zland
            CASE ( 'F' )                              ! F-point
               pt2d(jpi,:) = zland
            END SELECT
            !
         END SELECT
         !
         !                                     ! North-South boundaries
         !                                     ! ======================
         SELECT CASE ( nperio )
         !
         CASE ( 2 )                               !**  South symmetric  --  North closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'W' )                   ! T-, U-, W-points
               pt2d(:, 1 ) = pt2d(:,3)
               pt2d(:,jpj) = zland
            CASE ( 'V' , 'F' )                         ! V-, F-points
               pt2d(:, 1 ) = psgn * pt2d(:,2)
               pt2d(:,jpj) = zland
            END SELECT
            !
         CASE ( 3 , 4 , 5 , 6 )                   !**  North fold  T or F-point pivot  --  South closed
            SELECT CASE ( cd_type )                    ! South : closed
            CASE ( 'T' , 'U' , 'V' , 'W' , 'I' )             ! all points except F-point
               pt2d(:, 1 ) = zland
            END SELECT
            !                                          ! North fold
            pt2d( 1 ,1  ) = zland 
            pt2d( 1 ,jpj) = zland 
            pt2d(jpi,jpj) = zland
            CALL lbc_nfd( pt2d(:,:), cd_type, psgn )
            !
         CASE DEFAULT                             !**  North closed  --  South closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt2d(:, 1 ) = zland
               pt2d(:,jpj) = zland
            CASE ( 'F' )                               ! F-point
               pt2d(:,jpj) = zland
            END SELECT
            !
         END SELECT
         !
      ENDIF
      !    
   END SUBROUTINE lbc_lnk_2d


   !!======================================================================
END MODULE lbclnk
