

MODULE dynnxt
   !!=========================================================================
   !!                       ***  MODULE  dynnxt  ***
   !! Ocean dynamics: time stepping
   !!=========================================================================
   !! History :  OPA  !  1987-02  (P. Andrich, D. L Hostis)  Original code
   !!                 !  1990-10  (C. Levy, G. Madec)
   !!            7.0  !  1993-03  (M. Guyon)  symetrical conditions
   !!            8.0  !  1997-02  (G. Madec & M. Imbard)  opa, release 8.0
   !!            8.2  !  1997-04  (A. Weaver)  Euler forward step
   !!             -   !  1997-06  (G. Madec)  lateral boudary cond., lbc routine
   !!    NEMO    1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!             -   !  2002-10  (C. Talandier, A-M. Treguier) Open boundary cond.
   !!            2.0  !  2005-11  (V. Garnier) Surface pressure gradient organization
   !!            2.3  !  2007-07  (D. Storkey) Calls to BDY routines. 
   !!            3.2  !  2009-06  (G. Madec, R.Benshila)  re-introduce the vvl option
   !!            3.3  !  2010-09  (D. Storkey, E.O'Dea) Bug fix for BDY module
   !!            3.3  !  2011-03  (P. Oddo) Bug fix for time-splitting+(BDY-OBC) and not VVL
   !!-------------------------------------------------------------------------
  
   !!-------------------------------------------------------------------------
   !!   dyn_nxt      : obtain the next (after) horizontal velocity
   !!-------------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE phycst          ! physical constants
   USE dynspg_oce      ! type of surface pressure gradient
   USE dynadv          ! dynamics: vector invariant versus flux form
   USE domvvl          ! variable volume
   USE obc_oce         ! ocean open boundary conditions
   USE obcdyn          ! open boundary condition for momentum (obc_dyn routine)
   USE obcdyn_bt       ! 2D open boundary condition for momentum (obc_dyn_bt routine)
   USE obcvol          ! ocean open boundary condition (obc_vol routines)
   USE bdy_oce         ! ocean open boundary conditions
   USE bdydta          ! ocean open boundary conditions
   USE bdydyn          ! ocean open boundary conditions
   USE bdyvol          ! ocean open boundary condition (bdy_vol routines)
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! lateral boundary condition (or mpp link)
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! Memory Allocation
   USE prtctl          ! Print control



   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC    dyn_nxt   ! routine called by step.F90

   !! * Substitutions
   !!----------------------------------------------------------------------
   !!                    ***  domzgr_substitute.h90   ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute fsdep. and fse.., the vert. depth and scale
   !!      factors depending on the vertical coord. used, using CPP macro.
   !!----------------------------------------------------------------------
   !! History :  1.0  !  2005-10  (A. Beckmann, G. Madec) generalisation to all coord.
   !!            3.1  !  2009-02  (G. Madec, M. Leclair)  pure z* coordinate
   !!----------------------------------------------------------------------
! reference for s- or zps-coordinate (3D no time dependency)
! z- or s-coordinate (1D or 3D + no time dependency) use reference in all cases












   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: domzgr_substitute.h90 2528 2010-12-27 17:33:53Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: dynnxt.F90 3692 2012-11-28 07:54:04Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_nxt ( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_nxt  ***
      !!                   
      !! ** Purpose :   Compute the after horizontal velocity. Apply the boundary 
      !!             condition on the after velocity, achieved the time stepping 
      !!             by applying the Asselin filter on now fields and swapping 
      !!             the fields.
      !!
      !! ** Method  : * After velocity is compute using a leap-frog scheme:
      !!                       (ua,va) = (ub,vb) + 2 rdt (ua,va)
      !!             Note that with flux form advection and variable volume layer
      !!             (lk_vvl=T), the leap-frog is applied on thickness weighted
      !!             velocity.
      !!             Note also that in filtered free surface (lk_dynspg_flt=T),
      !!             the time stepping has already been done in dynspg module
      !!
      !!              * Apply lateral boundary conditions on after velocity 
      !!             at the local domain boundaries through lbc_lnk call,
      !!             at the one-way open boundaries (lk_obc=T),
      !!             at the AGRIF zoom     boundaries (lk_agrif=T)
      !!
      !!              * Apply the time filter applied and swap of the dynamics
      !!             arrays to start the next time step:
      !!                (ub,vb) = (un,vn) + atfp [ (ub,vb) + (ua,va) - 2 (un,vn) ]
      !!                (un,vn) = (ua,va).
      !!             Note that with flux form advection and variable volume layer
      !!             (lk_vvl=T), the time filter is applied on thickness weighted
      !!             velocity.
      !!
      !! ** Action :   ub,vb   filtered before horizontal velocity of next time-step
      !!               un,vn   now horizontal velocity of next time-step
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   iku, ikv     ! local integers
      REAL(wp) ::   zue3a, zue3n, zue3b, zuf, zec   ! local scalars
      REAL(wp) ::   zve3a, zve3n, zve3b, zvf        !   -      -
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  ze3u_f, ze3v_f 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_nxt')
      !
      CALL wrk_alloc( jpi,jpj,jpk, ze3u_f, ze3v_f )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_nxt : time stepping'
         IF(lwp) WRITE(numout,*) '~~~~~~~'
      ENDIF

      !
      ! Next velocity :   Leap-frog time stepping already done in dynspg_flt.F routine
      ! -------------

      ! Update after velocity on domain lateral boundaries      (only local domain required)
      ! --------------------------------------------------
      CALL lbc_lnk( ua, 'U', -1. )         ! local domain boundaries
      CALL lbc_lnk( va, 'V', -1. ) 
      !

      ! Time filter and swap of dynamics arrays
      ! ------------------------------------------
      IF( neuler == 0 .AND. kt == nit000 ) THEN        !* Euler at first time-step: only swap
         DO jk = 1, jpkm1
            un(:,:,jk) = ua(:,:,jk)                          ! un <-- ua
            vn(:,:,jk) = va(:,:,jk)
         END DO
      ELSE                                             !* Leap-Frog : Asselin filter and swap
         !                                ! =============!
         IF( .NOT. lk_vvl ) THEN          ! Fixed volume !
            !                             ! =============!
            DO jk = 1, jpkm1                              
               DO jj = 1, jpj
                  DO ji = 1, jpi    
                     zuf = un(ji,jj,jk) + atfp * ( ub(ji,jj,jk) - 2.e0 * un(ji,jj,jk) + ua(ji,jj,jk) )
                     zvf = vn(ji,jj,jk) + atfp * ( vb(ji,jj,jk) - 2.e0 * vn(ji,jj,jk) + va(ji,jj,jk) )
                     !
                     ub(ji,jj,jk) = zuf                      ! ub <-- filtered velocity
                     vb(ji,jj,jk) = zvf
                     un(ji,jj,jk) = ua(ji,jj,jk)             ! un <-- ua
                     vn(ji,jj,jk) = va(ji,jj,jk)
                  END DO
               END DO
            END DO
            !                             ! ================!
         ELSE                             ! Variable volume !
            !                             ! ================!
            !
            DO jk = 1, jpkm1                 ! Before scale factor at t-points
               e3t(:,:,jk) = e3t(:,:,jk)                                   &
                  &              + atfp * (  e3t(:,:,jk) + e3t(:,:,jk)     &
                  &                         - 2._wp * e3t(:,:,jk)            )
            END DO
            zec = atfp * rdt / rau0          ! Add filter correction only at the 1st level of t-point scale factors
            e3t(:,:,1) = e3t(:,:,1) - zec * ( emp_b(:,:) - emp(:,:) ) * tmask(:,:,1)
            !
            IF( ln_dynadv_vec ) THEN         ! vector invariant form (no thickness weighted calulation)
               !
               !                                      ! before scale factors at u- & v-pts (computed from fse3t_b)
               CALL dom_vvl_2( kt, e3u(:,:,:), e3v(:,:,:) )
               !
               DO jk = 1, jpkm1                       ! Leap-Frog - Asselin filter and swap: applied on velocity
                  DO jj = 1, jpj                      !                                                 --------
                     DO ji = 1, jpi
                        zuf = un(ji,jj,jk) + atfp * ( ub(ji,jj,jk) - 2.e0 * un(ji,jj,jk) + ua(ji,jj,jk) )
                        zvf = vn(ji,jj,jk) + atfp * ( vb(ji,jj,jk) - 2.e0 * vn(ji,jj,jk) + va(ji,jj,jk) )
                        !
                        ub(ji,jj,jk) = zuf                      ! ub <-- filtered velocity
                        vb(ji,jj,jk) = zvf
                        un(ji,jj,jk) = ua(ji,jj,jk)             ! un <-- ua
                        vn(ji,jj,jk) = va(ji,jj,jk)
                     END DO
                  END DO
               END DO
               !
            ELSE                             ! flux form (thickness weighted calulation)
               !
               CALL dom_vvl_2( kt, ze3u_f, ze3v_f )   ! before scale factors at u- & v-pts (computed from fse3t_b)
               !
               DO jk = 1, jpkm1                       ! Leap-Frog - Asselin filter and swap: 
                  DO jj = 1, jpj                      !                   applied on thickness weighted velocity
                     DO ji = 1, jpi                   !                              ---------------------------
                        zue3a = ua(ji,jj,jk) * e3u(ji,jj,jk)
                        zve3a = va(ji,jj,jk) * e3v(ji,jj,jk)
                        zue3n = un(ji,jj,jk) * e3u(ji,jj,jk)
                        zve3n = vn(ji,jj,jk) * e3v(ji,jj,jk)
                        zue3b = ub(ji,jj,jk) * e3u(ji,jj,jk)
                        zve3b = vb(ji,jj,jk) * e3v(ji,jj,jk)
                        !
                        zuf = ( zue3n + atfp * ( zue3b - 2._wp * zue3n  + zue3a ) ) / ze3u_f(ji,jj,jk)
                        zvf = ( zve3n + atfp * ( zve3b - 2._wp * zve3n  + zve3a ) ) / ze3v_f(ji,jj,jk)
                        !
                        ub(ji,jj,jk) = zuf                     ! ub <-- filtered velocity
                        vb(ji,jj,jk) = zvf
                        un(ji,jj,jk) = ua(ji,jj,jk)            ! un <-- ua
                        vn(ji,jj,jk) = va(ji,jj,jk)
                     END DO
                  END DO
               END DO
               e3u(:,:,1:jpkm1) = ze3u_f(:,:,1:jpkm1)      ! e3u_b <-- filtered scale factor
               e3v(:,:,1:jpkm1) = ze3v_f(:,:,1:jpkm1)
            ENDIF
            !
         ENDIF
         !
      ENDIF

      IF(ln_ctl)   CALL prt_ctl( tab3d_1=un, clinfo1=' nxt  - Un: ', mask1=umask,   &
         &                       tab3d_2=vn, clinfo2=' Vn: '       , mask2=vmask )
      ! 
      CALL wrk_dealloc( jpi,jpj,jpk, ze3u_f, ze3v_f )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_nxt')
      !
   END SUBROUTINE dyn_nxt

   !!=========================================================================
END MODULE dynnxt
