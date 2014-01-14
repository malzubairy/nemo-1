#!/bin/bash
#############################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# sette.sh   : principal script of SET TEsts for NEMO (SETTE)
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
#############################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ================
# sette.sh
# ================
#
# ----------------------------------------------
# Set of tests for NEMO
# ----------------------------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./sette.sh
#
# DESCRIPTION
# ===========
#
# Variables to be checked by user:
#
# COMPILER : name of compiler as defined in NEMOGCM/ARCH directory 
#
# BATCH_COMMAND :  name of the command for batch submission
#
# MPI_INTERACT :  
#
#         for MPP tests, "no" for batch execution, "yes" for interactive execution 
#
#         NOTE: for run with 1 process tests are run always in MPI_INTERACT="yes"
#
# Principal script is sette.sh, that calls 
#
#  makenemo 
#
#   creates the exectuable in ${CONFIG_NAME}/BLD/bin/nemo.exe  (and its link opa in ${CONFIG_NAME}/EXP00)
#
#  param.cfg : sets and loads following directories:
#
#   FORCING_DIR : is the directory for forcing files (tarfile)
#
#   INPUT_DIR : is the directory for input files storing 
#
#   TMPDIR : is the temporary directory (if needed)
# 
#   NEMO_VALIDATION_DIR : is the validation directory
#
#   (NOTE: this file is the same for all configrations to be tested with sette)
#
#
#  all_functions.sh : loads functions used by sette (note: new functions can be added here)
#
#   set_namelist : function declared in all_functions that set namelist parameters for tests
#
#   post_test_tidyup : creates validation storage directory and copy needed output files (solver.stat and ocean.output) in it after execution of test.
#
#   Tree of VALIDATION is:
#
#   NEMO_VALIDATION_DIR/WCONFIG_NAME/WCOMPILER_NAME/TEST_NAME/REVISION_NUMBER(or DATE)
#
#
#  prepare_exe_dir.sh : defines and creates directory where the test is executed
#
#       execution directory takes name of TEST_NAME defined in every test in sette.sh
#
#       ( each test in executed in its own directory )
#
#
#  fcm_job.sh 
#
#   runs job in interactive or batch mode : all jobs using 1 process are run interactive, and all MPP jobs are
#
#   run in batch (MPI_INTERACT="no") or interactive (MPI_INTERACT="yes") see sette.sh and BATCH_TEMPLATE directory
#
#   (note this job needs to have an input_CONFIG.cfg in which can be found input tar file)
# 
#  NOTE: if job is not launched for some problems you have executable ready in ${EXE_DIR} directory
#
#  NOTE: the changed namelists are leaved in ${EXE_DIR} directory whereas original namelist remains in ${NEW_CONF}/EXP00
# 
#  in ${SETTE_DIR} is created output.sette with the echo of executed commands
#
#  if sette.sh is stopped in output.sette there is written the last command executed by sette.sh
#
#  if you run: ./sette.sh 2>&1 | tee out.sette
#
#  in ${SETTE_DIR} out.sette is redirected standard error & standard output
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./sette.sh 
#
#
# TODO
# ====
#
# option debug
#
# EVOLUTIONS
# ==========
#
# $Id:$
#
#   * creation
#
#-
#
#-
# Compiler among those in NEMOGCM/ARCH
COMPILER=PW6_VARGAS
export BATCH_COMMAND_PAR="llsubmit"
export BATCH_COMMAND_SEQ=$BATCH_COMMAND_PAR
export INTERACT_FLAG="no"
export MPIRUN_FLAG="yes"

# Directory to run the tests
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=${SETTE_DIR%/SETTE}
CONFIG_DIR=${MAIN_DIR}/CONFIG
TOOLS_DIR=${MAIN_DIR}/TOOLS
COMPIL_DIR=${TOOLS_DIR}/COMPILE

CMP_NAM=${1:-$COMPILER}
# Copy job_batch_COMPILER file for specific compiler into job_batch_template
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/batch-${COMPILER} job_batch_template || exit

for config in 1 2 3 4 5 6 7 8 9
do

# TESTS FOR GYRE_LOBSTER CONFIGURATION
if [ ${config} -eq 1 ] ;  then
    ## Restartability tests for GYRE_LOBSTER
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n GYRELOB_LONG -r GYRE_LOBSTER add_key "key_mpp_mpi"
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    \rm ${JOB_FILE}
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"GYRELOB_LONG\"
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 120
    set_namelist namelist nn_stock 60
    set_namelist namelist ln_clobber .true.
    set_namelist namelist nn_solv 2
    set_namelist namelist_top ln_diatrc .false.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 4
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"GYRELOB_SHORT\"
    set_namelist namelist nn_it000 61
    set_namelist namelist nn_itend 120
    set_namelist namelist nn_stock 60
    set_namelist namelist ln_rstart .true.
    set_namelist namelist nn_rstctl 2
    set_namelist namelist ln_clobber .true.
    set_namelist namelist nn_solv 2
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 4
    set_namelist namelist cn_ocerst_in \"GYRELOB_LONG_00000060_restart\"
    set_namelist namelist_top cn_trcrst_in \"GYRELOB_LONG_00000060_restart_trc\"
    set_namelist namelist_top ln_diatrc .false.
    set_namelist namelist_top ln_rsttr .true.
    set_namelist namelist_top nn_rsttr 2
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/GYRELOB_LONG_00000060_restart_${L_NPROC}.nc .
        ln -sf ../LONG/GYRELOB_LONG_00000060_restart_trc_${L_NPROC}.nc .
    done
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 2 ] ;  then
    ## Repropducibility tests for GYRE_LOBSTER
    export TEST_NAME="REPRO_1_4"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n GYRELOB_4 -r GYRE_LOBSTER add_key "key_mpp_mpi key_mpp_rep"
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    \rm ${JOB_FILE}
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"GYRELOB_14\"
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 60
    set_namelist namelist nn_fwb 0
    set_namelist namelist nn_bench 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist nn_solv 2
    set_namelist namelist_top ln_diatrc .false.
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist jpni 1
    set_namelist namelist jpnj 4
    set_namelist namelist jpnij 4
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_2"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"GYRELOB_22\"
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 60
    set_namelist namelist nn_fwb 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist nn_solv 2
    set_namelist namelist_top ln_diatrc .false.
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 4
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

# TESTS FOR ORCA2_LIM_PISCES CONFIGURATION
if [ ${config} -eq 3 ] ;  then
    ## Restartability tests for ORCA2_LIM_PISCES
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n ORCA2LIMPIS_LONG -r ORCA2_LIM_PISCES -j 8 add_key "key_mpp_mpi" 
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    \rm ${JOB_FILE}
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"O2LP_LONG\"
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 150
    set_namelist namelist nn_stock 75
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 4
    set_namelist namelist nn_solv 2
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist_top ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"O2LP_SHORT\"
    set_namelist namelist nn_it000 76
    set_namelist namelist nn_itend 150
    set_namelist namelist nn_stock 75
    set_namelist namelist ln_rstart .true.
    set_namelist namelist nn_rstctl 2
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 4
    set_namelist namelist nn_solv 2
    set_namelist namelist cn_ocerst_in \"O2LP_LONG_00000075_restart\"
    set_namelist namelist_ice cn_icerst_in \"O2LP_LONG_00000075_restart_ice\"
    set_namelist namelist_top cn_trcrst_in \"O2LP_LONG_00000075_restart_trc\"
    set_namelist namelist_top ln_diatrc .false.
    set_namelist namelist_top ln_rsttr .true.
    set_namelist namelist_top nn_rsttr 2
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/O2LP_LONG_00000075_restart_${L_NPROC}.nc .
        ln -sf ../LONG/O2LP_LONG_00000075_restart_trc_${L_NPROC}.nc .
        ln -sf ../LONG/O2LP_LONG_00000075_restart_ice_${L_NPROC}.nc .
    done
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 4 ] ;  then
    ## Repropducibility tests for ORCA2_LIM_PISCES
    export TEST_NAME="REPRO_4_4"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n ORCA2LIMPIS_16 -r ORCA2_LIM_PISCES -j 8 add_key "key_mpp_rep key_mpp_mpi"
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    \rm $JOB_FILE
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 75
    set_namelist namelist nn_fwb 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 4
    set_namelist namelist jpnj 4
    set_namelist namelist jpnij 16
    set_namelist namelist nn_solv 2
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist_top ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_8"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 75
    set_namelist namelist nn_fwb 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 8
    set_namelist namelist jpnij 16
    set_namelist namelist nn_solv 2
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist_top ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

# TESTS FOR ORCA2_OFF_PISCES CONFIGURATION
if [ ${config} -eq 5 ] ;  then
    ## Restartability tests for ORCA2_OFF_PISCES
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n ORCA2OFFPIS_LONG -r ORCA2_OFF_PISCES -j 8 add_key "key_mpp_mpi key_mpp_rep"
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    \rm $JOB_FILE
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"OFFP_LONG\"
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 40
    set_namelist namelist nn_stock 20
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 4
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist_top ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist cn_exp \"OFFP_SHORT\"
    set_namelist namelist nn_it000 21
    set_namelist namelist nn_itend 40
    set_namelist namelist nn_stock 20
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 4
    set_namelist namelist_top ln_diatrc .false.
    set_namelist namelist_top ln_rsttr .true.
    set_namelist namelist_top nn_rsttr 2
    set_namelist namelist_top cn_trcrst_in \"OFFP_LONG_00000020_restart_trc\"
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/OFFP_LONG_00000020_restart_trc_${L_NPROC}.nc .
    done
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME}  ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC  ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 6 ] ;  then
    ## Repropducibility tests for ORCA2_OFF_PISCES
    export TEST_NAME="REPRO_4_4"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n ORCA2OFFPIS_16 -r ORCA2_OFF_PISCES -j 8 add_key "key_mpp_rep key_mpp_mpi"
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    \rm $JOB_FILE
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 40
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 4
    set_namelist namelist jpnj 4
    set_namelist namelist jpnij 16
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist_top ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_8"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 40
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 2
    set_namelist namelist jpnj 8
    set_namelist namelist jpnij 16
    set_namelist namelist_top ln_trcdta .false.
    set_namelist namelist_top ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces ln_ironsed .false.
    set_namelist namelist_pisces ln_river .false.
    set_namelist namelist_pisces ln_ndepo .false.
    set_namelist namelist_pisces ln_dust .false.
    set_namelist namelist_pisces ln_presatm .false.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC  ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 7 ] ;  then
    ## Restartability tests for AMM12
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n AMM12_LONG -r AMM12 
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    \rm $JOB_FILE
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 12
    set_namelist namelist nn_stock 6
    set_namelist namelist nn_fwb 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist nn_dyn2d 2
    set_namelist namelist nn_tra_dta 0
    set_namelist namelist jpni 8
    set_namelist namelist jpnj 4
    set_namelist namelist jpnij 32
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 7
    set_namelist namelist nn_itend 12
    set_namelist namelist nn_fwb 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist nn_dyn2d 2
    set_namelist namelist nn_tra_dta 0
    set_namelist namelist jpni 8
    set_namelist namelist jpnj 4
    set_namelist namelist jpnij 32
    set_namelist namelist ln_rstart .true.
    set_namelist namelist nn_rstctl 2
    set_namelist namelist cn_ocerst_in \"AMM12_00000006_restart\"
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/AMM12_00000006_restart_${L_NPROC}.nc .
    done
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 8 ] ;  then
    ## Reproducibility tests for AMM12
    export TEST_NAME="REPO_8_4"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n AMM12_32 -r AMM12 add_key "key_mpp_rep"
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    \rm ${JOB_FILE}
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 576
    set_namelist namelist nn_fwb 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist nn_dyn2d 2
    set_namelist namelist nn_tra_dta 0
    set_namelist namelist jpni 8
    set_namelist namelist jpnj 4
    set_namelist namelist jpnij 32
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPO_4_8"
    . prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 576
    set_namelist namelist nn_fwb 0
    set_namelist namelist ln_ctl .false.
    set_namelist namelist nn_dyn2d 2
    set_namelist namelist nn_tra_dta 0
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 4
    set_namelist namelist jpnj 8
    set_namelist namelist jpnij 32
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

# TEST FOR ORCA2_LIM_AGRIF : simple test of running AGRIF (no restartability neither reproducibility tests)
if [ ${config} -eq 9 ] ;  then
    ## ORCA2_LIM with Agulhas AGRIF zoom in MPI
    export TEST_NAME="SHORT"
    cd ${SETTE_DIR}
    . ../CONFIG/makenemo -m ${CMP_NAM} -n ORCA2AGUL_1_2 -r ORCA2_LIM -j 8 add_key "key_mpp_rep key_mpp_mpi key_agrif" del_key "key_zdftmx"
    cd ${SETTE_DIR}
    . param.cfg
    . all_functions.sh
    . prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=2
    \rm ${JOB_FILE}
    cd ${EXE_DIR}
    set_namelist namelist nn_it000 1
    set_namelist namelist nn_itend 75
    set_namelist namelist ln_ctl .false.
    set_namelist namelist ln_clobber .true.
    set_namelist namelist jpni 1
    set_namelist namelist jpnj 2
    set_namelist namelist jpnij 2
    set_namelist 1_namelist nn_it000 1
    set_namelist 1_namelist nn_itend 150
    set_namelist 1_namelist ln_ctl .false.
    set_namelist 1_namelist ln_clobber .true.
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

done
