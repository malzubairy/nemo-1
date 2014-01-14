#!/bin/bash
case_name=$1
echo $case_name will be created!
./makenemo -n $case_name -r ORCA2_LIM add_key "key_mpp_mpi key_nproci=2 key_nprocj=2" del_key "key_agrif"
cp ../INPUTDATA/ORCA2_LIM/* ./$case_name/EXP00/.
