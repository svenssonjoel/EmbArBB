#!/bin/bash




echo ----------------------
export ARBB_OPT_LEVEL=O3 

for threads in 1 2 4 
do 
  printf "Num Cores: "
  echo $threads
  export ARBB_NUM_CORES=$threads
  for x in  1024 2048
#4 8 16 32 64 128 256 384 512 640 768 
  do 
    s=0
    for i in  {1..10} 
    do
      i=`./matrix_mult $x | awk '{print $1}'`
      s=$s+$i
    done 
    s=($s)/10
  
    printf $x 
    printf ": "   
    echo $s | bc -l 
  done
done