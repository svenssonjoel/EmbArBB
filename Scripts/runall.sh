#!/bin/bash




echo ----------------------

for threads in 1 2 4 
do 
  printf "Num Cores: "
  echo $threads
  export ARBB_NUM_CORES=$threads
  for x in  4 8 16 32 64 128 256 384 512 640 768
  do 
    s=0
    for i in  {1..10} 
    do
      ./matrix_multiplication -m $x -n $x -q $x | grep arbb > tmp.txt 
      i=`awk '{print $2}' tmp.txt`
      s=$s+$i
    done 
    s=($s)/10
  
    printf $x 
    printf ": "   
    echo $s | bc -l 
  done
done