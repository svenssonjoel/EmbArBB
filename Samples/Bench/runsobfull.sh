#!/bin/bash




echo ----------------------
export ARBB_OPT_LEVEL=O3 

for threads in 1 2 4 
do 
  printf "Num Cores: "
  echo $threads
  export ARBB_NUM_CORES=$threads
  for x in "cat256.bmp" "cat512.bmp" "cat1024.bmp" "cat2048.bmp" "cat4096.bmp"

  do 
    s=0
    for iteration in  {1..10} 
    do
      i=`./sobelfull $x | awk '{print $1}'`
      s=$s+$i
    done 
    s=($s)/10

    printf $x 
    printf ": "   
    echo $s | bc -l 
  done
done