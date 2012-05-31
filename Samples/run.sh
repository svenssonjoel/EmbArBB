#!/bin/bash

s=0

echo ----------------------
for j in {1..10}
do
 for i in  {1..10} 
 do
   i=`./matrix_mult`
   s=$s+$i
 done 
 printf "."
done
printf "done: \n"
s=($s)/100

echo $s | bc -l 


