#!/bin/bash

s=0

echo ----------------------
for i in  {1..100} 
do
  i=`./matrix_mult`
#  i=`awk '{print $1}' tmp.txt`
  s=$s+$i
 printf "."
done 
printf "done: \n"
s=($s)/100

echo $s | bc -l 


