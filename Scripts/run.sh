#!/bin/bash

s=0

echo ----------------------
for i in  {1..10} 
do
  ./matrix_multiplication | grep arbb > tmp.txt 
  i=`awk '{print $2}' tmp.txt`
  s=$s+$i
done 

s=($s)/10

echo $s | bc -l 


echo ----------------------
for i in  {1..10} 
do
  ./matrix_multiplication -m 640 -n 640 -q 640 | grep arbb > tmp.txt 
  i=`awk '{print $2}' tmp.txt`
  s=$s+$i
done 

s=($s)/10

echo $s | bc -l 