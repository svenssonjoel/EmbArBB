#!/bin/bash




echo ----------------------
for cores in 4
do 
  printf "cores:"
  echo $cores
  for stencilSize in small medium large 
  do 
      printf stencilSize:
      echo $stencilSize
      for imageSize in  256 512 1024 2048
      do
	  s=0
	  for iteration in  {1..10} 
	  do
	      i=`./blurpart $stencilSize cat$imageSize.bmp catblur.bmp | awk '{print $1}'`
	      s=$s+$i
	  done 
	  s=($s)/10
	  
	  printf $imageSize 
	  printf ": "   
	  echo $s | bc -l 
      done
  done
done 