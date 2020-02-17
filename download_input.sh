#!/bin/bash
# Use Export Cookies Firefox extension 

for i in {1..9} {10..25}
do 
    wget --load-cookies=cookies-adventofcode-com.txt https://adventofcode.com/2019/day/$i/input -O input$i.txt
done
