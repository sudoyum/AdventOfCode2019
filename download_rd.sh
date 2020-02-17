#!/bin/bash
# Use Export Cookies Firefox extension 

for i in {1..9} {10..25}
do 
    filename="input$i.html"
    wget --load-cookies=cookies-adventofcode-com.txt https://adventofcode.com/2019/day/$i -O "$filename" 
    if [ "$i" -gt 9 ]; then
        outfile=Day$i/README.txt
    else
        outfile=Day0$i/README.txt
    fi
    w3m -dump "$filename" | sed '/---/,$!d' > "$outfile"
    rm "$filename"
done
