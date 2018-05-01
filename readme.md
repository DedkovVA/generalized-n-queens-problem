For run:
1. Cd in directory common-n-queens-problem
2. Print in console:  
sbt 'run -m 7 -n 7 n 1 k 2 b 2 q 2 PrintBoard none'
3. If you want to see boards, print "ascii" or "AsLine" after "PrintBoard"

Input args should be in format  
**-m x0 -n x1 [q x2] [r x3] [b x4] [k x5] [n x6] [PrintBoard x7]**  

where  
* -m, -n: stand for board size, mandatory params
* -m -- number of rows, -n -- number of columns
* q, r, b, k, n: stand for chess pieces, optional params  
* q -- queen, r -- rook, b -- bishop, k -- king, n -- knight  
* x0 .. x6 should be non-negative integer numbers  
* PrintBoard -- output format, optional param  
* x7 may be among values **{ascii, AsLine, none}**  
    * x7 = none -- default value -- if you don't want to see boards in output (only total count)
    * x7 = ascii  -- if you want to see output as ascii-graphic
    * x7 = AsLine -- if you want to see output as string-lines  
        * AsLine output example: '0:1:Q-3:5:B'
        * 0:0 -- top-left board corner  
        * first num -- for rows, second num -- for columns
    * x7 = ascii or AsLine prints only first 1000 boards

You can change the order of paired-params  
These all are case-insensitive params