cd "data"
set terminal png
set key off
set output "graph4.png"
set title "Distribution of actors by birthdays"
plot [-1:366] [] "data4.txt" using 0:3 with points, "" using 0:3 smooth unique with lines
set output "graph4-1.png"
set title "Distribution of actors by birthdays (zoomed)"
plot [-1:366] [750:1100] "data4.txt" using 0:3 with points, "" using 0:3 smooth unique with lines
