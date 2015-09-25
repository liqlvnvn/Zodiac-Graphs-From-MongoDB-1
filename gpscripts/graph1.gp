cd "data"
set terminal png
set key off
set output "graph1.png"
set title "Distribution of actors by signs of the zodiac"
plot "data1.txt" using 2:xtic(1) with linespoints
