cd "data"
set terminal png
set key off
set output "graph3.png"
set title "Distribution of actors by their first and second signs of the zodiac"
set style fill pattern
set style histogram clustered
set style data histograms
plot "data3.txt" using 2:xtic(1), "" u 3:xtic(1), "" u 4:xtic(1)
