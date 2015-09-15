cd "../data"
set terminal png
set key off
set output "graph3.png"
set title "Distribution of actors by their second sign of the zodiac"
set style fill pattern
set style histogram clustered
set style data histograms
plot "file3.txt" using 2:xtic(1)
