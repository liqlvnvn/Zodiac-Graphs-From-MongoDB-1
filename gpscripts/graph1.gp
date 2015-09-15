set terminal png
set output "graph1.png"
plot "file1.txt" using 2:xticlabels(1) with lines
