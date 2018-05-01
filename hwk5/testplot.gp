#
# run with
# gnuplot> load 'testplot.gp'
#
set yrange [-1.0:11.0]
set xrange [-1.0:11.0]
set ticslevel 0
set terminal postscript enhanced color solid
set output 'output.ps'
splot 'output.dat' u 1:2:3 
#pause -1
