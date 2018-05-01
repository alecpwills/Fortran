# For use with poly_int.f08
# Will plot the output binsminsim.dat and label the different convergences

# set x logscale per homework requirements
set logscale x

#set title
set title "Integral Convergence vs. Bin Size"

#label axes
set xlabel 'log(Number of Bins)'
set ylabel 'Integral (sq. units)'

# move legend
set key bottom right

#plot in gnuplot window: the convergence line from analytical solution,
#the bins vs midpoint values and the bins vs simpson values

set arrow 1 from 1, 483.3333 to 10000, 483.3333 nohead linetype '.' lc rgb 'red'
plot "binsmidsim.dat" using 1:2 title "midpoint" with points lc rgb 'blue', "binsmidsim.dat" using 1:3 title "simpson's" with points lc rgb 'green', 1/0  title "analytical solution" linecolor rgb 'red' linetype '.'

#takes advantage of fact gnuplot doesn't actually plot 1/0 things
#but adds the legend and title formats regardless
#dashtype has to be at the end for some reason because of line format syntax

#WOULDN'T have worked -- dashtype on my version (5.0), linetype here (4.6)
#LINETYPE FOR MATHLAB MACHINES


#now save to png
set terminal png
set output "poly_int.png"
replot

#reset terminal to qt, as it defaults to
set terminal wxt
#wxt for mathlab
