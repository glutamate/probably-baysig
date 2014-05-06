#!/usr/bin/Rscript

# Simple script for plotting a test trace.  Tested on OS X.

if (length(find.package('ggplot2', quiet = T)) == 0)
  install.packages('ggplot2', repos = 'http://cran.r-project.org')
require(ggplot2)

system('cabal run | tail -n +4 > tmp_trace.dat')
d = read.csv('tmp_trace.dat', header = F, colClasses = c('numeric', 'numeric'))

quartz()
print(ggplot(d, aes(V1, V2)) + geom_point(alpha = 0.05, col = 'darkblue'))
message('press something to continue')
invisible(readLines('stdin', n = 1))
dev.off()

system('rm tmp_trace.dat')

