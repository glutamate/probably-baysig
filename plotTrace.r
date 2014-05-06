#!/usr/bin/Rscript

# Simple script for plotting a test trace.

require(ggplot2)
system('cabal run > trace.dat')
d = read.csv('trace.dat', header = F)

quartz()
print(ggplot(d, aes(V1, V2)) + geom_point(alpha = 0.05, col = 'darkblue'))
message('press something to continue')
invisible(readLines('stdin', n = 1))
dev.off()

system('rm trace.dat')

