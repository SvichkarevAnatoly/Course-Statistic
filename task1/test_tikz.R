library(tikzDevice)
tikz('simpleEx.tex',width=3.5,height=3.5)
plot(1,main='Hello World!')
dev.off()