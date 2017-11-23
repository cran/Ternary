## ----Load package, eval=FALSE--------------------------------------------
#  install.packages('Ternary')

## ----Github package, eval=FALSE------------------------------------------
#  if (!require('devtools')) install.packages('devtools')
#  install_github('ms609/Ternary')

## ----Load----------------------------------------------------------------
library('Ternary')

## ----Create blank plot---------------------------------------------------
TernaryPlot()

## ----Do plotting---------------------------------------------------------
par(mfrow=c(1, 2), mar=rep(0.3, 4))
TernaryPlot(' Redder', ' Greener', 'Bluer', lab.cex=0.8,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white', 
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08)
data_points <- list(
  R = c(255, 0, 0), 
  O = c(240, 180, 52),
  Y = c(210, 222, 102),
  G = c(111, 222, 16),
  B = c(25, 160, 243),
  I = c(92, 12, 243),
  V = c(225, 24, 208)
)
AddToTernary(points, data_points, bg=vapply(data_points, function (x) rgb(x[1], x[2], x[3], 128, maxColorValue=255), character(1)), pch=21, cex=2.8)
AddToTernary(text, data_points, names(data_points), cex=0.8, font=2)
legend('bottomright', 
       pch=21, pt.cex=1.8,
       pt.bg=c(rgb(255, 0, 0,   128, NULL, 255), 
             rgb(240, 180,  52, 128, NULL, 255),
             rgb(210, 222, 102, 128, NULL, 255),
             rgb(111, 222,  16, 128, NULL, 255)),
       legend=c('Red', 'Orange', 'Yellow', 'Green'), 
       cex=0.8, bty='n')

TernaryPlot('Steam', 'Ice', 'water', grid.lines=5)
#HorizontalGrid()
middle_triangle <- matrix(c(
  30, 40, 30,
  30, 30, 40,
  55, 20, 25
), ncol=3, byrow=TRUE)
TernaryPolygon(middle_triangle, col='#aaddfa', border='grey')
TernaryLines(list(c(0, 100, 0), middle_triangle[1, ]), col='grey')
TernaryLines(list(c(0, 0, 100), middle_triangle[2, ]), col='grey')
TernaryLines(list(c(100, 0, 0), middle_triangle[3, ]), col='grey')



