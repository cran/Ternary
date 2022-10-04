## ----hcl-colors-hack, echo=FALSE----------------------------------------------
if (getRversion() < 3.6) {
  hcl.colors <- function(n, ...) {
    colorRampPalette(c("#4B0055", "#274983", "#008298", "#00B28A",
                         "#7ED357", "#FDE333"))(n)
  }
}

## ----holdridge, fig.asp = 1---------------------------------------------------
# Load package and data
library("Ternary")

data(holdridge, holdridgeLifeZonesUp, package = "Ternary")

# Suppress plot margins
par(mar = c(0, 0, 0, 0))

# Create blank Holdridge plot
HoldridgePlot(hex.labels = holdridgeLifeZonesUp)
HoldridgeBelts()

# Plot data, shaded by latitude; darker = equitorial.
HoldridgePoints(holdridge$PET, holdridge$Precipitation,
                col = hcl.colors(91)[abs(holdridge$Latitude) + 1],
                lwd = 2)

