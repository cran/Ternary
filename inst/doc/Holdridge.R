## ----holdridge, fig.asp = 1---------------------------------------------------
# Install the Ternary package, if it's not already installed
if (!requireNamespace("Ternary", quietly = TRUE)) {
  install.packages("Ternary")
}

# Load the Ternary package
library("Ternary")

# Load some example data from the Ternary package
data(holdridge, holdridgeLifeZonesUp, package = "Ternary")

# Suppress plot margins
par(mar = c(0, 0, 0, 0))

# Create blank Holdridge plot
HoldridgePlot(hex.labels = holdridgeLifeZonesUp)
HoldridgeBelts()

# Plot data, shaded by latitude
HoldridgePoints(holdridge$PET, holdridge$Precipitation,
                col = hcl.colors(91)[abs(holdridge$Latitude) + 1],
                lwd = 2)

# Add legend to interpret shading
PlotTools::SpectrumLegend(
  "topright", bty = "n", # No box
  horiz = TRUE, # Horizontal
  x.intersp = -0.5, # Squeeze in X direction
  legend = paste0(seq(0, 90, 15), "°"),
  palette = hcl.colors(91),
  title = "Latitude"
)

