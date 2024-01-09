## ----hcl-colors-hack, echo = FALSE--------------------------------------------
if (getRversion() < "3.6") {
  hcl.colors <- function(n, ...) {
    if (n == 4) {
      return(c("#A71B4B", "#F9C25C", "#81DEAD", "#584B9F"))
    } else {
      colorRampPalette(c("#4B0055", "#274983", "#008298", "#00B28A",
                         "#7ED357", "#FDE333"))(n)
    }
  }
}

## ----load-data----------------------------------------------------------------
data("Seatbelts")

# View the first few entries:
head(Seatbelts)

## ----first-plot, fig.asp = 1--------------------------------------------------
# Load the Ternary library
library("Ternary")

# Define our columns of interest:
seat <- c("drivers", "front", "rear")

# Extract and plot the data
TernaryPlot(alab = seat[1], blab = seat[2], clab = seat[3])
TernaryPoints(Seatbelts[, seat])

## ----pch, fig.asp = 1/16------------------------------------------------------
par(mar = c(0, 0, 0, 0))
plot(0:20, rep(2, 21), pch = 0:20,
     cex = 2, ylim = c(-1, 3), ann = FALSE)
text(0:20, rep(0, 21), 0:20)

## ----seat-pch-----------------------------------------------------------------
beltLawPch <- ifelse(Seatbelts[, "law"], 3, 1)

## ----by-law, fig.asp = 1------------------------------------------------------
# Shrink the margin so the plot's easier to read
par(mar = c(0, 0, 0, 0))

# Set up a blank plot, 
TernaryPlot(
  alab = seat[1], blab = seat[2], clab = seat[3],
  region = Seatbelts[, seat]
)

# Add a legend
legend("topleft", c("Belt law", "No law"), pch = c(3, 1))

# Use our beltLawPch variable to style points
TernaryPoints(Seatbelts[, seat], pch = beltLawPch)

## ----viridis-spec-------------------------------------------------------------
nPoints <- nrow(Seatbelts)
rowCol <- hcl.colors(nPoints, palette = "viridis", alpha = 0.8)

## ----spectrum, fig.asp = 1----------------------------------------------------
par(mar = c(0, 0, 0, 0))

TernaryPlot(
  alab = seat[1], blab = seat[2], clab = seat[3],
  region = Seatbelts[, seat]
)
legend("topleft", c("Belt law", "No law"), pch = c(3, 1))

# Add a legend for our colour scale
PlotTools::SpectrumLegend(palette = rowCol, legend = c("Dec 1984", "Jan 1969"))

# Use our rowCol variable to style points
TernaryPoints(Seatbelts[, seat], pch = beltLawPch,
              lwd = 2, # Use wider lines so points are clearer
              col = rowCol)

## ----month-spectrum, fig.asp = 1----------------------------------------------
# Define a suitable cyclical spectrum
fourSeasons <- hcl.colors(4, "Spectral")
monthCol <- colorRampPalette(fourSeasons[c(1:4, 1)])(13)[c(7:12, 1:6)]

par(mar = c(0, 0, 0, 0))

TernaryPlot(
  alab = seat[1], blab = seat[2], clab = seat[3],
  region = Seatbelts[, seat]
)
legend("topleft", c("Belt law", "No law"), pch = c(3, 1))

legendMonths <- seq(1, 12, by = 2)
# Add a legend for our colour index
legend(
  "topright",
  bty = "n", # No frame
  pch = 15, # Filled square
  col = monthCol,
  legend = month.abb,
  title = "Month"
)

# Style points by law and month
TernaryPoints(Seatbelts[, seat], pch = beltLawPch,
              lwd = 2, # Use wider lines so points are clearer
              col = monthCol)

## ----scale-by-deaths, fig.asp = 1---------------------------------------------
par(mar = c(0, 0, 0, 0))

TernaryPlot(alab = seat[1], blab = seat[2], clab = seat[3],
            # Magnify the "action":
            xlim = c(0.055, 0.095), ylim = c(0.48, 0.52))

legend("topleft", c("No law", "Belt law"),
       col = 2:3, pch = 1, lwd = 2, lty = NA)
sizes <- c(3, 7, 12)
 scale <- 200
legend("topright", title = "Casualties / Mm", legend = sizes,
      pt.cex = sizes / 1000 * scale,
      pch = 1, lwd = 2, lty = NA)

# Style points according to casualties and the law
TernaryPoints(Seatbelts[, seat], pch = 1, lwd = 2,
              cex = Seatbelts[, "DriversKilled"] / Seatbelts[, "kms"] * scale,
              col = 2 + Seatbelts[, "law"])

## ----all-deaths, fig.asp = 1--------------------------------------------------
# Subset our data to extract only Octobers:
oct <- month.name == "October"
octBelts <- Seatbelts[oct, ]

par(mar = c(0, 0, 0, 0))
TernaryPlot(alab = seat[1], blab = seat[2], clab = seat[3],
            xlim = c(0.055, 0.095), ylim = c(0.48, 0.52),
            padding = 0.04)


TernarySegments(octBelts[-nrow(octBelts), seat], octBelts[-1, seat],
                col = rowCol, lwd = 2)

# Label each point by its year
TernaryText(octBelts[, seat], paste0("'", 69:84),
            font = 2, cex = 1.5,
            # Semi-transparent colours
            col = adjustcolor(rowCol[oct], alpha.f = 0.8))

# Provide a legend for our colour scheme
PlotTools::SpectrumLegend(
  "topright",
  bty = "n",
  palette = rowCol,
  legend = seq(1984, 1969, length.out = 4),
  title = "Year"
)

