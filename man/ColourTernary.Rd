% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Contours.R
\name{ColourTernary}
\alias{ColourTernary}
\alias{ColorTernary}
\title{Colour ternary plot}
\usage{
ColourTernary(
  values,
  spectrum = hcl.colors(256L, palette = "viridis", alpha = 0.6),
  resolution = sqrt(ncol(values)),
  direction = getOption("ternDirection", 1L),
  legend,
  ...
)

ColorTernary(
  values,
  spectrum = hcl.colors(256L, palette = "viridis", alpha = 0.6),
  resolution = sqrt(ncol(values)),
  direction = getOption("ternDirection", 1L),
  legend,
  ...
)
}
\arguments{
\item{values}{Numeric matrix, possibly created using
\code{\link[=TernaryPointValues]{TernaryPointValues()}}, with four named rows:
\code{x}, \code{y}, Cartesian coordinates of each triangle centre;
\code{z}, value associated with that coordinate;
\code{down}, triangle direction: \code{0} = point upwards; \code{1} = point downwards.}

\item{spectrum}{Vector of colours to use as a spectrum, or \code{NULL} to use
\code{values["z", ]}.}

\item{resolution}{The number of triangles whose base should lie on the longest
axis of the triangle.  Higher numbers will result in smaller subdivisions and smoother
colour gradients, but at a computational cost.}

\item{direction}{(optional) Integer specifying the direction that the
current ternary plot should point: 1, up; 2, right; 3, down; 4, left.}

\item{legend}{Character vector specifying annotations for colour scale.
If not provided, no colour legend is displayed.
Specify \code{TRUE} to generate automatically, or a single integer to generate
\code{legend} annotations.}

\item{\dots}{Further arguments to
\code{\link[PlotTools:SpectrumLegend]{SpectrumLegend()}}.}
}
\value{
\code{ColourTernary()} is called for its side effect – colouring a ternary
plot according to \code{values}.  It invisibly returns \code{NULL}.
}
\description{
Colour a ternary plot according to the output of a function.
}
\examples{
# Depict a function across a ternary plot with colour and contours

TernaryPlot(alab = "a", blab = "b", clab = "c") # Blank plot

FunctionToContour <- function (a, b, c) {
  a - c + (4 * a * b) + (27 * a * b * c)
}

# Evaluate function
values <- TernaryPointValues(FunctionToContour, resolution = 24L)

# Use the value of the function to determine the brightness of the plot
ColourTernary(
  values,
  x = "topleft",
  bty = "n", # No box
  legend = signif(seq(max(values), min(values), length.out = 4), 3)
)

# Overlay contours
TernaryContour(FunctionToContour, resolution = 36L)

# Directly specify the colour with the output of a function

# Create a function that returns a vector of rgb strings:
rgbWhite <- function (r, g, b) {
  highest <- apply(rbind(r, g, b), 2L, max)
  rgb(r/highest, g/highest, b/highest)
}

TernaryPlot()
values <- TernaryPointValues(rgbWhite, resolution = 20)
ColourTernary(values, spectrum = NULL)


}
\seealso{
Fine control over continuous legends:
\code{\link[PlotTools:SpectrumLegend]{PlotTools::SpectrumLegend()}}

Other contour plotting functions: 
\code{\link{TernaryContour}()},
\code{\link{TernaryDensityContour}()},
\code{\link{TernaryPointValues}()}

Other functions for colouring and shading: 
\code{\link{TernaryTiles}()}
}
\author{
\href{https://orcid.org/0000-0001-5660-1727}{Martin R. Smith}
(\href{mailto:martin.smith@durham.ac.uk}{martin.smith@durham.ac.uk})
}
\concept{contour plotting functions}
\concept{functions for colouring and shading}
