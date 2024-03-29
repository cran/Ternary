as.list.ternRegion <- function(x) {
  list(min = apply(x, 2, min),
       max = apply(x, 2, max))
}

test_that(".Normalize works", {
  expect_equal(.Normalize(0, c(0, 1)), 0)
  expect_equal(.Normalize(1, c(0, 1)), 1)
  expect_equal(.Normalize(1, c(0, 2)), 1/2)
  
  expect_equal(.Normalize(1:3, c(1, 3)), 0:2 / 2)
  expect_equal(.Unnormalize(0:2 / 2, c(1, 3)), 1:3)
})

test_that(".Rebase handles vectorized input", {
  expect_equal(.Rebase(0:4, c(0, 4), 1 + c(0, 8)), 1 + (0:4 * 2))
})

test_that(".UnnormalizeXY() handles vectorized input", {
  # No renormalization
  expect_equal(
    .UnnormalizeXY(0:4, 4:0, region = ternRegionDefault),
    list(0:4, 4:0)
  ) 
  
  
  # Construct region
  expect_equal(
    .UnnormalizeXY(0:4, 4:0, region = as.list(ternRegion20)),
    .UnnormalizeXY(0:4, 4:0, region = ternRegion20)
  )
  
  expect_equal(TernaryToXY(40, 60, 0, region = ternRegionA), c(0.5, 0))
  expect_equal(TernaryToXY(100, 0, 0, region = ternRegionA), c(0, cos(pi/6)))
  expect_equal(TernaryToXY(40, 0, 60, region = ternRegionA), c(-.5, 0))
  
  expect_equal(
    XYToTernary(0, cos(pi/6), region = ternRegionA),
    rbind(a = 1, b = 0, c = 0)
  )
  expect_equal(
    .UnnormalizeXY(0, cos(pi/6), region = ternRegionA),
    list(0, cos(pi/6))
  )
  expect_equal(
    .UnnormalizeXY(0.5, 0, region = ternRegionA),
    list(0.3, .3464),
    tolerance = 0.0001
  )
  expect_equal(
    .UnnormalizeXY(-1:1 / 2, c(0, cos(pi/6), 0), region = ternRegionA),
    list(-1:1 * .3, c(.3464, cos(pi/6), .3464)),
    tolerance = 0.0001
  )
})

test_that(".SetRegion() is stable", {
  original <- .SetRegion(ternRegionDefault)
  on.exit(options(ternRegion = original))
  expect_equal(getOption("ternRegion"), ternRegionDefault)
  .SetRegion(ternRegion20)
  expect_equal(getOption("ternRegion"), ternRegion20)
  expect_equal(.SetRegion(ternRegionA, set = FALSE), ternRegionA)
})

test_that(".RegionCorners() can set up region", {
  expect_equal(
    .RegionCorners(as.list(ternRegionA)),
    .RegionCorners(ternRegionA)
  )
})

test_that(".SetRegion() handles input types", {
  corners <- .RegionCorners(ternRegion20)
  original <- .SetRegion(lapply(1:3, function(i) corners[i, ]))
  on.exit(options(ternRegion = original))
  expect_equal(getOption("ternRegion"), ternRegion20)
  .SetRegion(ternRegionDefault)
  .SetRegion(as.data.frame(corners))
  expect_equal(unname(getOption("ternRegion")), unname(ternRegion20))
})

test_that(".SetRegion() prettifies", {
  range <- c(min = 10.5, max = 19.5)
  region <- cbind(a = range, b = range,
                  c = c(100 - sum(range), 100 - (2 * range[1])))
  regionList <- list(min = region[1, ], max = region[2, ])
  expect_true(.RegionIsValid(region))
  pretty <- c(min = 10, max = 20)
  pretty <- cbind(a = pretty, b = pretty, c = c(70, 80))
  expect_true(.RegionIsValid(pretty))
  
  original <- .SetRegion(
    regionList,
    prettify = NA
  )
  on.exit(options(ternRegion = original))
  expect_equal(getOption("ternRegion"), structure(region, class = "ternRegion"))
  
  .SetRegion(regionList, prettify = 10)
  expect_equal(getOption("ternRegion"), structure(
    pretty,
    tickLabels = list(a = 10:20, b = 10:20, c = 70:80),
    class = "ternRegion")
  )
})

test_that(".SetRegion() handles bad input", {
  expect_warning(
    original <- .SetRegion(list(min = rep(0, 3), max = rep(200, 3))),
    "Largest possible region is"
  )
  on.exit(options(ternRegion = original))
  
  expect_equal(getOption("ternRegion"), ternRegionDefault)
  
  expect_warning(
    .SetRegion(list(min = rep(0, 3), max = rep(0, 3))),
    "Region must have positive size"
  )
  expect_equal(getOption("ternRegion"), ternRegionDefault)
  
  .SetRegion(ternRegion20)
  expect_warning(
    expect_equal(
      .SetRegion(list(min = rep(0, 3), max = rep(200, 3)), set = 0),
      ternRegionDefault
    ),
    "Largest possible region is"
  )
  expect_equal(getOption("ternRegion"), ternRegion20)
  
  .SetRegion(ternRegion20)
  expect_warning(
    expect_equal(
      .SetRegion(list(min = rep(0, 3), max = rep(0, 3)), set = 0),
      ternRegionDefault
    ),
    "Region must have positive size"
  )
  expect_equal(getOption("ternRegion"), ternRegion20)
})

test_that("Region validation works", {
  expect_true(.RegionIsValid(ternRegionDefault))
  expect_true(.RegionIsValid(ternRegion20))
  expect_true(.RegionIsValid(ternRegionA))
  
  expect_true(.RegionIsEquilateral(cbind(c(10, 30), c(0, 20), c(60, 80))))
  expect_false(.RegionIsEquilateral(cbind(c(10, 30), c(0, 20), c(60, 81))))
  
  expect_false(.RegionInRange(cbind(c(-10, 30), c(0, 20), c(60, 81))))
  expect_false(.RegionInRange(cbind(c(10, 100.1), c(0, 20), c(60, 81))))
  
  expect_false(.RegionCorners100(cbind(c(10, 30), c(60, 80), c(0, 20))))
})
