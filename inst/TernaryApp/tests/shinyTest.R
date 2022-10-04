library("methods", exclude = c("removeClass", "show"))
library("shinytest")
shinytest::testApp("../")
