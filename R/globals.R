#' Default quantiles
default_quantiles <-
  c(0.01,
    0.025,
    0.05,
    0.1,
    0.2,
    0.25,
    0.5,
    0.75,
    0.8,
    0.9,
    0.95,
    0.975,
    0.99)

#' Default weights
default_weights <- seq(0, 1, by = 0.005)

# Colors
#' Custom dark blue
tipmap_darkblue <- grDevices::rgb(0, 102, 153, max = 255)
#' Custom light red
tipmap_lightred <- grDevices::rgb(204, 51, 51, max = 255)

# Utils
utils::globalVariables(c(
  "default_quantiles",
  "default_weights",
  "tipmap_darkblue",
  "tipmap_lightred"
))
utils::globalVariables(
  c(
    "t.0.025",
    "t.0.05",
    "t.0.1",
    "t.0.2",
    "t.0.8",
    "t.0.9",
    "t.0.95",
    "t.0.975",
    "t.est",
    "x.at",
    "x.col"
  )
)

# Set environmental variable
# Sys.getenv("_R_CHECK_RD_VALIDATE_RD2HTML")
Sys.setenv("_R_CHECK_RD_VALIDATE_RD2HTML" = "F")

