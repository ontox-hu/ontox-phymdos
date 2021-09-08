## test perform_dunnett
type <- "Dunnett"
compound = "test"

cowplot::plot_grid(
perform_dunnett(
  df = df,
  compound = compound,
  variance_heterogeneity = FALSE,
  alternative = "two.sided",
  type = type
),
perform_dunnett(
  df = df,
  compound = compound,
  variance_heterogeneity = FALSE,
  alternative = "less",
  type = type
),
perform_dunnett(
  df = df,
  compound = compound,
  variance_heterogeneity = FALSE,
  alternative = "greater",
  type = type
))

type <- "Williams"

cowplot::plot_grid(
  perform_dunnett(
    df = df,
    compound = compound,
    variance_heterogeneity = FALSE,
    alternative = "two.sided",
    type = type
  ),
  perform_dunnett(
    df = df,
    compound = compound,
    variance_heterogeneity = FALSE,
    alternative = "less",
    type = type
  ),
  perform_dunnett(
    df = df,
    compound = compound,
    variance_heterogeneity = FALSE,
    alternative = "greater",
    type = type
  ))

variance_heterogeneity <- TRUE
type = "Dunnett"

cowplot::plot_grid(
  perform_dunnett(
    df = df,
    compound = compound,
    variance_heterogeneity = variance_heterogeneity,
    alternative = "two.sided",
    type = type
  ),
  perform_dunnett(
    df = df,
    compound = compound,
    variance_heterogeneity = variance_heterogeneity,
    alternative = "less",
    type = type
  ),
  perform_dunnett(
    df = df,
    compound = compound,
    variance_heterogeneity = variance_heterogeneity,
    alternative = "greater",
    type = type
  ))
