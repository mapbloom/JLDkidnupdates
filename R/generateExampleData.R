#' Generate example daily health update data
#'
#' `generateExampleData()` creates a small simulated dataset with the same
#' structure as the dataset produced by [dailyUpdate()]. It is mainly a helper
#' function used by [saveExampleData()].
#'
#' For testing the package, users should usually call [saveExampleData()]
#' instead. `saveExampleData()` creates the example dataset and saves it using
#' the expected test-data filename, `JLD_df_test.RDS`, so it can be selected
#' from the plotting menu in [dailyUpdate()].
#'
#' @param n Integer. Number of rows to generate. Defaults to 15.
#' @param seed Integer. Random seed used to make the simulated data reproducible.
#'   Defaults to 123.
#'
#' @return A data frame with simulated daily update data.
#'
#' @keywords internal
generateExampleData <- function(n = 15, seed = 123) {

  set.seed(seed)

  ranges <- list(
    prot = c(0, 500),
    pa_load = c(0, 300),
    pa_hr = c(100, 220),
    pred_level = c(0, 60),
    ft = c(1, 5),
    jp = c(1, 5),
    ha = c(1,5),
    sl = c(1,5)
  )

  df <- data.frame(
    dt = as.POSIXct("2026-05-01 09:00", tz = "Europe/London") +
      seq(0, by = 24 * 60 * 60, length.out = n),

    prot = sample(
      c(0, 50, 100, 500, NA),
      n,
      replace = TRUE,
      prob = c(0.35, 0.25, 0.20, 0.10, 0.10)
    ),

    pa_load = sample(
      c(seq(ranges$pa_load[1], ranges$pa_load[2], by = 10), NA),
      n,
      replace = TRUE,
      prob = c(rep(1, length(seq(0, 300, by = 10))), 6)
    ),

    pa_hr = sample(
      c(ranges$pa_hr[1]:ranges$pa_hr[2], NA),
      n,
      replace = TRUE,
      prob = c(rep(1, length(ranges$pa_hr[1]:ranges$pa_hr[2])), 8)
    ),

    pred = sample(
      c(0, 1),
      n,
      replace = TRUE,
      prob = c(0.35, 0.65)
    ),

    pred_level = 0,

    rit = sample(
      c(0, 1),
      n,
      replace = TRUE,
      prob = c(0.8, 0.2)
    ),

    ft = sample(
      c(ranges$ft[1]:ranges$ft[2], NA),
      n,
      replace = TRUE,
      prob = c(rep(1, 5), 2)
    ),

    jp = sample(
      c(ranges$jp[1]:ranges$jp[2], NA),
      n,
      replace = TRUE,
      prob = c(rep(1, 5), 2)
    ),

    ha = sample(
      c(ranges$ha[1]:ranges$ha[2], NA),
      n,
      replace = TRUE,
      prob = c(rep(1, 5), 2)
    ),

    sl = sample(
      c(ranges$sl[1]:ranges$sl[2], NA),
      n,
      replace = TRUE,
      prob = c(rep(1, 5), 2)
    ),

    notes = sample(
      c(
        "Feeling okay today.",
        "Tired but manageable.",
        "No major changes.",
        "Had some symptoms today.",
        "",
        NA
      ),
      n,
      replace = TRUE
    )
  )

  df$pred_level[df$pred == 1] <- sample(
    seq(ranges$pred_level[1], ranges$pred_level[2], by = 5),
    sum(df$pred == 1),
    replace = TRUE
  )

  df$pa_hr[is.na(df$pa_load)] <- NA
  return(df)
}
