#' Save example daily health update data
#'
#' `saveExampleData()` creates a simulated example dataset and saves it as
#' `JLD_df_test.RDS` in the folder specified by `wd`. This allows the plotting
#' functionality in [dailyUpdate()] to be tested without creating or modifying
#' the real `JLD_df.RDS` data file.
#'
#' @param wd Character string. Path to the folder where the example RDS file
#'   should be saved. If the folder does not exist, it will be created.
#' @param n Integer. Number of rows to generate. Defaults to 15.
#' @param seed Integer. Random seed used to make the simulated data reproducible.
#'   Defaults to 123.
#' @param overwrite Logical. Should an existing `JLD_df_test.RDS` file be
#'   overwritten? Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the saved example RDS file.
#'
#' @examples
#' \dontrun{
#' saveExampleData("~/Documents/JLD_updates")
#' }
#'
#' @family JLDkidnupdates functions
#' @export
saveExampleData <- function(wd, n = 15, seed = 123, overwrite = FALSE) {

  if (!dir.exists(wd)) {
    dir.create(wd, recursive = TRUE)
  }

  file <- file.path(wd, "JLD_df_test.RDS")

  if (file.exists(file) && !overwrite) {
    stop(
      "A test dataset already exists. ",
      "Use overwrite = TRUE if you want to replace it."
    )
  }

  df <- generateExampleData(n = n, seed = seed)

  saveRDS(df, file)

  cat("\nExample data saved successfully.\n")
  cat("File saved to: ", file, "\n", sep = "")

  invisible(file)
}
