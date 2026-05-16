#' Record and plot daily health updates
#'
#' `dailyUpdate()` is an interactive function for recording daily health
#' updates and saving them to an RDS file. The function prompts the user to
#' enter information on date/time, proteinurea, exercise, medication use,
#' mental health, physical health, and free-text notes. Entries are saved to
#' `JLD_df.RDS` in the folder specified by `wd`.
#'
#' After recording an update, the function can also produce simple plots of
#' the saved data, including scaled time trends for selected variables and
#' scatterplots of one variable against another.
#'
#' @param wd Character string. Path to the folder where `JLD_df.RDS` should be
#'   saved and read from. If the folder does not exist, it will be created.
#'
#' @return Invisibly returns `NULL`. The function is primarily used for its
#'   interactive prompts, saved RDS output, and plots.
#'
#' @details
#' The saved RDS file contains one row per recorded update, with the following
#' variables:
#' \describe{
#'   \item{dt}{Date-time of the update, stored as a POSIXct object in the Europe/London timezone.}
#'   \item{prot}{Proteinurea reading.}
#'   \item{pa_load}{Exercise load from Garmin, if exercise was recorded.}
#'   \item{pa_hr}{Average heart rate during exercise, if exercise was recorded.}
#'   \item{pred}{Indicator for current prednisone use, where 1 = yes and 0 = no.}
#'   \item{pred_level}{Prednisone dose.}
#'   \item{rit}{Indicator for current rituximab use, where 1 = yes and 0 = no.}
#'   \item{mh}{Mental health rating from 1 to 5.}
#'   \item{ph}{Physical health rating from 1 to 5.}
#'   \item{notes}{Free-text notes.}
#' }
#'
#' @examples
#' \dontrun{
#' dailyUpdate("~/Documents/JLD_updates")
#' }
#'
#'
#' @family JLDkidnupdates functions
#' @export

dailyUpdate <- function(wd) {

  cat(
    "Good morning Jamie! It is very nice to see you today.\n",
    "\nWould you like to record a new update?\n",
    "Enter 1 for yes and 0 for no.\n\n",
    sep = ""
  )

  cont <- readline("Record update?: ")

  if (cont == "1") {

    repeat {

      repeat {

        # Date time
        cat(
          "Please enter the date in the format yyyy-mm-dd.\n\n",
          sep = ""
        )

        date <- readline("Date: ")

        cat(
          "\nPlease enter the time in 24-hour format, for example: 22:49.\n",
          "By default, the time zone is set to London time.\n",
          "If you are travelling, please enter the time in London, not your current location.\n\n",
          sep = ""
        )

        time <- readline("Time: ")

        dt <- as.POSIXct(
          paste(date, time),
          format = "%Y-%m-%d %H:%M",
          tz = "Europe/London"
        )

        # Proteinurea
        cat(
          "\nGreat! Let's get started with proteinurea.\n",
          "Please enter your latest proteinurea reading.\n",
          "If you do not have a reading to report from today, enter NA.\n\n",
          sep = ""
        )

        prot <- readline("Proteinurea: ")

        if (prot == "NA") {
          prot <- NA
        }

        prot <- as.numeric(prot)

        # Physical activity level
        cat(
          "\nDid you do exercise yesterday? If so, please enter the exercise load from your Garmin watch.\n",
          "This can be found under the Stats tab for the activity.\n",
          "Enter NA if you did not do any exercise.\n\n",
          sep = ""
        )

        pa_load <- readline("Exercise load: ")

        if (pa_load == "NA") {
          pa_load <- NA
        }

        pa_load <- as.numeric(pa_load)

        if (!is.na(pa_load)) {
          cat(
            "\nIf you did exercise yesterday, please enter your average heart rate from your Garmin watch.\n\n",
            sep = ""
          )

          pa_hr <- readline("Average heart rate: ")
          pa_hr <- as.numeric(pa_hr)
        } else {
          pa_hr <- NA
        }

        # Medication
        cat(
          "\nAre you currently on prednisone?\n",
          "Please enter 1 for yes or 0 for no.\n\n",
          sep = ""
        )

        pred <- readline("Prednisone?: ")

        if (pred == "1") {
          cat(
            "\nPlease enter prednisone dose.\n\n",
            sep = ""
          )

          pred_level <- readline("Prednisone dose: ")
          pred_level <- as.numeric(pred_level)
        } else {
          pred_level <- 0
        }

        cat(
          "\nAre you currently on rituximab?\n",
          "Please enter 1 for yes or 0 for no.\n\n",
          sep = ""
        )

        rit <- readline("Rituximab?: ")
        rit <- as.numeric(rit)

        # Physical and mental health
        cat(
          "\nOn a scale of 1 to 5, where 1 is worst and 5 is best,\n",
          "how would you rate your fatigue today?\n\n",
          sep = ""
        )

        ft <- readline("Fatigue: ")
        ft <- as.numeric(mh)

        cat(
          "\nOn a scale of 1 to 5, where 1 is worst and 5 is best,\n",
          "how would you rate your joint pain today?\n\n",
          sep = ""
        )

        jp <- readline("Joint pain: ")
        jp <- as.numeric(ph)

        cat(
          "\nOn a scale of 1 to 5, where 1 is worst and 5 is best,\n",
          "how would you rate your headache today?\n\n",
          sep = ""
        )

        ha <- readline("Headache: ")
        ha <- as.numeric(ha)

        cat(
          "\nOn a scale of 1 to 5, where 1 is worst and 5 is best,\n",
          "how would you rate your sleep last night?\n\n",
          sep = ""
        )

        sl <- readline("Sleep: ")
        sl <- as.numeric(ph)

        # Notes
        cat(
          "\nDo you have any notes to add?\n",
          "Please enter as free text.\n\n",
          sep = ""
        )

        notes <- readline("Notes: ")

        # Produce new row
        row <- data.frame(
          dt = dt,
          prot = prot,
          pa_load = pa_load,
          pa_hr = pa_hr,
          pred = pred,
          pred_level = pred_level,
          rit = rit,
          ft = ft,
          jp = jp,
          ha = ha,
          sl = sl,
          notes = notes
        )

        cat("\nPlease check this entry:\n\n")
        check_row <- row
        colnames(check_row) <- c("Date time",
                                 "Proteinurea",
                                 "Yesterday's exercise load",
                                 "Yesterday's avg heart rate during exercise",
                                 "Prednisone (1=yes;0=no)",
                                 "Prednisone dose",
                                 "Rituximab (1=yes;0=no)",
                                 "Fatigue rating",
                                 "Joint pain rating",
                                 "Headache rating",
                                 "Last night sleep quality rating",
                                 "Notes")
        print(check_row)

        cat("\nSave this entry?",
            "\nEnter 1 for yes, 2 to start again, or 3 to exit the function without saving.\n")
        confirm <- readline("")

        if (confirm == "1") {
          break
        } else if (confirm == "3") {
          cat("\nNo update saved. Ta ta for now!\n")
          return(invisible(NULL))
        } else {
          cat("\nNo problem - let's start this entry again.\n\n")
        }
      }

      # Save in working directory
      if (!dir.exists(wd)) {
        dir.create(wd, recursive = TRUE)
      }

      file <- file.path(wd, "JLD_df.RDS")

      if (!file.exists(file)) {
        saveRDS(row, file)
      } else {
        jld <- readRDS(file)

        if (!is.data.frame(jld)) {
          stop("Existing JLD_df.RDS is not a data frame. Not updating file.")
        }

        jld <- rbind(jld, row)
        saveRDS(jld, file)
      }

      cat("\nUpdate saved successfully.\n")
      cat("File saved to: ", file, "\n", sep = "")

      cat("\nWould you like to record another entry?",
          "\nEnter 1 for yes or 0 for no.\n\n")
      confirm <- readline("Another entry?: ")

      if (confirm == "0") {
        cat("\nAll updates recorded and saved. :)\n\n")
        break
      }
    }

  } else {
    cat("\nNo update recorded today.\n")
  }

  repeat {
    cat("\nOkay, let's do some plotting!\n",
        "\nYou can plot:\n\n",
        "1: Time trends in all variables\n",
        "2: Var Y plotted against Var X\n",
        "3: Actually I don't want to plot today!\n",
        "4: I want a secret fourth thing :)\n\n")
    plot <- readline("Select a number: ")

    if (plot != "4" & plot != "3") {
      # Select file to plot with
      cat(
        "\nWhich dataset would you like to use for plotting?\n\n",
        "1: Real data\n",
        "2: Test/example data\n\n",
        sep = ""
      )


      data_choice <- readline("Select a number: ")

      if (data_choice == "2") {
        filename <- file.path(wd, "JLD_df_test.RDS")
      } else {
        filename <- file.path(wd, "JLD_df.RDS")
      }

      if (!file.exists(filename)) {
        cat("\nThat data file does not exist yet.\n")
        cat("Expected file: ", filename, "\n", sep = "")
        return(invisible(NULL))
      }

      df <- readRDS(filename)
    }

    if (plot == "3") {
      cat("\nNo problem! See ya later :)")
      return(invisible(NULL))
    } else if (plot == "1") {

      repeat {
        cat("\nWhich variables would you like to plot?\n\n",
            "1. Proteinurea\n",
            "2. Exercise load\n",
            "3. Average heart rate\n",
            "4. Prednisone level\n",
            "5. Fatigue\n",
            "6. Joint pain\n",
            "7. Headache\n",
            "8. Sleep quality")

        # Define plausible ranges
        ranges <- list(
          prot = c(0, 500),
          pa_load = c(0, 300),
          pa_hr = c(100, 210),
          pred_level = c(0, 60),
          ft = c(1, 5),
          jp = c(1, 5),
          ha = c(1,5),
          sl = c(1,5)
        )

        vars <- readline("Enter numbers separated by a space: \n")
        vars <- as.numeric(unlist(strsplit(vars," ")))
        var_names <- c("prot","pa_load","pa_hr",
                       "pred_level","ft","jp","ha","sl")
        var_names <- var_names[vars]
        names <- c("Proteinurea","Exercise load","Average heart rate",
                   "Prednisone level","Fatigue","Joint pain",
                   "Headache","Sleep quality")
        names <- names[vars]
        ranges <- ranges[var_names]

        # Point types
        points <- c(15,16,17,3,4,5)

        # Colours
        colours <- RColorBrewer::brewer.pal(6,"Dark2")

        # Plot margins
        par(mar=c(7,5,1,1))

        # Open plotting window
        plot(1,1,type='n',xlim=c(min(df$dt - lubridate::days(1)),max(df$dt)),
             ylim=c(0,1.4),xaxt='n',xlab="",ylab="Scaled values",
             yaxt='n')
        date_range <- c(df$dt-lubridate::days(1),max(df$dt))

        axis(1,at=date_range,
             labels=unlist(data.table::tstrsplit(date_range," ",keep=1)),cex=.8,las=2)
        axis(2,at=seq(0,1,.5))

        # Plot time trend of each var
        for (i in 1:length(var_names)) {
          col <- df[var_names[i]]

          if (names(col) %in% c("pa_load","pa_hr","sl")) {
            day_prev <- df$dt - lubridate::days(1)
            col <- cbind(day_prev,col)
          } else {
            col <- cbind(df$dt,col)
          }

          col <- na.omit(col)
          col[,2] <- (col[,2] - ranges[[i]][1]) /
            (ranges[[i]][2] - ranges[[i]][1])
          lines(x=col[,1],y=col[,2], type='o',pch=points[i],
                col=colours[i],lwd=2)
        }

        # Legend
        legend("topright",pch=points[1:length(var_names)],
               col = colours,legend = names,bty='n')

        cat("\nWould you like to make another time series plot?\n\n",
            "1: Another time series plot\n",
            "2: Another kind of plot\n",
            "3. That's all for today\n\n")
        cont <- readline("Enter number here: ")

        if (cont == "2") {
          break
        } else if (cont == "3") {
          cat("\nGoodbye for now!")
          return(invisible(NULL))
        }
      }

    } else if (plot == "2") {
      repeat{
        cat("\nWhich variables would you like to plot?\n",
            "The first variable you select will be the x-axis\n",
            "and the second the y-axis.\n\n",
            "1. Proteinurea\n",
            "2. Exercise load\n",
            "3. Average heart rate\n",
            "4. Prednisone level\n",
            "5. Fatigue\n",
            "6. Joint pain\n",
            "7. Headache\n",
            "8. Sleep quality")

        vars <- readline("Enter numbers separated by a space: \n")
        vars <- as.numeric(unlist(strsplit(vars," ")))
        var_names <- c("prot","pa_load","pa_hr",
                       "pred_level","ft","jp","ha","sl")
        var_names <- var_names[vars]
        names <- c("Proteinurea","Exercise load","Average heart rate",
                   "Prednisone level","Fatigue","Joint pain","Headache",
                   "Sleep quality")
        names <- names[vars]

        x <- df[,var_names[1]]
        y <- df[,var_names[2]]

        par(mar=c(5,5,1,1))

        plot(x=x,
             y=y,xlab=names[1],
             ylab=names[2],pch=21,cex=1.4,bg="grey",
        )
        fit <- lm(y ~ x)
        abline(fit, lty=2)

        cat("\nWould you like to make another X-Y plot?\n\n",
            "1: Another X-Y plot\n",
            "2: Another kind of plot\n",
            "3. That's all for today\n\n")
        cont <- readline("Enter number here: ")

        if (cont == "2") {
          break
        } else if (cont == "3") {
          cat("\nGoodbye for now!")
          return(invisible(NULL))
        }
      }
    } else {

      yn <- round(runif(1,0,1))

      if (yn == 1) {

        cat("\nCan I interest you in a bad pickup line?\n",
            "1 for yes, 0 for no.\n\n")
        pickup <- readline("Yay or nay?: \n")

        if (pickup == "1") {
          repeat {
            pickups <- c("Are you a parking ticket?\nCause you've got fine written all over you.\n",
                         "Are you from Tennessee? Because you're the only 10 I see.\n",
                         "Are you sure you’re not tired?\nYou’ve been running through my mind all day.\n",
                         "Do you know what the Little Mermaid and I have in common?\nWe both want to be part of your world.\n",
                         "Are your parents bakers? Because you're a cutie pie.\n",
                         "If you were a vegetable, you’d be a cute-cumber.\n",
                         "I hope you know CPR because you are taking my breath away.\n",
                         "I'd take you to the movies, but\nthey don't let you bring in your own snacks.\n",
                         "Do you have a sunburn or are you always this hot?\n",
                         "Are you a keyboard? Because you might just be my type.\n")
            n <- round(runif(1,min=1,max=10))
            cat(pickups[n])
            cat("\nAnother one queen?\n\n",
                "1: Oh hell yes.",
                "2: Please no.")
            more <- readline("Enter a number: \n")

            if (more == "2") {
              cat("\nOkay love you bye!")
              return(invisible(NULL))
            }

          }

        } else {
          cat("\nPleasure doing business with you. Come again soon.")
          return(invisible(NULL))
        }
      } else {
        showEasterEgg <- function() {
          img_path <- system.file("extdata", "hijamie.png", package = "JLDkidnupdates")

          img <- png::readPNG(img_path)

          old_par <- par(no.readonly = TRUE)
          on.exit(par(old_par))

          par(mar = c(0, 0, 0, 0))
          plot.new()
          rasterImage(img, 0, 0, 1, 1)

          invisible(NULL)
        }

        showEasterEgg()
        return(invisible(NULL))

      }
    }
  }
}


