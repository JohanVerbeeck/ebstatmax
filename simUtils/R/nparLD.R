#' Perform Hypothesis Test with nparLD
#'
#' Split the dataset according to the specified period and perform a hypothesis 
#' test.
#' 
#' `options$target` contains the name of the target variable in data.
#' `config$first_period_end` is the last point of time that belongs to the first 
#' study period.
#' `config$time_variable` is the name of the variable containing timepoints in 
#' data.
#' `config$subject_variable` is the name of the variable that identifies 
#' subjects in data.
#' `config$group_variable` is the name of the group variable in data.
#' `config$alpha` is the type-I error rate.
#'
#' @param data `data.table` with the simulation data
#' @param period study period (either 1 or 2)
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
#'
#' @return the test result (`TRUE` if H0 is rejected, `FALSE` otherwise)
#' @export
test_nparLD <- function(data,
                        period,
                        options,
                        config) {
  query <- data[[config$time_variable]] <= config$first_period_end
  if (period == 1) {
    data <- data[query]
  } else {  # period == 2
    data <- data[!query]
  }
  form <- as.formula(paste(
    options$target,
    paste(config$group_variable, config$time_variable, sep=" * "),
    sep=" ~ "))
  capture.output(
    p_value <- nparLD::nparLD(
      form,
      data,
      subject=config$subject_variable)$ANOVA.test[3,3]
  )
  return(p_value < config$alpha)
}