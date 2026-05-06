

#' Market data resampling
#'
#'`resample_data()` resamples the original data to other sampling frequency. This
#'function is based on `aggregatePrice` function from `highfrequency` package.
#'
#' @param mkt_data A `data.table` object of market data set.
#' @param time_column The column name that contains the timestamp, or time variable.
#' @param sample_freq The sampling frequency, e.g., second, minute.
#' @param sample_period The sampling period, e.g., 1, 5, 10
#' @param market_open The market open time that is in the format HH:MM:SS, 24-hour clock
#' @param market_close The market close time that is in the format HH:MM:SS, 24-hour clock
#' @param fill if there is no observation available at a specific timestamp, whether is should be
#' filled with the most recent value
#'
#' @import data.table highfrequency
#' @returns A `data.table` object of resampled market dataset ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#'
#' resampled_data <- resample(data,'time', "secs", 1, "00:00:00", "23:59:59")
#' ## Column with timestamp is "time"
#' ## resampled at 1 second level
#' # 24-hour market, market starts at 00:00:
#' }
resample <- function(mkt_data,
                     time_column,
                     sample_freq,
                     sample_period,
                     market_open,
                     market_close,
                     fill = TRUE) {
  mkt_data <- as.data.table(mkt_data)
  if (is.null(time_column)) {
    stop("A column with timestamps is needed.")
  } else{
    setnames(mkt_data, time_column, "DT")
  }
  ## if timezone is not UTC
  attr(mkt_data$DT, "tzone") <- "UTC"


  aggregate <- aggregatePrice(
    mkt_data,
    alignBy = sample_freq,
    alignPeriod = sample_period,
    marketOpen = market_open,
    marketClose = market_close,
    fill = TRUE,
    tz = "UTC"
  )

  return(aggregate)
}
