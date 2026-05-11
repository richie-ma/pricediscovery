
#' Price discovery analyses between multiple markets
#'
#' `price_discovery()` is a function to calculate price discovery measures under
#' Hasbrouck (1995)'s one-security-many-markets setting.
#'
#' @param data a `data.table` object of market data that contains all market prices.
#' Each column should represent each market price.
#' @param num_market Number of markets in the data.
#' @param price_columns A vector that includes all market price column names.
#' @param log_price Whether market prices should be transformed to natural logarithms,
#' the default is `TRUE`.
#' @param lag_selection Whether selecting VECM's lag based on information criteria.
#' @param vecm_max.lag When `lag_selection=TRUE`, the maximum lag length in the
#' vector error correction model (VECM).
#' @param lag_select_ceritera The information criterion is used to select VECM's lag.
#' @param vecm_lag The lag length is used in the VECM estimation when `lag_selection=FALSE`
#' @param coin_const Whether to include constant term into long-run equilibrium relationship.
#' @param coin_beta Whether the cointegrating beta vector is `[1, -1]`, which
#' is standard setting in Hasbrouck (1995).
#'
#' @returns A `data.table` object that stores component share, information share (IS),
#' and information leadership share (ILS).
#'
#' @import data.table vars tsDyn urca combinat highfrequency ggplot2
#' @importFrom stats cov
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test <- price.discovery(data = data,
#'                         num_market = 5,
#'                         price_columns = c("mkt1", "mkt2", "mkt3", "mkt4", "mkt5"),
#'                         log_price = TRUE,
#'                         vecm_max.lag = 60,
#'                         lag_selection = TRUE,
#'                         lag_select_ceritera = "SC",
#'                         coin_const = TRUE,
#'                         coin_beta = TRUE)
#'                         }
#'

price_discovery <- function(data,
                            num_market = NULL,
                            price_columns = NULL,
                            log_price = TRUE,
                            lag_selection = TRUE,
                            vecm_max.lag = NULL,
                            lag_select_ceritera = "SC",
                            vecm_lag = NULL,
                            coin_const = FALSE,
                            coin_beta = TRUE) {
  type <- NULL
  if (is.null(price_columns)) {
    stop("Price columns are needed. Insert the column names of the price data.")
  }


  if (isTRUE(lag_selection)) {
    if (is.null(vecm_max.lag)) {
      stop("The maximum lag number for VECM is needed. Insert any positive numbers.")
    }
  } else{
    if (is.null(vecm_lag)) {
      stop("The lag number for VECM is needed if the leg selection is not applied.")

    }
  }


  if (isTRUE(log_price)) {
    data <- data[, lapply(.SD, function(x) log(as.numeric(x))), .SDcols = price_columns]
  }else{
    data <- data[, lapply(.SD, as.numeric), .SDcols = price_columns]
  }


  ### Vector error correction model


  ## the cointegrating beta is (n-1) * 1 matrix of 1's and n-1 identity matrix
  ## beta=|1 -1  0|
  ##      |1  0 -1|

  ## If users do not specify the beta cointegrating vector, we will estimate directly.
  ## However, we do not let users estimate the information shares.

  if (isTRUE(coin_beta)) {
    if (num_market == 2) {
      beta = 1
    } else{
      beta <- t(cbind(
        matrix(1, nrow = num_market - 1, ncol = 1),
        matrix = -diag(num_market - 1)
      ))
    }
    if (isTRUE(coin_const)) {
      for (a in 2:length(price_columns)) {
        data <-  data[, (paste0("ect_", a)) := mean(get(price_columns[1]) - get(price_columns[a]))]
        data <- data[, (price_columns[a]) := get(price_columns[a]) + get(paste0("ect_", a))]
        data[, (paste0("ect_", a)) := NULL]


      }

    }

    if (isTRUE(lag_selection)) {
      var <- VARselect(
        data[, price_columns, with = FALSE],
        lag.max = vecm_max.lag,
        type = "none",
        season = NULL,
        exogen = NULL
      )

      if (lag_select_ceritera == "SC") {
        var.lag <- as.numeric(var$selection["SC(n)"])

      } else{
        if (!lag_select_ceritera %in% c('AIC', 'HQ', 'FPE', "SC")) {
          stop(
            'Lag selection ceritera should be one of the following: "AIC", "HQ", "SC", "FPE". Please check your input.'
          )



        } else{
          var.lag <- as.numeric(var$selection[paste0(lag_select_ceritera, "(n)")])

        }

      }

      vecm <- VECM(
        data,
        lag = ifelse(var.lag > 1, var.lag - 1, 1),
        r = num_market - 1,
        include = "none",
        beta = beta,
        estim = "ML",
        LRinclude = "none"
      )

    } else{
      vecm <- VECM(
        data,
        lag = vecm_lag,
        r = num_market - 1,
        include = "none",
        beta = beta,
        estim = "ML",
        LRinclude = "none"
      )

    }
  } else{
    if (isFALSE(coin_const)) {
      vecm <- VECM(
        data,
        lag = vecm_lag,
        r = num_market - 1,
        include = "none",
        beta = NULL,
        estim = "ML",
        LRinclude = "none"
      )

    } else{
      vecm <- VECM(
        data,
        lag = vecm_lag,
        r = num_market - 1,
        include = "none",
        beta = NULL,
        estim = "ML",
        LRinclude = "const"
      )


    }


  }



  ######### obtaining the permanent price component
  #### This could be done iteratively through the cumulative IRF
  ###  The VMA coefficient corresponds to the IRF

  IRF <- irf(vecm,
             n.ahead = 100000,
             ortho = FALSE,
             boot = FALSE)  #### The default is cumulative IRF
  ## One does not need to calculate the cumulative IRF manually

  IRF_list <- list()

  for (k in 1:num_market) {
    IRF_list[[k]] <- as.data.table(IRF$irf[[k]])[.N]

  }

  PhiInf <- rbindlist(IRF_list)
  PhiInf <- transpose(PhiInf)

  colnames(PhiInf) <- price_columns
  ## all of rows are identical


  #####################  Calculating price discovery share metrics ###########################

  # Let's calculate Hasbrouck (1995)'s information shares

  ## This depends on the Cholesky's decomposition
  ## The variance of residual might not be diagonal, which means the residual are correlated
  ## The information shares are not uniquely defined.

  ## Information shares boundary

  if (coin_beta) {
    ## getting all the permutations

    all_orders <- permn(seq(1, num_market, 1))

    residual_cov <- cov(vecm$residuals)

    IS <- matrix(nrow = length(all_orders), ncol = 2 * num_market)
    colnames(IS) <- c(paste0("order", seq(1, num_market, 1)), price_columns)

    for (j in 1:length(all_orders)) {
      order <- all_orders[[j]]
      omega   <- residual_cov[order, order]

      IS[j, 1:num_market] <- order

      perm.varaince <- as.matrix(PhiInf)[1, order] %*% omega %*% as.matrix(PhiInf)[1, order] ## denominator of IS
      numerator <- as.vector((as.matrix(PhiInf)[1, order] %*% t(chol(omega)))^2)

      IS[j, (num_market + 1):(2 * num_market)] <- c(numerator / as.numeric(perm.varaince))[sort(order, i =
                                                                                                  T)$ix]  ## vector index return
    }

    ### we take the midpoint

    IS <- as.data.table(IS)
    IS <- IS[, lapply(.SD, function(x)
      mean(x)), .SDcols = price_columns][, type := "IS"]
    colnames(IS)[1:num_market] <- paste0(price_columns)

    PT <- PhiInf[1, lapply(.SD, function(x) {
      x / rowSums(PhiInf)[1]
    }), .SDcols = price_columns][, type := "PT"]

    CS <- PhiInf[1, lapply(.SD, function(x) {
      abs(x) / rowSums(abs(PhiInf))[1]
    }), .SDcols = price_columns][, type := "CS"]

    ILS <- rbind(IS, CS)
    ILS <- rbind (ILS, (ILS[1, 1:num_market] / ILS[2, 1:num_market]), fill =
                    TRUE)

    deno <- rowSums((ILS[, 1:num_market])^2)[3]

    ILS[3, 1:num_market] <- ILS[3, lapply(.SD, function(x) {
      x^2 / deno
    }), .SDcols = price_columns]
    ILS[3, type := "ILS"]

  } else{
    stop(
      "Information shares are not defined without the user-specified cointegrating vector. Please set coin_beta=TRUE."
    )
  }

  ### We have component share as the VMA coefficient in PhiInf matrix




  #############################

  info.shares <- ILS

  return(info.shares)



}
