#' @include utils.R
NULL

#' QS Seasonality Test (auto-correlations at seasonal lags)
#'
#' @param data the input time series.
#' @param period the periodicity of the data.
#' @param nyears \code{integer} that corresponds to number of periods number of periods starting from the end of the series:
#' in periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#'
#' @return
#' @export
#'
#' @examples
seasonality.qs<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "qsTest",
         as.numeric(data), as.integer(period), as.integer(nyears))
  return (jd2r_test(jtest))
}

#' Kruskall-Wallis Seasonality Test
#'
#'
#' @inheritParams seasonality.qs
#'
#' @details Non parametric test on the ranks
#' @return
#' @export
#'
#' @examples
seasonality.kruskalwallis<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "kruskalWallisTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (jd2r_test(jtest))
}

#' Periodogram Seasonality Test
#'
#' @inheritParams seasonality.qs
#'
#' @details Tests on the sum of a periodogram at seasonal frequencies
#' @return
#' @export
#'
#' @examples
seasonality.periodogram<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "periodogramTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (jd2r_test(jtest))
}

#' Friedman Seasonality Test
#'
#' @inheritParams seasonality.qs
#'
#' @details Non parametric test ("ANOVA"-type)
#' @return
#' @export
#'
#' @examples
seasonality.friedman<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "friedmanTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (jd2r_test(jtest))
}

#' F-test on seasonal dummies
#'
#' @inheritParams seasonality.qs
#' @param model the model to use for the residuals.
#' @details Estimation of a model with seasonal dummies. Joint F-test on the coefficients of the dummies.
#' @return
#' @export
#'
#' @examples
seasonality.f<-function(data, period, model=c("AR", "D1", "WN"), nyears=0){
  model<-match.arg(model)
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "fTest",
                as.numeric(data), as.integer(period), model, as.integer(nyears))
  return (jd2r_test(jtest))
}

#' “X12” Test On Seasonality
#'
#' @inheritParams seasonality.qs
#' @param firstperiod
#' @param mul boolean indicating if the seasonal decomposition is multiplicative (\code{mul = TRUE}) or additive (\code{mul = FALSE}).
#' @details Combined test on the presence of identifiable seasonality (see Ladiray and Quenneville, 1999).
#' @return
#' @export
#'
#' @examples
seasonality.combined<-function(data, period, firstperiod=1, mul=T){
  jctest<-.jcall("demetra/sa/r/SeasonalityTests", "Ljdplus/sa/tests/CombinedSeasonality;", "combinedTest",
                as.numeric(data), as.integer(period), as.integer(firstperiod-1), as.logical(mul))
  q<-.jcall("demetra/sa/r/SeasonalityTests",  "[B", "toBuffer", jctest)
  p<-RProtoBuf::read(sa.CombinedSeasonalityTest, q)
  return (list(
    seasonality=enum_extract(sa.IdentifiableSeasonality, p$seasonality),
    kruskalwallis=p2r_test(p$kruskal_wallis),
    stable=p2r_anova(p$stable_seasonality),
    evolutive=p2r_anova(p$evolutive_seasonality)))
}

#' Trading Days Test
#'
#' @inheritParams seasonality.qs
#' @param s a \code{ts} object that corresponds to the input time series to test.
#' @param model the model to use for the residuals. See details.
#'
#' @details \loadmathjax
#' The function performs a residual seasonality test that is a joint F-Test on the coefficients of trading days regressors.
#' Several specifications can be used on the model:
#' \itemize{
#' \item \code{model = "WN"} the following model is used:
#' \mjsdeqn{
#' y_t - \bar y =\beta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "D1"} (the default) the following model is used:
#' \mjsdeqn{
#' \Delta y_t - \overline{\Delta y} =\beta \Delta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "DY"} the following model is used:
#' \mjsdeqn{
#' \Delta_s y_t - \overline{\Delta_s y} =\beta \Delta_s TD_t +  \varepsilon_t
#' }
#' \item \code{model = "DYD1"} the following model is used:
#' \mjsdeqn{
#' \Delta_s\Delta y_t - \overline{\Delta_s \Delta y} =\beta \Delta_s \Delta TD_t +  \varepsilon_t
#' }
#' }
#' @export
#'
#' @examples
td.f<-function(s, model=c("D1", "DY", "DYD1", "WN", "AIRLINE", "R011", "R100"), nyears=0){
  model<-match.arg(model)
  jts<-ts_r2jd(s)
  jtest<-.jcall("demetra/sa/r/TradingDaysTests", "Ldemetra/stats/StatisticalTest;", "fTest",
                jts, model, as.integer(nyears))
  return (jd2r_test(jtest))
}


