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
