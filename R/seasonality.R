#' @include utils.R
NULL


#' QS Seasonality Test
#'
#' QS (modified seasonal Ljung-Box) test
#'
#' @param data the input data.
#' @param period Tested periodicity.
#' @param nyears Number of number of periods number of cycles considered in the test, at the end of the series:
#' in periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#'
#' @return A \code{c("JD3_TEST", "JD3")} object (see \code{\link[rjd3toolkit]{statisticaltest}} for details).
#' @export
#'
#' @examples
#' seasonality.qs(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' seasonality.qs(rjd3toolkit::randomsT(2, 1000), 7)
seasonality.qs<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "qsTest",
         as.numeric(data), as.integer(period), as.integer(nyears))
  return (rjd3toolkit::jd2r_test(jtest))
}

#' Kruskall-Wallis Seasonality Test
#'
#'
#' @inheritParams seasonality.qs
#'
#' @details Non parametric test on the ranks
#' @return A \code{c("JD3_TEST", "JD3")} object (see \code{\link[rjd3toolkit]{statisticaltest}} for details).
#' @export
#'
#' @examples
#' seasonality.kruskalwallis(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' seasonality.kruskalwallis(rjd3toolkit::randomsT(2, 1000), 7)
seasonality.kruskalwallis<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "kruskalWallisTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (rjd3toolkit::jd2r_test(jtest))
}

#' Periodogram Seasonality Test
#'
#' @inheritParams seasonality.qs
#'
#' @details Tests on the sum of a periodogram at seasonal frequencies
#' @return A \code{c("JD3_TEST", "JD3")} object (see \code{\link[rjd3toolkit]{statisticaltest}} for details).
#' @export
#'
#' @examples
#' seasonality.periodogram(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' seasonality.periodogram(rjd3toolkit::randomsT(2, 1000), 7)
seasonality.periodogram<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "periodogramTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (rjd3toolkit::jd2r_test(jtest))
}

#' Friedman Seasonality Test
#'
#' @inheritParams seasonality.qs
#'
#' @details Non parametric test ("ANOVA"-type)
#' @return A \code{c("JD3_TEST", "JD3")} object (see \code{\link[rjd3toolkit]{statisticaltest}} for details).
#' @export
#'
#' @examples
seasonality.friedman<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "friedmanTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (rjd3toolkit::jd2r_test(jtest))
}

#' F-test on seasonal dummies
#'
#' @inheritParams seasonality.qs
#' @param model the model to use for the residuals.
#' @details Estimation of a model with seasonal dummies. Joint F-test on the coefficients of the dummies.
#' @return A \code{c("JD3_TEST", "JD3")} object (see \code{\link[rjd3toolkit]{statisticaltest}} for details).
#' @export
#'
#' @examples
#' seasonality.f(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' seasonality.f(rjd3toolkit::randomsT(2, 1000), 7)
seasonality.f<-function(data, period,
                        model=c("AR", "D1", "WN"),
                        nyears=0){
  model<-match.arg(model)
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "fTest",
                as.numeric(data), as.integer(period), model, as.integer(nyears))
  return (rjd3toolkit::jd2r_test(jtest))
}


#' "X12" Test On Seasonality
#'
#' @inheritParams seasonality.qs
#' @param firstperiod
#' @param mul boolean indicating if the seasonal decomposition is multiplicative (\code{mul = TRUE}) or additive (\code{mul = FALSE}).
#' @details Combined test on the presence of identifiable seasonality (see Ladiray and Quenneville, 1999).
#' @export
#'
#' @examples
#' seasonality.combined(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' seasonality.combined(rjd3toolkit::randomsT(2, 1000), 7)
seasonality.combined<-function(data, period, firstperiod=1, mul=T){
  jctest<-.jcall("demetra/sa/r/SeasonalityTests", "Ljdplus/sa/tests/CombinedSeasonality;", "combinedTest",
                as.numeric(data), as.integer(period), as.integer(firstperiod-1), as.logical(mul))
  q<-.jcall("demetra/sa/r/SeasonalityTests",  "[B", "toBuffer", jctest)
  p<-RProtoBuf::read(sa.CombinedSeasonalityTest, q)
  return (list(
    seasonality=rjd3toolkit::enum_extract(sa.IdentifiableSeasonality, p$seasonality),
    kruskalwallis=rjd3toolkit::p2r_test(p$kruskal_wallis),
    stable=p2r_anova(p$stable_seasonality),
    evolutive=p2r_anova(p$evolutive_seasonality)))
}

#' Title
#'
#' @param data
#' @param p0 Initial periodicity (included)
#' @param p1 Final periodicity (included)
#' @param np Number of periodicities equally spaced in [p0,p1]
#' @param original True for original algorithm, False for solution proposed by T. Proietti (based on Ox code)
#'
#' @return
#' @export
#'
#' @examples
seasonality.canovahansen<-function(data, p0, p1, np, original=FALSE){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "[D", "canovaHansenTest",
                as.numeric(data), as.numeric(p0), as.numeric(p1), as.integer(np), as.logical(original))
  return (jtest)
}

