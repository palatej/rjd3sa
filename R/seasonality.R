#' @include utils.R
#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
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
#' @return
#' @export
#'
#' @examples
#' seasonality.qs(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' seasonality.qs(rjd3toolkit::randomsT(2, 1000), 7)
seasonality.qs<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "qsTest",
         as.numeric(data), as.integer(period), as.integer(nyears))
  return (rjd3toolkit:::jd2r_test(jtest))
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
  return (rjd3toolkit:::jd2r_test(jtest))
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
  return (rjd3toolkit:::jd2r_test(jtest))
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
  return (rjd3toolkit:::jd2r_test(jtest))
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
  return (rjd3toolkit:::jd2r_test(jtest))
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
    seasonality=rjd3toolkit:::enum_extract(sa.IdentifiableSeasonality, p$seasonality),
    kruskalwallis=rjd3toolkit:::p2r_test(p$kruskal_wallis),
    stable=p2r_anova(p$stable_seasonality),
    evolutive=p2r_anova(p$evolutive_seasonality)))
}

