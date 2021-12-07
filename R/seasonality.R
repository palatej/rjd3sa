#' @include utils.R
NULL

#' Title
#'
#' @param data
#' @param period
#' @param nyears
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

#' Title
#'
#' @param data
#' @param period
#' @param nyears
#'
#' @return
#' @export
#'
#' @examples
seasonality.kruskalwallis<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "kruskalWallisTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (jd2r_test(jtest))
}

#' Title
#'
#' @param data
#' @param period
#' @param nyears
#'
#' @return
#' @export
#'
#' @examples
seasonality.periodogram<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "periodogramTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (jd2r_test(jtest))
}

#' Title
#'
#' @param data
#' @param period
#' @param nyears
#'
#' @return
#' @export
#'
#' @examples
seasonality.friedman<-function(data, period, nyears=0){
  jtest<-.jcall("demetra/sa/r/SeasonalityTests", "Ldemetra/stats/StatisticalTest;", "friedmanTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (jd2r_test(jtest))
}

#' Title
#'
#' @param data
#' @param period
#' @param model
#' @param nyears
#'
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

#' Title
#'
#' @param data
#' @param period
#' @param firstperiod
#' @param mul
#'
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


