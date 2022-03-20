#' @import rjd3toolkit
#' @import RProtoBuf
NULL

p2r_component<-function(p){
  s<-p$data$values
  n<-length(s)
  if (n == 0) return (NULL)
  freq<-p$data$annual_frequency
  start<-c(p$data$start_year, p$data$start_period)
  nb<-p$nbcasts
  nf<-p$nfcasts

  val<-ts(s[(nb+1):(n-nf)], frequency = freq, start=ts_move(start, freq, nb))
  rslt<-list(data=val)
  if (nb > 0){
    bcasts<-ts(s[1:nb], frequency = freq, start=start)
    rslt[['bcasts']]<-bcasts
  }
  if (nf > 0){
    fcasts<-ts(s[(n-nf+1):n], frequency = freq, start=ts_move(start, freq, n-nf))
    rslt[['fcasts']]<-fcasts
  }
  return (rslt)
}

p2r_sa_component<-function(p){
  e<-p$stde
  if (length(e) == 0) return (p2r_component(p))

  s<-p$data$values
  n<-length(s)
  if (n == 0) return (NULL)
  freq<-p$data$annual_frequency
  start<-c(p$data$start_year, p$data$start_period)
  nb<-p$nbcasts
  nf<-p$nfcasts
  dstart<-ts_move(start, freq, nb)
  fstart<-ts_move(start, freq, n-nf)

  idx<-(nb+1):(n-nf)
  data<-ts(s[idx], frequency = freq, dstart)
  edata<-ts(e[idx], frequency = freq, dstart)

  rslt<-list(data=data, data.stde=edata)
  if (nb > 0){
    idx<-1:nb
    bcasts<-ts(s[idx], frequency = freq, start=start)
    ebcasts<-ts(e[idx], frequency = freq, start=start)
    rslt[['bcasts']]<-bcasts
    rslt[['bcasts.stde']]<-ebcasts
  }
  if (nf > 0){
    idx<-(n-nf+1):n
    fcasts<-ts(s[idx], frequency = freq, start=fstart)
    efcasts<-ts(e[idx], frequency = freq, start=fstart)
    rslt[['fcasts']]<-fcasts
    rslt[['fcasts.stde']]<-efcasts
  }

  return (rslt)
}

p2r_sa_decomposition<-function(p, full=F){
  if (full){
    return (list(mode = rjd3toolkit::enum_extract(sa.DecompositionMode, p$mode),
                 series=p2r_sa_component(p$series),
                 sa=p2r_sa_component(p$seasonally_adjusted),
                 t=p2r_sa_component(p$trend),
                 s=p2r_sa_component(p$seasonal),
                 i=p2r_sa_component(p$irregular)
    ))
  }else{
    return (list(mode = rjd3toolkit::enum_extract(sa.DecompositionMode, p$mode),
                 series=p2r_component(p$series),
                 sa=p2r_component(p$seasonally_adjusted),
                 t=p2r_component(p$trend),
                 s=p2r_component(p$seasonal),
                 i=p2r_component(p$irregular)
    ))
  }
}

p2r_sa_diagnostics<-function(p){
  return (list(vardecomposition =p$variance_decomposition$as.list(),
               seas.ftest.i=rjd3toolkit::p2r_test(p$seasonal_ftest_on_irregular),
               seas.ftest.sa=rjd3toolkit::p2r_test(p$seasonal_ftest_on_sa),
               seas.qstest.i=rjd3toolkit::p2r_test(p$seasonal_qtest_on_irregular),
               seas.qstest.sa=rjd3toolkit::p2r_test(p$seasonal_qtest_on_sa),
               td.ftest.i=rjd3toolkit::p2r_test(p$td_ftest_on_irregular),
               td.ftest.sa=rjd3toolkit::p2r_test(p$td_ftest_on_sa)
  ))

}


ts_move<-function(period, freq, delta){
  if (delta == 0)return (period)
  if (freq == 1)return (c(period[1]+delta, 1))
  x<-period[1]*freq+(period[2]+delta-1)
  return (c(x %/% freq, (x %% freq)+1))
}

# Benchmarking

p2r_spec_benchmarking<-function(p){
  return (list(
    enabled=p$enabled,
    target=rjd3toolkit::enum_extract(sa.BenchmarkingTarget, p$target),
    lambda=p$lambda,
    rho=p$rho,
    bias=rjd3toolkit::enum_extract(sa.BenchmarkingBias, p$bias),
    forecast=p$forecast
  ))
}

r2p_spec_benchmarking<-function(r){
  p<-sa.BenchmarkingSpec$new()
  p$enabled<-r$enabled
  p$target<-rjd3toolkit::enum_of(sa.BenchmarkingTarget, r$target, "BENCH")
  p$lambda<-r$lambda
  p$rho<-r$rho
  p$bias<-rjd3toolkit::enum_of(sa.BenchmarkingBias, r$bias, "BENCH")
  p$forecast<-r$forecast
  return (p)
}
