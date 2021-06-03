p2r_anova<-function(p){
  return (list(SSM=p$SSM, dfM=p$dfm, SSR=p$SSR, dfR=p$dfr, test=test_anova(p$SSM, p$dfm, p$SSR, p$dfr)))
}

test_anova<-function(ssm, dfm, ssr, dfr){
  val<-(ssm/dfm)*(dfr/ssr)
  desc=paste0("F(",dfm,",",dfr,")")
  pval<-1-pf(val, dfm, dfr)
  return (statisticaltest(val, pval, desc))
}

ts_r2jd<-function(s){
  if (is.null(s)){
    return (NULL)
  }
  freq<-frequency(s)
  start<-start(s)
  .jcall("demetra/timeseries/r/TsUtility", "Ldemetra/timeseries/TsData;", "of",
         as.integer(freq), as.integer(start[1]), as.integer(start[2]), as.double(s))
}
