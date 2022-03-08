#' @importFrom stats pf frequency
NULL


p2r_anova<-function(p){
  return (list(SSM=p$SSM, dfM=p$dfm, SSR=p$SSR, dfR=p$dfr, test=test_anova(p$SSM, p$dfm, p$SSR, p$dfr)))
}

test_anova<-function(ssm, dfm, ssr, dfr){
  val<-(ssm/dfm)*(dfr/ssr)
  desc=paste0("F(",dfm,",",dfr,")")
  pval<-1-pf(val, dfm, dfr)
  return (statisticaltest(val, pval, desc))
}

