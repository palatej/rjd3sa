jd2r_test<-function(jtest){
  if (is.jnull(jtest))
    return (NULL)
  else{
    desc<-.jcall(jtest, "S", "getDescription")
    val<-.jcall(jtest, "D", "getValue")
    pval<-.jcall(jtest, "D", "getPvalue")
    return (list(value=val, pvalue=pval, distribution=desc))
  }
}

p2r_test<-function(p){
  if (is.null(p))
    return (NULL)
  else{
    return (p$as.list())
  }
}

p2r_anova<-function(p){
  return (list(SSM=p$SSM, dfM=p$dfm, SSR=p$SSR, dfR=p$dfr, test=test_anova(p$SSM, p$dfm, p$SSR, p$dfr)))
}

test_anova<-function(ssm, dfm, ssr, dfr){
  val<-(ssm/dfm)*(dfr/ssr)
  desc=paste0("F(",dfm,",",dfr,")")
  pval<-1-pf(val, dfm, dfr)
  return (list(value=val, pvalue=pval, distribution=desc))
}

enum_extract<-function(type, p){
  name<-type$value(number=p)$name()
  return (substring(name, regexpr("_", name)+1))
}

enum_of<-function(type, code, prefix){
  i<-type$value(name=paste(prefix, code, sep='_'))$number()
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

