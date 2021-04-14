#' Title
#'
#' @param y
#' @param sa
#' @param t
#' @param s
#' @param i
#' @param mul
#'
#' @return
#' @export
#'
#' @examples
sadecomposition<-function(y, sa, t, s, i, mul){
  if (! is.logical(mul))stop("Invalid SA decomposition")
  if (is.null(y))stop("Invalid SA decomposition")
  if (! is.ts(y))stop("Invalid SA decomposition")
  n=length(y)
  if (is.null(s)){
    if (mul){
      s=ts(rep(1,1,n))
    }else{
      s=ts(rep(0,1,n))
    }
  } else if (! is.ts(s))stop("Invalid SA decomposition")
  if (is.null(i)){
    if (mul){
      i=ts(rep(1,1,n))
    }else{
      i=ts(rep(0,1,n))
    }
  } else if (! is.ts(i))stop("Invalid SA decomposition")


  if (! is.ts(sa))stop("Invalid SA decomposition")
  if (! is.ts(t))stop("Invalid SA decomposition")

  return (structure(list(series=y, sa=sa, trend=t, seas=s, irr=i, multiplicative=mul), class=c("JD3SADECOMPOSITION", "JD3")))
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.JD3SADECOMPOSITION<-function(x,...){
  print(ts.union(series=x$series,sa=x$sa,trend=x$trend,seas=x$seas,irr=x$irr))
}
