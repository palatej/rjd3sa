#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom stats frequency is.ts pf ts ts.union
#' @import rjd3modelling
NULL

.onLoad <- function(libname, pkgname) {
  if (! requireNamespace("rjd3modelling", quietly = T)) stop("Loading rjd3 libraries failed")

  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  proto.dir <- system.file("proto", package = pkgname)
  RProtoBuf::readProtoFiles2(protoPath = proto.dir)

}

