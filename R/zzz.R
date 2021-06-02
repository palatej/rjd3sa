#' @importFrom rjd3toolkit .JD3_ENV
NULL

enum_extract<-NULL
enum_of<-NULL
p2r_test<-NULL
jd2r_test<-NULL

.onLoad <- function(libname, pkgname) {
  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  proto.dir <- system.file("proto", package = pkgname)
  readProtoFiles2(protoPath = proto.dir)

  .JD3_ENV$p2r_sa_diagnostics<-p2r_sa_diagnostics
  .JD3_ENV$p2r_sa_component<-p2r_sa_component
  .JD3_ENV$p2r_component<-p2r_component
  .JD3_ENV$p2r_sa_decomposition<-p2r_sa_decomposition

  p2r_test<<-.JD3_ENV$p2r_test
  enum_extract<<-.JD3_ENV$enum_extract
  enum_of<<-.JD3_ENV$enum_of
  jd2r_test<<-.JD3_ENV$jd2r_test
}

