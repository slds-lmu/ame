#' @import BBmisc
#' @import backports
#' @import checkmate
#' @import data.table
#' @importFrom stats predict setNames

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
}
