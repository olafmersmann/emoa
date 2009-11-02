##
## selection.r - Selection schemes for EAs
##
## All selection methods should have the same signature:
##
##   selection(values, n, ...)
##

##' Selection strategies for EMOA.
##'
##' The currently implemented strategies are nondominated sorting
##' followed by either hypervolume contribution or crowding distance
##' based ranking. Both of these implementations are currently
##' limited to selecting a single individual for replacement.
##'
##' @param values Matrix of function values.
##' @param n      Number of individuals to select for replacement.
##' @param ...    optimal parameters.
##' 
##' @title Selection strategies
##' @aliases nds_hv_selection nds_cd_selection
##'
##' @author Olaf Mersmann \email{olafm@@statistik.tu-dortmund.de}
##' @keywords optimize nonlinear
##' @export
nds_hv_selection <- function(values, n=1, ...) {
  #stopifnot(n == 1)
  ranks <- nds_rank(values)
  bad <- max(ranks)
  sel  <- which(ranks == bad)
  ## Identify individual which gets replaced:
  if (length(sel) == 1) {
    return(sel)
  } else {
    contrib <- hypervolume_contribution(values[,ranks==bad])
    return(sel[which.min(contrib)])
  }
}

##' @export
##' @rdname nds_hv_selection
nds_cd_selection <- function(values, n=1, ...) {
  #stopifnot(n == 1)
  ranks <- nds_rank(values)
  bad <- max(ranks)
  sel  <- which(ranks == bad)
  ## Identify individual which gets replaced:
  if (length(sel) == 1) {
    return(sel)
  } else {
    dist <- crowding_distance(values[,ranks==bad])
    return(sel[which.min(dist)])
  }
}
