##
## selection.r - Selection schemes for EAs
##
## All selection methods should have the same signature:
##
##   selection(values, n, ...)
##

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
