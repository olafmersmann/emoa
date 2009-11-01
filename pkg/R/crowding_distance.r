##
## crowding_distance.r - calculate crowding distance.
##

crowding_distance <- function(front)
  .Call("do_crowding_distance", front, PACKAGE="emoa")
