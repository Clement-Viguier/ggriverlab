#' Position letter
#'
#' @param x
#' @param y
#' @param xr
#' @param yr
#' @param win
#' @param offset
#' @param centred
#'
#' @return
#' @export
#'
#' @examples
position_letter <- function(x, y, xr, yr, win = 1, offset = 0.1, centred = T){
  p1 <- point_on(win/2, x, y)
  p2 <- point_on(win/2, xr, yr)

  # Compute the angle between the two points along the path to smooth the slope of the
  angle <- pi/2 + atan2(p1[2] - p2[2], p1[1] - p2[1])

  # Place the point given by the angle, the ref point and the vertical offset
  point <- c(cos(angle) * offset, sin(angle) * offset)
  # adjust to reference point:
  if(centred){
    point <- point + c(x[1], y[1])
  } else {
    point <- point + c((p1[1] + p2[1])/2, (p1[2] + p2[2])/2)
  }
  return(c(point, angle - pi / 2))
}
