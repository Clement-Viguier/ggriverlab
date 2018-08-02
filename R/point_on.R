#' Point on
#'
#' Find the coordinates of a point to a flight linear distance from the first point of the path.
#'
#' @param d distance to the first point.
#' @param x vector of the x-position of the points along the path.
#' @param y vector of the y-position of the points along the path.
#'
#' @return
#' @export
#'
#' @examples
#'
#' x <- sort(floor(runif(10)*100)/20)
#' y <- sort(floor(runif(10)*100)/20)
#' d <- mean(x)
#' point <- point_on(d, x, y)
#' ggplot(data.frame(x,y), aes(x,y)) + geom_path() + geom_point(data= data.frame(x = point[1], y = point[2]))

point_on <- function(d, x, y){

  df <- data.frame(x,y)
  df2 <- df %>% mutate(xn = lag(x, 1),
                      yn = lag(y, 1),
                      l = sqrt((xn-x)^2 + (yn-y)^2)) %>%
    filter(!is.na(l)) %>%
    mutate(d1 = sqrt((x-df$x[1])^2 + (y-df$y[1])^2),
           d0 = sqrt((xn-df$x[1])^2 + (yn-df$y[1])^2),
           # d0 = ifelse(is.na(d0), 0, d0),
           cross = (d1 >= d) & (d0 < d))

  if (all(!df2$cross)){
    return(numeric(0))
  }
  s <- df2[df2$cross,][1,]

  # dx = point2.X - point1.X;
  dx <- s$x - s$xn
  # dy = point2.Y - point1.Y;
  dy <- s$y - s$yn
  #
  A <- dx^2 + dy^2
  B <- 2 * (dx * (s$xn - df$x[1]) + dy * (s$yn - df$y[1]))
  C <- (s$xn - df$x[1])^2 + (s$yn - df$y[1])^2 - d^2

  det <- B^2 - 4 * A * C
  if ((A <= 0.0000001) || (det < 0)){
  # No real solutions.
    return(numeric(0))
  } else if (det == 0) {
   # One solution.
  t = - B / (2 * A)
    point <- c(s$xn + t * dx, s$yn + t * dy)
  }  else  {
  # Two solutions.
    t = (-B + sqrt(det)) / (2 * A)
    point <- c(s$xn + t * dx, s$yn + t * dy)
    # t = (-B - sqrt(det)) / (2 * A)
    # point2 <- c(s$xn + t * dx, s$yn + t * dy)
  }
  print(point)
}

