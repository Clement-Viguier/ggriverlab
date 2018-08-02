#' Place along
#'
#' Place letter s along a path.
#'
#' @param label word to place along the path.
#' @param x X coordinates of the path.
#' @param y y coordinates of the path.
#' @param offset distance from the starting point of the path.
#' @param dist distance between letters.
#' @param vpos Relative distance from the path (negative value put the label under the path).
#' @param win width of the window to smooth the slope.
#' @param centred is the label position centred on the path or adjusted to the window (default = T).
#'
#' @return
#' @export
#'
#' @examples
#' y <- sort(floor(runif(50)*300)/20)
#' x <- sort(floor(runif(50)*200)/20)
#' win <- 6
#' pos <- place_along("abcefg", x, y, 4, 1.5, 1.5, win =  win, centred = 1)
#' ggplot(data.frame(x, y), aes(x,y))+ geom_path() + geom_text(data=pos, aes(label = letter, angle= angle * 180 / pi), size = 14) + geom_point(data = pos, aes(refx, refy)) + coord_fixed() #+ geom_arc(data = pos, aes(refx, refy), radius = win/2, width = 15 * 2 * pi)
#'
#'
place_along <- function(label, x, y, offset, dist = 0.5, vpos = 0.2, win = 1, centred = T){
  n <- nchar(label[1])

  df <- data.frame(x, y)

  # compute segments
  segs <- df %>% mutate(xn = lag(x, 1),
                        yn = lag(y, 1),
                        l = sqrt((xn-x)^2 + (yn-y)^2)) %>%
    filter(!is.na(l)) %>%
    mutate(seg = 1:length(l))

  print(segs)
  # print(typeof(segs))

    # define ref point
  ref <- point_on(offset, x, y)

  positions <- data.frame(letter = unlist(strsplit(label, "")),
                          x = numeric(n),
                          y = numeric(n),
                          angle = numeric(n),
                          id = 1:n,
                          refx = numeric(n),
                          refy = numeric(n))

  for (i in 1:n){
    print(ref)

    # find segment corresponding to ref point
    print(mutate(segs, t1 =(((xn <= ref[1]) & (x >= ref[1])) | ((xn >= ref[1]) & (x <= ref[1]))),
                     t2 =   (((yn <= ref[2]) & (y >= ref[2])) | ((yn >= ref[2]) & (y <= ref[2]))),
                        t3 = (floor((ref[2] - yn) / (ref[1] - xn) - (y - ref[2])/(x - ref[1]) ) < 0.01)))

    segment <- filter(segs, (((xn <= ref[1]) & (x >= ref[1])) | ((xn >= ref[1]) & (x <= ref[1]))) &
                                 (((yn <= ref[2]) & (y >= ref[2])) | ((yn >= ref[2]) & (y <= ref[2]))) #&
                                 # (floor((ref[2] - yn) / (ref[1] - xn) - (y - ref[2])/(x - ref[1]) ) < 0.01)
    )[1,]

    print(segment)

    # define upstream path

    up <- segs %>%  filter(seg <= segment$seg) %>% select(xn, yn) %>% rename(x = xn, y = yn)
    print((up))
    print((up[dim(up)[1]:1,]))
    up <- rbind(ref, up[dim(up)[1]:1,])

    # define downsteam path
    down <- segs %>%  filter(seg >= segment$seg) %>% select(x, y)
    print(head(up))
    down <- rbind(ref, down)

    # compute letter position and angle
    point <- position_letter(down$x, down$y, up$x, up$y, win = win, offset = vpos, centred = T)
    print(point)

    # save
    positions[i, c("x", "y", "angle", "refx", "refy")] <- c(point, ref)

    # define new ref point
    ref <- point_on(dist, down$x, down$y)
  }
  return(positions)
}
