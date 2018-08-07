#' Place along
#'
#' Place letters of the given \code{text} along a path \code{(x, y)}. Used in \code{geom_river_label()}.
#'
#' @param text text to place along the path.
#' @param x X coordinates of the path.
#' @param y y coordinates of the path.
#' @param offset distance from the starting point of the path.
#' @param dist distance between letters.
#' @param vpos Relative distance from the path (negative value put the label under the path).
#' @param win width of the window to smooth the slope.
#' @param centred if TRUE (default) the label position centred on the path, otherwise it is adjusted to the window defined by the parameter \code{win}.
#' @param repeated if TRUE (default) the text is repeated along the path and each label is seperated by the offset, otherwise it is placed only once.
#'
#' @return
#' @export
#'
#' @examples
#' y <- sort(floor(runif(50)*300)/20)
#' x <- sort(floor(runif(50)*200)/20)
#' win <- 6
#' pos <- place_along("abcefg", x, y, 4, 1.5, 1.5, win =  win, centred = 1)
#' ggplot(data.frame(x, y), aes(x,y))+ geom_path() + geom_text(data=pos, aes(label = label, angle= angle2 * 180 / pi), size = 14) + geom_point(data = pos, aes(refx, refy)) + coord_fixed() #+ geom_arc(data = pos, aes(refx, refy), radius = win/2, width = 15 * 2 * pi)
#'
#'
#'# Another example with real river data from the riverdist package
#'
#' river <- Gulk$lines[[14]]
#' river <- as.data.frame(river)
#' colnames(river) <- c("x", "y")
#' river
#' win <- 1000
#' pos <- place_along("Gulk", river$x, river$y, 4000, 1000, 500, win =  win, centred = 1)
#' ggplot(river, aes(x,y))+ geom_path() + geom_text(data=pos, aes(label = letter, angle= angle * 180 / pi), size = 8) + geom_point(data = pos, aes(refx, refy)) + coord_fixed()
#'
place_along <- function(text, x, y, offset, dist = 0.5, vpos = 0.2, win = 1, centred = T, repeated = T, check_length = T){
  n <- nchar(text[1])

  df <- data.frame(x, y)

  # compute segments
  segs <- df %>% mutate(xn = lag(x, 1),
                        yn = lag(y, 1),
                        l = sqrt((xn-x)^2 + (yn-y)^2)) %>%
    filter(!is.na(l)) %>%
    mutate(seg = 1:length(l))

  # print(segs)
  # print(typeof(segs))

  # define ref point
  ref <- point_on(offset, x, y)
  if (length(ref) == 0){
    ref <- c(x[1], y[1])
    # warning("Could not find the reference point. You may want to adjust the offset.")
  }

  # List of data.frame to contain the positions:
  pos_list <- list()
  l <- 0

  continue = TRUE
  while(continue){
    continue <- repeated


    # Reset the position data.frame
    l <- l+1

    positions <- data.frame(label = unlist(strsplit(text, "")),
                            x = numeric(n),
                            y = numeric(n),
                            angle2 = numeric(n),
                            id = 1:n,
                            refx = numeric(n),
                            refy = numeric(n))

    for (i in 1:n){
      # print(ref)

      # find segment corresponding to ref point
      # print(mutate(segs, t1 =(((xn <= ref[1]) & (x >= ref[1])) | ((xn >= ref[1]) & (x <= ref[1]))),
      # t2 =   (((yn <= ref[2]) & (y >= ref[2])) | ((yn >= ref[2]) & (y <= ref[2]))),
      # t3 = (floor((ref[2] - yn) / (ref[1] - xn) - (y - ref[2])/(x - ref[1]) ) < 0.01)))

      segment <- filter(segs, (((xn <= ref[1]) & (x >= ref[1])) | ((xn >= ref[1]) & (x <= ref[1]))) &
                          (((yn <= ref[2]) & (y >= ref[2])) | ((yn >= ref[2]) & (y <= ref[2]))) #&
                        # (floor((ref[2] - yn) / (ref[1] - xn) - (y - ref[2])/(x - ref[1]) ) < 0.01)
      )[1,]

      # print(segment)

      # define upstream path

      up <- segs %>%  filter(seg <= segment$seg) %>% select(xn, yn) %>% rename(x = xn, y = yn)
      # print((up))
      # print((up[dim(up)[1]:1,]))
      up <- rbind(ref, up[dim(up)[1]:1,])

      # define downsteam path
      down <- segs %>%  filter(seg >= segment$seg) %>% select(x, y)
      # print(head(up))
      down <- rbind(ref, down)

      # compute letter position and angle
      point <- position_letter(down$x, down$y, up$x, up$y, win = win, offset = vpos, centred = T)
      # print(point)
      # print(ref)
      # print(c(point, ref))

      # save
      positions[i, c("x", "y", "angle2", "refx", "refy")] <- c(point, ref)


      # define new ref point
      if (i == n & repeated){
        # If last letter, go to next label position
        ref <- point_on(offset, down$x, down$y)
      }else {
        # else, find the next letter position
        ref <- point_on(dist, down$x, down$y)
      }

      # print(positions)
      # test what's goin on at the end of the path (cannot find the next position):
      if (length(ref) == 0){
        ref <- c(x[1], y[1])
        # warning("Could not find the reference point. You may want to adjust the offset.")


        # stop the procedure
        continue <- FALSE
      }

        # print(l)
        # print(i)
    } # end of loop over letters of the text

    # drop position if not filled
    # return empty data.frame is one letter is out of bounds
    if (any(is.na(positions$x)) & check_length){
      # return(positions[0,])
      positions <- positions[0, ]
    }

    pos_list[[l]] <- positions
  }
  # aggregate the list:
  positions <- dplyr::bind_rows(pos_list)

  # print(pos_list)

  # Still necessary ?
  positions$angle2[is.na(positions$angle2)] <- 0


  return(positions)
}
