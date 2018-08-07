#' Text
#'
#'
#' @eval rd_aesthetics("geom", "text")
#'
#' @section Alignment:
#' You can modify text alignment with the `vjust` and `hjust`
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character (`"left"`, `"middle"`, `"right"`, `"bottom"`,
#' `"center"`, `"top"`). There are two special alignments: `"inward"` and
#' `"outward"`. Inward always aligns text towards the center, and outward
#' aligns it away from the center.
#'
#'
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @import ggplot2 grid dplyr
#' @importFrom ggplot2 ggproto
#' @importFrom signal sgolayfilt
#' @importFrom plyr ddply
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @param relative if TRUE (default) the positioning \code{aes} such as offset, dist, wind and vpos is computed relative to the length of the longest path. The last three are relative to the longest path, for consistency reasons.
#' @param reverse if TRUE the label is writen in the opposite direction. By default the direction is defined by the sign of the x coordinates of the begining and end of the path.
#'   If the path ends on the left relative to the start, the labeling is reversed.
#' @param repeated if TRUE the label is repeated along the path as often as possible. Each label is separated by a distance of \code{offset}.
#' @param smoothing if TRUE(default)  smooth the path before computing the letter positioning. Recommended to avoid weird kerning.
#' @param sg_order Savitzky-Golay smoothing filter order. See \code{?sgolayfilt} for more details.
#' @param sg_length Savitzky-Golay smoothing filter length. Must be odd. See \code{?sgolayfilt} for more details.
#' @param check_length if TRUE (default) the label is pletted only if there is enough space.
#'
#' @details Because the path of a river can be tortuous, the distances are linear "flight" distances rather than distances following the path.
#' However if  \code{relative = TRUE} the computed distance are computed relatively to the length of the path group, therefore even a distance lower than 0.5 can place the label at the end of the path.
#' For consistency reason if not repeated == FALSE the offset is relative to the path group length (consistency in the position of the unique labels), else it is relative to the maximum path group length (consistency in the distance between labels).
#'
#' @export
#' @examples
#'
geom_river_label <- function(mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             ...,
                             parse = FALSE,
                             nudge_x = 0,
                             nudge_y = 0,
                             na.rm = FALSE,
                             centred = TRUE,
                             relative = TRUE,
                             reverse = NA,
                             repeated = FALSE,
                             smoothing = TRUE,
                             sg_order = 2,
                             sg_length = 51,
                             check_length = TRUE,
                             show.legend = FALSE,
                             inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRiverLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      centred = centred,
      relative = relative,
      reverse = reverse,
      repeated = repeated,
      smoothing = smoothing,
      sg_order = sg_order,
      sg_length = sg_length,
      check_length = check_length,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRiverLabel <- ggproto("GeomRiverLabel", Geom,
                          required_aes = c("x", "y", "label"),

                          default_aes = aes(
                            colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                            vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                            offset = 0.15, dist = 0.04, vpos = 0.03, win = 0.1
                          ),

                          draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                na.rm = FALSE, centred = TRUE, relative = TRUE, reverse = NA, repeated = TRUE,
                                                smoothing = TRUE, sg_order = 2, sg_length = 51, check_length = TRUE) {



                            # Compute by default aes (offset, dist, win) based on the length of the paths #######################################


                            # adjust the positioning aes to the path length
                            if (relative){
                              df2 <- data %>% group_by(group) %>%
                                mutate(xn = lag(x, 1),
                                                   yn = lag(y, 1),
                                                   l = sqrt((xn-x)^2 + (yn-y)^2)) %>%
                                filter(!is.na(l)) %>%
                                summarise(length = sum(l))

                              max_length <- max(df2$length)
                              # print(path_length)
                              data <- data %>% left_join(df2, by = "group") %>%
                                mutate(
                                dist = dist * max_length,
                                win = win * max_length,
                                vpos = vpos * max_length,
                                offset = (offset * length * (1 - repeated)) + (offset * max_length * repeated))
                              # If not repeated, the offset is relative to the path group length, else it is relative to the maximum path group length
                            }


                            # Compute the labels per group:
                            data <- plyr::ddply(data, "group", function(df, centred, reverse, relative, smoothing, sg_orger, sg_length, repeated, check_length) {

                                # Smoothing:
                              if (smoothing & length(df$x) > 3){
                                df$x <- sgolayfilt(df$x, sg_order, floor(min(sg_length, length(df$x) - 1)/2) * 2 + 1 )
                                df$y <- sgolayfilt(df$y, sg_order, floor(min(sg_length, length(df$x) - 1)/2) * 2 + 1 )
                                # df$x <- sgolayfilt(df$x, sg_order, sg_length )
                                # df$y <- sgolayfilt(df$y, sg_order, sg_length)
                              }

                              # Change the direction of the path to keep the label on top if not decided
                              if (is.na(reverse)){
                                l <- length(df$x)
                                reverse <- df$x[l] - df$x[1] < 0
                              }

                              if (reverse){
                                df <- df[dim(df)[1]:1,]
                              }

                              # COmpute the position of each letter
                              data2 <- place_along(text = df$label[1], df$x, df$y,
                                                   offset = df$offset[1], dist = df$dist[1], vpos = df$vpos[1], win =df$win[1],
                                                   centred = centred, repeated = repeated, check_length = check_length)
                              data2$angle2 <- data2$angle * 180 / pi # convert to degrees
                              # merge with original data to keep the aes
                              # print(data2)
                              data2$group <- df$group[min(1, length(data2$x))]
                              data2 <- inner_join(data2, select(df[1,], -x, -y, -label), by = "group")


                              return(data2)
                            },
                            centred, reverse, relative, smoothing, sg_order, sg_length, repeated, check_length
                            )

                            # print(data)
                            if (dim(data)[1] < 1){
                              warning("No label to print. Plese adjust the aes (they might be too big).")
                            }

                            # Apply the rest of the GeomText as normal
                            lab <- data$label
                            if (parse) {
                              lab <- parse(text = as.character(lab))
                            }

                            data <- coord$transform(data, panel_params)
                            if (is.character(data$vjust)) {
                              data$vjust <- compute_just(data$vjust, data$y)
                            }
                            if (is.character(data$hjust)) {
                              data$hjust <- compute_just(data$hjust, data$x)
                            }

                            textGrob(
                              lab,
                              data$x, data$y, default.units = "native",
                              hjust = data$hjust, vjust = data$vjust,
                              rot = data$angle + data$angle2,
                              gp = gpar(
                                col = alpha(data$colour, data$alpha),
                                fontsize = data$size * .pt,
                                fontfamily = data$family,
                                fontface = data$fontface,
                                lineheight = data$lineheight
                              )
                            )
                          },

                          draw_key = draw_key_text
)


compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
