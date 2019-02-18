####################################################
#' ggproto for stat_dnorm
#' @description
#' ggproto for stat_dnorm doing normal density to histograms
#' @export
#' @keywords internal
StatDnorm <- ggproto("StatDnorm", Stat,
                      compute_group = function(data, scales) {
                        p<-(data$x[!is.na(data$x)])
                        mx<-mean(p)
						sx<-sd(p)
						xx<-seq(min(c(mx-3*sx,p)),max(c(mx+3*sx,p)),length=50)
						data.frame(x=xx,y=dnorm(xx,mean=mx,sd=sx))
                      },
                      required_aes = c("x")
)

####################################################
#' Normal density
#'
#' @description
#' Normal density with ggplot2
#'
#' @param mapping
#' Set of aesthetic mappings created by aes or aes_.
#' @param data
#' The data to be displayed in this layer.
#' @param geom
#' Use to override the default geom
#' @param position
#' Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm
#' If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend
#' logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes
#' If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ...
#' other arguments passed on to layer.
#' @details
#' Provide normal QQplot
#' @export
#' @importFrom ggplot2 layer
stat_dnorm <- function(mapping = NULL, data = NULL, geom = "line",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatDnorm, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

########
####################################################
#' ggproto for stat_hmedian
#' @description
#' ggproto for stat_hmedian doing horizontal reference line at median
#' @export
#' @keywords internal
StatHmedian <- ggproto("StatHmedian", Stat,
                      compute_group = function(data, scales) {
                        p<-median(data$y[!is.na(data$y)])
						data.frame(yintercept=p)
                      },
                      required_aes = c("y")
)

####################################################
#' Horizontal reference at median
#'
#' @description
#' Horizontal reference at median with ggplot2
#'
#' @param mapping
#' Set of aesthetic mappings created by aes or aes_.
#' @param data
#' The data to be displayed in this layer.
#' @param geom
#' Use to override the default geom
#' @param position
#' Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm
#' If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend
#' logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes
#' If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ...
#' other arguments passed on to layer.
#' @details
#' Provide normal QQplot
#' @section Aesthetics:
#' geom_smooth understands the following aesthetics (required aesthetics are in bold):\cr
#' \bold{x}, alpha, colour, fill, group, shape, size, stroke
#'
#' @export
#' @importFrom ggplot2 layer
stat_hmedian <- function(mapping = NULL, data = NULL, geom = "hline",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatHmedian, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
########
####################################################
#' ggproto for stat_hmean
#'
#' @description
#' ggproto for stat_hmean doing horizontal reference line at mean
#' @export
#' @keywords internal
StatHmean <- ggproto("StatHmean",
                     Stat,
                     compute_group = function(data, scales) {
                          p<-mean(data$y[!is.na(data$y)])
						              data.frame(yintercept=p)
                     },
                     required_aes = "y"
)

####################################################
#' Horizontal reference at mean
#'
#' @description
#' Horizontal reference at mean with ggplot2
#'
#' @param mapping
#' Set of aesthetic mappings created by aes or aes_.
#' @param data
#' The data to be displayed in this layer.
#' @param geom
#' Use to override the default geom
#' @param position
#' Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm
#' If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend
#' logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes
#' If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ...
#' other arguments passed on to layer.
#' @details
#' Provide normal QQplot
#' @export
#' @importFrom ggplot2 layer
stat_hmean <- function(mapping = NULL, data = NULL, geom = "hline",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatHmean, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

####################################################
#' ggproto for position_scale
#' @description
#' ggproto for position_scale
#' @export
#' @importFrom ggplot2 Position
#' @keywords internal
PositionScale <- ggproto("PositionScale",
                    PositionDodge,
                    required_aes = "y",

                    setup_params = function(self, data) {
                      list(y = self$y)
                    },

                    #setup_data = function(self, data, params) {
                    #  check_required_aesthetics(self$required_aes, names(data), snake_class(self))
                    #  data
                    #},

                    compute_layer = function(data, params, panel) {
                      transform_position(data, NULL,function(y) y*params$y)
                    },

                    compute_panel = function(self, data, params, scales) {
                      stop("Not implemented", call. = FALSE)
                    }
)
####################################################
#' Position for scaling y
#' @description
#' Position for scaling y
#' @param y
#' scale value
#' @details
#' Position for scaling y
#' @export
#' @importFrom ggplot2 ggproto
position_scale <- function(y = 1) {
  ggproto(NULL, PositionScale, y = y)
}
######################### scale end



####################################################
#' ggproto for stat_corr
#'
#' @description
#' ggproto for stat_corr adding correlation to x-y plots
#' @export
#' @keywords internal
StatCorr <- ggproto("StatCorr", Stat,
                    compute_group = function(data, scales) {
                      x<-(data$x[!is.na(data$x)])
                      y<-(data$y[!is.na(data$y)])
                      mx<-mean(scales$x$range$range)
                      my<-mean(scales$y$range$range)
                      label<-paste("Corr.",sprintf("%#6.3g",cor(x,y)))
                      data.frame(x=mx,y=my,label=label)
                    },
                    required_aes = c("x","y")
)

####################################################
#' Adding correlation to x-y plots
#'
#' @description
#' Correlation to x-y plots. This stat is indended for providing correlations e.g. above the
#' diagonal of a pairs plot
#'
#' @param mapping
#' Set of aesthetic mappings created by aes or aes_.
#' @param data
#' The data to be displayed in this layer.
#' @param geom
#' Use to override the default geom
#' @param position
#' Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm
#' If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend
#' logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes
#' If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ...
#' other arguments passed on to layer.
#' @details
#' Provide normal QQplot
#' @export
#' @importFrom ggplot2 layer
stat_corr <- function(mapping = NULL, data = NULL, geom = "text",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatCorr, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

####################################################
#' ggproto for stat_denx
#'
#' @description
#' ggproto for stat_denx adding density for x with y-scaled to fit current scale
#' @export
#' @keywords internal
StatDenx <- ggproto("StatDenx", Stat,
                    compute_group = function(data, scales) {
                      x<-(data$x[!is.na(data$x)])
                      rx<-range(x)
                      dd<-density(x,kernel = "gaussian", n = 512,from=rx[1],to=rx[2])
                      yy<-dd$y
                      xx<-dd$x
                      sy<-scales$y$range$range
                      yy<-yy/max(yy)*diff(sy)+sy[1]
                      data.frame(x=c(xx,rx[2:1]),y=c(yy,sy[1],sy[1]))
                    },
                    required_aes = c("x","y")
)

####################################################
#' Density for x in x-y plot
#'
#' @description
#' Density for x in x-y plot. This stat is indended for providing the a density on the
#' diagonal of a pairs plot
#'
#' @param mapping
#' Set of aesthetic mappings created by aes or aes_.
#' @param data
#' The data to be displayed in this layer.
#' @param geom
#' Use to override the default geom
#' @param position
#' Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm
#' If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend
#' logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes
#' If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ...
#' other arguments passed on to layer.
#' @details
#' Provide normal QQplot
#' @export
#' @importFrom ggplot2 layer
stat_denx <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatDenx, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
############

