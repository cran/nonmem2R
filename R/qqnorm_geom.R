####################################################
#' ggproto for stat_QQnorm
#'
#' @description
#' ggproto for stat_QQnorm doing Quantile-Quantile Plots with ggplot2
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
#' #' If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend
#' logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes
#' If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ...
#' other arguments passed on to layer.
#' @details
#' internal package function
#' @export
#' @keywords internal
StatQQnorm <- ggproto("StatQQnorm", Stat,
                      compute_group = function(data, scales) {
                        p<-sort(data$x[!is.na(data$x)])
                        n<-length(p)
                        data.frame(x=qnorm(seq(0.5/n,1-0.5/n,length=n)),y=p)
                      },
                      required_aes = c("x")
)

####################################################
#' Quantile-Quantile Plots
#'
#' @description
#' Quantile-Quantile Plots with ggplot2
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
#' @examples
#' dd<-data.frame(gr=c(rep("A",20),rep("B",40)))
#' dd$dv<-2*(dd$gr=="B")+rnorm(nrow(dd))/(1+2*(dd$gr=="A"))
#' dd<-dd[order(dd$gr,dd$dv),]
#' dd$px<-NA
#' for(gri in levels(dd$gr)){
#'   dd$px[dd$gr==gri]<-qqnorm(dd$dv[dd$gr==gri],plot=FALSE)$x
#' }

#' ggplot(dd, aes(dv)) +
#'   stat_QQnorm()+
#'   facet_wrap(~gr)
#' \dontrun{
#' ggplot(dd, aes(dv,color=factor(gr))) +
#'   stat_QQnorm()
#'   }
stat_QQnorm <- function(mapping = NULL, data = NULL, geom = "point",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatQQnorm, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
####################################################
#' ggproto for stat_QQVPC
#'
#' @description
#' ggproto for stat_QQVPC doing VPC for Quantile-Quantile Plots with ggplot2
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
#' #' If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend
#' logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes
#' If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ...
#' other arguments passed on to layer.
#' @details
#' internal package function
#' @export
#' @keywords internal
StatQQVPC <- ggproto("StatQQVPC", Stat,
                    compute_group = function(data, scales,confidence.level) {
                      nsim<-10000
                      alpha<-1-confidence.level
                      xnona<-data$x[!is.na(data$x)]
                      n<-length(xnona)
                      px<-qnorm(seq(0.5/n,1-0.5/n,length=n))
                      m<-mean(xnona)
                      s<-sd(xnona)

                      x1<-matrix(rnorm(n*nsim),nsim,n)
                      x2<-scale(apply(x1,1,sort))
                      x3<-apply(x2,1,quantile,c(alpha/2,1-alpha/2))
                      data.frame(x=px,ymin=x3[1,]*s+m,ymax=x3[2,]*s+m)
                    },
                    required_aes = c("x")
)

## ggplot(dd, aes(dv,color=factor(gr))) + stat_QQVPC(alpha=0.25,confidence.level=0.5) + stat_QQnorm()
####################################################
#' Add VPC confidence interval for Quantile-Quantile Plots
#'
#' @description
#' Add Visual predictive check confidence interval for Quantile-Quantile Plots with ggplot2.
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
#' @param confidence.level
#' Confidence level for confidence intervals, default=0.95
#' @param ...
#' other arguments passed on to layer.
#' @details
#' Provide Visual predictive check confidence interval for normal QQplot under the assumption of normal distribution.
#' With confidence.level=0.95, by chance one 1 of 20 data points may fall outside the confidence interval
#' when data truly are normal distributed. Confidence intervals are based on repeated simulation of normal
#' distributed data with mean and sd equal to the mean and sd of input data and confidence limits
#' are defined by the quantiles of simulated data.
#'
#' @section Aesthetics:
#' geom_smooth understands the following aesthetics (required aesthetics are in bold):\cr
#' \bold{x}, alpha, colour, fill, group, shape, size, stroke
#'
#' @export
#' @importFrom ggplot2 layer
#' @examples
#' dd<-data.frame(gr=c(rep("A",20),rep("B",40)))
#' dd$dv<-2*(dd$gr=="B")+rnorm(nrow(dd))/(1+2*(dd$gr=="A"))
#' dd<-dd[order(dd$gr,dd$dv),]
#' dd$px<-NA
#' for(gri in levels(dd$gr)){
#'   dd$px[dd$gr==gri]<-qqnorm(dd$dv[dd$gr==gri],plot=FALSE)$x
#' }

#' ggplot(dd, aes(dv)) +
#'   stat_QQVPC(alpha=0.25)+
#'   stat_QQnorm()+
#'   facet_wrap(~gr)
#' \dontrun{
#' ggplot(dd, aes(dv,color=factor(gr))) +
#'   stat_QQVPC(alpha=0.25) +
#'   stat_QQnorm()
#' }
stat_QQVPC <- function(mapping = NULL, data = NULL, geom = "ribbon",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, confidence.level=0.95,   ...) {
  layer(
    stat = StatQQVPC, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(confidence.level=confidence.level,na.rm = na.rm, ...)
  )
}

#dd<-data.frame(gr=c(rep("A",20),rep("B",40)))
#dd$dv<-2*(dd$gr=="B")+rnorm(nrow(dd))/(1+2*(dd$gr=="A"))
#dd<-dd[order(dd$gr,dd$dv),]
#dd$px<-NA
#for(gri in levels(dd$gr)){
#  dd$px[dd$gr==gri]<-qqnorm(dd$dv[dd$gr==gri],plot=FALSE)$x
#}

#ggplot(dd, aes(dv)) +
#  stat_QQVPC(alpha=0.5)+
#  stat_QQnorm()+
#  facet_wrap(~gr)

#ggplot(dd, aes(dv,color=factor(gr))) +
#  stat_QQVPC()+
#  stat_QQnorm()

