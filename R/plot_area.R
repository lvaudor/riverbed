#' A function to plot the area between two profiles
#'
#' This function takes as input the result of function area_between()
#' @param area_data the result of function area_between() or area_uncertainty()
#' @param distinguish_type whether or not to distinguish between upper area and lower area
#' @param show_uncertainty whether or not to show the sources of error
#' @return a plot showing the area difference between series
#' @export
#' @examples
#' l1=c(1,3,5,6,9)
#' z1=c(1,2,3,2.5,5)
#' l2=c(0.5,2.5,4,6,8)
#' z2=c(3,1,2,4,3)
#' result_area <- area_between(l1,z1,l2,z1)
#' plot_area(result_area)
plot_area=function(result_area,
                   distinguish_type=TRUE,
                   show_uncertainty=FALSE){
  dat=result_area$data
  p <- ggplot(filter(dat,!is.na(series)),
              aes(x=l_obs,y=z_obs,color=series))+
    geom_point()+
    geom_line()
  if(distinguish_type){
    p <- p +
      geom_ribbon(data=filter(dat,!is.na(type)),
                  aes(x=l,
                      ymin=z1,
                      ymax=z2,
                      fill=type),
                  color=NA,
                  alpha=0.5)
  }else{
    p <- p +
      geom_ribbon(aes(x=l,
                      ymin=z1,
                      ymax=z2),
                  color=NA,
                  alpha=0.5)
  }
  if(show_uncertainty){
    p <- p +
      geom_linerange(aes(x=l,ymin=z1min,ymax=z1max))+
      geom_linerange(aes(x=l,ymin=z2min,ymax=z2max))+
      geom_errorbarh(aes(x=l_obs,y=z_obs,xmin=lmin,xmax=lmax),height=0.01)
  }
  return(p)
}
