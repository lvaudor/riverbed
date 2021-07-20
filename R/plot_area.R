#' A function to plot the area between two profiles
#'
#' This function takes as input the result of function area_between()
#' @param result_area the result of function area_between() or area_uncertainty()
#' @param distinguish_type whether or not to distinguish between upper area and lower area
#' @param show_uncertainty whether or not to show the sources of error
#' @return a plot showing the area difference between series
#' @export
#' @examples
#' s1=tibble::tibble(l=c(1,3,5,6,9),z=c(1,2,3,2.5,5))
#' s2=tibble::tibble(l=c(0.5,2.5,4,6,8),z=c(3,1,2,4,3))
#' result_area <- area_between(s1,s2,sigma_z=c(0.2,0.5),sigma_l=c(0.25,0.25))
#' plot_area(result_area, show_uncertainty=TRUE)
plot_area=function(result_area,
                   distinguish_type=TRUE,
                   show_uncertainty=FALSE){
  dat=result_area$data
  p <- ggplot2::ggplot(dplyr::filter(dat,!is.na(series)),
                       ggplot2::aes(x=l_obs,y=z_obs,color=series))+
    ggplot2::geom_point()+
    ggplot2::geom_line()

  if(show_uncertainty){
    p <- p +
      ggplot2::geom_linerange(data=dat,
                              ggplot2::aes(x=l,ymin=zmin,ymax=zmax))+
      ggplot2::geom_errorbarh(data=dat,
                              ggplot2::aes(x=l_obs,y=z_obs,xmin=lmin,xmax=lmax),height=0.01)
  }
  if(distinguish_type){
    dat1=dat %>%
      dplyr::filter(type=="lower") %>%
      dplyr::mutate(zmin=z1,zmax=z2)
    dat2=dat %>%
      dplyr::filter(type=="upper") %>%
      dplyr::mutate(zmin=z2,zmax=z1)
    p=p+ggplot2::geom_ribbon(data=dplyr::bind_rows(dat1,dat2),
                             ggplot2::aes(x=l,
                        ymin=zmin,
                        ymax=zmax,
                        fill=type),
                    color=NA,
                    alpha=0.5)
  }else{
    p <- p +
      ggplot2::geom_ribbon(aes(x=l,
                      ymin=z1,
                      ymax=z2),
                  color=NA,
                  alpha=0.5)
  }
  return(p)
}
