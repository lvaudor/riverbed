#' A function to project data on a line defined by two points A & B
#'
#' @param x x coordinate of points
#' @param y y coordinate of points
#' @return xp x coordinate of points projected on the transect
#' @return yp y coordinate of points projected on the transect
#' @return xt x coordinate along the transect (new coordinate x)
#' @return yt y coordinate corresponding to the distance to the transect (new coordinate y)
#' @export
#' @examples
#' dat=tibble(x=c(3.3,5.2,1.6,6.3),
#'            y=c(2.1,6.3,2.1,1.6))
#' datAB=tibble(x=c(0.2,4.2),
#'              y=c(2.6,6.6))
#' dat=project_on_AB(dat,datAB)
#' ggplot(dat,  aes(x=x,y=y))+
#'   geom_point(col="red")+
#'   geom_line(data=datAB, aes(x=x,y=y))+
#'   geom_point(data=datAB)+
#'   geom_point(data=dat,col="blue", aes(x=xp,y=yp))+
#'   coord_fixed(ratio=1)

project_on_AB=function(dat,datAB,datOg=datAB[1,]){
  slopeACp=datAB %>%
    summarise(diffx=diff(x),
              diffy=diff(y)) %>%
    mutate(p=diffy/diffx) %>%
    pull(p)
  if(abs(slopeACp)==Inf){
    dat=dat %>%
      mutate(xp=datAB$x[1],
             yp=y) %>%
      mutate(l=yp-datOg$y)
  }
  if(slopeACp==0){
    dat=dat %>%
      mutate(xp=x,
             yp=datAB$y[1]) %>%
      mutate(l=xp-datOg$x)
  }
  if(abs(slopeACp)!=Inf & slopeACp!=0){
    slopeCCp=-1/slopeACp
    interceptACp=datAB %>%
      filter(point=="a") %>%
      mutate(interceptACp=y-slopeACp*x) %>%
      pull(interceptACp)
    interceptCCp=dat %>%
      mutate(interceptCCp=y-slopeCCp*x)
    dat=interceptCCp %>%
      mutate(xp=(interceptCCp-interceptACp)*(slopeACp/(slopeACp^2+1))) %>%
      mutate(yp=slopeACp*xp+interceptACp) %>%
      mutate(xt=sqrt((yp-datOg$y)^2+(xp-datOg$x)^2),
             yt=sqrt((y-yp)^2+(x-xp)^2)) %>%
      select(-interceptCCp)
  }
  return(dat)
}

