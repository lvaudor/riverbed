#' A function to project data on a line defined by two points A & B
#'
#' @param dat tibble with coordinates columns x and y
#' @param datAB tibble with two rows (points A and B) and coordinates columns x and y
#' @return xp x coordinate of points projected on the transect
#' @return yp y coordinate of points projected on the transect
#' @return xt x coordinate along the transect (new coordinate x)
#' @return yt y coordinate corresponding to the distance to the transect (new coordinate y)
#' @export
#' @examples
#' dat=tibble(x=c(866142.3, 866149.9, 866143.7),
#'            y=c(84496.97, 84496.93, 84498.50))
#' datAB=tibble(x=c(866141.3,866148.7),
#'              y=c(84498.97, 84496.50))
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
      slice(1) %>%
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
  dat=dat %>%
    mutate(xt=xt-min(xt))
  return(dat)
}

