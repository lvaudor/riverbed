#' This function crosses two (l,z) series and provides a tibble regarding all points of interest (observations, intersections, interpolations)
#'
#' @param s1 tibble with columns l and z describing first profile
#' @param s2 tibble with columns l and z describing second profile
#' @param h if provided by user, the second profile is supposed to be horizontal, with constant height=h (defaults to NA)
#' @return a tibble providing info on all points of interest in the crossing of the two series.
#' @export
#' @examples
#' data(s1)
#' data(s2)
#' cross_series(s1,s2)
#' cross_series(s1,h=2)
cross_series=function(s1,
                      s2,
                      h=NA){
  if(!is.na(h)){
    s2=tibble::tibble(l=range(s1$l,na.rm=T),
              z=rep(h,2))
  }
  s1 <- clean_transect(s1)
  s2 <- clean_transect(s2)
  n1 <- nrow(s1)
  n2 <- nrow(s2)

  dat_interp=function(s_a,s_b,s_index){
    dat  <-  dplyr::bind_cols(l=s_a$l,
                        z=s_a$z,
                        l_obs=s_a$l,
                        z_obs=s_a$z) %>%
      dplyr::mutate(p="observed")
    dati <- dplyr::bind_cols(l=s_b$l) %>%
      dplyr::filter(!(l %in% s_a$l),
             l>min(s_a$l),
             l<max(s_a$l)) %>%
      dplyr::mutate(z=stats::approx(x=s_a$l,y=s_a$z,xout=l)$y,
             p="interpolated")
    dat <- dplyr::bind_rows(dat,dati) %>%
      dplyr::arrange(l) %>%
      dplyr::mutate(series=s_index)
    return(dat)
  }
  ## interpolate values of z1 for all abscissae l2
  dat1=dat_interp(s1,s2,"s1")
  ## interpolate values of z2 for all abscissae l1
  dat2=dat_interp(s2,s1,"s2")
  # get coordinates of all intersects between profiles
  ## get all locations in data where intersects occur
  dat=dplyr::bind_rows(dat1,dat2) %>%
    dplyr::mutate(z1=dplyr::case_when(series=="s1"~z),
           z2=dplyr::case_when(series=="s2"~z)) %>%
    dplyr::select(-z) %>%
    dplyr::arrange(l) %>%
    dplyr::group_by(l) %>%
    dplyr::mutate(z1=z1[which(!is.na(z1))][1],
           z2=z2[which(!is.na(z2))][1]) %>%
    dplyr::ungroup()

  dat_intersect <- dat %>%
    dplyr::select(l,z1,z2) %>%
    dplyr::mutate(gap=z1-z2) %>%
    dplyr::mutate(gapl=dplyr::lead(gap,1,default=NA)) %>%
    dplyr::mutate(cross=gap*gapl<=0) %>%
    dplyr::mutate(a=(lead(z1,1)-z1)/(lead(l,1)-l),
           c=(lead(z2,1)-z2)/(lead(l,1)-l)) %>%
    dplyr::mutate(b=z1-a*l,
           d=z2-c*l) %>%
    dplyr::mutate(x=-(b-d)/(a-c)) %>%
    dplyr::mutate(y=a*x+b) %>%
    dplyr::select(-a,-b,-c,-d, -gap, -gapl) %>%
    dplyr::filter(cross) %>%
    dplyr::mutate(l=x,
           z1=y,
           z2=y,
           p="intersect",
           l_obs=NA,
           z_obs=NA,
           series=NA) %>%
    dplyr::mutate(series=as.character(series)) %>%
    dplyr::select(-cross,-x,-y)
  dat=dplyr::bind_rows(dat,dat_intersect) %>%
    dplyr::arrange(l)
  return(dat)
}
