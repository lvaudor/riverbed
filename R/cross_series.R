#' This function crosses two (l,z) series and provides a tibble regarding all points of interest (observations, intersections, interpolations)
#'
#' @param s1 tibble with columns l and z describing first profile
#' @param s2 tibble with columns l and z describing second profile
#' @param h if provided by user, the second profile is supposed to be horizontal, with constant height=h (defaults to NA)
#' @return a tibble providing info on all points of interest in the crossing of the two series.
#' @export
#' @examples
#' s1 <- tibble(l=c(1,3,5,6,9),
#'              z=c(1,2,3,2.5,5))
#' s2 <- tibble(l=c(0.5,2.5,4,6,8),
#'              z=c(3,1,2,4,3))
#' cross_series(s1,s2)
#' cross_series(s1,h=2)
cross_series=function(s1,
                      s2,
                      h=NA){
  if(!is.na(h)){
    s2=tibble(l=range(s1$l,na.rm=T),
              z=rep(h,2))
  }
  s1 <- arrange(s1,l)
  s2 <- arrange(s2,l)
  n1 <- nrow(s1)
  n2 <- nrow(s2)

  dat_interp=function(s_a,s_b,s_index){
    dat  <-  bind_cols(l=s_a$l,
                        z=s_a$z,
                        l_obs=s_a$l,
                        z_obs=s_a$z) %>%
      mutate(p="observed")
    dati <- bind_cols(l=s_b$l) %>%
      filter(!(l %in% s_a$l),
             l>min(s_a$l),
             l<max(s_a$l)) %>%
      mutate(z=approx(x=s_a$l,y=s_a$z,xout=l)$y,
             p="interpolated")
    dat <- bind_rows(dat,dati) %>%
      arrange(l) %>%
      mutate(series=s_index)
    return(dat)
  }
  ## interpolate values of z1 for all abscissae l2
  dat1=dat_interp(s1,s2,"s1")
  ## interpolate values of z2 for all abscissae l1
  dat2=dat_interp(s2,s1,"s2")
  # get coordinates of all intersects between profiles
  ## get all locations in data where intersects occur
  dat=bind_rows(dat1,dat2) %>%
    mutate(z1=case_when(series=="s1"~z),
           z2=case_when(series=="s2"~z)) %>%
    select(-z) %>%
    arrange(l) %>%
    group_by(l) %>%
    mutate(z1=z1[which(!is.na(z1))][1],
           z2=z2[which(!is.na(z2))][1]) %>%
    ungroup()

  dat_intersect <- dat %>%
    select(l,z1,z2) %>%
    mutate(gap=z1-z2) %>%
    mutate(gapl=lead(gap,1,default=NA)) %>%
    mutate(cross=gap*gapl<=0) %>%
    mutate(a=(lead(z1,1)-z1)/(lead(l,1)-l),
           c=(lead(z2,1)-z2)/(lead(l,1)-l)) %>%
    mutate(b=z1-a*l,
           d=z2-c*l) %>%
    mutate(x=-(b-d)/(a-c)) %>%
    mutate(y=a*x+b) %>%
    select(-a,-b,-c,-d, -gap, -gapl) %>%
    filter(cross) %>%
    mutate(l=x,
           z1=y,
           z2=y,
           p="intersect",
           l_obs=NA,
           z_obs=NA,
           series=NA) %>%
    select(-cross,-x,-y)
  dat=bind_rows(dat,dat_intersect) %>%
    arrange(l)
  return(dat)
}
