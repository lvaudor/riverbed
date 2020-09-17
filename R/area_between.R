#' A function to calculate area between two profiles
#'
#' This function takes two profiles defined as (l1,z1) and (l2,z2) as inputs and calculates area between them
#' @param s1 tibble with columns l and z describing first profile
#' @param s2 tibble with columns l and z describing second profile
#' @param h if provided by user, the second profile is supposed to be horizontal, with constant height=h (defaults to NA)
#' @param sigma_z a vector of length 1 or 2 providing an estimate of the error in measures of height z1 and z2
#' @param sigma_l a vector of length 1 or 2 providing an estimate of the error in measures of longitudinal coordinates l1 and l2z1 and z2
#' @return area
#' @return area_by_type
#' @return sigma_area
#' @return data
#' @export
#' @examples
#' s1 <- tibble(l=c(1,3,5,6,9),
#'              z=c(1,2,3,2.5,5))
#' s2 <- tibble(l=c(0.5,2.5,4,6,8),
#'              z=c(3,1,2,4,3))
#' area_between(s1,s2)
#' area_between(s1,h=2)
area_between=function(s1,
                      s2=NA,
                      h=NA,
                      sigma_z=c(NA,NA),
                      sigma_l=c(NA,NA)){
  # calculate area
  ## calculate area of all successive trapezia
  dat <- cross_series(s1,s2,h) %>%
    mutate(w=lead(l,1)-l,
           L=z1-z2) %>%
    mutate(La=L,
           Lb=lead(L,1)) %>%
    mutate(a=w*(La+Lb)/2) %>%
    mutate(type=case_when(a>0~"upper",
                          a<0~"lower")) %>%
    arrange(l) %>%
    select(l,z1,p,z2,a,type,l_obs,z_obs,series) %>%
    mutate(order=2)
  datc <- dat %>%
    filter(p=="intersect") %>%
    mutate(type=case_when(type=="upper"~"lower",
                          type=="lower"~"upper"),
           a=NA,
           order=1)
  dat=bind_rows(dat,datc) %>%
    arrange(l,order) %>%
    select(-order)
  ind=max(which(!(is.na(dat$type))))
  if(ind<=nrow(dat)){dat$type[ind+1]=dat$type[ind]}

  area <- dat %>%
    filter(!is.na(a)) %>%
    summarise(area=sum(a,na.rm=TRUE)) %>%
    pull()
  area_by_type <- dat %>%
    filter(!is.na(a)) %>%
    group_by(type) %>%
    summarise(area=sum(a,na.rm=TRUE)) %>%
    filter(!is.na(type))

 # calculation of uncertainty
  if(length(sigma_z)==1){sigma_z=rep(sigma_z,2)}
  if(length(sigma_l)==1){sigma_l=rep(sigma_l,2)}
  calc_var=function(dat,index){
    sigz=sigma_z[index]
    sigl=sigma_l[index]
    var <-
      dat %>%
      filter(series==paste0("s",index)) %>%
      # With W=l_b-l_a (horizontal length)
      # And Z=0.5*(z_b+z_a) (mean vertical height)
      # Formula of area= W*Z
      mutate(W=lead(l_obs,1)-l_obs,
             Z=0.5*(z_obs+lead(z_obs,1))) %>%
      # For each trapezia we consider only one point as a variable
      mutate(varW=2*(sigl^2),
             varZ=2*(0.5^2)*(sigz^2))  %>%
      mutate(ntot=n(),
             boundary=row_number()==1|row_number()==ntot-1) %>%
      # Adjust to boundaries of series -> twice the variance
      mutate(varW=case_when(boundary~2*varW,
                            !boundary~varW),
             varZ=case_when(boundary~2*varZ,
                            !boundary~varZ)) %>%
      # Var(XY)=var(X)var(Y)+var(X)(E(Y)^2)+var(Y)(E(X)^2)
      mutate(var=varW*varZ + varW*(Z^2) + varZ*(W^2)) %>%
      summarise(var=sum(var,na.rm=TRUE)) %>%
      pull()
    return(var)
  }
  var1 <- calc_var(dat,index=1)
  var2 <- calc_var(dat,index=2)
  sigma_area <- sqrt(var1+var2)
  dat <- dat %>%
    mutate(z1min=case_when(series=="s1"~z1-sigma_z[1]),
           z1max=case_when(series=="s1"~z1+sigma_z[1]),
           z2min=case_when(series=="s2"~z2-sigma_z[2]),
           z2max=case_when(series=="s2"~z2+sigma_z[2])) %>%
    mutate(lmin= case_when(series=="s1"~l-sigma_l[1]),
           lmax= case_when(series=="s1"~l+sigma_l[1]),
           lmin= case_when(series=="s2"~l-sigma_l[2]),
           lmax= case_when(series=="s2"~l+sigma_l[2]))

  return(list(data=dat,
              area=area,
              area_by_type=area_by_type,
              sigma_area=sigma_area))
}
