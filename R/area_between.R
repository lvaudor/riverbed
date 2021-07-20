#' A function to assess the variability in the area calculation
#'
#' This function takes two profiles defined as (l1,z1) and (l2,z2) as inputs and calculates area between them
#' @param dat data with all trapezoids information for area calculation
#' @param index series identifier (1 or 2)
#' @param sigma_z the uncertainty in measures z (can be different for the two series)
#' @param sigma_z the uncertainty in measures l (can be different for the two series)
#' @return var a table regarding the variance in areas calculated.
calc_var=function(dat,index,sigma_z,sigma_l){
  if(length(sigma_z)==1){sigma_z=rep(sigma_z,2)}
  if(length(sigma_l)==1){sigma_l=rep(sigma_l,2)}
  sigz=sigma_z[index]
  sigl=sigma_l[index]
  var <- dat %>%
    dplyr::filter(series==paste0("s",index)) %>%
    # With W=l_b-l_a (horizontal length)
    # And Z=0.5*(z_b+z_a) (mean vertical height)
    # Formula of area= W*Z
    dplyr::mutate(W=dplyr::lead(l_obs,1)-l_obs,
                  Z=0.5*(z_obs+dplyr::lead(z_obs,1))) %>%
    # For each trapezia we consider only one point as a variable
    dplyr::mutate(varW=2*(sigl^2),
                  varZ=2*(0.5^2)*(sigz^2))  %>%
    dplyr::mutate(ntot=dplyr::n(),
                  boundary=dplyr::row_number()==1|dplyr::row_number()==ntot-1) %>%
    # Adjust to boundaries of series -> twice the variance
    dplyr::mutate(varW=dplyr::case_when(boundary~2*varW,
                          !boundary~varW),
                  varZ=dplyr::case_when(boundary~2*varZ,
                          !boundary~varZ)) %>%
    # Var(XY)=var(X)var(Y)+var(X)(E(Y)^2)+var(Y)(E(X)^2)
    dplyr::mutate(var=varW*varZ + varW*(Z^2) + varZ*(W^2)) %>%
    dplyr::summarise(var=sum(var,na.rm=TRUE)) %>%
    dplyr::pull()
  return(var)
}


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
#' data(s1)
#' data(s2)
#' area_between(s1,s2)
#' area_between(s1,h=-5)
area_between=function(s1,
                      s2=NA,
                      h=NA,
                      sigma_z=c(NA,NA),
                      sigma_l=c(NA,NA)){
    # calculate area
    ## calculate area of all successive trapezia
    dat <- cross_series(s1,s2,h) %>%
      dplyr::mutate(w=dplyr::lead(l,1)-l,
                    L=z1-z2) %>%
      dplyr::mutate(La=L,
                    Lb=dplyr::lead(L,1)) %>%
      dplyr::mutate(a=w*(La+Lb)/2) %>%
      dplyr::mutate(type=dplyr::case_when(a>0~"upper",
                                   a<0~"lower",
                                   TRUE~NA_character_)%>%
                      factor(levels=c("upper","lower"))) %>%
      dplyr::arrange(l) %>%
      dplyr::select(l,z1,p,z2,a,type,l_obs,z_obs,series) %>%
      dplyr::mutate(order=2)
    datc <- dat %>%
      dplyr::filter(p=="intersect") %>%
      dplyr::mutate(type=dplyr::case_when(type=="upper"~"lower",
                                   type=="lower"~"upper") %>%
                      factor(levels=c("upper","lower")),
                    a=NA,
                    order=1)
    dat=dplyr::bind_rows(dat,datc) %>%
      dplyr::arrange(l,order) %>%
      dplyr::select(-order)
    ind=max(which(!(is.na(dat$type))))
    if(ind<=nrow(dat)){dat$type[ind+1]=dat$type[ind]}

    area <- dat %>%
      dplyr::filter(!is.na(a)) %>%
      dplyr::summarise(area=sum(a,na.rm=TRUE)) %>%
      dplyr::pull()
    area_by_type <- dat %>%
      dplyr::filter(!is.na(a)) %>%
      tidyr::complete(type) %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(area=sum(a,na.rm=TRUE)) %>%
      dplyr::filter(!is.na(type))

 # calculation of uncertainty
  var1 <- calc_var(dat,index=1,
                              sigma_z=sigma_z,sigma_l=sigma_l)
  var2 <- calc_var(dat,index=2,
                              sigma_z=sigma_z,sigma_l=sigma_l)
  sigma_area <- sqrt(var1+var2)
  dat <- dat %>%
    dplyr::mutate(zmin=dplyr::case_when(series=="s1"~z1-sigma_z[1],
                                        series=="s2"~z2-sigma_z[2]),
                  zmax=dplyr::case_when(series=="s1"~z1+sigma_z[1],
                                        series=="s2"~z2+sigma_z[2])) %>%
    dplyr::mutate(lmin= dplyr::case_when(series=="s1"~l-sigma_l[1],
                                         series=="s2"~l-sigma_l[2]),
                  lmax= dplyr::case_when(series=="s1"~l+sigma_l[1],
                                         series=="s2"~l+sigma_l[2]))

  return(list(data=dat,
              area=area,
              area_by_type=area_by_type,
              sigma_area=sigma_area))
}
