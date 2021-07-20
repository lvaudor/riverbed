#' A function that returns local maxima and minima in a profile
#'
#' @param sr tibble with columns l and z describing profile
#' @return tibm a tibble of local maxima ("locmax") and minima ("locmin")
#' @export
#' @examples
#' s1 <- tibble(l=c(1.2,5.3,8.1,12.0,13.1,
#'                  15.5,16.8,17.6,18.1,19.2,
#'                  21.3,23.1,24.1,25.2,26.5,
#'                  29.7,38.0,40.0,42.1,44.4),
#'              z=c(7.2,7.3,8.1,6.8,7.1,
#'                  1.3,2.0,2.2,1.1,1.8,
#'                  1.5,2.5,2.8,3.1,2.8,
#'                  2.1,3.3,5.0,4.6,6.2))
#' local_extrema(s1)
local_extrema=function(sr){
  sr=clean_transect(sr)
  tibm=sr %>%
    dplyr::arrange(l) %>%
    dplyr::mutate(id=1:dplyr::n()) %>%
    dplyr::mutate(zm1=dplyr::lag(z,1),
                  zp1=dplyr::lead(z,1)) %>%
    dplyr::mutate(bef=sign(z-zm1),
           aft=sign(z-zp1)) %>%
    dplyr::mutate(locext=dplyr::case_when(bef==aft & bef==-1~"locmin",
                                          bef==aft & bef==+1~"locmax",
                                          bef+aft==1~"locmax",
                                          bef+aft==-1~"locmin",
                                          TRUE~NA_character_)) %>%
    dplyr::filter(!is.na(locext)) %>%
    dplyr::select(id,l,z,locext)
  tibm=dplyr::bind_rows(sr[1,] %>%
                   dplyr::mutate(id=1,locext="locmax") %>%
                     dplyr::select(id,l,z,locext),
                 tibm,
                 sr[nrow(sr),] %>%
                   dplyr::mutate(id=nrow(sr),locext="locmax") %>%
                   dplyr::select(id,l,z,locext))
  return(tibm)
}
