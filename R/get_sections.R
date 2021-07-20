#' Describe levee-to-levee section in which a local minimum with z=z_min falls.
#' @param rtib tibble describing local minimum (id and z)
#' @param tibm tibble with local maxima
#' @param sr tibble with columns l and z describing profile
#' @param hmin the minimum value for a local maximum to be delimiting a section (z > z_min + hmin)
#' @return a tibble describing the section.
find_section=function(rtib,tibm,sr,hmin){
  imax=tibm %>%
    dplyr::filter(locext=="locmax") %>%
    dplyr::filter(z>rtib$z+hmin)
  imaxl=imax %>%
    dplyr::filter(id<rtib$id) %>%
    dplyr::pull(id)
  i_a=max(c(1,imaxl))
  imaxr=imax %>%
    dplyr::filter(id>rtib$id) %>%
    dplyr::pull(id)
  i_b=min(c(max(tibm$id),imaxr))

  actual_min=sr %>%
    dplyr::mutate(ind=1:dplyr::n()) %>%
    dplyr::filter(ind>=i_a, id<=i_b) %>%
    dplyr::filter(z==min(z)) %>%
    dplyr::slice(1)

  result=tibble::tibble(id_c=actual_min$id,
                        l_s=actual_min$l,
                        i_a=i_a,
                        i_b=i_b,
                        z_min=actual_min$z)
  return(result)
}

#' A function that returns sections delimited by local maxima with height > hmin.
#'
#' @param sr tibble with columns l and z describing first profile
#' @param hmin the minimum height (relative to local minimum of z) of levees-defining local maxima
#' @param hmax the maximum height (relative to minimum of z) of a channel-defining local minima
#' @return hmin
#' @export
#' @examples
#' data(s1)
#' get_sections(s1, hmin=1,hmax=5)
get_sections=function(sr, hmin,hmax){
  sr=clean_transect(sr)
  tibm=local_extrema(sr)

  tibn=tibm %>%
    dplyr::filter(locext=="locmin") %>%
    dplyr::filter(z<min(z)+hmax) %>%
    dplyr::mutate(idr=1:dplyr::n()) %>%
    dplyr::group_by(idr) %>%
    tidyr::nest() %>%
    dplyr::mutate(section=purrr::map(data,find_section,sr=sr,tibm=tibm,hmin=hmin)) %>%
    tidyr::unnest(cols=c(data,section)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id_c) %>%
    dplyr::summarise(l=unique(l_s),
              z=unique(z_min),
              i_a=min(i_a),
              i_b=max(i_b))
  tibv=tibble::tibble(
    i_a=tibn$i_a,
    i_b=tibn$i_b,
    l_a=sr$l[tibn$i_a],
    l_b=sr$l[tibn$i_b],
    z_a=sr$z[tibn$i_a],
    z_b=sr$z[tibn$i_b],
    z_min=tibn$z,
    z_max=pmax(z_a,z_b)) %>%
    dplyr::mutate(id_c=1:dplyr::n()) %>%
    dplyr::select(id_c,dplyr::everything())
  return(tibv)
}
