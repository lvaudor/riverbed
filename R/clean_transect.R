#' A function that cleans and formats sr.
#'
#' @param sr tibble with columns l and z describing first profile
#' @return a clean sr data with variables id, l, and z (arranged by ascending l)
clean_transect=function(sr){
  result=sr %>%
    dplyr::arrange(l) %>%
    dplyr::mutate(id=1:dplyr::n()) %>%
    dplyr::select(id,dplyr::everything())
  return(result)
}

#' A function that returns the part of sr delimited by a and b.
#' @param sr tibble with columns l and z describing profile
#' @param channels the channel-describing data to use to cut the data into sub-series
#' @return the part of sr that is comprised between a and b
#' @export
#' @examples
#' data(s1)
#' channels=get_channels(s1,hmin=1,hmax=4.5)
#' cut_series(s1, channels)
cut_series=function(sr,channels){
  slice_ab=function(sr,l_a,l_b){
    range_id=sr %>%
      clean_transect() %>%
      dplyr::mutate(in_range=(l>=l_a & l<=l_b)) %>%
      dplyr::filter(in_range) %>%
      dplyr::pull(id) %>%
      range()
    range_id=c(max(1,range_id[1]-1),min(nrow(sr),range_id[2]+1))
    sr_slice=sr %>%
      dplyr::slice(range_id[1]:range_id[2])
    return(sr_slice)
  }
  result=channels %>%
    dplyr::mutate(sr=purrr::map(id_c,~sr)) %>%
    dplyr::mutate(sr=purrr::pmap(list(sr=sr,l_a=l_a,l_b=l_b),slice_ab))
  return(result)
}
