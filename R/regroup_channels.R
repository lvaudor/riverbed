#' A function that cleans and formats sr.
#' @param channels the table describing channels
#' @param by_id the channel id (defines how the regrouping is done)
#' @export
#' @param sr tibble with columns l and z describing first profile
#' @return a clean sr data with variables id, l, and z (arranged by ascending l)
regroup_channels=function(channels,by_id){
  by_id=dplyr::enquo(by_id)
  result=channels %>%
    dplyr::group_by(!!by_id) %>%
    dplyr::summarise(l_a=min(l_a),
              l_b=max(l_b)) %>%
    dplyr::ungroup()
  return(result)
}
