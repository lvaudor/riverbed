#' A function that returns channels delimited by one local maxima (on one side).
#'
#' @param section tibble with a section's descriptors (one row-tibble)
#' @param sr sr tibble with columns l and z describing profile
#' @return descriptors of the channel corresponding to section
get_channel_from_section=function(section,sr){
  sr=clean_transect(sr)

  bounds= sr %>%
    dplyr::filter(id>=section$i_a,
                  id<=section$i_b) %>%
    cross_series(h=min(section$z_a,section$z_b)) %>%
    dplyr::filter(p=="intersect") %>%
    dplyr::pull(l) %>%
    range(na.rm=TRUE)
  l_bounds=tibble::tibble(l_a=bounds[1],
                          l_b=bounds[2])
  i_bounds=tibble::tibble(i_a=sr %>%
                     dplyr::filter(l<=bounds[1]) %>%
                     dplyr::pull(id) %>%
                     max(),
                   i_b=sr %>%
                     dplyr::filter(l>=bounds[2]) %>%
                     dplyr::pull(id) %>%
                     min())
  result=dplyr::bind_cols(l_bounds,
                          i_bounds) %>%
    dplyr::mutate(id_c=section$id_c,
                  z_a=min(section$z_a,section$z_b),
                  z_b=min(section$z_a,section$z_b),
                  z_min=section$z_min) %>%
    dplyr::mutate(z_max=min(z_a,z_b)) %>%
    dplyr::select(id_c,i_a,i_b,l_a,l_b,z_a,z_b,z_min,z_max)
  return(result)
}

#' A function that returns sections and channels
#' @param sr tibble with columns l and z describing profile
#' @param hmin the minimum height (relative to local minimum of z) of levees-defining local maxima
#' @param hmax the maximum height (relative to minimum of z) of a channel-defining local minima
#' @param type whether to return section or bankfull channel ("section" or "bankfull"). Defaults to "bankfull"
#' @return channels a tibble with sections and channels descriptors
#' @export
#' @examples
#' data(s1)
#' channels=get_channels(s1, hmin=1, hmax=5.5)
get_channels=function(sr, hmin,hmax,type="bankfull"){
  sr=clean_transect(sr)
  channels=get_sections(sr,hmin=hmin,hmax=hmax)
  if(type=="bankfull"){
    channels=channels %>%
      dplyr::mutate(id_r=1:dplyr::n()) %>%
      dplyr::group_by(id_r) %>%
      tidyr::nest() %>%
      dplyr::mutate(data=purrr::map(data,get_channel_from_section,sr)) %>%
      tidyr::unnest(cols=c(data)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-id_r)
  }
  return(channels)
}


#' A function that plots channels and sections.
#' @param channels a table with channels and sections coordinates, as returned by get_channels
#' @param sr tibble with columns l and z describing profile
#' @return a plot
#' @export
#' @examples
#' data(s1)
#' channels=get_channels(s1, hmin=1, hmax=5.5)
#' plot_channels(channels,s1)
plot_channels=function(channels,sr){
  ggplot2::ggplot(data=channels)+
    ggplot2::geom_rect(ggplot2::aes(xmin=l_a,xmax=l_b,
                                    ymin=z_min,ymax=z_max,
                                    fill=factor(id_c)),
              alpha=0.5)+
    ggplot2::geom_line(data=sr,ggplot2::aes(x=l,y=z))
}
