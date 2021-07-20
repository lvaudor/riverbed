s1=readRDS("data-raw/s1.RDS") %>% select(l,z)
s2=readRDS("data-raw/s2.RDS") %>% select(l,z)

usethis::use_data(s1, overwrite=TRUE)
usethis::use_data(s2, overwrite=TRUE)
