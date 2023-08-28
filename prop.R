#requires dplyr
prop <- function(.dat, ..., name = "p", .keep_count = F){
  dat <- .dat %>% 
    group_by(...) %>%
     summarize(n=n()) %>% 
    mutate(p = n/sum(n)) %>% 
    ungroup
  if(.keep_count != F){
    dat <- dat %>% select(-n)
  }
  names(dat)[names(dat) == "p"] <- name
  return(dat)
}

add_prop <- function(.dat, ..., name = "p"){
  propdat <- .dat %>% prop(..., name = name)
  return(.data %>% merge(propdat))
}