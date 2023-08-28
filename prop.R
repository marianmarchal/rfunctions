#requires dplyr and tidyr
prop <- function(.dat, ..., name = "p", .keep_count = F){
  dat <- .dat %>% 
    group_by(...) %>%
     summarize(n=n()) %>% 
    mutate({{p}} := n/sum(n)) %>% 
    ungroup
  if(.keep_count != F){
    dat <- dat %>% select(-n)
  }
  return(dat)
}

add_prop <- function(.dat, ..., name = "p"){
  propdat <- .dat %>% prop(..., name = name)
  return(.data %>% merge(propdat))
}

surprisal <- function(.dat, ..., name = "surprisal"){
  .dat %>% 
    prop(...) %>% 
    mutate({{surprisal}} := -log2(p))
}

entropy <- function(.dat, ..., name = "entropy"){
  col_vars <- quos(...)
  .dat %>% 
    # add_surprisal(!!!col_vars) %>%
    add_surprisal(...) %>% 
    mutate(ent = p *surprisal) %>% 
    #calculate entropy over last grouping var
    group_by(!!!col_vars[1:length(col_vars)-1]) %>%
    summarize({{name}} := sum(ent))
}