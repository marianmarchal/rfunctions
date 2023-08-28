#requires dplyr and stringr
#can yield warning if there are multiple workerids that did not fill out the same answer (e.g. if you piloted the experiment)
cleanstats <- function(statdat){
  statdatnew <- statdat %>% 
    #somehow there are always duplicates in the stats data, remove them
    distinct() %>% 
    #replace quotation marks to easy regex searches and trim string
    mutate(statistics = str_sub(gsub("\"", "__", statistics, fixed = T), 3, -3)) %>% 
    #put each question on new row
    separate_rows(statistics, sep = "},{") %>% 
    #put each conditional question on new row
    mutate(statistics = gsub("conditional__:[{__on__:__Yes__,__statistics__:[{", "%%%", statistics, fixed = T)) %>% 
    separate_rows(statistics, sep = "%%%") %>%
    #split question from answer
    mutate(statistics = gsub("__,__type.*?answer__:", "%%%", statistics)) %>%
    #needed for conditionals for which there is no answer  
    mutate(statistics = gsub("]}]}]", "%%%", statistics, fixed = T)) %>% 
    mutate(statistics = gsub("__,__type.*?%%%", "%%%", statistics)) %>%
    separate(statistics, c("question", "answer"), sep = "%%%") %>% 
    #remove trailing unnecessary stuff
    mutate(question = gsub("__name__:__", "", question),
           answer = gsub("^[^[:alnum:]]*|[^[:alnum:]]*$", "", answer)) %>%
    #make wide file from long file
    pivot_wider(names_from = question, values_from = c(answer)) %>% 
    #make everything character
    mutate(across(everything(), as.character))
  return(statdatnew)
}