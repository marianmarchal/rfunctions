#' Clean answers from LingoTurk
#'
#' @description 
#' Cleans answers from LingoTurk output.
#'
#' @param dat Dataframe
#' @param col String with column name of answer values
#' @param answer_options Vector with strings of the names of the answer options
#'
#' @return Dataset with answer options as column names
#' @importFrom dplyr mutate
#' @importFrom tidyr separate
#' @export
#'
cleananswer <- function(dat,
                        col = "answer",
                        answer_options = c("answer", "timeTaken")
                        ){
  
  names(dat)[names(dat) == col] <- "answer"
  
  dat <- dat %>% 
    #remove all those annoying \\\
    dplyr::mutate(answer = gsub("\\\"", "", answer, fixed=T),
                  #remove everything up to first answer_option
                  answer = gsub(paste0(".*", answer_options[1], ":"), "", answer),
                  ) %>% 
    tidyr::separate(answer, answer_options, 
                    sep = paste0(paste(answer_options[2:length(vector)], collapse = ":|"), ":"))
  
  return(dat)
}
