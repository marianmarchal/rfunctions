#' Plot fitted estimates of one or two discrete variables with two levels each
#'
#' @param model Model that the estimates are taken from
#' @param y.lab String, Title of the y-axis
#' @param plot.type String, Either "point" or "bar"
#' @param x.var String, Variable on the x-axis
#' @param x.level Numeric vector, Levels of this variable
#' @param x.names Character vector, Names of the levels of this variable
#' @param x.title String, Name of this variable in the plot
#' @param group.var String, Grouping variable - optional
#' @param group.levels Numeric vector, Levels of this variable - optional
#' @param group.names Character vector, Names of the levels of this variable - optional
#' @param group.title String, Name of this variable in the plot - optional
#' @param line Boolean, whether lines are present
#' @param confidence.level Numeric, Confidence level
#'
#' @return Plot with fitted estimates
#' @export
#'
plot_fitted <- function(model,
                        y.lab,
                        plot.type = "point",
                        x.var,
                        x.levels = c(-1, 1),
                        x.names,
                        x.title,
                        group.var = NULL,
                        group.levels = c(-1, 1),
                        group.names,
                        group.title,
                        line = T,
                        confidence.level = 0.95){
  #set xlevels
  if(is.null(group.var)){
    xlevels <- list(x.levels)
    names(xlevels) <- c(x.var)
  } else {
    xlevels <- list(x.levels, group.levels)
    names(xlevels) <- c(x.var, group.var)
  }
  
  #set position_dodge
  pd_size <- ifelse(plot.type == "point", .2, .9)
  pd <- position_dodge(width = pd_size)
  
  #create basis of plot
  if(is.null(group.var)){
    base_plot <- effects::Effect(x.var,
                                 model, se = T, confidence.level = confidence.level,
                                 xlevels = xlevels) %>%
      as.data.frame %>%
      mutate({{x.title}} := ifelse(.[[1]] == x.levels[1], x.names[1], x.names[2])) %>%
      print %>%
      ggplot(
        aes(x = .data[[x.title]],
            y = fit,
            group = 1
        ))
  } else {
    base_plot <- effects::Effect(c(x.var, group.var),
                                 model, se = T, confidence.level = confidence.level,
                                 xlevels = xlevels) %>%
      as.data.frame %>%
      mutate({{x.title}} := ifelse(.[[1]] == x.levels[1], x.names[1], x.names[2]),
             {{group.title}} := ifelse(.[[2]] == group.levels[1], group.names[1], group.names[2])) %>%
      print %>%
      ggplot(
        aes(x = .data[[x.title]],
            y = fit,
            group = .data[[group.title]],
            fill = .data[[group.title]]
        ))
  }
  
  #update layout
  base_plot <- base_plot +
    theme(text = element_text(family = "serif")) +
    theme_bw(base_size = 22) +
    xlab(x.title) +
    ylab(y.lab)
  # scale_linetype_discrete(name=group.title,
  #                     	labels=group.names
  # )
  
  if(plot.type == "point"){
    base_plot <- base_plot +
      geom_point(
        size= 3,
        position = pd)
  } else {
    base_plot <- base_plot +
      geom_bar(
        stat = "identity",
        color = "black",
        position = pd) +
      scale_fill_grey(start = 0.4, end = 0.8)
  }
  
  if(line == T & !is.null(group.var)){
    base_plot <- base_plot +
      geom_line(
        aes(linetype = .data[[group.title]]),
        size = 1,
        position = pd)
  } else if(line == T & is.null(group.var)){
    base_plot <- base_plot +
      geom_line(
        aes(linetype = "dotted",
            size = 1,
            position = pd))
  }
  
  p <- base_plot +
    geom_errorbar(
      aes(ymin = fit-se,
          ymax= fit+se),
      size = .5,
      width = .3,
      position = pd
    )
  
  return(p)
}
