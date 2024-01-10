#' Plot fitted estimates of one to three discrete variables with two levels each
#'
#' @param model Model that the estimates are taken from
#' @param x.var String, Variable on the x-axis
#' @param x.level Numeric vector, Levels of this variable
#' @param x.names Character vector, Names of the levels of this variable
#' @param x.title String, Name of this variable in the plot
#' @param group.var String, Grouping variable - optional
#' @param group.levels Numeric vector, Levels of this variable - optional
#' @param group.names Character vector, Names of the levels of this variable - optional
#' @param group.title String, Name of this variable in the plot - optional
#' @param wrap.var String, Grouping variable - optional
#' @param wrap.levels Numeric vector, Levels of this variable - optional
#' @param wrap.names Character vector, Names of the levels of this variable - optional
#' @param wrap.title String, Name of this variable in the plot - optional
#' @param y.lab String, Title of the y-axis
#' @param plot.type String, Either "point" or "bar"
#' @param line Boolean, whether lines are present
#' @param errorbar String, Either "se" (for standard error) or "ci" (for confidence interval)
#' @param confidence.level Numeric, Confidence level
#'
#' @return Plot with fitted estimates
#' @export
#'

plot_fitted <-  function(model,
                         x.var,
                         x.levels = c(-1, 1),
                         x.names = c("Condition A", "Condition B"),
                         x.title = "A vs. B",
                         group.var = NULL,
                         group.levels = c(-1, 1),
                         group.names = c("Condition X", "Condition Y"),
                         group.title = "X vs. Y",
                         wrap.var = NULL,
                         wrap.levels = c(-1,1),
                         wrap.names = c("Condition K", "Condition L"),
                         wrap.title = "K vs. L",
                         line = T,
                         y.lab = "Fitted estimates",
                         plot.type = "point", #c("point", "bar")
                         errorbar = "se", #c("se", "ci")
                         confidence.level = 0.95
                         ){
  
  xlevels <- list(x.levels, group.levels, wrap.levels)
  names(xlevels) <- c(x.var, group.var, wrap.var)
  
  #set position_dodge
  pd_size <- ifelse(plot.type == "point", .2, .9)
  pd <- ggplot2::position_dodge(width = pd_size)
  
  #extract estimates
  if(is.null(wrap.var)){
    est.dat <- effects::Effect(c(x.var, group.var),
                               model, se = T, confidence.level = confidence.level,
                               xlevels = xlevels) %>%
      as.data.frame() %>%
      dplyr::mutate({{x.title}} := ifelse(.[[1]] == x.levels[1], x.names[1], x.names[2]),
                    {{group.title}} := ifelse(.[[2]] == group.levels[1], group.names[1], group.names[2]))
  } else if(is.null(group.var)){
    est.dat <- effects::Effect(x.var,
                               model, se = T, confidence.level = confidence.level,
                               xlevels = xlevels) %>%
      as.data.frame() %>%
      dplyr::mutate({{x.title}} := ifelse(.[[1]] == x.levels[1], x.names[1], x.names[2]))
  } else {
    est.dat <- effects::Effect(c(x.var, group.var, wrap.var),
                               model, se = T, confidence.level = confidence.level,
                               xlevels = xlevels) %>%
      as.data.frame() %>%
      dplyr::mutate({{x.title}} := ifelse(.[[1]] == x.levels[1], x.names[1], x.names[2]),
                    {{group.title}} := ifelse(.[[2]] == group.levels[1], group.names[1], group.names[2]),
                    {{wrap.title}} := ifelse(.[[3]] == wrap.levels[1], wrap.names[1], wrap.names[2]))
  }
  
  print(est.dat)
  
  #create basis of plot
  if(is.null(group.var)){
    base_plot <- ggplot2::ggplot(est.dat,
                                 ggplot2::aes(x = .data[[x.title]],
                                              y = fit,
                                              group = 1
                                 ))
  } else {
    base_plot <- ggplot2::ggplot(est.dat,
                                 ggplot2::aes(x = .data[[x.title]],
                                              y = fit,
                                              group = .data[[group.title]],
                                              fill = .data[[group.title]]
                                 ))
  }
  
  #update layout
  base_plot <- base_plot +
    ggplot2::theme(text = element_text(family = "serif")) +
    ggplot2::theme_bw(base_size = 22) +
    ggplot2::xlab(x.title) +
    ggplot2::ylab(y.lab)
  
  #add points or bar  
  if(plot.type == "point" & !is.null(group.var)){
    base_plot <- base_plot +
      ggplot2::geom_point(
        size= 3,
        position = pd)
  } else if(plot.type == "point" & is.null(group.var)){
    base_plot <- base_plot +
      ggplot2::geom_point(
        size= 3)
  } else {
    base_plot <- base_plot +
      ggplot2::geom_bar(
        stat = "identity",
        color = "black",
        position = pd) +
      ggplot2::scale_fill_grey(start = 0.4, end = 0.8)
  }
  
  #add lines
  if(line == T & !is.null(group.var)){
    base_plot <- base_plot +
      ggplot2::geom_line(
        ggplot2::aes(linetype = .data[[group.title]]),
        size = 1,
        position = pd)
  } else if(line == T & is.null(group.var)){
    base_plot <- base_plot +
      ggplot2::geom_line(
        linetype = "dotted",
                     size = 1)
  }
  
  #add facet_wrap
  if(!is.null(wrap.var)){
    base_plot <- base_plot +
      ggplot2::facet_wrap(~.data[[wrap.title]])
  }
  
  #add error bars
  if(errorbar == "se"){
  p <- base_plot +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = fit-se,
                   ymax= fit+se),
      size = .5,
      width = .3,
      position = pd
    )
  } else if(errorbar == "ci"){
    p <- base_plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower,
                     ymax= upper),
        size = .5,
        width = .3,
        position = pd
      )
  }
  
  return(p)
}
