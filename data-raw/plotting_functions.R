### Header ---------------------------
###
### Title: plotting.R
###
### Description: Functions used to plot figures in Daljord et al. (2021)
###
### Author: Omkar A. Katta
###
### Notes:
###
###

### Preliminaries ---------------------------

### Color Palette ---------------------------


get_color_palette <- function(number, grayscale = grayscale){
  # colors: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  orange <- "#E69F00"
  blue <- "#56B4E9"
  green <- "#009E73"
  yellow <- "#F0E442"
  darkblue <- "#0072B2"
  red <- "#D55E00"
  pink <- "#CC79A7"

  black <- "#000000"
  dark <- "#0c0c0c"
  mid <- "#383838"
  light <- "#757575"
  lightest <- "#dbd9d9"

  annotate_light <- "#bcbcbc"

  if (number == 1){
    # assign(col1, black, envir = .GlobalEnv)
    col1 <- black
    return(col1)
  }
  if (number == 2 & !grayscale){
    # assign(col1, blue, envir = .GlobalEnv)
    # assign(col2, green, envir = .GlobalEnv)
    col1 <- orange
    col2 <- blue
    return(c(col1, col2))
  }
  if (number == 3 & !grayscale){
    # assign(col1, brown, envir = .GlobalEnv)
    # assign(col2, olive, envir = .GlobalEnv)
    # assign(col3, purple, envir = .GlobalEnv)
    col1 <- orange
    col2 <- blue
    col3 <- green
    return(c(col1, col2, col3))
  }
  if (number == 4 & !grayscale){
    col1 <- orange
    col2 <- blue
    col3 <- green
    col4 <- darkblue
    return(c(col1, col2, col3, col4))
  }
  if (number == 5 & !grayscale){
    col1 <- orange
    col2 <- blue
    col3 <- green
    col4 <- darkblue
    col5 <- red
    return(c(col1, col2, col3, col4, col5))
  }
  if (number == 2 & grayscale){
    # assign(col1, dark, envir = .GlobalEnv)
    # assign(col2, light, envir = .GlobalEnv)
    col1 <- dark
    col2 <- lightest
    return(c(col1, col2))
  }
  if (number == 3 & grayscale){
    # assign(col1, dark, envir = .GlobalEnv)
    # assign(col2, mid, envir = .GlobalEnv)
    # assign(col3, light, envir = .GlobalEnv)
    col1 <- dark
    col2 <- mid
    col3 <- light
    return(c(col1, col2, col3))
  }
  if (number == 4 & grayscale){
    col1 <- black
    col2 <- dark
    col3 <- mid
    col4 <- light
    return(c(col1, col2, col3, col4))
  }
  if (number == 5 & grayscale){
    col1 <- black
    col2 <- dark
    col3 <- mid
    col4 <- light
    col5 <- lightest
    return(c(col1, col2, col3, col4, col5))
  }
}

### Theme  ---------------------------


theme_bmp <- function(legend.direction = "vertical",
                      legend.position = c(0.8, 0.8),
                      legend.title = ggplot2::element_blank(),
                      xangle = NULL,
                      sizefont = fontsize,
                      axissizefont = fontsizeaxis){
  '%+replace%' <- ggplot2::'%+replace%'
  ggplot2::theme_classic() %+replace%
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill=NA, size=0.5),
      legend.background = ggplot2::element_rect(fill="transparent", color=NA),
      legend.key = ggplot2::element_rect(fill="transparent", color=NA),
      # panel.grid.major = element_line(colour = lightest, size = 0.20),
      # panel.grid.minor = element_line(colour = lightest, size = 0.10),
      axis.line = ggplot2::element_blank(),
      legend.direction = legend.direction,
      legend.position = legend.position,
      legend.title = legend.title,
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = xangle),
      text = ggplot2::element_text(size = sizefont),
      axis.title = ggplot2::element_text(size = axissizefont)
    )
}

### Plotting  ---------------------------


bmp_plot <- function(data,
                     color = NULL, colorgray = grayscale,
                     fill = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     ggtitle = "",
                     xtype = "ignore",
                     ytype = "ignore",
                     xbreaks = ggplot2::waiver(),
                     xlabels = ggplot2::waiver(),
                     xminor = ggplot2::waiver(),
                     ybreaks = ggplot2::waiver(),
                     ylabels = ggplot2::waiver(),
                     yminor = ggplot2::waiver(),
                     xlims = NULL,
                     ylims = NULL,
                     legendlabels = ggplot2::waiver(),
                     size = 1,
                     ...){
  # warning: any additional layers must have the same aesthetic components
  list(
    theme_bmp(...),
    # if (!is.null(quo(color))){
    ggplot2::scale_color_manual(values = get_color_palette(data %>% dplyr::select({{color}}) %>% dplyr::distinct() %>% nrow,
                                                           grayscale = colorgray),
                                labels = legendlabels,
                                name = ""),
    # },
    # if (!is.null(quo(fill))){
    ggplot2::scale_fill_manual(values = get_color_palette(data %>% dplyr::select({{fill}}) %>% dplyr::distinct() %>% nrow,
                                                          grayscale = colorgray),
                               labels = legendlabels,
                               name = ""),
    # },
    ggplot2::xlab(xlab),
    ggplot2::ylab(ylab),
    ggplot2::ggtitle(ggtitle),
    ggplot2::coord_cartesian(xlim = xlims, ylim = ylims),
    if (xtype == "date") {
      ggplot2::scale_x_date(breaks = xbreaks,
                            date_labels = xlabels,
                            minor_breaks = xminor)
    },
    if (xtype == "ym"){
      ggplot2::scale_x_date(breaks = seq(min(data$ym), max(data$ym), by = "1 year"),
                            date_labels = "%Y",
                            minor_breaks = data$ym)
    },
    if (xtype == "continuous"){
      ggplot2::scale_x_continuous(breaks = xbreaks,
                                  labels = xlabels,
                                  minor_breaks = xminor)
    },
    if (ytype == "continuous"){
      ggplot2::scale_y_continuous(breaks = ybreaks,
                                  labels = ylabels,
                                  minor_breaks = yminor)
    }

  )
}


bmp_point <- function(x, y, data,
                      color = NULL,
                      size = 1,
                      alpha = 1,
                      ...){
  list(
    ggplot2::geom_point(data = data,
                        mapping = ggplot2::aes(x = {{x}},
                                               y = {{y}},
                                               color = {{color}}),
                        alpha = alpha,
                        size = size,
                        ...)
  )
}


bmp_twohist <- function(data1, data2,
                        x, scale,
                        binwidth = NULL,
                        alpha = ifelse(grayscale, 0.8, 0.35)){
  list(
    ggplot2::geom_histogram(data = data1,
                            mapping = ggplot2::aes(x = {{x}} / scale,
                                                   y = ..density..,
                                                   fill = "1",
                                                   color = "1"),
                            alpha = alpha,
                            binwidth = binwidth),
    ggplot2::geom_histogram(data = data2,
                            mapping = ggplot2::aes(x = {{x}} / scale,
                                                   y = ..density..,
                                                   fill = "2",
                                                   color = "2"),
                            alpha = alpha,
                            binwidth = binwidth),
    ggplot2::guides(color = F)
  )
}


bmp_vline <- function(xint, color = "#bcbcbc", linetype = 5){
  list(
    ggplot2::geom_vline(xintercept = xint,
                        color = color,
                        linetype = linetype)
  )
}
