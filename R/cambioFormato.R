#'Funcion para cambiar los ambitos de la graficas
#'

cambiarGraficas <- function(tamFuente){
  pkg.env$fontSize <-tamFuente
  print(c("El tamaño de la fuente es: ", pkg.env$fontSize))
  pkg.env$sizeText <- pkg.env$fontSize * (5/14)
  
  pkg.env$temaINE <- ggplot2::theme_gray(base_size = pkg.env$fontSize, base_family = "Open Sans Condensed Light")+ ggplot2::theme(
    text= ggplot2::element_text(family = "Open Sans Condensed Light", face = "plain", colour='black', size = pkg.env$fontSize),
    axis.text.x = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
    axis.text.y = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
    panel.background = ggplot2::element_rect(fill = NA),
    panel.grid = ggplot2::element_line(colour = NA),
    panel.grid.major = ggplot2::element_line(colour = NA),
    panel.grid.minor = ggplot2::element_line(colour = NA),
    panel.grid.major.y =  ggplot2::element_line(colour = NA),
    axis.line = ggplot2::element_line(colour = pkg.env$grisBase),
    plot.margin = rep(grid::unit(0,"null"),4),
    axis.ticks = ggplot2::element_line(colour = NA),
    axis.ticks.x = ggplot2::element_line( size=NULL, color=NA ),
    axis.ticks.y = ggplot2::element_line(size = NULL, color=NA),
    plot.background = ggplot2::element_rect(fill = NA, colour=NA),
    panel.border = ggplot2::element_rect(colour = NA, fill=NA),
    panel.ontop = TRUE
  )
  
  pkg.env$temaBarras <- pkg.env$temaINE
  pkg.env$temaBarras <- pkg.env$temaBarras + ggplot2::theme(
    axis.line.y = ggplot2::element_line(colour = NA),
    axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0.5),
    axis.line.x = ggplot2::element_line(colour = NA),
    plot.title = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9)
  ) 
  
  pkg.env$temaColumnas <- pkg.env$temaINE
  pkg.env$temaColumnas <- pkg.env$temaBarras + ggplot2::theme(
    axis.line.x = ggplot2::element_line(colour = NA),
    axis.line.y = ggplot2::element_line(colour = NA),
    axis.text.y = ggplot2::element_text(colour = NA)
  )
  
  pkg.env$temaFacets <- pkg.env$temaINE
  pkg.env$temaFacets <- pkg.env$temaFacets + ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = NA),
    axis.line.x = ggplot2::element_line(colour = NA),
    axis.line.y = ggplot2::element_line(colour = NA),
    axis.text.y = ggplot2::element_text(colour = NA),
    panel.margin = grid::unit(0.2,"cm"),
    #plot.margin = grid::unit(c(1,0,0,-1),"cm"),
    #strip.text = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =1.7, angle = 0, lineheight = 0.9),
    plot.margin = grid::unit(c(0,0,0,-1),"cm"),
    strip.text = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust = 1.5 , angle = 0, lineheight = 0.9)
  )
  
  
  pkg.env$temaAnillo <- pkg.env$temaINE
  pkg.env$temaAnillo <- pkg.env$temaAnillo +ggplot2::theme(
    plot.margin = grid::unit(c(0,inc2mm(3.19/4),0,-20),"mm"), axis.line.y = ggplot2::element_line(colour=NA),
    axis.ticks.y = ggplot2::element_line(colour=NA),
    axis.line.x = ggplot2::element_line(colour = NA),
    panel.margin = grid::unit(c(0,inc2mm(3.19/4),0,-20),"mm"),
    axis.text.y = ggplot2::element_text(colour = NA, vjust = -3, hjust = -3),
    axis.text.x = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = -10, vjust =-10, angle = 0, lineheight = 0.9)
  ) 
  
  
  
  
#   pkg.env$temaINE <- ggplot2::theme_gray(base_size = pkg.env$fontSize, base_family = "Open Sans Condensed Light")+ ggplot2::theme(
#     text= ggplot2::element_text(family = "Open Sans Condensed Light", face = "plain", colour='black', size = pkg.env$fontSize),
#     axis.text.x = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
#     axis.text.y = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
#     panel.background = ggplot2::element_rect(fill = NA),
#     panel.grid = ggplot2::element_line(colour = NA),
#     panel.grid.major = ggplot2::element_line(colour = NA),
#     panel.grid.minor = ggplot2::element_line(colour = NA),
#     panel.grid.major.y =  ggplot2::element_line(colour = NA),
#     axis.line = ggplot2::element_line(colour = pkg.env$grisBase),
#     plot.margin = rep(grid::unit(0,"null"),4),
#     axis.ticks = ggplot2::element_line(colour = NA),
#     axis.ticks.x = ggplot2::element_line( size=NULL, color=NA ),
#     axis.ticks.y = ggplot2::element_line(size = NULL, color=NA),
#     plot.background = ggplot2::element_rect(fill = NA)
#   )
#   
#   pkg.env$temaBarras <- pkg.env$temaINE
#   pkg.env$temaBarras <- pkg.env$temaBarras + ggplot2::theme(
#     axis.line.y = ggplot2::element_line(colour = "black"),
#     axis.line.x = ggplot2::element_line(colour = NA),
#     axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0.5)
#   ) 
#   
#   pkg.env$temaColumnas <- pkg.env$temaINE
#   pkg.env$temaColumnas <- pkg.env$temaBarras + ggplot2::theme(
#     axis.line.x = ggplot2::element_line(colour = NA),
#     axis.line.y = ggplot2::element_line(colour = NA),
#     axis.text.y = ggplot2::element_text(colour = NA)
#   )
}