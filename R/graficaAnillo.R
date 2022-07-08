#' Hace graficas de anillo para dos y tres categorias
#' 
#' @param data Data frame con el cual se hace el anillo
#' @param nombre Ruta en la cual se desea generar el tex
#' @param preambulo Booleano que cuando se le indica verdadero le pone todo el preambulo para compilar la gráfica como un documento
#' por sí mismo y con falso deja el código para ser incluido en un documento más grande. 
#' @return No retorna
#' @export
graficaAnillo <- function(data, nombre, preambulo = T)
{
  colorBorde <- NULL
  if ( pkg.env$modalidad == "trimestral"){
    colorBorde <- rgb(0,0,0)
  }else{
    colorBorde <- rgb(1,1,1)
  }
  names(data)<- c("x","y")
  data <- data[ordenarNiveles(data),]
  data$x <- factor(data$x, levels = data$x)
  data$x <- factor(data$x, as.character(data$x))
  tikzDevice::tikz(nombre, standAlone = preambulo, bg = "transparent",bareBones = !preambulo , width = pkg.env$ancho , height= pkg.env$alto, sanitize= F)
  ggplot2::theme_set(pkg.env$temaAnillo)
  data$ymax = cumsum(data$y)
  data$ymin = c(0, head(data$ymax, n=-1))
  y.breaks <- cumsum(data$y)-data$y/2
  colores <- calcularRampaAnillo(data$x, categoria = F)
  data$y <- round(data$y,1)
  p1 <- ggplot2::ggplot(data, ggplot2::aes(fill =  x, ymax = ymax, ymin = ymin ,xmax= 10, xmin= 5))+
    #geom_rect(show_guide=F)+
    ggplot2::geom_rect(fill = colores ,colour= colorBorde, show_guide=F)+
    ggplot2::labs(x=NULL, y=NULL)+
    ggplot2::scale_y_continuous(breaks = y.breaks, labels=data$y, expand = c(0,0))+
    ggplot2::labs(x = NULL, y=NULL)+
    ggplot2::coord_polar(theta ="y")+
    ggplot2::xlim(c(0,10 ))+
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p1))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid::grid.draw(temp)
  
  colorBorde <- NULL
  if ( pkg.env$modalidad == "trimestral"){
    colorBorde <- rgb(1,1,1)
    colores[1] <- pkg.env$color1
    print(pkg.env$color1)
    print(colores[1])
  }else{
    colorBorde <- pkg.env$color1
  }
  
  if(length(data$y) == 2){
    tikzDevice::tikzCoord(2*pkg.env$ancho/3, pkg.env$alto/2, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
    tikzDevice::tikzCoord(0,mm2inch(2.5+ 4), name = "desY", units= "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0+ 4), name = "desX", units = "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct1}{HTML}{",substr(colores[1],2,7),"}"))
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{borde}{HTML}{",substr(colorBorde,2,7),"}"))
    tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect) + 0.5*(desX) + 0.5*(desY)$);")
    tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect)+0.5*(mdesX)-0.5*(desY)$);")
    tikzDevice::tikzAnnotate(c("\\draw [color=ct1,fill=borde] ($(rect)+(desY)$) rectangle ($(rect)+(desX)$);"))
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                               mm2pt(20), 
                               ",right= 0.3cm of t1,scale = 0.9]{", as.character(data$x[[1]]),"};"))
    tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ($(rect)-(desY)$) rectangle ($(rect)+(mdesX)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                               mm2pt(20), 
                               ",right= 0.3cm of t2,scale = 0.9]{",as.character(data$x[[2]]),"};"))  
  }else{
    tikzDevice::tikzCoord(2*pkg.env$ancho/3, pkg.env$alto/2, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
    tikzDevice::tikzCoord(0,mm2inch(1.25 + 0), name = "desY", units= "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0-1.25), name = "desX", units = "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
    tikzDevice::tikzCoord(mm2inch(1.25),0, name="tdesX", units = "inches")
    tikzDevice::tikzCoord(0,mm2inch(6+1.25), name ="tdesY", units = "inches")
    tikzDevice::tikzCoord(0,mm2inch(6), name = "espacio", units = "inches")
    tikzDevice::tikzCoord(0, mm2inch(2.5), name = "lonY", units = "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),0, name = "lonX", units = "inches")
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{borde}{HTML}{",substr(colorBorde,2,7),"}"))
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct1}{HTML}{",substr(colores[1],2,7),"}"))
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct3}{HTML}{",substr(colores[3],2,7),"}"))
    tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect) +0.5*(lonX)$);")
    tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect)+ 0.5*(lonX) + (lonY) + (espacio) $);")
    tikzDevice::tikzAnnotate("\\coordinate (t3) at ($(rect) + 0.5*(lonX) - (lonY) - (espacio)$);")
    tikzDevice::tikzAnnotate(c("\\draw [color=ct1,fill = borde] ($(rect)+1.5*(lonY) + (espacio)$) rectangle ($(rect)+(lonX)+ 0.5*(lonY) + (espacio)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                               mm2pt(20), 
                               ",right= 0.3cm of t1,scale = 0.9]{",as.character(data$x[[1]]),"};"))
    tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ($(rect)+0.5*(lonY)$) rectangle ($(rect)+(lonX)-0.5*(lonY)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                               mm2pt(20), 
                               ",right= 0.3cm of t2,scale = 0.9]{", as.character(data$x[[2]]),"};"))
    tikzDevice::tikzAnnotate(c("\\path [fill=ct3] ($(rect)-1.5*(lonY) - (espacio)$) rectangle ($(rect)+(lonX)- 0.5*(lonY) - (espacio)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                               mm2pt(20), 
                               ",right= 0.3cm of t3,scale = 0.9]{",as.character(data$x[[3]]),"};"))
    
  }
  dev.off()
}