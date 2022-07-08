#' Graficas de columnas por categorias
#' 
#' @param data El data frame para hacer la grafica, con el formato de tres o mas columnas de la forma y,z,w,...
#' @param etiquetasCategorias Indica la posición en las que se desea poner las etiquetas para las categorias.
#' Por defecto la posición es hacia Arriba, lo que se denota con la letra "A", cuando las etiquetas se desean en la parte
#' derecha de la grafica se debe indicar con la letra "D"
#' @param escala Indica la escala en la cual debe estar el eje y de la grafica. Por defecto se encuentra en normal. Las opciones
#' son "miles", "millones" o "milesmillones".
#' @param ruta Nombre de la ruta en la que se desea guardar la salida del archivo tex para su compilacion
#' @param etiquetas Posición de las etiquetas. Por defecto se ponen verticales, para pasar a horizontales de escribir "h"
#' @param preambulo Etiqueta boolean que indica si se desea que la gráfica tenga preámbulo o no. Por defecto se tiene Falso. 
#' @return No regresa ningun valor

graficaBalanza <- function(data, etiquetasCategorias = "A", escala = "normal", ruta, etiquetas = "v", ancho = 0.8, preambulo = F){
  ##ALGUNAS VARIABLES UTILES
  lonEtiqueta1 <- 0
  lonEtiqueta2 <- 0
  lonEtiqueta3 <- 0
  apoyoX <- 0
  separacion <- 0
  caso <- 0
  altoRect <- 0
  
  
  tikzDevice::tikz(ruta, standAlone = preambulo, bg = "transparent",bareBones = !preambulo, width = pkg.env$ancho, height= pkg.env$alto, sanitize= F)
  x <- rep(data$x,length(data)-1)
  y <- NULL
  for(i in 2:length(data)){
    y <- c(y,as.matrix(data)[,i])
  }
  categoria <- gl(length(data)-1, length(data$x), labels = names(data))
  dataLista <- data.frame(x,y,categoria)
  dataLista <- fact2Num(dataLista)
  if(toupper(escala) == "MILES"){
    dataLista$y <- dataLista$y/1000
  }else if(toupper(escala) == "MILLONES"){
    dataLista$y <- dataLista$y/1000000
  }else if(toupper(escala) == "MILESMILLONES"){
    dataLista$y <- dataLista$y/1000000000
  }
  
  
  colores <-   rampaColAgrupadas(dataLista)
  dataLista$x <- as.character(dataLista$x)
  ggplot2::theme_set(pkg.env$temaColumnas)
  grafica <- ggplot2::ggplot(dataLista, ggplot2::aes(x = x, y = y, fill = categoria))+
    ggplot2::geom_bar(stat = 'identity', position =  "dodge", width=ancho)+
    ggplot2::labs(x=NULL, y=NULL)+
    ggplot2::scale_y_continuous(breaks=NULL, expand = c(0,0))+
    ggplot2::geom_abline(intercept = 0, slope = 0)+
    #ggplot2::scale_x_discrete()+
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NULL),
      panel.background = ggplot2::element_rect(fill = NULL),
      plot.margin = grid::unit(c(0,0,0,0),"mm")
    )+
    ggplot2::scale_fill_manual(values=colores)+
    ggplot2::guides(fill = F)+
    ggplot2::geom_text(ggplot2::aes(familly = "Open Sans Condensed Light",label=formatC(y,format = "f",big.mark = ",", digits = 1)), position=ggplot2::position_dodge(width=0.5),size=3.2, angle = 90, hjust=-0.2, vjust = 0.5)
  
  
  
  ##Clasificando el caso.
  if( length(levels(dataLista$categoria)) -1 == 2 ){
    lonEtiqueta1 <- mm2inch(pt2mm(tikzDevice::getLatexStrWidth(names(data)[2], cex = 0.9)))
    lonEtiqueta2 <- mm2inch(pt2mm(tikzDevice::getLatexStrWidth(names(data)[3], cex = 0.9)))
    apoyoX <- 0
    separacion <- 0
    caso <- 0
    print(paste("La longitud de la etiqueta 1 en in es: ", lonEtiqueta1, sep = " "))
    print(paste("La longitud de la etiqueta 2 en in es: ", lonEtiqueta2, sep = " "))
    print( paste("La longitud 1 es: ", lonEtiqueta1 + mm2inch(3 - 0.5 * pkg.env$longCuadrado) + mm2inch(pkg.env$longCuadrado), sep = " ")  )
    print( paste("La longitud 2 es:", lonEtiqueta2 + mm2inch(3 - 0.5 * pkg.env$longCuadrado) + mm2inch(pkg.env$longCuadrado), sep = " ") )
    if( lonEtiqueta1 + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado) < 0.5 * pkg.env$ancho - pkg.env$tol && 
          lonEtiqueta2 + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado)  < 0.5 * pkg.env$ancho - pkg.env$tol  )
    {
      print("CASO 1")
      altoRect <- max(calcularAlto(names(data)[2], largo = inc2mm( pkg.env$ancho ) ), calcularAlto(names(data)[3], largo = inc2mm(pkg.env$ancho) ) )
      print(altoRect)
      caso <- 1
    }else if( 1.10 * (  lonEtiqueta1  + 2 * mm2inch(3 - 0.5 * pkg.env$longCuadrado) + 2 * mm2inch(pkg.env$longCuadrado) + lonEtiqueta2 ) <  pkg.env$ancho - 2 * pkg.env$tol){
      print("CASO 2")
      altoRect <- max(calcularAlto(names(data)[2], largo = inc2mm( pkg.env$ancho)/0.9 ), calcularAlto(names(data)[3], largo  = inc2mm(pkg.env$ancho)/0.9 ) )
      print(altoRect)
      caso <- 2
    }else {
      print(paste("No se cumple ninguna de las anteriores: ",  1.10 * (  lonEtiqueta1  + 2 * mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + 2 * mm2inch(pkg.env$longCuadrado) + lonEtiqueta2 ) , sep = " " ))
      print("CASO 3")
      altoRect <- max(calcularAlto(names(data)[2], largo = 0.5 * pkg.env$ancho - pkg.env$tol - mm2inch(3 - 0.5 * pkg.env$longCuadrado) - mm2inch( pkg.env$longCuadrado ) ), calcularAlto(names(data)[3], largo = 0.5 * pkg.env$ancho - pkg.env$tol - mm2inch(3 - 0.5 * pkg.env$longCuadrado) - mm2inch( pkg.env$longCuadrado ) ) )
      print(altoRect)
      caso <- 3  
    }
  }else{
    lonEtiqueta1 <- mm2inch(pt2mm(tikzDevice::getLatexStrWidth(names(data)[2], cex = 0.9)))
    lonEtiqueta2 <- mm2inch(pt2mm(tikzDevice::getLatexStrWidth(names(data)[3], cex = 0.9)))
    lonEtiqueta3 <- mm2inch(pt2mm(tikzDevice::getLatexStrWidth(names(data)[4], cex = 0.9)))
    
    altoRect <- max(calcularAlto(names(data)[2], largo = inc2mm( pkg.env$ancho/3) ), calcularAlto(names(data)[3], largo = inc2mm(pkg.env$ancho/3) ), calcularAlto(names(data)[4], largo = inc2mm(pkg.env$ancho/3) )  )
    print(altoRect)
  } 
  
  if ( toupper(etiquetas) == "V" ){
    max <-ggplot2::ggplot_build(grafica)$panel$ranges[[1]]$y.range[2]
    longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = 1), cex = pkg.env$fEscala)
    longitud <- pt2mm(longitud + altoRect) + 1.2 + 2 ## Se contempla la distancia de las barras a las etiquetas y de las etiquetas a la leyenda
    print(c(" La longitud en mm es: ", longitud))
    grafica <- grafica + ggplot2::theme(
      plot.margin = grid::unit(c(longitud,0,0,0),"mm")
    )
  }
  
  
  
  
  
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grafica))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid::grid.draw(temp)
  if(etiquetasCategorias == "D"){
    if(length(data$y) == 2){
      tikzDevice::tikzCoord(2*pkg.env$ancho/3,altoRect, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
      tikzDevice::tikzCoord(0,mm2inch(2.5+ 4), name = "desY", units= "inches")
      tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0+ 4), name = "desX", units = "inches")
      tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
      tikzDevice::tikzAnnotate("\\definecolor[named]{ct1}{rgb}{0.0,0.0,0.0}")
      tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect) + 0.5*(desX) + 0.5*(desY)$);")
      tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect)+0.5*(mdesX)-0.5*(desY)$);")
      tikzDevice::tikzAnnotate(c("\\draw [color=ct1] ($(rect)+(desY)$) rectangle ($(rect)+(desX)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t1,scale = 0.9]{", as.character(data$x[[1]]),"};"))
      tikzDevice::tikzAnnotate(c("\\path [fill=blue] ($(rect)-(desY)$) rectangle ($(rect)+(mdesX)$);"))
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
      tikzDevice::tikzAnnotate("\\definecolor[named]{ct1}{HTML}{000000}")
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct3}{HTML}{",substr(colores[3],2,7),"}"))
      tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect) +0.5*(lonX)$);")
      tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect)+ 0.5*(lonX) + (lonY) + (espacio) $);")
      tikzDevice::tikzAnnotate("\\coordinate (t3) at ($(rect) + 0.5*(lonX) - (lonY) - (espacio)$);")
      tikzDevice::tikzAnnotate(c("\\draw [color=ct1] ($(rect)+1.5*(lonY) + (espacio)$) rectangle ($(rect)+(lonX)+ 0.5*(lonY) + (espacio)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t1,scale = 0.9]{",as.character( (names(data))[2] ),"};"))
      tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ($(rect)+0.5*(lonY)$) rectangle ($(rect)+(lonX)-0.5*(lonY)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t2,scale = 0.9]{", as.character( (names(data))[3] ),"};"))
      tikzDevice::tikzAnnotate(c("\\path [fill=ct3] ($(rect)-1.5*(lonY) - (espacio)$) rectangle ($(rect)+(lonX)- 0.5*(lonY) - (espacio)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t3,scale = 0.9]{",as.character( (names(data))[4] ),"};"))
      
    }
  }else{
    if(length(levels(dataLista$categoria)) -1 == 2){
      
      
      
      ## Caluculando las posiciones de las etiquetas para que quede centrado
      cadenaEtiqueta1 <- ""
      cadenaEtiqueta2 <- ""
      if( caso == 1  )
      {
        print("CASO 1")
        print(paste("El punto medio para la primera etiqueta es: ", 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ), sep = " "))
        apoyoX  <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) -  0.5 * ( lonEtiqueta1  + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado) )
        finEtiqueta1 <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) +  0.5 * (  lonEtiqueta1  + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado) )
        print(paste("El fin de la etiqueta 1 es:" , finEtiqueta1, sep = " "))
        separacion <-  ( mm2inch(pkg.env$longCuadrado) + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + lonEtiqueta1 )  + ( 0.5 * pkg.env$ancho - finEtiqueta1 )  + 0.5 * ( 0.5 * pkg.env$ancho - pkg.env$tol - (  lonEtiqueta2  + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado) )  )  
        cadenaEtiqueta1 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,text width=",longEtiqueta1,",midway,right,scale = 0.9]{", as.character( names(data)[2] ),"};", sep = "")
        cadenaEtiqueta2 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,midway,right,scale = 0.9]{", as.character( names(data)[3] ),"};", sep = "")
      }else if( caso == 2 ){
        print("CASO 2")
        apoyoX <- ( 0.5 * pkg.env$ancho  + pkg.env$tol ) - 0.5 * 1.10 * ( lonEtiqueta1 + 2 * mm2inch(3 - 0.5 * pkg.env$longCuadrado ) + 2 * mm2inch(pkg.env$longCuadrado) + lonEtiqueta2 )
        separacion <-  mm2inch(pkg.env$longCuadrado) + mm2inch( 3 - 0.5 * pkg.env$longCuadrado )  + lonEtiqueta1   + 0.10 * ( lonEtiqueta1 + 2 * mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + 2 * mm2inch(pkg.env$longCuadrado) + lonEtiqueta2 ) 
        cadenaEtiqueta1 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,text width=",longEtiqueta1,",midway,right,scale = 0.9, draw]{", as.character( names(data)[2] ),"};", sep = "")
        cadenaEtiqueta2 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,midway,right,scale = 0.9]{", as.character( names(data)[3] ),"};", sep = "")
      }else {
        print("CASO 3")
        print(paste("El punto medio para la primera etiqueta es: ", 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ), sep = " "))
        apoyoX  <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) -  0.5 * ( lonEtiqueta1  + mm2inch(3) + mm2inch(pkg.env$longCuadrado) )
        finEtiqueta1 <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) +  0.5 * (  lonEtiqueta1  + mm2inch(3) + mm2inch(pkg.env$longCuadrado) )
        print(paste("El fin de la etiqueta 1 es:" , finEtiqueta1, sep = " "))
        separacion <- finEtiqueta1  + 0.5 * ( 0.5 * pkg.env$ancho - pkg.env$tol - (  lonEtiqueta2  + mm2inch(3) + mm2inch(pkg.env$longCuadrado) )  )
      }
      
      
      print(paste("El valor de apoyo es:" , apoyoX, sep = " "))
      print(paste("La separción es:" , separacion, sep = " "))
      print(paste("La tolerancia es:" , pkg.env$tol, sep = " "))
      print(paste("La distancia de un cuadro a otro es:" ,separacion + lonEtiqueta1, sep = " "))
      tikzDevice::tikzCoord(apoyoX, pkg.env$alto-mm2inch(pt2mm(altoRect)), name= "apoyo", units = "inches") ## ESTA ES LA QUE FUNCIONA 
      tikzDevice::tikzCoord(mm2inch(pkg.env$longCuadrado),mm2inch(pt2mm(altoRect)), name = "longitudFicticia", units= "inches")
      tikzDevice::tikzCoord(mm2inch(pkg.env$longCuadrado),mm2inch(pkg.env$longCuadrado), name = "longitud", units= "inches")
      tikzDevice::tikzCoord(separacion,mm2inch(0), name = "desX", units = "inches")
      tikzDevice::tikzCoord(mm2inch(0), 0.5* mm2inch(pt2mm(altoRect)) - 0.5*mm2inch(pkg.env$longCuadrado), name = "desY", units = "inches")
      #tikzDevice::tikzCoord(mm2inch(10),0, name = "mdesX", units = "inches")
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct1}{HTML}{",substr(colores[1],2,7),"}"))
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
      tikzDevice::tikzAnnotate(c("\\path [fill=none] (apoyo) rectangle ($(apoyo)+(longitudFicticia)$)"))
      tikzDevice::tikzAnnotate(cadenaEtiqueta1)
      tikzDevice::tikzAnnotate(c("\\path [fill=ct1] ( $(apoyo)  + (desY) $) rectangle ($(apoyo)+ (desY) +(longitud)$);"))
      tikzDevice::tikzAnnotate(c("\\path [fill=none] ($(apoyo)+(desX)$) rectangle ($(apoyo)+(desX)+(longitudFicticia)$)"))
      tikzDevice::tikzAnnotate(cadenaEtiqueta2)  
      tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ( $(apoyo)  + (desY) + (desX) $) rectangle ($(apoyo)+ (desY)+ (desX) +(longitud)$);"))
    }else{
      ##Definiendo los nodos necesarios:
      
      print(paste('La mitad de la etiqueta 1 vale: ' , 0.5 * ( lonEtiqueta1  + mm2inch(3) + mm2inch(pkg.env$longCuadrado) )))
      apoyoX1  <-  pkg.env$tol + 1/6 * ( pkg.env$ancho - 2 * pkg.env$tol)  -  0.5 * ( lonEtiqueta1  + mm2inch(3) + mm2inch(pkg.env$longCuadrado) ) + 0.1
      apoyoX2 <- pkg.env$tol + 1/2 * ( pkg.env$ancho - 2 * pkg.env$tol ) - 0.5 * ( lonEtiqueta2  + mm2inch(3) + mm2inch(pkg.env$longCuadrado) ) + 0.1
      apoyoX3 <- pkg.env$tol + 5/6 * ( pkg.env$ancho - 2 * pkg.env$tol ) - 0.5 * ( lonEtiqueta3  + mm2inch(3) + mm2inch(pkg.env$longCuadrado) ) + 0.1
      print(paste("El valor de apoyo es :" , apoyoX, sep = " "))
      
      ##Formateando el texto a mostrar
      cadenaEtiqueta1 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,text width=", lonEtiqueta1,",midway,right,scale = 0.9]{", as.character( names(data)[2] ),"};", sep = "")
      cadenaEtiqueta2 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,text width=", lonEtiqueta2, ",midway,right,scale = 0.9]{", as.character( names(data)[3] ),"};", sep = "")
      cadenaEtiqueta3 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,text width=", lonEtiqueta3, ",midway,right,scale = 0.9]{", as.character( names(data)[4] ),"};", sep = "")
      
      
      
      ##Escribiendo en el fichero TEX
      print(paste('El valor en Y de apoyoX es: ',mm2inch(pt2mm(altoRect)), sep = ' '))
      tikzDevice::tikzCoord(apoyoX1, pkg.env$alto-mm2inch(pt2mm(altoRect)), name= "apoyo1", units = "inches") ## ESTA ES LA QUE FUNCIONA 
      tikzDevice::tikzCoord(apoyoX2, pkg.env$alto-mm2inch(pt2mm(altoRect)), name= "apoyo2", units = "inches") ## ESTA ES LA QUE FUNCIONA 
      tikzDevice::tikzCoord(apoyoX3, pkg.env$alto-mm2inch(pt2mm(altoRect)), name= "apoyo3", units = "inches") ## ESTA ES LA QUE FUNCIONA 
      tikzDevice::tikzCoord(mm2inch(pkg.env$longCuadrado),mm2inch(pt2mm(altoRect)), name = "longitudFicticia", units= "inches")
      tikzDevice::tikzCoord(mm2inch(pkg.env$longCuadrado),mm2inch(pkg.env$longCuadrado), name = "longitud", units= "inches")
      tikzDevice::tikzCoord(mm2inch(0), 0.5* mm2inch(pt2mm(altoRect)) - 0.5*mm2inch(pkg.env$longCuadrado), name = "desY", units = "inches")
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct1}{HTML}{",substr(colores[1],2,7),"}"))
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct3}{HTML}{",substr(colores[3],2,7),"}"))
      tikzDevice::tikzAnnotate(c("\\path [fill=none] (apoyo1) rectangle ($(apoyo1)+(longitudFicticia)$)"))
      tikzDevice::tikzAnnotate(cadenaEtiqueta1)
      tikzDevice::tikzAnnotate(c("\\path [fill=ct1] ( $(apoyo1)  + (desY) $) rectangle ($(apoyo1)+ (desY) +(longitud)$);"))
      tikzDevice::tikzAnnotate(c("\\path [fill=none] (apoyo2) rectangle ($(apoyo2)+(longitudFicticia)$)"))
      tikzDevice::tikzAnnotate(cadenaEtiqueta2)
      tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ( $(apoyo2)  + (desY) $) rectangle ($(apoyo2)+ (desY) +(longitud)$);"))
      tikzDevice::tikzAnnotate(c("\\path [fill=none] (apoyo3) rectangle ($(apoyo3)+(longitudFicticia)$)"))
      tikzDevice::tikzAnnotate(cadenaEtiqueta3)
      tikzDevice::tikzAnnotate(c("\\path [fill=ct3] ( $(apoyo3)  + (desY) $) rectangle ($(apoyo3)+ (desY) +(longitud)$);"))
      
    }
  }
  grDevices::dev.off()
}