#' Funcion basica para graficar Columnas
#' @param data Data frame para elaborar la grafica
#' @param color1 Color con el que se desea hacer la grafica
#' @param color2 Color secundario para las graficas
#' @param ancho Porcentaje que ocupan las columnas, segun paquete ggplot2
#' @param ordenar Booleano que indica si los datos deben ser ordenados
#' @param escala Indica la escala en la cual debe estar el eje y de la grafica. Por defecto se encuentra en normal. Las opciones
#' son "miles", "millones" o "milesmillones".
#' @return La gráfica lista para agregarle etiquetas y toques finales 
#' @export
graficaCol <- function(data, color1=pkg.env$color1, ancho = 0.6, ordenar = TRUE, escala = "normal")
{
  
  ggplot2::theme_set(pkg.env$temaColumnas)
  names(data)<- c("x","y")
  data <- data[ordenarNiveles(data, ordenar),]
  data$x <- factor(data$x, levels = data$x)
  levels(data$x) <- gsub("\\\\n", "\n", levels(data$x))
  print(levels(data$x))
  
  
  ##Poniendo la escala correspondiente
  if(toupper(escala) == "MILES"){
    data$y <- data$y/1000
  }else if(toupper(escala) == "MILLONES"){
    data$y <- data$y/1000000
  }else if(toupper(escala) == "MILESMILLONES"){
    data$y <- data$y/1000000000
  }
  
  numeroCol <- nrow(data)
  
  if( numeroCol == 3 ){
    ancho <- 0.4
  }else if( numeroCol == 4){
    ancho <- 0.5
  }else if( numeroCol == 5){
    ancho <- 0.55
  }
  print(ancho)
  print(data$x)
  print(cortarEtiquetas(data$x))
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x, y))
  grafica <- grafica + 
    ggplot2::geom_bar(stat = 'identity', colour = calcularRampa(data, color1), fill = calcularRampa(data,pkg.env$colorRelleno), width = ancho, position =  "dodge")+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::scale_y_continuous(breaks=NULL)+
    ggplot2::scale_x_discrete(breaks =  unique(data$x), labels = cortarEtiquetas(data$x))+
    ggplot2::geom_abline(intercept = 0, slope = 0, size = 0.1)
  

  return(grafica)
}








#'Hace graficas de Barras
#'@param data Data frame con el cual se hace la grafica
#'@param color1 El color principal para la grafica
#'@param ancho Ancho de las barras en porcentaje, segun ggplot2
#'@param ordenar Booelano que indica si los datos deben ser ordenados o no
#'@param escala Indica la escala en la cual debe estar el eje y de la grafica. Por defecto se encuentra en normal. Las opciones
#' son "miles", "millones" o "milesmillones".
graficaBar <- function(data, color1=pkg.env$color1, ancho = 0.6, ordenar = TRUE, escala = "normal")
{
  ggplot2::theme_set(pkg.env$temaBarras)
  names(data)<- c("x","y")
  data <- data[rev(ordenarNiveles(data, ordenar)),]
  data$x <- factor(data$x, levels = data$x)
  levels(data$x) <- gsub("\\n", "\n", levels(data$x))
  
  ##Poniendo la escala correspondiente
  if(toupper(escala) == "MILES"){
    data$y <- data$y/1000
  }else if(toupper(escala) == "MILLONES"){
    data$y <- data$y/1000000
  }else if(toupper(escala) == "MILESMILLONES"){
    data$y <- data$y/1000000000
  }
  
  
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x, y))
  grafica <- grafica + 
    ggplot2::geom_bar(stat = 'identity',fill = calcularRampa(data, pkg.env$colorRelleno), colour = calcularRampa(data, color1), width = ancho, position =  "dodge")+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::geom_abline(intercept = 0, slope = 0, size = 0.1)+
    ggplot2::scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))+
    #intento de partir las etiquetas del eje
    #ggplot2::scale_x_discrete(labels = function(x) strwrap(x,width = 30)) + 
    ggplot2::theme(
      axis.line.y = ggplot2::element_line(colour = NA)
    ) +
    ggplot2::coord_flip()
  


  return(grafica)
}

#'Hace una grafica de linea, util para series historicas
#'
#'@param data El data frame para hacer la gráfica
#'@param color1 El color en el que se desea la linea
#'@param inicio El dato desde donde se quiere que se visualice la gráfica
#'@param ancho El grosor de la linea
#' @param escala Indica la escala en la cual debe estar el eje y de la grafica. Por defecto se encuentra en normal. Las opciones
#' son "miles", "millones" o "milesmillones".
#'@param precision Se refiere al número de decimales que se desean mostrar en la gráfica. Por defecto se usa
#'un decimal.
#'@export

graficaLinea <- function(data, color1 = pkg.env$color1, inicio = -1, ancho = 1.5, precision=1, escala = "normal", rotar = T, final = NA)
{
  pkg.env$precision <- precision
  print("El tamaño de la fuente es: ")
  print(pkg.env$fontSize)
  ggplot2::theme_set(pkg.env$temaColumnas)
  names(data)<- c("x","y")
  
  ##Poniendo la escala correspondiente
  if(toupper(escala) == "MILES"){
    data$y <- data$y/1000
  }else if(toupper(escala) == "MILLONES"){
    data$y <- data$y/1000000
  }else if(toupper(escala) == "MILESMILLONES"){
    data$y <- data$y/1000000000
  }
  
  ##Fijando los niveles para que R no los cambie
  data$x <- factor(data$x, levels = data$x)
  
  
  ## Cambiando el ancho cuando es trimestral
  if (pkg.env$modalidad == "trimestral" ){
    ancho <- 0.5
  }else if(pkg.env$modalidad =="presentacion"){
    print('Como presentacion')
    ancho <- 0.8
  }
  print("La modalidad es: \n")
  print(pkg.env$modalidad)
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x,y, group=1))
  grafica <- grafica + ggplot2::geom_line( colour = color1, size = ancho)+
    ggplot2::labs(x=NULL,y=NULL)
  grafica <- etiquetasLineas(grafica, calcularPosiciones(grafica), precision = pkg.env$precision)
  margenArriba <- pt2mm(calcularAlto(10))
  ## Rotanto las etiquetas del eje x cuando la modalidad es trimestral
  
  if(pkg.env$modalidad == "trimestral" || rotar == T){
    grafica <- grafica + ggplot2::theme(axis.text.x = ggplot2::element_text(family = pkg.env$fuente,angle = 90, vjust =0.5 , hjust= 1))
  }
  
  minimo <- min(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  maximo <- max(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  
  if (inicio == -1){
    limite <- minimo - 
      0.3*(maximo - minimo)  
  }else{
    limite <- inicio
  }
  
  
  print(c('El límite es: ', limite))
  grafica <- grafica + ggplot2::geom_abline(intercept = limite, slope = 0, size = 0.1
                                            )
  if(ggplot2::ggplot_build(grafica)$data[[1]]$y[1] > 3)
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,final))+
      ggplot2::theme(plot.margin = grid::unit(c(margenArriba,3,1,-5), "mm"))
  }
  else
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,final))+
      ggplot2::theme(plot.margin = grid::unit(c(margenArriba,3,1,-2), "mm"))
  }
  return(grafica)
}

#'Genera graficas de dobles lineas en un mismo panel.
#'
#'@param data El data frame para hacer la gráfica
#'@param color1 El color en el que se desea la linea
#'@param inicio El dato desde donde se quiere que se visualice la gráfica
#'@param fin El dato hasta donde se desea visualizar la gráfica
#'@param ancho El grosor de la linea
#' @param escala Indica la escala en la cual debe estar el eje y de la grafica. Por defecto se encuentra en normal. Las opciones
#' son "miles", "millones" o "milesmillones".
#' @param ruta Es la ruta en disco donde se desea almacenar la grafica
#' @param preambulo Booleano que indica si una grafica debe llevar preambulo o no. 
#'@param precision Se refiere al número de decimales que se desean mostrar en la gráfica. Por defecto se usa un decimal.
#'@param ejeX Parámetro de tipo cadena que determina la orientación de las etiquetas para el eje X. Las opciones son "h" para horizontal y
#'"v" para vertical. Por defecto se usa horizontal.
#'@export

graficaDobleLinea <- function(data, ruta, preambulo = F, color1 = pkg.env$color1, color2 = pkg.env$color2, ancho = 1.5, precision=1, escala = "normal", inicio = 0, fin = 0, ejeX = "h")
{
  ##ALGUNAS VARIABLES UTILES
  lonEtiqueta1 <- 0
  lonEtiqueta2 <- 0
  lonEtiqueta3 <- 0
  apoyoX <- 0
  separacion <- 0
  caso <- 0
  altoRect <- 0
  
  
  tikzDevice::tikz(ruta, standAlone = preambulo, bg = "transparent",bareBones = !preambulo, width = pkg.env$ancho, height= pkg.env$alto, sanitize= F)
  
  ggplot2::theme_set(pkg.env$temaColumnas)
  nombres <- names(data)
  names(data)<- c("x","y","z")
  
  
  ##Poniendo la escala correspondiente
  if(toupper(escala) == "MILES"){
    data$y <- data$y/1000
    data$z <- data$z/1000
  }else if(toupper(escala) == "MILLONES"){
    data$y <- data$y/1000000
    data$z <- data$z/1000000
  }else if(toupper(escala) == "MILESMILLONES"){
    data$y <- data$y/1000000000
    data$z <- data$z/1000000000
  }
  
  ##Fijando los niveles para que R no los cambie
  data$x <- factor(data$x, levels = data$x)
  
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x,group=1))
  grafica <- grafica + ggplot2::geom_line( ggplot2::aes(y = y), size = ancho, colour = color1)+
  ggplot2::geom_line( ggplot2::aes(y = z), size = ancho, colour = color2 )+
    ggplot2::labs(x=NULL,y=NULL)
  grafica <- etiquetasLineasDobles(grafica, calcularPosicionesDobles(grafica), precision = precision)
  
  
  ## Rotando el eje X si fuera necesario
  if ( ejeX == "v"){
    grafica <- grafica + ggplot2::theme(axis.text.x = ggplot2::element_text(family = pkg.env$fuente,angle = 90, vjust =0.5 , hjust= 1))
  }
  
  
  
  ##Estableciendo los límites para las gráficas
  if ( inicio == 0 && fin == 0){
    minimo <- min(ggplot2::ggplot_build(grafica)$data[[1]]$y)
    maximo <- max(ggplot2::ggplot_build(grafica)$data[[1]]$y)
    limite <- minimo - 
      0.3*(maximo - minimo)
    limiteFin <- NA
    print("El límite es: ")
    print(limite)
  }else{
    limite <- inicio
    if ( fin == 0){
      limiteFin <- NA
    }else{
      limiteFin <- fin
    }
      
  }
  
  
  grafica <- grafica + ggplot2::geom_abline(intercept = limite, slope = 0, size = 0.1)
  if(ggplot2::ggplot_build(grafica)$data[[1]]$y[1] > 3)
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,limiteFin))+
      ggplot2::theme(plot.margin = grid::unit(c(2.5,3,0,-5), "mm"))
  }
  else
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,limiteFin))+
      ggplot2::theme(plot.margin = grid::unit(c(2.5,3,0,-2), "mm"))
  }
  
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grafica))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid::grid.draw(temp)
  
  ## Eligiendo los colores
  rampaAux = grDevices::colorRampPalette( c(pkg.env$color1, pkg.env$color2 ) )
  col <- list(rampaAux(2),rampaAux(2))
  colores <- col[[1]]
  coloresBorde <- col[[2]]
  
  
  ## Haciendo el etiquetado
  names(data) <- nombres
  
  lonEtiqueta1 <- mm2inch(pt2mm(tikzDevice::getLatexStrWidth(names(data)[2], cex = 1)))
  lonEtiqueta2 <- mm2inch(pt2mm(tikzDevice::getLatexStrWidth(names(data)[3], cex = 1)))
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
  
  
  ## Caluculando las posiciones de las etiquetas para que quede centrado
  cadenaEtiqueta1 <- ""
  cadenaEtiqueta2 <- ""
  if( caso == 1  )
  {
    print("CASO 1")
    print(paste("El punto medio para la primera etiqueta es: ", 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ), sep = " "))
    apoyoX  <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) -  0.5 * ( lonEtiqueta1  + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado ) )
    finEtiqueta1 <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) +  0.5 * (  lonEtiqueta1  + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado) )
    print(paste("El fin de la etiqueta 1 es:" , finEtiqueta1, sep = " "))
    separacion <-  ( mm2inch(pkg.env$longCuadrado) + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + lonEtiqueta1 )  + ( 0.5 * pkg.env$ancho - finEtiqueta1 )  + 0.5 * ( 0.5 * pkg.env$ancho - pkg.env$tol - (  lonEtiqueta2  + mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + mm2inch(pkg.env$longCuadrado) )  )  
    cadenaEtiqueta1 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,midway,right,scale = 1]{", as.character( names(data)[2] ),"};", sep = "")
    cadenaEtiqueta2 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,midway,right,scale = 1]{", as.character( names(data)[3] ),"};", sep = "")
  }else if( caso == 2 ){
    print("CASO 2")
    apoyoX <- ( 0.5 * pkg.env$ancho  + pkg.env$tol ) - 0.5 * 1.10 * ( lonEtiqueta1 + 2 * mm2inch(3 - 0.5 * pkg.env$longCuadrado ) + 2 * mm2inch(pkg.env$longCuadrado) + lonEtiqueta2 )
    separacion <-  mm2inch(pkg.env$longCuadrado) + mm2inch( 3 - 0.5 * pkg.env$longCuadrado )  + lonEtiqueta1   + 0.10 * ( lonEtiqueta1 + 2 * mm2inch( 3 - 0.5 * pkg.env$longCuadrado ) + 2 * mm2inch(pkg.env$longCuadrado) + lonEtiqueta2 ) 
    cadenaEtiqueta1 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,midway,right,scale = 1, draw]{", as.character( names(data)[2] ),"};", sep = "")
    cadenaEtiqueta2 <- paste("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,midway,right,scale = 1]{", as.character( names(data)[3] ),"};", sep = "")
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
  tikzDevice::tikzAnnotate(c("\\definecolor[named]{ctb1}{HTML}{",substr(coloresBorde[1],2,7),"}"))
  tikzDevice::tikzAnnotate(c("\\definecolor[named]{ctb2}{HTML}{",substr(coloresBorde[2],2,7),"}"))
  tikzDevice::tikzAnnotate(c("\\path [fill=none] (apoyo) rectangle ($(apoyo)+(longitudFicticia)$)"))
  tikzDevice::tikzAnnotate(cadenaEtiqueta1)
  tikzDevice::tikzAnnotate(c("\\draw [color = ctb1,fill=ct1] ( $(apoyo)  + (desY) $) rectangle ($(apoyo)+ (desY) +(longitud)$);"))
  tikzDevice::tikzAnnotate(c("\\path [fill=none] ($(apoyo)+(desX)$) rectangle ($(apoyo)+(desX)+(longitudFicticia)$)"))
  tikzDevice::tikzAnnotate(cadenaEtiqueta2)  
  tikzDevice::tikzAnnotate(c("\\draw [color = ctb2 ,fill=ct2] ( $(apoyo)  + (desY) + (desX) $) rectangle ($(apoyo)+ (desY)+ (desX) +(longitud)$);"))
  
  grDevices::dev.off()
  
}

#'Esta función desaparecerá en futuras versiones, deberá hacer uso 
#'de la función graficaLinea en la modalidad trimestral. 
#'Genera graficas de lineas para series historicas de los trimestrales
#'
#'@param data El data frame con el que se hará la grafica
#'@param color1 El color en el que se desea la linea
#'@param inicio El punto en el eje y a partir del cual se desea mostrar la grafica
#'@param El ancho de la linea
#'@param precision Indica el número de decimales con el que se desea ver la etiqueta. Por defecto es uno. 
#'@return El objeto ggplot2 listo para grafica
#'@export
graficaLineaTrim <- function(data, color1 = color, inicio = 0, ancho = 0.5, precision=1)
{
  ggplot2::theme_set(pkg.env$temaColumnas)
  names(data)<- c("x","y")
  nomX <- data$x
  data$x <- factor(data$x, as.character(data$x))
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x,y, group = 1))
  grafica <- grafica + ggplot2::geom_line( colour = pkg.env$color1, size = ancho)+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(family = pkg.env$fuente,angle = 90, vjust =0.5 , hjust= 1))
  grafica <-  etiquetasLineas(grafica, calcularPosiciones(grafica), precision = precision)
  minimo <- min(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  maximo <- max(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  limite <- minimo - 0.5*(maximo - minimo)
  grafica <- grafica + ggplot2::geom_abline(intercept = limite, slope = 0, size = 0.1)
  if(ggplot2::ggplot_build(grafica)$data[[1]]$y[1] > 3)
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,NA))+
      ggplot2::theme(plot.margin = grid::unit(c(2.5,3,0,-7), "mm")) #-9
  }
  else
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,NA))+
      ggplot2::theme(plot.margin = grid::unit(c(2.5,3,0,-3), "mm")) #-4
  }
  return(grafica)
}

#'Función para hacer gráficas piramidales de población
#'@param data Es el data frame con el que se hará la gráfica
#'@param ancho Gradua el ancho de las Barras. Por defecto es 0.6
#'@param color1 El color con el que se desea hacer la gráfica
#'@param ruta La ruta completa de donde se desea almacenar la gráfica
#'@param digitos El número de dígitos que se usa para los decimales


piramidePoblacional <- function(data,ancho = 0.6 , escala = "normal", color1 = pkg.env$color1, ruta, preambulo = F, digitos = 1, porcentual = T){
  nombres <- names(data)
  if( toupper( names(data)[2] ) == 'HOMBRES' || toupper( names(data)[2] ) == 'HOMBRE'){
    temp <- data[3]
    data[3] <- data[2]
    data[2] <- temp
  }
  
  names(data)<- c("x","y","z")
  ##Calculando los porcentajes
  
  if (porcentual == T){
    poblacion <- sum(data$y, data$z)
    data$y <- data$y / poblacion * 100
    data$z <- data$z / poblacion * 100  
  }
  
  
  
  print(data)
  
  ggplot2::theme_set(pkg.env$temaBarras)

  
  ## Graduando el ancho
  numeroCol <- nrow(data)
  
  if( numeroCol == 3 ){
    ancho <- 0.4
  }else if( numeroCol == 4){
    ancho <- 0.5
  }else if( numeroCol == 5){
    ancho <- 0.55
  }
  print(ancho)
  

  etiquetaMaxima <- tikzDevice::getLatexStrWidth(max(data$x))
  etiquetaMaxima <- pt2mm(etiquetaMaxima)
  print(c("La etiqueta maxima mide ",  etiquetaMaxima))
  pkg.env$enteros <- sonEnteros( data )
  pkg.env$digitos <- digitos
  
  
  data$x <- factor(data$x, levels = data$x)
  levels(data$x) <- gsub("\\\\n", "\n", levels(data$x))
  print(levels(data$x))  
  
  
  if ( pkg.env$modalidad == "trimestral"){
    color1 <- pkg.env$color1
    pkg.env$color1 <- pkg.env$color2
    pkg.env$color2 <- rgb(0.9,0.9,0.9)
    relleno <- pkg.env$colorRelleno 
    pkg.env$colorRelleno <- pkg.env$colorRelleno2
    pkg.env$colorRelleno2 <- rgb(0.9,0.9,0.9)
  }
  
  
  print(data$z)
  maximo1 <- max( data$z )
  print(data$y)
  maximo2 <- max( data$y )
  maximo <- max(c(maximo1, maximo2))
  print(c("El maximo para la escala es: ", maximo))
  
  print(c("El maximo en z es: ", maximo1))
  longitudy <- tikzDevice::getLatexStrWidth(trimws( formatC(maximo2,format = "f",big.mark = ",", digits = pkg.env$digitos, drop0trailing = pkg.env$enteros) ), cex = pkg.env$fEscala)  
  longitudy <- pt2mm(longitudy) + 2
  print(longitudy)
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x,z,y))
  grafica.y <- grafica +    
    ggplot2::geom_bar(ggplot2::aes(x,y = z), stat = 'identity',fill = calcularRampa(data, pkg.env$colorRelleno), colour = calcularRampa(data, pkg.env$color1), width = ancho, position =  "dodge")+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::scale_y_reverse(breaks=NULL,limits = c(maximo,NA), expand= c(0.0,0.0) )+
    #ggplot2::scale_x_discrete(breaks=NULL)+
    ggplot2::geom_text(ggplot2::aes(family = pkg.env$fuente,label= formatC(z,format = "f",big.mark = ",", digits = pkg.env$digitos,drop0trailing = pkg.env$enteros)),size=pkg.env$sizeText, hjust=1.2, vjust = 0.5)+
    ggplot2::theme(
      #axis.ticks.margin = grid::unit(c(0,longitudy),"mm"),
      #axis.ticks.margin = grid::unit(c(-20, 10),'mm'),
      axis.line.y = ggplot2::element_line(colour = NA),
      #axis.text.y = ggplot2::element_text(colour = NA),
      axis.line.x = ggplot2::element_line(colour = NA),
      plot.margin = grid::unit(c(0,-7.3,-longitudy,0), "mm")
      #plot.margin = grid::unit(c(0,-10,10,0), "mm")
    )+
    ggplot2::coord_flip()+ggplot2::ggtitle("Hombres")
  
  print(grafica.y)
  
  tempy<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grafica.y))
  tempy$layout$clip[tempy$layout$name=="panel"] <- "off"
  
  
  
  
  print(c("El maximo en y es: ", maximo2))
  longitud <- tikzDevice::getLatexStrWidth(formatC(maximo2,format = "f",big.mark = ",", digits = pkg.env$digitos, drop0trailing = pkg.env$enteros), cex = pkg.env$fEscala) 
  longitud <- pt2mm(longitud) + 3
  
    
  grafica.x <- grafica  +
    ggplot2::geom_bar(ggplot2::aes(x,y = y), stat = 'identity',fill = calcularRampa(data, pkg.env$colorRelleno2), colour = calcularRampa(data, pkg.env$color2), width = ancho, position =  "dodge")+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::scale_y_continuous(breaks=NULL,  limits = c(NA,maximo ), expand= c(0.0,0.0))+
    ggplot2::scale_x_discrete(breaks=NULL)+
    #ggplot2::geom_abline(intercept = 0, slope = 0, size = 0.1, ggplot2::aes(colour = "gray"))+
    ggplot2::geom_text(ggplot2::aes(x, y=y, family = pkg.env$fuente,label= formatC(y,format = "f",big.mark = ",", digits = pkg.env$digitos, drop0trailing = pkg.env$enteros)),size=pkg.env$sizeText, hjust=-0.2, vjust = 0.5)+
    ggplot2::theme(
      axis.text = ggplot2::element_text(family = pkg.env$fuente,vjust =0.5 , hjust= 0.5, margin = grid::unit(c(0,longitudy),"mm")),
       axis.ticks.margin=grid::unit(c(-2.5),'mm'),
       axis.text.y = ggplot2::element_text(family = pkg.env$fuente,vjust =0.5 , hjust= 0.5),
      axis.line.y = ggplot2::element_line(colour = NA),
      plot.margin = grid::unit(c(0,longitud,-longitudy,etiquetaMaxima-longitudy-2.9), "mm")
      #plot.margin = grid::unit(c(0,longitud,-longitudy,etiquetaMaxima-longitudy-2.9), "mm")  
      )+
    ggplot2::coord_flip()+ ggplot2::ggtitle("Mujeres")
  
  
  print(grafica.x)
  tempx<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grafica.x))
  tempx$layout$clip[tempx$layout$name=="panel"] <- "off"
  



  
  
#   
  tikzDevice::tikz(ruta, standAlone = preambulo, bareBones = TRUE, bg = "transparent", width = pkg.env$ancho, height= pkg.env$alto, sanitize = F)
  gr <- gridExtra::grid.arrange(tempy, tempx, 
                              widths = c(1,1),
                              ncol =2)
  dev.off()

if ( pkg.env$modalidad == "trimestral"){
  trimestral()
  }
  return(data)
  }

