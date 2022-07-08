#' Hace una paleta del color indicado
#' 
#' @param data El data frame con la informacion
#' @param color1 El color primario con el cual se desea hacer la paleta
#' @param color2 El color secundario que va cuando encuentra palabras clave
#' @return Vector con la rampa de colores
calcularRampa <- function(data, color1= pkg.env$color1, color2 = pkg.env$color2)
{
  rampa = NULL
  for(elemento in data$x)
  {
    if(elemento %in% pkg.env$ignorado)
    {
      rampa = c(rampa,pkg.env$gris)
    }
    else if(elemento %in% pkg.env$repu)
    {
      rampa = c(rampa, color2)
    }
    else
    {
      rampa = c(rampa,color1)
    }
  }
  return(rampa)
}

#' Funcion de uso interno para ordenar los niveles según los datos
#' @param data El data frame con el cual se desean hacer los calculos
#' @param ordenar Booleno que indica si se deben ordenar los datos de mayor a menor
#' @return Vector con los valores en el orden deseado
ordenarNiveles <- function(data, ordenar = TRUE)
{
  nuevoOrden <- NULL
  ignNombre <- NULL
  ign = 0
  orden <- NULL
  if(ordenar)
  {
    orden <- order(data$y, decreasing = T)
    
  }
  else
  {
    for(i in (1:length(data$x)))
      orden <- c(orden,i)
  }
  for(elemento in orden)
  {
    if( data[elemento,]$x %in% pkg.env$ignorado)
    {
      ign = 1
      pos <- elemento
    }
    else{
      nuevoOrden <- c(nuevoOrden, elemento)
    }
  }
  
  if(ign == 1)
  {
    nuevoOrden <- c(nuevoOrden, pos)
  }
  
  return(nuevoOrden)
}


#'Función para ordenar los niveles de un data frame 
#'excluyendo ciertas palabras claves, como ignorado y otros. 
#'Se puede hacer personalizable eligiendo los identificadores de los
#'niveles que se desean excluir.
#'@param data El data frame con el que se desea trabajar
#'@param palabras Vector de palabras claves que se desean excluir,
#'@return Data frame ordenado y con las exclusiones de los niveles

excluirNiveles <- function(data, palabras = pkg.env$exclusion){
  temp <- NULL
  orden <- NULL
  orden <- order(data$y, decreasing = T)
  nuevoOrden <- NULL
  for(elemento in orden)
  {
    if( !(tolower(data[elemento,]$x) %in% palabras) )
    {
      nuevoOrden <- c(nuevoOrden, elemento)
    }
  }  
  
  return(data[nuevoOrden,])
}


#'Funcion en fase que beta, en teoria mide el ancho de una palabra y determina si existe el espacio
#'suficiente para que quepa en la grafica
#'@param graph Objeto del tipo ggplot2 que se desea modificar
#'@param ancho El ancho de las barras en porcentaje, segun indicaciones de ggplot2
existeTraslape <- function(graph,ancho = 0.6)
{
  ejeX <- 99.1 *0.0393700787
  etiquetas <- ggplot2::ggplot_build(graph)$panel$ranges[[1]]$x.labels
  tam <- list()
  for(i in 1:length(etiquetas))
  {
    tam[i] <- tikzDevice::getLatexStrWidth(etiquetas[[i]], cex = ancho)   
  }
  lapply(tam, pt2mm)
  nuBarras <- length(etiquetas)
  semiEspacio <- 99.1/(2*nuBarras)
}

#' Convierte de puntos a milimetros
#' @param unidad Valor numerico que desea ser convertido
#' @return Valor en milimetros
#' @export
pt2mm <- function(unidad)
{
  return (unidad*0.352777778)
}

#' Funcion experimental que determina cuando un dato es entero o no 
#' solamente datos enteros o del tipo flotante
#' @param x Dato numerico
#' @return Un valor booleano indicando si el dato es entero o flotante
#' @export
is.wholenumber <-
function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#' Funcion experimental que determina si un vector numerico 
#' esta conformado exclusivamente por valores entero o no
#' @param Vector numerico
#' @return Un valor booleano indicando si el vector es de enteros o no
sonEnteros <- function(data)
{
  contador = 0
  res <- FALSE
  for(i in (1:length(data$y))){
    if(is.wholenumber(data$y[[i]]) == FALSE){
      break
    }
  else{
    contador = contador +1
    }
  }
  if(contador == length(data$y)){
    res <- 1
  }
  else{
    res <-0
  }
  return(res)
}

#' Funcion para convertir de pulgadas a milimetros
#' @param number Dato a ser convertido
#' @return Valor en milimetros
#' @export
inc2mm <-function(number)
{
  return(number*25.4)
}

#' Funcion para convertir de pulgadas a puntos
#' @param number Dato a ser convertido
#' @return Valor en puntos
inc2pt <- function(number)
{
  return(number*72)
}

#'Funcion para convertir de milimetros a pulgadas
#'@param number Dato a ser convertido
#'@return Valor en pulgadas
mm2inch <- function(number)
{
  return(number*0.0393700787)
}

#'Funcion para convertir de milimetros a puntos
#'@param number Dato a ser convertido
#'@return Valor en puntos
mm2pt <- function(number)
{
  return(mm2inch(inc2pt(number)))
}

#'Funcion que calcula las posiciones para las etiquetas en las graficas de linea
#'
#'@param graph Objeto del tipo ggplot2 al cual se le quiere poner las etiquetas
#'@return Un vector indicando las posiciones de las etiquetas

calcularPosiciones <- function(graph)
{
  #SIMBOLOGIA
  # 1 HACIA ARRIBA
  #-1 HACIA ABAJO
  #0.5 A LA DERECHA
  #-0.5 A LA IZQUIERDA
  data <- ggplot2::ggplot_build(graph)$data[[1]]
  print(data)
  posiciones <- NULL
  if(data$y[[1]] < data$y[[2]])
  {
    posiciones <- c(posiciones, -1)
  }
  else{
    posiciones <- c(posiciones, 1)  
  }
  
  
  for(i in 2:(length(data$y)-1))
  {
    if(data$y[[i-1]] == data$y[[i]])
    {
      if(data$y[[i+1]] > data$y[[i]])
      {
        posiciones <- c(posiciones, -1)
      }else if(data$y[[i+1]] == data$y[[i]])
      {
        posiciones <- c(posiciones, 1)
      }else if(data$y[[i+1]] < data$y[[i]]){
        posiciones <- c(posiciones, 1)
      }
    }else if(data$y[[i-1]] > data$y[[i]])
    {
      if(data$y[[i]] > data$y[[i+1]])
      {
        posiciones <- c(posiciones, 0.5)
      }
      else{
        posiciones <- c(posiciones, -1)
      }
    }else
    {
      if(data$y[[i]] < data$y[[i+1]])
      {
        posiciones <- c(posiciones, -0.5)
      }
      else
      {
        posiciones <- c(posiciones, 1)
      }
    }
  }
  if(data$y[[length(data$y)]] == data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, 1)
  }else if(data$y[[length(data$y)]] < data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, -1)
  }else
  {
    posiciones <- c(posiciones, 1)
  }
  print("Las etiquetas son: ")
  print( posiciones )
  return(posiciones)
}


#'Funcion que calcula las posiciones para las etiquetas en las graficas de linea Doble
#'
#'@param graph Objeto del tipo ggplot2 al cual se le quiere poner las etiquetas
#'@return Un vector indicando las posiciones de las etiquetas

calcularPosicionesDobles <- function(graph)
{
  #SIMBOLOGIA
  # 1 HACIA ARRIBA
  #-1 HACIA ABAJO
  #0.5 A LA DERECHA
  #-0.5 A LA IZQUIERDA
  data <- ggplot2::ggplot_build(graph)$data[[1]]
  data2 <- ggplot2::ggplot_build(graph)$data[[2]]
  posiciones <- NULL
  posiciones2 <- NULL
  
  
  ##Etiquetado para la primera linea
  if(data$y[[1]] < data$y[[2]])
  {
    posiciones <- c(posiciones, -1)
  }
  else{
    posiciones <- c(posiciones, 1)  
  }
  
  
  for(i in 2:(length(data$y)-1))
  {
    if(data$y[[i-1]] == data$y[[i]])
    {
      if(data$y[[i+1]] > data$y[[i]])
      {
        posiciones <- c(posiciones, -1)
      }else if(data$y[[i+1]] == data$y[[i]])
      {
        posiciones <- c(posiciones, 1)
      }else if(data$y[[i+1]] < data$y[[i]]){
        posiciones <- c(posiciones, 1)
      }
    }else if(data$y[[i-1]] > data$y[[i]])
    {
      if(data$y[[i]] > data$y[[i+1]])
      {
        posiciones <- c(posiciones, 0.5)
      }
      else{
        posiciones <- c(posiciones, -1)
      }
    }else
    {
      if(data$y[[i]] < data$y[[i+1]])
      {
        posiciones <- c(posiciones, -0.5)
      }
      else
      {
        posiciones <- c(posiciones, 1)
      }
    }
  }
  if(data$y[[length(data$y)]] == data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, 1)
  }else if(data$y[[length(data$y)]] < data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, -1)
  }else
  {
    posiciones <- c(posiciones, 1)
  }
  
  
  ##Etiquetado para la segunda linea
  if(data2$y[[1]] < data2$y[[2]])
  {
    posiciones2 <- c(posiciones2, -1)
  }
  else{
    posiciones2 <- c(posiciones2, 1)  
  }
  
  
  for(i in 2:(length(data2$y)-1))
  {
    if(data2$y[[i-1]] == data2$y[[i]])
    {
      if(data2$y[[i+1]] > data2$y[[i]])
      {
        posiciones2 <- c(posiciones2, -1)
      }else if(data2$y[[i+1]] == data2$y[[i]])
      {
        posiciones2 <- c(posiciones2, 1)
      }else if(data2$y[[i+1]] < data2$y[[i]]){
        posiciones2 <- c(posiciones2, 1)
      }
    }else if(data2$y[[i-1]] > data2$y[[i]])
    {
      if(data2$y[[i]] > data2$y[[i+1]])
      {
        posiciones2 <- c(posiciones2, 0.5)
      }
      else{
        posiciones2 <- c(posiciones2, -1)
      }
    }else
    {
      if(data2$y[[i]] < data2$y[[i+1]])
      {
        posiciones2 <- c(posiciones2, -0.5)
      }
      else
      {
        posiciones2 <- c(posiciones2, 1)
      }
    }
  }
  if(data2$y[[length(data2$y)]] == data2$y[[length(data2$y)-1]])
  {
    posiciones2 <- c(posiciones2, 1)
  }else if(data2$y[[length(data2$y)]] < data2$y[[length(data2$y)-1]])
  {
    posiciones2 <- c(posiciones2, -1)
  }else
  {
    posiciones2 <- c(posiciones2, 1)
  }
  
  
  
  
  print("Las etiquetas son: ")
  print( list(posiciones, posiciones2)  )
  return( list(posiciones, posiciones2) )
}

#'Le pone las etiquetas a una grafica de linea
#'
#'@param graph Objeto del tipo ggplot2 que desea anotar
#'@param posiciones Vector de posiciones en que van las etiquetas
#'@param precision Numero de decimales con el que se desea el vector respuesta.
#'Por defecto se usa un decimal
etiquetasLineas <- function(graph, posiciones, precision=1)
{
  pkg.env$precision <- precision
  d <- ggplot2::ggplot_build(graph)$data[[1]]
  enteros <- sonEnteros(d)
  if (pkg.env$maxMin == T){
    print("La función de cuatro etiquetas está activada")
    lista <- NULL
    lista <- c(lista,max(d$y), min(d$y))
    if( !(d$y[[1]] %in% lista) ){
      lista <- c(lista, d$y[[1]])
    }
    
    if( !(d$y[[length(posiciones)]] %in% lista) ){
      lista <- c(lista, d$y[[length(posiciones)]])
    }
    print(c("La lista inicial es: " , lista))
  }  
  

  for(i in 1:length(posiciones))
  {
    dato <- d$y[[i]]
    print(c("El dato correspondiente a la posición i es: ", i, " con valor ", dato, " mientras que la longitud de la posicion es: " , length(posiciones)))
    
    if (pkg.env$maxMin == T) { 
      if( !(dato %in% lista) ){
        dato <- NA
      }else if ( ( dato ==d$y[[length(posiciones)]]  && i != length(posiciones) )  ){
        if( !( i == 1 && dato == d$y[[1]] ) ){  
        print("Etiqueta en riesgo")
          dato <-NA
        }
      }else{
          lista <- lista[lista != dato ] 
        }
      print("La lista es ")
      print(lista)
    }
    
    
    
   
    
    d$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d$x))), format = 'f', big.mark = ',', digits = pkg.env$precision, drop0trailing = enteros)
    print("#####LAS ETIQUETAS SON ##########" )
    print(d$etiqueta)
    
    if(posiciones[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d, ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta) == "NA" ,"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = -0.5)
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta) == "NA","",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = 1.5)
    }else if(posiciones[[i]] == 0.5)
    {
      graph <- graph +ggplot2::geom_text(data =d,ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta) == "NA","", etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta) == "NA","",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 1.2, vjust = 0)
    }
    
    
    
  }
  return(graph)
}



#'Le pone las etiquetas a una grafica de linea
#'
#'@param graph Objeto del tipo ggplot2 que desea anotar
#'@param posiciones Vector de posiciones en que van las etiquetas
#'@param precision Numero de decimales con el que se desea el vector respuesta.
#'Por defecto se usa un decimal
#'@return Gráfica con las etiquetas puestasw
#'@export
etiquetasLineasDobles <- function(graph, pos, precision=1)
{
  print(c('La precision es: ', precision ))
  pkg.env$precision <- precision
  d1 <- ggplot2::ggplot_build(graph)$data[[1]]
  d2 <- ggplot2::ggplot_build(graph)$data[[2]]
  enteros1 <- sonEnteros(d1)
  enteros2 <- sonEnteros(d2)
  posiciones <- pos[[1]]
  posiciones2 <- pos[[2]]
  
  
  for(i in 1:length(posiciones))
  {
    dato <- d1$y[[i]] 
    
    if(enteros1 == 0)
    {
      d1$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d1$x))), format = 'f', big.mark = ',', digits = pkg.env$precision)
    }
    else
    {
      d1$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d1$x))), format = 'f', big.mark = ',', digits = pkg.env$precision, drop0trailing = T)
    }
    
    
    if(posiciones[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d1, ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA' ,"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = -0.5)
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d1,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = 1.5)
    }else if(posiciones[[i]] == 0.5)
    {
      graph <- graph +ggplot2::geom_text(data =d1,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"", etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + ggplot2::geom_text(data = d1,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 1.2, vjust = 0)
    }
    
  }
  
  
  for(i in 1:length(posiciones2))
  {
    dato <- d2$y[[i]] 
    
    if(enteros1 == 0)
    {
      d2$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d2$x))), format = 'f', big.mark = ',', digits = pkg.env$precision)
    }
    else
    {
      d2$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d2$x))), format = 'f', big.mark = ',', digits = pkg.env$precision, drop0trailing = T)
    }
    
    
    if(posiciones2[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d2, ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA' ,"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = -0.5)
    }else if(posiciones2[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d2,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = 1.5)
    }else if(posiciones2[[i]] == 0.5)
    {
      graph <- graph +ggplot2::geom_text(data =d2,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"", etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + ggplot2::geom_text(data = d2,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 1.2, vjust = 0)
    }
    
  }
  
  return(graph)
}



#'Funcion interna que le pone el relleno al vector de etiquetas
#'del tipo ("","",etiqueta,"",..)
#'
#'@param dato Dato que va en el vector
#'@param posicion Es la posicion que ocupa el dato dentro del vector
#'@param tam El tamano del vector de salida
#'@return El vector completo rellenado de la forma ("", "", dato, "", ...)
completarEtiquetas <- function(dato,posicion, tam = 5)
{
  etiquetas <- NULL
  for(i in 1:tam)
  {
    if(i == posicion)
    {
      etiquetas <- c(etiquetas, dato)
    }
    else
    {
      etiquetas <- c(etiquetas,"NA")  
    }
  }
  return(etiquetas)
}


#'Funcion que rota las etiquetas en el eje X para graficas de columnas que poseen
#'etiquetas horizontales para las columnas 
#'
#'@param graph El objeto ggpot2 que se desea modificar
#'@return Objeto ggplot2 modificado en las etiquetas del eje X
rotarEtiX <- function(graph)
{
  
  longitud <- 2.8 + 2
  graph <- graph + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =0.5 , hjust= 1))
}

#'Funcion que rota las etiquetas en el eje X para graficas de columnas que poseen
#'etiquetas verticales para las columnas 
#'
#'@param graph El objeto ggpot2 que se desea modificar
#'@return Objeto ggplot2 modificado en las etiquetas del eje X
rotarEtiX2 <- function(graph)
{
  ##max <-ggplot2::ggplot_build(graph)$panel$ranges[[1]]$y.range[2]
  ##longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = 1), cex = pkg.env$fEscala) 
  ##longitud <- longitud*0.352777778 + 1
  graph <- graph + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =0.5 , hjust= 1))
}


#' Anota las etiquetas para una grafica de barras
#' 
#' @param graph Objeto ggplot2 que se desea anotar
#' @return Retorna objeto ggplot2 listo para graficar
#' @return cambiarNegativas parámetro temporal, desaparecerá en futuras versiones.
etiquetasBarras <- function(graph, margenIz = 0, precision = 1, cambiarNegativas = F)
{
  pkg.env$precision <- precision
    
  data <- ggplot2::ggplot_build(graph)$data[[1]]
  if ( nrow(subset(data, y<0)) > 0 ){
    min <- min(data$y)
    print(c("El mínimo es: ", min))
    espacio <- tikzDevice::getLatexStrWidth(formatC(min,format = "f",big.mark = ",", digits = pkg.env$precision), cex = pkg.env$fEscala)
    espacio <- pt2mm(espacio)+ 3 
  }else{
    espacio <- 0
  }
  longitudIzquierda <- 5
  max <-max(ggplot2::ggplot_build(graph)$data[[1]]$y)
  min <-min(ggplot2::ggplot_build(graph)$data[[1]]$y)
  longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = pkg.env$precision), cex = pkg.env$fEscala)
  print(paste("El valor de longitud antes de conversion es ", longitud, max))
  longitud <- longitud*0.352777778 + 2.3
  longitudInferior <- tikzDevice::getLatexStrWidth(formatC(min,format = "f",big.mark = ",", digits = pkg.env$precision), cex = pkg.env$fEscala)
  longitudInferior <- longitudInferior*0.352777778 + 2.3
  mIz <- 0 + margenIz
  print(paste("El valor de longitud es", longitud))
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    pkg.env$botarCeros <- T
  }
  else
  {
    pkg.env$botarCeros <- F
  }
  


  
  data$etiqueta <- formatC( formatC(data$y, format = "f",big.mark = ",", digits = pkg.env$precision,  drop0trailing = !pkg.env$botarCeros) ) 

  
  graph <- graph + ggplot2::geom_text(data = data ,ggplot2::aes(label= etiqueta ,family=pkg.env$fuente),size=pkg.env$sizeText,hjust = -0.2, vjust = 0.5)
  
  

  
  graph <- graph + ggplot2::theme(plot.margin = grid::unit(c(0,longitud + 2 ,0,0), "mm")
                                  ,axis.text.y= ggplot2::element_text(margin=ggplot2::margin(0,espacio,0,0, unit = "mm") )                
                  #,axis.text.y = ggplot2::element_text(margin=ggplot2::margin(0,longitud,1,longitudIzquierda), "mm")
                  )
  
  return(graph)
} 


#' Anota las etiquetas para una grafica de barras
#' Haciendo pruebas de no medir el largo de las etiquetas con 
#' TikzDevice y hacerlo puramente con ggplot2
#' 
#' @param graph Objeto ggplot2 que se desea anotar
#' @return Retorna objeto ggplot2 listo para graficar
#' @return cambiarNegativas parámetro temporal, desaparecerá en futuras versiones.
etiquetasBarras2 <- function(graph, margenIz = 0, precision = 1, cambiarNegativas = F)
{
  pkg.env$precision <- precision
  posiciones <- NULL
  
  posiciones <- posicionesBarras(ggplot2::ggplot_build((graph))$data[[1]]$y)
  
  data <- ggplot2::ggplot_build(graph)$data[[1]]
  print(names(data))
  print(data)
  
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    pkg.env$botarCeros <- T
  }
  else
  {
    pkg.env$botarCeros <- F
  }
  
  d <- ggplot2::ggplot_build(graph)$data[[1]]
  for(i in 1:length(posiciones)){
    if (cambiarNegativas == T){
      dato <- -1*d$y[[i]]  
    }else{
      dato <- d$y[[i]]
    }
    
    
    d$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d$x))), format = 'f', big.mark = ',', digits = pkg.env$precision, drop0trailing = !pkg.env$botarCeros)
    
    print("#####LAS ETIQUETAS SON ##########" )
    print(d$etiqueta)
    
    
    
    if(posiciones[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d, ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta)  == 'NA' ,"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = "outward", vjust = "outward")
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta) == 'NA',"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = "outward", vjust = "outward")
    }
  }
  
  
  
  
  return(graph)
} 


#'Función para calcular vector de posiciones para las etiquetas de Barras
#'@param x Vector de valores para el eje y.
posicionesBarras <- function(x){
  posiciones <- c()
  for(var in x){
    if(var < 0){
     posiciones <- c(posiciones,-1) 
    }else{
      posiciones <-c(posiciones,1)
    }
  }
  return(posiciones)
}

#'Pone etiquetas a las columnas en una grafica de columnas
#'
#'@param graph El objeto ggplot2 que se desea anotar
#'@return Objeto ggplot2 anotado listo para usar
#'@export
etiquetasHorizontales <- function(graph, precision = 1, cambiarNegativas = F)
{
  pkg.env$precision <- precision
  posiciones <- NULL
  
  posiciones <- posicionesBarras(ggplot2::ggplot_build((graph))$data[[1]]$y)
  
  data <- ggplot2::ggplot_build(graph)$data[[1]]
  print(names(data))
  print(data)
  if ( nrow(subset(data, y<0)) > 0 ){
    min <- min(data$y)
    espacio <- 4.5
  }else{
    espacio <- 0
  }
  print(c("El valor de espacio es: ", espacio))
  longitudIzquierda <- 0
  max <-ggplot2::ggplot_build(graph)$panel$ranges[[1]]$x.range[2]
  min <-ggplot2::ggplot_build(graph)$panel$ranges[[1]]$x.range[1]
  longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = pkg.env$precision), cex = pkg.env$fEscala) 
  longitud <- longitud*0.352777778 + 2.3
  longitudInferior <- tikzDevice::getLatexStrWidth(formatC(min,format = "f",big.mark = ",", digits = pkg.env$precision), cex = pkg.env$fEscala)
  longitudInferior <- longitudInferior*0.352777778 + 2.3
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    pkg.env$botarCeros <- T
  }
  else
  {
    pkg.env$botarCeros <- F
  }
  
  d <- ggplot2::ggplot_build(graph)$data[[1]]
  for(i in 1:length(posiciones)){
    if (cambiarNegativas == T){
      dato <- -1*d$y[[i]]  
    }else{
      dato <- d$y[[i]]
    }
    
    
    d$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d$x))), format = 'f', big.mark = ',', digits = pkg.env$precision, drop0trailing = !pkg.env$botarCeros)
    

    
    print("#####LAS ETIQUETAS SON ##########" )
    print(d$etiqueta)
    top <- 6
    
    
    if(posiciones[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d, ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta)  == 'NA' ,"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = -0.5)
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(stringr::str_trim(etiqueta) == 'NA',"",etiqueta),family=pkg.env$fuente),size=pkg.env$sizeText,hjust = 0.5, vjust = 1.5 )
    }
  }
  
 #axis.ticks.margin = grid::unit(c(espacio,0),"mm"),
  
  graph <- graph + ggplot2::theme(axis.text.x = ggplot2::element_text(margin=ggplot2::margin(0,0,espacio,0,"mm")))+
    ggplot2::theme(plot.margin = grid::unit(c(7,0,0,0), "mm"))
  
  return(graph)
  
  #######################
#   pkg.env$digitos <- precision
#   longitud <- 6
#   if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
#   {
#     graph <- graph +
#       ggplot2::geom_text(ggplot2::aes(family = pkg.env$fuente,label= formatC(y,format = "f",digits = pkg.env$digitos,big.mark = ",", drop0trailing = F)),size=pkg.env$sizeText, hjust=0.5, vjust = -0.5)+
#       ggplot2::theme(plot.margin = grid::unit(c(longitud,0,3,0), "mm"))
#   }
#   else
#   {
#     graph <- graph +
#       ggplot2::geom_text(ggplot2::aes(family = pkg.env$fuente,label= formatC(y,format = "f",digits = pkg.env$digitos,big.mark = ",", drop0trailing = T)),size=pkg.env$sizeText, hjust=0.5, vjust = -0.5)+
#       ggplot2::theme(plot.margin = grid::unit(c(longitud,0,3,0), "mm"))
#   }
}


#'Pone etiquetas a las columnas en una grafica de columnas
#'
#'@param graph El objeto ggplot2 que se desea anotar
#'@return Objeto ggplot2 anotado listo para usar
#'@export
etiquetasFacets <- function(graph, precision = 1)
{
  pkg.env$digitos <- precision
  longitud <- 6
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = pkg.env$fuente,label= formatC(y,format = "f",digits = pkg.env$digitos,big.mark = ",", drop0trailing = F)),size=pkg.env$sizeText, hjust=0.5, vjust = -0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(4,0,2,-8), "mm"))
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = pkg.env$fuente,label= formatC(y,format = "f",digits = pkg.env$digitos,big.mark = ",", drop0trailing = T)),size=pkg.env$sizeText, hjust=0.5, vjust = -0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(4,0,2,-8), "mm"))
  }
}


#'Pone etiquetas verticales a las columnas en una grafica de columnas 
#'
#'@param graph El objeto ggplot2 que se desea anotar
#'@return Objeto ggplot2 anotado listo para usar
#'@export
etiquetasVerticales <- function(graph, precision = 1)
{
  pkg.env$digitos <- precision
  max <-ggplot2::ggplot_build(graph)$panel$ranges[[1]]$y.range[2] 
  longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = pkg.env$digitos), cex = pkg.env$fEscala) 
  longitud <- longitud*0.352777778 + 2.5
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = pkg.env$fuente,label= formatC(y, big.mark = ",", format = "f", digits =pkg.env$digitos)), angle = 90, size=pkg.env$sizeText, hjust=-0.3, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))  
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = pkg.env$fuente,label= formatC(y, big.mark = ",", format = "f", digits =0, drop0trailing = T)), angle = 90, size=pkg.env$sizeText, hjust=-0.3, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))
  }
}


#'Exporta a codigo tikz en LaTeX usando tikzDevice
#'
#'@param nombre Ruta del fichero LaTeX
#'@param graph Objeto ggplot2 que se desea exportar a LaTeX
exportarLatex <- function(nombre = grafica.tex, graph, preambulo = F)
{
  tikzDevice::tikz(nombre, standAlone = preambulo, bareBones = TRUE, bg = "transparent",width = pkg.env$ancho, height= pkg.env$alto, sanitize = F)
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  temp$layout$clip <- "off"
  grid::grid.draw(temp)
  dev.off()
}



retocarGrafica <- function(graph){
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  temp$layout$clip <- "off"
  grid::grid.draw(temp)
}

#'Calcula la rampa de colores para usar en las graficas
#'de anillo, mandando los ignorados hasta el final y haciendo
#'la separcion por categorias (blanco para una categoría y negro para la otra)
#'El gris siempre se usa para ignorado
#'
#'@param x El vector de datos en el cual se basa la paleta de colores
#'@param categoria Booleano que indica si se desea categorizar la rampa
#'@return El vector de la paleta de colores
#'@export 

calcularRampaAnillo <- function(x, categoria = TRUE){
  rampa = NULL
  print( pkg.env$modalidad )
  if (pkg.env$modalidad == "trimestral"){
    pkg.env$color1 <- rgb(1,1,1)
  }
  
  if(categoria == TRUE){
    if("IGNORADO" %in% toupper(x))
    {
      #print("IGNORADO")
      rampa = c(pkg.env$color1, pkg.env$color2, pkg.env$gris)
    }else{
      rampa = c(pkg.env$color1, pkg.env$color2)
    }
  }else{
    rampaAux <- grDevices::colorRampPalette(c(pkg.env$color1,pkg.env$color2))
    if("IGNORADO" %in% toupper(x)){
      rampa <- c(rampaAux(2), pkg.env$gris)
    }else{
      rampa <- rampaAux(length(x))
    }
  }
  
  if (pkg.env$modalidad == "trimestral"){
    pkg.env$color1 <- rgb(0,0,0)
  }
  return(rampa)
}


#' Convierte los factores de un data frame a datos numericos
#' 
#'@param tabla El data frame que se quiere trabajar
#'@return Regresa el data frame con valores numéricos en lugar de factores
#'@export  
fact2Num <- function(tabla)
{
  if(length(names(tabla)) > 1 ){
    print(tabla)
    nombres <- names(tabla)
    names(tabla) <- c("x","y")
    tabla$y <- as.numeric(gsub(",","", tabla$y))
    if(is.factor(tabla$y))
    {
      tabla$y<- as.numeric(levels(tabla$y))[tabla$y]    
    }
    else
    {
      tabla$y<- as.numeric(tabla$y)   
    }
    names(tabla) <- nombres
  }

  #print(names(tablas))
  return(tabla)
}

#'Cambia la codificación en ciertas partes del data frame
#'@param tabla Es el data frame al cual se le desea cambiar la 
#'codificacion
#'@return Data frame recodificado
#'

cambiarCodificacion <- function(tabla){
  print(length(names(tabla)))
  if( length( names(tabla) ) >1){
    print('Entre al if')
    print(names(tabla))
    nombres <- names(tabla)
      nombres[1] <- "x"
    if(nombres[2] == "Y")
      nombres[2] <- "y"
    names(tabla) <- nombres
    ##nombres <- gsub("\\.", " ", nombres)
    nombres <- iconv(nombres, to = "UTF8//TRANSLIT")
    names(tabla) <- nombres
    x <- tabla$x
    x <- iconv(x, to = "UTF8//TRANSLIT")
    tabla$x <- x
  }
  return(tabla)
}

#'Función que comila con xelatex y muestra el resultado
#'@param ruta Es la ruta de donde se desea guardar el fichero .tex
#'@param mostrar Booleano, cuando es verdadero muestra el pdf compilado.
#'@export
compilar <- function(ruta = "", mostrar = T){
  if (.Platform$OS.type == "windows") {
    print(paste("C: && cd", dirname(ruta), "&&xelatex  --synctex=1 --interaction=nonstopmode",ruta), mustWork=TRUE, intern=TRUE, translate=TRUE)
    shell(cmd=paste("C: && cd", dirname(ruta), "&&xelatex  --synctex=1 --interaction=nonstopmode",ruta), mustWork=TRUE, intern=TRUE, translate=TRUE)  
  }else{
  cadenaCompilacion <-  paste("cd", dirname(ruta), "&&xelatex  --synctex=1 --interaction=nonstopmode",ruta)
  print(cadenaCompilacion)
  suppressWarnings(silence <- system( cadenaCompilacion, intern=T, ignore.stderr=T))
  }
  if( mostrar == T){
    if ( .Platform$OS.type == "windows"){ 
    print(paste0('start ',paste(dirname(ruta), gsub(".tex",".pdf",basename(ruta)), sep="/")))
    system(paste0('cmd.exe /c start ',paste(dirname(ruta), gsub(".tex",".pdf",basename(ruta)), sep="/")), intern = T, ignore.stderr = T) 
    }else{
     system(paste(dirname(ruta), gsub(".tex",".pdf",basename(ruta)), sep="/"), intern = T, ignore.stderr = T) 
      }
    }
  
}

#'Función que hace pdfcrop
#'@param ruta Es la ruta de donde se desea guardar el fichero .tex
#'@param mostrar Booleano, cuando es verdadero muestra el pdf compilado.
#'@export
pdfcrop <- function(ruta = ""){
  if (.Platform$OS.type == "windows") {
    shell(cmd=paste("cd", dirname(ruta), "&& pdfcrop ",gsub(".tex",".pdf",basename(ruta))), mustWork=TRUE, intern=TRUE, translate=TRUE)  
  }else{
    cadenaCompilacion <-  paste("cd", dirname(ruta), "&& pdfcrop",gsub(".tex",".pdf",basename(ruta)), gsub(".tex",".pdf",basename(ruta)))
    print(cadenaCompilacion)
    suppressWarnings(silence <- system( cadenaCompilacion, intern=T, ignore.stderr=T))
  }
  
}


preview <- function(graph)
{
  nombre = tempfile(pattern="Preview", tmpdir= paste(normalizePath(getwd()),"Temporal", sep="\\"))
  tikz(paste(nombre,".tex", sep= ""), standAlone = TRUE, bg = "transparent",bareBones = FALSE, width = 3.19, height= 1.91, sanitize= F)
  temp<- ggplot_gtable(ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid.draw(temp)
  dev.off()
  shell(cmd=paste("xelatex   --synctex=1 --interaction=nonstopmode", "--output-directory",dirname(nombre),paste(nombre,".tex", sep="")))
  shell.exec(paste(nombre,".pdf", sep=""))
}


#'Calcula la rampa de colores para usar en las graficas
#'de columnas agrupadas, mandando los ignorados hasta el final y haciendo
#'la separcion por categorias (blanco para una categoría y negro para la otra)
#'El gris siempre se usa para ignorado
#'
#'@param data  El data frame con el que se harán los calculos
#'@return El vector de la paleta de colores

rampaColAgrupadas <- function(data){
  rampa = NULL
  rampa1 = NULL
  if(nrow(subset(data, y>0)) > 0){
    print("No hay valores negativos")
    if(toupper("Ignorado") %in% toupper(data$categoria)){
      print("Hay ignorados")
      if( pkg.env$modalidad == "trimestral"){
        rampa = c(grDevices::rgb(1,1,1), grDevices::rgb(0.5,0.5,0.5), pkg.env$gris)
        rampa1 = c(grDevices::rgb(0,0,0), grDevices::rgb(0.5,0.5,0.5), pkg.env$gris)
      }
      
    }else{
      print("No hay ignorados")
      if( pkg.env$modalidad == "trimestral"){
        print("La modalidad es trimestral")
        rampaAux = grDevices::colorRampPalette(c(grDevices::rgb(1,1,1), grDevices::rgb(0.5,0.5,0.5)))
        rampaAux1 <- grDevices::colorRampPalette(c(grDevices::rgb(0,0,0), grDevices::rgb(0.5,0.5,0.5)))
        rampa = rampaAux(length(levels(data$categoria)))
        rampa1 = rampaAux1(length(levels(data$categoria)))  
      }else if(pkg.env$modalidad == "anual" || pkg.env$modalidad == "presentacion"){
        print("Es anual o presentacion")
        rampaAux = grDevices::colorRampPalette( c(pkg.env$color1, pkg.env$color2 ) )
        rampaAux1 <- rampaAux
        if (length(levels(data$categoria) ) == 3 ){
          print("Hay 3 categorias")
          print(levels(data$categoria))
          rampa = c(rampaAux(2), pkg.env$gris)
          rampa1 = c(rampaAux(2), pkg.env$gris)
        }else{
          rampa = rampaAux(length(levels(data$categoria)))
          rampa1 = rampaAux1(length(levels(data$categoria)))  
        }
        
      }
      
    }
  }else {
    print("Hay valores negativos")
    rampaAux = grDevices::colorRampPalette(c(grDevices::rgb(0.2,0.2,0.2), grDevices::rgb(0.4,0.4,0.4)))
    rampa = rampaAux(2)
    rampa = c(rampa, pkg.env$gris)
  }
  return(list(rampa,rampa1))
}

#' Función para hacer carga masiva 
#' de archivos CSV para su posterior uso con una lista 
#' dentro de R.
#' 
#' @param ruta Ruta dentro del disco duro en la cual están contenidos los CSV
#' @return Una lista con los data frame que contiene la información.

cargaMasiva <- function (ruta, codificacion = 'iso') {
  filenames <- list.files(path = ruta, pattern = ".csv", full.names = TRUE)
  numfiles <- length(filenames)
  fn <- list.files(path = paste(tempdir(), "CSV", sep = "/") , full.names = TRUE)
  file.remove(fn)
  dir <- ruta
  
  pkg.env$cod = ''
  if ( toupper(codificacion) == 'ISO' ){
    print("Estas usando codificacion Windows")
    pkg.env$cod = 'iso-8859-1'  
  }else if( toupper(codificacion) == 'UTF8' )
  {
    print('Estas usando codificacion linux')
    pkg.env$cod = 'utf-8'
  }
  print(pkg.env$cod)
  filenames <- list.files(path = dir, pattern = ".csv", full.names = TRUE)
  All <- lapply(filenames,function(i){
    #iso-8859-1
    #utf-8
    read.csv(i,header = TRUE, sep = detectarSeparador(i),  fileEncoding=pkg.env$cod, check.names = F, quote="\"") 
  })
  filenames <- gsub(".csv","", filenames)
  names(All) <- basename(filenames)
  tablas <- lapply(All,fact2Num)
  tablas <- lapply(tablas, cambiarCodificacion)
  return(tablas)
}

#'Función que calcula el cambio interanual en porcentaje para un data frame dado
#'
#'@param data El data frame sobre el cual se desea hacer el calculo
#'@param paso El paso de retroceso para el calculo
#'@return Cambio interanual
#'
cambioInterAnual <- function(data, primeraPos = 5, ultimaPos = 9){
    cambio <- ( data$y[ultimaPos] / data$y[primeraPos] ) *100
    return(abs(100-cambio))
}


#'Función que calcula el cambio interanual neto para un data frame dado
#'
#'@param data El data frame sobre el cual se desea hacer el calculo
#'@param paso El paso de retroceso para el calculo
#'@return Cambio interanual
#'
cambioInterAnualNeto <- function(data, paso = 4){
  cambio <- ( data$y[length(data$y)] - data$y[length(data$y) - paso] )
  return(abs(cambio))
}

#'Función para poner parametrización del formato de trimestrales
trimestral <- function(){
  pkg.env$alto <- 1.91 
  pkg.env$ancho <- 3.19
  options(tikzDocumentDeclaration= "\\documentclass[10pt,twoside]{book}")
  pkg.env$modalidad <- "trimestral"
  cambiarGraficas(tamFuente = 10)
  pkg.env$color1 <- rgb(0,0,0)
  pkg.env$color2 <- rgb(0.6,0.6,0.6)
  pkg.env$colorRelleno <- rgb(1,1,1)
  pkg.env$colorRelleno2 <- rgb(0.6,0.6,0.6)
}

#'Función para poner parametrización del formato anual
#'@param color1 Color primario para graficar, definido por el usuario. Este debe ser definido 
#'con el formato rgb(v1,v2,v3, maxColorValue = 255), si maxColorValue no está definido
#'se usa por defecto 1. 
#'@param color2 Color secundario para graficar, definido por el usuario con el formato 
#'rgb(v1,v2,v3, maxColorValue = 255), si maxColorValue no está definido
#'se usa por defecto 1. 
#'rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1)
anual <- function(color1 = rgb(0,0,1), color2 = rgb(0.6156862745098039,0.7333333333333333,1),fuente = pkg.env$fuente){
  pkg.env$alto <- 2.75  ##2.75
  pkg.env$ancho <- 4
  options(tikzDocumentDeclaration= "\\documentclass[11pt,twoside]{book}")
  pkg.env$fontSize = 11
  pkg.env$color1 <- color1
  pkg.env$color2 <- color2
  pkg.env$colorRelleno <- color1
  pkg.env$colorRelleno2 <- color2
  pkg.env$modalidad <- "anual"
  pkg.env$fEscala <- 1
  pkg.env$fuente <- fuente
  cambiarGraficas(tamFuente = 11)
  #pkg.env$fontSize <- 11
}

web <- function(color1 = rgb(0,0,1), color2 = rgb(0.6156862745098039,0.7333333333333333,1)){
  pkg.env$alto <- 50 ##2.75
  pkg.env$ancho <- 4
  options(tikzDocumentDeclaration= "\\documentclass[11pt,twoside]{book}")
  pkg.env$fontSize = 11
  pkg.env$color1 <- color1
  pkg.env$color2 <- color2
  pkg.env$colorRelleno <- color1
  pkg.env$colorRelleno2 <- color2
  pkg.env$modalidad <- "anual"
  pkg.env$fEscala <- 2.3
  cambiarGraficas(tamFuente = 20)
  #pkg.env$fontSize <- 11
}



#'Función para poner parametrización del formato de presentaciones
presentacion <- function(){
  pkg.env$modalidad <- "presentacion"
  pkg.env$alto <- 1.91 
  pkg.env$ancho <- 3.19
  options(tikzDocumentDeclaration= "\\documentclass[10pt,twoside]{book}")
  #pkg.env$modalidad <- "trimestral"
  cambiarGraficas(tamFuente = 10)
  pkg.env$color1 <- rgb(0,0,1) #0 0 0
  pkg.env$color2 <- rgb(0.3,0.7,1)
  pkg.env$colorRelleno <- rgb(0,0,1) # 1 1 1 
}

#'Funcion para activar las cuatro etiquetas, maximo y minimo.
cuatroEtiquetas <- function(encendido = TRUE){
  print(c("El valor de encendido es: ", encendido))
  if (encendido == T){
    print("maxMin activado")
    pkg.env$maxMin <- T
  } else{
    print("maxMin desactivado")
    pkg.env$maxMin <- F
  }
}

detectarSeparador <- function(ruta){
  separador = NULL
  if( length ( read.csv(ruta,header = TRUE, sep = ";",  fileEncoding='utf-8', check.names = F) ) == 1 ){
    separador = ','
  }else{
    separador = ';'
  }
  return(separador)
}

#'Función para escribir un libro de excel a partir de una lista de data frames
#'@lista Lista que aloja los data frames
#'@ruta Ruta completa del archivo que se desea como salida
escribirLibro <- function(lista, ruta){
  nombres <- names(lista)
  contador <- 1
  for( i in lista){
    xlsx::write.xlsx(i, file = ruta, sheetName = nombres[contador], append = T, row.names = FALSE)
    contador = contador + 1
  }
}


#' Función leer libro excel
#' @param ruta Ruta dentro del disco duro en la cual están contenidos los CSV
#' @return Una lista con los data frame que contiene la información.

leerLibro <- function (ruta, codificacion = 'iso') {
  libro <- xlsx::loadWorkbook(ruta)
  hojas <- xlsx::getSheets(libro)
  nombres <- names(hojas)
  lista <- list()
  #names(lista) <- nombres
  contador <-1
  for( x in hojas){
    if (  is.na(as.numeric(substring(nombres[contador],1,1))) == TRUE ){
      pkg.env$header = F
    }else{
      pkg.env$header = T
    }
    print(c("El nombre es: ", nombres[contador], " y el booleano dio " , pkg.env$header))
    data <- xlsx::read.xlsx2(ruta, sheetName = nombres[contador], as.data.frame = T, header = pkg.env$header, check.names = F, stringsAsFactors = F)
    print(data)
    temp <- data[-which(data[1] == ""),]
    if ( nrow(temp) != 0){
      data <- temp
    }
    print(data)
    lista[[contador]] <- data
    contador = contador +1 
  }
  names(lista) <- nombres
  return(lista)
}


#' Función leer libro excel
#' @param ruta Ruta dentro del disco duro en la cual están contenidos los CSV
#' @return Una lista con los data frame que contiene la información.

leerLibroNormal <- function (ruta, codificacion = 'iso') {
  libro <- xlsx::loadWorkbook(ruta)
  hojas <- xlsx::getSheets(libro)
  nombres <- names(hojas)
  lista <- list()
  #names(lista) <- nombres
  contador <-1
  for( x in hojas){
    data <- xlsx::read.xlsx2(ruta, sheetName = nombres[contador], as.data.frame = T, check.names = F, stringsAsFactors = F)
#     print(data)
#     temp <- data[-which(data[1] == ""),]
#     if ( nrow(temp) != 0){
#       data <- temp
#     }
#     print(data)
    lista[[contador]] <- data
    contador = contador +1 
  }
  names(lista) <- nombres
  return(lista)
}


#' Función leer convertirFechas de vitales
#' @return lista Una lista con los data frame que contiene la información.

convertirFechas <- function (lista) {  nombres <- names(lista)
  contador <-1
  lis <- list()
  for ( x in lista ){
    print(is.na(as.numeric(substring(nombres[contador],1,1))) )
    if (  is.na(as.numeric(substring(nombres[contador],1,1))) == TRUE ){
      if(as.numeric( substring(nombres[contador],nchar(nombres[contador]), nchar(nombres[contador]) ) ) %% 2 == 0 ){
          name <- format(as.Date(as.numeric(x$'1'[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]), origin="1899-12-30", format = "%Y-%m-%d"), "%B/%Y" )
          x$'1'[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]  <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
          print(x)
          }
    }
    lis[[contador]] <- x
    contador <- contador +1 
    
  }
  names(lis) <- nombres
  return(lis)
}

#' Función leer convertirFechas de IPC
#' @return lista Una lista con los data frame que contiene la información.

convertirFechasIPC <- function (lista) {  nombres <- names(lista)
contador <-1
lis <- list()
for ( x in lista ){
  print(x[1][[1]])
  if( is.na( as.numeric(x[1][[1]]) ) == FALSE && is.na(as.numeric(substring(nombres[contador],1,1))) == FALSE ){
    print("Entre al if")
    name  <- format(as.Date(as.numeric(x[1][[1]]), origin="1899-12-30", format = "%Y-%m-%d"), "%b-%Y" )  
    x[1][[1]] <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
    }
  lis[[contador]] <- x
  contador <- contador +1 
  
}
names(lis) <- nombres
return(lis)
}


#' Función leer convertirFechas de Transportes
#' @return lista Una lista con los data frame que contiene la información.

convertirFechasTransporte <- function (lista) {  nombres <- names(lista)
contador <-1
lis <- list()
for ( x in lista ){
  print(substring(nombres[contador],1,1))
  print( c( "El numero mágico es: " , substring(nombres[contador],nchar(nombres[contador])-1, nchar(nombres[contador]) )  ) )
  print( is.na(as.numeric(substring(nombres[contador],1,1) ) ) )
  if (  is.na(as.numeric(substring(nombres[contador],1,1))) == TRUE ){
    print( c("Antes de entrar al segundo if: ", substring(nombres[contador],nchar(nombres[contador])-1, nchar(nombres[contador]) ) %in% list("03","06","08","09","10","11","12","13","14","15") ) )
    if( substring(nombres[contador],nchar(nombres[contador])-1, nchar(nombres[contador]) ) %in% list("03","06","08","09","10","11","12","13","14","15")  ){
      print(c("Entre al if con: ",substring(nombres[contador],nchar(nombres[contador])-1, nchar(nombres[contador]) )  ))
      name <- format(as.Date(as.numeric(x$'1'[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]), origin="1899-12-30", format = "%Y-%m-%d"), "%B/%Y" )
      x$'1'[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]  <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
      #print(x)
    }
  }
  lis[[contador]] <- x
  contador <- contador +1 
  
}
names(lis) <- nombres
return(lis)
}



convertirFechasTodos <- function (lista) {  nombres <- names(lista)
                                       contador <-1
                                       lis <- list()
                                       for ( x in lista ){
                                         print(is.na(as.numeric(substring(nombres[contador],1,1))) )
                                         if (  is.na(as.numeric(substring(nombres[contador],1,1))) == TRUE ){
                                             print( format(as.Date(as.numeric(x$'1'[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]), origin="1899-12-30", format = "%Y-%m-%d"), "%b/%Y" ) )
                                             x$'1'[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]  <- format(as.Date(as.numeric(x$'1'[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]), origin="1899-12-30", format = "%Y-%m-%d"), "%B/%Y" )
                                             print(x)
                                         }
                                         lis[[contador]] <- x
                                         contador <- contador +1 
                                         
                                       }
                                       names(lis) <- nombres
                                       return(lis)
}



#'Función para escribir CSV
#'@param lista Es la lista que contiene los data frame
#'@ruta Es la ruta padre donde irán alojados los archivos csv

escribirCSV <- function(lista, ruta){
  nombres <- names(lista)
  print(nombres)
  contador <- 1
  for(x in lista){
    print( file.path(ruta, paste( nombres[contador], ".csv", sep = '' ) ) )
    print( nombres[contador] )
    if( is.na(as.numeric(names(x)))[1] == F  ){
      pkg.env$quitarNombres <- F
      print(c(nombres[contador],"Quitando los numeros"))
    }else{
      print(c(nombres[contador],"Si tiene encabezado"))
      pkg.env$quitarNombres <- T
    }
    write.table(x, file.path(ruta, paste( nombres[contador], ".csv", sep = ''
                                         ) ), col.names= pkg.env$quitarNombres, row.names = F, quote = F, sep = ";" )
    contador <- contador +1 
  }
}



#' Función para cortar las etiquetas cuando son muy largas para las gráficas
#'
#' @param etiquetas Es el vector que contiene las etiquetas del eje x o el eje y según sea el caso
#'
#' @return
#' @export
#'
#' @examples
cortarEtiquetas <- function(etiquetas){
  temp <- ""
  print(paste("Las etiquetas son:", etiquetas))
  etiquetas <- as.character(etiquetas)
  print(paste("Despues como caracter son:", etiquetas))
  for( i in 1:length(etiquetas) ){
    temp <- ""
    for(x in strwrap(etiquetas[i], width = 15)){
      temp <- paste( temp, x, sep = "\n" )
    }
    etiquetas[i] <- substring(temp, first = 2)
    print(etiquetas)
  }
  return(etiquetas)
}