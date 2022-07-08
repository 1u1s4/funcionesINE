#'Función para setear la lista con los data frames para faltas judiciales

setListJudiciales <- function(lista){
  pkg.env$judiciales <- lista
}

#'Función para obtener de nuevo la lista de datos para faltas judiciales

getListJudiciales <- function(){
  return(pkg.env$judiciales)
}

#'Funcion para establecer la ruta en la cual se exportarán los tex con las graficas
#'@param ruta Es la ruta dentro del disco duro donde se almacenaran los csv
setPathJudiciales <- function(ruta){
  pkg.env$rutaJudiciales <- ruta
}

#'Funcion para obtener la ruta en la cual se exportan los tex con las gráficas

getPathJudiciales <- function(){
  return(pkg.env$rutaJudiciales)
}

j01_01 <- function(){
  g <- graficaLinea(data = getListJudiciales()$'1_01')
  exportarLatex(nombre = paste(getPathJudiciales() , "1_01.tex", sep="/"), g)
}

j01_02 <- function(){
  g <- graficaCol(data = getListJudiciales()$'1_02')
  g <- etiquetasVerticales(g)
  g <- rotarEtiX2(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_02.tex", sep="/"), g)
}

j01_03 <- function(){
  g <- graficaCol(data = getListJudiciales()$'1_03')
  g <- etiquetasHorizontales(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_03.tex", sep="/"), g)
}

j01_04 <- function(){
  g <- graficaCol(data = getListJudiciales()$'1_04', ordenar = F)
  g <- etiquetasHorizontales(g)
  g <- rotarEtiX(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_04.tex", sep="/"), g)
}

j01_05 <- function(){
  g <- graficaAnillo(data = getListJudiciales()$'1_05', nombre = paste(getPathJudiciales() , "1_05.tex", sep="/"), preambulo = F)
}

j01_06 <- function(){
  g <- graficaCol(data = getListJudiciales()$'1_06')
  g <- etiquetasHorizontales(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_06.tex", sep="/"), g)
}


j01_07 <- function(){
  g <- graficaCol(data = getListJudiciales()$'1_07')
  g <- etiquetasHorizontales(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_07.tex", sep="/"), g)
}

j01_08 <- function(){
  g <- graficaCol(data = getListJudiciales()$'1_08')
  g <- etiquetasHorizontales(g)
  g <- rotarEtiX(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_08.tex", sep="/"), g)
}

j01_09 <- function(){
  g <- graficaCol(data = getListJudiciales()$'1_09')
  g <- etiquetasHorizontales(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_09.tex", sep="/"), g)
}

j01_10 <- function(){
  g <- graficaBar(data = getListJudiciales()$'1_10')
  g <- etiquetasBarras(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "1_10.tex", sep="/"), g)
}

j01_11 <- function(){
  g <- graficaColCategorias(getListJudiciales()$'1_11', ruta = paste(getPathJudiciales() , "1_11.tex", sep="/"))
}

j02_01 <- function(){
  g <- graficaLinea(data = getListJudiciales()$'2_01')
  exportarLatex(nombre = paste(getPathJudiciales() , "2_01.tex", sep="/"), g)
}

j02_02 <- function(){
  g <- graficaCol(data = getListJudiciales()$'2_02')
  g <- etiquetasVerticales(g)
  g <- rotarEtiX2(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "2_02.tex", sep="/"), g)
}

j02_03 <- function(){
  g <- graficaAnillo(data = getListJudiciales()$'2_03', nombre = paste(getPathJudiciales() , "2_03.tex", sep="/"), preambulo = F)
}

j02_04 <- function(){
  g <- graficaCol(data = getListJudiciales()$'2_04')
  g <- etiquetasHorizontales(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "2_04.tex", sep="/"), g)
}

j02_05 <- function(){
  g <- graficaAnillo(data = getListJudiciales()$'2_05', nombre = paste(getPathJudiciales() , "2_05.tex", sep="/"), preambulo = F)
}

j02_06 <- function(){
  g <- graficaCol(data = getListJudiciales()$'2_06', ordenar = F)
  g <- etiquetasHorizontales(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "2_06.tex", sep="/"), g)
}

j02_07 <- function(){
  g <- graficaCol(data = getListJudiciales()$'2_07')
  g <- etiquetasHorizontales(g)
  exportarLatex(nombre = paste(getPathJudiciales() , "2_07.tex", sep="/"), g)
}

j03_01 <- function(){
  g <- graficaLinea(data = getListJudiciales()$'3_01')
  exportarLatex(nombre = paste(getPathJudiciales() , "3_01.tex", sep="/"), g)
}

j03_02 <- function(){
  g <- graficaLinea(data = getListJudiciales()$'3_02')
  exportarLatex(nombre = paste(getPathJudiciales() , "3_02.tex", sep="/"), g)
}


j03_03 <- function(){
  g <- graficaLinea(data = getListJudiciales()$'3_03')
  exportarLatex(nombre = paste(getPathJudiciales() , "3_03.tex", sep="/"), g)
}


j03_04 <- function(){
  g <- graficaLinea(data = getListJudiciales()$'3_04')
  exportarLatex(nombre = paste(getPathJudiciales() , "3_04.tex", sep="/"), g)
}


j03_05 <- function(){
  g <- graficaLinea(data = getListJudiciales()$'3_05')
  exportarLatex(nombre = paste(getPathJudiciales() , "3_05.tex", sep="/"), g)
}

graficasJudiciales <- function(modalidad = 'trimestral'){
  if( toupper( modalidad ) == 'TRIMESTRAL' ){
    trimestral()
  }else{
    presentacion()
  }
  j01_01()
  j01_02()
  j01_03()
  j01_04()
  j01_05()
  j01_06()
  j01_07()
  j01_08()
  j01_09()
  j01_10()
  j01_11()
  j02_01()
  j02_02()
  j02_03()
  j02_04()
  j02_05()
  j02_06()
  j02_07()
  j03_01()
  j03_02()
  j03_03()
  j03_04()
  j03_04()
  j03_05()
  }

