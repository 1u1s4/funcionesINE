#'Función encargada de fabricar las graficas para
#'las estadísticas de hechos delictivos trimestrales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasDelictivos<- function(lista, ruta, modalidad = "trimestral"){
  pre <- T
  if( toupper(modalidad)  == "TRIMESTRAL"){
    trimestral()
    pre <-T
  }else if ( toupper(modalidad) == "ANUAL"){
    anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  }else {
    presentacion()
    pre <- T
  }
  
  
  g1<- graficaLinea(lista$"1_01", inicio = 500)
  exportarLatex(paste(ruta, "1_01.tex", sep=""),g1)
  
  g2 <- graficaBar(lista$"1_02")
  g2 <- etiquetasBarras(g2, margenIz = 1)
  exportarLatex(paste(ruta, "1_02.tex", sep=""),g2)
  
  
  g3 <- graficaCol(lista$"1_03")
  g3 <- etiquetasVerticales(g3)
  g3 <- rotarEtiX(g3)
  exportarLatex(paste(ruta, "1_03.tex", sep=""),g3)
  
  t6<- graficaAnillo(data = lista$"1_04",
                     nombre =  paste(ruta, "1_04.tex", sep=""), preambulo = F)
  
  
  piramidePoblacional(lista$"1_05", ruta = paste(ruta, "1_05.tex", sep=""),
                          porcentual=F,preambulo = F)
  
  g6 <- graficaBar(lista$"1_06")
  g6<- etiquetasBarras(g6, margenIz = 1)
  exportarLatex(paste(ruta, "1_06.tex", sep=""),g6)
  
  g7 <- graficaBar(lista$"1_07")
  g7 <- etiquetasBarras(g7, margenIz = -3)
  exportarLatex(paste(ruta, "1_07.tex", sep=""),g7)
  
  
  
  
  g8 <- graficaCol(lista$"1_08")
  g8 <- etiquetasVerticales(g8)
  g8 <- rotarEtiX(g8)
  exportarLatex(paste(ruta, "1_08.tex", sep=""),g8)
  
  
  t6<- graficaAnillo(data = lista$"1_09",
                     nombre =  paste(ruta, "1_09.tex", sep=""), preambulo = F)
  
  
  
  g10<-piramidePoblacional(lista$"1_10", ruta = paste(ruta, "1_10.tex", sep=""),
                           porcentual=F,preambulo = F)
  
  
  
  
  
  
  
  
  g11<- graficaLinea(lista$"2_01", inicio = 500)
  exportarLatex(paste(ruta, "2_01.tex", sep=""),g11)
  
  
  g12 <- graficaBar(lista$"2_02")
  g12 <- etiquetasBarras(g12, margenIz = 1)
  exportarLatex(paste(ruta, "2_02.tex", sep=""),g12)
  
  
  g13 <- graficaCol(lista$"2_03")
  g13 <- etiquetasVerticales(g13)
  g13 <- rotarEtiX2(g13)
  exportarLatex(paste(ruta, "2_03.tex", sep=""),g13)
  
  t14<- graficaAnillo(data = lista$"2_04",
                      nombre =  paste(ruta, "2_04.tex", sep=""), preambulo = F)
  
  g15<-piramidePoblacional(lista$"2_05", ruta = paste(ruta, "2_05.tex", sep=""),porcentual = F,
                           preambulo = F)
  
  g16 <- graficaBar(lista$"2_06")
  g16<- etiquetasBarras(g16)
  exportarLatex(paste(ruta, "2_06.tex", sep=""),g16)
  
  g17 <- graficaBar(lista$"2_07")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste(ruta, "2_07.tex", sep=""),g17)
  
  
  
  
  
  
  
  g8 <- graficaCol(lista$"2_08")
  g8 <- etiquetasVerticales(g8)
  g8 <- rotarEtiX(g8)
  exportarLatex(paste(ruta, "2_08.tex", sep=""),g8)
  
  
  t6<- graficaAnillo(data = lista$"2_09",
                     nombre =  paste(ruta, "2_09.tex", sep=""), preambulo = F)
  
  
  
  g10<-piramidePoblacional(lista$"2_10", ruta = paste(ruta, "2_10.tex", sep=""),
                           porcentual=F, preambulo = F)
  
  
  
  
  
  
  
  
  
  
  
  
  g18<- graficaLinea(lista$"3_01", inicio=0)
  exportarLatex(paste(ruta, "3_01.tex", sep=""),g18)
  
  
  g19 <- graficaBar(lista$"3_02")
  g19 <- etiquetasBarras(g19, margenIz = -3)
  exportarLatex(paste(ruta, "3_02.tex", sep=""),g19)
  
  
  g20 <- graficaCol(lista$"3_03")
  g20 <- etiquetasVerticales(g20)
  g20 <- rotarEtiX(g20)
  exportarLatex(paste(ruta, "3_03.tex", sep=""),g20)
  
  
#   g200 <- graficaCol(lista$"3_04")
#   g200 <- etiquetasHorizontales(g200)
#   exportarLatex(paste(ruta, "3_04.tex", sep=""),g200)
#   
  
  
  graficaAnillo(data = lista$"3_05",
                       nombre =  paste(ruta, "3_05.tex", sep=""), preambulo = F)
  
  
  g10<-piramidePoblacional(lista$"3_06", ruta = paste(ruta, "3_06.tex", sep=""),
                           porcentual=F,preambulo = F)
  
  
  
  
  g21<- graficaLinea(lista$"4_01", inicio = 0)
  exportarLatex(paste(ruta, "4_01.tex", sep=""),g21)
  
  
  g22 <- graficaBar(lista$"4_02")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste(ruta, "4_02.tex", sep=""),g22)
  
  
  g23 <- graficaCol(lista$"4_03")
  g23 <- etiquetasVerticales(g23)
  g23 <- rotarEtiX(g23)
  exportarLatex(paste(ruta, "4_03.tex", sep=""),g23)
  
  
  t6<- graficaAnillo(data = lista$"4_04",
                     nombre =  paste(ruta, "4_04.tex", sep=""), preambulo = F)

  
  
  
  g25 <- graficaCol(lista$"4_05",ordenar = F)
  g25 <- etiquetasHorizontales(g25)
  g25 <- rotarEtiX(g25)
  exportarLatex(paste(ruta, "4_05.tex", sep=""),g25)
  
  
  
  
  
  
  g31<- graficaLinea(lista$"5_01", inicio = 0)
  exportarLatex(paste(ruta, "5_01.tex", sep=""),g31)
  
  
  g32 <- graficaBar(lista$"5_02")
  g32 <- etiquetasBarras(g32)
  exportarLatex(paste(ruta, "5_02.tex", sep=""),g32)
  
  
  g33 <- graficaCol(lista$"5_03")
  g33 <- etiquetasHorizontales(g33)
  g33 <- rotarEtiX(g33)
  exportarLatex(paste(ruta, "5_03.tex", sep=""),g33)
  
  
  t6<- graficaAnillo(data = lista$"5_04",
                     nombre =  paste(ruta, "5_04.tex", sep=""), preambulo = F)
  
  
  g35 <- graficaCol(lista$"5_05", ordenar = F)
  g35<- etiquetasHorizontales(g35)
  g35 <- rotarEtiX(g35)
  exportarLatex(paste(ruta, "5_05.tex", sep=""),g35)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  g18<- graficaLinea(lista$"6_01", inicio=0)
  exportarLatex(paste(ruta, "6_01.tex", sep=""),g18)
  
  
  t333<- graficaAnillo(data = lista$"6_02",
                       nombre =  paste(ruta, "6_02.tex", sep=""), preambulo = F)
  
  
  g20 <- graficaCol(lista$"6_03")
  g20 <- etiquetasVerticales(g20)
  g20 <- rotarEtiX(g20)
  exportarLatex(paste(ruta, "6_03.tex", sep=""),g20)
  
  
  t334<- graficaAnillo(data = lista$"6_04",
                       nombre =  paste(ruta, "6_04.tex", sep=""), preambulo = F)
  
  
  graficaBarFacets(lista$"6_05", ruta = paste(ruta, "6_05.tex", sep=""),
                         escala = 'normal', etiquetas = 'H', preambulo = F)
  
  
  
  g20 <- graficaBar(lista$"6_06")
  g20 <- etiquetasBarras(g20)
  exportarLatex(paste(ruta, "6_06.tex", sep=""),g20)
  
  
  g20 <- graficaBar(lista$"6_07")
  g20 <- etiquetasBarras(g20)
  exportarLatex(paste(ruta, "6_07.tex", sep=""),g20)
  
  
  
  g20 <- graficaCol(lista$"6_08")
  g20 <- etiquetasHorizontales(g20)
  g20 <- rotarEtiX(g20)
  exportarLatex(paste(ruta, "6_08.tex", sep=""),g20)
  
  
  graficaAnillo(lista$'6_09', nombre = file.path(ruta, '6_09.tex'), preambulo = F)
  
  
  
  g20 <- graficaBar(lista$"6_11")
  g20 <- etiquetasBarras(g20)
  exportarLatex(paste(ruta, "6_11.tex", sep=""),g20)
  
  graficaBarFacets(lista$'6_10', ruta = file.path(ruta, "6_10.tex"), preambulo = F)
  
  
  
  g11 <-graficaBar(lista$"6_12")
  g11 <- etiquetasBarras(g11)
  exportarLatex(file.path(ruta, '6_12.tex'), g11)
  

  
  
}