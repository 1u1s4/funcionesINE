#'Función encargada de fabricar las graficas para
#'las estadísticas de transito  trimestrales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasTransito<- function(lista, ruta, modalidad = "trimestral"){
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
  
  g1<- graficaLinea(lista$"1_01", inicio = 0)
  exportarLatex(paste(ruta,"1_01.tex", sep = ""),g1)
  
  
  g2 <- graficaCol(lista$"1_02")
  g2 <- etiquetasHorizontales(g2)
  g2 <- rotarEtiX(g2)
  exportarLatex(paste(ruta,"1_02.tex", sep = ""),g2)
  
  
  g3 <- graficaCol(lista$"1_03",ordenar = F)
  g3 <- etiquetasHorizontales(g3)
  exportarLatex(paste(ruta,"1_03.tex", sep = ""),g3)
  
  g4 <- graficaCol(lista$"1_04")
  g4 <- etiquetasHorizontales(g4)
  exportarLatex(paste(ruta,"1_04.tex", sep = ""),g4)
  
  g5 <- graficaLinea(lista$"1_05", inicio = 0)
  exportarLatex(paste(ruta,"1_05.tex", sep = ""),g5)
  
  
  g6 <- graficaCol(lista$"1_06")
  g6<- etiquetasHorizontales(g6)
  g6 <- rotarEtiX(g6)
  exportarLatex(paste(ruta,"1_06.tex", sep = ""),g6)
  
  graficaAnillo(lista$"1_07", paste(ruta, '1_07.tex', sep = ""),preambulo = F)
  
  g8 <- graficaCol(lista$"1_08",ordenar = F)
  g8 <- etiquetasHorizontales(g8)
  g8<- rotarEtiX(g8)
  exportarLatex(paste(ruta,"1_08.tex", sep = ""),g8)
  
  g9 <- graficaCol(lista$"1_09",ancho = .5)
  g9 <- etiquetasHorizontales(g9)
  exportarLatex(paste(ruta,"1_09.tex", sep = ""),g9)
  
  g10<- graficaLinea(lista$"1_10", inicio = 0)
  exportarLatex(paste(ruta,"1_10.tex", sep = ""),g10)
  
  
  
  g0 <- graficaLinea(lista$"2_01", inicio = 0)
  exportarLatex(paste(ruta,"2_01.tex", sep = ""),g0)
  
  
  
  graficaAnillo(lista$"2_02", paste(ruta, '2_02.tex', sep = ""),preambulo = F)
  
  
  g12 <- graficaLinea(lista$"2_03", inicio = 0)
  exportarLatex(paste(ruta,"2_03.tex", sep = ""),g12)
  
  
  g13 <- graficaCol(lista$"2_04")
  g13 <- etiquetasHorizontales(g13)
  g13 <- rotarEtiX(g13)
  exportarLatex(paste(ruta,"2_04.tex", sep = ""),g13)
  
  graficaAnillo(lista$"2_05", paste(ruta,'2_05.tex', sep=""),preambulo = F)
  
  g15 <- graficaCol(lista$"2_06", ordenar = F)
  g15<- etiquetasHorizontales(g15)
  g15<- rotarEtiX(g15)
  exportarLatex(paste(ruta,"2_06.tex", sep = ""),g15)
  
  g16 <- graficaLinea(lista$"2_07", inicio = 0)
  exportarLatex(paste(ruta,"2_07.tex", sep = ""),g16)
  
  g17 <- graficaCol(lista$"2_08")
  g17 <- etiquetasHorizontales(g17)
  g17<- rotarEtiX(g17)
  exportarLatex(paste(ruta,"2_08.tex", sep = ""),g17)
  
  graficaAnillo(lista$"2_09", paste(ruta, '2_09.tex', sep = ""),preambulo = F)
  
  
  g19 <- graficaCol(lista$"2_10",ordenar = F)
  g19 <- etiquetasHorizontales(g19)
  g19 <- rotarEtiX(g19)
  exportarLatex(paste(ruta,"2_10.tex", sep = ""),g19)
  
  
  g21<- graficaLinea(lista$"3_01", inicio = 0)
  exportarLatex(paste(ruta,"3_01.tex", sep = ""),g21)
  
  
  g22 <- graficaCol(lista$"3_02")
  g22<- etiquetasHorizontales(g22)
  g22<- rotarEtiX(g22)
  exportarLatex(paste(ruta,"3_02.tex", sep = ""),g22)
  
  
  g23 <- graficaCol(lista$"3_03", ancho = 0.5, ordenar = F)
  g23 <- etiquetasHorizontales(g23)
  g23 <- rotarEtiX(g23)
  exportarLatex(paste(ruta,"3_03.tex", sep = ""),g23)
  
  g24<- graficaCol(lista$"3_04", ordenar = F)
  g24<- etiquetasHorizontales(g24)
  g24<- rotarEtiX(g24)
  exportarLatex(paste(ruta,"3_04.tex", sep = ""),g24)
  
  g25 <- graficaCol(lista$"3_05", ancho = 0.5)
  g25<- etiquetasHorizontales(g25)
  exportarLatex(paste(ruta,"3_05.tex", sep = ""),g25)
  
  g26 <- graficaLinea(lista$"3_06", inicio = 0)
  exportarLatex(paste(ruta,"3_06.tex", sep = ""),g26)
  
  g27 <- graficaCol(lista$"3_07")
  g27 <- etiquetasHorizontales(g27)
  g27<- rotarEtiX(g27)
  exportarLatex(paste(ruta,"3_07.tex", sep = ""),g27)
  
  
  graficaAnillo(lista$"3_08", paste(ruta,'3_08.tex',sep = ""),preambulo= F)
  
  
  g29 <- graficaCol(lista$"3_09",ordenar = F)
  g29 <- etiquetasHorizontales(g29)
  g29 <- rotarEtiX(g29)
  exportarLatex(paste(ruta,"3_09.tex", sep = ""),g29)
  
  
  g5<-piramidePoblacional(lista$"3_10", ruta = paste(ruta,"3_10.tex", sep = ""),
                          porcentual=F,preambulo = F)
  
  
  g32 <- graficaLinea(lista$"3_11", inicio = 0)
  exportarLatex(paste(ruta,"3_11.tex", sep = ""),g32)
  
  
  g33 <- graficaCol(lista$"3_12")
  g33 <- etiquetasVerticales(g33)
  g33 <- rotarEtiX(g33)
  exportarLatex(paste(ruta,"3_12.tex", sep = ""),g33)
  
  
  graficaAnillo(lista$"3_13",paste(ruta,"3_13.tex", sep = ""),preambulo = F)####

  
  
  g35 <- graficaCol(lista$"3_14",ordenar = F)
  g35 <- etiquetasHorizontales(g35)
  g35 <- rotarEtiX(g35)
  exportarLatex(paste(ruta,"3_14.tex", sep = ""),g35)
  
  
  g45<-piramidePoblacional(lista$"3_15", ruta = paste(ruta,"3_15.tex", sep = ""),
                           porcentual=F,preambulo = F)
  
  
  
  
  
}