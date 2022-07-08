#'Función encargada de fabricar las graficas para
#'las estadísticas hospitalarias trimestrales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasHospitalarias<- function(lista, ruta, modalidad = "trimestral"){
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
  
  
  t1 <- graficaLinea(lista$"1_01", escala = "miles",inicio = 0)
  exportarLatex(paste(ruta, "1_01.tex", sep=""), t1)
  
  t2 <- graficaCol(lista$"1_02",escala = "miles")
  t2 <- etiquetasVerticales(t2)
  t2 <- rotarEtiX2(t2)
  exportarLatex(paste(ruta, "1_02.tex", sep=""), t2)
  
  
  t3 <- graficaCol(lista$"1_03", ordenar = F)
  t3 <- etiquetasHorizontales(t3)
  t3 <- rotarEtiX(t3)
  exportarLatex(paste(ruta, "1_03.tex", sep=""), t3)
  
  t4 <- graficaCol(lista$"1_04", ordenar = F)
  t4 <- etiquetasHorizontales(t4)
  t4 <- rotarEtiX(t4)
  exportarLatex(paste(ruta, "1_04.tex", sep=""), t4)
  
  graficaAnillo(lista$"1_05",nombre = paste(ruta, "1_05.tex", sep=""), preambulo = F)
  
  
  t6<- graficaAnillo(data = lista$"1_06",
                     nombre =  paste(ruta, "1_06.tex", sep=""), preambulo = FALSE)
  
  
  
  t7 <- graficaCol(lista$"1_07")
  t7 <- etiquetasHorizontales(t7)
  exportarLatex(paste(ruta, "1_07.tex", sep=""), t7)
  
  
  
  t8 <- graficaLinea(lista$"2_01", inicio = 0)
  exportarLatex(paste(ruta, "2_01.tex", sep=""), t8)
  
  
  t9 <- graficaCol(lista$"2_02")
  t9 <- etiquetasVerticales(t9)
  t9 <- rotarEtiX2(t9)
  exportarLatex(paste(ruta, "2_02.tex", sep=""), t9)
  
  
  t10 <- graficaCol(lista$"2_03", ordenar = F)
  t10 <- etiquetasHorizontales(t10)
  t10 <- rotarEtiX2(t10)
  exportarLatex(paste(ruta, "2_03.tex", sep=""), t10)
  
  
  t11 <- graficaLinea(lista$"2_04", inicio = 0)
  exportarLatex(paste(ruta, "2_04.tex", sep=""), t11)
  
  
  
  t12<- graficaAnillo(data = lista$"2_05",
                      nombre =  paste(ruta, "2_05.tex", sep=""), preambulo = FALSE)
  
  t13 <- graficaCol(lista$"2_06")
  t13 <- etiquetasHorizontales(t13)
  exportarLatex(paste(ruta, "2_06.tex", sep=""), t13)
  
  
  t14 <- graficaCol(lista$"2_07")
  t14 <- etiquetasHorizontales(t14)
  exportarLatex(paste(ruta, "2_07.tex", sep=""), t14)
  
  
  
  t16 <- graficaLinea(lista$"2_08", inicio = 0)
  exportarLatex(paste(ruta, "2_08.tex", sep=""), t16)
  
  
  
  
  t17 <- graficaCol(lista$"2_09")
  t17 <- etiquetasHorizontales(t17)
  exportarLatex(paste(ruta, "2_09.tex", sep=""), t17)
  
  
}