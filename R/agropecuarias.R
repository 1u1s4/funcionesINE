#'Función encargada de fabricar las graficas para
#'las estadísticas agropecuarias trimestrales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasAgropecuarias<- function(lista, ruta, modalidad = "trimestral"){
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
  
  
  
#   t25 <- graficaCol(lista$"1_01", ordenar = F, escala = "millones")
#   t25 <- etiquetasHorizontales(t25)
#   t25 <- rotarEtiX(t25)
#   exportarLatex(paste(ruta, "1_01.tex", sep = ""), t25)
#   
#   t26 <- graficaCol(lista$"1_02", ordenar = F, escala = "millones")
#   t26 <- etiquetasHorizontales(t26)
#   t26 <- rotarEtiX(t26)
#   exportarLatex(paste(ruta, "1_02.tex", sep = ""), t26)
#   
#   t27 <- graficaCol(lista$"1_03", ordenar = F, escala = "millones")
#   t27 <- etiquetasHorizontales(t27)
#   t27 <- rotarEtiX(t27)
#   exportarLatex(paste(ruta, "1_03.tex", sep = ""), t27)
  
  
  
  t4 <- graficaLinea(lista$"1_04", escala = "miles", inicio = 0,precision = 0)
  exportarLatex(paste(ruta, "1_04.tex", sep = ""), t4)
  
  
  
  t18 <- graficaLinea(lista$"1_05", escala = "miles", inicio = 0,precision = 0)
  exportarLatex(paste(ruta, "1_05.tex", sep = ""), t18)
  
  t19 <- graficaLinea(lista$"1_06", escala = "miles", inicio = 0,precision = 0)
  exportarLatex(paste(ruta, "1_06.tex", sep = ""), t19)
  
  t20 <- graficaLinea(lista$"1_07", escala = "miles", inicio = 0,precision = 0)
  exportarLatex(paste(ruta, "1_07.tex", sep = ""), t20)
  
  t21 <- graficaLinea(lista$"1_08", escala = "miles", inicio = 0,precision = 0)
  exportarLatex(paste(ruta, "1_08.tex", sep = ""), t21)
  
  t22 <- graficaLinea(lista$"1_09", inicio = 0,precision = 0)
  exportarLatex(paste(ruta, "1_09.tex", sep = ""), t22)
  
  
  t5 <- graficaLinea(lista$"1_10", inicio = 0, escala = "miles",precision = 0)
  exportarLatex(paste(ruta, "1_10.tex", sep = ""), t5)
  
  
  
  t8 <- graficaLinea(lista$"2_01", inicio = 0)
  exportarLatex(paste(ruta, "2_01.tex", sep = ""), t8)
  
  
  t9 <- graficaLinea(lista$"2_02", inicio = 0)
  exportarLatex(paste(ruta, "2_02.tex", sep = ""), t9)
  
  
  t10 <- graficaLinea(lista$"2_03", inicio = 0)
  exportarLatex(paste(ruta, "2_03.tex", sep = ""), t10)
  
  
  t11 <- graficaLinea(lista$"2_04", inicio = 0)
  exportarLatex(paste(ruta, "2_04.tex", sep = ""), t11)
  
  
  
}