#'Función encargada de fabricar las graficas para
#'las estadísticas vitales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasVitales<- function(lista, ruta, modalidad = "trimestral"){
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
  t1 <- graficaLinea(lista$"1_01")
  exportarLatex(paste(ruta,"1_01.tex", sep=""), t1)
  
  t2 <- graficaCol(lista$"1_02")
t2 <- etiquetasVerticales(t2)
  t2 <- rotarEtiX2(t2)
  exportarLatex(paste(ruta,"1_02.tex", sep=""), t2)

  
  t3 <- graficaCol(lista$"1_03", ordenar = F)
  t3 <- etiquetasHorizontales(t3)
  t3 <- rotarEtiX(t3)
  exportarLatex(paste(ruta,"1_03.tex", sep=""), t3)
  
  t4 <- graficaCol(lista$"1_04", ordenar = F)
  t4 <- etiquetasHorizontales(t4)
  exportarLatex(paste(ruta,"1_04.tex", sep=""), t4)
  
  t5 <- graficaBar(lista$"1_05")
  t5 <- etiquetasBarras(t5)
  exportarLatex(paste(ruta,"1_05.tex", sep=""), t5)
  
  t6 <- graficaCol(lista$"1_06", ancho = 0.4)
  t6 <- etiquetasHorizontales(t6)
  exportarLatex(paste(ruta,"1_06.tex", sep=""), t6)
  
   t7 <- graficaAnillo(lista$"1_07", paste(ruta,"1_07.tex", sep=""), preambulo = pre)
  if( pre == T ){
    compilar(paste(ruta,"1_07.tex", sep=""),F)  
  } 
  
  
  t8 <- graficaBar(lista$"1_08")
  t8 <- etiquetasBarras(t8)
  exportarLatex(paste(ruta,"1_08.tex", sep=""), t8)
  
  
  
  
  t9 <- graficaLinea(lista$"1_09")
  exportarLatex(paste(ruta,"1_09.tex", sep=""), t9)
  
  
  t10 <- graficaCol(lista$"1_10")
  t10 <- etiquetasHorizontales(t10)
  exportarLatex(paste(ruta,"1_10.tex", sep=""), t10)
  
  
  t11 <- graficaLinea(lista$"2_01")
  exportarLatex(paste(ruta,"2_01.tex", sep=""), t11)
  
  
  t12 <- graficaCol(lista$"2_02")
  t12 <- etiquetasVerticales(t12)
  t12 <- rotarEtiX2(t12)
  exportarLatex(paste(ruta,"2_02.tex", sep=""), t12)
  
  
  t13 <- graficaCol(lista$"2_03", ordenar = F)
  t13 <- etiquetasHorizontales(t13)
  exportarLatex(paste(ruta,"2_03.tex", sep=""), t13)
  
  #pkg.env$modalidad = modalidad
  t14 <- graficaColCategorias(lista$'2_04', ruta = paste(ruta,"2_04.tex", sep=""), preambulo = pre, etiquetas = "h", ancho = 0.5)
  if( pre == T){
    compilar(paste(ruta,"2_04.tex", sep=""), F) 
  }
  #pkg.env$modalidad = "trimestral"
  
   t15 <- graficaLinea(lista$"2_05")
   exportarLatex(paste(ruta,"2_05.tex", sep=""), t15 )
  
  
  t16 <- graficaCol(lista$"2_06")
  t16 <- etiquetasHorizontales(t16)
  exportarLatex(paste(ruta,"2_06.tex", sep=""), t16)
  
  
  t17 <- graficaBar(lista$"2_07")
  t17 <- etiquetasBarras(t17)
  exportarLatex(paste(ruta,"2_07.tex", sep=""), t17)
  
  
  t18 <- graficaLinea(lista$"3_01")
  exportarLatex(paste(ruta,"3_01.tex", sep=""), t18)
  
  t19 <- graficaCol(lista$"3_02")
  t19 <- etiquetasVerticales(t19)
  t19 <- rotarEtiX2(t19)
  exportarLatex(paste(ruta,"3_02.tex", sep=""), t19)
  
  t20 <- graficaCol(lista$"3_03", ordenar = F)
  t20 <- etiquetasHorizontales(t20)
  exportarLatex(paste(ruta,"3_03.tex", sep=""), t20)
  
  t21 <- graficaCol(lista$"3_04", ordenar = F)
  t21 <- etiquetasHorizontales(t21)
  t21 <- rotarEtiX(t21)
  exportarLatex(paste(ruta,"3_04.tex", sep=""), t21)
  
  t22 <- graficaBar(lista$"3_05")
  t22 <- etiquetasBarras(t22)
  exportarLatex(paste(ruta,"3_05.tex", sep=""), t22)
  
  t23 <- graficaCol(lista$"3_06")
  t23 <- etiquetasHorizontales(t23)
  exportarLatex(paste(ruta,"3_06.tex", sep=""), t23)
  
  t24 <- graficaBar(lista$"3_07")
  t24 <- etiquetasBarras(t24)
  exportarLatex(paste(ruta,"3_07.tex", sep=""), t24)
  
  t25 <- graficaLinea(lista$"4_01")
  exportarLatex(paste(ruta,"4_01.tex", sep=""), t25)
  
  t26 <- graficaCol(lista$"4_02")
  t26 <- etiquetasVerticales(t26)
  t26 <- rotarEtiX2(t26)
  exportarLatex(paste(ruta,"4_02.tex", sep=""), t26)
  
  t27 <- graficaCol(lista$"4_03", ordenar = F)
  t27 <- etiquetasHorizontales(t27)
  exportarLatex(paste(ruta,"4_03.tex", sep=""), t27)
  
  t28 <- graficaBar(lista$"4_04")
  t28 <- etiquetasBarras(t28)
  exportarLatex(paste(ruta,"4_04.tex", sep=""), t28)
  
  
  t29 <- graficaLinea(lista$"5_01")
  exportarLatex(paste(ruta,"5_01.tex", sep=""), t29)
  
  t30 <- graficaCol(lista$"5_02")
  t30 <- etiquetasVerticales(t30)
  t30 <- rotarEtiX2(t30)
  exportarLatex(paste(ruta,"5_02.tex", sep=""), t30)
}