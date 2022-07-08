#'Función encargada de fabricar las graficas para
#'las estadísticas hospitalarias trimestrales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasVIF<- function(lista, ruta, modalidad = "trimestral"){
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
  
  
  t1 <- graficaLineaTrim(lista$"1_01")
  exportarLatex(paste(ruta, "1_01.tex", sep=""), t1)
  
  t2 <- graficaCol(lista$"1_02")
  t2 <- etiquetasVerticales(t2)
  t2 <- rotarEtiX2(t2)
  exportarLatex(paste(ruta, "1_02.tex", sep=""), t2)
  
  t3<- graficaBar(lista$"1_03")
  t3 <- etiquetasBarras(t3, margenIz = -5)
  exportarLatex(paste(ruta, "1_03.tex", sep=""), t3)
  
  t4<- graficaBar(lista$"1_04")
  t4 <- etiquetasBarras(t4, margenIz = -8 )
  exportarLatex(paste(ruta, "1_04.tex", sep=""), t4)
  
  
  t5 <- graficaBar(lista$"1_05")
  t5 <- etiquetasBarras(t5, margenIz = 2)
  exportarLatex(paste(ruta, "1_05.tex", sep=""), t5)
  
  
  
  t6 <- graficaLineaTrim(lista$"2_01")
  exportarLatex(paste(ruta, "2_01.tex", sep=""), t6)
  
  
  t7 <- graficaCol(lista$"2_02")
  t7 <- etiquetasHorizontales(t7)
  t7 <- rotarEtiX2(t7)
  exportarLatex(paste(ruta, "2_02.tex", sep=""), t7)
  
  
  t8 <- graficaCol(lista$"2_03", ordenar = F)
  t8 <- etiquetasHorizontales(t8)
  t8 <- rotarEtiX(t8)
  exportarLatex(paste(ruta, "2_03.tex", sep=""), t8)
  
  
  t9 <- graficaBar(lista$"2_04")
  t9 <- etiquetasBarras(t9, margenIz = -8)
  exportarLatex(paste(ruta, "2_04.tex", sep=""), t9)
  
  
  t10 <- graficaCol(lista$"2_05")
  t10 <- etiquetasHorizontales(t10)
  exportarLatex(paste(ruta, "2_05.tex", sep=""), t10)
  
  
  t11 <- graficaBar(lista$"2_06")
  t11 <- etiquetasBarras(t11,margenIz = -6)
  exportarLatex(paste(ruta, "2_06.tex", sep=""), t11)
  
  
  t12 <- graficaBar(lista$"2_07")
  t12 <- etiquetasBarras(t12, margenIz=2.5)
  exportarLatex(paste(ruta, "2_07.tex", sep=""), t12)
  
  
  t13 <- graficaCol(lista$"3_01", ordenar = F)
  t13 <- etiquetasHorizontales(t13)
  t13 <- rotarEtiX(t13)
  exportarLatex(paste(ruta, "3_01.tex", sep=""), t13)
  
  t14 <- graficaBar(lista$"3_02")
  t14 <- etiquetasBarras(t14,margenIz = 2)
  exportarLatex(paste(ruta, "3_02.tex", sep=""), t14)
  
  t15 <- graficaBar(lista$"3_03")
  t15 <- etiquetasBarras(t15,margenIz = 2.1)
  exportarLatex(paste(ruta, "3_03.tex", sep=""), t15)
  
  t16 <- graficaCol(lista$"3_04", ordenar = F)
  t16 <- etiquetasHorizontales(t16)
  exportarLatex(paste(ruta, "3_04.tex", sep=""), t16)
  
  t17 <- graficaBar(lista$"3_05")
  t17 <- etiquetasBarras(t17,margenIz = -10)
  exportarLatex(paste(ruta, "3_05.tex", sep=""), t17)
  
  t18 <- graficaBar(lista$"3_06")
  t18 <- etiquetasBarras(t18, margenIz = -6)
  exportarLatex(paste(ruta, "3_06.tex", sep=""), t18)
  
  
  t19 <- graficaLineaTrim(lista$"4_01")
  exportarLatex(paste(ruta, "4_01.tex", sep=""), t19)
  
  t20 <- graficaCol(lista$"4_02", ordenar=F)
  t20 <- etiquetasVerticales(t20)
  t20 <- rotarEtiX2(t20)
  exportarLatex(paste(ruta, "4_02.tex", sep=""), t20)
  
  t21 <- graficaCol(lista$"4_03", ordenar = F)
  t21 <- etiquetasHorizontales(t21)
  t21 <- rotarEtiX(t21)
  exportarLatex(paste(ruta, "4_03.tex", sep=""), t21)
  
  t22 <- graficaBar(lista$"4_04")
  t22 <- etiquetasBarras(t22,margenIz = -10)
  exportarLatex(paste(ruta, "4_04.tex", sep=""), t22)
  
  t23 <- graficaCol(lista$"4_05")
  t23 <- etiquetasHorizontales(t23)
  t23 <- rotarEtiX(t23)
  exportarLatex(paste(ruta, "4_05.tex", sep=""), t23)
  
  t24 <- graficaBar(lista$"4_06")
  t24 <- etiquetasBarras(t24, margenIz=-5)
  exportarLatex(paste(ruta, "4_06.tex", sep=""), t24)
  
  
  t25 <- graficaCol(lista$"5_01",ordenar = F)
  t25 <- etiquetasHorizontales(t25)
  t25 <- rotarEtiX(t25)
  exportarLatex(paste(ruta, "5_01.tex", sep=""), t25)
  
  t26 <- graficaCol(lista$"5_02", ordenar = F)
  t26 <- etiquetasHorizontales(t26)
  t26 <- rotarEtiX2(t26)
  exportarLatex(paste(ruta, "5_02.tex", sep=""), t26)
  
  t27 <- graficaBar(lista$"5_03")
  t27 <- etiquetasBarras(t27, margenIz = -10)
  exportarLatex(paste(ruta, "5_03.tex", sep=""), t27)
  
  t28 <- graficaCol(lista$"5_04")
  t28 <- etiquetasHorizontales(t28)
  t28 <- rotarEtiX2(t28)
  exportarLatex(paste(ruta, "5_04.tex", sep=""), t28)
  
  
  t29 <- graficaBar(lista$"5_05")
  t29 <- etiquetasBarras(t29, margenIz = -5.5)
  exportarLatex(paste(ruta, "5_05.tex", sep=""), t29)
  
}