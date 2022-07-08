#'Función encargada de fabricar las graficas para
#'las estadísticas vitales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasTransportes<- function(lista, ruta, modalidad = "trimestral"){
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
  
  
  t3<- graficaLinea(lista$"1_01", escala = "miles", inicio=2000,precision = 0)
  exportarLatex(file.path(ruta,"1_01.tex"), t3)
  
  t4 <- graficaLinea(lista$"1_02", escala = "miles", inicio=2000)
  exportarLatex(file.path(ruta,"1_02.tex"), t4)
  
  
  t6 <- graficaLinea(lista$"1_03", inicio=5000)
  exportarLatex(file.path(ruta,"1_03.tex"), t6)
  
  t7 <- graficaLinea(lista$"1_04", inicio=2000)
  exportarLatex(file.path(ruta,"1_04.tex"), t7)
  
  
  t9 <- graficaLinea(lista$"1_05", escala = "miles", inicio=50)
  exportarLatex(file.path(ruta,"1_05.tex"), t9)
  
  
  t13 <- graficaLinea(lista$"1_06", escala = "miles", inicio=50)
  exportarLatex(file.path(ruta,"1_06.tex"), t13)
  
  
  t10 <- graficaLinea(lista$"1_07", inicio=1000)
  exportarLatex(file.path(ruta,"1_07.tex"), t10)
  
  
  t11 <- graficaLinea(lista$"1_08", inicio=1000)
  exportarLatex(file.path(ruta,"1_08.tex"), t11)
  
  
  
  
  
  
  
  t50   <- graficaLinea(lista$"2_01", inicio=1000)
  exportarLatex(file.path(ruta,"2_01.tex"),t50)
  t51 <- graficaLinea(lista$"2_02", inicio=1000)
  exportarLatex(file.path(ruta,"2_02.tex"),t51)
  
  t62 <- graficaBar(lista$"2_03")
  t62<- etiquetasBarras(t62)
  exportarLatex(file.path(ruta,"2_03.tex"),t62)
  
  t64 <- graficaLinea(lista$"2_04", inicio=500)
  exportarLatex(file.path(ruta,"2_04.tex"),t64)
  t65 <- graficaLinea(lista$"2_05", inicio=500)
  exportarLatex(file.path(ruta,"2_05.tex"),t65)
  
  t76 <- graficaCol(lista$"2_06")
  t76<- etiquetasHorizontales(t76)
  exportarLatex(file.path(ruta,"2_06.tex"),t76)
  
  
  
  
  
  
  t18 <- graficaLinea(lista$"3_01", inicio=10)
  exportarLatex(file.path(ruta,"3_01.tex"), t18)
  
  
  t20   <- graficaLinea(lista$"3_02", inicio=10)
  exportarLatex(file.path(ruta,"3_02.tex"), t20)
  t21 <- graficaLinea(lista$"3_03", inicio=10)
  exportarLatex(file.path(ruta,"3_03.tex"), t21)
  
  
  
  t26 <- graficaColCategorias(lista$"3_04",ruta = file.path(ruta,"3_04.tex"),etiquetasCategorias = "A", etiquetas="h", ejeX = "h",ancho = 0.6)
  
  
  t27 <- graficaLinea(lista$"3_05", inicio=10)
  exportarLatex(file.path(ruta,"3_05.tex"), t27)
  
  t28 <- graficaLinea(lista$"3_06", inicio=10)
  exportarLatex(file.path(ruta,"3_06.tex"), t28)
  
  t29 <- graficaLinea(lista$"3_07", inicio=10)
  exportarLatex(file.path(ruta,"3_07.tex"), t29)
  
  t30 <- graficaLinea(lista$"3_08", inicio=10)
  exportarLatex(file.path(ruta,"3_08.tex"), t30)
  
  t36 <- graficaLinea(lista$"3_09", inicio=10)
  exportarLatex(file.path(ruta,"3_09.tex"), t36)
  
  t37 <- graficaLinea(lista$"3_10", inicio=0)
  exportarLatex(file.path(ruta,"3_10.tex"), t37)
  
  t43 <- graficaLinea(lista$"3_11", inicio=0)
  exportarLatex(file.path(ruta,"3_11.tex"), t43)
  
  t44 <- graficaLinea(lista$"3_12", inicio=0)
  exportarLatex(file.path(ruta,"3_12.tex"), t44)
  
  t45 <- graficaLinea(lista$"3_13", inicio=10)
  exportarLatex(file.path(ruta,"3_13.tex"), t45)
  
  t46 <- graficaLinea(lista$"3_14", inicio=10)
  exportarLatex(file.path(ruta,"3_14.tex"), t46)
  
  
  t47 <- graficaLinea(lista$"3_15", inicio=10)
  exportarLatex(file.path(ruta,"3_15.tex"), t47)
  
  t48 <- graficaLinea(lista$"3_16", inicio=10)
  exportarLatex(file.path(ruta,"3_16.tex"), t48)
  
  
  
  
  
  # 
  # 
  # t25 <- graficaCol(lista$"4_01", ordenar = "F", escala = "miles")
  # t25 <- etiquetasHorizontales(t25)
  # exportarLatex(file.path(ruta,"4_01.tex"), t25)
  
  
  
  t30 <- graficaLinea(lista$"4_01", escala = "miles",inicio = 200)
  exportarLatex(file.path(ruta,"4_01.tex"), t30)
  
  
  
  t32 <- graficaCol(lista$"4_02", ordenar = "F")
  t32 <- etiquetasVerticales(t32)
  t32 <- rotarEtiX2(t32)
  exportarLatex(file.path(ruta,"4_02.tex"), t32)
  
  
  t26 <- graficaCol(lista$"4_03")
  t26 <- etiquetasVerticales(t26)
  t26 <- rotarEtiX2(t26)
  exportarLatex(file.path(ruta,"4_03.tex"), t26)
  # 
  # t27 <- graficaBar(lista$"4_04")
  # t27 <- etiquetasBarras(t27,margenIz = -10.5)
  # exportarLatex(file.path(ruta,"4_04.tex"), t27)
  
  
  t27<- graficaColCategorias(data = lista$"4_04",ancho = 0.45,etiquetas = "v", etiquetasCategorias = "v",ruta = file.path(ruta,"4_04.tex"),preambulo = F)
  
  
  # t29 <- graficaCol(lista$"4_05", ancho = 0.5)
  # t29 <- etiquetasHorizontales(t29)
  # exportarLatex(file.path(ruta,"4_05.tex"), t29)
  
  
  t30 <- graficaLinea(lista$"4_05",inicio = 60)
  exportarLatex(file.path(ruta,"4_05.tex"), t30)
  
  
  t30 <- graficaLinea(lista$"4_06", inicio = 0)
  exportarLatex(file.path(ruta,"4_06.tex"), t30)
  
  t31 <- graficaLinea(lista$"4_07", inicio = 0)
  exportarLatex(file.path(ruta,"4_07.tex"), t31)  
  
 
}