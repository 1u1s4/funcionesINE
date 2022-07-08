#'Función encargada de establecer la lista donde estarán alojadados los datos
#'para la cb

setListCB <- function(lista){
  pkg.env$cb <- lista
}


#'Función para obtener de nuevo la lista de datos para la cb

getListCB <- function(){
  return(pkg.env$cb)
}

#'Funcion para establecer la ruta en la cual se exportarán los csv
#'@param ruta Es la ruta dentro del disco duro donde se almacenaran los csv
setPath <- function(ruta){
  pkg.env$rutaCB <- ruta
}

#'Funcion para obtener la ruta en la cual se exportan los csv

getPath <- function(){
  return(pkg.env$rutaCB)
}

#'Funcion para generar las graficas de la canasta básica
graficasCanasta <- function(modalidad = 'trimestral'){
  if( modalidad == 'trimestral'){
    pkg.env$modalidad = 'trimestral'
  }else{
    pkg.env$modalidad = 'presentacion'
  }
  capitulo1CB()
  capitulo2CB()
}


#'Funcion para hacer las graficas del capitulo 1
capitulo1CB <- function(){
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  cuatroEtiquetas()
  g1<- graficaLinea(getListIpc()$"1_01")
  exportarLatex(paste(getPath() , "1_01.tex", sep = "/"),g1)
  g2<- graficaLinea(getListIpc()$"1_02", precision = 2)
  exportarLatex(paste(getPath() , "1_02.tex", sep = "/"),g2)
}


#'Fucnion para hacer el capitulo 2

capitulo2CB <- function(){
  trimestral()
  cuatroEtiquetas()
  print("Este es el capitulo2")
  print(getListCB()$"2_03")
  print(getListCB())
  
  g<- graficaLinea(getListCB()$"2_01")
  exportarLatex(paste(getPath(),"2_01.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_02")
  exportarLatex(paste(getPath(),"2_02.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_03")
  exportarLatex(paste(getPath(),"2_03.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_04")
  exportarLatex(paste(getPath(),"2_04.tex",sep="/"),g, preambulo = T )

  
  g<- graficaLinea(getListCB()$"2_05")
  exportarLatex(paste(getPath(),"2_05.tex",sep="/"),g, preambulo = T )
  
  
  
  g<- graficaLinea(getListCB()$"2_06")
  exportarLatex(paste(getPath(),"2_06.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_06")
  exportarLatex(paste(getPath(),"2_06.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_07")
  exportarLatex(paste(getPath(),"2_07.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_08")
  exportarLatex(paste(getPath(),"2_08.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_09")
  exportarLatex(paste(getPath(),"2_09.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_10")
  exportarLatex(paste(getPath(),"2_11.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_12")
  exportarLatex(paste(getPath(),"2_13.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_14")
  exportarLatex(paste(getPath(),"2_14.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_15")
  exportarLatex(paste(getPath(),"2_16.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_17")
  exportarLatex(paste(getPath(),"2_17.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_18")
  exportarLatex(paste(getPath(),"2_18.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_19")
  exportarLatex(paste(getPath(),"2_19.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_20")
  exportarLatex(paste(getPath(),"2_20.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_21")
  exportarLatex(paste(getPath(),"2_21.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_22")
  exportarLatex(paste(getPath(),"2_22.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_23")
  exportarLatex(paste(getPath(),"2_23.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_24")
  exportarLatex(paste(getPath(),"2_24.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_25")
  exportarLatex(paste(getPath(),"2_25.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_26")
  exportarLatex(paste(getPath(),"2_26.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_27")
  exportarLatex(paste(getPath(),"2_27.tex",sep="/"),g, preambulo = T )
  

  g<- graficaLinea(getListCB()$"2_28")
  exportarLatex(paste(getPath(),"2_28.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_29")
  exportarLatex(paste(getPath(),"2_29.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_30")
  exportarLatex(paste(getPath(),"2_30.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_31")
  exportarLatex(paste(getPath(),"2_31.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_32")
  exportarLatex(paste(getPath(),"2_32.tex",sep="/"),g, preambulo = T )
  
  g<- graficaLinea(getListCB()$"2_33")
  exportarLatex(paste(getPath(),"2_33.tex",sep="/"),g, preambulo = T )
  
  
  g<- graficaLinea(getListCB()$"2_34")
  exportarLatex(paste(getPath(),"2_34.tex",sep="/"),g, preambulo = T )
  
  
  
  g<- graficaLinea(getListCB()$"2_35")
  exportarLatex(paste(getPath(),"2_35.tex",sep="/"),g, preambulo = T )
  

  
  g<- graficaLinea(getListCB()$"2_36")
  exportarLatex(paste(getPath(),"2_36.tex",sep="/"),g, preambulo = T )
  
    
  
  
}
