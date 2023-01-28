

#Archivo de pruebas

#Creando archivo de pruebas

categorias <- c("Agua", "Saneamiento", "Electricidad", "Eliminación desechos")
valores <- c(80, 15, 20, 30)

testSpider <- data.frame(categorias, valores)
colnames(testSpider) <- c("x","y")



arreglar <- function(data){
  dataT <- t(data)
  dataTT <- rbind(max(rep(max(data$y)*1.25, 100), length(data$x)), rep(0, length(data$x)), dataT[2,] )
  colnames(dataTT) <- data$x
  return(as.data.frame(dataTT))
}

fmsb::radarchart(tt)
