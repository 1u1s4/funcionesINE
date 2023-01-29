

#Archivo de pruebas

#Creando archivo de pruebas

categorias <- c("Agua", "Saneamiento", "Electricidad", "Eliminacion de desechos", 'Otro')
valores <- c(80, 15, 20, 30, 50)

testSpider <- data.frame(categorias, valores)
colnames(testSpider) <- c("x","y")



arreglar <- function(data){
  dataT <- as.data.frame(matrix(ncol = length(data$x)))
  dataT[1,] <- data$y
  colnames(dataT) <- data$x
  dataTT <- rbind(rep(max(data$y*1.15,100), length(data$x)), rep(0, length(data$x)), dataT )
  return(dataTT)
}

tt <- arreglar(testSpider)
tt <- as.data.frame(tt)


tikzDevice::tikz('../ENEI2022/Graficas/prueba.tex', standAlone = F, bg = "transparent" , width = 4 , height= 2.75, sanitize= F)
op <- par(mar = c(1, 2, 2, 2))
fmsb::radarchart(tt, #custom polygon
                 pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 )
dev.off()
