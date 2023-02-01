

#Archivo de pruebas

#Creando archivo de pruebas

categorias <- c("Agua", "Saneamiento", "Electricidad", "Eliminacion de desechos", 'Otro')
valores <- c(80, 15, 20, 30, 50)

testSpider <- data.frame(categorias, valores)
colnames(testSpider) <- c("x","y")










graficaRadar(testSpider, file = 'C:/Users/hagarcia/Documents/Proyectos/ENEI2022/Graficas/prueba.tex')
