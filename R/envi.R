# Creamos un nuevo entorno para el paquete
pkg.env <- new.env()

# Definimos colores en formato RGB
pkg.env$color1 <- rgb(0, 0, 0, maxColorValue = 255) # Color negro
pkg.env$color2 <- rgb(0.5, 0.5, 0.5) # Color gris
pkg.env$colorRelleno <- rgb(1, 1, 1) # Color blanco
pkg.env$gris <- rgb(200, 200, 200, maxColorValue = 255) 
pkg.env$grisBase <- rgb(152, 152, 152, maxColorValue = 255)

# Definimos parámetros generales
pkg.env$fEscala <- 0.85039370025172
pkg.env$fontSize <- 11
pkg.env$tamEti <- 3.2
pkg.env$botarCeros <- TRUE
pkg.env$maxMin <- FALSE
pkg.env$modalidad <- NULL

# Definimos las repúblicas
pkg.env$repu <- c("Total República", "Total republica", "Total república", "Total Republica")

# Definimos los valores que se van a ignorar
pkg.env$ignorado <- c("Ignorado", "ignorado", "IGNORADO", "Ignorada", "ignorada")

# Definimos los valores adicionales a excluir
exclusion_adicional <- c("otro", "otros", "otra", "otras", "Otras", "Otros")

# Unimos los valores ignorados y los adicionales en una lista de exclusión
pkg.env$exclusion <- c(pkg.env$ignorado, exclusion_adicional)
