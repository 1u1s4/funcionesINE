.onAttach <- function(libname, pkgname) {
  # Cargamos la biblioteca 'extrafont' y mostramos un mensaje inicial
  require(extrafont)
  packageStartupMessage("Este paquete usa una fuente en especifico, registrandola")
  
  # Si el sistema operativo es Windows, cargamos las fuentes específicas de Windows
  if (.Platform$OS.type == "windows"){
    extrafont::loadfonts("win", quiet = TRUE)
  }

  # Establecemos las opciones para TikZ
  options(tikzDefaultEngine = "xetex")
  options(tikzXelatexPackages = c("\\usepackage[T1]{fontenc}",
                                  "\\usepackage{tikz}",
                                  "\\usepackage[active,tightpage,xetex]{preview}",
                                  "\\usepackage{fontspec,xunicode}",
                                  "\\PreviewEnvironment{pgfpicture}",
                                  "\\setlength\\PreviewBorder{0pt}",
                                  "\\usetikzlibrary{calc}",
                                  "\\usetikzlibrary{positioning}",
                                  "\\setmainfont{Open Sans Condensed Light}"))
  options(tikzUnicodeMetricPackages = c("\\usetikzlibrary{calc}"))
  
  # Establecemos la ruta del diccionario según el sistema operativo
  rutaPadre <- ifelse(.Platform$OS.type == "unix", "~/Dictionary", file.path("C:/Users", Sys.getenv("USERNAME")))
  rutaDiccionario <- "tikzMetricsDictionary"
  
  # Creamos la ruta del padre si no existe
  if (!file.exists(rutaPadre)) {
    dir.create(rutaPadre)
  }
  
  # Establecemos la opción 'tikzMetricsDictionary' 
  options(tikzMetricsDictionary = file.path(rutaPadre, rutaDiccionario))
  
  # Establecemos las variables de entorno del paquete
  pkg.env$alto <- 1.91
  pkg.env$ancho <- 3.19
  pkg.env$longCuadrado <- 2.5
  pkg.env$tol <- pkg.env$ancho / 40
  pkg.env$longitudMaxima <- 25
  
  # Verificamos si existe el diccionario de palabras y, si no, lo creamos
  # crearDiccionarioPalabras() # no se usa u.u
  
  # Establecemos la fuente del paquete
  pkg.env$fuente <- "Open Sans Condensed Light"
}
