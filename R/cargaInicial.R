.onAttach <- function(libname, pkgname) {
  require(extrafont)
  packageStartupMessage("Este paquete usa una fuente en especifico, registrandola")
  ## Load all fonts
  extrafont::loadfonts("pdf", quiet = TRUE)
  extrafont::loadfonts("postscript", quiet = TRUE)
  if ( ! "Open Sans Condensed Light" %in% fonts() ) {
    print("Vamos a importar las fuentes, ingrese Y y presione ENTER")
    font_import()
  }
  if (.Platform$OS.type == "windows") {
    extrafont::loadfonts("win", quiet = TRUE)
  }
  options(tikzDefaultEngine = "xetex")
  #options(tikzXelatex = "/usr/local/texlive/2014/bin/x86_64-linux/xelatex")
  options(tikzXelatexPackages = c("\\usepackage[T1]{fontenc}",
                                  "\\usepackage{tikz}\n",
                                  "\\usepackage[active,tightpage,xetex]{preview}\n",
                                  "\\usepackage{fontspec,xunicode}\n",
                                  "\\PreviewEnvironment{pgfpicture}\n",
                                  "\\setlength\\PreviewBorder{0pt}\n",
                                  "\\usetikzlibrary{calc}\n",
                                  "\\usetikzlibrary{positioning}\n",
                                  "\\usepackage{fontspec,xunicode}\n",
                                  "\\setmainfont{Open Sans Condensed Light}\n"))
  
  options(tikzUnicodeMetricPackages = c("\\usetikzlibrary{calc}\n"))
  
  if (.Platform$OS.type == "unix" ) {
    rutaPadre = "~/Dictionary"
    rutaDiccionario = "tikzMetricsDictionary"  
  }else{
    usuario = Sys.getenv("USERNAME")
    rutaPadre = paste0("C:/Users/", usuario)
    rutaDiccionario = "tikzMetricsDictionary"
  }
  
  if (!file.exists( rutaPadre )){
    dir.create( file.path(rutaPadre) )
  }
  options( tikzMetricsDictionary = file.path(rutaPadre,rutaDiccionario))
  
  # Variables para el tamaño de la gráfica
  
  pkg.env$alto <- 1.91 
  pkg.env$ancho <- 3.19
  
  # Variables para las legendas
  pkg.env$longCuadrado <- 2.5 #<--- Esta está en mm
  pkg.env$tol <- pkg.env$ancho/40
  
  #La longitud máxima para una palabra
  
  pkg.env$longitudMaxima <- 25
  
  #Verificando si existe el diccionario de palabras
  crearDiccionarioPalabras()
  
  pkg.env$fuente <- "Open Sans Condensed Light"
  
  
}