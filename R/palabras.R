

#'Función para cortar nombres, versión 1
#'@param data El data frame con la información necesaria
#'@param noColumn El número de columna sobre el cúal se quiere hacer el análisis de la longitud de la palabra
cambiarNombres <- function(data,noColumn){
  for(x in 1:nrow(data[noColumn]) ){
    if ( stringi::stri_length( data[noColumn][[1]][x] ) > pkg.env$longitudMaxima ){
      corta <- buscarPalabraDiccionario( data[noColumn][[1]][x] )
      if ( corta == -1 ){
        print( data[noColumn][[1]][x] )
        message("La palabra es muy larga, escriba una frase corta: \n")
        corta <- readLines(n=1)
        data[noColumn][[1]][x] <- corta
        guardarPalabraEnDiccionario( data[noColumn][[1]][x], corta )
      }
    }else{
      print("Palabras cortas")
    }
  }
  return(data)
}


#' Función que crea el diccionario en caso de que no esté
#'
#' @return
#' @export
#'
#' @examples
crearDiccionarioPalabras <- function(){
 #La ruta para la creación del diccionario
  if (.Platform$OS.type == "unix" ) {
    rutaPadre = "~/Dictionary"
  }else{
    usuario = Sys.getenv("USERNAME")
    rutaPadre = paste0("C:/Users/", usuario)
  }
  
  rutaDiccionario = "diccionarioAcortador"  
  
  pkg.env$rutaDiccionarioAcortador = file.path(rutaPadre,rutaDiccionario)
  
  if (!file.exists( rutaPadre )){
    dir.create( file.path(rutaPadre) )
  }
  
  if ( !file.exists( pkg.env$rutaDiccionarioAcortador ) ){
    unlink( pkg.env$rutaDiccionarioAcortador, recursive = F )
    filehash::dbCreate(pkg.env$rutaDiccionarioAcortador, type = "DB1")
    message("Diccionario Creado")
  }
  
  pkg.env$diccionarioEncortador <- filehash::dbInit(pkg.env$rutaDiccionarioAcortador)
  
  
  

  
}

#' Función que busca una palabra en el diccionario
#'
#' @param key 
#'
#' @return
#' @export
#'
#' @examples
buscarPalabraDiccionario <- function(key){
  crearDiccionarioPalabras()
  #revisando si se tiene la palabra
  estaPalabra <- filehash::dbExists(pkg.env$diccionarioEncortador, filehash:::sha1(key))
  
  if( estaPalabra ){
    corta <- filehash::dbFetch( pkg.env$diccionarioEncortador, filehash:::sha1(key) )
  }else{
    corta <- -1
  }
  
  return(corta)
}


#' Función que guarda el resumen de una palabra larga cuando el usuario la ingresa
#'
#' @param key 
#' @param corta 
#'
#' @return
#' @export
#'
#' @examples
guardarPalabraEnDiccionario <- function(key, corta){
  filehash::dbInsert(pkg.env$diccionarioEncortador, filehash:::sha1(key), corta)
}


