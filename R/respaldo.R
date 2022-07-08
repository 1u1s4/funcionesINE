#'Funcion de respaldo
#'
#'
respaldo <- function(direccion, asunto, cuerpo, ruta){
  sender <- "reportesine@gmail.com"
  #, "<rdnarcisoc@gmail.com>",
  if (.Platform$OS.type == "windows") {
    ## No hay impelentacion  
  }else{
    archivo <- substr(basename(ruta), 1, nchar(basename(ruta))-4)
    print(archivo)
    cadena <-  paste("tar -zcvf ", file.path(dirname(ruta), paste("respaldo",str(basename(ruta), 1, nchar(basename(ruta))-4),".tar.gz", sep= "") ),dirname(ruta))
    print(cadena)
    suppressWarnings(silence <- system( cadena, intern=T, ignore.stderr=T))
  }
  recipients <- direccion
  mailR::send.mail(from = sender,
                   to = recipients,
                   subject=asunto,
                   body = cuerpo,
                   smtp = list(host.name = "smtp.gmail.com", port = 465, 
                               user.name="reportesine@gmail.com", passwd="Ine$2020", ssl=TRUE),
                   authenticate = TRUE,
                   send = TRUE,
                   attach.files = file.path(dirname(ruta), paste("respaldo",str(basename(ruta), 1, nchar(basename(ruta))-4),".tar.gz", sep= "") ))
}