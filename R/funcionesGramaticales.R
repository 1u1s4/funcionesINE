#'Función para comparación de porcentajes
#'La cota sirve para saber que mensaje se debe emitir,
#'la función está pensada en el documento de vitales
#'@param data Data frame con el que se desea trabajar.
#'@param cota Valor númerico que indica que cota se usará.
#'@param ultimaPos Posición final del vector y en el data frame.
#'@param primeraPos Posición inicial del vector y en data frame.
#'
calcularRespuestaPor <- function(data, cota = 1, primeraPos = 5, ultimaPos = 9){
  respuesta = ''
  if(data$y[ultimaPos] - data$y[primeraPos] > 0 && abs(cambioInterAnual(data, primeraPos, ultimaPos)) > cota ){
    respuesta = paste('un aumento del ', round(cambioInterAnual(data, primeraPos, ultimaPos),1), '\\%  respecto a lo registrado en ', sep = '')
  }else if(data$y[ultimaPos] - data$y[primeraPos] < 0 && abs(cambioInterAnual(data)) > cota){
    respuesta = paste(' una reducción del  ', round(cambioInterAnual(data, primeraPos, ultimaPos), 1), '\\% respecto a lo registrado en ', sep = '')  
  }else if(data$y[ultimaPos] - data$y[primeraPos] == 0){
    respuesta = 'lo mismo que se registró en'
  }else if(data$y[ultimaPos] - data$y[primeraPos] > 0){
    respuesta = paste('apenas un aumento del ', round(cambioInterAnual(data, primeraPos, ultimaPos), 3), '\\%  respecto a lo registrado en ', sep = '')
  }else if(data$y[ultimaPos] - data$y[primeraPos] < 0){
    respuesta = paste('apenas una reducción del ',round(cambioInterAnual(data, primeraPos, ultimaPos), 3) , '\\%  respecto a lo registrado en ', sep = '')
  }
  #return(iconv(respuesta, to = 'utf8'))
  return(respuesta)
}

#'Función para comparación de porcentajes, cuando los datos viene 
#'dados en porcentajes.
#'La cota sirve para saber que mensaje se debe emitir,
#'la función está pensada en el documento de vitales
#'@param data Data frame con el que se desea trabajar.
#'@param cota Valor númerico que indica que cota se usará.
#'@param ultimaPos Posición final del vector y en el data frame.
#'@param primeraPos Posición inicial del vector y en data frame.
#'
calcularRespuestaNeta <- function(data, cota = 1, primeraPos = 5, ultimaPos = 9){
  respuesta = ''
  paso <- ultimaPos - primeraPos
  punto <- ifelse(cambioInterAnualNeto(data,paso) == 1, " punto porcentual "," puntos porcentuales ")
  if(data$y[ultimaPos] - data$y[primeraPos] > 0 && abs(cambioInterAnualNeto(data,paso)) > cota ){
    respuesta = paste('un aumento de  ', round(cambioInterAnualNeto(data, paso), 1), punto,'   respecto a lo registrado en ', sep = '')
  }else if(data$y[ultimaPos] - data$y[primeraPos] < 0 && abs(cambioInterAnual(data,paso)) > cota){
    respuesta = paste(' una reducción de  ', round(cambioInterAnualNeto(data, paso), 1), punto,'  respecto a lo registrado en ', sep = '')  
  }else if(data$y[ultimaPos] - data$y[primeraPos] == 0){
    respuesta = 'lo mismo que se registró en'
  }else if(data$y[ultimaPos] - data$y[primeraPos] > 0){
    respuesta = paste('apenas un aumento de ', round(cambioInterAnualNeto(data,paso), 3),punto, '  respecto  a lo registrado en ', sep = '')
  }else if(data$y[ultimaPos] - data$y[primeraPos] < 0){
    respuesta = paste('apenas una reducción de  ',round(cambioInterAnualNeto(data,paso), 3) ,punto,' respecto a lo registrado en ', sep = '')
  }
  #return(iconv(respuesta, to = 'utf8'))
  return(respuesta)
}
