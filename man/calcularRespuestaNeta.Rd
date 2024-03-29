% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcionesGramaticales.R
\name{calcularRespuestaNeta}
\alias{calcularRespuestaNeta}
\title{Función para comparación de porcentajes, cuando los datos viene 
dados en porcentajes.
La cota sirve para saber que mensaje se debe emitir,
la función está pensada en el documento de vitales}
\usage{
calcularRespuestaNeta(data, cota = 1, primeraPos = 5, ultimaPos = 9)
}
\arguments{
\item{data}{Data frame con el que se desea trabajar.}

\item{cota}{Valor númerico que indica que cota se usará.}

\item{primeraPos}{Posición inicial del vector y en data frame.}

\item{ultimaPos}{Posición final del vector y en el data frame.}
}
\description{
Función para comparación de porcentajes, cuando los datos viene 
dados en porcentajes.
La cota sirve para saber que mensaje se debe emitir,
la función está pensada en el documento de vitales
}
