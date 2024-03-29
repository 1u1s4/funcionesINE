% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcionesGraficas.R
\name{graficaLineaTrim}
\alias{graficaLineaTrim}
\title{Esta función desaparecerá en futuras versiones, deberá hacer uso 
de la función graficaLinea en la modalidad trimestral. 
Genera graficas de lineas para series historicas de los trimestrales}
\usage{
graficaLineaTrim(data, color1 = color, inicio = 0, ancho = 0.5, precision = 1)
}
\arguments{
\item{data}{El data frame con el que se hará la grafica}

\item{color1}{El color en el que se desea la linea}

\item{inicio}{El punto en el eje y a partir del cual se desea mostrar la grafica}

\item{precision}{Indica el número de decimales con el que se desea ver la etiqueta. Por defecto es uno.}

\item{El}{ancho de la linea}
}
\value{
El objeto ggplot2 listo para grafica
}
\description{
Esta función desaparecerá en futuras versiones, deberá hacer uso 
de la función graficaLinea en la modalidad trimestral. 
Genera graficas de lineas para series historicas de los trimestrales
}
