% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{entropias}
\alias{entropias}
\title{Entropia del Dataset}
\usage{
entropias(ds, discrete = TRUE, norm = TRUE, plot = FALSE)
}
\arguments{
\item{ds}{Objeto de tipo Dataset.}

\item{discrete}{Logical. Por defecto es TRUE, y calculará la entropía de
las columnas de tipo \code{character} e \code{integer}. Con valor FALSE,
se calculará en todas las columnas.}

\item{norm}{Logical. Parametro para indicar si se desean obtener las entropías
normalizadas o no. Por defecto, TRUE}

\item{plot}{Logical. Mostrar la entropía de las columnas gráficamente.}
}
\value{
Vector con la entropía correspondiente a cada columna.
En caso de \code{discrete=TRUE}, el valor será \code{NA} en las columnas
que no sean de tipo \code{character} e \code{integer}.
}
\description{
Calcula la entropía de cada columna de un \code{\linkS4class{Dataset}},
o de las columnas discretas.
}
\details{
De cara a normalizar, si una columna solo dispone de un único valor,
independientemente del valor del parámetro siempre se obtendra 0.

Si contiene más valores y se normaliza,
la entropía siempre estará entre valores 0 y 1.

Sin normalizar el valor de la entropía se considera como si tuviera
2 valores distintos. Por ello, se recomienda normalizar.
}
