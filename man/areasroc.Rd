% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{areasroc}
\alias{areasroc}
\title{Calcula la curva ROC y su área}
\usage{
areasroc(ds, cols_valores, col_etiquetas, plot = FALSE)
}
\arguments{
\item{ds}{Objeto de tipo Dataset}

\item{cols_valores}{Índice o vector de índices que indican columnas del Dataset.
Las columnas tienen que ser numéricas (sin ser categóricas).}

\item{col_etiquetas}{Índice que indica una columna de tipo logical del Dataset.}

\item{plot}{Logical. Se mostrará gráficamente la curva ROC en caso de TRUE.
Se mostrará coloreado la diferencia respecto a la diagonal (área de 0.5)}
}
\value{
Área bajo la curva (por cada columna indicada en cols_valores).
Obtener más información sobre el procedimiento en apartado Details.
}
\description{
Dada una columna de valores y una columna de etiquetas de un \code{\linkS4class{Dataset}},
por cada valor calculará el TPR y FPR para después obtener el área bajo la curva ROC.
Más info en apartado Details.
}
\details{
El algoritmo hace lo siguiente:

1- Ordena ambas columnas en orden ascendente en base a la columna de valores.
(No modifica el Dataset original)

2- Por cada valor, se presupone que todos los anteriores a ese valor se han
predicho como TRUE, y los siguientes como FALSE.

3- Se comparan las etiquetas predichas con las originales de la columna
de etiquetas y se calculan los ratios 'True Positive', 'False Positive',
'True Negative', 'False Negative', 'True Positive Rate' y 'False Positive Rate'.

4- Finalmente, se obtiene la función/curva entre 'False Positive Rate' y
'True Positive Rate' y el área que quede bajo la curva será devuelto por
esta función.

Es posible pasar varias columnas como etiquetas a la vez en una misma llamada
a esta función, en cuyo caso se obtendrá el área obtenida al aplicar el
algoritmo anterior a cada columna.

Las columnas de valores han de ser numéricas (sin ser categóricas, clase factor),
y la columna de etiquetas de clase logical, es decir con TRUE y FALSE como valores.
}
