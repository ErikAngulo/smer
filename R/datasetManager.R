
#' Clase Dataset
#' @description
#' Clase para guardar dataframes con un nombre
#' @details
#' Este paquete inclute viñetas con ejemplos de uso de este paquete
#' @slot data Dataframe que contenga los datos
#' @slot name Nombre asignado al Dataset
# @export
setClass(Class="Dataset",
         slots=c(data="data.frame", name="character"))

checkValidityDataset <- function(object){
  if (nrow(object@data) <= 1){
    stop("Los datos introducidos deben contener más de una fila.")
  }
  return(TRUE)
}

setValidity(Class="Dataset", method=checkValidityDataset)

# Factorizar dataframe
# @description
# Dado un dataframe y un número, factorizará las columnas que tengan cantidad de
#   valores únicos diferentes inferiores a número. No se aplicará en logical
# @param df Dataframe a factorizar
# @param num Columnas con menos variedad que num serán factorizadas
# @return Dataframe con columnas factorizadas, si procede
factorizarDataframe <- function(df, num){
  factores <- apply(df, MARGIN=2, FUN=unique)
  #if one col, returns factor. If > 1, returns factors in vector
  if (ncol(df) == 1){
    if (length(factores) <= num && !is.logical(factores[1])){
      df[,1] <- factor(df[,1])
    }
    return (df)
  }
  for (i in 1:length(factores)){
    if (lengths(factores)[i] <= num && !'TRUE' %in% factores[[i]] && !'FALSE' %in% factores[[i]]){
      df[,i] <- factor(df[,i])
    }
  }
  return (df)
}

#global variable for package
pkg.env <- new.env()
pkg.env$numero <- 0
sig_num <- function(){
  pkg.env$numero <- pkg.env$numero + 1
  return(pkg.env$numero)
}

#' Creación de Datasets
#' @description
#' Función para crear \code{\linkS4class{Dataset}} de este paquete y poder operar con ellas
#' @param datos Un vector, matriz o dataframe de datos. Mínimo dos elementos
#' en el vector y dos filas en la matriz o dataframe
#' @param id (Opcional) Nombre que se le proporcionará al Dataset (string).
#'   En caso de no declararlo, se usará un número (el cuál incrementará por cada
#'   Dataset creado sin nombre)
#' @param factorizar Las columnas que contengas menos valores diferentes que
#'   este parámetro serán factorizadas
#' @return Objeto \code{\linkS4class{Dataset}} creado, con nombre y factorizado (si procede)
#' @examples
#' dataset(c(3,3,5,8,9,5), id="prueba", factorizar = 5)
#' @seealso
#' \code{\link{leer_datos}} para crear Dataset desde un fichero de datos
# @export
dataset <- function(datos, id, factorizar=0){
  if (missing(id) || !is(id,"character")){
    id <- as.character(sig_num())
  }
  df <- as.data.frame(datos) #siempre crea columnas (vector, matrix, df)
  if (factorizar >=2){
    df <- factorizarDataframe(df, factorizar)
  }
  object <- new(Class="Dataset", data=df, name=id)
  return(object)
}

#' Cargar los datos
#' @description
#' Lee los datos de un fichero csv y crea objeto \code{\linkS4class{Dataset}}
#' @details
#' La función está preparada para leer ficheros csv por defecto. Sin embargo,
#' se puede leer tsv y otros formatos indicando el separador adecuado y caracter
#' para números decimales
#' @param path Ruta del fichero csv (o derivados) a leer
#' @param  id (Opcional) Nombre que se le proporcionará al Dataset (string).
#' En caso de no declararlo, se usará un número (el cuál incrementará por cada
#' Dataset creado sin nombre)
#' @param encabezado Logical. Indicar si el fichero contiene en la primera línea
#' los nombres de las columnas. Por defecto es \code{FALSE}
#' @param sep Caracter que separa las columnas en el fichero. Por defecto, ','
#' @param dec Caracter que indica como están representados los decimales.
#' Por defecto, '.'
#' @param factorizar Las columnas que contengas menos valores diferentes que
#'   este parámetro serán factorizadas. Por defecto no se factorizará (0)
#' @return Objecto de tipo \code{\linkS4class{Dataset}} con los datos del fichero
#' @examples
#' \dontrun{
#' leer_datos(mi_path, encabezado = TRUE)
#' }
#' @seealso
#' \code{\link{dataset}} para crear objetos Dataset desde variables
# @export
leer_datos <- function(path, id, encabezado=FALSE, sep=",", dec=".", factorizar=0){
  df <- read.table(path, header=encabezado, sep=sep, dec=dec, stringsAsFactors = FALSE)
  if (missing(id) || !is(id,"character")){
    id <- as.character(sig_num())
  }
  ds <- dataset(df, id, factorizar)
  return(ds)
}

#' Guardar los datos
#' @description
#' Guarda los datos que tiene el \code{\linkS4class{Dataset}} en formato csv
#' @param path Nombre del fichero con el que se desea guardar la información
#' @param ds Objeto de tipo \code{\linkS4class{Dataset}}
# @export
guardar_datos <- function(path, ds){
  write.csv(ds@data, file=path, quote = FALSE, row.names = FALSE)
}

#' Cambiar los nombres de las columnas
#' @description
#' Esta función cambiará los nombres de las columnas de un \code{\linkS4class{Dataset}}.
#' Pueden cambiarse los nombres de todas las columnas a la vez o de las seleccionadas únicamente
#' @param ds Objeto de tipo \code{\linkS4class{Dataset}}
#' @param nombres Un vector con los nombres de las columnas. Ha de contener un nombre
#' por cada columna existente en el Dataset si no se define el parámetro \code{"columnas"}.
#' En ese caso el vector podrá contener menos nombres.
#' @param columnas Por defecto NULL. Será definido si se desea reemplazar el nombre de
#' una o varias columnas del dataframe. Para ello se pasará al argumento un número o
#' vector de números los cuales serán los índices de las columnas deseadas. En ellas
#' se asignarán los nombres indicados en el parámetro \code{"nombres"}
#' @examples
#' \dontrun{
#' nombres_columna(ds, c("1", "2", "3", "4"))
#' nombres_columna(ds, c("5","6"), c(1,2))
#' nombres_columna(ds3, 9, 3)
#' }
# @export
nombres_columna <- function(ds, nombres, columnas = NULL){
  if (length(nombres) == ncol(ds@data)){
    colnames(ds@data) <- nombres
  }
  else if (!is.null(columnas) && length(nombres) == length(columnas) && max(columnas) <= ncol(ds@data) && min(columnas) >= 1){
    colnames(ds@data)[columnas] <- nombres
  }
  else{
    if (is.null(columnas)){
      stop("Se deben especificar índices o renombrar todas las columnas")
    }
    else if (length(nombres) != length(columnas)){
      stop(paste("Faltan nombres de columas o índices por definir; o mayor que columnas tiene el dataset: ",ncol(ds@data)))
    }
    else{
      stop(paste("Los índices han de encontrarse entre 1 y ", ncol(ds@data)))
    }
  }
  return(ds)
}


#' @title Imprimir Dataset
#' @description Imprime el \code{\linkS4class{Dataset}}, junto a su nombre y número de filas/columnas
#' @param x Objeto Dataset a imprimir
#' @rdname print
#' @docType methods
# @export
setMethod(f="print",
          signature="Dataset",
          definition=function(x) {
            print(paste("Nombre del Dataset: ", x@name))
            print(paste("Cantidad columnas: ", ncol(x@data)))
            print(paste("Cantidad filas: ", nrow(x@data)))
            print("Contenido del dataset:")
            print(x@data)
          })

