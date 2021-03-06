
##### Discretización #####


#' Discretización de variables continuas
#' @description
#' Discretiza las columnas seleccionadas de un \code{\linkS4class{Dataset}}
#' con el número de tramos o puntos de corte proporcionados.
#' @param ds Objeto de tipo Dataset.
#' @param metodo Algoritmo de discretización. Con valor 'frecuencia' se aplicará
#' discretización por igual frecuencia. Con valor 'anchura' se aplicará discretización
#' por igual anchura. Con valor 'manual' se discretizará con los tramos obtenidos
#' en otro algoritmo de discretización ya ejecutado o con los tramos deseados.
#' @param puntos_corte Cuántos puntos de corte se desean (el valor + 1 serán los tramos).
#' Si el valor del parámetro 'metodo' es 'manual', se deberá indicar en este parámetro
#' cuáles son los puntos de corte a través de un vector.
#' @param columnas El índice de columnas a las que se aplicará la discretización.
#' Las columnas deberán ser numéricas y no deberán estar ya factorizadas.
#' @details Uno de los algoritmos clásicos es equal width (igual anchura). Dado un vector de números reales y un número de intervalos, determina cuales son los puntos de corte para generar un vector categórico de tal manera que esos puntos están uniformemente distribuidos en el rango de los valores.
#' Por ejemplo, si tenemos los valores (11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4) y queremos generar una variable categórica (su implementación en Python puede ser como un string) con cuatro posibles valores, tenemos que determinar tres puntos de corte, que serán valores que separen el intervalo (entre 0.5 y 20.5 en este caso) en 4 tramos de igual tamaño. Es decir, el primer tramo irá de 0.5 a 5.5, el segundo de 5.5 a 10.5, el tercero de 10.5 a 15.5 y el último de 15.5 a 20.5. Es decir, tendríamos 3 puntos de corte, 5.5, 10.5 y 15.5.
#' Normalmente cuando se lleva a cabo esta tarea, ante un nuevo valor es necesario determinar a que intervalo pertence. Dado que cuando eso ocurre el valor puede estar fuera de los límites del vector original, el comienzo del primer tramos se suele considerar -infinito y el final del último como infinito. Es decir, una vez aplicado el algoritmo, el resultado sería un vector categórico de este estilo:
#' `["I3", "I2", "I1", "I1", "I4", "I2"]`, donde I1=(-infinito, 5.5], I2=(5.5, 10.5], I3=(10.5, 15.5], I4=(15.5, infinito).
#' Otro de los algoritmos clásicos es equal frequency (igual frecuencia), donde el objetivo es buscar los puntos de corte que hagan que el número de valores del vector a discretizar que caen en cada uno de los intervalos sea el mismo (+- 1, según el número de intervalos y de puntos).
#' @return Un nuevo \code{\linkS4class{Dataset}} con las columnas proporcionadas
#' discretizadas y factorizadas.
# @export
discretizar <- function(ds, metodo, puntos_corte, columnas){
  mensaje <- verificarColumnaNumerica(ds, columnas) #si no son numéricas, parar programa
  if (!is.null(mensaje)){
    stop(mensaje)
  }

  if (!is.numeric(puntos_corte)){
    stop("Los puntos de corte tienen que ser numéricos")
  }

  disc_df <- ds@data


  if (metodo == 'manual'){
    disc_df[columnas] <- lapply(ds@data[columnas], function(x){
      factor(discretizeCutPoints(as.vector(x), puntos_corte))
    } )
    masnombre <- '_disc_manual'
  }

  else{
    if (length(puntos_corte) > 1){
      warning("Se esperaba número indicando tramos y se ha detectado vector.
              Se usará el primer número del vector del argumento 'puntos_corte'
              para seleccionar el número de tramos con el que discretizar.")
    }

    tramos <- round(puntos_corte[1])

    if (metodo == 'frecuencia'){
      disc_df[columnas] <- lapply(ds@data[columnas], function(x){
        puntos_corte <- discretizeEF(x, tramos)
        return(factor(discretizeCutPoints(x, puntos_corte)))
      })
      masnombre <- '_disc_freq'
    }

    else if (metodo == 'anchura'){
      disc_df[columnas] <- lapply(ds@data[columnas], function(x){
        puntos_corte <- discretizeEW(x, tramos)
        return(factor(discretizeCutPoints(x, puntos_corte)))
      })
      masnombre <- '_disc_anch'
    }

    else{
      stop("El método seleccionado no está disponible.
           Consulte la ayuda para ver los métodos disponibles.")
    }
  }

  nds <- dataset(disc_df, id=paste(ds@name, masnombre))
  return(nds)
}


discretizeCutPoints <- function(x, cut.points){
  x.discretized <- c(0, length = length(x))

  for (num in seq(1:length(x))){
    if (x[num] <= cut.points[1]){
      x.discretized[num] <- paste('(-Inf,', cut.points[1], ']', sep="")
    }
    else if (x[num] > cut.points[length(cut.points)]){
      x.discretized[num] <- paste('(', cut.points[length(cut.points)], ',+Inf)', sep="")
    }
    else {
      for (limite in seq(2:length(cut.points))){
        if (x[num] <= cut.points[limite]){
          x.discretized[num] <- paste('(', cut.points[limite-1], ',', cut.points[limite], ']', sep="")
          break
        }
      }
    }
  }
  return(x.discretized)
}

discretizeEW <- function (x, num.bins) {
  x.discretized <- c(0, length = length(x))
  tramo <- round((max(x) - min(x)) / num.bins, digits = 4)
  cut.points <- seq(min(x)+tramo, max(x)-tramo, tramo) #puntos de corte que delimitan los tramos


  return(cut.points)
}

discretizeEF <- function (x, num.bins) {
  orderedIndeX <- order(x, decreasing=FALSE)

  secuencia <- length(x) / num.bins
  cut.index <- round(seq(0, length(x), secuencia))
  cut.index <- cut.index[2:(length(cut.index)-1)]

  cut.points <- x[orderedIndeX[cut.index]]

  return(cut.points)
}

verificarColumnaNumerica <- function(ds, columnas){
  if (any(columnas > ncol(ds@data)) || any(columnas < 1)){
    return(paste("Los índices de las columnas tienen que estar en el rango [1:",
               ncol(ds@data), "] (número de columnas del Dataset)"))
  }
  #1 mirar que no haya logical
  #2 mirar que no haya otro tipo de datos no numéricos
  #(se hace unlist ya que los names de las columnas también comprueba)
  #3 mirar que no haya factores entre los datos numéricos
  if (any(sapply(ds@data[,columnas], typeof) == "logical") ||
      !is.numeric(unlist(ds@data[,columnas])) ||
      any(sapply(ds@data[,columnas], is.factor))){
    return("Alguna de las columnas señaladas no son numéricas")
  }
  return(NULL)
}


##### Normalización #####

#' Normalización de variables
#' @description
#' Normaliza las columnas señaladas de un \code{\linkS4class{Dataset}}.
#' Los valores estarán en el rango de entre 0 y 1 incluídos.
#' @param ds Objeto de tipo Dataset.
#' @param columnas El índice de columnas a las que se aplicará la normalización.
#' Las columnas deberán ser numéricas y no deberán estar ya factorizadas.
#' Si no se especifica ningún valor por defecto se aplicará la estandarización
#' a todas las columnas numéricas.
#' @return Un nuevo \code{\linkS4class{Dataset}} con las columnas proporcionadas
#' normalizadas
#' @seealso \code{\link{estandarizar}}
# @export
normalizar <- function(ds, columnas = NULL){
  if (is.null(columnas)){
    columnas <- sapply(ds@data, class) == "numeric"
  }
  else{
    mensaje <- verificarColumnaNumerica(ds, columnas) #si no son numéricas, parar programa
    if (!is.null(mensaje)){
      stop(mensaje)
    }
  }
  norm_df <- ds@data

  norm_df[columnas] <- lapply(ds@data[columnas], function(x) norm(x))
  masnombre <- '_normalized'

  nds <- dataset(norm_df, id=paste(ds@name, masnombre))
  return(nds)
}

norm <- function(x){
  minimo <- min(x)
  maximo <- max(x)
  n <- (x - minimo) / (maximo - minimo)
  return(n)
}

##### Estandarización #####

#' Estandarización de variables
#' @description
#' Estandariza las columnas señaladas de un \code{\linkS4class{Dataset}}.
#' Los valores seguirán una distribución normal con media 0 y desviación estandar 1.
#' @param ds Objeto de tipo Dataset.
#' @param columnas El índice de columnas a las que se aplicará la estandarización.
#' Las columnas deberán ser numéricas y no deberán estar ya factorizadas.
#' Si no se especifica ningún valor por defecto se aplicará la estandarización
#' a todas las columnas numéricas.
#' @return Un nuevo \code{\linkS4class{Dataset}} con las columnas proporcionadas
#' estandarizadas
#' @seealso \code{\link{normalizar}}
# @export
estandarizar <- function(ds, columnas=NULL){
  if (is.null(columnas)){
    columnas <- sapply(ds@data, class) == "numeric"
  }
  else{
    mensaje <- verificarColumnaNumerica(ds, columnas) #si no son numéricas, parar programa
    if (!is.null(mensaje)){
      stop(mensaje)
    }
  }
  est_df <- ds@data

  est_df[columnas] <- lapply(ds@data[columnas], function(x) est(x))
  masnombre <- '_standarized'

  nds <- dataset(est_df, id=paste(ds@name, masnombre))
  return(nds)
}

est <- function(x){
  media <- mean(x)
  desviacion <- sd(x)
  e <- (x-media) / desviacion
  return(e)
}


##### Filtrado #####

#' Filtrar elementos de un Dataset
#' @description
#' Selecciona las filas de un \code{\linkS4class{Dataset}} que cumplen un criterio en una columna.
#' Este criterio se especificará a través de una función
#' @param ds Objeto de tipo Dataset.
#' @param columna El índice de columna al que se aplicará la función de filtrado.
#' @param funcion Función de filtrado. Deberá constar de un único argumento de entrada
#' que será un vector, y su salida (return) deberá ser un vector de TRUE o FALSE si se
#' cumple el criterio de la función por cada elemento del vector de entrada.
#' @return Un nuevo \code{\linkS4class{Dataset}} con las filas filtradas.
#' @examples
#' \dontrun{
#' filtrar(ds, 3, function(x) x == "Radio")
#' filtrar(ds, 3, function(x) x == "Radio" || x == "Television")
#' filtrar(ds, 1, function(x) x > 4.7)
#' filtrar(ds, 4, function(x) x < 3)
#' filtrar(ds, 2, function(x) x == TRUE)
#' }
# @export
filtrar <- function(ds, columna, funcion){
  if (length(columna) > 1){
    stop("Solo se puede seleccionar una columna de filtrado")
  }
  if (any(columna > ncol(ds@data)) || any(columna < 1)){
    stop(paste("El índices de la columnas tienen que estar en el rango [1:",
                 ncol(ds@data), "] (número de columnas del Dataset)"))
  }
  filt_df <- ds@data
  nombre <- ds@name

  filas <- which(funcion(filt_df[columna]) == TRUE)

  filt_df <- filt_df[filas, ]

  if (length(filas)==0){
    stop("No hay ningún valor en la columna que cumpla la función especificada.
         No se han obtenido filas. Filtrado no exitoso.")
  }
  else if (length(filas) == 1){
    warning("Solo hay una única fila después del proceso de filtrado que cumple las condiciones.
            No se puede crear objeto de tipo Dataset. Se devolverán los valores como dataframe")
    return(filt_df[1,])
  }

  if (!grepl("_filtrado", ds@name)){
    nombre <- paste(nombre, "_filtrado")
  }

  nds <- dataset(filt_df, id=nombre)
  return(nds)

}
