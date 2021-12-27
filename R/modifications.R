
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
  if (any(columnas > ncol(ds@data)) || any(columnas < 1)){
    stop(paste("Los índices de las columnas tienen que estar en el rango [1:",
               ncol(ds@data), "] (número de columnas del Dataset)"))
  }
  #1 mirar que no haya logical
  #2 mirar que no haya otro tipo de datos no numéricos
  #(se hace unlist ya que los names de las columnas también comprueba)
  #3 mirar que no haya factores entre los datos numéricos
  if (any(sapply(ds@data[,columnas], typeof) == "logical") ||
      !is.numeric(unlist(ds@data[,columnas])) ||
      any(sapply(ds@data[,columnas], is.factor))){
    stop("Alguna de las columnas señaladas como valores no son numéricas")
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
