
###### Cálculo de métricas para los atributos de un dataset #####

#' Varianza del Dataset
#' @description
#' Calcula la varianza de cada columna de un \code{\linkS4class{Dataset}}.
#' @param ds Objeto de tipo Dataset.
#' @return Vector con la varianza correspondiente a cada columna.
#' Si la columna de datos no es numérica, devolverá NA en su respectiva posición.
# @export
varianzas <- function(ds){
  #suprressWarnings para que no imprima warning al poner un NA
  suppressWarnings(result <- apply(ds@data, MARGIN = 2, FUN=var))
  return(result)
}

#Calcula la entropía de cada columna
entropy <- function (x){
  count <- table(x)
  H <- 0
  for (elem in count){
    prop <- (elem/length(x))
    H <- H - prop * log2(prop)
  }

  return(H)
}

#calcula las entropías discretas
entropyDiscretes <- function(df, normalizar=2){
  H <- rep(NA, ncol(df))
  clases <- lapply(df, typeof)
  for (i in 1:length(clases)){
    if (clases[i] == "character" || clases[i] == "integer"){
      H[i] <- entropy(df[,i])
      if (length(normalizar) == 1){
        H[i] <- H[i] / log2(normalizar[1])
      }
      else{
        H[i] <- H[i] / log2(normalizar[i])
      }
    }
  }
  return(H)
}


#' Entropia del Dataset
#' @description
#' Calcula la entropía de cada columna de un \code{\linkS4class{Dataset}},
#' o de las columnas discretas.
#' @param ds Objeto de tipo Dataset.
#' @param discrete Logical. Por defecto es TRUE, y calculará la entropía de
#' las columnas de tipo \code{character} e \code{integer}. Con valor FALSE,
#' se calculará en todas las columnas.
#' @param normalizar En este parámetro se indicará cuántos valores posibles
#' puede contener el Dataset de cara a obtener la entropía normalizada.
#' En caso de que cada columna tenga diferentes valores posibles,
#' el valor de este parámetro podrá ser un vector de longitud número de columnas
#' donde cada posición indique la cantidad de valores posibles.
#' En caso de \code{discrete=TRUE}, en las posiciónes de columnas no discretas
#' se podrá usar cualquier instancia (ej: -1, NaN).
#' Se puede usar un único número que será aplicado a todas las columnas.
#' Por defecto este parámetro tiene el valor 2, lo que significa que
#' no se obtendrán entropías normalizadas. Para más información, ver Details.
#' @details
#' De cara a normalizar, si una columna solo dispone de un único valor,
#' independientemente del valor del parámetro siempre se obtendra 0.
#'
#' En caso de tener dos valores diferentes, se usará el parámetro con valor 2 (defecto),
#' y la entropía siempre estará normalizada (entre valores 0 y 1).
#'
#' Finalmente, en caso de tener más de dos valores diferentes se deberá especificar
#' la cantidad de valores posibles si se desea normalizar (obtener valores entre 0 y 1).
#'
#' Internamente, la fórmula de la entropía está implementada con logaritmo en base 2,
#' por ello cualquier columna que se desee normalizar que tenga más de 2 valores
#' posibles se deberá indicar mediante el parámetro normalizar en la respectiva
#' posición de la columna.
#' @return Vector con la entropía correspondiente a cada columna.
#' En caso de \code{discrete=TRUE}, el valor será \code{NA} en las columnas
#' que no sean de tipo \code{character} e \code{integer}.
# @export
entropias <- function(ds, discrete = TRUE, normalizar = 2){
  if (length(normalizar) != 1 && length(normalizar) != ncol(ds@data)){
    stop(paste("El parámetro 'normalizar' ha de ser un número o un vector de la misma
         longitud que columnas tenga el Dataset: ", ncol(ds@data)))
  }
  if (discrete){
    H <- entropyDiscretes(ds@data, normalizar)
  }
  else{
    H <- apply(ds@data, MARGIN = 2, FUN=entropy)
    H <- H / log2(normalizar)
  }
  names(H) <- names(ds@data)
  return(H)
}




#' Calcula la curva ROC y su área
#' @description
#' Dada una columna de valores y una columna de etiquetas de un \code{\linkS4class{Dataset}},
#' por cada valor calculará el TPR y FPR para después obtener el área bajo la curva ROC.
#' Más info en apartado Details.
#' @details
#' El algoritmo hace lo siguiente:
#'
#' 1- Ordena ambas columnas en orden ascendente en base a la columna de valores.
#' (No modifica el Dataset original)
#'
#' 2- Por cada valor, se presupone que todos los anteriores a ese valor se han
#' predicho como TRUE, y los siguientes como FALSE.
#'
#' 3- Se comparan las etiquetas predichas con las originales de la columna
#' de etiquetas y se calculan los ratios 'True Positive', 'False Positive',
#' 'True Negative', 'False Negative', 'True Positive Rate' y 'False Positive Rate'.
#'
#' 4- Finalmente, se obtiene la función/curva entre 'False Positive Rate' y
#' 'True Positive Rate' y el área que quede bajo la curva será devuelto por
#' esta función.
#'
#' Es posible pasar varias columnas como etiquetas a la vez en una misma llamada
#' a esta función, en cuyo caso se obtendrá el área obtenida al aplicar el
#' algoritmo anterior a cada columna.
#'
#' Las columnas de valores han de ser numéricas (sin ser categóricas, clase factor),
#' y la columna de etiquetas de clase logical, es decir con TRUE y FALSE como valores.
#' @param ds Objeto de tipo Dataset
#' @param cols_valores Índice o vector de índices que indican columnas del Dataset.
#' Las columnas tienen que ser numéricas (sin ser categóricas).
#' @param col_etiquetas Índice que indica una columna de tipo logical del Dataset.
#' @return Área bajo la curva (por cada columna indicada en cols_valores).
#' Obtener más información sobre el procedimiento en apartado Details.
# @export
areasroc <- function(ds, cols_valores, col_etiquetas){
  if (col_etiquetas < 1 || col_etiquetas > ncol(ds@data) ||
      any(cols_valores > ncol(ds@data)) || any(cols_valores < 1)){
    stop(paste("Los índices de las columnas tienen que estar en el rango [1:",
               ncol(ds@data), "] (número de columnas del Dataset)"))
  }
  if (typeof(ds@data[,col_etiquetas]) != "logical"){
    stop("La columna señalada como etiqueta no es de tipo 'logical'")
  }
  #1 mirar que no haya logical
  #2 mirar que no haya otro tipo de datos no numéricos
  #(se hace unlist ya que los names de las columnas también comprueba)
  #3 mirar que no haya factores entre los datos numéricos
  if (any(sapply(ds@data[,cols_valores], typeof) == "logical") ||
      !is.numeric(unlist(ds@data[,cols_valores])) ||
      any(sapply(ds@data[,cols_valores], is.factor))){
    stop("Alguna de las columnas señaladas como valores no son numéricas")
  }

  areas <- rep(0, length(cols_valores))
  for (i in 1:length(cols_valores)){
    result <- arearoc(ds, cols_valores[i], col_etiquetas)
    areas[i] <- result$area
  }
  names(areas) <- names(ds@data[cols_valores])

  return(areas)

}


arearoc <- function(ds, col_valores, col_etiquetas){
  #if (col_valores < 1 || col_etiquetas < 1 || col_valores > ncol(ds@data)|| col_etiquetas > ncol(ds@data)){
  #  stop(paste("Los índices de las columnas tienen que estar en el rango [1:",
  #            ncol(ds@data), "] (número de columnas del Dataset)"))
  #}
  df <- data.frame(variable=ds@data[,col_valores],
                   etiqueta=ds@data[,col_etiquetas])

  dfOrden <- df[order(df$variable),]

  ratios <- apply(dfOrden[c('variable')], 1, function(x) getRatios(dfOrden, x[1]))

  TPRarray <- sapply(ratios, "[[", 5)
  FPRarray <- sapply(ratios, "[[", 6)

  area <- integraOptimizada(FPRarray, TPRarray)
  return(list(area=area, TPRarray=TPRarray, FPRarray=FPRarray))
}


getRatios <- function(df, cortar){
  #cogemos los indices de las filas que valor sea menor a corte,
  #despues, nos quedamos con el vector de etiquetas de esas filas
  #y dividimos en los elementos que vamos a predecir como true y false
  #(segun si se ha cumplido que los valores son menores a corte o no)
  prediccion <- which(df[1] <= cortar)
  predTrue <- df[prediccion, 2]
  predFalse <- df[-prediccion, 2]

  TP <- sum(predTrue)
  FP <- length(predTrue) - TP
  FN <- sum(predFalse)
  TN <- length(predFalse) - FN

  TPR <- TP / (TP + FN)
  FPR <- FP / (FP + TN)
  return(list(TP, FP, FN, TN, TPR, FPR))
}

integraOptimizada <- function(x, y) {
  delta.x <- diff(x)
  mean.y  <- rowMeans(cbind(y[-1], y[-length(y)]))
  return(sum(delta.x * mean.y))
}


#' Calcula las correlaciones
#' @description
#' Calcula las correlaciones entre pares de variables (columnas) de un \code{\linkS4class{Dataset}}.
#' Se seleccionarán las columnas númericas automáticamente
#' @param ds Objeto de tipo Dataset
#' @return Matriz de correlaciones
# @export
correlaciones <- function(ds){
  num <- unlist(lapply(ds@data, is.numeric))
  return(data.frame (cor(ds@data[num])))
}

#' Calcula las informaciones mutuas
#' @description
#' Calcula las informaciones mutuas entre pares de variables (columnas) de un \code{\linkS4class{Dataset}}.
#' Se seleccionarán las columnas de valores numéricos discretos y factores
#' automáticamente.
#' @param ds Objeto de tipo Dataset
#' @param logical Variable para controlar si se tendrán en cuenta también
#' las columnas del Dataset que sean de tipo logical. Por defecto, FALSE (no
#' se tendrán en cuenta). Si se fija en TRUE, se considerarán.
#' @return Matriz de correlaciones
# @export
infmutuas <- function(ds, logical = FALSE){
  if(!requireNamespace("infotheo")){
    stop("La función 'infmutuas' requiere la instalación del
         paquete 'infotheo'. Instala este paquete y
         vuelve a ejecutar la función")
  }
  #https://search.r-project.org/CRAN/refmans/infotheo/html/mutinformation.html
  int <- unlist(lapply(ds@data, is.integer))
  fac <- unlist(lapply(ds@data, is.factor))
  or <- int | fac
  if (logical){
    log <- unlist(lapply(ds@data, is.logical))
    or <- or | log
  }
  result <- infotheo::mutinformation(ds@data[or])
  return(result)
}
