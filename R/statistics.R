
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
#' @param norm Logical. Parametro para indicar si se desean obtener las entropías
#' normalizadas o no. Por defecto, TRUE
#' @param plot Logical. Mostrar la entropía de las columnas gráficamente.
#' @details
#' De cara a normalizar, si una columna solo dispone de un único valor,
#' independientemente del valor del parámetro siempre se obtendra 0.
#'
#' Si contiene más valores y se normaliza,
#' la entropía siempre estará entre valores 0 y 1.
#'
#' Sin normalizar el valor de la entropía se considera como si tuviera
#' 2 valores distintos. Por ello, se recomienda normalizar.
#'
#' @return Vector con la entropía correspondiente a cada columna.
#' En caso de \code{discrete=TRUE}, el valor será \code{NA} en las columnas
#' que no sean de tipo \code{character} e \code{integer}.
# @export
entropias <- function(ds, discrete = TRUE, norm = TRUE, plot=FALSE){
  if (norm){
    normalizar <- apply(ds@data, 2, function(x) return(length(unique(x))))
  }
  else{
    normalizar <- 2
  }

  if (discrete){
    H <- entropyDiscretes(ds@data, normalizar)
  }
  else{
    H <- apply(ds@data, MARGIN = 2, FUN=entropy)
    H <- H / log2(normalizar)
  }
  names(H) <- names(ds@data)
  if (plot==TRUE){
    barplot(H, main="Entropías", xlab="Columnas del Dataset", ylab="Valor de la entropía")
  }
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
#' @param plot Logical. Se mostrará gráficamente la curva ROC en caso de TRUE.
#' Se mostrará coloreado la diferencia respecto a la diagonal (área de 0.5)
#' @return Área bajo la curva (por cada columna indicada en cols_valores).
#' Obtener más información sobre el procedimiento en apartado Details.
# @export
areasroc <- function(ds, cols_valores, col_etiquetas, plot=FALSE){
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

    if (plot==TRUE){
      area <- result$area
      TPRarray <- result$TPRarray
      FPRarray <- result$FPRarray
      plot(FPRarray, TPRarray, type='l', main='Curva ROC', sub=paste('Área: ',area), xlab='FPR', ylab='TPR')
      polygon(FPRarray,TPRarray, col="cadetblue3")
    }
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
#' @param plot Logical. Fijado en TRUE, mostrará las correlaciones en un gráfico
#' @return Matriz de correlaciones
#' @seealso \code{\link{infmutuas}}
# @export
correlaciones <- function(ds, plot=FALSE){
  num <- unlist(lapply(ds@data, is.numeric))
  correlaciones <- cor(ds@data[num])
  if (plot==TRUE){
    if(!requireNamespace("reshape2") || !requireNamespace("ggplot2")){
      stop("Para mostrar este gráfico es necesario tener instalados
           los paquetes reshape2 y ggplot2")
    }
    else{
      df3 <- reshape2::melt(correlaciones)
      names(df3) <- c("Columna", "Fila", "Marginal")
      cols.char <- c("Columna","Fila")
      df3[cols.char] <- sapply(df3[cols.char],as.character)
      g <- ggplot2::ggplot(df3, ggplot2::aes(x=Fila, y=Columna, fill=Marginal)) +
        ggplot2::geom_tile() +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank())
      plot(g)
    }
  }
  return(data.frame (correlaciones))
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
#' @param plot Logical. Fijado en TRUE, mostrará las informaciones mutuas en un gráfico
#' @seealso \code{\link{correlaciones}}
#' @return Matriz de correlaciones
# @export
infmutuas <- function(ds, logical = FALSE, plot=FALSE){
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

  if (plot==TRUE){
    if(!requireNamespace("reshape2") || !requireNamespace("ggplot2")){
      stop("Para mostrar este gráfico es necesario tener instalados
           los paquetes reshape2 y ggplot2")
    }
    else{
      df3 <- reshape2::melt(result)
      names(df3) <- c("Columna", "Fila", "Marginal")
      cols.char <- c("Columna","Fila")
      df3[cols.char] <- sapply(df3[cols.char],as.character)
      g <- ggplot2::ggplot(df3, ggplot2::aes(x=Fila, y=Columna, fill=Marginal)) +
        ggplot2::geom_tile() +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank())
      plot(g)
    }
  }
  return(result)
}

#' Mostrar gráficamente Boxplot
#' @description
#' Se visualizará mediante un gráfico los boxplot correspondientes a cada columna.
#' Se aplicará solo a las columnas numéricas, las no numéricas se omitiran
#' automáticamente en el gráfico.
#' @param ds Objeto de tipo Dataset
# @export
graficoBoxplot <- function(ds){
  if(!requireNamespace("ggplot2")){
    stop("Para mostrar este gráfico es necesario tener instalado
           el paquete ggplot2")
  }
  g <- ggplot2::ggplot(stack(ds@data), ggplot2::aes(x = ind, y = values)) +
    ggplot2::geom_boxplot()
  plot(g)
}

