

setClass(Class="Dataset",
         slots=c(data="data.frame", name="character"))

checkValidityDataset <- function(object){
  if (nrow(object@data) <= 1){
    stop("Los datos introducidos deben contener más de una fila.")
  }
  return(TRUE)
}

setValidity(Class="Dataset", method=checkValidityDataset)

factorizarDataframe <- function(df, num){
  factores <- apply(df, MARGIN=2, FUN=unique)
  #if one col, returns factor. If > 1, returns factors in vector
  if (ncol(df) == 1){
    if (length(factores) <= num){
      df[,1] <- factor(df[,1])
    }
    return (df)
  }
  for (i in range(1:length(factores))){
    if (lengths(factores)[i] <= num){
      df[,i] <- factor(df[,i])
    }
  }
  return (df)
}

pkg.env <- new.env()
pkg.env$numero <- 0
sig_num <- function(){
  pkg.env$numero <- pkg.env$numero + 1
  return(pkg.env$numero)
}

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

leer_datos <- function(path, id, encabezado=FALSE, sep=",", dec=".", factorizar=0){
  df <- read.table(path, header=encabezado, sep=sep, dec=dec, stringsAsFactors = FALSE)
  if (missing(id) || !is(id,"character")){
    id <- as.character(sig_num())
  }
  ds <- dataset(df, id, factorizar)
  return(ds)
}


guardar_datos <- function(path, ds){
  write.csv(ds@data, file=path, quote = FALSE, row.names = FALSE)
}


