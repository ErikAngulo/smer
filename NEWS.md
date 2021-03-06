#smer unreleased

# smer 0.6.0 (2021-12-19)
## Added 

- Viñeta tutorial con ejemplos de uso del paquete

## Changed

- La entropía calculará automáticamente la cantidad de valores distintos para normalizar, y por defecto se normalizará

## Fixed

- Mostrar correctamente accesos directos a funciones en las ayudas

# smer 0.5.0 (2021-12-12)
## Added

- Gráfico de la curva ROC
- Gráfico de las entropías
- Gráfico de las correlaciones
- Gráfico de las informaciones mutuas
- Función para visualizar los Boxplot de cada columna gráficamente

## Changed

- Estandarización y normalización por defecto se aplicará a todas las columnas numéricas
- Funciones de areasroc, entropias y correlaciones disponen de un atributo nuevo para indicar si se desean obtener gráficos. Actualizada documentación


# smer 0.4.2 (2021-12-05)
## Added

- Herramienta de filtrado para dataset: obtener filas que cumplan condición en una columna
- Documentación de funciones


# smer 0.4.1 (2021-11-28)
## Added

- Normalización de variables
- Estandarización de variables
- Documentación de funciones


# smer 0.4.0 (2021-11-21)
## Added

- Discretización de variables por igual frecuencia, igual anchura y con tramos especificados.
- Documentación de funciones

## Fixed 

- Mostrar correctamente changelog desde ayuda del paquete


# smer 0.3.1 (2021-11-14)
## Added

- Cálculo de métricas para dataset: correlaciones e información mutua
- Documentación de funciones


# smer 0.3.0 (2021-11-12)
## Added

- Cálculo de métricas para dataset: área bajo la curva ROC


# smer 0.2.0 (2021-11-11)
## Added

- Cálculo de métricas para dataset: varianza y entropía
- Documentación de las funciones


# smer 0.1.4 (2021-11-09)
## Added

- Documentación y funciones de ayuda (visibles con help() o ?nombre_funcion)

## Changed

- Formato de NEWS (ahora se puede visualizar en la ayuda del paquete)
  Fichero NEWS vaciado, ver contenido en NEWS.md


# smer 0.1.3 (2021-11-08)
## Added

- Añadir/cambiar nombres de columna al Dataset
- Función genérica print compatible

## Fixed

- Factorización dataset (booleanos no)


# smer 0.1.2 (2021-11-07)
## Added

- Miniviñeta con ejemplos de uso del paquete


# smer 0.1.1 (2021-11-06)
## Changed

- Escritura en csv sin comillas
- Forma de añadir número id a Dataset

## Fixed

- Lectura de ficheros


# smer 0.1.0 (2021-11-05)
## Added

- Clase S4 para guardar datos (Dataset)
- Factorización de Dataset
- Lectura de ficheros y carga como Dataset
- Escritura de Dataset con formato csv
