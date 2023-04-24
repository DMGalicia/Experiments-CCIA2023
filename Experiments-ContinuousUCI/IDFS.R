library(tidyverse)
library(RWeka)

IDF <- function(datos, conservarEmp = F){ 
  library(tidyverse)
  library(RWeka)
  #Función que regresa los indices de las instancias que no son inconsistencias
  atributos <- names(datos)
  clase <- tail(atributos, 1)
  atributos <- atributos[1:(length(atributos)-1)]
  #Grupos de ejemplos con valores de atributos iguales
  unicos <- datos %>% mutate(n = rownames(.)) %>% group_by_at(atributos) %>% 
    summarise(indices = list(n) ,.groups = "keep") 
  #Indices de ejemplos inconsistentes
  descartadas = c()
  conservadas = c()
  #Recoleccion de indices
  for (i in unicos$indices){
    if (length(unlist(unique(datos[i,clase]))) > 1){
      conteos <- table(datos[i,clase])
      clases <- unlist(dimnames(conteos))
      valores <- as.numeric(conteos)
      max <- -Inf
      maxClase <- NULL
      empate <- F
      #Verificacion de clase mayoritaria
      for (j in 1:length(clases)){
        if (valores[j] > max){
          max <- valores[j]
          maxClase <- clases[j]
          empate <- F
        } else if (valores[j] == max){
          empate = T
        }
      }
      #Verificacion de empates
      if(empate == T & conservarEmp == T){
        conservadas <- c(conservadas, unlist(i))
      }
      if(empate == T & conservarEmp == F){
        descartadas <- c(descartadas, unlist(i))
      } else{
        descartadas <- c(descartadas, unlist(i)[datos[i,clase] != maxClase])
        conservadas <- c(conservadas, unlist(i)[datos[i,clase] == maxClase])
      }
    }
    else{
      conservadas <- c(conservadas, unlist(i))
    }
  }
  #Resultado, puede modificarse para regresar indices de inconsistencias
  list(conserved = list(as.numeric(conservadas)), 
       deleted = list(as.numeric(descartadas)))
}

IDF2 <- function(datos){ 
  library(tidyverse)
  library(RWeka)
  #Función que regresa los indices de las instancias que no son inconsistencias
  atributos <- names(datos)
  clase <- tail(atributos, 1)
  atributos <- atributos[1:(length(atributos)-1)]
  #Grupos de ejemplos con valores de atributos iguales
  unicos <- datos %>% mutate(n = rownames(.)) %>% group_by_at(atributos) %>% 
    summarise(indices = list(n) ,.groups = "keep") 
  #Indices de ejemplos inconsistentes
  descartadas = c()
  conservadas = c()
  empates <- list()
  #Recoleccion de indices
  for (i in unicos$indices){
    if (length(unlist(unique(datos[i,clase]))) > 1){
      conteos <- table(datos[i,clase])
      clases <- unlist(dimnames(conteos))
      valores <- as.numeric(conteos)
      max <- -Inf
      maxClase <- NULL
      empate <- F
      ids <- list()
      #Verificacion de clase mayoritaria
      for (j in 1:length(clases)){
        id <- datos[i,] %>% rownames_to_column('x') %>% 
          filter(!!as.name(clase) == clases[j]) %>% 
          column_to_rownames('x') %>% rownames() 
        ids <- append(ids, list(id))
        if (valores[j] > max){
          max <- valores[j]
          maxClase <- clases[j]
          empate <- F
        } else if (valores[j] == max){
          empate = T
        }
      }
      #Verificacion de empates
      if(empate == T){
        empates <- append(empates, ids)
      } else{
        descartadas <- c(descartadas, unlist(i)[datos[i,clase] != maxClase])
        conservadas <- c(conservadas, unlist(i)[datos[i,clase] == maxClase])
      }
    }
    else{
      conservadas <- c(conservadas, unlist(i))
    }
  }
  modelo <- J48(class~., datos[conservadas,])
  if(length(empates) > 0){
    empatesC <- datos[unlist(empates),]
    row.names(empatesC) <- unlist(empates)  
    empatesC <- empatesC %>% mutate(pclass = predict(modelo, datos[unlist(empates),]))
    empConservados <- empatesC %>% filter(class == pclass) %>% row.names()
    empDescartados <- empatesC %>% filter(class != pclass) %>% row.names()
    conservadas <- c(conservadas, empConservados)
    descartadas <- c(descartadas, empDescartados)
  }
  list(conserved = list(as.numeric(conservadas)), 
       deleted = list(as.numeric(descartadas)))
}

inconsistency <- function(datos){
  library(tidyverse)
  row.names(datos) <- NULL
  atributos <- names(datos)
  atributos <- atributos[1:(length(atributos)-1)]
  #Grupos de ejemplos con valores de atributos iguales
  unicos <- datos %>% mutate(Class = NULL, n = strtoi(rownames(.))) %>%
    group_by_at(vars(atributos)) %>% summarise(indices = list(n), .groups = "keep") 
  #?ndices de ejemplos inconsistentes
  inconsistencia = c()
  #Recolecci?n de ?ndices
  for (i in unicos$indices){
    if (length(unique(datos[i,]$class)) > 1){
      inconsistencia <- c(inconsistencia, i)
    }
  }
  length(inconsistencia)/nrow(datos)
}

CFE <- function(data){
  #Características del conjunto de datos
  rows <- nrow(data)
  variables <- length(data)
  rownames(data) <- seq(nrow(data))
  #Lista de atributos que mejor separan a los ejemplos
  listAttribs <- c()
  #Mientra exista un atributo y la clase
  while (variables > 1){
    #Obtener la lista de atributos
    attributes <- head(colnames(data),-1)
    class <- tail(colnames(data),1)
    #Variable para almacenar los índices de los ejemplos en la región de tralape
    best <- c()
    #Variable que almacena el atributo que mejor separa la clase
    bestAttrib <- ""
    #Por cada atributo
    for(attribute in attributes){
      range = unique(data[[attribute]])
      overlap <- c()
      #Calcular la región de traslape y el rango
      for (value in range){
        split <- data[data[[attribute]]==value,]
        cn <- length(unique(split[,class]))
        if(cn <= 1){
          overlap <- c(overlap, as.integer(rownames(split)))
        }
      }
      #Si con un atributo existen menos ejemplos en la región de tralape se considera el mejor
      if(length(best) < length(overlap)){
        best <- overlap
        bestAttrib <- attribute
      }
    }
    #Si existe un atributo que separe las clases, se almacena y se borra de los datos
    if(bestAttrib != ""){
      listAttribs <- c(listAttribs, bestAttrib)
      data <- data[!(rownames(data) %in% best), ]
      data[bestAttrib] <- NULL
    }
    #Caso de paro cuando ningún atributo separa la clase
    if(length(best) == 0){
      break
    }
    variables <- length(data)
  }
  #Se regresa f4 y una lista de atributos
  #list(result = nrow(data)/rows, attrib = paste(listAttribs, collapse=','))
  nrow(data)/rows
}