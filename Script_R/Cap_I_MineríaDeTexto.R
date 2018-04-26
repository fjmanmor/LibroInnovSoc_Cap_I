#############################
#############################
#############################
#############################
#Libro: Innovación social: desarrollo teórico 
#y experiencias en México
#############################
#############################
#Capítulo I
#############################
#############################
#¿Qué categorías analíticas integran la 
#innovación social?: Un análisis de la 
#literatura a través de minería de texto
#############################
#############################
#############################
#############################

#############################
#############################
#Francisco Manzano Mora (UNAM)
#Nayeli Martínez Velázquez (UAM-X)
#Diana Rivera Delgado (UAM-X)
#############################
#############################

#############################
#############################
#Se trabajó con 90 artículos que se encuentran 
#en el archivo Matriz_Capítulo_I
#############################
#############################

#############################
#############################
#Activar paqueterías
library(tm)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(Rgraphviz)
library(graph)
library(rJava)
library(RWeka)
#############################
#############################

#############################
#############################
#MINERÍA DE TEXTO POR PERÍODOS
#############################
#############################

#############################
#############################
#PERÍODO I
#############################
#############################
#Ubico la carpeta que contiene el archivo a analizar
Per_1<-file.path("DIRECTORIO_DE_TRABAJO_DONDE_SE_ENCUENTRA_EL_ARCHIVO_TXT_Per_1")
Per_1
#############################
#############################

#############################
#############################
#Creo un corpus volátil (Volatile Corpora)
Per_1a<-VCorpus(DirSource(Per_1))
Per_1a
#Reviso características 
class(Per_1a)
summary(Per_1a)
inspect(Per_1a)
#############################
#############################

#############################
#############################
#Limpio el corpus
#¿Qué tengo que limmpiar?
getTransformations()
#removeNumbers, removePunctuation, removeWords, stemDocument, stripWhitespace

#Transformo a "espacio" los siguientes caracteres: "/" , "@" , "-" y "$" 
A_Espacio<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
Per_2<-tm_map(Per_1a, A_Espacio, "/")
Per_3<-tm_map(Per_2, A_Espacio, "@")
Per_4<-tm_map(Per_3, A_Espacio, "-")
Per_5<-tm_map(Per_4, A_Espacio, "$")

#Transformo en letras minúsculas a los caracteres del corpus
Per_6<-tm_map(Per_5, content_transformer(tolower))

#Al realizar un análisis de texto no me interesa los números expresados 
#en el corpus
#Remuevo los números
Per_7<-tm_map(Per_6, removeNumbers)

#Remuevo los signos de puntuación
Per_8<-tm_map(Per_7, removePunctuation)

#Remuevo las palabras frecuentes en textos de español
#OJO: por default siempre se removerá palabras en inglés, por lo que es
#necesario especificar el idioma 
#En este caso es un corpus que contiene un documento en español 
#¿Cuántas hay?
length(stopwords(kind="en"))

#¿Cuáles son?
stopwords(kind="en")
Per_9<-tm_map(Per_8, removeWords, stopwords(kind="en"))

#Remuevo los espacios en blanco dejados por las modificaciones 
#anteriores
Per_10<-tm_map(Per_9, stripWhitespace)
#############################
#############################

#############################
#############################
#N-Gram Frequency: TRI-GRAM
#############################
#############################
TrigramTokenizer<-function(x) NGramTokenizer(x, 
                                             Weka_control(min = 3, max = 3))

Per_11<-DocumentTermMatrix(Per_10, control = list(tokenize = TrigramTokenizer))

Frecuencia<-sort(colSums(as.matrix(Per_11)), decreasing=TRUE)

wof<-data.frame(Conjunto_De_Palabras=names(Frecuencia), Frecuencia=Frecuencia)

pl <- ggplot(subset(wof, Frecuencia > 1), aes(Conjunto_De_Palabras, Frecuencia))
x11()
pl <- pl + geom_bar(stat="identity", fill="yellow2")
pl + theme(axis.text.x=element_text(angle=45, hjust=1))
#############################
#############################

#############################
#############################
#COEFICIENTE DE ASOCIACIÓN
#############################
#############################
Per_11<-DocumentTermMatrix(Per_10)
#Coeficiente de asociación definido por el investigador
#De 0 a 1
findAssocs(Per_13, "PALABRA_A_ESTUDIAR", corlimit = COLOCAR_VALORES_DE_0_A_1)
#And so on...
#############################
#############################

#############################
#############################
#############################
#############################
#El mismo procedimiento se realizó para el
#Período II y Período III
#############################
#############################
#############################
#############################