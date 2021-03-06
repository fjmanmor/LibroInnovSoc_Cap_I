#############################
#############################
#############################
#############################
#Libro: Innovaci�n social: desarrollo te�rico 
#y experiencias en M�xico
#############################
#############################
#Cap�tulo I
#############################
#############################
#�Qu� categor�as anal�ticas integran la 
#innovaci�n social?: Un an�lisis de la 
#literatura a trav�s de miner�a de texto
#############################
#############################
#############################
#############################

#############################
#############################
#Francisco Manzano Mora (UNAM)
#Nayeli Mart�nez Vel�zquez (UAM-X)
#Diana Rivera Delgado (UAM-X)
#############################
#############################

#############################
#############################
#Se trabaj� con 90 art�culos que se encuentran 
#en el archivo Matriz_Cap�tulo_I
#############################
#############################

#############################
#############################
#Activar paqueter�as
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
#MINER�A DE TEXTO POR PER�ODOS
#############################
#############################

#############################
#############################
#PER�ODO I
#############################
#############################
#Ubico la carpeta que contiene el archivo a analizar
Per_1<-file.path("DIRECTORIO_DE_TRABAJO_DONDE_SE_ENCUENTRA_EL_ARCHIVO_TXT_Per_1")
Per_1
#############################
#############################

#############################
#############################
#Creo un corpus vol�til (Volatile Corpora)
Per_1a<-VCorpus(DirSource(Per_1))
Per_1a
#Reviso caracter�sticas 
class(Per_1a)
summary(Per_1a)
inspect(Per_1a)
#############################
#############################

#############################
#############################
#Limpio el corpus
#�Qu� tengo que limmpiar?
getTransformations()
#removeNumbers, removePunctuation, removeWords, stemDocument, stripWhitespace

#Transformo a "espacio" los siguientes caracteres: "/" , "@" , "-" y "$" 
A_Espacio<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
Per_2<-tm_map(Per_1a, A_Espacio, "/")
Per_3<-tm_map(Per_2, A_Espacio, "@")
Per_4<-tm_map(Per_3, A_Espacio, "-")
Per_5<-tm_map(Per_4, A_Espacio, "$")

#Transformo en letras min�sculas a los caracteres del corpus
Per_6<-tm_map(Per_5, content_transformer(tolower))

#Al realizar un an�lisis de texto no me interesa los n�meros expresados 
#en el corpus
#Remuevo los n�meros
Per_7<-tm_map(Per_6, removeNumbers)

#Remuevo los signos de puntuaci�n
Per_8<-tm_map(Per_7, removePunctuation)

#Remuevo las palabras frecuentes en textos de espa�ol
#OJO: por default siempre se remover� palabras en ingl�s, por lo que es
#necesario especificar el idioma 
#En este caso es un corpus que contiene un documento en espa�ol 
#�Cu�ntas hay?
length(stopwords(kind="en"))

#�Cu�les son?
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
#COEFICIENTE DE ASOCIACI�N
#############################
#############################
Per_11<-DocumentTermMatrix(Per_10)
#Coeficiente de asociaci�n definido por el investigador
#De 0 a 1
findAssocs(Per_13, "PALABRA_A_ESTUDIAR", corlimit = COLOCAR_VALORES_DE_0_A_1)
#And so on...
#############################
#############################

#############################
#############################
#############################
#############################
#El mismo procedimiento se realiz� para el
#Per�odo II y Per�odo III
#############################
#############################
#############################
#############################