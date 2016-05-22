# LEE ALTURA DE CONSTRUCCIONES
setwd("C:/DADES/DadesObertes/CLUSTER")
setwd("C:/Users/Antonio/Google Drive/civitas")
library(rgdal)

# Funcion para poner altura en numerico
#   Podria tener mas conceptos
valora<-function(x){
  cade=c()
  for (k in x){
    cad=unlist(strsplit(as.character(k),"[+]"))
    uni=c()
    for(n in cad){    #Ordenados por longitud de cadena
      n<-gsub("VIII","8",n)
      n<-gsub("VII","7",n)
      n<-gsub("XII","12",n)
      n<-gsub("XI","11",n)
      n<-gsub("IX","9",n)
      n<-gsub("X","10",n)
      n<-gsub("IV","4",n)
      n<-gsub("VII","7",n)
      n<-gsub("VI","6",n)
      n<-gsub("V","5",n)
      n<-gsub("III","3",n)
      n<-gsub("II","2",n)
      n<-gsub("I","1",n)
      uni=paste0(uni,"+",n)
    }
    uni2<-sub("[+]","",uni)
    cade<-append(cade,uni2)
  }
  return(cade)
}

# FUNCION PARA VALORAR RESTO DE TEXTO
texto<-function(x){
  cade=c()
  for (k in x){
    cad=unlist(strsplit(as.character(k),"[+]"))
    uni=c()
    for(n in cad){
      n<-gsub("POR","*1",n)
      n<-gsub("TZA","*0",n)
      n<-gsub("B","*0",n)
      n<-gsub("PI","*0",n)
      n<-gsub("CONS","*0",n)
      n<-gsub("CO","*1",n)
      n<-gsub("SS","*0",n)
      n<-gsub("SUELO","*0",n)
      n<-gsub("SOP","*1",n)
      n<-gsub("TEN","*0",n)
      n<-gsub("TRF","*1",n)
      n<-gsub("ZPAV","*0",n)
      n<-gsub("ZD","*0",n)
      n<-gsub("Z","*0",n)
      n<-gsub("ALT","*1",n)
      n<-gsub("T","*1",n)
      n<-gsub("DEP","*1",n)
      n<-gsub("ESC","*1",n)
      n<-gsub("RF","*0",n)
      n<-gsub("EN","*0",n)
      n<-gsub("PJE","*0",n)
      n<-gsub("P","*0",n)
      n<-gsub("RU1NA","*0",n)
      n<-gsub("A","*0*",n)
      n<-gsub("S","*0",n)
      n<-gsub("K","0",n)
      n<-gsub("\\?","0",n)
      n<-gsub("-","0*",n) # Evita contabilizar bajo rasantes
      uni=paste0(uni,"+",n)
    }
    uni<-gsub("\\+\\*","+0*",uni)
    #     uni<-sub("[+,*]","",uni)
    cade<-append(cade,uni)
  }
  return(cade)
}

# LEE LA CONSTRUCCION
constru<- readOGR("../2016/43_155_UA_2016-01-24_SHF/CONSTRU", "CONSTRU" ,stringsAsFactors=FALSE)

# CREACION DE DICCIONARIO DE ALTURAS
# FACTORES DE ALTURA EN UN VECTOR
plantas<-levels(as.factor(constru$CONSTRU))

# NOMBRA CADA FACTOR
names(plantas)<-plantas

# APLICA LA FUNCION DE TRADUCIR NUMERO DE PLANTAS
valor<-valora(plantas)
# VINCULA NOMBRES Y ALTURAS
names(valor)<-plantas

# OPCIONALMENTE - APLICACION DE LA FUNCION A TODO EL FICHERO
# constru$plantas<-valora(constru$CONSTRU)

# APLICA LA FUNCION DE TRADUCIR USOS DE PLANTAS
edif<-texto(valor)

# RESULTADOS
edif
valor
f1<- function(x) eval(parse(text=x))
num<-lapply(edif,f1)
head(num)

# OPCIONALMENTE - APLICACION DE LA FUNCION A TODO EL FICHERO
constru$texto<-valora(constru$CONSTRU)
constru$alto<-texto(constru$texto)
plantas<-lapply(constru$alto,f1)
str(plantas)
constru$plantas<-unlist(plantas)
str(constru@data)
# AGRUPA CONTRUCCIONES POR $REFCAT
# spol1 <- gUnionCascaded(constru, constru$REFCAT)

parcel<- readOGR(".", "MORFOcluster")
plot(constru,col=constru$plantas,lty=0)
plot(parcel,col="transparent", add=TRUE)

writeOGR(constru,"../2016/43_155_UA_2016-01-24_SHF/CONSTRU",layer="CONSTRU_alto",driver="ESRI Shapefile")

