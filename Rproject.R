# Installation et appelation des libraries nécessaires
# install.packages('xlsx')
# install.packages(janitor)
library(xlsx)
library(janitor)

# Fonction de simulation de données à partir d'un tableau
Simul= function(tab,s){
  tab=read.xlsx(tab,2)
  View(tab)
  tab=as.data.frame(tab)
  clean_names(tab)
  print(sapply(tab,class))
  #Je crée une dataframe pour acceuillir les valeurs simulées qualitatives
  b=data.frame()
  a=data.frame()
  noms2=c()
  noms=c()
  print(ncol(tab))
  print(nrow(tab))
  for (i in 1:ncol(tab)){
    for (j in 1:nrow(tab)){
      val1=tab[j,i,drop=T]  
      val2=tab[j+1,i,drop=T]
      if (is.numeric(val1)&&is.numeric(val2)&&!is.na(val2)){
        a=rbind(a,sample(val1:val2,s,replace=TRUE))
        print(colnames(tab[i]))
        noms=cbind(noms,colnames(tab[i]))
      }
    }
    if(!is.numeric(tab[[i]])){
      b=rbind(b,sample(na.omit(tab[[i]][tab[[i]]!=""]),s,replace=T))
      noms2=cbind(noms2,colnames(tab[i]))
    }
  }
  rownames(a)=noms
  rownames(b)=noms2
  View(a)
  View(b)
  colnames(b)=paste0("id",1:ncol(b))
  if(nrow(a)!=0){
    colnames(a)=paste0("id",1:ncol(a))
  }
  print("Souhaitez vous afficher un graphique?(O/N)")
  b=rbind(a,b)
  b=t(b)
  View(b)
  b=as.data.frame(b)
  return(b)
}