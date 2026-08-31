# D'abord, j'importe la bibliothèque pour les chaines de Markov
library(markovchain)
# Je crée ma matrice
Test_m=matrix(c(1/4,3/4,3/5,2/5),nrow = 2,byrow = TRUE)
# J'en fais une matrice de transition
Test=new("markovchain",states=c("I","V"),transitionMatrix=Test_m,name="Test")
# J'affiche le truc dont je ne me rappelle plus le nom
plot(Test)
# Je calcul et affiche quelques puissances de la matrice pour ...
Test^2
plot(Test^2)
# Je fais un petit résumée
summary(Test)
# Etats absorbant et états 
steadyStates(Test)
absorbingStates(Test)
# Irréductible
is.irreducible(Test)
# Je crée une séquence à partir de ma chaîne de markov
Test2=markovchainSequence(n=5, markovchain = Test, t0="I")
# Je crée une fonction qui permet de crée des matrices de transtion à partir d'une dataFrame
## Je transforme ma matrice de départ en dataframe
Test_mbis=Test_m^2
Test_Dataframe=data.frame(Test_m,row.names = c("I","V"))
colnames(Test_Dataframe)=c("I","V")
## Je transforme mon data.frame en matrice
as.matrix(Test_Dataframe)
Testbis=new("markovchain",transitionMatrix=as.matrix(Test_Dataframe),name="Testbis")
## Maintenant je crée ma fonction
Markovmatrix=function(Df){
  Df=as.matrix(Df)
  MarkovMat=new("markovchain",transitionMatrix=Df,name="Matrix")
  summary(MarkovMat)
  jpeg(filename = "MarkovChainplot.jpeg")
  plot(MarkovMat,main="Chaine de markov des degrés")
  dev.off()
  return(MarkovMat)
}
## Je test ma fonction
Test_Dataframe=data.frame(Test_m,row.names = c("I","V"))
colnames(Test_Dataframe)=c("I","V")
Markovmatrix(Test_Dataframe)
