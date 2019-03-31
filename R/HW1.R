install.packages("plotly")
library(plotly)
n_observation=10000
kup=matrix(runif(n_observation*3,-1,1),ncol=3)
origine_uzalik=sqrt(kup[,1]^2+kup[,2]^2+kup[,3]^2)
colnames(kup) = c("X","Y","Z")
kup2 = as.data.frame(kup)
kupadd = as.data.frame(origine_uzalik)
kup3 = merge(kup2, kupadd, by = "row.names", all = FALSE)
High=ifelse (kup3$origine_uzalik <=1,"0","1")
kup4=data.frame(kup3,High)
plot_ly(kup4, x= ~X, y= ~Y, z= ~Z, color = ~High, group = High, mode = "markers", 
        marker=list( size=3 , opacity=0.5) )
tahminipi=6*sum(origine_uzalik<=1)/n_observation
print(pi)
print(tahminipi)
