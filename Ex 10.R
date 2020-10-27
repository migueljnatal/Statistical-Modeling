# Lista 1 - Script Ex 10

# Ex 10

n=20 #tamanho de cada amostra
x=seq(from=1,to=10.5,by=.5) #valores de x em cada amostra (fiz até 10.5 para serem 20, até 10 eram 19)
nam=100 #número de amostras

amostra=list()
dados=list()
e=list()
reg=50+10*x

for(i in 1:nam){
  e[[i]]=rnorm(n,0,4)
  amostra[[i]]=e[[i]]+reg
  dados[[i]]=data.frame(amostra[[i]],x)
  colnames(dados[[i]])=c("y","x")
}

reglin=list()
b0=rep(0,nam)
b1=rep(0,nam)
for(i in 1:nam){
  attach(dados[[i]])
  reglin[[i]]=lm(y~x)
  b0[i]=reglin[[i]]$coefficients[1]
  b1[i]=reglin[[i]]$coefficients[2]
}

hist(b0)
hist(b1)


## b)

ychapeu=rep(0,nam)
for(i in 1:nam){
  ychapeu[i]=b0[i]+b1[i]*50
}
hist(ychapeu)

## c)

ssres=rep(0,nam)
msres=rep(0,nam)
sxx=rep(0,nam)
ICB1inf=rep(0,nam)
ICB1sup=rep(0,nam)

for(i in 1:nam){
  ssres[i]=sum((dados[[i]][1]-apply(dados[[i]][1],2,mean))^2)-b1[i]*sum((dados[[i]][1]-apply(dados[[i]][1],2,mean))*(dados[[i]][2]-apply(dados[[i]][2],2,mean))) #402.54
  msres[i]=ssres[i]/(n-2)
  sxx[i]=sum((dados[[i]][2]-apply(dados[[i]][2],2,mean))^2)
  ICB1inf[i]=b1[i]-qt(0.975,(n-2))*sqrt(msres[i]/sxx[i]) 
  ICB1sup[i]=b1[i]+qt(0.975,(n-2))*sqrt(msres[i]/sxx[i])
}

a=ICB1inf<=10
b=ICB1sup>=10
c=a+b
table(c)[2]/nam #95% dos IC's para B1 contém o verdadeiro valor de B1 (valor esperado teórico: 95%)

## d)

ICinf95_x50=rep(0,nam)
ICsup95_x50=rep(0,nam)

for(i in 1:nam){
  ICinf95_x50[i]=ychapeu[i]-qt(0.975,(n-2))*sqrt(msres[i]*((1/n)+(50-apply(dados[[i]][2],2,mean))^2/sxx[i]))
  ICsup95_x50[i]=ychapeu[i]+qt(0.975,(n-2))*sqrt(msres[i]*((1/n)+(50-apply(dados[[i]][2],2,mean))^2/sxx[i]))
}


d=ICinf95_x50<=550
e=ICsup95_x50>=550
f=d+e
table(f)[2]/nam #94% (valor esperado teórico 95%)