library(VGAM)
brant_cr=function(index, print.res){
  
  Xtest=read.csv("Data_clean.csv", header=TRUE, row.names = 1)
  Xtest=Xtest[,index]
  setwd('..')
  setwd('..')
  y=factor(read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)$Result.W.L.D, ordered = TRUE)
  setwd('./Analyse/2020')
  J=3
  K=dim(Xtest)[2]
  
  
  y1=ifelse(y==-1,0,1)
  y2=ifelse(y==1,1,0)
  binmod1=vglm(y1~as.matrix(Xtest), family=sratio(parallel = FALSE, reverse = TRUE))
  binmod2=vglm(y2~as.matrix(Xtest), family=sratio(parallel = FALSE, reverse=TRUE))
  mod1=vglm(data=Xtest, formula=y~., family = sratio(parallel = TRUE, reverse=TRUE))
  beta.hat=matrix(NA,nrow=J-1,ncol=K+1,byrow=T)
  beta.hat[1,]=coef(binmod1)
  beta.hat[2,]=coef(binmod2)
  var.hat=list()
  var.hat[[1]]=vcov(binmod1)
  var.hat[[2]]=vcovvlm(binmod2)
  
  
  
  tau = matrix(mod1@coefficients[1:2],nrow=1,ncol=J-1,byrow=T)
  pi.hat = matrix(NA,nrow=length(binmod1@fitted.values[,2]),ncol=J-1,byrow=T)
  pi.hat[,1] = binmod1@fitted.values[,2]
  pi.hat[,2] = binmod2@fitted.values[,2]
  
  Xtest = cbind(1, Xtest)
  Xtest=as.matrix(Xtest)
  
  varBeta = matrix(NA,nrow = (J-1)*K, ncol = (J-1)*K)
  for(m in 1:(J-2)){
    for(l in (m+1):(J-1)){
      Wml = Matrix::Diagonal(x=pi.hat[,l] - pi.hat[,m]*pi.hat[,l])
      Wm = Matrix::Diagonal(x=pi.hat[,m] - pi.hat[,m]*pi.hat[,m])
      Wl = Matrix::Diagonal(x=pi.hat[,l] - pi.hat[,l]*pi.hat[,l])
      Xt = t(Xtest)
      varBeta[((m-1)*K+1):(m*K),((l-1)*K+1):(l*K)] = as.matrix((solve(Xt %*% Wm %*% Xtest)%*%(Xt %*% Wml %*% Xtest)%*%solve(Xt %*% Wl %*% Xtest))[-1,-1])
      varBeta[((l-1)*K+1):(l*K),((m-1)*K+1):(m*K)] = varBeta[((m-1)*K+1):(m*K),((l-1)*K+1):(l*K)]
    }
  }
  
  betaStar = c()
  for(m in 1:(J-1)){
    betaStar = c(betaStar,beta.hat[m,-1])
  }
  
  for(m in 1:(J-1)){
    varBeta[((m-1)*K+1):(m*K),((m-1)*K+1):(m*K)] = var.hat[[m]][-1,-1]
  }
  
  I = diag(1,K)
  E0 = diag(0,K)
  for(i in 1:(J-2)){
    for(j in 1:(J-1)){
      if(j == 1){
        temp = I
      }else if(j == i+1){
        temp = cbind(temp,-I)
      }else{
        temp = cbind(temp,E0)
      }
    }
    if(i==1){
      D = temp
    }else{
      D = rbind(D,temp)
    }
  }
  X2 = t(D%*%betaStar) %*% solve(D %*% varBeta %*% t(D)) %*% (D %*% betaStar)
  df.v = (J-2)*K
  
  
  for(k in 1:K){
    s = seq(from=k,to=K*(J-1),by=K)
    Ds = D[,s]
    if (!is.null(dim(Ds))){
      Ds = Ds[which(!apply(Ds == 0, 1, all)), ]
    }
    if(!is.null(dim(Ds)))
      X2 = c(X2,t(Ds%*%betaStar[s]) %*% solve(Ds %*% varBeta[s,s] %*% t(Ds)) %*% (Ds %*% betaStar[s]))
    else
      X2 = c(X2,t(Ds%*%betaStar[s]) %*% solve(Ds %*% varBeta[s,s] %*% t(t(Ds))) %*% (Ds %*% betaStar[s]))
    df.v = c(df.v,J-2)
  }
  
  
  p=pchisq(X2,df.v, lower.tail = FALSE)
  dec=ifelse(p<0.05,"H1","H0")
  
  res=data.frame(cbind(c("Omnibus",colnames(Xtest)[-1]),round(X2, digits = 2),df.v,round(p,digits = 3),dec))
  colnames(res)=c("Test for","X2", "df", "p-value", "Test decision")
  if(print.res==TRUE){
    print(res)
  }
  return(res)
}
