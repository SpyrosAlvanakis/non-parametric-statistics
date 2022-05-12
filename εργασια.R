plaisio<-data.frame(letters,1:26)
spyros006alvanakis=c("s" ,"p" ,"y" ,"r", "o", "s",0 ,0 ,6,"a" ,"l", "v", "a" ,"n" ,"a", "k", "i", "s")
Xi<-function (a){
  k=length(a)
  z<-0
  for (i in 1:k){
    for (j in 1:26){
    if (plaisio[j,1]==spyros006alvanakis[i]){
      z<-z+plaisio[j,2]
    }
    else if(plaisio[j,2]==a[i]){
      z<-z+plaisio[j,2]
    }
   }
  }
  print(z)
}
X<-Xi(spyros006alvanakis)

isPrime<-function(n){
  if(n<=1){
    return(FALSE)
  }else if (n<=3){
    return(TRUE)

  }else if(n%%2==0 | n%%3==0){
    return(FALSE)
  }else if(n>=5){
    i=5
    while((i^2)<=n){
      if(n%%i==0 | n%%(i+2)==0){
        return(FALSE)
      }else{
        i=i+6
      }
    }
        return(TRUE)}
  }

SumOfPrimeDivisors<-function(n){
  Sum<-0
  root_n=as.integer(sqrt(n))
  for(i in 1:root_n){
    if(n%%i==0){
      if(i==as.integer(n/i) && isPrime(i)==TRUE){
        Sum=Sum+i
      }else{
        if(isPrime(i)==TRUE){
          Sum=Sum+i
        }
        else if(isPrime(as.integer(n/i))==T){
          Sum=Sum+as.integer(n/i)
        }
      }
    }
  }
  return(Sum)
}
paste(" Sum of prime divisors of", n ,"is", Sum ,sep=" ")

y<-SumOfPrimeDivisors(10*X+1000)
y  

set.seed(y)

x500<-rlnorm(500,0,1)
hist(x500,breaks=100,freq=F,main=paste("Histogram of LogNorm(0,1) for 500 samples"))
curve(dlnorm(x,0,1),add=T,col="red",xlim=c(-10,30),type="l")

x20<-rlnorm(20,0,1)
ks.test(x20,"pexp")
y20<-rexp(20,1)
chisq.test(cbind(x20,y20))

x100<-rlnorm(100,0,1)
ks.test(x100,"pgamma",100,1)
y100<-rgamma(100,100,1)
chisq.test(cbind(x100,y100))

x500<-rlnorm(500,0,1)
ks.test(x500,"pweibull",1,2)
y500<-rweibull(500,1,2)
chisq.test(cbind(x500,y500))


testLogNormal<-function(N,n,a){
  xn1<-rlnorm(n,0,1)
  retur<-c(0,0)
  set.seed(NULL)
  for(i in 1:N){
    xn2<-rlnorm(n,0,1)
    y1<-ks.test(xn1,xn2)
    p1<-y1$p.value
    if(p1<a){
      retur<-retur+c(1,0)
    }
    y2=chisq.test(cbind(xn1,xn2))
    p2=y2$p.value
    if(p2<a){
      retur=retur+c(0,1)
    }
  }
  set.seed(y)
  return(retur/N)
}
testLogNormal(10^4,20,0.05)
testLogNormal(10^4,500,0.05)


set.seed(25)
x500<-rlnorm(500,0,1)
sampleun<-function(x,m,s){
  samunif<-plnorm(x,m,s)
  samunif
}
sampleun(x500,0,1)
ks.test(sampleun(x500,0,1),"punif")
y<-runif(500)
chisq.test(cbind(sampleun(x500,0,1),y))

sample(x100,0,1)
ks.test(sampleun(x100,0,1),"punif")
y<-runif(100)
chisq.test(cbind(sampleun(x100,0,1),y))


antistrof100<-qlnorm(sampleun(x100,0,1),0,1)
ks.test(antistrof100,x100)
chisq.test(cbind(antistrof100,x100))

antistrof20<-qlnorm(sampleun(x20,0,1),0,1)
ks.test(antistrof20,x20)
chisq.test(cbind(antistrof20,x20))


