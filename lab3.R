## generating a random variable from binomial(n,p) using  geometric(p) as proposal
n=10
p=0.3
bino_vec=numeric(n)
geom_vec=numeric(n)
c_vec=numeric(n)
for (i in 1:n){
  bino_vec[i]=choose(n,i)*(p^i)*((1-p)^(n-i))
  geom_vec[i]=((1-p)^i)*p
  c_vec[i]=bino_vec[i]/ geom_vec[i]
}
c=max(c_vec)
## choosing 100  random variable
count=0
bino_vector=numeric(100)
while(count<=100){
  u=runif(1)
  y=rgeom(1,p)
  pi=dbinom(y,n,p)
  qi=rgeom(y,p)
  if (u<pi/(c*qi)){
    bino_vector[count+1]=y
    count<-count+1
  }
  
}
bino_vector


