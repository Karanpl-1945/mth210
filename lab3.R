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

## generating a random variable from binomial(n,p) using  geometric(p) as proposal
n=10
p=0.3
bino_vec=numeric(n+1)
geom_vec=numeric(n+1)
c_vec=numeric(n+1)
for (i in 0:n){
  bino_vec[i+1]=choose(n,i)*(p^i)*((1-p)^(n-i))
  geom_vec[i+1]=((1-p)^i)*p
  c_vec[i+1]=bino_vec[i+1]/geom_vec[i+1]
}
c=max(c_vec)

## choosing 10000 random variable
count=0
trial=0
bino_vector=numeric(10000)
trial_vector=numeric(10000)
while(count<=10000){
 
  u=runif(1)
  y=rgeom(1,p)
  pi=dbinom(y,n,p)
  qi=dgeom(y,p)
  if (u<pi/(c*qi)){
    bino_vector[count+1]=y
    trial_vector[count+1]= trial+1
    trial=0
    count<-count+1
   
  }else{
    trial<-trial+1
  }
  
}
trial_vector
mean(bino_vector)
mean(trial_vector)
c_vec


