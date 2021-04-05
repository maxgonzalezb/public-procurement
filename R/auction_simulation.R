#Explorar el tipo de distribucion de las bids
bids.period1.study=df.period1%>%filter(NumeroOferentes<20&a)
ggplot(df.period1,aes(x=NumeroOferentes, y=MCA_MPO))+geom_jitter()+geom_smooth()+xlim(0,15)

df.period1$NumeroOferentes
  df.period1$MCA_MPO

head(simulated.2)
vert.top.sim2=simulated.2%>%group_by(v.top)%>%count()
vert.bottom.sim2=simulated.2%>%group_by(v.bottom)%>%count()


colnames(df)
hist(a)
b=rnorm(n = 1000, mean = 1, sd = 0.2)%>%as.data.frame()
colnames(b)<-'x'
a=rlogis(n = 10000, location = 1, scale = 0.05)
c=(rt(n = 10000, df = 10)+1)%>%as.data.frame()
colnames(c)<-'x'
ggplot(c,aes(x=x))+geom_density()
ggplot(c,aes(x=x))+geom_density()+geom_density(data=b,aes(x=x),color='red')
summary(c)
hist(a)


#Create Table
#sdparam=0.15
sdparam=0.08
scaleparam=0.05
df=50
min_n=rep(100,300)
for (i in seq_len(300)) {
min_j=rep(0,1000)
for (j in seq_len(1000)) {
  min_j[j]=min(rnorm(n = i,mean = 1,sd = sdparam)%>%as.vector())
  #min_j[j]=min(rt(n = i,df=df)%>%as.vector())
 
  #min_j[j]=min(rlogis(n = 1000, location = 1, scale = 0.001))
  #min_j[j]=min(rlnorm(n = i,meanlog = 0,sdlog = 0.25)%>%as.vector())
  #min_j[j]=min(runif(n = i,min=0.6,max=1.5)%>%as.vector())
}
min_n[i]=mean(min_j)
#vec_min=rt(n = 4,df = 4,),mean = 1,sd = 0.05)%>%as.vector()  
#min_n[i]=min(vec_min)
}
min_n[1]=2#Immportante que sea dos

bids.sim=data.frame()
for (v in seq_len(nrow(vert.top.sim2))) {

edges.gov=simulated.2%>%filter(v.top==v)
connected.firms=vert.bottom.sim2%>%filter(v.bottom%in%edges.gov$v.bottom)  

totfirms=nrow(connected.firms)
#commoncomp=rnorm(n = 1,mean = 0,sd = 0.1)
commoncomp=runif(n = 1,min = -0.01,max = 0.01)
#commoncomp=rt(n = 1,df=10)+1
valuations=rnorm(n = totfirms,mean = 1,sd = sdparam)%>%as.vector()
#valuations=rt(n = totfirms,df=df)%>%as.vector()+1
#valuations=rlnorm(n = totfirms,mean = 0,sdlog = 0.25)%>%as.vector()
#valuations=rlogis(n = totfirms, location = 1, scale = scaleparam)
#valuations=runif(n = totfirms,min = 0.6,max=1.5)%>%as.vector()
#connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse(valuation<(1/log(1+nrow(connected.firms))),yes = 1,no = 0))
connected.firms.vals=connected.firms%>%cbind(valuation=valuations)%>%mutate(binbid=ifelse((commoncomp+valuation)<=commoncomp+min_n[1+max(0,ceiling((log(totfirms)-0*rbinom(n=1,size = 1,prob = 0.5))))],yes = 1,no = 0))
submitted.bids.unit=connected.firms.vals%>%filter(binbid==1)%>%as.data.frame()%>%mutate(ID=v,bid=valuation+commoncomp)
bids.sim=rbind(submitted.bids.unit,bids.sim)

}

participants.sim=bids.sim%>%group_by(ID)%>%count()
participants.period1=((df.period1%>%group_by(Codigo)%>%summarise(participants=NumeroOferentes[1])))

mean(participants.period1$participants)
mean(participants.sim$n)
mean(bids.sim$bid)
mean(bids.sim$valuation)
mean(df.period1$MCA_MPO)
mean(df.period2$MCA_MPO)
sd(bids.sim$bid)
sd(df.period1$MCA_MPO)

hist(df.period2$MCA_MPO,xlim = c(0,2))
hist(bids.sim$bid,xlim = c(0,2))
hist(bids.sim$valuation,xlim = c(0,2))



  uk=df.period1%>%filter(NumeroOferentes<10&MCA_MPO!=1)
uk=uk%>%mutate(month=month(FechaInicio))%>%mutate(size=ntile(MontoEstimado, 10),time=ntile(TiempoDuracionContrato, 10))
# ggplot(uk,aes(x=MCA_MPO))+geom_density()+ylim(0,30)
# ggplot(bids.sim,aes(x=bid))+geom_density()+ylim(0,30)
# ggplot(uk,aes(x=MCA_MPO))+geom_histogram(aes(y=..density..))+ylim(0,10)
# ggplot(bids.sim,aes(x=bid))+geom_histogram(aes(y=..density..))+ylim(0,30)
#  ggplot(bids.sim,aes(x=bid))+geom_histogram(aes(y=..density..))+ylim(0,30)

hist(df.period1$MCA_MPO, xlim = c(0,2),breaks = seq(0,2,by = 0.1))
hist(bids.sim$bid, breaks = seq(-1,2,by = 0.1),xlim = c(0,2))
hist(bids.sim$valuation, breaks = seq(-1,2,by = 0.1),xlim = c(0,2))
#hist(bids.sim$valuation, xlim = c(0,2), breaks=10)

hist(participants.period1$participants)
summary(participants.period1$participants)
hist(participants.sim$n)
summary(participants.sim$n)

nrow(vert.top.sim2)

hist(min_n,xlim=c(0,2))
