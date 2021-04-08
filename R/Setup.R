library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(tidyr)
library(poweRlaw)
library(dplyr)

library(lubridate)
library(MASS)
library(fitdistrplus)
library(parallel)
library(foreach)
library(doParallel)
library(BiocManager)
library(transport)
library(waddR)
library(cowplot)
library(rbenchmark)
library(RColorBrewer)

source('C:\\repos\\mop-auctions\\R\\Functions.R')
source('C:\\repos\\public-procurement\\R\\Functions.R')
bids <- read.csv("C:/repos/public-procurement/data/bids.csv", encoding="CP1252")

df=bids%>%mutate(FechaInicio=as.Date(FechaInicio))

start=2
split1=2
split2=2

#Create different datasets
cutoff0=ymd(min(df$FechaInicio)) + years(start)
cutoff1=ymd(min(df$FechaInicio)) + years(split1+start)
cutoff2=cutoff1 + years(split2)
df.period1=df%>%filter(FechaInicio<=cutoff1&FechaInicio>=cutoff0)
df.period2=df%>%filter(FechaInicio>cutoff1&FechaInicio<=cutoff2)
length(unique(df.period1$Codigo))
length(unique(df.period2$Codigo))

#Create degree distributions for period 1.
#todo: arreglar esta parte
degree.bottom.1=df.period1%>%group_by(RutProveedor)%>%summarise(d=length(unique(NombreOrganismo)))%>%arrange(-d)
degree.bottom.1=degree.bottom.1%>%mutate(v.bottom=seq_len(nrow(degree.bottom.1)))
degree.bottom.2=df.period2%>%group_by(RutProveedor)%>%summarise(d=length(unique(NombreOrganismo)))%>%arrange(-d)
degree.bottom.2=degree.bottom.2%>%mutate(v.bottom=seq_len(nrow(degree.bottom.2)))

degree.top.1=df.period1%>%group_by(NombreOrganismo)%>%summarise(d=length(unique(RutProveedor)))%>%arrange(-d)
degree.top.1=degree.top.1%>%mutate(v.top=seq_len(nrow(degree.top.1)))
degree.top.2=df.period2%>%group_by(NombreOrganismo)%>%summarise(d=length(unique(RutProveedor)))%>%arrange(-d)
degree.top.2=degree.top.2%>%mutate(v.top=seq_len(nrow(degree.top.2)))

n.top.2=nrow(degree.top.2)
n.bottom.2=nrow(degree.bottom.2)
nbottom=nrow(degree.bottom.2)

n.top.1=nrow(degree.top.1)
n.bottom.1=nrow(degree.bottom.1)
nbottom.1=nrow(degree.bottom.1)




ggplot(degree.bottom.1,aes(x=d))+geom_histogram(binwidth = 3)+scale_y_log10()
ggplot(degree.top.1,aes(x=d))+geom_histogram(binwidth = 10)

#TrueEdgelist
lookup_names.top.1=degree.top.1%>%mutate(v.top=seq_len(nrow(degree.top.1)))%>%dplyr::select(NombreOrganismo,v.top)
lookup_names.bottom.1=degree.bottom.1%>%mutate(v.bottom=seq_len(nrow(degree.bottom.1)))%>%dplyr::select(RutProveedor,v.bottom)
lookup_names.top.2=degree.top.2%>%mutate(v.top=seq_len(nrow(degree.top.2)))%>%dplyr::select(NombreOrganismo,v.top)
lookup_names.bottom.2=degree.bottom.2%>%mutate(v.bottom=seq_len(nrow(degree.bottom.2)))%>%dplyr::select(RutProveedor,v.bottom)
edgelist.period1=df.period1%>%dplyr::select(RutProveedor,NombreOrganismo)%>%left_join(lookup_names.top)%>%left_join(lookup_names.bottom)%>%dplyr::select(v.top,v.bottom)















#Auxiliary
names=bids%>%group_by(RutUnidad,NombreOrganismo)%>%summarise()%>%slice(1)

contractors.age=bids%>%group_by(RutProveedor,NombreProveedor)%>%summarise(firstYear=min(year),
                                         lastYear=max(year),
                                         life=lastYear-firstYear,
                                         firstYearAw=min(year[CantidadAdjudicada>=1]),
                                         uniqueOrganism=length(unique(NombreOrganismo)),
                                         uniqueUnits=length(unique(NombreUnidad)),
                                         tot=length(Codigo),
                                         totAdj=length(Codigo[CantidadAdjudicada>=1]),
                                         montoTot=sum(Valor.Total.Ofertado[CantidadAdjudicada>=1]))%>%arrange(-montoTot)%>%ungroup()%>%
                                        mutate(acum=cumsum(montoTot)/sum(montoTot))%>%mutate(Pareto=ifelse(acum<=0.9,1,0))
                                        

#1. Previous cleaning steps 
bids2=bids%>%mutate(nombre=substr(Nombre.producto.genérico,1,10))%>%left_join(contractors.age)%>%mutate(age=year-firstYear, bid_str=MCA_MPO,age_fct=as.factor(age),exp=ifelse(year-firstYearAw>0,year-firstYearAw,0),expFactor=as.factor(exp))
units=bids2%>%group_by(RutUnidad)%>%summarise(monto=sum(Valor.Total.Ofertado))%>%arrange(-monto)%>%left_join(names,by='RutUnidad')

#2. Create Summary Tables
nrow(bids)
nrow(bids2)


#General Contracts statistics
general.statistics=bids2%>%
    filter(Oferta.seleccionada=='Seleccionada')%>%dplyr::select(Codigo,MCA_MPO,Valor.Total.Ofertado,NumeroOferentes,year)%>%
    group_by(Codigo)%>%pivot_longer(cols=MCA_MPO:year)%>%group_by(name)%>%summarise(N=length(Codigo),mean=mean(value),std=sd(value),
 max=max(value),min=min(value))

bids.statistics=bids2 %>%dplyr::select(Codigo,Valor.Total.Ofertado,MCA_MPO)%>%
  pivot_longer(cols=Valor.Total.Ofertado:MCA_MPO)%>%group_by(name)%>%
  summarise(N=length(Codigo),mean=mean(value),std=sd(value),
 max=max(value),min=min(value))

#General Firm Statistics
firm.statistics=contractors.age %>%dplyr::select(RutProveedor,life,tot,totAdj,montoTot,uniqueOrganism,uniqueUnits)%>%
  pivot_longer(cols=life:uniqueUnits)%>%group_by(name)%>%summarise(N=length(value), mean=mean(value),std=sd(value),                                                                                                                                        
                                                                max=max(value),min=min(value))%>%
                                        dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3))))


#3. Key Metrics: Efficiency
#General Descriptive Statistics Of effiency indicator: winning vs all bids
plot.efficency=ggplot(bids,aes(x=MCA_MPO))+geom_histogram(bins=25,color='white',aes(y=..density..))+geom_vline(xintercept = 1,color='red',linetype='dashed')+theme_minimal()+xlab('Bid Amount Divided by Government Estimate')
#plot.efficency.separated=ggplot(bids,aes(x=MCA_MPO))+geom_histogram(bins=25,color='white',aes(y=..density..))+geom_vline(xintercept = 1,color='red',linetype='dashed')+theme_minimal()+xlab('Bid Amount Divided by Government Estimate')+facet_wrap(~Oferta.seleccionada)
#plot.efficency
#plot.efficency.separated
#colnames(bids)

#4. Network Structure
##Degree Distribution
contractors.age.degrees=<contractors.age%>%mutate(centile=ntile(uniqueOrganism,n=100))%>%filter()

degree.organisms=contractors.age.degrees$uniqueOrganism
m_m = displ$new(degree.organisms)
estx = estimate_xmin(m_m)
m_m$setXmin(1)
est = estimate_pars(m_m)
m_m$setPars(est)

plot1=plot(m_m)%>%rename(observed=y)
b=lines(m_m, col = 2)%>%rename(expected=y)
fit.plot=full_join(plot1,b)%>%rename(degree=x)%>%pivot_longer(expected:observed)
g1=ggplot(fit.plot,aes(x=degree))+geom_point(aes(y=value,color=name))+scale_y_log10()+scale_x_log10()+theme_minimal()+ylab('Density')+labs(color='Model')+theme(legend.position='bottom')+theme(legend.text = element_text(size=12))
g1
degree.organisms=contractors.age.degrees$uniqueOrganism
m_m = displ$new(degree.organisms)
estx = estimate_xmin(m_m)
m_m$setXmin(1)
est = estimate_pars(m_m)
m_m$setPars(est)
plot1=plot(m_m)%>%rename(observed=y)
b=lines(m_m, col = 2)%>%rename(expected=y)
fit.plot=full_join(plot1,b)%>%rename(degree=x)%>%pivot_longer(expected:observed)
g2=ggplot(fit.plot,aes(x=degree))+geom_point(aes(y=value,color=name))+scale_y_log10()+scale_x_log10()+theme_minimal()+ylab('Density')+labs(color='')+theme(legend.position='top')+geom_vline(xintercept = 20)

#Formatting Output
general.statistics.output=general.statistics[c(3,2,1,4),]%>%mutate(name=gsub(replacement = "Standarized Winning Bid",pattern='MCA_MPO',x=name),
                                                                   name=gsub(pattern = "Valor.Total.Ofertado",replacement='Total Contract Amount',x=name),
                                                                   name=gsub(pattern = "NumeroOferentes",replacement='Number of Bidders',x=name),
                                                                   name=gsub(pattern = "year",replacement='Year',x=name))%>%mutate_if(is.numeric, funs(as.character(signif(., 3))))
                                                              

bids.statistics.output=bids.statistics[c(2,1),]%>%mutate(name=gsub(replacement = "Standarized Winning Bid",pattern='MCA_MPO',x=name),
                                                         name=gsub(pattern = "Valor.Total.Ofertado",replacement='Total Contract Amount',x=name))%>%mutate_if(is.numeric, funs(as.character(signif(., 3))))

firm.statistics.output=firm.statistics[c(2,3,4,1,5,6),]%>%mutate(name=gsub(replacement = "Total Amount Bidded",pattern='montoTot',x=name),
                                                                                  name=gsub(pattern = "totAdj",replacement='Total Bids Won',x=name),
                                                                                  name=gsub(pattern = "tot",replacement='Total Bids Submittted',x=name),
                                                                                  name=gsub(pattern = "uniqueOrganism",replacement='Unique Organisms participated with',x=name),
                                                                                  name=gsub(pattern = "uniqueUnits",replacement='Unique Units Participated with',x=name),
                                                                                  name=gsub(pattern = "life",replacement='Years with Bids Submitted',x=name))%>%mutate_if(is.numeric, funs(as.character(signif(., 3))))



#create Output
create_kable(general.statistics.output,caption = 'Statistics Across All COntracts')
create_kable(bids.statistics.output,caption = 'Statistics Across All Bids')
create_kable(firm.statistics.output,caption = 'Firms Descriptive Statistics')

###Adress Proble. No works under the rcop are included. 
#1. No MOP Units
bids.MOP=bids%>%filter(RutUnidad=='61.202.000-0')
  grepl( 'MOP', NombreUnidad, fixed = TRUE)

##2. Are there Housing Units and big housing projects
bids.MINVU=bids%>%filter(grepl( 'SERVIU', NombreUnidad, fixed = TRUE))

##3. Are big Health Projects present
bids.salud=bids%>%filter(grepl('Servicio de Salud', NombreUnidad, fixed = TRUE))

##Going to have to be very careful with the filtering in the case of big projects. 
#Study of size of the projects



simple=bids%>%filter(Valor.Total.Ofertado>50e6)%>%arrange(-Valor.Total.Ofertado)
  summary(bids$Valor.Total.Ofertado)
hist(simple$Valor.Total.Ofertado)



####Problem why no MOP UNITS
a=bids%>%group_by(NombreOrganismo,NombreUnidad)%>%count()
b=bids%>%group_by(year,Obras)%>%count()
bids


final.statistics=rbind(general.statistics,bids.statistics)%>%dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3))))
final.statistics=final.statistics[c(4,2,3,1,7,6,5),]

create_kable<-function(df,caption){
kable(
  df, "latex",
  booktabs = T,
  linesep = "",
  align = rep('c', 5),
  caption = caption
) %>% kable_styling(latex_options = c("hold_position"))
}
#Create Histogram of Bids vs Estimated

firm.statistics=contractors.age %>%select(RutProveedor,life,tot,totAdj,montoTot)%>%
  pivot_longer(cols=life:montoTot)%>%group_by(name)%>%summarise(N=length(value),mean=mean(value),std=sd(value),                                                                                                                                        
 max=max(value),min=min(value))%>%
dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3))))
firm.statistics=firm.statistics[c(3,4,2,1),]


ggplot(bids2,aes(x=age))+geom_histogram(aes())+theme_minimal()+scale_x_continuous(breaks=seq(1:10),labels=seq(1:10))

#Model
lm1<-lm(bid_str~exp+Rubro3+NombreUnidad,data=bids2)
stargazer(lm2, type = "latex",omit=c('NombreUnidad'),
          df = FALSE,omit.stat = c( "f","LL","ser","adj.rsq"),no.space = T,title="Regression for specifications 1 and 2")


lm2<-lm(bid_str~expFactor+Rubro3+NombreUnidad,data=bids2)
my.summary.lm(summary(lm2),my.rows =1:20 )

stargazer(lm1,lm2, type = "latex",omit=c('NombreUnidad'),
          df = FALSE,omit.stat = c( "f","LL","ser","adj.rsq"),no.space = T,title="Regression with experience as numerical (1) and factor (2)")


lm2.table=broom::tidy(lm2)[-1,]
lm2.table=lm2.table%>%mutate(AGE=seq_len(nrow(lm2.table)))%>%filter(AGE<=10)%>%
  mutate(significance=ifelse(p.value>0.05, yes='>0.05',no='<0.05'))%>%
  mutate(significance=ifelse(p.value<0.01,yes='<0.01',no=significance))
ggplot(lm2.table,aes(x=AGE,y=estimate,fill=significance))+geom_bar(stat='identity')+theme_minimal()+scale_x_continuous(breaks=seq(1:10),labels=seq(1:10))+
  labs(fill='P-Value')+ylab('EStimate')+theme(legend.position = 'right')+ylim(-0.03,0.015)

library(stargazer)

#Model2. Only big contracts
my.summary.lm(summary(lm1),my.rows =690:730 )
load(file='C:\\repos\\mop-auctions\\DATA\\Tratados\\directorySimplifedDB.Rdata')


#Model 3 Instrument

bids2=bids2%>%mutate(rutcontratista=gsub('\\.','',x = RutProveedor))%>%mutate(rutcontratista=(gsub('-','',x = rutcontratista)))%>%
  mutate(rutcontratista=substr(rutcontratista,1,nchar(rutcontratista)-1))%>%mutate(rutcontratista=as.numeric(rutcontratista))

directory.simplifed.minimal=directory.simplifed%>%select(rutcontratista,inicioact.year)
bids3=bids2%>%left_join(directory.simplifed.minimal)
bids3=bids3%>%mutate(totexp=year-inicioact.year)
nrow(bids2)
summary(bids3$totexp)

