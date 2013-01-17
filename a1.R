library("rstudio") 
library("timeSeries")
library("forecast")
library("RJDBC")
library("expsmooth")
library("ggplot2")

library("SparseM")
library("hts")


drv <- JDBC("com.vertica.jdbc.Driver",
            "/home/gianferrarif/Software/squirrel-sql-3.3.0/jdbc/vertica_5.1.1_jdk_5.jar",
            "'")
conn <- dbConnect(drv, "jdbc:vertica://alivert00:5433/vertica",
                  user="report",password="report1")


rs <- dbSendQuery(conn, statement = paste(
  "select sum(qta) qta , sum(incasso)/sum(qta) as prezzo,  
v.aaaammgg, 
case when v.COD_TIPO_OFFERTA='00' then 0 else 1 end  as inOfferta,
case when t.des_gds='lun' then 1 else 0 end as isLun,
case when t.des_gds='mar' then 1 else 0 end as isMar,
case when t.des_gds='mer' then 1 else 0 end as isMer,
case when t.des_gds='gio' then 1 else 0 end as isGio,
case when t.des_gds='ven' then 1 else 0 end as isVen,
case when t.des_gds='sab' then 1 else 0 end as isSab,
case when t.des_gds='dom' then 1 else 0 end as isDom
from olap_vendite.fact_vendggall v
inner join ods.dim_tempo t
on v.AAAAMMGG=t.aaaammgg
where
cod_articolo='0141398'
and cod_negozio='022'
group by v.aaaammgg, v.cod_tipo_offerta,t.des_gds
order by v.aaaammgg"
))

dati <- fetch(rs, n = -1)

q<-ts(dati$qta, start = c(2010, 1), frequency = 365) 
p<-ts(dati$prezzo, start = c(2010, 1), frequency = 365) 
prom<-ts(dati$inOfferta, start = c(2010, 1), frequency = 365)
lun<-ts(dati$isLun, start = c(2010, 1), frequency = 365)
mar<-ts(dati$isMar, start = c(2010, 1), frequency = 365)
mer<-ts(dati$isMer, start = c(2010, 1), frequency = 365)
gio<-ts(dati$isGio, start = c(2010, 1), frequency = 365)
ven<-ts(dati$isVen, start = c(2010, 1), frequency = 365)
sab<-ts(dati$isSab, start = c(2010, 1), frequency = 365)
dom<-ts(dati$isDom, start = c(2010, 1), frequency = 365)

qz<-na.interp(q)

qsmooth<-ma(qz, 15,0)

d<-decompose(qsmooth)

seasonal<-d$seasonal

qi<-qz-(d$seasonal)

lim<-800
nextvals<-50

#xregrs=matrix(c(p[0:lim],prom[0:lim]),ncol=2)
xregrs=matrix(c(p[0:lim],prom[0:lim],lun[0:lim],mar[0:lim],mer[0:lim],gio[0:lim],ven[0:lim],sab[0:lim],dom[0:lim]),ncol=9)

#xnextregrs=matrix(c(p[(lim+1):(lim+100)],prom[(lim+1):(lim+100)]),ncol=2)
xnextregrs=matrix(c(p[(lim+1):(lim+nextvals)],prom[(lim+1):(lim+nextvals)],lun[(lim+1):(lim+nextvals)],mar[(lim+1):(lim+nextvals)],mer[(lim+1):(lim+nextvals)],gio[(lim+1):(lim+nextvals)],ven[(lim+1):(lim+nextvals)],sab[(lim+1):(lim+nextvals)],dom[(lim+1):(lim+nextvals)]),ncol=9)


fit2<-auto.arima((qi[0:lim]),xreg=xregrs,allowdrift=TRUE, stepwise=TRUE, D=365) 

fcast2<-forecast(fit2,xreg=xnextregrs) 

time <- seq((lim-100), (lim+nextvals))
fc <- c(qi[(lim-100):(lim)],fcast2$mean+seasonal[(lim+1):(lim+nextvals)])
actual<-qi[(lim-100):(lim+nextvals)]

data <- data.frame(time, fc ,actual)
Molten <- melt(data, id.vars = "time")
ggplot(Molten, aes(x = time, y = value, colour = variable)) + geom_line()




