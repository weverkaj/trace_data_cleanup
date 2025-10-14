#Codeing by: Daniela Yaffar 
#For ms: Experimental warming and its legacy effect on root dynamics after hurricane disturbance in a 
#tropical forest of Puerto Rico
#root recovery after hurricane disturbance in a tropical forest

#Glossary
#"before" = before the hurricanes
#"after"= after the hurricanes
#"Session"= When root dynamics where measured. We started with session 1 in Feb 2017, and every
#2 weeks aprox is another session (continues even after the hurricanes)
#treament= Warming vs Control

#Packages

library(ggplot2)
library(officer)
library(rvg)
######################### READ VECTORIZED DATA BY TUBE (NO TUBES 6 OR 10)############################

# setwd(here("data_raw/ess_dive_1f9df391a77aeeb_20241028T151655779170/data/"))
#Reading each tube
t1=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 1.csv")
t2=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 2.csv")
t3=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 3.csv")
t4=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 4.csv")
t5=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 5.csv")
#t6=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 6.csv") removed due to hurricane damage
t7=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 7.csv")
t8=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 8.csv")
t9=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 9.csv")
#t10=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 10.csv") removed due to hurricane damage
t11=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 11.csv")
t12=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tube 12.csv")

#binding all tubes
all=rbind(t1[,1:142],t2[,1:142],t3[,1:142],t4[,1:142],t5[,1:142],t7[,1:142],t8[,1:142],t9[,1:142],t11[,1:142],t12[,1:142]) #t6[,1:142],t10[,1:142],
all$Died..session.number.=as.numeric(all$Died..session.number.)


### tube dimensions##########################################
Tubes <- data.frame(tube=seq(1,12,1))
Tubes$minwin[Tubes$tube==1] <- 11; Tubes$windepth[Tubes$tube==1]  <- 0.524
Tubes$minwin[Tubes$tube==2] <- 17; Tubes$windepth[Tubes$tube==2]  <- 0.133
Tubes$minwin[Tubes$tube==3] <- 17; Tubes$windepth[Tubes$tube==3]  <- 0.405
Tubes$minwin[Tubes$tube==4] <- 21; Tubes$windepth[Tubes$tube==4]  <- 0.229
Tubes$minwin[Tubes$tube==5] <- 18; Tubes$windepth[Tubes$tube==5]  <- 0.472
#Tubes$minwin[Tubes$tube==6] <- 19; Tubes$windepth[Tubes$tube==6]  <- 0.144
Tubes$minwin[Tubes$tube==7] <- 17; Tubes$windepth[Tubes$tube==7]  <- 0.344
Tubes$minwin[Tubes$tube==8] <- 20; Tubes$windepth[Tubes$tube==8]  <- 0.361
Tubes$minwin[Tubes$tube==9] <- 23; Tubes$windepth[Tubes$tube==9]  <- 0.213
#Tubes$minwin[Tubes$tube==10] <- 21; Tubes$windepth[Tubes$tube==10]  <- 0.290
Tubes$minwin[Tubes$tube==11] <- 19; Tubes$windepth[Tubes$tube==11]  <- 0.570
Tubes$minwin[Tubes$tube==12] <- 20; Tubes$windepth[Tubes$tube==12]  <- 0.180

Tubes$nWindows <- (220 - Tubes$minwin) +1
Tubes$binlength <- 10*0.63/Tubes$windepth
Tubes$tubelength <- Tubes$nWindows*0.63
Tubes$tubedepth <- Tubes$nWindows*Tubes$windepth

write.csv(Tubes,"Tubes.csv",row.names = F)


all <- merge(all,Tubes, by.x="Tube",by.y="tube")
all$depth <- (all$Window-all$minwin+1)*all$windepth


#Classifying windows by every 10 cm range in the soil depth profile
all$Depth.bin[all$depth<10]="0-10"
all$Depth.bin[all$depth>=10&all$depth<20]="10-20"
all$Depth.bin[all$depth>=20&all$depth<30]="20-30"
all$Depth.bin[all$depth>=30&all$depth<40]="30-40"
all$Depth.bin[all$depth>=40&all$depth<50]="40-50"
all$Depth.bin[all$depth>=50&all$depth<60]="50-60"
all$Depth.bin[all$depth>=60&all$depth<70]="60-70"
all$Depth.bin[all$depth>=70&all$depth<80]="70-80"
all$Depth.bin[all$depth>=80&all$depth<90]="80-90"
all$Depth.bin[all$depth>=90&all$depth<100]="90-100"
all$Depth.bin[all$depth>=100&all$depth<110]="100-110"
all$Depth.bin[all$depth>=110&all$depth<120]="110-120"


#Assigning plots to tubes
all$plot[all$Tube==1|all$Tube==2] = 1
all$plot[all$Tube==3|all$Tube==4] = 2
all$plot[all$Tube==5|all$Tube==6] = 4
all$plot[all$Tube==7|all$Tube==8] = 6
all$plot[all$Tube==9|all$Tube==10] = 5
all$plot[all$Tube==11|all$Tube==12] = 3

#calculate days
sessions=as.Date(c("2/6/2017","2/21/2017","3/6/2017","3/20/2017","3/29/2017","4/28/2017","5/15/2017","5/30/2017",
                   "6/19/2017","7/3/2017","7/18/2017","7/31/2017","8/13/2017","8/31/2017","10/12/2017","10/27/2017",
                   "11/14/2017","11/29/2017","12/12/2017","12/27/2017","1/10/2018","1/25/2018","2/7/2018","2/22/2018",
                   "3/4/2018","3/28/2018","4/11/2018","4/25/2018","5/9/2018","5/23/2018","6/6/2018","6/21/2018","7/4/2018"),
                 "%m/%d/%Y")


#Calculating production, mortality, and stock by session 

allbydate=matrix(nrow=0,ncol=20)
colnames(allbydate)=c("tube","plot","window","depthbin(cm)","depth(cm)","rootID","date","session","duration(d)",
                      "del_length(mm)","length(mm)","diam(mm)","del_diam.mm","production(mg)","prodbystock","prod2.mg",
                      "mortality(mg)","mortLength (mm)","mortdiam_mm","stock(mg)")

datecols=which(grepl("Length",colnames(all)))
diamcols=which(grepl("Diameter",colnames(all)))
datecols = datecols[-1]
#diamcols = diamcols[-1]

i=1
pb<- txtProgressBar(min=1, max=nrow(all),style=3)
for (r in 1:nrow(all)){
  dead<-F
  for (c in datecols[2:length(datecols)]){
    if (dead==T){}else{
      Date=sessions[i]+floor((sessions[i+1]-sessions[i]))/2
      Dur = as.numeric(difftime(sessions[i+1],sessions[i],"days"))
      if (is.na(all[r,datecols[which(datecols %in% c)-1]])){
        if (!is.na(all[r,c])){
          del.len.mm <- all[r,c]   ## root appeared with this length
          len.prev <- 0
          diam.prev <-0
          del.diam.mm. <- all[r,c+1]   ## root appeared with this diam
          len.mm <- all[r,c]
          diam.mm.<-all[r,c+1]
        }else{
          del.len.mm <- NA   ### root has not yet appeared or is already dead
          len.prev <- NA
          diam.prev <- NA
          del.diam.mm.<-NA
          len.mm <- NA
          diam.mm.<-NA
        }
      }else{
        len.prev <- all[r,datecols[which(datecols %in% c)-1]]
        del.len.mm=all[r,c] - len.prev  ### change in length from previous session
        diam.prev <- all[r,diamcols[which(diamcols %in% (c+1))-1]]
        del.diam.mm.<-all[r,c+1]-diam.prev
        len.mm <- all[r,c]
        diam.mm.<-all[r,c+1]
      }
      maxRML= 3.002*(diam.mm.^(2.0096))  ##mg/cm ### JW - what are these transformations?
      prod.mg=maxRML*(del.len.mm/10)
      prod2.mg=(3.002*(diam.mm.^(2.0096))*(len.mm/10)) - (3.002*(diam.prev^(2.0096))*(len.prev/10))
      stock <- maxRML*(len.mm/10) 
      stock.prev <- 3.002*(diam.prev^(2.0096))*(len.prev/10)
      prod.mg.bystock <-  stock-stock.prev
      #if (del.len.mm==0&del.diam.mm.!=0){
      #  prod.mg <- prod.mg.bystock
      #}
      biomassmort <- NA
      len.mm.dead <- NA
      diam.mm.dead<-NA
      if (all$Died[r]!="-"&!is.na(all$Died[r])){
        deaddate=sessions[which(grepl(sessions[all$Died..session.number.[r]],sessions))]+
          floor((sessions[which(grepl(sessions[all$Died..session.number.[r]],sessions))-1]-sessions[which(grepl(sessions[all$Died..session.number.[r]],sessions))]))/2
        lastcol=which(grepl(sessions[all$Died..session.number.[r]],sessions))-1
        if (lastcol == which(datecols %in% c)-1){
          deadsess=lastcol+1
          lastcol=datecols[lastcol]
          Dur = as.numeric(difftime(sessions[which(grepl(sessions[all$Died..session.number.[r]],sessions))],
                                    sessions[which(grepl(sessions[all$Died..session.number.[r]],sessions))-1],"days"))
          len.mm.dead = all[r,lastcol]
          diam.mm.dead=all[r,lastcol+1]
          biomassmort=len.mm.dead*3.002*(diam.mm.dead^(2.0096))/10
          prod.mg <- NA
          prod.mg.bystock <- NA
          prod2.mg<- NA
          stock <- NA
          dead=T
        }else{
          len.mm.dead=NA
          biomassmort=NA
          diam.mm.dead=NA
        }
      }
      allbydate=rbind(allbydate,c(all$Tube[r],all$plot[r],all$Window[r],as.character(all$Depth.bin[r]),all$depth[r],all$RootID[r],as.character(Date),
                                  i+1,Dur,del.len.mm,len.mm,diam.mm.,del.diam.mm.,prod.mg,prod.mg.bystock,prod2.mg,biomassmort,len.mm.dead,diam.mm.dead,stock))
      #if (nrow(allbydate)>1&!is.na(as.numeric(allbydate[nrow(allbydate),19]))&sum(allbydate[nrow(allbydate),1:6]==allbydate[nrow(allbydate)-1,1:6])==6){
      # if (round(sum(as.numeric(allbydate[nrow(allbydate),19]),-as.numeric(allbydate[nrow(allbydate)-1,19]),na.rm=T),10)!=
      #    round(as.numeric(allbydate[nrow(allbydate),15]),10)){browser()}
      #}
      
      i=i+1  #keep track of session number
    }
  }
  Sys.sleep(0.1); setTxtProgressBar(pb, r)
  i=1
}
close(pb)

#save the new csv 
allbydate_copy <- allbydate 

write.csv(allbydate, "allbydateWithSA2_16.12.20.csv") ######check the whole code and make sure the results are the same than before now witht this version for dec 16th 2020
write.csv(all, "all_16.12.20.csv")


#######   with "all" dataset already constructed
#read the csv created
allbydate=read.csv("allbydateWithSA2_16.12.20.csv")
all=read.csv("all_16.12.20.csv")
Tubes <- read.csv("Tubes.csv")


allbydate$date <- as.Date(allbydate$date,"%Y-%m-%d")

#Clasify before and after the hurricane

allbydate$hurricane[allbydate$session <= 14]="before"
allbydate$hurricane[allbydate$session > 14]="after"

##calculate previous diameter and length for the newRML prod2
allbydate$diam.prev <- allbydate$diam.mm.-allbydate$del_diam.mm
allbydate$length.prev <- allbydate$length.mm.-allbydate$del_length.mm.

#Clasify by diameter
allbydate$diam2[allbydate$diam.mm.<1]=allbydate$diam2[allbydate$mortdiam_mm <1]="Finer"
allbydate$diam2[allbydate$diam.mm.<2&allbydate$diam.mm.>1]=allbydate$diam2[allbydate$mortdiam_mm<2&allbydate$mortdiam_mm>1]="Fine"
allbydate$diam2[allbydate$diam.mm.>=2]=allbydate$diam2[allbydate$mortdiam_mm>=2]="Coarse"

#Clasify with treatment
allbydate$treatment[allbydate$tube==3|allbydate$tube==4|allbydate$tube==5|allbydate$tube==6|allbydate$tube==7|allbydate$tube==8]="Warming"
allbydate$treatment[allbydate$tube==1|allbydate$tube==2|allbydate$tube==9|allbydate$tube==10|allbydate$tube==11|allbydate$tube==12]="Control"
allbydate$treatment=factor(allbydate$treatment,levels=c("Warming","Control"))


##adding root surface area (SA)
allbydate$SA=allbydate$length.mm.*2*pi*(allbydate$diam.mm./2)
#allbydate$crossarea=(3.14*(allbydate$diam.mm./2)^2)##remember that this is only for one cross section and not 2, if you want to add this to the SA then multiply by 2
allbydate$prodSA=allbydate$del_length.mm.*2*pi*(allbydate$del_diam.mm/2)
allbydate$morSA=allbydate$mortLength..mm.*2*pi*(allbydate$mortdiam_mm/2)



#####################################################
##########   find optimal RML coefficients   ########
library(ggplot2)
rml=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/RML_calibration_2019.csv")
rml_before= read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/all.core.campaigns.forRML.csv")
rml$hurricane <- "after"
rml_before$hurricane <- "before"
rml <- rml[,c("Root.Diameter..mm.","RML.g.cm","hurricane")]
rml_before <- rml_before[rml_before$type!="remain"&rml_before$date!="Nov"&rml_before$Year!=2019,c("Ave..Diam.mm","RML.g.cm","hurricane")]
colnames(rml_before) <- colnames(rml)
rml <- rbind(rml,rml_before)
rml$RML.g.cm=rml$RML.g.cm*1000 


power_eqn = function(df, start){
  m = nls(RML.g.cm ~ a*Root.Diameter..mm.^b, start = start, data = df);
  eq <- substitute(italic(y) == a  ~italic(x)^b, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2)))
  as.character(as.expression(eq));                 
}

##now using Iversen et al (2008) equation

Iversen <- as.data.frame(seq(0,6,.1))
names(Iversen)<-"Root.Diameter..mm."
Iversen$RML.g.cm <- (3*Iversen$Root.Diameter..mm.^2.01)#*sample(c(0.9,0.91,0.96,0.97,0.95,0.98),nrow(Iversen),replace=T)
Iversen$hurricane="Iversen"

###  using nls to extract the a and b coefficients
mod.before.a <- nls(RML.g.cm ~ a*(Root.Diameter..mm.^b), start = list(a=3,b=2),
                  data = rml[rml$hurricane=="before",],control=list(maxiter=1000))
summary(mod.before.a)
rml$RML.before.a[rml$hurricane=="before"] <- coefficients(mod.before.a)[1]*(rml$Root.Diameter..mm.[rml$hurricane=="before"]^coefficients(mod.before.a)[2])
plot(rml$Root.Diameter..mm.,rml$RML.g.cm)
points(rml$Root.Diameter..mm.,rml$RML.before.a,col="red")


# y = ax^b
# log(y)=log(a*x^b)
# log(y)=log(a)+b*log(x)
# log(a) = intercept; a= exp(a)
# b = slope

mod.before.b <- lm(data=rml[rml$hurricane=="before",],formula=log(RML.g.cm)~log(Root.Diameter..mm.))
exp(mod.before.b$coefficients[1]) #a
mod.before.b$coefficients[2] #b
rml$RML.before.b[rml$hurricane=="before"] <- exp(mod.before.b$coefficients[1])*(rml$Root.Diameter..mm.[rml$hurricane=="before"]^mod.before.b$coefficients[2])


### compare the two methodsnls and log
plot(rml$RML.before.a,rml$RML.before.b)
abline(a=0,b=1)

### Compare with the actual data
plot(rml$Root.Diameter..mm.[rml$hurricane=="before"],rml$RML.g.cm[rml$hurricane=="before"])
# ##  nls prediction
points(rml$Root.Diameter..mm.[rml$hurricane=="before"],rml$RML.before.a[rml$hurricane=="before"],pch=20,
       col="red")
# ###  linear prediction
points(rml$Root.Diameter..mm.[rml$hurricane=="before"],rml$RML.before.b[rml$hurricane=="before"],pch=20,
       col="blue")
# ###  Iversen equation
points(Iversen$Root.Diameter..mm.,Iversen$RML.g.cm,col="green",pch=20)


#AFTER
###  using nls to extract the a and b coefficients
mod.after.a <- nls(RML.g.cm ~ a*(Root.Diameter..mm.^b), start = list(a=3,b=2),
                 data = rml[rml$hurricane=="after",],control=list(maxiter=1000))
rml$RML.after.a[rml$hurricane=="after"] <- coefficients(mod.after.a)[1]*(rml$Root.Diameter..mm.[rml$hurricane=="after"]^coefficients(mod.after.a)[2])
summary(mod.after.a)

# y = ax^b
# log(y)=log(a*x^b)
# log(y)=log(a)+b*log(x)
# log(a) = intercept; a= exp(a)
# b = slope

mod.after.b <- lm(data=rml[rml$hurricane=="after"&rml$RML.g.cm>0,],formula=log(RML.g.cm)~log(Root.Diameter..mm.))
exp(mod.after.b$coefficients[1]) #a
mod.after.b$coefficients[2] #b
rml$RML.after.b[rml$hurricane=="after"] <- exp(mod.after.b$coefficients[1])*(rml$Root.Diameter..mm.[rml$hurricane=="after"]^mod.after.b$coefficients[2])

###  compare the rml predictions for after the hurricane, between nls and linear methods
plot(rml$RML.after.a,rml$RML.after.b)
abline(a=0,b=1)
plot(rml$Root.Diameter..mm.[rml$hurricane=="after"],rml$RML.g.cm[rml$hurricane=="after"])
# ##  nls prediction
points(rml$Root.Diameter..mm.[rml$hurricane=="after"],rml$RML.after.a[rml$hurricane=="after"],pch=20,
       col="red")
# 
# ###  linear prediction
points(rml$Root.Diameter..mm.[rml$hurricane=="after"],rml$RML.after.b[rml$hurricane=="after"],pch=20,
       col="blue")
# ###  Iversen equation
points(Iversen$Root.Diameter..mm.,Iversen$RML.g.cm,col="green",pch=20)



### apply the modeled coefficients to the data ####
#difference between production.mg.newRML and production2.mg.newRML is the RML used (to test difference
#in results for using same vs different RML before and after the hurricanes)

##  first the before data 
##  
maxRML= 3.0002*(allbydate$diam.mm.[allbydate$session<14]^2.0096)
allbydate$production.mg.newRML[allbydate$session<14] <-  maxRML*allbydate$del_length.mm.[allbydate$session<14]/10
plot(allbydate$production.mg.[allbydate$session<14],allbydate$production.mg.newRML[allbydate$session<14])

### Using the new coefficients for the before data
maxRML= exp(mod.before.b$coefficients[1])*(allbydate$diam.mm.[allbydate$session<=14]^mod.before.b$coefficients[2])
allbydate$production.mg.newRML[allbydate$session<=14] <-  maxRML*allbydate$del_length.mm.[allbydate$session<=14]/10
allbydate$production2.mg.newRML[allbydate$session<=14] <-  exp(mod.before.b$coefficients[1])*(allbydate$diam.mm.[allbydate$session<=14]^mod.before.b$coefficients[2])*(allbydate$length.mm.[allbydate$session<=14]/10)-
  exp(mod.before.b$coefficients[1])*(allbydate$diam.prev[allbydate$session<=14]^mod.before.b$coefficients[2])*(allbydate$length.prev[allbydate$session<=14]/10)


allbydate$stock.mg.newRML[allbydate$session<=14] <-  maxRML*allbydate$length.mm.[allbydate$session<=14]/10
maxRML.mort= exp(mod.after.b$coefficients[1])*(allbydate$mortdiam_mm[allbydate$session<=14]^mod.after.b$coefficients[2])
allbydate$mortality.mg.newRML[allbydate$session<=14] <-  maxRML.mort*allbydate$mortLength..mm.[allbydate$session<=14]/10
plot(allbydate$production.mg.[allbydate$session<=14],allbydate$production.mg.newRML[allbydate$session<=14],
     xlab="Original RML Prod",ylab="New RML Prod",main="Before")

###  Then after
maxRML= exp(mod.after.b$coefficients[1])*(allbydate$diam.mm.[allbydate$session>14]^mod.after.b$coefficients[2])
allbydate$production.mg.newRML[allbydate$session>14] <-  maxRML*allbydate$del_length.mm.[allbydate$session>14]/10
allbydate$production2.mg.newRML[allbydate$session>14] <- exp(mod.before.b$coefficients[1])*(allbydate$diam.mm.[allbydate$session>14]^mod.before.b$coefficients[2])*(allbydate$length.mm.[allbydate$session>14]/10)-
  exp(mod.before.b$coefficients[1])*(allbydate$diam.prev[allbydate$session>14]^mod.before.b$coefficients[2])*(allbydate$length.prev[allbydate$session>14]/10)


allbydate$stock.mg.newRML[allbydate$session>14] <-  maxRML*allbydate$length.mm.[allbydate$session>14]/10
maxRML.mort= exp(mod.after.b$coefficients[1])*(allbydate$mortdiam_mm[allbydate$session>14]^mod.after.b$coefficients[2])
allbydate$mortality.mg.newRML[allbydate$session>14] <-  maxRML.mort*allbydate$mortLength..mm.[allbydate$session>14]/10
plot(allbydate$production.mg.[allbydate$session>14],allbydate$production.mg.newRML[allbydate$session>14],
     xlab="Original RML Prod",ylab="New RML Prod",main="After")



#### converting NAs to zero should only be done when calculating per m3 or per m2, not per root

allbydate$mortality.mg.[is.na(allbydate$mortality.mg.)] <- 0
allbydate$production.mg.[is.na(allbydate$production.mg.)] <- 0
allbydate$prodbystock[is.na(allbydate$prodbystock)] <- 0
allbydate$stock.mg.[is.na(allbydate$stock.mg.)] <- 0 
allbydate$del_length.mm.[is.na(allbydate$del_length.mm.)] <- 0
allbydate$length.mm.[is.na(allbydate$length.mm.)] <- 0
allbydate$mortLength..mm.[is.na(allbydate$mortLength..mm.)] <- 0
allbydate$del_diam.mm[is.na(allbydate$del_diam.mm)]<-0
allbydate$diam.mm.[is.na(allbydate$diam.mm.)]<-0
allbydate$mortdiam_mm[is.na(allbydate$mortdiam_mm)] <- 0
allbydate$SA[is.na(allbydate$SA)]<-0
allbydate$prodSA[is.na(allbydate$prodSA)]<-0
allbydate$morSA[is.na(allbydate$morSA)] <- 0
allbydate$mortality.mg.newRML[is.na(allbydate$mortality.mg.newRML)] <- 0
allbydate$production.mg.newRML[is.na(allbydate$production.mg.newRML)] <- 0
allbydate$stock.mg.newRML[is.na(allbydate$stock.mg.newRML)] <- 0
allbydate$prod2.mg[is.na(allbydate$prod2.mg)] <- 0
allbydate$production2.mg.newRML[is.na(allbydate$production2.mg.newRML)] <- 0
write.csv(allbydate, "allbydateWithSAandNEWRML2.16.12.20.csv")




totProd <- aggregate(data=allbydate,cbind(production.mg.,prodbystock,prod2.mg,mortality.mg.,stock.mg.,del_length.mm.,mortLength..mm.,length.mm.,prodSA,morSA,SA,production.mg.newRML,production2.mg.newRML,stock.mg.newRML,mortality.mg.newRML)~
                       treatment+tube+diam2+hurricane+session+depthbin.cm.+date,FUN=sum,na.action = na.omit)

names(totProd)[c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)] <- c("prodbybin","prodbybin.bystock","prod2mg.bin","mortbybin","stockbybin","dlenbybin","mortLengthbybin","lenbybin","prodSAbybin","morSAbybin","SAbybin",
                                                                   "prodnewRMLbybin","prod2newRMLbybin","stocknewRMLbybin","mornewRMLbybin")

totProd <- merge(totProd,Tubes[,c("tube","binlength","tubelength","tubedepth")],by="tube",all.x=T)

totProd$depthbin.cm.<- as.character(totProd$depthbin.cm.)
totProd$treatment <- as.character(totProd$treatment)
totProd$date <- as.character(totProd$date)
totProd$date <- as.Date(totProd$date)



#### finds windows that are NA and puts a zero instead ------------------------------
totProd$date <- as.character(totProd$date)
totProd$added=NA
pb<- txtProgressBar(min=1, max=12,style=3)

for (t in c(1:5,7:9,11:12)){
  dat <- totProd[totProd$tube==t,]
  maxd <- 10*(Tubes$tubedepth[Tubes$tube==t]%/%10 + as.logical(Tubes$tubedepth[Tubes$tube==t]%%10))
  bins <- paste(seq(0,floor(Tubes$tubedepth[Tubes$tube==t]/10) * 10,10),seq(10,maxd,10),sep="-")
  for(d in bins){
    for (s in unique(dat$session)){
      if (!d %in% dat$depthbin.cm.[dat$session==s]){
        totProd <- rbind(totProd,c(t,unique(dat$treatment),"Finer",as.character(unique(dat$hurricane[dat$session==s])),s,d,as.character(unique(dat$date[dat$session==s])),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,unique(dat$binlength[dat$tube==t]),unique(dat$tubelength[dat$tube==t]),unique(dat$tubedepth[dat$tube==t]),"added"))
        totProd <- rbind(totProd,c(t,unique(dat$treatment),"Fine",as.character(unique(dat$hurricane[dat$session==s])),s,d,as.character(unique(dat$date[dat$session==s])),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,unique(dat$binlength[dat$tube==t]),unique(dat$tubelength[dat$tube==t]),unique(dat$tubedepth[dat$tube==t]),"added"))
        totProd <- rbind(totProd,c(t,unique(dat$treatment),"Coarse",as.character(unique(dat$hurricane[dat$session==s])),s,d,as.character(unique(dat$date[dat$session==s])),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,unique(dat$binlength[dat$tube==t]),unique(dat$tubelength[dat$tube==t]),unique(dat$tubedepth[dat$tube==t]),"added"))
      }else{
        for(f in c("Fine","Finer","Coarse")){
          if( !f %in% dat$diam2[dat$depthbin.cm.==d&dat$session==s]){
            totProd <- rbind(totProd,c(t,unique(dat$treatment),f,as.character(unique(dat$hurricane[dat$session==s])),s,d,as.character(unique(dat$date[dat$session==s])),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,unique(dat$binlength[dat$tube==t]),unique(dat$tubelength[dat$tube==t]),unique(dat$tubedepth[dat$tube==t]),"added"))
          }
        }
        
      }
    }
  }
  Sys.sleep(0.1); setTxtProgressBar(pb,t)
}

write.csv(totProd,"totProdWSAandNEWRML2.16.12.20.csv",row.names=F)

           
##########################################
#####  Start here once datasets are made
############################################

allbydate=read.csv("allbydateWithSAandNEWRML2.16.12.20.csv")
session_lengths <- aggregate(allbydate$duration.d.~allbydate$session,FUN=mean)
names(session_lengths) <- c("session","dur")
totProd <- read.csv("totProdWSAandNEWRML2.16.12.20.csv")

totProd$date <- as.character(totProd$date)
totProd$date <- as.Date(totProd$date)


#totProd$diam.mm <- totProd$SA/(totProd$lenbybin*pi)

Tubes <- read.csv("Tubes.csv")

###adding plots (Also possible to add this in the merge of creating totprod before adding the zeros)
totProd$plot[totProd$tube==1|totProd$tube==2] = 1
totProd$plot[totProd$tube==3|totProd$tube==4] = 2
totProd$plot[totProd$tube==5] = 4
totProd$plot[totProd$tube==7|totProd$tube==8] = 6
totProd$plot[totProd$tube==9] = 5
totProd$plot[totProd$tube==11|totProd$tube==12] = 3

totProd <- merge(totProd,session_lengths,by=c("session"),all.x=T)



#Make mg.cm3 and mg cm2 by day using the new rml
#first to cm3
depthoffield=0.1
#distance from the tube into the soil (interface where we see the root grow)



#######   divide by the volume of soil        #########
totProd$prod.mg.cm3 <- totProd$prodbybin/(totProd$binlength*0.84*depthoffield)
totProd$prod.mg.cm3.day=totProd$prod.mg.cm3/totProd$dur

totProd$prod2bin.mg.cm3 <-totProd$prod2mg.bin/(totProd$binlength*0.84*depthoffield)
totProd$prod2.mg.cm3.day=totProd$prod2bin.mg.cm3/totProd$dur

totProd$prodbystock.mg.cm3 <- totProd$prodbybin.bystock/(totProd$binlength*0.84*depthoffield)
totProd$prodbystock.mg.cm3.day=totProd$prodbystock.mg.cm3/totProd$dur

totProd$mort.mg.cm3 <- totProd$mortbybin/(totProd$binlength*0.84*depthoffield)
totProd$mort.mg.cm3.day=totProd$mort.mg.cm3/totProd$dur

totProd$stock.mg.cm3 <- totProd$stockbybin/(totProd$binlength*0.84*depthoffield)

totProd$dlen.mm.cm3 <- totProd$dlenbybin/(totProd$binlength*0.84*depthoffield)
totProd$dlen.mm.cm3.day=totProd$dlen.mm.cm3/totProd$dur

totProd$mortLength.mm.cm3 <- totProd$mortLengthbybin/(totProd$binlength*0.84*depthoffield)
totProd$mortLength.mm.cm3.day <- totProd$mortLength.mm.cm3/totProd$dur

totProd$len.mm.cm3 <- totProd$lenbybin/(totProd$binlength*0.84*depthoffield)

totProd$prod.mg.newRMLmg.cm3 <- totProd$prodnewRMLbybin/(totProd$binlength*0.84*depthoffield)##depth of field 0.1
totProd$prod.mg.newRMLmg.cm3.day <- totProd$prod.mg.newRMLmg.cm3/totProd$dur
totProd$mort.mg.newRML.cm3 <- totProd$mornewRMLbybin/(totProd$binlength*0.84*depthoffield)
totProd$mort.mg.newRML.cm3.day <- totProd$mort.mg.newRML.cm3/totProd$dur
totProd$stock.mg.newRML.cm3 <- totProd$stocknewRMLbybin/(totProd$binlength*0.84*depthoffield)
totProd$prod2.mg.newRML.cm3=totProd$prod2newRMLbybin/(totProd$binlength*0.84*depthoffield)
totProd$prod2.mg.newRML.cm3.day <- totProd$prod2.mg.newRML.cm3/totProd$dur

#SA
totProd$prodSA.mm2.cm3 <- totProd$prodSAbybin/(totProd$binlength*0.84*depthoffield)
totProd$prodSA.mm2.cm3.day=totProd$prodSA.mm2.cm3/totProd$dur
totProd$morSA.mm2.cm3 <- totProd$morSAbybin/(totProd$binlength*0.84*depthoffield)
totProd$morSA.mm2.cm3.day=totProd$morSA.mm2.cm3/totProd$dur

totProd$SA.mm2.cm3 <- totProd$SAbybin/(totProd$binlength*0.84*depthoffield)


#########   to get by area of the soil surface   ########
totProd$prod.mg.cm2 <- totProd$prod.mg.cm3*10 ##10 is the depth bin
totProd$prod.mg.cm2.day=totProd$prod.mg.cm2/totProd$dur

totProd$prod2.mg.cm2 <- totProd$prod2bin.mg.cm3*10 ##10 is the depth bin
totProd$prod2.mg.cm2.day=totProd$prod2.mg.cm2/totProd$dur

totProd$prodbystock.mg.cm2 <- totProd$prodbystock.mg.cm3*10 ##10 is the depth bin
totProd$prodbystock.mg.cm2.day=totProd$prodbystock.mg.cm2/totProd$dur

totProd$mort.mg.cm2 <- totProd$mort.mg.cm3*10 
totProd$mort.mg.cm2.day=totProd$mort.mg.cm2/totProd$dur

totProd$stock.mg.cm2 <- totProd$stock.mg.cm3*10 ##10 is the depth bin

totProd$dlen.mm.cm2 <- totProd$dlen.mm.cm3*10 ##10 is the depth bin
totProd$dlen.mm.cm2.day=totProd$dlen.mm.cm2/totProd$dur

totProd$mortLength.mm.cm2 <- totProd$mortLength.mm.cm3*10
totProd$mortLength.mm.cm2.day=totProd$mortLength.mm.cm2/totProd$dur

totProd$len.mm.cm2 <- totProd$len.mm.cm3*10 ##10 is the depth bin

#new RML
totProd$stock.newRMLmg.cm2 <- totProd$stock.mg.newRML.cm3*10 ##10 is the depth bin
totProd$prod.newRMLmg.cm2 <- totProd$prod.mg.newRMLmg.cm3*10 ##10 is the depth bin
totProd$prod.newRMLmg.cm2.day=totProd$prod.newRMLmg.cm2/totProd$dur
totProd$prod.newRMLmg.cm2.day[totProd$prod.newRMLmg.cm2.day<0]=0

totProd$prod2.newRMLmg.cm2 <- totProd$prod2.mg.newRML.cm3*10 ##10 is the depth bin
totProd$prod2.newRMLmg.cm2.day=totProd$prod2.newRMLmg.cm2/totProd$dur
totProd$prod2.newRMLmg.cm2.day[totProd$prod2.newRMLmg.cm2.day<0]=0

totProd$mort.newRMLmg.cm2 <- totProd$mort.mg.newRML.cm3*10 
totProd$mort.newRMLmg.cm2.day=totProd$mort.newRMLmg.cm2/totProd$dur
totProd$mort.newRMLmg.cm2.day[totProd$mort.newRMLmg.cm2.day<0]=0


#SA (Surface area of the root)
totProd$prodSA.mm2.cm2 <- totProd$prodSA.mm2.cm3*10
totProd$prodSA.mm2.cm2.day=totProd$prodSA.mm2.cm2/totProd$dur
totProd$prodSA.mm2.cm2.day[totProd$prodSA.mm2.cm2<0]=0
totProd$morSA.mm2.cm2 <- totProd$morSA.mm2.cm3*10
totProd$morSA.mm2.cm2.day=totProd$morSA.mm2.cm2/totProd$dur
totProd$morSA.mm2.cm2.day[totProd$morSA.mm2.cm2<0]=0
totProd$SA.mm2.cm2 <- totProd$SA.mm2.cm3*10


#make all columns numeric
totProd[,c("session","prodbybin","mortbybin","prod.mg.cm3","mort.mg.cm3","prod.mg.cm2","mort.mg.cm2","stock.mg.cm2","stock.mg.cm3","SAbybin","morSAbybin","prodSAbybin")] <- sapply(totProd[,c("session","prodbybin","mortbybin","prod.mg.cm3","mort.mg.cm3","prod.mg.cm2","mort.mg.cm2","stock.mg.cm2","stock.mg.cm3","SAbybin","morSAbybin","prodSAbybin")],as.numeric)
#totProd.win[,c("session","prodbywin","mortbywin","prod.mg.cm3","mort.mg.cm3","prod.mg.cm2","mort.mg.cm2","stock.mg.cm2","stock.mg.cm3","morSAbywin","prodSAbywin","SAbywin")] <- sapply(totProd.win[,c("session","prodbywin","mortbywin","prod.mg.cm3","mort.mg.cm3","prod.mg.cm2","mort.mg.cm2","stock.mg.cm2","stock.mg.cm3","morSAbywin","prodSAbywin","SAbywin")],as.numeric)



totProd$SA.m2.m2=totProd$SA.mm2.cm2/100
totProd$prodSA.m2.m2.day=totProd$prodSA.mm2.cm2.day/100
totProd$morSA.m2.m2.day=totProd$morSA.mm2.cm2.day/100
totProd$stock.newRMLg.m2=totProd$stock.newRMLmg.cm2*10
totProd$prod.newRMLg.m2.day=totProd$prod.newRMLmg.cm2.day*10
totProd$mort.newRMLg.m2.day=totProd$mort.newRMLmg.cm2.day*10
totProd$stock.g.m2=totProd$stock.mg.cm2*10
totProd$prod.g.m2.day=totProd$prod.mg.cm2.day*10
totProd$mort.g.m2.day=totProd$mort.mg.cm2.day*10
totProd$prod.g.m2.day.arit=totProd$prodbystock.mg.cm2.day*10
totProd$prod2.g.m2.day=totProd$prod2.mg.cm2.day*10
totProd$prod2.newRMLg.m2.day=totProd$prod2.newRMLmg.cm2.day*10



write.csv(totProd,"totProdfinal2Dec9.2019.16.12.20.csv")



###NPP calculations ______________________________________________________________________________

NPPcal=read.csv("totProdfinal2Dec9.2019.16.12.20.csv")

NPP <- aggregate(data=NPPcal,prod2.newRMLmg.cm2~
                   tube+plot+treatment+hurricane,FUN=sum)

NPP2=aggregate(data=NPP,prod2.newRMLmg.cm2~plot+treatment+hurricane,FUN=mean)

NPP3=aggregate(data=NPP2,prod2.newRMLmg.cm2~treatment+hurricane,FUN=mean)




###################calculating turnover#######################################################################


turnover <- merge(aggregate(data=NPPcal,cbind(prod2.newRMLmg.cm2,mort.newRMLmg.cm2,SA.m2.m2)~
                              tube+plot+treatment+hurricane,FUN=sum),
                  aggregate(data=NPPcal[!duplicated(NPPcal[,c("session","tube")]),
                                        c("session","tube","dur","hurricane")],dur~tube+hurricane,FUN=sum),
                  by=c("tube","hurricane"))


turnover2 <- aggregate(data=NPPcal,cbind(stock.newRMLmg.cm2,SA.m2.m2)~
                         tube+plot+treatment+session+hurricane,FUN=sum)

turnover3 <- merge(turnover,aggregate(data=turnover2,cbind(stock.newRMLmg.cm2,SA.m2.m2)~
                                        tube+plot+treatment+hurricane,FUN=mean),by=c("tube","hurricane","treatment","plot"))

turnover3$prod.turnover <- (turnover3$prod2.newRMLmg.cm2/(turnover3$dur)*365)/turnover3$stock.newRMLmg.cm2
turnover3$mort.turnover <- (turnover3$mort.newRMLmg.cm2/(turnover3$dur)*365)/turnover3$stock.newRMLmg.cm2




#############################################
#### plotting time-series    ############### 
############################################
totProd=read.csv("totProdfinal2Dec9.2019.16.12.20.csv")
totProd$date <- as.Date(totProd$date,"%Y-%m-%d")


totProd$treatment <- factor(totProd$treatment,levels=c("Warming","Control"))



##this is by totprod old RML (just to test the RML differences)

SE <- function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))
Agg1 <- aggregate(data=totProd,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~tube+plot+treatment+date,FUN=sum)
Agg1 <- aggregate(data=Agg1,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~plot+treatment+date,FUN=mean)
Agg1_depth <- aggregate(data=totProd,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~tube+plot+treatment+depthbin.cm.+date,FUN=sum)
Agg1_depth <- aggregate(data=Agg1_depth,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~plot+treatment+depthbin.cm.+date,FUN=mean)

Agg2 <- aggregate(data=Agg1,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~treatment+date,FUN=mean,na.rm=T)
Agg2_depth <- aggregate(data=Agg1_depth,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~treatment+date+depthbin.cm.,FUN=mean,na.rm=T)
Agg2 <- merge(Agg2,aggregate(data=Agg1,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~treatment+date,FUN=SE),
              by=c("treatment","date"))
Agg2_depth <- merge(Agg2_depth,aggregate(data=Agg1_depth,cbind(prod.g.m2.day,prod2.g.m2.day,mort.g.m2.day,stock.g.m2,prod.g.m2.day.arit)~treatment+date+depthbin.cm.,FUN=SE),by=c("treatment","date","depthbin.cm."))


library(ggplot2)
######  production
   
g <- ggplot(Agg2,aes(x=date,y=prod.g.m2.day.x,col=factor(treatment),shape=treatment))+
  geom_point(size=2) +
  geom_errorbar(aes(x=date,ymin=prod.g.m2.day.x-prod.g.m2.day.y,ymax=prod.g.m2.day.x+prod.g.m2.day.y),
                size=1,alpha=0.7)+
  geom_line(size=1)+
  scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.POSIXct("2017-09-27"),lty=2)+
  labs(x="Date",y="Root Production (mg/cm2/day)")+
  theme_classic()



##############NOW prod with NEW RML ######################################################### BY DAY______________?

totProd$date <- as.POSIXct(totProd$date)

# totProd$prod.g.m2= totProd$prod.mg.cm2*10
# totProd$prod2.g.m2= totProd$prod2.mg.cm2*10
# totProd$mort.g.m2= totProd$mort.mg.cm2*10


SE <- function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))
Agg1.new <- aggregate(data=totProd,cbind(prod.newRMLg.m2.day, prod2.newRMLg.m2.day, mort.newRMLg.m2.day,stock.newRMLg.m2,prod.g.m2.day.arit)~tube+plot+treatment+date,FUN=sum)
Agg1.new <- aggregate(data=Agg1.new,cbind(prod.newRMLg.m2.day,prod2.newRMLg.m2.day,mort.newRMLg.m2.day,stock.newRMLg.m2,prod.g.m2.day.arit)~plot+treatment+date,FUN=mean)

Agg2.new <- aggregate(data=Agg1.new,cbind(prod.newRMLg.m2.day,prod2.newRMLg.m2.day,mort.newRMLg.m2.day,stock.newRMLg.m2,prod.g.m2.day.arit)~treatment+date,FUN=mean,na.rm=T)
Agg2.new <- merge(Agg2.new,aggregate(data=Agg1.new,cbind(prod.newRMLg.m2.day,prod2.newRMLg.m2.day,mort.newRMLg.m2.day,stock.newRMLg.m2,prod.g.m2.day.arit)~treatment+date,FUN=SE),
                  by=c("treatment","date"))


Agg2.new$rml <-"new"
Agg2$rml <- "old"
names(Agg2) <- names(Agg2.new)
Agg2.new <- rbind(Agg2.new,Agg2)


library(ggplot2)
library(scales)

######  production
######       
g <- ggplot(Agg2.new[Agg2.new$rml=="new",],aes(x=date,y=prod2.newRMLg.m2.day.x,col=factor(treatment),shape=treatment))+
  geom_point(size=2) +
  geom_errorbar(aes(x=date,ymin=prod2.newRMLg.m2.day.x-prod2.newRMLg.m2.day.y,ymax=prod2.newRMLg.m2.day.x+prod2.newRMLg.m2.day.y),
                size=0.5,alpha=0.5)+
  geom_line(size=1)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.POSIXct("2017-09-21"),lty=2)+
  #scale_x_date(date_breaks="3 months",labels=date_format("%m-%y"))+
  labs(x="Date",y="Root Production (g/m2/day)")+
  theme_classic()


#graph mortality with time

g <- ggplot(Agg2.new[Agg2.new$rml=="new",],aes(x=date,y=mort.newRMLg.m2.day.x,col=factor(treatment),shape=treatment))+
  geom_point(size=2) +
  geom_errorbar(aes(x=date,ymin=mort.newRMLg.m2.day.x-mort.newRMLg.m2.day.y,ymax=mort.newRMLg.m2.day.x+mort.newRMLg.m2.day.y),
                size=0.5,alpha=0.5)+
  geom_line(size=1)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.POSIXct("2017-09-21"),lty=2)+
  labs(x="Date",y="Root Mortality (g/m2/day)")+
  ylim(0,80)+
  theme_classic()




##graph stock with time

g <- ggplot(Agg2.new[Agg2.new$rml=="new",],aes(x=date,y=stock.newRMLg.m2.x,col=factor(treatment),shape=treatment))+
  geom_point(size=2) +
  geom_errorbar(aes(x=date,ymin=stock.newRMLg.m2.x-stock.newRMLg.m2.y,ymax=stock.newRMLg.m2.x+stock.newRMLg.m2.y),
                size=0.5,alpha=0.5)+
  geom_line(size=1)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.POSIXct("2017-09-21"),lty=2)+
  labs(x="Date",y="Root stock (g/m2)")+
  theme_classic()


###  use the Check Timelines of Aggregates script to check the timelines.


#---------------MEANS---------------------------------------

##before

meanbefore=Agg2.new[Agg2.new$rml=="new"&Agg2.new$date<"2017-09-20 19:00:00",]

#stock**************
meanstock=mean(meanbefore$stock.newRMLg.m2.x)
SE(meanbefore$stock.newRMLg.m2.x)
mean(meanbefore$stock.newRMLg.m2.x[meanbefore$treatment=="Control"])
SE(meanbefore$stock.newRMLg.m2.x[meanbefore$treatment=="Control"])

mean(meanbefore$stock.newRMLg.m2.x[meanbefore$treatment=="Warming"])
SE(meanbefore$stock.newRMLg.m2.x[meanbefore$treatment=="Warming"])

sdstock=sd(meanbefore$stock.newRMLg.m2.x)
CV=(sdstock/meanstock)*100


#initial date values before hurricane
Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-02-13 18:00:00"&
                              Agg2.new$treatment=="Control"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2017-02-13 18:00:00"&
                              Agg2.new$treatment=="Control"]

Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-02-13 18:00:00"&
                              Agg2.new$treatment=="Warming"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2017-02-13 18:00:00"&
                              Agg2.new$treatment=="Warming"]

#last dates before the hurricanes
Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                              Agg2.new$treatment=="Control"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                              Agg2.new$treatment=="Control"]

Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                              Agg2.new$treatment=="Warming"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                              Agg2.new$treatment=="Warming"]

#prod*************
meanprod=mean(meanbefore$prod2.newRMLg.m2.day.x)
mean(meanbefore$prod2.newRMLg.m2.day.x[meanbefore$treatment=="Control"])
mean(meanbefore$prod2.newRMLg.m2.day.x[meanbefore$treatment=="Warming"])
sdprod=sd(meanbefore$prod2.newRMLg.m2.day.x)
CV=(sdprod/meanprod)*100

#mort
meanmort=mean(meanbefore$mort.newRMLg.m2.day.x)
mean(meanbefore$mort.newRMLg.m2.day.x[meanbefore$treatment=="Control"])
mean(meanbefore$mort.newRMLg.m2.day.x[meanbefore$treatment=="Warming"])

sdmort=sd(meanbefore$mort.newRMLg.m2.day.x)
CV=(sdmort/meanmort)*100


##after hurricane means

meanafter=Agg2.new[Agg2.new$rml=="new"&Agg2.new$date>"2017-09-19 19:00:00",]

#Stock
mean(meanafter$stock.newRMLg.m2.x)
mean(meanafter$stock.newRMLg.m2.x[meanafter$treatment=="Control"])
mean(meanafter$stock.newRMLg.m2.x[meanafter$treatment=="Warming"])

#initial dates after the hurricanes
Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-10-19 19:00:00"&
                              Agg2.new$treatment=="Control"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                              Agg2.new$treatment=="Control"]

Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                              Agg2.new$treatment=="Warming"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                              Agg2.new$treatment=="Warming"]

#final dates after the hurricanes
Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2018-6-26 19:00:00"&
                              Agg2.new$treatment=="Control"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2018-6-26 19:00:00"&
                              Agg2.new$treatment=="Control"]

Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2018-6-26 19:00:00"&
                              Agg2.new$treatment=="Warming"]
Agg2.new$stock.newRMLg.m2.y[Agg2.new$rml=="new"&Agg2.new$date=="2018-6-26 19:00:00"&
                              Agg2.new$treatment=="Warming"]

#prod
mean(meanafter$prod2.newRMLg.m2.day.x)
mean(meanafter$prod2.newRMLg.m2.day.x[meanafter$treatment=="Control"])
mean(meanafter$prod2.newRMLg.m2.day.x[meanafter$treatment=="Warming"])

#mort
mean(meanafter$mort.newRMLg.m2.day.x)
mean(meanafter$mort.newRMLg.m2.day.x[meanafter$treatment=="Control"])
mean(meanafter$mort.newRMLg.m2.day.x[meanafter$treatment=="Warming"])


#-----
  
#Differences in biomass
#after hurricane
DifControl=Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2018-6-26 19:00:00"&
                                         Agg2.new$treatment=="Control"] -
  Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-10-19 19:00:00"&
                                Agg2.new$treatment=="Control"]
#percet
100*(DifControl/Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-10-19 19:00:00"&
                                              Agg2.new$treatment=="Control"])


DifWarm=Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2018-6-26 19:00:00"&
                                      Agg2.new$treatment=="Warming"]-
  Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                                Agg2.new$treatment=="Warming"]
#percent
100*(DifWarm/ Agg2.new$stock.newRMLg.m2.x[Agg2.new$rml=="new"&Agg2.new$date=="2017-09-20 19:00:00"&
                                            Agg2.new$treatment=="Warming"])



#now only coarse roots (if you run this, you have to re-run the above code for NEW RML in order to
#run the next code of "differences of root dynamics with treatment")

SE <- function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))
Agg1.new <- aggregate(data=totProd,cbind(prod.newRMLg.m2.day, prod2.newRMLg.m2.day, mort.newRMLg.m2.day,stock.newRMLg.m2,prod.g.m2.day.arit)~tube+plot+treatment+date+diam2,FUN=sum)
Agg1.new <- aggregate(data=Agg1.new,cbind(prod.newRMLg.m2.day,prod2.newRMLg.m2.day,mort.newRMLg.m2.day,stock.newRMLg.m2,prod.g.m2.day.arit)~plot+treatment+date+diam2,FUN=mean)
#
Agg1.coarsea=Agg1.new[Agg1.new$diam2=="Coarse"&Agg1.new$date>"2017-09-20",] #for the mixed model
write.csv(Agg1.coarsea,"coarse roots for mixed model.csv")




###DIFFERENCES OF ROOT DYNAMICS with treatment plots ####################------------------------

### difference warming - control
#production
plotdata <- merge(Agg2.new[Agg2.new$treatment=="Warming"&Agg2.new$rml=="new",c("date","prod2.newRMLg.m2.day.x","mort.newRMLg.m2.day.x", "stock.newRMLg.m2.x"),],
                  Agg2.new[Agg2.new$treatment=="Control"&Agg2.new$rml=="new",c("date","prod2.newRMLg.m2.day.x","mort.newRMLg.m2.day.x", "stock.newRMLg.m2.x") ],
                  by="date")
plotdata$date=as.character(plotdata$date)
plotdata$date=as.Date(plotdata$date)
plotdata$hurricane[plotdata$date<=as.Date("2017-09-20")]="before"
plotdata$hurricane[plotdata$date>as.Date("2017-09-20")]="after"
plotdata$dif=plotdata$prod2.newRMLg.m2.day.x.y-plotdata$prod2.newRMLg.m2.day.x.x
plotdata$dif2=plotdata$mort.newRMLg.m2.day.x.y-plotdata$mort.newRMLg.m2.day.x.x
plotdata$dif3=plotdata$stock.newRMLg.m2.x.y-plotdata$stock.newRMLg.m2.x.x


#Graph production

g=ggplot(plotdata,aes(x=date,y=prod2.newRMLg.m2.day.x.y-prod2.newRMLg.m2.day.x.x))+
  geom_point(size=2) +
  geom_line(size=0.5)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.Date("2017-09-21"),lty=2)+
  geom_hline(yintercept=0,lty=2)+
  labs(x="Date",y="Root Production diference control vs warming (g/m2/day)")+
  ylim(-7,30)+
  theme_classic()


#mort diff
plotdata <- merge(Agg2.new[Agg2.new$treatment=="Warming"&Agg2.new$rml=="new",c("date","prod2.newRMLg.m2.day.x","mort.newRMLg.m2.day.x", "stock.newRMLg.m2.x"),],
                  Agg2.new[Agg2.new$treatment=="Control"&Agg2.new$rml=="new",c("date","prod2.newRMLg.m2.day.x","mort.newRMLg.m2.day.x", "stock.newRMLg.m2.x") ],
                  by="date")
g=ggplot(plotdata,aes(x=date,y=mort.newRMLg.m2.day.x.y-mort.newRMLg.m2.day.x.x))+
  geom_point(size=2) +
  geom_line(size=0.5)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.Date("2017-09-21"),lty=2)+
  geom_hline(yintercept=0,lty=2)+
  labs(x="Date",y="Root mortality diference control vs warming (g/m2/day)")+
  ylim(-7,30)+
  theme_classic()


#stock diff
plotdata <- merge(Agg2.new[Agg2.new$treatment=="Warming"&Agg2.new$rml=="new",c("date","prod2.newRMLg.m2.day.x","mort.newRMLg.m2.day.x", "stock.newRMLg.m2.x"),],
                  Agg2.new[Agg2.new$treatment=="Control"&Agg2.new$rml=="new",c("date","prod2.newRMLg.m2.day.x","mort.newRMLg.m2.day.x", "stock.newRMLg.m2.x") ],
                  by="date")
g=ggplot(plotdata,aes(x=date,y=stock.newRMLg.m2.x.y-stock.newRMLg.m2.x.x))+
  geom_point(size=2) +
  geom_line(size=0.5)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.Date("2017-09-21"),lty=2)+
  geom_hline(yintercept=0,lty=2)+
  labs(x="Date",y="Root stock diference control vs warming (g/m2)")+
  theme_classic()



##################NOW graphs WITH SA########################################### 


totProd$treatment <- factor(totProd$treatment,levels=c("Warming","Control"))
totProd$date <- as.POSIXct(totProd$date)



SE <- function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))
Agg1 <- aggregate(data=totProd,cbind(prodSA.m2.m2.day,morSA.m2.m2.day,SA.m2.m2)~tube+plot+treatment+date,FUN=sum)
Agg1 <- aggregate(data=Agg1,cbind(prodSA.m2.m2.day,morSA.m2.m2.day,SA.m2.m2)~plot+treatment+date,FUN=mean)

Agg2 <- aggregate(data=Agg1,cbind(prodSA.m2.m2.day,morSA.m2.m2.day,SA.m2.m2)~treatment+date,FUN=mean,na.rm=T)
Agg2 <- merge(Agg2,aggregate(data=Agg1,cbind(prodSA.m2.m2.day,morSA.m2.m2.day,SA.m2.m2)~treatment+date,FUN=SE),
              by=c("treatment","date"))


######  prod
######       
g <- ggplot(Agg2,aes(x=date,y=prodSA.m2.m2.day.x,col=factor(treatment),shape=treatment))+
  geom_point(size=2) +
  geom_errorbar(aes(x=date,ymin=prodSA.m2.m2.day.x-prodSA.m2.m2.day.y,ymax=prodSA.m2.m2.day.x+prodSA.m2.m2.day.y),
                size=0.5,alpha=0.5)+
  geom_line(size=1)+
  # scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.POSIXct("2017-09-20"),lty=2)+
  labs(x="Date",y="Root SA Production (m2/m2/day)")+
  theme_classic()


#graph mortality with time #####################################################
g <- ggplot(Agg2,aes(x=date,y=morSA.m2.m2.day.x,col=factor(treatment),shape=treatment))+
  geom_point(size=2) +
  geom_errorbar(aes(x=date,ymin=morSA.m2.m2.day.x-morSA.m2.m2.day.y,ymax=morSA.m2.m2.day.x+morSA.m2.m2.day.y),
                size=0.5,alpha=0.5)+
  geom_line(size=1)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.POSIXct("2017-09-20"),lty=2)+
  labs(x="Date",y="Root SA Mortality (m2/m2/day)")+
  theme_classic()



##graph SA with time #######################################################
g <- ggplot(Agg2,aes(x=date,y=SA.m2.m2.x,col=factor(treatment),shape=treatment))+
  geom_point(size=2) +
  geom_errorbar(aes(x=date,ymin=SA.m2.m2.x-SA.m2.m2.y,ymax=SA.m2.m2.x+SA.m2.m2.y),
                size=0.5,alpha=0.5)+
  geom_line(size=1)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.POSIXct("2017-09-21"),lty=2)+
  labs(x="Date",y="Root SA (m2/m2)")+
  theme_classic()

#----------------------------------------------------------------

#difference warming - control
#prod
plotdata <- merge(Agg2[Agg2$treatment=="Warming",c("date","prodSA.m2.m2.day.x","morSA.m2.m2.day.x", "SA.m2.m2.x"),],
                  Agg2[Agg2$treatment=="Control",c("date","prodSA.m2.m2.day.x","morSA.m2.m2.day.x", "SA.m2.m2.x") ],
                  by="date")
g=ggplot(plotdata,aes(x=date,y=prodSA.m2.m2.day.x.y-prodSA.m2.m2.day.x.x))+
  geom_point(size=2) +
  geom_line(size=0.5)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.Date("2017-09-21"),lty=2)+
  geom_hline(yintercept=0,lty=2)+
  labs(x="Date",y="Root Surface area production diference control vs warming (m2/m2/day)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx')
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4) 
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/SA Production diffference.pptx")  #Save the file wherever you want

#mort diff

g=ggplot(plotdata,aes(x=date,y=morSA.m2.m2.day.x.y-morSA.m2.m2.day.x.x))+
  geom_point(size=2) +
  geom_line(size=0.5)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.Date("2017-09-21"),lty=2)+
  geom_hline(yintercept=0,lty=2)+
  labs(x="Date",y="Root surface area mortality diference control vs warming (m2/m2/day)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx')
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4) 
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/SA mortality difference.pptx")  #Save the file wherever you want


#stock diff

g=ggplot(plotdata,aes(x=date,y=SA.m2.m2.x.y-SA.m2.m2.x.x))+
  geom_point(size=2) +
  geom_line(size=0.5)+
  #scale_y_log10()+
  # scale_color_manual(values=rev(cols))+
  geom_vline(xintercept=as.Date("2017-09-21"),lty=2)+
  geom_hline(yintercept=0,lty=2)+
  labs(x="Date",y="Root surface area diference control vs warming (m2/m2)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx')
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4) 
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/SA difference.pptx")  #Save the file wherever you want




############################################  DEPTH GRAPHS###############################################


totProd=read.csv("totProdfinal2Dec9.2019.16.12.20.csv")
totProd$date=as.character(totProd$date)
totProd$date=as.Date(totProd$date)


######    now with depth
SE <- function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))
#totProd.win$Depth10 <- round(totProd.win$depth/10)*10


DepthAgg <- aggregate(data=totProd,cbind(prod2.g.m2.day,mort.g.m2.day,stock.g.m2,
                                         prod2.newRMLg.m2.day,mort.newRMLg.m2.day,stock.newRMLg.m2)~
                        tube+treatment+hurricane+depthbin.cm.+session+diam2,FUN=sum)
DepthAgg <- DepthAgg[,c(names(DepthAgg)[-which(names(DepthAgg)=="diam2")],"diam2")]
DepthAgg <- rbind(DepthAgg,data.frame(cbind(aggregate(data=totProd,cbind(prod2.g.m2.day,mort.g.m2.day,stock.g.m2,
                                                                         prod2.newRMLg.m2.day,mort.newRMLg.m2.day,stock.newRMLg.m2)~
                                                        tube+treatment+hurricane+depthbin.cm.+session,FUN=sum),diam2="all")))

DepthAgg2 <- merge(aggregate(data=DepthAgg,cbind(prod2.g.m2.day,mort.g.m2.day,stock.g.m2,
                                                 stock.newRMLg.m2,prod2.newRMLg.m2.day,mort.newRMLg.m2.day)~
                               treatment+hurricane+depthbin.cm.+diam2,FUN=mean),
                   aggregate(data=DepthAgg,cbind(prod2.g.m2.day,mort.g.m2.day,stock.g.m2,
                                                 prod2.newRMLg.m2.day,mort.newRMLg.m2.day,stock.newRMLg.m2)~
                               treatment+depthbin.cm.+hurricane+diam2,FUN=SE),by=c("treatment","depthbin.cm.","hurricane","diam2"))


DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="0-10"]=5
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="10-20"]=15
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="20-30"]=25
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="30-40"]=35
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="40-50"]=45
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="50-60"]=55
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="60-70"]=65
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="70-80"]=75
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="70-80"]=85
DepthAgg2$depthbin.cm.2[DepthAgg2$depthbin.cm.=="70-80"]=95


#change colors by treatment
DepthAgg2$treatment <- factor(DepthAgg2$treatment,levels=c("Warming","Control"))

##Before

write.csv(DepthAgg2,"DepthAgg2.16.12.20.csv")




#production
g=ggplot(DepthAgg2[DepthAgg2$hurricane=="before"&DepthAgg2$diam2=="all",],aes(x=depthbin.cm.2,y=prod2.newRMLg.m2.day.x,col=factor(treatment)))+
  #reverse depth so it starts at zero
  scale_x_reverse()+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  coord_flip()+
  geom_errorbar(aes(x=depthbin.cm.2,ymin=prod2.newRMLg.m2.day.x-prod2.newRMLg.m2.day.y,ymax=prod2.newRMLg.m2.day.x+prod2.newRMLg.m2.day.y),
                size=1,alpha=0.7,width=4)+
  geom_point() +
  #scale_color_manual(values=rev(cols))+
  geom_line(size=1)+#aes(size=diam2))+
  #scale_size_manual(values=c(3,2,1))+
  labs(x="Depth (cm)",y="Root production (g m-2 day)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx') ## empty powerpoint slide that is pre saved with the dimensions you want. I use 8" x 11", which is a standard document size for publishing
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")# This just adds a slide to the document, not shure why
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4)  ### this adds the plot to the document, in this case i created a plot called "g_grid", but this will depend on your specific plot. I set the size to be as big as the page (8 x 11)
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/prod2 new RML by depth before2 all diam.pptx")  #Save the file wherever you want


##mortality

g=ggplot(DepthAgg2[DepthAgg2$hurricane=="before"&DepthAgg2$diam2=="all",],aes(x=depthbin.cm.2,y=mort.newRMLg.m2.day.x,col=factor(treatment)))+
  scale_x_reverse()+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  coord_flip()+
  geom_errorbar(aes(x=depthbin.cm.2,ymin=mort.newRMLg.m2.day.x-mort.newRMLg.m2.day.y,ymax=mort.newRMLg.m2.day.x+mort.newRMLg.m2.day.y),
                size=1,alpha=0.7,width=4)+
  geom_point() +
  #scale_color_manual(values=rev(cols))+
  geom_line(size=1)+#aes(size=diam2))+
  #scale_size_manual(values=c(3,2,1))+
  labs(x="Depth (cm)",y="Root mortality (g m-2 day)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx') ## empty powerpoint slide that is pre saved with the dimensions you want. I use 8" x 11", which is a standard document size for publishing
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")# This just adds a slide to the document, not shure why
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4)  ### this adds the plot to the document, in this case i created a plot called "g_grid", but this will depend on your specific plot. I set the size to be as big as the page (8 x 11)
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/mort new RML by depth before all diam.pptx")  #Save the file wherever you want


#Stock
g=ggplot(DepthAgg2[DepthAgg2$hurricane=="before"&DepthAgg2$diam2=="all",],aes(x=depthbin.cm.2,y=stock.newRMLg.m2.x,col=factor(treatment)))+
  scale_x_reverse()+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  coord_flip()+
  geom_errorbar(aes(x=depthbin.cm.2,ymin=stock.newRMLg.m2.x-stock.newRMLg.m2.y,ymax=stock.newRMLg.m2.x+stock.newRMLg.m2.y),
                size=1,alpha=0.7,width=4)+
  geom_point() +
  # scale_color_manual(values=rev(cols))+
  geom_line(size=1)+#aes(size=diam2))+
  #scale_size_manual(values=c(3,2,1))+
  labs(x="Depth (cm)",y="Root Production (g m-2)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx') ## empty powerpoint slide that is pre saved with the dimensions you want. I use 8" x 11", which is a standard document size for publishing
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")# This just adds a slide to the document, not shure why
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4)  ### this adds the plot to the document, in this case i created a plot called "g_grid", but this will depend on your specific plot. I set the size to be as big as the page (8 x 11)
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/stock new RML by depth before all diam.pptx")  #Save the file wherever you want


#AFTER

#Production
g=ggplot(DepthAgg2[DepthAgg2$hurricane=="after"&DepthAgg2$diam2=="all",],aes(x=depthbin.cm.2,y=prod2.newRMLg.m2.day.x,col=factor(treatment)))+
  scale_x_reverse()+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  coord_flip()+
  geom_errorbar(aes(x=depthbin.cm.2,ymin=prod2.newRMLg.m2.day.x-prod2.newRMLg.m2.day.y,ymax=prod2.newRMLg.m2.day.x+prod2.newRMLg.m2.day.y),
                size=1,alpha=0.7,width=4)+
  geom_point() +
  # scale_color_manual(values=rev(cols))+
  geom_line(size=1)+#aes(size=diam2))+
  #scale_size_manual(values=c(3,2,1))+
  labs(x="Depth (cm)",y="Root Production (g m-2 day)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx') ## empty powerpoint slide that is pre saved with the dimensions you want. I use 8" x 11", which is a standard document size for publishing
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")# This just adds a slide to the document, not shure why
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4)  ### this adds the plot to the document, in this case i created a plot called "g_grid", but this will depend on your specific plot. I set the size to be as big as the page (8 x 11)
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/prod2 new RML by depth after all diam.pptx")  #Save the file wherever you want


##mortality

g=ggplot(DepthAgg2[DepthAgg2$hurricane=="after"&DepthAgg2$diam2=="all",],aes(x=depthbin.cm.2,y=mort.newRMLg.m2.day.x,col=factor(treatment)))+
  scale_x_reverse()+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  coord_flip()+
  geom_errorbar(aes(x=depthbin.cm.2,ymin=mort.newRMLg.m2.day.x-mort.newRMLg.m2.day.y,ymax=mort.newRMLg.m2.day.x+mort.newRMLg.m2.day.y),
                size=1,alpha=0.7,width=4)+
  geom_point() +
  # scale_color_manual(values=rev(cols))+
  geom_line(size=1)+#aes(size=diam2))+
  #scale_size_manual(values=c(3,2,1))+
  labs(x="Depth (cm)",y="Root mortality (g m-2 day)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx') ## empty powerpoint slide that is pre saved with the dimensions you want. I use 8" x 11", which is a standard document size for publishing
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")# This just adds a slide to the document, not shure why
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4)  ### this adds the plot to the document, in this case i created a plot called "g_grid", but this will depend on your specific plot. I set the size to be as big as the page (8 x 11)
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/mort new RML by depth after all diam.pptx")  #Save the file wherever you want


#Stock
g=ggplot(DepthAgg2[DepthAgg2$hurricane=="after"&DepthAgg2$diam2=="all",],aes(x=depthbin.cm.2,y=stock.newRMLg.m2.x,col=factor(treatment)))+
  scale_x_reverse()+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  coord_flip()+
  geom_errorbar(aes(x=depthbin.cm.2,ymin=stock.newRMLg.m2.x-stock.newRMLg.m2.y,ymax=stock.newRMLg.m2.x+stock.newRMLg.m2.y),
                size=1,alpha=0.7,width=4)+
  geom_point() +
  # scale_color_manual(values=rev(cols))+
  geom_line(size=1)+#aes(size=diam2))+
  #scale_size_manual(values=c(3,2,1))+
  labs(x="Depth (cm)",y="Root Production (g m-2)")+
  theme_classic()

doc <- read_pptx(path='C:/Users/17875/Documents/template.pptx') ## empty powerpoint slide that is pre saved with the dimensions you want. I use 8" x 11", which is a standard document size for publishing
doc <- add_slide(doc,layout = "Title and Content",master="Office Theme")# This just adds a slide to the document, not shure why
doc <- ph_with_vg(doc, code=print(g), width = 4, height = 4)  ### this adds the plot to the document, in this case i created a plot called "g_grid", but this will depend on your specific plot. I set the size to be as big as the page (8 x 11)
print(doc, target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/stock new RML by depth after all diam.pptx")  #Save the file wherever you want




#BETA DISTRIBUTION########################################################################

######Cummulative biomass beta
cum=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/cummulative beta.csv")
###Y(cumulative percent) = 1- Beta d(depth)
library(minqa)
min.rss <- function(beta){
  x = dat$Percent/100
  y = 1-beta^dat$Depth.for.beta
  sum((x-y)^2,na.rm=T)
}
dat=cum[cum$treatment=="Warming"&cum$hurricane=="before",]
beta.before.w <- bobyqa(0.9,min.rss,0.8,1)$par
dat=cum[cum$treatment=="Control"&cum$hurricane=="before",]
beta.before.c <- bobyqa(0.9,min.rss,0.8,1)$par
dat=cum[cum$treatment=="Warming"&cum$hurricane=="after",]
beta.after.w <- bobyqa(0.9,min.rss,0.8,1)$par
dat=cum[cum$treatment=="Control"&cum$hurricane=="after",]
beta.after.c <- bobyqa(0.9,min.rss,0.8,1)$par



#by treatment and hurricane 
cum$treatment=factor(cum$treatment,levels=c("Warming","Control"))
linedata = rbind(data.frame(Depth.for.beta=c(0:100),pred=100*(1-beta.before.c^c(0:100)),treatment="Control",hurricane="before"),
                 data=data.frame(Depth.for.beta=c(0:100),pred=100*(1-beta.after.c^c(0:100)),treatment="Control",hurricane="after"),
                 data=data.frame(Depth.for.beta=c(0:100),pred=100*(1-beta.before.w^c(0:100)),treatment="Warming",hurricane="before"),
                 data=data.frame(Depth.for.beta=c(0:100),pred=100*(1-beta.after.w^c(0:100)),treatment="Warming",hurricane="after"))
linedata$treatment=factor(linedata$treatment,levels=c("Warming","Control"))

g=ggplot(data=cum,aes(x=Depth.for.beta,y=Percent,col=?..treatment,shape=hurricane,lty=hurricane))+
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
  scale_x_continuous(trans = "reverse",breaks=c(10,20,30,40,50,60,70,80,90,100))+
  scale_y_continuous(position="top")+
  geom_point()+
  geom_line(data=linedata,aes(x=Depth.for.beta,y=pred,lty=hurricane)) +
  labs(x="Depth",y="Root biomass (Comulative percent)")+
  theme_classic()


read_pptx(path='C:/Users/17875/Documents/template.pptx') %>%
  add_slide() %>%
  ph_with(dml(ggobj=g), location=ph_location(width = 4, height = 4)) %>%
  print(target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/cummulative beta distribution by treatment and hurricane.pptx")  #Save the file wherever you want


#for overall
dat=cum
beta.all <- bobyqa(0.9,min.rss,0.8,1)$par

# overall
g=ggplot(data=cum,aes(x=Depth.for.beta,y=Percent))+
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
  scale_x_continuous(trans = "reverse",breaks=c(10,20,30,40,50,60,70,80,90,100))+
  scale_y_continuous(position="top")+
  geom_line(data=data.frame(Depth.for.beta=c(0:100),pred=100*(1-beta.all^c(0:100)),?..treatment="Warming",hurricane="before"),aes(x=Depth.for.beta,y=pred)) +
  geom_point(aes())+
  labs(x="Depth",y="Root biomass (Comulative percent)")+
  theme_classic()


read_pptx(path='C:/Users/17875/Documents/template.pptx') %>%
  add_slide() %>%
  ph_with(dml(ggobj=g), location=ph_location(width = 4, height = 4)) %>%
  print(target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/cummulative beta distribution overall.pptx")  #Save the file wherever you want



#######################################################################################################################

###MORPHOLOGY FROM INGROWTH CORES and soil cores

mor=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/morphology from ingrowth and core.csv")


## most roots scanned are finer than 1 mm in diameter

####only ingrowth core morphology

ingonly=mor[mor$Method=="Ingrowth",]


#aggregate rem and res

agg_ing1=aggregate(data=ingonly,cbind(Length.cm,dryroots.g)~Core+plot+treatment+Year+date,FUN=sum,na.rm=T)

agg_ing2=aggregate(data=ingonly,Ave..Diam.mm~Core+plot+treatment+Year+date,FUN=mean,na.rm=T)

#merge
agg_ing1$Core=as.numeric(agg_ing1$Core)
agg_ing1$plot=as.numeric(agg_ing1$plot)
agg_ing2$Core=as.numeric(agg_ing2$Core)
agg_ing2$plot=as.numeric(agg_ing2$plot)

agg_ing=merge(agg_ing1,agg_ing2,by=c("Core","plot","treatment","Year","date"),all.y=TRUE)
agg_ing$day=1
colnames(agg_ing)=c("Core","plot", "treatment", "Year","month","length.cm","rootmass.g","diam.mm","day")

#creating a date column

agg_ing$date=as.Date(with(agg_ing,paste(Year,month,day,sep="-")),"%Y-%b-%d")

boxplot(agg_ing$length.cm~agg_ing$date)

##by plot
plot_agg=aggregate(data=agg_ing,cbind(length.cm,rootmass.g,diam.mm)~plot+treatment+date,FUN=mean,na.rm=T)

##length
#in meter
plot_agg$length.m=plot_agg$length.cm/100

boxplot(plot_agg$length.m~plot_agg$date,ylab="Fine-root length (m)",
        col=rep(c("lightgreen","darkolivegreen","burlywood4","darkolivegreen3")))


read_pptx(path='C:/Users/17875/Documents/template.pptx') %>%
  add_slide() %>%
  ph_with(dml(ggobj=g), location=ph_location(width = 4, height = 4)) %>%
  print(target = "C:/Users/17875/Desktop/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/scanned length ingrowth core by date.pptx")  #Save the file wherever you want


#SRL (specific root length)
plot_agg$srl=plot_agg$length.m/plot_agg$rootmass.g

boxplot(plot_agg$srl~plot_agg$date,ylab="Specific fine-root length (m)",
        col=rep(c("lightgreen","darkolivegreen","burlywood4","darkolivegreen3")))


read_pptx(path='C:/Users/17875/Documents/template.pptx') %>%
  add_slide() %>%
  ph_with(dml(ggobj=g), location=ph_location(width = 4, height = 4)) %>%
  print(target = "C:/Users/17875/Desktop/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/scanned ingrowth core srl by date.pptx")  #Save the file wherever you want


#Diameter

boxplot(plot_agg$diam.mm~plot_agg$date,ylab="Fine-root diameter (mm)",
        col=rep(c("lightgreen","darkolivegreen","burlywood4","darkolivegreen3")))


read_pptx(path='C:/Users/17875/Documents/template.pptx') %>%
  add_slide() %>%
  ph_with(dml(ggobj=g), location=ph_location(width = 4, height = 4)) %>%
  print(target= "C:/Users/17875/Desktop/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/scanned diameter ingrowth core by date.pptx")  #Save the file wherever you want



#####################PREPARING DATA FOR MIXED MODEL############################################################

##########################################
#####  Start here once datasets are made
############################################

llbydate=read.csv("allbydateWithSAandNEWRML2.16.12.20.csv")
session_lengths <- aggregate(allbydate$duration.d.~allbydate$session,FUN=mean)
names(session_lengths) <- c("session","dur")


totProd=read.csv("totProdfinal2Dec9.2019.16.12.20.csv")


Tubes <- read.csv("Tubes.csv")
#make all columns numeric
totProd[,c("session","prodbybin","mortbybin","prod.mg.cm3","mort.mg.cm3","prod.mg.cm2","mort.mg.cm2","stock.mg.cm2","stock.mg.cm3")] <- sapply(totProd[,c("session","prodbybin","mortbybin","prod.mg.cm3","mort.mg.cm3","prod.mg.cm2","mort.mg.cm2","stock.mg.cm2","stock.mg.cm3")],as.numeric)
totProd$date=as.character(totProd$date)
totProd$date=as.Date(totProd$date)


#by depth bin every 10 cm and adding all diam class using SA, new RML prod and length
totProdSAS=aggregate(data=totProd,cbind(len.mm.cm2,mortLength.mm.cm2.day,dlen.mm.cm2.day,
                                        SA.m2.m2,morSA.m2.m2.day,prodSA.m2.m2.day,
                                        stock.newRMLg.m2,prod2.newRMLg.m2.day,mort.newRMLg.m2.day)~
                       treatment+tube+hurricane+session+depthbin.cm.+date+plot,FUN=sum,na.rm=T)



totProdSAS$dlen.mm.cm2.day[totProdSAS$dlen.mm.cm2.day<0]=0
totProdSAS$mortLength.mm.cm2.day[totProdSAS$mortLength.mm.cm2.day<0]=0
totProdSAS$prodSA.m2.m2.day[totProdSAS$prodSA.m2.m2.day<0]=0
totProdSAS$morSA.m2.m2.day[totProdSAS$morSA.m2.m2.day<0]=0
#totProdSAS$prod.newRMLg.m2.day[totProdSAS$prod.newRMLg.m2.day<0]=0
totProdSAS$prod2.newRMLg.m2.day[totProdSAS$prod2.newRMLg.m2.day<0]=0
totProdSAS$mort.newRMLg.m2.day[totProdSAS$mort.newRMLg.m2.day<0]=0
#totProdSAS$prodbystock.mg.cm2.day[totProdSAS$prodbystock.mg.cm2.day<0]=0


### by tube aggregate
totProdSAS2=aggregate(data=totProdSAS,cbind(dlen.mm.cm2.day,mortLength.mm.cm2.day,len.mm.cm2,
                                            prodSA.m2.m2.day,morSA.m2.m2.day,SA.m2.m2,
                                            stock.newRMLg.m2,prod2.newRMLg.m2.day,mort.newRMLg.m2.day)~
                        treatment+tube+hurricane+session+date+plot,FUN=sum,na.rm=T)



#TO ADD A LOG, THE DATA CANNOT BE ZERO, SO WE ADDED 0.0001 TO ALL DATA
totProdSAS2$lndlen.mm.cm2.day=log(totProdSAS2$dlen.mm.cm2.day+0.0001)
totProdSAS2$lnlen.mm.cm2=log(totProdSAS2$len.mm.cm2+0.0001)
totProdSAS2$lnmortLength.mm.cm2.day=log(totProdSAS2$mortLength.mm.cm2.day+0.0001)


totProdSAS2$lnprodSA.m2.m2.day=log(totProdSAS2$prodSA.m2.m2.day+0.0001)
totProdSAS2$lnSA.m2.m2=log(totProdSAS2$SA.m2.m2+0.0001)
totProdSAS2$lnmorSA.m2.m2.day=log(totProdSAS2$morSA.m2.m2.day+0.0001)


totProdSAS2$lnprod2.newRMLg.m2.day=log(totProdSAS2$prod2.newRMLg.m2.day+0.0001)
totProdSAS2$lnstock.newRMLg.m2=log(totProdSAS2$stock.newRMLg.m2+0.0001)
totProdSAS2$lnmort.newRMLg.m2.day=log(totProdSAS2$mort.newRMLg.m2.day+0.0001)


##totprodSAS3dec is by plot 
totProdSAS3dec=aggregate(data=totProdSAS2,cbind(dlen.mm.cm2.day,mortLength.mm.cm2.day,len.mm.cm2,prodSA.m2.m2.day,
                                                morSA.m2.m2.day,SA.m2.m2,stock.newRMLg.m2,prod2.newRMLg.m2.day,mort.newRMLg.m2.day,
                                                lndlen.mm.cm2.day,lnmortLength.mm.cm2.day,lnlen.mm.cm2,lnprodSA.m2.m2.day,
                                                lnmorSA.m2.m2.day,lnSA.m2.m2,lnstock.newRMLg.m2,lnprod2.newRMLg.m2.day,lnmort.newRMLg.m2.day)~
                           treatment+hurricane+session+date+plot,FUN=mean,na.rm=T)



write.csv(totProdSAS3dec,"dataset by length and SA and new RML for SAS by plot dec2019.16.12.20.csv")
write.csv(totProdSAS3dec[totProdSAS3dec$hurricane=="after",],"length and SA and new RML for SAS dec by plot after.16.12.20.csv")
write.csv(totProdSAS3dec[totProdSAS3dec$hurricane=="before",],"length and SA and new RML for SAS dec by plot before.16.12.20.csv")



###################################Add soil mositure and temperature####################################

moistall=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/moistall 2021.csv")
moistall$date=as.Date(moistall$date)
moistall$plot=as.numeric(moistall$plot)
##################################################################################
########################################################################################################

###aggregate with prod by length 
prodlenTM=read.csv("dataset by length and SA and new RML for SAS by plot dec2019.16.12.20.csv")
prodlenTM$date=as.character(prodlenTM$date)
prodlenTM$date=as.Date(prodlenTM$date)

moistSAS=aggregate(data=moistall[moistall$variable2=="moisture",],value~date+treatment+plot, FUN=mean,na.rm=T)
tempSAS=aggregate(data=moistall[moistall$variable2=="temperature",],value~date+treatment+plot, FUN=mean,na.rm=T)


moistSAS$treatment=as.character(moistSAS$treatment)
tempSAS$treatment=as.character(tempSAS$treatment)

moistSAS$treatment[moistSAS$treatment=="C"]="Control"
moistSAS$treatment[moistSAS$treatment=="W"]="Warming"

tempSAS$treatment[tempSAS$treatment=="C"]="Control"
tempSAS$treatment[tempSAS$treatment=="W"]="Warming"

prodbylengthcorrM=merge(moistSAS,prodlenTM,by=c("date","plot","treatment"),all.y=TRUE)
prodbylengthcorrT=merge(tempSAS,prodlenTM,by=c("date","plot","treatment"),all.y=TRUE)

prodlengthMT=merge(prodbylengthcorrM,prodbylengthcorrT,by=c("date","plot","treatment"),all.y=TRUE)

prodlengthMT=prodlengthMT[c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]

colnames(prodlengthMT)=c("date","plot","treatment","moisture","hurricane","session","dlen.mm.cm2.day",
                         "mortLength.mm.cm2.day","len.mm.cm2","prodSA.m2.m2.day","morSA.m2.m2.day",
                         "SA.m2.m2","stock.newRMLg.m2","prod2.newRMLg.m2.day","mort.newRMLg.m2.day",
                         "lndlen.mm.cm2.day","lnmortLength.mm.cm2.day","lnlen.mm.cm2","lnprodSA.m2.m2.day","lnmorSA.m2.m2.day",
                         "lnSA.m2.m2","lnstock.newRMLg.m2","lnprod2.newRMLg.m2.day","lnmort.newRMLg.m2.day","temperature")



write.csv(prodlengthMT,"prodlengthMTsasDec9.16.12.20.csv")


write.csv(prodlengthMT[prodlengthMT$hurricane=="before",],"prodlengthMTBsasDec9.16.12.20.csv")
write.csv(prodlengthMT[prodlengthMT$hurricane=="after",],"prodlengthMTAsasDec9.16.12.20.csv")

##################################################################################
########################################################################################################

###aggregate with prod by length 
prodlenTM=read.csv("dataset by length and SA and new RML for SAS by plot dec2019.16.12.20.csv")
prodlenTM$date=as.character(prodlenTM$date)
prodlenTM$date=as.Date(prodlenTM$date)

moistSAS=aggregate(data=moistall[moistall$variable2=="moisture",],value~date+treatment+plot, FUN=mean,na.rm=T)
tempSAS=aggregate(data=moistall[moistall$variable2=="temperature",],value~date+treatment+plot, FUN=mean,na.rm=T)


moistSAS$treatment=as.character(moistSAS$treatment)
tempSAS$treatment=as.character(tempSAS$treatment)

moistSAS$treatment[moistSAS$treatment=="C"]="Control"
moistSAS$treatment[moistSAS$treatment=="W"]="Warming"

tempSAS$treatment[tempSAS$treatment=="C"]="Control"
tempSAS$treatment[tempSAS$treatment=="W"]="Warming"

prodbylengthcorrM=merge(moistSAS,prodlenTM,by=c("date","plot","treatment"),all.y=TRUE)
prodbylengthcorrT=merge(tempSAS,prodlenTM,by=c("date","plot","treatment"),all.y=TRUE)

prodlengthMT=merge(prodbylengthcorrM,prodbylengthcorrT,by=c("date","plot","treatment"),all.y=TRUE)

prodlengthMT=prodlengthMT[c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]

colnames(prodlengthMT)=c("date","plot","treatment","moisture","hurricane","session","dlen.mm.cm2.day",
                         "mortLength.mm.cm2.day","len.mm.cm2","prodSA.m2.m2.day","morSA.m2.m2.day",
                         "SA.m2.m2","stock.newRMLg.m2","prod2.newRMLg.m2.day","mort.newRMLg.m2.day",
                         "lndlen.mm.cm2.day","lnmortLength.mm.cm2.day","lnlen.mm.cm2","lnprodSA.m2.m2.day","lnmorSA.m2.m2.day",
                         "lnSA.m2.m2","lnstock.newRMLg.m2","lnprod2.newRMLg.m2.day","lnmort.newRMLg.m2.day","temperature")



write.csv(prodlengthMT,"prodlengthMTsasDec9.16.12.20.csv")


write.csv(prodlengthMT[prodlengthMT$hurricane=="before",],"prodlengthMTBsasDec9.16.12.20.csv")
write.csv(prodlengthMT[prodlengthMT$hurricane=="after",],"prodlengthMTAsasDec9.16.12.20.csv")






###########################HERARCHICAL LINERAL MODEL ____________________________________________----

#This dataset is the same as prodlengthMTAsasDec9.16.12.20 but plus nutrients

forSAS_mergedb=read.csv("prodlengthMTBsasDec9.16.12.20.csv")
forSAS_mergeda=read.csv("prodlengthMTAsasDec9.16.12.20.csv")
forSAS_mergeda$newsession=forSAS_mergeda$session-14


#forSAS_mergedb=read.csv("all factors before plus nutrient for SAS April 2020.csv")
#forSAS_mergeda=read.csv("all factors after plus nutrient for SAS April 2020new.csv")

#coarse
Agg1.coarsea=read.csv("coarse roots for mixed model.csv")


prodlengthMT=read.csv("prodlengthMTAsasDec9.16.12.20.csv")
prodlengthMT2=prodlengthMT[,c(2,3,4,5,7,26)]

coarsemodelA=merge(prodlengthMT2,Agg1.coarsea,by=c("date","plot","treatment"),all=TRUE)

coarsemodelA=coarsemodelA[,-c(7,8,9,13)]


#packages

library(lme4)
library(lmerTest)
library(nlme); library(dplyr)
library(emmeans)
library(officer)
library(rvg)
library(qqplotr)
library(performance)

# Allow the model to run despite small rep:

CustomControls<-lmerControl(optimizer = "nloptwrap",
                            restart_edge = TRUE,
                            boundary.tol = 1e-5,
                            calc.derivs=TRUE,
                            use.last.params=TRUE,
                            sparseX = FALSE,
                            
                            ## input checking options
                            check.nobs.vs.rankZ = "ignore",
                            check.nobs.vs.nlev = "ignore",
                            check.nlev.gtreq.5 = "ignore",
                            check.nlev.gtr.1 = "stop",
                            check.nobs.vs.nRE="ignore",
                            check.rankX = c("message+drop.cols", "silent.drop.cols", "warn+drop.cols",
                                            "stop.deficient", "ignore"),
                            check.scaleX = c("warning","stop","silent.rescale",
                                             "message+rescale","warn+rescale","ignore"),
                            check.formula.LHS = "stop",
                            ## convergence checking options
                            check.conv.grad     = .makeCC("ignore", tol = 1E-3, relTol = NULL),
                            check.conv.singular = .makeCC(action = "warning",  tol = 1e-12),
                            check.conv.hess     = .makeCC(action = "warning", tol = 1e-8),
                            ## optimizer args
                            optCtrl = list(iprint=0, rhoend=1e-8))



#now only for before
forSAS_mergedb$plot<-as.factor(forSAS_mergedb$plot)
forSAS_mergedb$treatment<-as.factor(forSAS_mergedb$treatment)
forSAS_mergedb$session<-as.numeric(forSAS_mergedb$session)


####################### S T O C K ######################################################

###CREATING MODELS

##treatment and session interaction

simple0=lmer(stock.newRMLg.m2 ~treatment+session+treatment:session+temperature + moisture+(1|plot)
             , data=forSAS_mergedb, control = CustomControls,REML=T)
check_collinearity(simple0)
# #collinearity of temperature

#remove temperature

simple=lmer(stock.newRMLg.m2 ~treatment+session+treatment:session+moisture+(1|plot)
            , data=forSAS_mergedb, control = CustomControls,REML=T)
check_collinearity(simple)
shapiro.test(residuals(simple))
summary(simple)

#Remove moisture
simple5<-lmer(stock.newRMLg.m2 ~treatment+session+treatment:session+
                (1|plot),data=forSAS_mergedb, control = CustomControls)

summary(simple5)
check_collinearity(simple5)
shapiro.test(residuals(simple5))

#compare both models
anova(simple5,simple)

##best model (simple5)

#Graphs for each plot
forSAS_mergedb$preds1<-predict(simple5)

forSAS_mergedb$TrtPlot<-paste(forSAS_mergedb$treatment,forSAS_mergedb$plot)


forSAS_mergedb %>%
  
  ggplot(aes(x=session, y=preds1, group=TrtPlot, color=TrtPlot)) +
  
  geom_point() + geom_line() +theme_bw()


#model adequacy: check model assumptions 
hist(residuals(simple5), col="darkgray")
plot(fitted(simple5),residuals(simple5)) 

#model fit: coefficient of determination of the model
r.squaredGLMM(simple5) 



########################### P R O D U C T I O N ################################

## INTERACTION WITH SESSION AND TREATMENT

simplep0<-lmer(lnprod2.newRMLg.m2.day ~treatment+session+treatment:session+temperature+moisture+
                 (1|plot),data=forSAS_mergedb, control = CustomControls)


summary(simplep0)
check_collinearity(simplep0)
#collinearity between temperature and session

#Remove temperature
simplep<-lmer(lnprod2.newRMLg.m2.day ~treatment+session+treatment:session+moisture+
                (1|plot),data=forSAS_mergedb, control = CustomControls,REML=T)


summary(simplep)
check_collinearity(simplep)
shapiro.test(residuals(simplep))

# NORMALITY TEST

df<-data.frame(resid=resid(simplep))

ggplot(data = df, mapping = aes(sample = resid)) +
  
  stat_qq_line(identity=TRUE, na.rm=TRUE) +
  
  stat_qq_point(col="black") +
  
  stat_qq_band(bandType = "boot", conf=0.95, mapping =aes(fill = "Boot CI bands"), alpha = 0.5, qtype=3) +
  
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()


simplep5<-lmer(lnprod2.newRMLg.m2.day ~treatment+session+treatment:session+
                 (1|plot),data=forSAS_mergedb, control = CustomControls,REML=T)


summary(simplep5)
check_collinearity(simplep5)
shapiro.test(residuals(simplep5))

# EXTRA NORMALITY TEST

df<-data.frame(resid=resid(simplep5))

ggplot(data = df, mapping = aes(sample = resid)) +
  
  stat_qq_line(identity=TRUE, na.rm=TRUE) +
  
  stat_qq_point(col="black") +
  
  stat_qq_band(bandType = "boot", conf=0.95, mapping =aes(fill = "Boot CI bands"), alpha = 0.5, qtype=3) +
  
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()



anova(simplep5,simplep)

#Best model (simplep5)


#model adequacy: check model assumptions 
hist(residuals(simplep5), col="darkgray")
plot(fitted(simplep5),residuals(simplep5)) 

#model fit: coefficient of determination of the model
r.squaredGLMM(simplep5) 


########################### M O R T A L I T Y ################################

#INTERACTION TREATMENT SESSION


simplem0<-lmer(lnmort.newRMLg.m2.day ~treatment+session+treatment:session+moisture+temperature+
                 (1|plot),data=forSAS_mergedb, control = CustomControls)


summary(simplem0)
check_collinearity(simplem0)
shapiro.test(residuals(simplem0))
#collinearity temp and session

#Remove temperature
simplem<-lmer(lnmort.newRMLg.m2.day ~treatment+session+treatment:session+moisture+
                (1|plot),data=forSAS_mergedb, control = CustomControls,REML=T)


summary(simplem)
check_collinearity(simplem)
shapiro.test(residuals(simplem))

#Remove temperature and moisture
simplem5<-lmer(lnmort.newRMLg.m2.day ~treatment+session+treatment:session+
                 (1|plot),data=forSAS_mergedb, control = CustomControls,REML=T)


summary(simplem5)
check_collinearity(simplem5)
shapiro.test(residuals(simplem5))

#Test best model
anova(simplem5,simplem)

#Best model (simplem5)


#model adequacy: check model assumptions 
hist(residuals(simplem5), col="darkgray")
plot(fitted(simplem5),residuals(simplem5)) 

#model fit: coefficient of determination of the model
r.squaredGLMM(simplem5) 
#r2m


#-------------------------------------------------------------------------------
##after HURRICANES
#-----------------------------------------------------------------------------------


forSAS_mergeda$plot<-as.factor(forSAS_mergeda$plot)
forSAS_mergeda$treatment<-as.factor(forSAS_mergeda$treatment)
forSAS_mergeda$newsession<-as.numeric(forSAS_mergeda$newsession)


#coarse roots only
coarsemodelA$plot <- as.factor(coarsemodelA$plot)
coarsemodelA$treatment <- as.factor(coarsemodelA$treatment)
coarsemodelA$session <- as.numeric(coarsemodelA$session)


#################################### S T O C K ##############################################
######################################################################################

#With interaction session and treatment


simpleA0<-lmer(stock.newRMLg.m2 ~temperature+moisture+newsession:treatment+newsession+treatment+
                 (1|plot),data=forSAS_mergeda, control = CustomControls,REML=T)


summary(simpleA0)
check_collinearity(simpleA0)
shapiro.test(residuals(simpleA0))

#Remove moisture
simpleA6<-lmer(stock.newRMLg.m2 ~newsession:treatment+temperature+treatment+newsession+
                 (1|plot),data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleA6)
check_collinearity(simpleA6)
shapiro.test(residuals(simpleA6))

#Remove temperature
simpleA5<-lmer(stock.newRMLg.m2 ~moisture+newsession:treatment+newsession+treatment+
                 (1|plot),data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleA5)
check_collinearity(simpleA5)
shapiro.test(residuals(simpleA5))

#Remove temperature and moisture
simpleAx<-lmer(stock.newRMLg.m2 ~newsession:treatment+treatment+newsession+
                 (1|plot),data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleAx)
check_collinearity(simpleAx)
shapiro.test(residuals(simpleAx))

#Graph each plot
forSAS_mergeda$preds1<-predict(simpleAx)
summary(simpleAx)

forSAS_mergeda$TrtPlot<-paste(forSAS_mergeda$treatment,forSAS_mergeda$plot)

forSAS_mergeda %>%
  
  ggplot(aes(x=newsession, y=preds1, group=TrtPlot, color=TrtPlot)) +
  
  geom_point() + geom_line() +theme_bw()


#Check best model
anova(simpleA5,simpleA6,simpleAx,simpleA0)
anova(simpleA5,simpleAx)

#Best model (simpleAx)


#model adequacy: check model assumptions 
hist(residuals(simpleAx), col="darkgray")
plot(fitted(simpleAx),residuals(simpleAx)) 

#model fit: coefficient of determination of the model
r.squaredGLMM(simpleAx)


#########
#######
#################coarse roots only stock######

#stock
##with interaction

simpleA0<-lmer(stock.newRMLg.m2 ~temperature+moisture+session:treatment+session+treatment+
                 (1|plot),data=coarsemodelA, control = CustomControls,REML=T)

summary(simpleA0)
check_collinearity(simpleA0)
shapiro.test(residuals(simpleA0))

#Remove moisture
simpleA6<-lmer(stock.newRMLg.m2 ~session:treatment+temperature+treatment+session+
                 (1|plot),data=coarsemodelA, control = CustomControls,REML=T)


summary(simpleA6)
check_collinearity(simpleA6)
shapiro.test(residuals(simpleA6))

#Remove temperature
simpleA5<-lmer(stock.newRMLg.m2 ~moisture+session:treatment+session+treatment+
                 (1|plot),data=coarsemodelA, control = CustomControls,REML=T)


summary(simpleA5)
check_collinearity(simpleA5)
shapiro.test(residuals(simpleA5))

#Remove moisture and temperature
simpleAx<-lmer(stock.newRMLg.m2 ~session:treatment+treatment+session+
                 (1|plot),data=coarsemodelA, control = CustomControls,REML=T)

summary(simpleAx)
check_collinearity(simpleAx)
shapiro.test(residuals(simpleAx))

#Graph
coarsemodelA$preds1<-predict(simpleAx)
summary(simpleAx)

coarsemodelA$TrtPlot<-paste(coarsemodelA$treatment,coarsemodelA$plot)

coarsemodelA %>%
  
  ggplot(aes(x=session, y=preds1, group=TrtPlot, color=TrtPlot)) +
  
  geom_point() + geom_line() +theme_bw()

#Test best model
anova(simpleA5,simpleA6,simpleAx,simpleA0)
anova(simpleA5,simpleAx)

#Best model (simpleAx)
summary(simpleAx)

r.squaredGLMM(simpleAx)

######################################
########################### P R O D U C T I O N ################################

#WITH interaction session and treatment

simpleAp0<-lmer(lnprod2.newRMLg.m2.day ~ treatment+temperature+moisture+newsession+newsession:treatment+
                  (1 | plot),
                data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleAp0)
check_collinearity(simpleAp0)
shapiro.test(residuals(simpleAp0))

#Remove moisture
simpleAp1<-lmer(lnprod2.newRMLg.m2.day ~ treatment+temperature+newsession+newsession:treatment+
                  (1 | plot),
                data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleAp1)
check_collinearity(simpleAp1)
shapiro.test(residuals(simpleAp1))

#Normality graph
df<-data.frame(resid=resid(simpleAp1))


ggplot(data = df, mapping = aes(sample = resid)) +
  
  stat_qq_line(identity=TRUE, na.rm=TRUE) +
  
  stat_qq_point(col="black") +
  
  stat_qq_band(bandType = "boot", conf=0.95, mapping =aes(fill = "Boot CI bands"), alpha = 0.5, qtype=3) +
  
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()
#

#Remove temperature amd moisture
simpleAp3<-lmer(lnprod2.newRMLg.m2.day ~ treatment+newsession+newsession:treatment+
                  (1 | plot),
                data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleAp3)
check_collinearity(simpleAp3)
shapiro.test(residuals(simpleAp3))

#Normality graph
df<-data.frame(resid=resid(simpleAp3))

ggplot(data = df, mapping = aes(sample = resid)) +
  
  stat_qq_line(identity=TRUE, na.rm=TRUE) +
  
  stat_qq_point(col="black") +
  
  stat_qq_band(bandType = "boot", conf=0.95, mapping =aes(fill = "Boot CI bands"), alpha = 0.5, qtype=3) +
  
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()
#

#Remove temperature
simpleAp7<-lmer(lnprod2.newRMLg.m2.day ~ treatment+newsession+moisture+newsession:treatment+
                  (1 | plot),
                data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleAp7)
check_collinearity(simpleAp7)
shapiro.test(residuals(simpleAp7))

#Normality graph
df<-data.frame(resid=resid(simpleAp7))

ggplot(data = df, mapping = aes(sample = resid)) +
  
  stat_qq_line(identity=TRUE, na.rm=TRUE) +
  
  stat_qq_point(col="black") +
  
  stat_qq_band(bandType = "boot", conf=0.95, mapping =aes(fill = "Boot CI bands"), alpha = 0.5, qtype=3) +
  
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()
#

#Testing best model
anova(simpleAp0,simpleAp1,simpleAp3,simpleAp7)

#Best model (simpleAp1)
summary(simpleAp1)
r.squaredGLMM(simpleAp1) 


#############
#################### coarse roots only production ## NOT NORMAL DO NOT USE

#production
# 
# coarsemodelA$prod2.newRMLg.m2.day[coarsemodelA$prod2.newRMLg.m2.day<0]=0
# coarsemodelA$mort.newRMLg.m2.day[coarsemodelA$mort.newRMLg.m2.day<0]=0
# 
# coarsemodelA$lnprod2.newRMLg.m2.day=log(coarsemodelA$prod2.newRMLg.m2.day+0.0001)
# coarsemodelA$lnmort.newRMLg.m2.day=log(coarsemodelA$mort.newRMLg.m2.day+0.0001)
# 
# 
# ##with interaction
# 
# simpleA0<-lmer(lnprod2.newRMLg.m2.day ~temperature+moisture+session:treatment+session+treatment+
#                  (1|plot),data=coarsemodelA, control = CustomControls,REML=T)
# 
# 
# summary(simpleA0)
# check_collinearity(simpleA0)
# shapiro.test(residuals(simpleA0))
# 
# #Remove moisture
# simpleA6<-lmer(lnprod2.newRMLg.m2.day ~session:treatment+temperature+treatment+session+
#                  (1|plot),data=coarsemodelA, control = CustomControls,REML=T)
# 
# 
# summary(simpleA6)
# check_collinearity(simpleA6)
# shapiro.test(residuals(simpleA6))
# 
# #Remove temperature
# simpleA5<-lmer(lnprod2.newRMLg.m2.day ~moisture+session:treatment+session+treatment+
#                  (1|plot),data=coarsemodelA, control = CustomControls,REML=T)
# 
# 
# summary(simpleA5)
# check_collinearity(simpleA5)
# shapiro.test(residuals(simpleA5))
# 
# #Remove temperature and moisture
# simpleAx<-lmer(lnprod2.newRMLg.m2.day ~session:treatment+treatment+session+
#                  (1|plot),data=coarsemodelA, control = CustomControls,REML=T)
# 
# #Graphs per plot
# coarsemodelA$preds1<-predict(simpleAx)
# 
# coarsemodelA$TrtPlot<-paste(coarsemodelA$treatment,coarsemodelA$plot)
# 
# 
# coarsemodelA %>%
#   
#   ggplot(aes(x=session, y=preds1, group=TrtPlot, color=TrtPlot)) +
#   
#   geom_point() + geom_line() +theme_bw()
# 
# 
# summary(simpleAx)
# check_collinearity(simpleAx)
# shapiro.test(residuals(simpleAx))
# 
# #Testing best model
# anova(simpleA5,simpleA6,simpleAx,simpleA0)
# anova(simpleA5,simpleAx)
# 
# #Best model (simpleAx)
# summary(simpleAx)
# #NOT NORMAL ()


########################### M O R T A L I T Y ################################

##with interaction

simpleAm0<-lmer(lnmort.newRMLg.m2.day ~ treatment+temperature+moisture+newsession+newsession:treatment+
                  (1| plot),
                data=forSAS_mergeda, control = CustomControls,REML=T)
# 
summary(simpleAm0)
check_collinearity(simpleAm0)
shapiro.test(residuals(simpleAm0))
# 
#Remove temperature
simpleAm<-lmer(lnmort.newRMLg.m2.day ~ treatment+moisture+newsession+newsession:treatment+
                 (1| plot),
               data=forSAS_mergeda, control = CustomControls,REML=T)
# 
summary(simpleAm)
check_collinearity(simpleAm)
shapiro.test(residuals(simpleAm))

#Remove moisture
simpleAm1<-lmer(lnmort.newRMLg.m2.day ~ treatment+temperature+newsession+newsession:treatment+
                  (1| plot),
                data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleAm1)
check_collinearity(simpleAm0)
shapiro.test(residuals(simpleAm1))

#Remove temperature and moisture
simpleAm2<-lmer(lnmort.newRMLg.m2.day ~ treatment+newsession+newsession:treatment+
                  (1| plot),
                data=forSAS_mergeda, control = CustomControls,REML=T)

summary(simpleAm2)
check_collinearity(simpleAm2)
shapiro.test(residuals(simpleAm2))

#Testing best model
anova(simpleAm0,simpleAm,simpleAm1,simpleAm2)


#Best model (simpleAm2)
summary(simpleAm2)


#model adequacy: check model assumptions 
hist(residuals(simpleAm2), col="darkgray")
plot(fitted(simpleAm2),residuals(simpleAm2)) 

#model fit: coefficient of determination of the model
r.squaredGLMM(simpleAm2) 



#######################################################################################

####linear regressions with soil nutrient and aboveground factors ######################
#Package:
library(tidyr)

##aboveground factors:

#leaf data from Cavaleri (available in Yaffar et al. 2021)

leaf=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tla_leafarea.csv")

leaf=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/tla_leafarea.csv")
l=leaf[leaf$year==2017|leaf$year==2018,]
l$hurricane[l$year==2017]="BEFORE"
l$hurricane[l$year==2018]="AFTER"
names(l)[2]="Plot"
names(l)[5]="HURRICANE"

##canopy cover (TRACE sensus)

c=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/Canopy_cover.csv")
c$date=as.Date(c$?..date,"%d-%b-%y")
plot(c$Canopy.openness~c$date)

c$treatment[c$plot %in% c(1,3,5)]="control"
c$treatment[c$plot %in% c(2,4,6)]="warming"

##color change

c$treatment=factor(c$treatment,levels=c("warming","control"))

canopy=c[c$date=="2017-06-01"|c$date=="2018-05-21",]

canopy$hurricane[canopy$date=="2017-06-01"]="BEFORE"
canopy$hurricane[canopy$date=="2018-05-21"]="AFTER"
canopy=canopy[,c(2,7,8,9,10)]
names(canopy)[1]="Plot"
colnames(canopy)[5]="HURRICANE"
canopy$treatment=as.character(canopy$treatment)
canopy$treatment[canopy$treatment=="control"]="Control"
canopy$treatment[canopy$treatment=="warming"]="Warming"



#belowground:
#soil data provided by Reed et al (2020)

MPT3d=read.csv("NGEE_Tropics_archive_Yaffar_TRACE_2021_20210319205333/Soil nutrient data GCB, 2021.csv")


#MERGING BELOWGROUND AND ABOVEGROUND
#merge soil nutrient data and leaf total area

MPT3d=merge(MPT3d,l,by=c("HURRICANE","Plot"))
MPT3d=MPT3d[,-c(3,22,23)]

#Merging soil nut+leaf and canopy cover

MPT3d=merge(MPT3d,canopy,by=c("HURRICANE","Plot","treatment"))
MPT3d=MPT3d[,-23]

MPT3d$treatment=factor(MPT3d$treatment,levels=c("Warming","Control"))


##BEFORE THE HURRICANES
###Graphs of all belowground and aboveground factor relationship with roo production ####################

g=MPT3d[MPT3d$HURRICANE=="BEFORE",] %>%
  gather(-HURRICANE, -Plot, -mort.newRMLg.m2.day,-prod2.newRMLg.m2.day,-treatment,
         key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = prod2.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  scale_shape_manual(values=c(21,24))+
  stat_smooth(method="lm",formula=y~x,se=F)+
  # ylim(0,7)+
  facet_wrap(~ var, scales = "free") +
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target ="C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/soil properties sig vs prod from all before by plot.pptx")  #Save the file wherever you want


#Significant regressions
#soil extractable NH4
m1=lm(prod2.newRMLg.m2.day~NH4,data=MPT3d[MPT3d$HURRICANE=="BEFORE",])
summary(m1)
#NOT SIG, BUT it is when CONTROL is separated from warming

m1=lm(prod2.newRMLg.m2.day~NH4,data=MPT3d[MPT3d$HURRICANE=="BEFORE"&
                                            MPT3d$treatment=="Control",])
summary(m1)
m1=lm(prod2.newRMLg.m2.day~NH4,data=MPT3d[MPT3d$HURRICANE=="BEFORE"&
                                            MPT3d$treatment=="Warming",])
summary(m1)

#Graph
Model.1 <- lm(prod2.newRMLg.m2.day~NH4+treatment,data=MPT3d[MPT3d$HURRICANE=="BEFORE",])
##coef from each model 
summary(Model.1)

equation1=function(x){summary(Model.1)$coefficients[2]*x+summary(Model.1)$coefficients[1]}

equation2=function(x){summary(Model.1)$coefficients[2]*x+summary(Model.1)$coefficients[1]+
    summary(Model.1)$coefficients[3]}

#GRAPH 2 LINES (one for control and another one for warming)

g=ggplot(MPT3d[MPT3d$HURRICANE=="BEFORE",],aes(y=prod2.newRMLg.m2.day,x=NH4,color=treatment))+
  geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])+
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target ="C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/soil NH4 vs prod two lines with TReatment.pptx")  #Save the file wherever you want



#microbial N concentration
m1=lm(prod2.newRMLg.m2.day~ubial.N,data=MPT3d[MPT3d$HURRICANE=="BEFORE",])
summary(m1)
#
#Graph with lm
g=ggplot(MPT3d[MPT3d$HURRICANE=="BEFORE",], aes(y =prod2.newRMLg.m2.day, x = ubial.N)) +
  labs(x="Microbial N",y="Root production")+
  geom_point(aes(col=treatment),shape = 16, size=1.8) +
  stat_smooth(formula= y~x,method="lm",alpha=0)+
  # geom_abline(aes(intercept=`(Intercept)`, slope=ubial.N), as.data.frame(t(fixef(Model.1))))+
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target ="C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/soil ubialN vs prod by plot mixed model before without treatment.pptx")  #Save the file wherever you want


###Graphs of all belowground and aboveground factor relationship with roo mortality ####################
g=MPT3d[MPT3d$HURRICANE=="BEFORE",] %>%
  gather(-HURRICANE, -Plot, -mort.newRMLg.m2.day,-prod2.newRMLg.m2.day,-treatment,
         key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = mort.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  scale_shape_manual(values=c(21,24))+
  stat_smooth(method="lm",formula=y~x,se=F)+
  # ylim(0,7)+
  facet_wrap(~ var, scales = "free") +
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/soil properties vs mort aded from before.pptx")


#regressions
#Significant regression
m1=lm(mort.newRMLg.m2.day~NH4,data=MPT3d[MPT3d$HURRICANE=="BEFORE",])
summary(m1)

#Graph
g=ggplot(MPT3d[MPT3d$HURRICANE=="BEFORE",],aes(x =NH4, y = mort.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  #scale_shape_manual(values=c(21,24))+
  stat_smooth(formula= y~x,method="lm",alpha=0)+
  #ylim(0,15)+
  # facet_wrap(~ var, scales = "free") +
  theme_classic()


m1=lm(mort.newRMLg.m2.day~ubial.N,data=MPT3d[MPT3d$HURRICANE=="BEFORE",])
summary(m1)

#Graph
g=ggplot(MPT3d[MPT3d$HURRICANE=="BEFORE",],aes(x =ubial.N, y = mort.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  #scale_shape_manual(values=c(21,24))+
  stat_smooth(formula= y~x,method="lm",alpha=0)+
  #ylim(0,15)+
  # facet_wrap(~ var, scales = "free") +
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target ="C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/ubialN vs mort before.pptx")  #Save the file wherever you want


#AFTER HURRICANES
#Production
###Graphs of all belowground and aboveground factor relationship with root production ####################

g=MPT3d[MPT3d$HURRICANE=="AFTER",] %>%
  gather(-HURRICANE, -Plot, -mort.newRMLg.m2.day,-prod2.newRMLg.m2.day,-treatment,
         key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = prod2.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  scale_shape_manual(values=c(21,24))+
  stat_smooth(method="lm",formula=y~x,se=F)+
  # ylim(0,7)+
  facet_wrap(~ var, scales = "free") +
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/soil properties vs prod aded from after.pptx")


#regressions
#Significant regressions

#NH4
m1=lm(prod2.newRMLg.m2.day~NH4,data=MPT3d[MPT3d$HURRICANE=="AFTER",])
summary(m1)

#Graph
g=ggplot(MPT3d[MPT3d$HURRICANE=="AFTER",],aes(x =NH4, y = prod2.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  #scale_shape_manual(values=c(21,24))+
  stat_smooth(method="lm",formula=y~x)+
  #ylim(0,15)+
  # facet_wrap(~ var, scales = "free") +
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target ="C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/NH4 vs prod after.pptx")  #Save the file wherever you want

#Microbila N
Model.1 <- lm(prod2.newRMLg.m2.day~ubial.N+treatment,data=MPT3d[MPT3d$HURRICANE=="AFTER",])
summary(Model.1)

equation1=function(x){summary(Model.1)$coefficients[2]*x+summary(Model.1)$coefficients[1]}

equation2=function(x){summary(Model.1)$coefficients[2]*x+summary(Model.1)$coefficients[1]+
    summary(Model.1)$coefficients[3]}

#Graph

g=ggplot(MPT3d[MPT3d$HURRICANE=="AFTER",],aes(y=prod2.newRMLg.m2.day,x=ubial.N,color=treatment))+
  geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])+
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/soil ubial N vs prod aded from after two lines of treatent.pptx")

#Mortality
###Graphs of all belowground and aboveground factor relationship with root mortality ####################

g=MPT3d[MPT3d$HURRICANE=="AFTER",] %>%
  gather(-HURRICANE, -Plot, -mort.newRMLg.m2.day,-prod2.newRMLg.m2.day,-treatment,
         key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = mort.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  scale_shape_manual(values=c(21,24))+
  stat_smooth(method="lm",formula=y~x,se=F)+
  # ylim(0,7)+
  facet_wrap(~ var, scales = "free") +
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target = "C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/soil properties vs mort aded from after.pptx")


#regressions
#Sig regressions
#NH4
m1=lm(mort.newRMLg.m2.day~NH4,data=MPT3d[MPT3d$HURRICANE=="AFTER",])
summary(m1)

#Graph
g=ggplot(MPT3d[MPT3d$HURRICANE=="AFTER",],aes(x =NH4, y = mort.newRMLg.m2.day)) +
  geom_point(aes(color=treatment)) +
  #scale_shape_manual(values=c(21,24))+
  stat_smooth(method="lm",formula=y~x)+
  #ylim(0,15)+
  # facet_wrap(~ var, scales = "free") +
  theme_classic()

read_pptx(path="C:/Users/17875/Documents/template.pptx") %>%
  add_slide() %>%
  ph_with(dml(ggobj=g),location=ph_location(left=0.5, top=0.5,width=8,height=4)) %>%
  print(target ="C:/Users/17875/Google Drive/Chapter 3 MR analysis/Scripts and data paper only/Version 2/graphs/NH4 ratio vs mort after.pptx")  #Save the file wherever you want

