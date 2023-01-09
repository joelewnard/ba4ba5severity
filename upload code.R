load('completeDat.Rdata')
load('analDat.Rdata')


############ endpoints of ED, sympt-ED, hosp, sympt-hosp, ICU, Vent, Death

analDat = list()

for (m in 1:10){
  
  analDat[[m]] = completeDat[[m]]
  
  #### switch to old definition of symptomatic hospital admission and ED
  #analDat[[m]]$symptHosp = analDat[[m]]$hosp
  #analDat[[m]]$symptHosp[which(analDat[[m]]$hosp==1&analDat[[m]]$tSympt>analDat[[m]]$tHosp)] = 0
  #analDat[[m]]$symptHosp[which(analDat[[m]]$sympt==0)] = 0
  
  
  #analDat[[m]]$symptEd = analDat[[m]]$ed
  #analDat[[m]]$symptEd[which(analDat[[m]]$hosp==1&analDat[[m]]$tSympt>analDat[[m]]$tEd)] = 0
  #analDat[[m]]$symptEd[which(analDat[[m]]$sympt==0)] = 0
  
  analDat[[m]]$edY = analDat[[m]]$ed
  analDat[[m]]$edT = analDat[[m]]$tEd
  analDat[[m]]$symptEdY = analDat[[m]]$symptEd
  
  analDat[[m]]$ed30Y = analDat[[m]]$edY
  analDat[[m]]$ed30T = analDat[[m]]$edT
  analDat[[m]]$symptEd30Y = analDat[[m]]$symptEd
  
  analDat[[m]]$hospY = analDat[[m]]$hosp
  analDat[[m]]$hospT = analDat[[m]]$tHosp
  analDat[[m]]$symptHospY = analDat[[m]]$symptHosp
  
  analDat[[m]]$hosp15Y = analDat[[m]]$hosp
  analDat[[m]]$hosp15T = analDat[[m]]$tHosp
  analDat[[m]]$symptHosp15Y = analDat[[m]]$symptHosp
  
  analDat[[m]]$icuY = analDat[[m]]$icu
  analDat[[m]]$icuT = analDat[[m]]$tIcu
  
  analDat[[m]]$ventY = analDat[[m]]$vent
  analDat[[m]]$ventT = analDat[[m]]$tVent
  
  analDat[[m]]$deathY = analDat[[m]]$death
  analDat[[m]]$deathT = analDat[[m]]$tDeath
  
  
  analDat[[m]]$edY[analDat[[m]]$edT>15] = 0
  analDat[[m]]$edT[analDat[[m]]$edT>15] = 15
  analDat[[m]]$symptEdY[analDat[[m]]$symptEdT>15] = 0
  
  analDat[[m]]$ed30Y[analDat[[m]]$edT>30] = 0
  analDat[[m]]$ed30T[analDat[[m]]$edT>30] = 30
  analDat[[m]]$symptEd30Y[analDat[[m]]$symptEd30T>30] = 0
  
  selEd = which(paxStart<analDat[[m]]$edT)
  analDat[[m]]$edT[selEd] = endTime[selEd]
  analDat[[m]]$edY[selEd] = 0
  analDat[[m]]$symptEdY[selEd] = 0
  analDat[[m]]$edT[analDat[[m]]$edT>15] = 15
  
  selEd30 = which(paxStart<analDat[[m]]$ed30T)
  analDat[[m]]$ed30T[selEd30] = endTime[selEd30]
  analDat[[m]]$ed30Y[selEd30] = 0
  analDat[[m]]$symptEd30Y[selEd30] = 0
  analDat[[m]]$ed30T[analDat[[m]]$ed30T>30] = 30
  
  
  analDat[[m]]$hospY[analDat[[m]]$hospT>30] = 0
  analDat[[m]]$hospT[analDat[[m]]$hospT>30] = 30
  analDat[[m]]$symptHospY[analDat[[m]]$symptHospT>30] = 0
  
  analDat[[m]]$hosp15Y[analDat[[m]]$hospT>15] = 0
  analDat[[m]]$hosp15T[analDat[[m]]$hospT>15] = 15
  analDat[[m]]$symptHosp15Y[analDat[[m]]$symptHospT>15] = 0
  
  selHosp = which(paxStart<analDat[[m]]$hospT)
  analDat[[m]]$hospT[selHosp] = endTime[selHosp]
  analDat[[m]]$hospY[selHosp] = 0
  analDat[[m]]$symptHospY[selHosp] = 0
  
  selHosp15 = which(paxStart<analDat[[m]]$hosp15T)
  analDat[[m]]$hosp15T[selHosp15] = endTime[selHosp15]
  analDat[[m]]$hosp15Y[selHosp15] = 0
  analDat[[m]]$symptHosp15Y[selHosp15] = 0
  
  analDat[[m]]$icuY[analDat[[m]]$icuT>60] = 0
  analDat[[m]]$icuT[analDat[[m]]$icuT>60] = 60
  
  selIcu = which(paxStart<analDat[[m]]$icuT)
  analDat[[m]]$icuT[selIcu] = endTime[selIcu]
  analDat[[m]]$icuY[selIcu] = 0
  
  analDat[[m]]$ventY[analDat[[m]]$ventT>60] = 0
  analDat[[m]]$ventT[analDat[[m]]$ventT>60] = 60
  
  selVent = which(paxStart<analDat[[m]]$ventT)
  analDat[[m]]$ventT[selVent] = endTime[selVent]
  analDat[[m]]$ventY[selVent] = 0
  
  analDat[[m]]$deathY[analDat[[m]]$deathT>60] = 0
  analDat[[m]]$deathT[analDat[[m]]$deathT>60] = 60
  
  selDeath = which(paxStart<analDat[[m]]$deathT)
  analDat[[m]]$deathT[selDeath] = endTime[selDeath]
  analDat[[m]]$deathY[selDeath] = 0
}

agelb = c(-Inf,seq(10,80,10)); ageub = c(agelb[2:length(agelb)],Inf)
for (m in 1:10){
  analDat[[m]]$agegrp = NA
  for (j in 1:length(agelb)){
    analDat[[m]]$agegrp[which(analDat[[m]]$age>=agelb[j]&analDat[[m]]$age<ageub[j])] = j
  }
  analDat[[m]]$agegrp[which(analDat[[m]]$agegrp==4)] = 0
  analDat[[m]]$totVax = substr(analDat[[m]]$totVaxTime,1,1)
}


library(survival)
set.seed(1)
estsEd = estsEd30 = estsHosp15 = estsHosp = estsSymptHosp = estsIcu = estsVent = estsDeath = c()
for (m in 1:10){
  
  strat = paste(analDat[[m]]$testWeek,analDat[[m]]$weekend,sep='-')
  analSel = analDat[[m]]$tf==1&analDat[[m]]$testWeek%in%analWeeks&analDat[[m]]$enroll==1
  
  mod = coxph(Surv(edT,edY)~sgtf+as.factor(agegrp)+sex+totVaxTime+priorCovid+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsEd = c(estsEd,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))

  
  mod = coxph(Surv(ed30T,ed30Y)~sgtf+as.factor(agegrp)+sex+totVaxTime+priorCovid+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsEd30 = c(estsEd30,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))

  
  mod = coxph(Surv(hosp15T,hosp15Y)~sgtf+as.factor(agegrp)+sex+totVaxTime+priorCovid+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsHosp15 = c(estsHosp,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))

  
  mod = coxph(Surv(hospT,hospY)~sgtf+as.factor(agegrp)+sex+totVaxTime+priorCovid+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsHosp = c(estsHosp,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))

  
  mod = coxph(Surv(hospT,symptHospY)~sgtf+as.factor(agegrp)+sex+totVaxTime+priorCovid+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsSymptHosp = c(estsSymptHosp,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))

  
  analSel = analDat[[m]]$tf==1&analDat[[m]]$testWeek%in%18:29&analDat[[m]]$enroll==1
  mod = coxph(Surv(icuT,icuY)~sgtf+as.factor(agegrp)+sex+totVax+priorCovid+chargrp+priorOutpt+as.factor(priorInpt!='0')+as.factor(priorEd!='0')+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsIcu = c(estsIcu,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))

  
  mod = coxph(Surv(ventT,ventY)~sgtf+as.factor(agegrp)+sex+totVax+priorCovid+chargrp+priorOutpt+as.factor(priorInpt!='0')+as.factor(priorEd!='0')+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsVent = c(estsVent,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))
  
  mod = coxph(Surv(deathT,deathY)~sgtf+as.factor(agegrp)+sex+totVax+priorCovid+chargrp+priorOutpt+as.factor(priorInpt!='0')+as.factor(priorEd!='0')+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsDeath = c(estsDeath,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))
  
  print(m)
}




##############################################################
##############################################################
#### Table 2: ORs for SGTF ###################################
##############################################################
##############################################################



library(survival)
set.seed(1)
estsORadj = estsORunadj = array(NA,dim=c(10,1e5,5))
for (m in 1:10){
  
  strat = paste(analDat[[m]]$testWeek,analDat[[m]]$weekend,sep='-')
  analSel = analDat[[m]]$tf==1&analDat[[m]]$testWeek%in%analWeeks&analDat[[m]]$enroll==1
  
  mod = glm(as.numeric(sgtf)~totVax+priorCovid+as.factor(agegrp)+sex+chargrp+priorOutpt+priorInpt+priorEd+
              smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+
              strata(strat),data=analDat[[m]],subset=(analSel),family='binomial')
  estsORadj[m,,1:5] = mvrnorm(1e5,coef(mod)[2:6],vcov(mod)[2:6,2:6])
  
  mod = glm(as.numeric(sgtf)~totVax+
              strata(strat),data=analDat[[m]],subset=(analSel),family='binomial')
  
  estsORunadj[m,,1:4] = mvrnorm(1e5,coef(mod)[2:5],vcov(mod)[2:5,2:5])
  
  mod = glm(as.numeric(sgtf)~priorCovid+
              strata(strat),data=analDat[[m]],subset=(analSel),family='binomial')
  
  estsORunadj[m,,5] = mvrnorm(1e5,coef(mod)[2],vcov(mod)[2,2])
  
  print(m)
}



##############################################################
##############################################################
#### Table 3 vax and natural immun estimates #################
##############################################################
##############################################################


library(survival)
set.seed(1)
estsEd = estsEd30 = estsHosp = estsHosp15 = estsSymptHosp = estsIcu = estsVent = estsDeath = c()
for (m in 1:10){
  
  strat = paste(analDat[[m]]$testWeek,analDat[[m]]$weekend,sep='-')
  analSel = analDat[[m]]$tf==1&analDat[[m]]$testWeek%in%analWeeks&analDat[[m]]$enroll==1&analDat[[m]]$sgtf==0
  
  mod = coxph(Surv(edT,edY)~totVax+priorCovid+as.factor(agegrp)+sex+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+#sgtf+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsEd = rbind(estsEd,rnorm(1e5,coef(mod)[1:5],sqrt(diag(vcov(mod)[1:5,1:5]))))
  
  mod = coxph(Surv(ed30T,ed30Y)~totVax+priorCovid+as.factor(agegrp)+sex+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+#sgtf+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsEd30 = rbind(estsEd30,rnorm(1e5,coef(mod)[1:5],sqrt(diag(vcov(mod)[1:5,1:5]))))
  
  mod = coxph(Surv(hosp15T,hosp15Y)~totVax+priorCovid+as.factor(agegrp)+sex+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+#sgtf+
                strata(strat),
              data=analDat[[m]],subset=(analSel));
  estsHosp15 = rbind(estsHosp15,rnorm(1e5,coef(mod)[1:5],sqrt(diag(vcov(mod)[1:5,1:5]))))
  
  mod = coxph(Surv(hospT,hospY)~totVax+priorCovid+as.factor(agegrp)+sex+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+#sgtf+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsHosp = rbind(estsHosp,rnorm(1e5,coef(mod)[1:5],sqrt(diag(vcov(mod)[1:5,1:5]))))
  #
  mod = coxph(Surv(hospT,symptHospY)~totVax+priorCovid+as.factor(agegrp)+sex+chargrp+priorOutpt+priorInpt+priorEd+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+#sgtf+
                strata(strat),
              data=analDat[[m]],subset=(analSel)); 
  estsSymptHosp = rbind(estsSymptHosp,rnorm(1e5,coef(mod)[1:5],sqrt(diag(vcov(mod)[1:5,1:5]))))
  
  analSel = analDat[[m]]$tf==1&analDat[[m]]$testWeek%in%analWeeks60&analDat[[m]]$enroll==1&analDat[[m]]$sgtf==0
  mod = coxph(Surv(icuT,icuY)~totVax+priorCovid+as.factor(agegrp)+sex+chargrp+priorOutpt+as.factor(priorInpt!='0')+as.factor(priorEd!='0')+
                smoke+as.factor(ndi)+(bmiClass)+as.factor(race)+#sgtf+
                strata(strat),
              data=analDat[[m]],subset=(analSel));
  estsIcu = rbind(estsIcu,rnorm(1e5,coef(mod)[1:5],sqrt(diag(vcov(mod)[1:5,1:5]))))
  
  print(m)
}

