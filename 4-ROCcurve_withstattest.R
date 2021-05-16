library(tidyr)
library(pROC)
library(rmda)
library(ggplot2)

options(scipen = 1)

flag="dizziness"
Testset=read.csv(sprintf("/home/hdd1/Projects/dizziness/results/ml_200710/ml_preds.csv"))

algs=c("lr","svm","rf","cat")
print(algs)
# write.csv(Testset,sprintf("/home/hdd1/Projects/mRSprediction/results/publishver_2005/y_preds_mrs%s_200527.csv",flag),row.names = F)

# roc object
roc_lr=roc(Testset$Y_test,Testset$Pred_lr)
roc_svm=roc(Testset$Y_test,Testset$Pred_svm)
roc_rf=roc(Testset$Y_test,Testset$Pred_rf)
roc_cat=roc(Testset$Y_test,Testset$Pred_cat)

# delong test
delong=function(rocobj){
  delongtest=roc.test(roc_lr,rocobj,method="delong")
  print(delongtest)
  # delongtest$p.value%>% round(3)%>% print
}
delong(roc_svm)$p.value%>% round(3)%>% print
delong(roc_rf)$p.value%>% round(3)%>% print
delong(roc_cat)$p.value%>% round(3)%>% print



library(tidyr)
library(pROC)
library(OptimalCutpoints)
date="200710"
savedir=paste0('/home/hdd1/Projects/dizziness/results/ml_200710')
thrdf=read.csv('/home/hdd1/Projects/dizziness/results/ml_200710/thresholds.csv')


takeSESP=function( alg, atsensitivity=99 ){ #"Pred1" ,"Y1"
  

  a=sprintf('thr= thrdf[thrdf$X=="%s","sen%s"]',alg,atsensitivity)
  eval(parse( text=a))  
  a=sprintf('rocobj = plot.roc(Testset$Y_test~Testset$Pred_%s)',alg)
  eval(parse( text=a))
  
  ci.sesp <- ci(rocobj, of="thresholds",thresholds=thr, boot.n=1000)
  print(ci.sesp)
  tmp=c(ci.sesp$specificity  
        ,ci.sesp$sensitivity)
  
  a=(sprintf("Specificity  = %.3f ( %.3f - %.3f )
          Sensitivity  = %.3f ( %.3f - %.3f )",
                tmp[2], tmp[1],tmp[3],
                tmp[5], tmp[4],tmp[6]))
  return(a)}

set.seed(42)
per_lr99=takeSESP("lr")
per_svm99=takeSESP("svm")
per_rf99=takeSESP("rf")
per_cat99=takeSESP("cat")

per_lr999=takeSESP("lr",999)
per_svm999=takeSESP("svm",999)
per_rf999=takeSESP("rf",999)
per_cat999=takeSESP("cat",999)

str(per_lr99)
df99=data.frame(c(per_lr99,per_svm99,per_rf99,per_cat99))
df999=data.frame(c(per_lr999,per_svm999,per_rf999,per_cat999))
df=cbind(df99,df999)

write.csv(df,'/home/hdd1/Projects/dizziness/results/ml_200710/performance_table.csv',row.names = F)


# mcnemar test

testMcnemar=function( alg,alg2="lr", atsensitivity=99 ){ #"Pred1" ,"Y1"
  a=sprintf('thr = thrdf[thrdf$X=="%s","sen%s"]',alg,atsensitivity)
  eval(parse( text=a)) 
  a=sprintf('thr2 = thrdf[thrdf$X=="%s","sen%s"]',alg2,atsensitivity)
  eval(parse( text=a))
  a=sprintf('tab = table((Testset$Pred_%s>thr)[Testset$Y_test==1],(Testset$Pred_%s>thr2)[Testset$Y_test==1]);print(tab)',alg, alg2)
  eval(parse( text=a))
  return(mcnemar.test(tab,correct = F))}

testMcnemarwithTrue=function( alg, atsensitivity=99 ){ #"Pred1" ,"Y1"
  a=sprintf('thr = thrdf[thrdf$X=="%s","sen%s"]',alg,atsensitivity)
  eval(parse( text=a))  
  a=sprintf('tab = table(Testset$Y_test,Testset$Pred_%s>thr);print(tab)',alg)
  eval(parse( text=a))
  
  return(mcnemar.test(tab,correct = F))}

testMcnemar("svm")
testMcnemar("svm",atsensitivity=999)
testMcnemar("rf")
testMcnemar("rf",atsensitivity=999)
testMcnemar("cat")
testMcnemar("cat",atsensitivity=999)

binom.test()



# LR
dset=read.csv('/home/hdd1/Projects/dizziness/data/DizzinessDataset_withTestsetLabel_191127.csv')
y_train=dset[dset$Testset=="False","Central.dizziness"]
trset=read.csv('/home/hdd1/Projects/dizziness/data/DizzinessTrainingset_withStandardization.csv')
indx <- which(sapply(trset, is.integer))
trset[, indx] <- lapply(trset[, indx], factor)

model <- glm(y_train ~ ., data =trset, family = "binomial")
summary(model)


# multi visit
dset_tr=dset[dset$Testset=="False",]
duppat=dset_tr$ID[duplicated(dset_tr$ID)]%>% unique()
length(duppat)
tmp=dset_tr[dset_tr$ID%in%duppat,]
table(tmp$ID)%>% sort


dset_tr_sort=dset_tr[order(dset_tr$arrival_date),]
dset_tr_sort[duplicated(dset_tr_sort$ID),]