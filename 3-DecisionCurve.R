library(rmda)
library(tidyr)
data(dcaData)

diz=read.csv("/home/hdd1/Projects/dizziness/results/prediction/prediction_1357rows_200326.csv",stringsAsFactors = F)
diz%>% str

set.seed(42)
#first use rmda with the default settings (set bootstraps = 50 here to reduce computation time). 
baseline.model <- decision_curve(TrueLabel~Prediction, #fitting a logistic model
                                 data = dcaData, 
                                 study.design = "cohort", 
                                 policy = "opt-in",  #default 
                                 bootstraps = 50)

#plot the curve
plot_decision_curve(baseline.model,  curve.names = "baseline model")


full.model <- decision_curve(TrueLabel ~ Prediction,
                             data = diz, 
                             bootstraps = 1000)
plot_decision_curve(full.model,  curve.names = "Ours")

#since we want to plot more than one curve, we pass a list of 'decision_curve' objects to the plot
# plot_decision_curve( list(baseline.model, full.model), 
#                      curve.names = c("Baseline model", "Full model"), xlim = c(0, 1), legend.position = "bottomright") 


#plot the components of the roc curve--true positive rate and false positive rate
plot_roc_components(full.model,  xlim = c(0, 1), 
                    col = c("black", "red"))

#plot the clinical impact 
plot_clinical_impact(full.model, xlim = c(0, 1), 
                     col = c("black", "blue"))


full.model_cv <- cv_decision_curve(TrueLabel ~ Prediction,
                                   data = diz,
                                   folds = 5, 
                                   thresholds = seq(0, 1, by = .01), 
                                   policy = "opt-out")

full.model_apparent <- decision_curve(TrueLabel ~ Prediction,
                                      data = diz, 
                                      thresholds = seq(0, 1, by = .01),
                                      confidence.intervals = 'none', policy = "opt-out")


plot_decision_curve( list(full.model_apparent, full.model_cv), 
                     curve.names = c("Apparent curve", "Cross-validated curve"),
                     col = c("red", "blue"), 
                     lty = c(2,1), 
                     lwd = c(3,2, 2, 1), 
                     legend.position = "bottomright") 
