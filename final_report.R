## ---- echo=FALSE---------------------------------------------------------
setwd("~/Desktop/MS_Courses/Stats_inf_regression/Project_1")
BGS = read.csv('BGS.csv', row.names=c(1))
girls = BGS[BGS$Sex == 1,]
boys = BGS[BGS$Sex == 0,]

get_cont_summary = function(dataframe){
  continuous_data = dataframe[,!(names(dataframe) %in% c('Sex', 'Soma'))]
  sample_mean = apply(continuous_data, 2, mean)
  sample_sd = apply(continuous_data, 2, sd)
  sample_median = apply(continuous_data,2,median)
  sample_min = apply(continuous_data, 2, min)
  sample_max = apply(continuous_data, 2, max)
  
  summary_cont = t(data.frame(sample_mean, sample_sd, sample_median, sample_min, sample_max))
  row.names(summary_cont) = c('Sample mean', 'Sample SD','Sample median', 'Sample min', 'Sample max')
  return(summary_cont)
}

get_sex_summary = function(dataframe){
  sex_table = table(dataframe[,c('Sex')])
  sex_table = cbind(sex_table, sex_table / sum(sex_table))
  colnames(sex_table) = c('Count', 'Sample proportion')
  return(sex_table)
}

get_soma_summary = function(BGS){
  girls = BGS[BGS$Sex == 1,]
  boys = BGS[BGS$Sex == 0,]
  girls_soma_table = as.data.frame(table(girls[,c('Soma')]),stringsAsFactors=FALSE)
  girls_soma_table[,'Perc_girls'] = girls_soma_table$Freq / sum(girls_soma_table$Freq)
  boys_soma_table =  as.data.frame(table(boys[,c('Soma')]),stringsAsFactors=FALSE)
  boys_soma_table[,'Perc_boys'] = boys_soma_table$Freq / sum(boys_soma_table$Freq)
  soma_table = merge(girls_soma_table, boys_soma_table, by = c('Var1'), all = TRUE)
  soma_table[is.na(soma_table)] = 0
  colnames(soma_table) = c('Soma', 'Count- girls', 'Sample proportion- girls', 'Count- boys', 'Sample proportion- boys')
  soma_table$Soma = as.numeric(soma_table$Soma)
  soma_table = soma_table[order(soma_table$Soma),]
  return(soma_table)
}

## ---- echo=FALSE---------------------------------------------------------
library(knitr)
kable(get_cont_summary(BGS), format='latex',digits = 2, caption = 'Summary of continuous features- both genders')
kable(get_cont_summary(boys), format='latex',digits = 2, caption = 'Summary of continuous features- boys')
kable(get_cont_summary(girls), format='latex',digits = 2, caption = 'Summary of continuous features- girls')
kable(get_soma_summary(BGS), format='latex',digits = 2, caption = 'Summary of soma distribution- both genders')
kable(get_sex_summary(BGS), format='latex',digits = 2, caption = 'Summary of sex distribution')


## ---- echo = FALSE, fig.cap = 'Comparing numeric feature distributions by sex (0 = Male, 1 = Female)', fig.width=8, fig.height=3.65----
library(reshape2)
library(ggplot2)
meltData <- melt(BGS, id=c("Sex"))
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot(aes(x=as.factor(Sex), fill=factor(Sex))) + facet_wrap(~variable, scale="free", nrow=2) + ylab('Numeric feature value') + xlab('Numeric feature split by sex (0 = Male, 1 = Female)') + labs(fill='Sex')

## ---- echo = FALSE, fig.cap = 'Somatype value vs numeric feature value by sex (0 = Male, 1 = Female)', fig.width=8, fig.height=3.65----
library(reshape2)
meltData <- melt(BGS, id=c("Sex", "Soma"))
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_point(aes(x=value, y=Soma, colour=factor(Sex))) + facet_wrap(~variable, scale="free", nrow=2) + ylab('Somatype value (0-7)') + xlab('Continuous feature') + labs(colour='Sex')

## ----echo = FALSE, message=FALSE-----------------------------------------
library(glmnet)

## ----in-text-fig, echo=FALSE, fig.cap= 'Using cross validation to optimize regularization', cache=TRUE, fig.height=3.1----
X = model.matrix(~0 + Sex + WT2 + HT2 + WT9 + LG9 + ST9 + WT18 + HT18 + LG18 + ST18 + Sex:(WT2 + HT2 + WT9 + LG9 + ST9 + WT18 + HT18 + LG18 + ST18), data = BGS)
y = BGS$Soma
set.seed(3513643)
cvfit = cv.glmnet(X, y, nlambda = 100,nfolds = 10)
plot(cvfit)

## ----echo=FALSE----------------------------------------------------------
coefs = as.matrix(coef(cvfit, s = "lambda.1se"))
not_zero = coefs[coefs != 0,]
kable(t(not_zero), format='latex', digits = 3, caption = 'Non-zero coefficients of final lasso model')

## ---- echo=FALSE, cache=TRUE---------------------------------------------
all_data = cbind(X,y)
coefficients = matrix(0, 500, 20)
for(i in 1:500){
  bootsample = all_data[sample(136, replace=T),]
  boot_X = bootsample[,(colnames(bootsample)!='y')]
  boot_y = bootsample[,c('y')]
  model = glmnet(boot_X, boot_y)
  coef.exact = coef(model, s = cvfit$lambda.1se, exact = TRUE)
  coefficients[i,] = t(as.matrix(coef.exact))
}
colnames(coefficients) = rownames(coef.exact)
target_vars = coefficients[,names(not_zero)]

## ---- echo=FALSE,error=FALSE, fig.cap='Bootstrap sample estimates of l1 regularized coefficients', fig.height=4----
meltCoefs = melt(target_vars)
p = ggplot(meltCoefs, aes(x = value))
p + geom_histogram(aes(y=..density..), bins = 15) + geom_density(color='red') + facet_wrap(~Var2, scale="free", nrow=3, ncol=4) + xlab('Feature in the lasso model') + ylab('Bootstrap estimate of density')

## ----echo = FALSE, message=FALSE-----------------------------------------
library(matrixStats)

## ---- echo=FALSE---------------------------------------------------------
summary_table = cbind(colQuantiles(target_vars, probs=c(0.025, 0.975)), apply(target_vars,2,mean))
colnames(summary_table) = c('2.5% Quant.', '97.5% Quant.', 'Mean')
kable(t(summary_table), format='latex',digits = 3, caption = 'Bootstrap estimate of the lasso coefficents distribution')

## ----echo=FALSE----------------------------------------------------------
y_hat = predict(cvfit, X, s = cvfit$lambda.1se)
resids = y - y_hat

resid_plot_df = data.frame(X[,'Sex'], y, resids)
colnames(resid_plot_df) = c('Sex','Soma','Resid')

ggplot(resid_plot_df, aes(x=Soma, y = Resid)) + geom_point(aes(color = factor(Sex, labels = c('Male','Female')))) + ggtitle('Residual plot') + ylab('Residuals') + xlab('Somatype') + labs(color = "Sex")

## ----echo = FALSE--------------------------------------------------------
kable(t(not_zero), format='latex', digits = 3, caption = 'Non-zero coefficients of final lasso model')

