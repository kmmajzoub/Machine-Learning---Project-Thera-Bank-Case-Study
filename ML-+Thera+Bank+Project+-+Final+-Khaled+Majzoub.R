remove(Thera_Bank_Personal_Loan_Modelling_dataset_1)
getwd()
bpl = Thera_Bank_Personal_Loan_Modelling_dataset_1

anyNA(bpl)
bpl_w_na = na.omit(bpl)

anyNA(bpl_w_na)

str(bpl_w_na)

bpl_w_na$Education = as.factor(bpl_w_na$Education)
bpl_w_na$`Securities Account` = as.factor(bpl_w_na$`Securities Account`)
bpl_w_na$`CD Account` = as.factor(bpl_w_na$`CD Account`)
bpl_w_na$Online = as.factor(bpl_w_na$Online)
bpl_w_na$CreditCard = as.factor(bpl_w_na$CreditCard)
bpl_w_na$`Personal Loan` = as.factor(bpl_w_na$`Personal Loan`)

summary(bpl_w_na)

bpl_w_na = bpl_w_na[,-c(1,5,6)]
str(bpl_w_na)

?boxplot
par(mfrow=c(1,1))
boxplot(bpl_w_na$`Age (in years)`,horizontal = TRUE, main = "ages")
boxplot(bpl_w_na$`Experience (in years)`,horizontal = TRUE, main = "Exp")
boxplot(bpl_w_na$`Income (in K/month)`,horizontal = TRUE, main = "income")
boxplot(bpl_w_na$CCAvg,horizontal = TRUE, main = "CCavg")
boxplot(bpl_w_na$Mortgage,horizontal = TRUE, main = "Mortgage")

bpl_w_na$`Experience (in years)`= abs(bpl_w_na$`Experience (in years)`)


qplot(`Age (in years)`, data = bpl_w_na , geom = "histogram",xlab = "ages" ,ylab = "freq",
      main = "age freq")
qplot(`Experience (in years)`, data = bpl_w_na , geom = "histogram",xlab = "Exp" ,ylab = "freq",
      main = "Exp freq")
qplot(`Income (in K/month)`, data = bpl_w_na , geom = "histogram",xlab = "income" ,ylab = "freq",
      main = "income freq")
qplot(CCAvg, data = bpl_w_na , geom = "histogram",xlab = "ccavg" ,ylab = "freq",
      main = "ccavg freq")
qplot(Mortgage, data = bpl_w_na , geom = "histogram",xlab = "mortgage" ,ylab = "freq",
      main = "mortgage freq")


qplot(`Personal Loan`, data = bpl_w_na , geom = "bar",xlab = "loan" ,ylab = "freq",
      main = "loanfreq")
qplot(bpl_w_na$Education , data = bpl_w_na , geom = "bar",xlab = "Edu" ,ylab = "freq",
      main = "Edu")


bpl_w_na %>% group_by(`Personal Loan`,Education) %>% summarise (counts = n())
loan_vs_edu
bpl_w_na %>% group_by(`Personal Loan`,Online) %>% summarise (counts = n())
loan_vs_edu

qplot(bpl_w_na$Education , data = bpl_w_na , geom = "auto",xlab = "Edu" ,ylab = "freq",
      main = "Edu")

qplot(`Age (in years)`,`Personal Loan` , data = bpl_w_na , geom = "auto",xlab = "mortgage" ,ylab = "loan",
      main = "mortgage vs loan")

cor(bpl_w_na[,c(1,2,3)])

## Kmeans Clustering 
bpl_w_na_num = bpl_w_na[,-c(5,7,8,9,10,11)]
bpl_w_na_num.scaled = scale(bpl_w_na_num)
bpl_w_na_num.scaled
seed = 10
set.seed(seed)

clust2= kmeans(x=bpl_w_na_num.scaled, centers = 2, nstart = 5)
clust2

library(cluster)
clusplot(bpl_w_na_num.scaled,clust2$cluster,color = TRUE,shade = TRUE,labels = 2,lines = 1)

totWss= rep(0,5)
for(k in 1:5){set.seed(seed)
   clust= kmeans(x=bpl_w_na_num.scaled, centers = k, nstart = 5)
   totWss[k]= clust$tot.withinss}
plot(c(1:5),totWss,type = "b")

seed = 10
set.seed(seed)

clust_final= kmeans(x=bpl_w_na_num.scaled, centers = 3, nstart = 5)
clusplot(bpl_w_na_num.scaled,clust_final$cluster,color = TRUE,shade = TRUE,labels = 2,lines = 3)
bpl_w_na$cluster = clust_final$cluster
cust_profile = aggregate(bpl_w_na ,list(bpl_w_na$cluster), FUN = "mean")
cust_profile
head(bpl_w_na)
group1 = subset(bpl_w_na, cluster == 1)
group2 = subset(bpl_w_na, cluster == 2)
group3 = subset(bpl_w_na, cluster == 3)
                         
### CART

seed = 10
set.seed(seed)
split = sample.split(bpl_w_na$`Personal Loan`,SplitRatio = 0.7)

train = subset(bpl_w_na,split==TRUE)
test = subset(bpl_w_na, split==FALSE)
table(train$`Personal Loan`)
table(test$`Personal Loan`)
?rpart
tree_train_CART = rpart(formula = `Personal Loan` ~.,data = train , method = "class", minbucket = 3 , cp = 0 )
tree_test_CART = rpart(formula = `Personal Loan` ~.,data = test , method = "class", minbucket = 3 , cp = 0 )
tree_test_CART
tree_train_CART


rpart.plot(tree_test_CART)
rpart.plot(tree_train_CART)
printcp(tree_test_CART)
printcp(tree_train_CART)
plotcp(tree_test_CART)
plotcp(tree_train_CART)


### Pruning 
ptree_test = prune(tree_test_CART,cp = 0.004, "CP")
ptree_train = prune(tree_train_CART,cp = 0.003, "CP")
printcp(ptree_test)
printcp(ptree_train)
rpart.plot(ptree_test)
rpart.plot(ptree_train)


path.rpart(ptree_test, c(4:7))
path.rpart(ptree_train, c(4:7))


### prediction 

test$prediction = predict(ptree_test, data = test, type = "class")
test$score = predict(ptree_test, data = test, type = "prob")
train$prediction = predict(ptree_train, data = test, type = "class")
train$score = predict(ptree_train, data = test, type = "prob")


head(test)
head(train)
getwd()
train$`Experience (in years)`

### forest
set.seed(seed)
attach(test)
detach(test)
attach(train)
detach(train)
attach(test_w_c_p_s)
detach(test_w_c_p_s)

?randomForest
colnames(test)

test_w_c_p_s = test[,-c(12,13,14)]
test_rndforest = randomForest(`Personal Loan`~.,data = test_w_c_p_s , ntree=251, mtry = 3 , nodesize = 5 , importance = TRUE)


attach(train_w_c_p_s)
detach(train_w_c_p_s)
train_w_c_p_s = train[,-c(12,13,14)]
train_rndforest = randomForest( `Personal Loan` ~ ., data = train_w_c_p_s , ntree= 251 , mtry = 3 , nodesize = 5, importance = TRUE)

print(test_rndforest)
print(train_rndforest)


plot(test_rndforest)
plot(train_rndforest)



importance(test_rndforest)
importance(train_rndforest)

## tuning 
test_w_c_p_s = test_w_c_p_s[,-c(5,8,9,10,11)]

set.seed(seed)
tuned_test_rndforest = tuneRF(x = test_w_c_p_s, y = test_w_c_p_s$`Personal Loan`, mtryStart = 3, stepFactor = 1.5, ntreeTry = 55
                              ,improve = 0.0001 , nodesize=5 , trace = TRUE, plot = TRUE, doBest = TRUE, importance= TRUE ) 

tuned_train_rndforest = tuneRF(x = train_w_c_p_s, y = train_w_c_p_s$`Personal Loan`, mtryStart = 3, stepFactor = 1.5, ntreeTry = 40
                              ,improve = 0.0001 , nodesize=5 , trace = TRUE, plot = TRUE, doBest = TRUE, importance= TRUE ) 

### predict 
test_w_c_p_s$predict.class = predict(test_rndforest,test_w_c_p_s, type = "class")
test_w_c_p_s$prob1 = predict(test_rndforest,test_w_c_p_s, type = "prob")[,"1"]

head(test_w_c_p_s)
train_w_c_p_s$predict.class = predict(train_rndforest,train_w_c_p_s, type = "class")
train_w_c_p_s$prob1 = predict(train_rndforest,train_w_c_p_s, type = "prob")[,"1"]
head(train_w_c_p_s)

tbl_test = table(test_w_c_p_s$`Personal Loan`, test_w_c_p_s$predict.class)
tbl_train = table(train_w_c_p_s$`Personal Loan`, train_w_c_p_s$predict.class)
(tbl_test[1,2]+tbl_test[2,1])/nrow(test_w_c_p_s)
(tbl_train[1,2]+tbl_train[2,1])/nrow(train_w_c_p_s)


### CHAID

bpl_chaid = bpl_w_na[,-c(1,2,3,4,6,12)]
str(bpl_chaid)

tedu= table(bpl_chaid$`Personal Loan`, bpl_chaid$Education)
tsl=table(bpl_chaid$`Personal Loan`, bpl_chaid$`Securities Account`)
tcd=table(bpl_chaid$`Personal Loan`, bpl_chaid$`CD Account`)
tonline=table(bpl_chaid$`Personal Loan`, bpl_chaid$Online)
tcc=table(bpl_chaid$`Personal Loan`, bpl_chaid$CreditCard)
chisq.test(tedu)
chisq.test(tsl)
chisq.test(tcd)
chisq.test(tonline)
chisq.test(tcc)

split_chaid = sample.split(bpl_chaid$`Personal Loan`,SplitRatio = 0.7)
chaid_train_bpl = subset(bpl_chaid,split==TRUE)
chaid_test_bpl = subset(bpl_chaid,split==FALSE)

library(CHAID)
chaid.cntrl = chaid_control(minbucket = 30, minsplit = 100, alpha2 = .05,alpha4 = 0.05)
chaid_tree_train = chaid(`Personal Loan` ~Education + `CD Account`+ `Securities Account`,
                   data = chaid_train_bpl , control = chaid.cntrl)
chaid_tree_test = chaid(`Personal Loan` ~Education + `CD Account`+ `Securities Account`,
                         data = chaid_test_bpl , control = chaid.cntrl)
plot(chaid_tree_test)


#### chaid performance 
chaid_test_bpl$chaid.pred = predict(chaid_tree_test, data=chaid_test_bpl, type="response")
chaid_test_bpl$chaid.score = predict(chaid_tree_test, data=chaid_test_bpl, type="prob")[,"1"]
chaid_train_bpl$chaid.pred = predict(chaid_tree_train, data=chaid_train_bpl, type="response")
chaid_train_bpl$chaid.score = predict(chaid_tree_train, data=chaid_train_bpl, type="prob")[,"1"]


## chaid Confusion matrix 
chaid_test_cm = table(chaid_test_bpl$`Personal Loan`,chaid_test_bpl$chaid.pred)
chaid_train_cm = table(chaid_train_bpl$`Personal Loan`,chaid_train_bpl$chaid.pred)
chaid_test_cm
chaid_train_cm

## error rate 
(chaid_test_cm[1,2]+chaid_test_cm[2,1])/nrow(chaid_test_bpl)
(chaid_train_cm[1,2]+chaid_train_cm[2,1])/nrow(chaid_train_bpl)

chaidpredobjtrain = prediction(chaid_train_bpl$chaid.score,chaid_train_bpl$`Personal Loan`)
chaidpreftrain= performance(chaidpredobjtrain,"tpr","fpr")
plot(chaidpreftrain)

chaidpredobjtest = prediction(chaid_test_bpl$chaid.score,chaid_test_bpl$`Personal Loan`)
chaidpreftest= performance(chaidpredobjtest,"tpr","fpr")
plot(chaidpreftest)

#KS
max(chaidpreftrain@y.values[[1]]-chaidpreftrain@x.values[[1]])
max(chaidpreftest@y.values[[1]]-chaidpreftest@x.values[[1]])

## AUC
chaid_test_auc=performance(chaidpredobjtest,"auc")
as.numeric(chaid_test_auc@y.values)
chaid_train_auc=performance(chaidpredobjtrain,"auc")
as.numeric(chaid_train_auc@y.values)
   
##gini
ineq(chaid_test_bpl$chaid.score, "gini")
ineq(chaid_train_bpl$chaid.score, "gini")

## concordance 
Concordance(actuals = chaid_test_bpl$`Personal Loan`,predictedScores = chaid_test_bpl$chaid.score)
Concordance(actuals = chaid_train_bpl$`Personal Loan`,predictedScores = chaid_train_bpl$chaid.score)


   
### performance 


forest_tbl_test = table(test_w_c_p_s$`Personal Loan`, test_w_c_p_s$predict.class)
forest_tbl_train = table(train_w_c_p_s$`Personal Loan`, train_w_c_p_s$predict.class)
(forest_tbl_test[1,2]+forest_tbl_test[2,1])/nrow(test_w_c_p_s)
(forest_tbl_train[1,2]+forest_tbl_train[2,1])/nrow(train_w_c_p_s)

cart_tbl_test = table(test$`Personal Loan`, test$prediction)
cart_tbl_train = table(train$`Personal Loan`, train$prediction)
(cart_tbl_test[1,2]+cart_tbl_test[2,1])/nrow(test)
(cart_tbl_train[1,2]+cart_tbl_train[2,1])/nrow(train)


probs=seq(0,1,length=11)
forest_test_qs = quantile(test$score, probs)
forest_train_qs = quantile(train$score, probs)
forest_test_qs
forest_train_qs

probs=seq(0,1,length=11)
cart_test_qs = quantile(test_w_c_p_s$prob1, probs)
cart_train_qs = quantile(train_w_c_p_s$prob1, probs)
cart_test_qs
cart_train_qs

test$deciles = cut(test$score, unique(forest_test_qs), include.lowest = TRUE)
train$deciles = cut(train$score, unique(forest_train_qs), include.lowest = TRUE)
test_w_c_p_s$deciles = cut(test_w_c_p_s$prob1, unique(cart_test_qs), include.lowest = TRUE)
train_w_c_p_s$deciles = cut(train_w_c_p_s$prob1, unique(cart_train_qs), include.lowest = TRUE)


library(data.table)

cart_test_dt=data.table(test_w_c_p_s)
cart_train_dt=data.table(train_w_c_p_s)
test_rank_table = cart_test_dt[,list(cnt=length(`Personal Loan`),
                                     cnt_tar1=sum(`Personal Loan`==1),
                                     cnt_tar0=sum(`Personal Loan`==0)), by=deciles][order(-deciles)]
train_rank_table = cart_train_dt[,list(cnt=length(`Personal Loan`),
                                     cnt_tar1=sum(`Personal Loan`==1),
                                     cnt_tar0=sum(`Personal Loan`==0)), by=deciles][order(-deciles)]

test_rank_table$rrate = round(test_rank_table$cnt_tar1/test_rank_table$cnt,4)*100
train_rank_table$rrate = round(train_rank_table$cnt_tar1/train_rank_table$cnt,4)*100

test_rank_table
train_rank_table

### cumulative response rate 

test_rank_table$cum_resp = cumsum(test_rank_table$cnt_tar1)
train_rank_table$cum_resp = cumsum(train_rank_table$cnt_tar1)
test_rank_table$cum_non_resp = cumsum(test_rank_table$cnt_tar0)
train_rank_table$cum_non_resp = cumsum(train_rank_table$cnt_tar0)


## KS
test_rank_table$cum_rel_resp = round(test_rank_table$cum_resp/sum(test_rank_table$cnt_tar1),4)*100
test_rank_table$cum_rel_non_resp = round(test_rank_table$cum_non_resp/sum(test_rank_table$cnt_tar0),4)*100
train_rank_table$cum_rel_resp = round(train_rank_table$cum_resp/sum(train_rank_table$cnt_tar1),4)*100
train_rank_table$cum_rel_non_resp = round(train_rank_table$cum_non_resp/sum(train_rank_table$cnt_tar0),4)*100
test_rank_table
train_rank_table


test_rank_table$ks = abs(test_rank_table$cum_rel_resp - test_rank_table$cum_rel_non_resp)
train_rank_table$ks = abs(train_rank_table$cum_rel_resp - train_rank_table$cum_rel_non_resp)

### ROCR 

test_predobj = prediction(test_w_c_p_s$prob1, test_w_c_p_s$`Personal Loan`)
train_predobj = prediction(train_w_c_p_s$prob1, train_w_c_p_s$`Personal Loan`)
test_perf = performance(test_predobj, "tpr", "fpr")
train_perf = performance(train_predobj, "tpr", "fpr")
plot(test_perf)
plot(train_perf)

test_ks=max(test_perf@y.values[[1]]-test_perf@x.values[[1]])
test_ks
train_ks=max(train_perf@y.values[[1]]-train_perf@x.values[[1]])
train_ks
### auc 
train_auc= performance(train_predobj, "auc")
train_auc= as.numeric(train_auc@y.values)
train_auc
test_auc= performance(test_predobj, "auc")
test_auc= as.numeric(test_auc@y.values)
test_auc


### Gini
train_gini = ineq(train_w_c_p_s$prob1, "gini")
test_gini = ineq(test_w_c_p_s$prob1, "gini")
train_gini
test_gini


## Concordance 
Concordance(actuals = test_w_c_p_s$`Personal Loan`,predictedScores = test_w_c_p_s$prob1)
Concordance(actuals = train_w_c_p_s$`Personal Loan`,predictedScores = train_w_c_p_s$prob1)


### performance on RF

rfpredobjtrain = prediction(train$score , train$`Personal Loan`)
rfpreftrain = performance(rfpredobjtrain, "tpr", "fpr")
plot(rfpreftrain)


## gini
ineq(train$score, "gini")
ineq(test$score, "gini")

##concordance
Concordance(actuals = train$`Personal Loan`, predictedScores = train$score)
Concordance(actuals = test$`Personal Loan`, predictedScores = test$score)

## chaid 

predobjchaid = prediction(bpl_chaid$)