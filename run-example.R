library(lme4)
source("fun-regional-var.R")

#--- generating a sample data ---
ncluster=30
n.set = rpois(ncluster, lambda=20)
cluster = as.factor(rep(1:30, n.set))
nn = sum(n.set)
D=data.frame(
id = 1:nn, 
cluster = as.factor(rep(1:30, n.set)),
x1 = rnorm(nn, mean = 2, sd = 1),
x2 = rbinom(nn, size=1,prob=0.5),
x3 = rnorm(nn, mean = 0, sd = 5),
resp = rbinom(nn, size=1,prob=0.3))


#--- specify the variables --
fixed_effects = c("x1","x2","x3")
random_effects = "cluster"

#--- formular for model 1 and model 2
fmla1 = formula(paste0("resp ~ ",paste(fixed_effects,collapse="+")))
fmla2 = formula(paste0("resp ~ ",paste(fixed_effects,collapse="+"),"+(1|", random_effects,")"))

#--- model fit 
ft1=glm(fmla1, data=D, family=binomial)
ft2=glmer(fmla2, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0, family = binomial, data=D) 

#--- get variances (simulation)
tmp = data.frame(resp=D$resp, hsa=D$cluster, predicted1=predict(ft1, type="response"), predicted2=predict(ft2, type="response"))
bb = regionvar_model(tmp, ITR=1000, SEED=123)
save(bb, file="tmp.RData")

#---------------
#--- results ---
#---------------
bb$var_p0 #--- cluster-level variance of Pj when the null model (no effects) were correct
bb$var_p1 #--- cluster-level variance of Pj when Model 1 were correct
bb$var_p2 #--- cluster-level variance of Pj when Model 2 were correct
bb$var_p3 #--- cluster-level variance of Pj (observed)

#--- cluster-level varinace (Boxplot)
#load(file="tmp.RData")
#pdf("boxplot.pdf")
boxplot(bb$simdata$p3, 
        bb$simdata$p2, 
        bb$simdata$p1, 
        bb$simdata$p0, 
        ylab="%Treatment Received",
        names=c(
        "Observed",
        "Model 2",
        "Model 1",
        "No Model"), 
        main="Distribution of HSA-wise outcome")
#dev.off()

#--- cluster-level varinace decomposition into 4 groups
#--- Variance of Pj when the null model (no effects) were correct (by binomial distribution) 
bb$v0 # = bb$var_p0

#--- Variation of pj explained by Model 1 (fixed-effects) on top of the null model
bb$v1 # = bb$var_p1 - bb$v0 
 
#--- Variation of pj explained by Model 2 (Random-effects) on top of Model 1
bb$v2 # = bb$var_p2 - (bb$v0 + bb$v1)

#--- Variation unexplained by Model 2
bb$v3 # = bb$var_p3 - (bb$v0 + bb$v1 + bb$v2)


#---- proportions ---
aa=c(bb$v0,bb$v1, bb$v2, bb$v3)
paste0(round(aa/sum(aa)*100, digits=1),"%")




