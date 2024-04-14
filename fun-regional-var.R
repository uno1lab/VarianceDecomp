regionvar_model = function(D, ITR=1000, SEED=NULL){

    if(!is.null(SEED)){set.seed(SEED)}
 
 	#--- input --- 
	#- data (suject-level) -- data.frame that maust include the following 3 variables  
	#   resp -- response (outcome) 
	#   predicted1 -- predicted probablity by the fixed-effect model (model1)
	#   predicted2 -- predicted probablity by the mixed-effect model (model2)
	#   hsa -- HSA (random effects: health service area)
	
	ucluster = unique(D$hsa)
	ncluster = length(ucluster)

    #--- observed ---
    ff = as.matrix(table(D$hsa, D$resp))
    dim(ff)
    data=data.frame(hsa=rownames(ff), y=ff[,2], n=ff[,1] + ff[,2])
    data$pobs = data$y/data$n
    
    tmp=data[,c("hsa","pobs")]
    D=merge(D,tmp)
    
    #--- simdata ---
    simdata=c()
    D$tmp_y0 = D$tmp_y1 = D$tmp_y2 = D$tmp_y3 = rep(0,nrow(D))
    for (i in 1:ITR){

     for (j in 1:nrow(D)){
        D$tmp_y0[j]=rbinom(1, size=1,prob=mean(D$resp)) ;
        D$tmp_y1[j]=rbinom(1, size=1,prob=D$predicted1[j])  ;
        D$tmp_y2[j]=rbinom(1, size=1,prob=D$predicted2[j])  ;
        D$tmp_y3[j]=rbinom(1, size=1,prob=D$pobs[j])  ;
       }


     ff0 = as.matrix(table(D$hsa, D$tmp_y0))
     ff1 = as.matrix(table(D$hsa, D$tmp_y1))
     ff2 = as.matrix(table(D$hsa, D$tmp_y2))
     ff3 = as.matrix(table(D$hsa, D$tmp_y3))
     tmp=data.frame(itration=i, hsa=rownames(ff0), y0=ff0[,2], y1=ff1[,2], y2=ff2[,2], y3=ff3[,2], n=ff0[,1] + ff0[,2])
     tmp$p0 = tmp$y0/tmp$n
     tmp$p1 = tmp$y1/tmp$n
     tmp$p2 = tmp$y2/tmp$n
     tmp$p3 = tmp$y3/tmp$n
     simdata=rbind(simdata, tmp)   
    }

    #--- simulation-based variance     
    var_p0 = var(simdata$p0) #--- when pj = pbar for all HSAs
    var_p1 = var(simdata$p1) #--- when pj = the one derivded for Model 1 for each HSA
    var_p2 = var(simdata$p2) #--- when pj = the one derivded for Model 2 for each HSA
    var_p3 = var(simdata$p3) #--- observed variance of pj 
 	

    #----------------------------------------------------
    # cluster-level varinace decomposition into 4 groups
    #----------------------------------------------------
    #--- Variance of Pj when if the null model (no effects) is correct (by binomial distribution) 
    v0 = var_p0 

    #--- Variation of pj explained by Model 1 (fixed-effects)
    v1 = var_p1 - v0 

    #--- Variation of pj explained by Model 2 (Random-effects)
    v2 = var_p2 - (v0 + v1) 

    #--- Variation unexplained
    v3 = var_p3 - (v0 + v1 + v2) 

    #--------------
    #--- output ---
    #--------------
    Z=list()
    Z$data = data
    Z$simdata = simdata

    Z$var_p3 = var_p3
    Z$var_p2 = var_p2
    Z$var_p1 = var_p1
    Z$var_p0 = var_p0

    Z$v0=v0
    Z$v1=v1
    Z$v2=v2
    Z$v3=v3
        
    return(Z)
}
