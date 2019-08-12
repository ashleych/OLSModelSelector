LHS_list<-strsplit(allModelsDiagnostics$model,"[~]")[[1]]
           
RHS_list<-strsplit(allModelsDiagnostics$model,"[~]")[[2]]
           


allModelsDiagnostics[,c("LHS","RHS") := tstrsplit(model,"[~]")]

allModelsDiagnostics[,RHS:=strsplit(RHS,"[+]")]
allModelsDiagnostics[,c("LHS","RHS") :=lapply(.SD,function(x) sapply(x,trimws)),.SDcols=c("LHS","RHS")][]

t<-c("avg_oil_pri_barrel_lag_3")

allModelsDiagnostics[apply(matrix(RHS),1,function(x)setequal(unlist(x),t)),.(model)]
test <- function() {
  allModelsDiagnostics[,{
    ifelse(setequal(RHS,t),0,1)}]
}
test()
allModelsDiagnostics[,which]
which(LETTERS == "R")
which((1:12)%%2 == 0,arr.ind = FALSE) 
(x <- c(sort(sample(1:20, 9)), NA))
(y <- c(sort(sample(3:23, 7)), NA))
setequal(t,c("avg_oil_pri_barrel_lag_3","avg_oil_pri_barrel_lag_2"))

setequal(c(1,2,4),c(2,4,1))


ma <- matrix(c(1:4, 1, 6:8), nrow = 2)
ma
apply(ma, 2, table)  #--> a list of length 2
apply(ma, 1, stats::quantile) # 5 x n matrix with rownames