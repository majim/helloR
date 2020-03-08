#######################################ASSIGMENT WEEK 2 average solution 1

library(plyr)
library(data.table)

pollutantmean <- function(directory, id = 1:332, pollutant) {
    oldwd<-getwd()        
    setwd(file.path(getwd(), directory, fsep = "/"))
    mylist <- list.files(pattern = ".csv") ## identify files
    full_list<- lapply(mylist, read.csv) ## convert into a list of data frame, 
    alldata <-data.table::rbindlist(full_list)
    mydata <- alldata[ID %in% id,]
    setwd(oldwd)
    mean(mydata[[pollutant]], na.rm = TRUE) ##https://stackoverflow.com/questions/1169456/the-difference-between-bracket-and-double-bracket-for-accessing-the-el
}
tables()

########################################ASSIGMENT WEEK 2 complete



complete <- function(directory, id) {
    oldwd<-getwd()        
    setwd(file.path(getwd(), directory, fsep = "/"))
    mylist <- list.files(pattern = ".csv") ## identify files
    full_list<- lapply(mylist, read.csv) ## convert into a list of data frame, 
    alldata <-data.table::rbindlist(full_list)
    mydata <- alldata[ID %in% id,]
    setwd(oldwd)
    nobs<- mydata[,sum(complete.cases(nitrate, sulfate)), by = ID]
    colnames(nobs) <- c("ID","nobs")
    nobs[order(id)]
}


########################################ASSIGMENT WEEK 2 correlation

corr <- function(directory, threshold) {
    oldwd<-getwd()        
    setwd(file.path(getwd(), directory, fsep = "/"))
    mylist <- list.files(pattern = ".csv") ## identify files
    full_list<- lapply(mylist, read.csv) ## convert into a list of data frame, 
    alldata <-data.table::rbindlist(full_list)
    setwd(oldwd)
    tnobs<- alldata[,sum(complete.cases(nitrate, sulfate)), by = ID]
    corre <- alldata[,cor(nitrate, sulfate, use = "pairwise.complete.obs"), by = ID]
    finalt <- cbind(tnobs, corre[,-1])
    colnames(finalt) <- c("ID","nobs", "corre")       
    myvalid <- finalt[nobs > threshold]
    print(myvalid)
}
