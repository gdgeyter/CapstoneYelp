#------------------------------------------------------------------------------#
#                                                                              #
#                           Data Science Capstone                              #                                                                              #                                  
#                     Gert De Geyter, gertdegeyter@gmail.com                   #
#                                                                              #
#------------------------------------------------------------------------------#
# Program description

#------------------------------------------------------------------------------#
####                    includes and function definitions                   ####
#------------------------------------------------------------------------------#

#library("rjson")
#library(jsonlite)


#------------------------------------------------------------------------------#
####                           Data Exploration                             ####
#------------------------------------------------------------------------------#

library(rjson)
library(plyr)
if(!require('BBmisc')){
    install.packages('BBmisc',dep=TRUE)
}

suppressPackageStartupMessages(library('BBmisc'))
pkgs <- c('jsonlite','plyr','plyr','stringr','doParallel','ff','ffbase')
suppressAll(lib(pkgs)); rm(pkgs)
registerDoParallel(cores=4)
# 
# fnames <- c('business','checkin','review','tip','user')
# jfile <- paste0(getwd(),'/Data/yelp_academic_dataset_',fnames,'.json')
# dat <- llply(as.list(jfile), function(x) stream_in(file(x),pagesize = 10000))
# names(dat) <- fnames
# save.image(paste0(getwd(),'/Capstone_Quiz.RData'))
load(paste0(getwd(),'/Capstone_Quiz.RData'))





file <- paste(getwd(),'/Data/yelp_academic_dataset_user.json', sep="")
conn <- file(file, "r")
input <- readLines(conn, -1L)
test <- lapply(input, fromJSON)
test <- lapply(test, cbind)
test <- as.data.frame(test)
test <- as.data.frame(t(test))
row.names(test) <- seq(1, nrow(test))
save(test, file = "user.RData")


file <- paste(getwd(),'/Data/yelp_academic_dataset_review.json', sep="")
conn <- file(file, "r")
input <- readLines(conn, -1L)
test <- lapply(input, fromJSON)
test <- lapply(test, cbind)
test <- as.data.frame(test)
test <- as.data.frame(t(test))
row.names(test) <- seq(1, nrow(test))
save(test, file = "review.RData")

file <- paste(getwd(),'/Data/yelp_academic_dataset_business.json', sep="")
conn <- file(file, "r")
input <- readLines(conn, -1L)
test <- lapply(input, fromJSON)
test <- lapply(test, cbind)
test <- as.data.frame(test)
test <- as.data.frame(t(test))
row.names(test) <- seq(1, nrow(test))
save(test, file = "business.RData")
load("business.RData")


#'@ fnames <- c('business','checkin','review','tip','user')
#'@ jfile <- paste0(getwd(),'/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_',fnames,'.json')
#'@ dat <- llply(as.list(jfile), function(x) stream_in(file(x),pagesize = 10000))
#'@ names(dat) <- fnames

## Since read the json files will cost us few minutes time, here I save as .RData will be faster.
#'@ save.image(paste0(getwd(),'/Capstone_Quiz.RData'))

tmp <- unlist(test$attributes, use.names = FALSE)
table(tmp) 
7323/61184


length(na.omit(dat[['business']]$attributes$'Wi-Fi'[dat[['business']]$attributes$'Wi-Fi'=='free']))/length(na.omit(dat[['business']]$attributes$'Wi-Fi'=='free')) * 100





write.table(test,paste(getwd(),'/Data/yelp_review.csv', sep="")
            ,sep=";",dec=".", row.names = F)

