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
library(Hmisc)
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

user <- dat[['user']]
check <- dat[['checkin']]
bus <- dat[['business']]
reviews <- dat[['review']]
df.rev  <- data.frame(users = reviews$user_id, stars=reviews$stars, type = reviews$type,
                      business = reviews$business_id, 
                      date=as.Date(as.POSIXlt(strptime(reviews$date, "%Y-%m-%d"))))

categories <- unique(as.vector(unlist(bus$categories)))
df.cat <- data.frame(cat=categories)
write.table(df.cat,"Categories.csv",sep=";",dec=".", row.names = F)
FoodCat<- read.csv("FoodStuff.csv",sep = ";", dec = ".")

foodcat <- as.vector(FoodCat$Cat)
bus.selection <- bus["Bars" %in% as.vector(unlist(bus$categories)),]

isFoodRelated <- sapply(1:nrow(bus), function(x) sum(foodcat %in% unlist(bus$categories[x])) >0)
bus.selec <- bus[isFoodRelated,]
bus.narrowed <- data.frame(buID= bus.selec$business_id, revCnt=bus.selec$review_count)

ambiences <- c("romantic", "intimate", "classy", "hipster", "divey", "touristy", "trendy", "upscale", "casual")
restrictions <- c("dairy-free", "gluten-free", "vegan", "kosher", "halal", "soy-free", "vegetarian")
for(i in 1:length(ambiences)){
    bus.narrowed[[ambiences[i]]] <- as.numeric(bus.selec$attributes$'Ambience'[[ambiences[i]]])
}
for(i in 1:length(restrictions)){
    bus.narrowed[[restrictions[i]]] <- as.numeric(bus.selec$attributes$'Dietary Restrictions'[[restrictions[i]]])
}


rev.sm <- reviews[, which(names(reviews) %in% c("user_id", "stars", "business_id"))]


busids <- rev.sm$business_id
pos.bus<- match(busids,bus.narrowed$buID)
userids <- rev.sm$user_id
pos.user<- match(userids,user$user_id)

length(unique(rev.sm$user_id))


df.positions <- data.frame(pos.bus = pos.bus, busID= busids, pos.users = pos.user, userids=userids)
df.pos.comp <- na.omit(df.positions)
bus.narrowed$busID <- bus.narrowed$buID
df.bu <- merge(df.pos.comp, bus.narrowed, by="busID")
df.bu2 <- df.bu[, -which(names(df.bu) %in% c("busID", "pos.bus", "pos.users","buID"))]
df.bu2 <- na.omit(df.bu2)
df.bu2$nrev <- rep(1,nrow(df.bu2))

library(plyr)
df.bu3 <- ddply(df.bu2,"userids",numcolwise(sum))
names(df.bu3)[names(df.bu3) == 'userids'] <- 'user_id'
df.bu4 <- merge(user,df.bu3, by="user_id")



