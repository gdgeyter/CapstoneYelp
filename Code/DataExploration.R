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
library(ggplot2)
if(!require('BBmisc')){
    install.packages('BBmisc',dep=TRUE)
}

# Reading in the data
suppressPackageStartupMessages(library('BBmisc'))
pkgs <- c('jsonlite','plyr','plyr','stringr','doParallel','ff','ffbase','corrplot','rjson','Hmisc')
suppressAll(lib(pkgs)); rm(pkgs)
registerDoParallel(cores=4)

# fnames <- c('business','checkin','review','tip','user')
# jfile <- paste0(getwd(),'/Data/yelp_academic_dataset_',fnames,'.json')
# dat <- llply(as.list(jfile), function(x) stream_in(file(x),pagesize = 10000))
# names(dat) <- fnames
# save.image(paste0(getwd(),'/Capstone_Quiz.RData'))
load(paste0(getwd(),'/Capstone_Quiz.RData'))

# reading the relevant datasets
user <- dat[['user']]
bus <- dat[['business']]
reviews <- dat[['review']]

# create a list of all business categories and write it out
categories <- unique(as.vector(unlist(bus$categories)))
df.cat <- data.frame(cat=categories)
write.table(df.cat,"Categories.csv",sep=";",dec=".", row.names = F)

# read in the relevant food categories which were manually selected
FoodCat<- read.csv("FoodStuff.csv",sep = ";", dec = ".")
foodcat <- as.vector(FoodCat$Cat)
isFoodRelated <- sapply(1:nrow(bus), function(x) sum(foodcat %in% unlist(bus$categories[x])) >0)
bus.selec <- bus[isFoodRelated,]

# create a narrowed down business data frame and make the booleans numeric 
bus.narrowed <- data.frame(buID= bus.selec$business_id, revCnt=bus.selec$review_count)
ambiences <- c("romantic", "intimate", "classy", "hipster", 
               "divey", "touristy", "trendy", "upscale", "casual")
restrictions <- c("dairy-free", "gluten-free", "vegan", "kosher", 
                  "halal", "soy-free", "vegetarian")
for(i in 1:length(ambiences)){
    bus.narrowed[[ambiences[i]]] <- as.numeric(bus.selec$attributes$'Ambience'[[ambiences[i]]])
}
for(i in 1:length(restrictions)){
    bus.narrowed[[restrictions[i]]] <- as.numeric(bus.selec$attributes$'Dietary Restrictions'[[restrictions[i]]])
}


# select the relevant information from the reviews and find the matching
# positions in the users and business data frames
rev.sm <- reviews[, which(names(reviews) %in% c("user_id", "stars", "business_id"))]
busids <- rev.sm$business_id
pos.bus<- match(busids,bus.narrowed$buID)
userids <- rev.sm$user_id
df.positions <- data.frame(pos.bus = pos.bus, busID= busids, userids=userids)
df.pos.comp <- na.omit(df.positions)
bus.narrowed$busID <- bus.narrowed$buID
df.bu <- merge(df.pos.comp, bus.narrowed, by="busID")

df.bu2 <- df.bu[, -which(names(df.bu) %in% c("busID", "pos.bus","buID"))]
df.bu2 <- na.omit(df.bu2)
df.bu2$nrev <- rep(1,nrow(df.bu2))
df.bu3 <- ddply(df.bu2,"userids",numcolwise(sum))
names(df.bu3)[names(df.bu3) == 'userids'] <- 'user_id'
df.bu4 <- merge(user,df.bu3, by="user_id")
envDiet <- c(ambiences,restrictions)

df.envDiet.unscaled <- df.bu4[, which(names(df.bu4) %in% c(envDiet))]
df.colsums <- data.frame(Environment= envDiet, UserReviews = as.numeric(colSums(df.envDiet.unscaled)))
write.table(df.colsums ,"colsums .csv",sep=";",dec=".", row.names = F)
df.colsums  <- read.csv("colsums .csv",sep = ";", dec = ".")

barplt <- ggplot(data = df.colsums, 
                 aes(x = Environment, y=UserReviews)) + geom_bar(stat = "identity", fill="darkgreen",colour="darkgrey")+
    xlab("Ambience and Dietary restrictions") +
    ylab("User reviews") +
    theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
barplt


df.final <- df.bu4



for(i in 1:length(envDiet)){
    df.final[[envDiet[i]]] <- df.final[[envDiet[i]]]/df.final$nrev
    df.final[[envDiet[i]]] <- df.final[[envDiet[i]]]/sum(df.final[[envDiet[i]]])
}
df.envDiet <- df.final[, which(names(df.final) %in% c(envDiet))]


df.envDiet <- df.envDiet[, -which(names(df.envDiet) %in% c("touristy"))]
write.table(df.envDiet ,"envDiet .csv",sep=";",dec=".", row.names = F)
df.envDiet  <- read.csv("envDiet .csv",sep = ";", dec = ".")
corr.envDiet <- cor(df.envDiet)
corrplot(corr.envDiet, method = "ellipse")


