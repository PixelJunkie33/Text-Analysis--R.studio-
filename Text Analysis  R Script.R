
#R.Version() 
setwd("/mnt/commons/rachel")
getwd()

library("rjson")
library(plyr)
library(stringr)
#library("utils")
#library("data.table")
library(readr)
library(reshape)
library(foreign)



df0=read_csv("/mnt/commons/rachel/100kSamplOfAllSeed4694329905allBooksReadMin1revV1.csv")


df1=df0[which(df0$rev!=""),]

df1$isbn13[1:100] 
# need to filter out <isbn13 nil="true" />
df1$isbn13[df1$isbn13=="<isbn13 nil=\"true\" />"]=NA
#dim(df1)

for (i in df1$isbn13[1:20]){
  print(i)
  #d=paste("https://openlibrary.org/api/books?bibkeys=ISBN:",i,"&jscmd=data&format=json",sep="")  
}
#d

#https://openlibrary.org/dev/docs/api/books
json_file <- fromJSON(file="https://openlibrary.org/api/books?bibkeys=ISBN:9780391007604&jscmd=data&format=json")
#str(json_file)
#json_file
#json_file$`ISBN:9780391007604`$subjects
df<-as.data.frame(json_file$`ISBN:9780391007604`$subjects)
#df=df[,grep("name*", names(df), value=TRUE)]
df$isbn13=9780391007604
df

#trying differentones

#https://openlibrary.org/dev/docs/api/books
json_file <- fromJSON(file="https://openlibrary.org/api/books?bibkeys=ISBN:9780060004873&jscmd=data&format=json")
#str(json_file)
#json_file
#json_file$`ISBN:9780391007604`$subjects
df<-as.data.frame(json_file$`ISBN:9780060004873`$subjects)
#df=df[,grep("name*", names(df), value=TRUE)]
try((df$isbn13=9780060004873))
df

#json_file <- fromJSON(file="https://openlibrary.org/api/books?bibkeys=ISBN:9780552152945&jscmd=data&format=json")
#str(json_file)
#json_file
#json_file$`ISBN:9780391007604`$subjects
#try(df<-as.data.frame(json_file$`ISBN:9780552152945`$subjects))
#df=df[,grep("name*", names(df), value=TRUE)]
#df <- data.frame("isbn13" = NA) #this  is needed or it breaks eg 9780552152945
#df$isbn13 = NA #this  is needed or it breaks eg 9780552152945
#df$isbn13=json_file$`ISBN:9780552152945`$isbn13
#df

#df=df3[FALSE,]
#df$isbn13=9780391007604
#df
#df
length(na.omit(df1$isbn13))
length((df1$isbn13))

#https://openlibrary.org/dev/docs/api/books
json_file <- fromJSON(file="https://openlibrary.org/api/books?bibkeys=ISBN:9780391007604&jscmd=data&format=json")
#str(json_file)
#json_file
#json_file$`ISBN:9780391007604`$subjects
df<-as.data.frame(json_file$`ISBN:9780391007604`$subjects)
df=df[,grep("name*", names(df), value=TRUE)]
df$isbn13=9780391007604
df
df3=df[FALSE,]
df3

#for (i in na.omit(df1$isbn13[1:1000])){
for (i in na.omit(df1$isbn13)){
    print(i)
    json_file <- fromJSON(file=paste("https://openlibrary.org/api/books?bibkeys=ISBN:",i,"&jscmd=data&format=json",sep=""))
    #need try as some books dont exist in api
    df <- data.frame("isbn13" = NA) #this  is needed or it breaks eg 9780552152945
    try(df<-as.data.frame(json_file[[1]]$subjects)) #huge troubles looping over and concatenating $ for first single quoted ISBN item in json
    #df=df[,grep("name*", names(df), value=TRUE)] #breaks for some eg 9780399167836
    try((df$isbn13=i))
    df3=rbind.fill(df3,df)
}

length(df1$isbn13)
length(df3$isbn13)
tail(df1$isbn13,20)
tail(df3$isbn13,20)
#TODO: FIXME, and double check this
#BUG so ony about a fourth and some bugs, eg: 9780374126544 9780380800681 9780385721790 9781400030743 9780738723648 9780768422528 should be there!

#df3
df4=df3[,grep("name*|isbn13", names(df3), value=TRUE)] 
head(df4)
library(foreign)
write.dta(df4, "subjects.dta")

subjects=read.dta("subjects.dta")
length(subjects$isbn13)
head(subjects)
#tail(subjects,20)

sort(table(subjects$name_1),decreasing=T)
#either freq across names or better yet reshape! yes!

#subjectsR=subjectsR[order(subjectsR$isbn13),]  #sorting takes time
subjects=subjects[order(subjects$isbn13),]
options(width=5000) 
#print(subjects[c("isbn13", "name", "name_1", "name_2")][1:200,],row.names = FALSE)
subjects[1:200,]
#BUG FIXME! >>>just remove duplicates on isbn13--some rows for some reason are duplicates!


subjectsR <- melt(subjects, id=c("isbn13"))

head(subjectsR)
sort(table(subjectsR$value),decreasing=T)


ls()
for (thing in ls()) { message(thing); print(object.size(get(thing)), units='auto') }


rm(df0)

#length(subjects$isbn13)

length(df1$isbn13)

all <- join(subjectsR, df1, by='isbn13', type='right', match='all') #m:1 or 1:m!?
#right means all rows from right matching
#https://www.rdocumentation.org/packages/plyr/versions/1.8.4/topics/join

subjects

all=all[order(all$isbn13),] 
head(all,100)

length(subjectsR$isbn13)
length(all$isbn13)
names(all)
names(subjectsR)
head(all)

sort(table(all$value),decreasing=T) #weird kind of too many, checking above
#TODO: make sure thisis right and save all nicely sorted and ordered and go from there, next step
# find words in effiicent way and plot them

rm(list=ls())
# install.packages ("Rcpp")
# install.packages ("sjPlot")
# install.packages ("DescTools")
# install.packages ("qdap")
# install.packages ("R.utils")
# install.packages ("digest")
# install.packages ("rJava")
# install.packages ("lubridate")
# install.packages ("lme4")
# install.packages ("stringr")
# install.packages ("sjmisc")
# install.packages ("lmerTest")
# install.packages ("nlme")
# install.packages ("lattice")
# install.packages ("Hmisc")
# install.packages("knitr")
# library (stringi, lib.loc = "/var/autofs/misc/hpc/hart/RPackages")
#library (dplyr, lib.loc = "/var/autofs/misc/hpc/hart/RPackages")

# install.packages("devtools")

# library (Hmisc)
# library (rJava)
# library (DescTools)
# library (ggplot2)
# library (tm)
# library (lubridate)
# library (sjmisc)
# library (sjPlot)

ls()

#install.packages("readr")
#install.packages("data.table")
#install.packages("qdap")
#install.packages("lubridate")

library (readr)
library (data.table)
library (qdap)
library (lubridate)
library (dplyr)


# this next step gets the dataset that adam has downloaded
t1<- (read_csv("t1.csv"))


names (t1)

# we count words in each review using word_count from the QDAP pacakge
t1$wc<- word_count (t1$rev, byrow=TRUE, missing = NA, digit.remove=TRUE,
                    names=FALSE)

truncdf(t1[1:10, ], 40)

#calculate dates 
t1$month <- substr(t1$datAdd, 5, 7)
t1$day <- substr(t1$datAdd, 9, 10)
t1$time <- substr (t1$datAdd, 11, 18)
# library (stringr)
t1$year <-stringr::str_sub(t1$datAdd,-5,-1)
t1$date2 <- paste (t1$day, t1$month, t1$year, sep='-')
t1$date3 <- paste (t1$date2, t1$time, sep=' ')
t1$date <-lubridate::dmy_hms(t1$date3)
t1$date2a <- lubridate::dmy(t1$date2)

library (dplyr)
t1$usrID <- as.factor(t1$usrID)

diff_df <- t1  %>%    
