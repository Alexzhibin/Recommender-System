########
#Create a sample matrix 
#######

data = data.frame(User1=c(5,4,2,1),User2=c(5,5,0,2),User3=c(1,2,5,5),User4=c(0,0,4,5),
                  row.names=c("Romance1","Romance2","Action1","Action2"))
attach(data)

newdata = data.frame(User5=c(2,1,5,NA),User6=c(1,1,NA,3),User7=c(NA,5,NA,1),User8=c(NA,NA,NA,2),
                     row.names=c("Romance1","Romance2","Action1","Action2"))
#######
#Method: Cosine 
######
library(lsa)
#####User-Based: 
rela_user = cosine(as.matrix(data))
rela_user
rela_user * newdata
##Predict value:
for (i in 1:nrow(newdata)){
   index= is.na(newdata[1,])
   user = newdata[1,][,index]
  
}


#####Item-Based: 
rela_item = cosine(t(as.matrix(data)))

