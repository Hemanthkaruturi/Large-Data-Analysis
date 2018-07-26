load(file = "Data/data_999.RData")
load(file = "Data/data_miss.RData")

########################## Feature Selection with Boruta ###########################
#Finding important features using boruta package
library(Boruta)
boruta.train.999 <- Boruta(y~., data = data.999, doTrace = 2,maxRuns = 150 )
boruta.train.miss <- Boruta(y~., data = data.miss, doTrace = 2, maxRuns = 150)

print(boruta.train.999)
print(boruta.train.miss)

#Visualizing important features
  # boruta.train <- boruta.train.999
  # plot(boruta.train, xlab = "", xaxt = "n")
  # lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  #   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  # names(lz) <- colnames(boruta.train$ImpHistory)
  # Labels <- sort(sapply(lz,median))
  # axis(side = 1,las=2,labels = names(Labels),
  #      at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# Blue boxplots correspond to minimal, average and maximum Z score of a shadow attribute.
# Red, yellow and green boxplots represent Z scores of rejected, tentative and confirmed attributes respectively

#Now find which tentative variables are important
final.boruta.999 <- TentativeRoughFix(boruta.train.999)
print(final.boruta.999)

#All confirmed variables
getSelectedAttributes(final.boruta.999, withTentative = F)

#output with all confirmed variables
data.999.final <- attStats(final.boruta.999)
View(head(data.999.final))

#Final Output removing unimportant variables
#which( colnames(data)=="x151" )
data.999.boruta <- data.999[,c(-1,-32,-37,-48,-49,-50,-60,-67,-68,-69,-70,
                               -76,-77,-78,-82,-83,-84,-89,-90,-91,-92,-98,-103,
                               -104,-120,-121,-127,-130,-132,-134,-136,-137,-138,
                               -139,-140,-141,-145,-147,-148,-149,-150,-151,-152,-153,-155,-156
                               ,-157,-158,-159,-172,-222,-243)]

save(data.999.boruta, file = "data_999_boruta.RData")

#correlation matrix to find the correlation between the variables
library(caret)
correlationMatrix <- cor(data.999.boruta)

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

# print indexes of highly correlated attributes
print(highlyCorrelated)

#Removing variables that are highly correlated
data.999.boruta.cor <- data.999.boruta[,c(-2,-3,-7,-18,-32,-33,-34,-35,-36,-37,-40,-52,-56,-57,-58,-60,-66,-69,-70,-84,-89
                                          ,-90,-91,-92,-94,-97,-99,-100,-102,-105,-108,-113,-114,-115,-116,-120,-121,-122,-124,-128,-129,-130
                                          ,-131,-132,-133,-134,-135,-137,-138,-139,-140,-141,-142,-143,-144,-150,-151,-152,-153,-156,-161,-166,-167
                                          ,-168,-169,-173,-177,-179,-180,-184,-185,-186,-187,-189,-190,-191,-194,-195,-196,-200,-201,-202,-203,-205
                                          ,-206,-207,-209,-217,-221,-14,-15,-21,-19,-30,-22,-38,-53,-59,-64,-67,-73,-77 ,-79,-85,-86
                                          ,-88,-82,-98,-81,-104,-111,-117,-123,-136,-145,-146,-147,-96,-148,-157,-154,-155,-158,-162,-51,-164
                                          ,-170,-6,-174,-176,-165,-24,-197,-210,-211,-78,-216,-220)]

save(data.999.boruta.cor, file = "data_999_boruta_cor.RData")
################# FOR DATA.MISS
#Now find which tentative variables are important
final.boruta.miss <- TentativeRoughFix(boruta.train.miss)
print(final.boruta.miss)


#All confirmed variables
getSelectedAttributes(final.boruta.miss, withTentative = F)

#output with all confirmed variables
data.miss.final <- attStats(final.boruta.miss)
View(head(data.miss.final))

# 219,137,141,151,153,135,079 important
#138,140,161,184,217,136,134,124,074,56 not important
#Final Output removing unimportant variables
which( colnames(data)=="x151" )
data.miss.boruta <- data.miss[,c(-1,-32,-37,-48,-49,-50,-56,-60,-67,-68,-69,-70,-73,
                                 -76,-77,-82,-83,-84,-89,-90,-91,-92,-98,-103,
                                 -104,-119,-120,-121,-127,-129,-131,-133,-134,-135,-137,-138,
                                 -139,-140,-141,-148,-149,-150,-151,-152,-153,-154,-155,-156
                                 ,-157,-158,-159,-172,-176,-209,-222,-243)]

save(data.miss.boruta, file = "data_miss_boruta.RData")
#correlation matrix to find the correlation between the variables
library(caret)
correlationMatrix <- cor(data.miss.boruta)

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

# print indexes of highly correlated attributes
print(highlyCorrelated)

#Removing variables that are highly correlated
data.miss.boruta.cor <- data.miss.boruta[,c(-1,-4,-7,-18,-32,-33,-34,-35,-36,-37,-40,-55,-56,-57,-59,-65,-68,-69,-83,-88,-89
                                            ,-90,-91,-93,-95,-97,-98,-105,-111,-112,-113,-114,-118,-119,-120,-122,-125,-126,-127,-128,-129,-130
                                            ,-131,-132,-134,-135,-136,-137,-138,-139,-140,-147,-148,-149,-150,-151,-153,-157,-161,-162,-163,-164,-165,-169
                                            ,-173,-174,-176,-181,-182,-183,-185,-186,-187,-190,-191,-192,-197,-196,-199,-201,-202,-203,-205
                                            ,-213,-217,-14,-15,-21,-19,-30,-38,-52,-62,-66,-72,-76,-78,-84,-85,-87,-81,-96,-80,-94,-100,-104
                                            ,-106,-109,-115,-121,-133,-142,-143,-144,-145,-154,-152,-155,-158,-166,-6,-170,-172,-2,-175,-22
                                            ,-180,-24,-193,-206,-207,-77,-212,-216)]

save(data.miss.boruta.cor, file = "data_miss_boruta_cor.RData")
