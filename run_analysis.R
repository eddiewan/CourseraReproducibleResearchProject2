##Libraries
library(ggplot2)
library(xtable)
library(kableExtra)

data <- read.csv("repdata%2Fdata%2FStormData.csv")
str(data)

##Transform data

#date formats
data$BGN_DATE <- as.Date(data$BGN_DATE, format = "%m/%d/%Y")
data$END_DATE <- as.Date(data$END_DATE, format = "%m/%d/%Y")

#ALL EVTYPES to uppercase
data$EVTYPE <- toupper(data$EVTYPE)

##Remove spaces
data$EVTYPE <- sub(" ","", data$EVTYPE)
data$EVTYPE <- sub("  ","", data$EVTYPE)
data$EVTYPE <- sub("AVALANCE","AVALANCHE", data$EVTYPE)

data$dangerHealth <- data$FATALITIES + data$INJURIES

totalDanger <- aggregate(dangerHealth~EVTYPE, data = data, FUN = sum)
names(totalDanger) <- c("type", "total")

##only incorporate the activities with atleast 1 injury or fatality
totalDangerSub <- totalDanger[which(totalDanger$total >0),]
totalDangerSub <- totalDangerSub[with(totalDangerSub, order(-total)),]

##Top 25 most fatal
mostFatal <- totalDangerSub[1:25,]

mostFatal

g <- ggplot(mostFatal, aes(x = reorder(type, -total), y = total )) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        ggtitle("Top 25 most dangerous events with most injuries and fatalities") +
        labs(x = "Event type", y = "Total injuries and fatalities")

##Economic consequences

data$multiplyPROP <- 0 ##default
data$multiplyPROP[which(data$PROPDMGEXP == 'B' | data$PROPDMGEXP == 'b')] <- 1000000000
data$multiplyPROP[which(data$PROPDMGEXP == 'M' | data$PROPDMGEXP == 'm')] <- 1000000
data$multiplyPROP[which(data$PROPDMGEXP == 'K' | data$PROPDMGEXP == 'k')] <- 1000
data$multiplyPROP[which(data$PROPDMGEXP == 'H' | data$PROPDMGEXP == 'h')] <- 100
data$multiplyPROP[which(data$PROPDMGEXP %in% c(0,1,2,3,4,5,6,7,8))]<- 10

data$multiplyCROP <- 0 ##default
data$multiplyCROP[which(data$CROPDMGEXP == 'B' | data$CROPDMGEXP == 'b')] <- 1000000000
data$multiplyCROP[which(data$CROPDMGEXP == 'M' | data$CROPDMGEXP == 'm')] <- 1000000
data$multiplyCROP[which(data$CROPDMGEXP == 'K' | data$CROPDMGEXP == 'k')] <- 1000
data$multiplyCROP[which(data$CROPDMGEXP == 'H' | data$CROPDMGEXP == 'h')] <- 100
data$multiplyCROP[which(data$CROPDMGEXP %in% c(0,1,2,3,4,5,6,7,8))]<- 10

data$totalDamage <- (data$PROPDMG*data$multiplyPROP) + (data$CROPDMG * data$multiplyCROP)

totalEventDamage <- aggregate(totalDamage~EVTYPE, data = data, FUN = sum)
names(totalEventDamage) <- c("type", "totalDamage")

totalEventDamageSub <- totalEventDamage[which(totalEventDamage$totalDamage > 0),]
totalEventDamageSub <- totalEventDamageSub[with(totalEventDamageSub, order(-totalDamage)),]

mostDamage <- totalEventDamageSub[1:25,]
mostDamage

gEconomic <- ggplot(mostDamage, aes(x=reorder(type, -totalDamage), y = totalDamage)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust =1)) +
        ggtitle("Top 25 events causing the most damage to property and crops") +
        labs(x = "Event type", y = "Total damage costs")
