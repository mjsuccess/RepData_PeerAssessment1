# Reproducible Research: Peer Assessment 1

setwd("C:/Users/J Miller/Desktop/Data Science Course/Reproducible Research/RepData_PeerAssessment1")
require(lattice)

## Loading and preprocessing the data
a <- read.csv("./activity/activity.csv")
a$date = as.Date(a$date)

## What is mean total number of steps taken per day?
m = mean(a$steps,na.rm=T)
par(mar=c(5, 4, 4, 2) + 0.1)
hist(a$steps,col="green",mfrow=c(1,1))
summary(a$steps)

## What is the average daily activity pattern?
mti = tapply(a$steps,a$interval,mean,na.rm=T)
plot(mti,type="l",lwd=2,col="red",xlab="Interval",xaxt="n",ylab="Average number of steps",main="Average Daily Activity")

lablist = names(mti)
select = seq(0,288,by=50)
lablist = lablist[select]
lablist[2:6] = lablist[1:5]
lablist[1]= "0"
axis(1, at=seq(0, 288, by=50), labels = FALSE, tck=-.02)
text(seq(0, 288, by=50),par("usr")[3], labels = c("0","50","100","150","200","250"), pos = 1, xpd = TRUE)
text(seq(0, 288, by=50),par("usr")[3]-17, labels = lablist, pos = 1, xpd = TRUE, font=3,cex.lab=0.1)
abline(v=104)
which(mti==max(mti,na.rm=T))

## Imputing missing values
sum(!complete.cases(a)); #2304

#But using is.na(), we discover that only 'steps' has missing values
sum(is.na(a$steps))
sum(is.na(a$date))
sum(is.na(a$interval))

## Replace NA's with the mean for that 5 minute interval
a2 = a
for(i in 1:length(a2$steps))
{
    if(is.na(a2[i,1]))
    {
        a2[i,1] <- mti[paste(a2[i,3])]
    }
}

par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
hist(a2$steps,col="blue")
summary(a2$steps)

## Are there differences in activity patterns between weekdays and weekends?
a2[,4] = weekdays(a2[,2])
colnames(a2) <- c("steps","date","interval","day_type")
for(i in 1:length(a2$day_type))
{
    if(a2[i,4] %in% c("Saturday","Sunday"))
    {
        a2[i,4] <- "weekend"
    }
    else a2[i,4] <- "weekday"
}
a2[,4] = as.factor(a2[,4])

mtiwkend = as.data.frame(tapply(subset(a2,day_type=="weekend")$steps,subset(a2,day_type=="weekend")$interval,mean,na.rm=T))
mtiwkend[,2]=c("weekend")
mtiwkend[,3]=rownames(mtiwkend)
colnames(mtiwkend) = c("ave_steps","day_type","interval")
mtiwkday = as.data.frame(tapply(subset(a2,day_type=="weekday")$steps,subset(a2,day_type=="weekday")$interval,mean,na.rm=T))
mtiwkday[,2]=c("weekday")
mtiwkday[,3]=rownames(mtiwkday)
colnames(mtiwkday) = c("ave_steps","day_type","interval")

mtitot = rbind(mtiwkend,mtiwkday)
mtitot$day_type = as.factor(mtitot$day_type)
mtitot$interval = as.numeric(mtitot$interval)

xyplot(ave_steps~interval|day_type,data=mtitot,layout=c(1,2),type="l")



##Extra code not used
#md = tapply(a$steps,a$date,mean,na.rm=T)
#par(mar=c(7, 4, 3, 2) + 0.1,mfrow=c(1,1))
#plot(md,type="l",lwd=2,col="red",xaxt="n",xlab=NA,ylab="Average number of steps",main="Average Daily Activity")

#lablist = unique(a$date)
#select = seq(1,61,by=5)
#lablist = lablist[select]
#lablist = format(lablist,"%d %b,%y")
#axis(1, at=seq(1, 61, by=5), labels = FALSE)
#text(seq(-2, 59, by=5),par("usr")[3]-17, labels = lablist, pos = 1, xpd = TRUE, srt = 75)
