pdf(file = 'CO2.pdf',width = 5,height = 5)
co2 <- read.table('co2_exdata.txt',header = TRUE, sep="\t")
plot(x = co2$Time..minutes.,y = co2$CO2.levels..glucose..ppm.,
     ylim=c(0,6000),col="blue",xlab = "Time (m)", ylab = "CO2 level (ppm)")
glu_lm<-lm(co2$CO2.levels..glucose..ppm. ~ co2$Time..minutes.)
lac_lm <-lm(co2$CO2.levels..lactose..ppm. ~ co2$Time..minutes.)
points(x = co2$Time..minutes.,y = co2$CO2.levels..lactose..ppm.,col="red")
abline(lm(co2$CO2.levels..lactose..ppm. ~ co2$Time..minutes.),col="red")
abline(lm(co2$CO2.levels..glucose..ppm. ~ co2$Time..minutes.),col="blue")
legend(1,6000,c("glucose", "lactose"), text.col = c("red","blue"))
dev.off()

exdata <- read.table('example_data.txt',header = TRUE, sep="\t", row.names = 1)
exdata
library(psych)
ex_info <- describe(exdata)
plot(ex_info$mean, xlab='Sugar', ylab='Respiration rate (ppm / m)', xaxt='n',ylim=c(0,12))
axis(side=1,at=1:4,labels=colnames(exdata))
arrows(c(1,2,3,4), ex_info$mean - ex_info$sd, c(1,2,3,4),ex_info$mean +
         ex_info$sd, angle = 90, code = 3, length = 0.05)
boxplot(exdata, xlab = "Sugar", ylab = "Respiration rate (ppm / m)")
