library(ggplot2)
ggplot2
data=read.csv("psfd_ri1999_v202110_csv.csv",header=TRUE)
age<-1999-data$a02-1911
work<-data$c01


for (i in c(1:length(data$c01))) {
  if(data$c01[i]==2){
    data$c01[i]<-0
  }
}

age<-c(min(age):max(age))
rate <- c(min(age):max(age))

min(age)

for (i in c(1:length(rate))) {
  rate[i] <- mean(work[age== 35+i])
}

data_agerate<-cbind.data.frame(age,rate)


ggplot(data = data_agerate) + 
  geom_point(mapping = aes(x = rate, y = age))+
  ggtitle("Rate of Working against Age")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = "Rate_of_Working.png", 
)
