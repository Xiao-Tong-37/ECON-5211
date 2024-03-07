#3.4
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

#4.2

#pick favorite value 

mu_0<-2
mu_1<-1
sigma_0<-3
sigma_1<-2
sigma_01<-0
sigma_v<-1
c<-0

# Load the data.table package
library(data.table)

# Set the seed for reproducibility
set.seed(123)

# Define the number of individuals
N <- 10000000

# Simulate ϵ0 and ϵ1
epsilon <- data.table(
  epsilon0 = rnorm(N),
  epsilon1 = rnorm(N)
)

# Print the first few rows of the data.table
print(head(epsilon))

#Create the columns for w0 and w1
w0<-mu_0+epsilon$epsilon0
w1<-mu_1+epsilon$epsilon1

# Generate the column I that take binary value.
I = cbind((w1 > w0+c)*1)

# Calculate E[w0|I] and E[w1|I]
E_w0 <- mean(mu_0+epsilon$epsilon0[I == 0])
E_w1 <- mean(mu_1+epsilon$epsilon1[I == 1])
E_e0 <- mean(epsilon$epsilon0[I == 0])
E_e1 <- mean(epsilon$epsilon1[I == 1])

