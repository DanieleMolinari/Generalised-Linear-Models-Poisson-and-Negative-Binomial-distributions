data <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"))
set.seed(10)
#checking the structure of data
str(data)
#some variables need to be transformed into numeric and factors
data$gdp16 <- as.numeric(as.character(data$gdp16))
data$gdp00 <- as.numeric(as.character(data$gdp00))
data$soviet <- as.factor(data$soviet)
data$comm <- as.factor(data$comm)
data$muslim <- as.factor(data$muslim)
data$oneparty <- as.factor(data$oneparty)
data$ssoviet <- as.factor(data$soviet)

#checking values of variables
summary(data)
#totgold and totmedals won in a year will be removed as they are the sum of medals won by all 
#countries in a year
data <- data[, -c(27:36)]
#the wide range of some variables suggest to log-transform them
#athletes has some values equal to zero so I add a small amount in oreder to not get any NANs
data$athletes00[data$athletes00==0] <- 0.1
data$athletes04[data$athletes04==0] <- 0.1
data$athletes08[data$athletes08==0] <- 0.1
data$athletes12[data$athletes12==0] <- 0.1
data$athletes16[data$athletes16==0] <- 0.1

data[3:12] <- log(data[, 3:12])
data[28:32] <- log(data[, 28:32])

#The data are divided into training and test data sets
#I will reshape the data set in order to use the whole data set as train set
#I first devide the data set in years and then I will put them together again when I create the 
#train set
data00 <- data[,(names(data) %in% c("gdp00", "pop00", "gold00", "tot00", "totgold00", 
                                     "totmedals00", "athletes00", "country", "country.code",
                                     "soviet", "comm", "muslim", "oneparty", "altitude",
                                     "host"))]
data00$year <- rep(2000, 108)
data04 <- data[,(names(data) %in% c("gdp04", "pop04", "gold04", "tot04", "totgold04", 
                                    "totmedals04", "athletes04", "country", "country.code",
                                    "soviet", "comm", "muslim", "oneparty", "altitude",
                                    "host"))]
data04$year <- rep(2004, 108)
data08 <- data[,(names(data) %in% c("gdp08", "pop08", "gold08", "tot08", "totgold08", 
                                    "totmedals08", "athletes08", "country", "country.code",
                                    "soviet", "comm", "muslim", "oneparty", "altitude",
                                    "host"))]
data08$year <- rep(2008, 108)
data12 <- data[,(names(data) %in% c("gdp12", "pop12", "gold12", "tot12", "totgold12", 
                                    "totmedals12", "athletes12", "country", "country.code",
                                    "soviet", "comm", "muslim", "oneparty", "altitude",
                                    "host"))]
data12$year <- rep(2012, 108)
# I now create the train set. What I want is having the variables with all observations from all years
# and not divided by years. This will also make the prediction phase easier.
train.data <- data.frame(c(data00$gdp00, data04$gdp04, data08$gdp08, data12$gdp12),
                         c(data00$pop00, data04$pop04, data08$pop08, data12$pop12),
                         c(data00$soviet, data04$soviet, data08$soviet, data12$soviet),
                         c(data00$comm, data04$comm, data08$comm, data12$comm),
                         c(data00$muslim, data04$muslim, data08$muslim, data12$muslim),
                         c(data00$oneparty, data04$oneparty, data08$oneparty, data12$oneparty),
                         c(data00$gold00, data04$gold04, data08$gold08, data12$gdp12),
                         c(data00$tot00, data04$tot04, data08$tot08, data12$tot12),
                         c(data00$altitude, data04$altitude, data08$altitude, data12$altitude),
                         c(data00$athletes00, data04$athletes04, data08$athletes08, data12$athletes12),
                         c(data00$host, data04$host, data08$host, data12$host),
                         c(data00$year, data04$year, data08$year, data12$year))
colnames(train.data) <- c("gdp", "pop", "soviet", "comm", "muslim", "oneparty", "gold", "tot",
                          "altitude", "athletes", "host", "year")

#I create the test data and I will rename the columns eliminitaing the year from the names in order
#to have the same variables' names in the test set and train set.  
test.data <- data[,(names(data) %in% c("gdp16", "pop16", "gold16", "tot16", "totgold16", 
                                       "totmedals16", "athletes16", "country", "country.code",
                                       "soviet", "comm", "muslim", "oneparty", "altitude",
                                       "host"))]
colnames(test.data) <- c("country", "country.code", "gdp", "pop", "soviet", "comm", "muslim", 
                         "oneparty", "gold", "tot", "altitude", "athletes", "host")

#I do the same for the set called data12 because this is the set used for exploratory analysis
colnames(data12) <- c("country", "country.code","gdp", "pop", "soviet", "comm", "muslim", 
                      "oneparty", "gold", "tot", "altitude", "athletes", "host", "year")

# I now need to check NAs.
# I want to see how many Nas there are in the two data sets. If there are not too many I will 
# eliminate them.
sum(is.na(train.data))
#There is only one NA, so it wil be removed
train.data <- na.omit(train.data)
sum(is.na(test.data))
#There are only two NAs so they will be removed
test.data <- na.omit(test.data)

#some exploratory plots
#I am going to check the relationships between the response variable 
#(tot number of medals won by a country) vs all the explanatory variables
library(ggplot2)
library(ggfortify)

ggplot(data12, aes(x=gdp, y=log(tot))) +
  geom_point() +
  xlab("GDP") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=pop, y=log(tot))) +
  geom_point() +
   xlab("Population") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=gold, y=log(tot))) +
  geom_point() +
  xlab("Number of gold medals won in 2012") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=athletes, y=log(tot))) +
  geom_point() +
  xlab("Number of athletes") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=log(altitude), y=log(tot))) +
  geom_point() +
  xlab("Altitude of county's capital city") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=soviet, y=log(tot))) +
  geom_boxplot() +
  xlab("Not Soviet Countries        ---------------            Soviet Countries") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=comm, y=log(tot))) +
  geom_boxplot() +
  xlab("Not former communist Countries        ------------       Former communist Countries") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=muslim, y=log(tot))) +
  geom_boxplot() +
  xlab("Not Muslim Countries        ---------------            Muslim Countries") +
  ylab("Total number of medals won in 2012")

ggplot(data12, aes(x=oneparty, y=log(tot))) +
  geom_boxplot() +
  xlab("Countries with more parties       ---------------         Countries with one party") +
  ylab("Total number of medals won in 2012")

#It might be also interesting to plot a correlation matrix 
#to have an idea how strong correlations are
train.new <- train.data[, -c(3:6, 11:12)]
correl <- cor(train.new)
corrplot::corrplot(correl, method = "number", type = "upper")

#I create a very general linear model first using glm poisson 
model0 <- glm(tot ~ gdp + pop + athletes + host + soviet + comm + muslim + 
                oneparty, family = poisson, data = train.data)
summary(model0)

#From this general model I can perform some variable selections. I will use the built in function
#step and the fanction from Mass library stepAIC specifying the perameter direction once as backward
#and once as forward
library(MASS)
step_model0 <- step(model0)
forw_model0 <- stepAIC(model0, direction = "forward")
BICstep <- step(model0, k = log(nrow(train.data))) #It uses BIC instead of AIC
summary(step_model0)
summary(forw_model0)
summary(BICstep)

library(olsrr)

#From the passage above the response variables statistically significant that have a p value bigger
#than 0.05 seem to be pop, athletes, host and muslim. Therefore I will create a model considering only
#these varibles as predictors of the response variable tot.

model1 <- glm(tot ~ pop + athletes + host + muslim , 
              family = poisson, data = train.data)
exp(confint(model1))
summary(model1)

#I check if outlier are responsible for the high deviace and linearity
autoplot(model1, which = c(1, 2, 4))

#It seems I can easily drop host
model2 <- glm(tot ~ pop + athletes + muslim , 
              family = poisson, data = train.data)
exp(confint(model2))
summary(model2)

#model 3 is my final model with poisson distribution. Will check for interactions
model3 <- glm(tot ~ pop*muslim + athletes*muslim, family = poisson, data = train.data)
summary(model3)

# I now want to check if with negative binomial distribution is possible to create a better model
model.selection <- glm.nb (tot ~ ., train.data)
step.model.selection <-step(model.selection)
forward <- step(model.selection, direction = "forward")
BICmodel.selection <- step(model.selection, k = log(nrow(train.data)))
summary(step.model.selection)
summary(forward)
summary(BICmodel.selection)

#The variables with p value less than 0.05 (excluding gold) are athletes pop and comm
model4 <- glm.nb(tot ~ athletes + pop + comm, data = train.data)
summary(model4)
exp(confint(model4))

#I will check if an interaction with comm can create a better model
model5 <- glm.nb(tot ~ athletes*comm + pop*comm, data = train.data)
summary(model5)
exp(confint(model5))

test.data$soviet <- as.numeric(test.data$soviet)
test.data$comm <- as.numeric(test.data$comm)
test.data$muslim <- as.numeric(test.data$muslim)
test.data$oneparty <- as.numeric(test.data$oneparty)
test.data$ssoviet <- as.numeric(test.data$soviet)

# I now check which model between model3 and model4 is better
pred.model.3 <- predict(model3, newdata = test.data, type = "response")
pred.model.4 <- predict(model4, newdata = test.data, type = "response")
RMSE.model.3 <- sqrt(mean((pred.model.3 - test.data$tot)^2))
RMSE.model.4 <- sqrt(mean((pred.model.4 - test.data$tot)^2))
RMSE.matrix <- matrix(c(RMSE.model.3, RMSE.model.4), 1, 2)
colnames(RMSE.matrix) <- c("Poisson", "Negative Binomial")
RMSE.matrix


