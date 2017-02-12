#MArina E. JOhnson - mjohnson393

#problem 1
install.packages('ggplot2') #upload ggplot2 library
install.packages('plyr') #upload plyr library
install.packages('reshape2')
install.packages('gridExtra')
install.packages('colorspace')
library(colorspace)
library(reshape2)
library(ggplot2) #activate ggplot2 library
library(plyr) # activate plyr library
library(gridExtra)

#upload the dataset
data(midwest)
midwest = midwest

#plot the percent prfessional of each country in five states on a boxplot
ggplot(data = midwest, aes(x = midwest$state, y = midwest$percprof)) + geom_boxplot() + 
  labs(title = "Boxplots of percent prof by states", x ='midwest states', y = 'percent professional')

# agregate the data and calcuate the percent professional by state
data1 = ddply(midwest, .(state), summarise, population = sum(poptotal))
midwest$profpop = (midwest$percprof*midwest$poptotal)/100
data2 = ddply(midwest, .(state), summarise, population_prof = sum(profpop))
data = merge(data1, data2, by = 'state')
data$percent_prof=data$population_prof/data$population

# plot the agregated percentage of professionals of each state on a line plot
plot(data$percent_prof, xaxt = 'n', type = 'h', col = 'red', cex = .9, main = 'plot of agregated percentages')
axis(1, at = 1:5, labels = data$state)

# plot the agregated percentage of professionals of each state on a dot plot
ggplot(data = data, aes(x = data$state, y = (data$percent_prof)*100)) + geom_point(size = 7, color = 'red') +
  labs(title = 'plot of agregated percentages', x = 'midwest states', y = ' total percent professional')

#calculate the descriptive statistics for percprof of each state 
mean_prof = ddply(midwest, .(state), summarise, mean = mean(percprof))
first_quartile_prof = ddply(midwest, .(state), summarise, first_quartile = quantile(percprof)[2])
median_prof = ddply(midwest, .(state), summarise, first_quartile = quantile(percprof)[3])
third_quartile_prof = ddply(midwest, .(state), summarise, first_quartile = quantile(percprof)[4])

##################################################################################################

#problem 2

#create a dataframe with states and their number of high school and college graduates, as well as the percentages
data_pop = ddply(midwest, .(state), summarise, population = sum(poptotal))
midwest$highpop = (midwest$perchsd * midwest$poptotal) / 100
midwest$colpop = (midwest$percollege * midwest$poptotal) / 100
data_high = ddply(midwest, .(state), summarise, population_high = sum(highpop))
data_edu = merge(data_pop, data_high, by = 'state')
data_edu$percent_high=data_edu$population_high / data_edu$population
data_col = ddply(midwest, .(state), summarise, population_col = sum(colpop))
data_edu = merge(data_edu, data_col, by = 'state')
data_edu$percent_col = data_edu$population_col / data_edu$population


#graphs
ggplot(data = midwest, aes(x = midwest$perchsd, y = midwest$percollege, color = state)) + geom_point() +
  facet_grid(.~state) + labs(title = "scatter plots of high school % vs. college %", x = 'high school %', y = 'college school %')

#correlation between attributes

panel.cor = function(x, y, digits = 2, cex.cor, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r = cor(x, y)
  txt = format(c(r, 0.123456789), digits = digits)[1]
  txt = paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p = cor.test(x, y)$p.value
  txt2 = format(c(p, 0.123456789), digits = digits)[1]
  txt2 = paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

myvars = names(midwest) %in% c("percollege", "perchsd") 
pairs(midwest[myvars], panel = panel.smooth, upper.panel = panel.cor, main = 'Scatter plot of high school % vs. college % ')


# aggregated data and plotting it
data_edu_long = melt(data_edu, id="state")  # convert to long format
data_long2 = subset(data_edu_long, data_edu_long$variable == 'percent_col')
data_long1 = subset(data_edu_long, data_edu_long$variable == 'percent_high')
data_long = rbind(data_long1, data_long2)
ggplot(data = data_long,
       aes(x = state, y = value*100, colour = variable)) +
  geom_point(size = 7) + labs(title = "% high school and college graduates by state", x = 'state', y = ' % graduates')



# boxplots of highschool and college graduates
myvars2 = names(midwest) %in% c("percollege", "perchsd", 'state')
state_col_hs = midwest[myvars2]
require(gridExtra)
plot1 = ggplot(data = midwest, aes(x = state_col_hs$state, y = state_col_hs$perchsd)) + geom_boxplot() + labs(title = "% high school graduates by state", x = 'state', y = ' % high school graduates')
plot2 = ggplot(data = midwest, aes(x = state_col_hs$state, y = state_col_hs$percollege)) + geom_boxplot() + labs(title = "% college graduates by state", x = 'state', y = ' % college graduates')
grid.arrange(plot1, plot2, ncol=2)

########################################################################################################

# problem 4
X = runif(100000)
Y = runif(100000)
jpeg('myplotN100000.jpg')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
dev.copy(png, 'myplotN100000.png')
dev.off()
pdf('myplotN100000.pdf')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y, main = 'N=100000')
postscript('myplotN100000.ps', onefile=TRUE)
dev.off()

# N=50000
X = runif(50000)
Y = runif(50000)
jpeg('myplotN50000.jpg')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
dev.copy(png, 'myplotN50000.png')
dev.off()
pdf('myplotN50000.pdf')
plot(x = X, y=Y)
dev.off()
plot(x = X, y=Y)
postscript('myplotN50000.ps', onefile=TRUE)
dev.off()


#N=10000
X = runif(10000)
Y = runif(10000)
jpeg('myplotN10000.jpg')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
dev.copy(png, 'myplotN10000.png')
dev.off()
pdf('myplotN10000.pdf')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
postscript('myplotN10000.ps', onefile=TRUE)
dev.off()


# N = 5000
X = runif(5000)
Y = runif(5000)
jpeg('myplotN5000.jpg')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
dev.copy(png, 'myplotN5000.png')
dev.off()
pdf('myplotN5000.pdf')
plot(x = X, y = Y)
dev.off()



# N = 1000
X = runif(1000)
Y = runif(1000)
jpeg('myplotN1000.jpg')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
dev.copy(png, 'myplotN1000.png')
dev.off()
pdf('myplotN1000.pdf')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
postscript('myplotN1000.ps', onefile=TRUE)
dev.off()


# N = 100
X = runif(100)
Y = runif(100)
jpeg('myplotN100.jpg')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
dev.copy(png, 'myplotN100.png')
dev.off()
pdf('myplotN100.pdf')
plot(x = X, y = Y)
dev.off()
plot(x = X, y = Y)
postscript('myplotN100.ps', onefile=TRUE)
dev.off()

#saving file sizes
file_size = c(17, 56, 88, 88, 30, 15, 6,	12,	41,	76,	361, 715, 4,	11,	21,	22,	5,	3, 6, 6, 6, 6, 6, 6)
file_type = c(rep('JPEG', 6), rep('PDF', 6), rep('PNG', 6), rep('PS', 6)) 
N_size = rep(c(100,	1000, 5000, 10000, 50000, 1000000), 4)
file_data = as.data.frame(file_type)
file_data$N = N_size
file_data$values = file_size
summary(file_data)



ggplot(data = file_data,
       aes(x = log(N_size), y = log(file_size), colour = file_type)) +
  geom_point(size = 7) + geom_line() + labs(title = "File type vs File size vs Sample size (N)", x ='log(N)', y = 'log(file size)')

# plotting the graph for sample size and file size
par(mfrow = c(1,2))
X = runif(100000)
Y = runif(100000)
plot(x = X, y = Y, main = 'N=100000')
X = runif(100)
Y = runif(100)
plot(x = X, y = Y, main = 'N=100')



###############################################################################################

# Problem 5
data(diamonds)
myvars3 = names(diamonds) %in% c("color", "carat", "price") 
data_diamond = diamonds[, myvars3]
summary(data_diamond)
p = ggplot(data_diamond, aes(log(price), log(carat), colour = color)) + geom_point()
# With one variable
p + facet_grid(. ~ color) + labs(title = '3-way relationship among color, carat, and price')

#histogram of price, carat, and color
par(mfrow = c(1,3))
hist(data_diamond$price, col = 'red', main = 'histogram of diamond price', xlab = 'price')
hist(data_diamond$carat, col = 'blue', main = 'histogram of diamond carat', xlab = 'carat')
hist(as.numeric(data_diamond$color), col = 'green', main = 'histogram of color', xlab = 'color' )


# correlation plot of the three variables
panel.cor = function(x, y, digits = 2, cex.cor, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r = cor(x, y)
  txt = format(c(r, 0.123456789), digits = digits)[1]
  txt = paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p = cor.test(x, y)$p.value
  txt2 = format(c(p, 0.123456789), digits = digits)[1]
  txt2 = paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 = paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(data_diamond, upper.panel = panel.cor)

par(mfrow = c(1,1))
plot(log(price)~log(carat),data_diamond,col=rainbow_hcl(7)[c(color)],pch=16, main = 'price vs carat')
legend("topleft", pch=16, col = rainbow_hcl(7),
       legend = unique(data_diamond$color))

#calculating price per carat
data_diamond$price_per_carat = data_diamond$price/data_diamond$carat
aggregate(.~color, data=data_diamond, mean)

