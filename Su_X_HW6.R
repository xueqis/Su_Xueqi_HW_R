library(ggplot2)
data(diamonds)
attach(diamonds)

#2. 

ggplot(diamonds, aes(x = carat, y = price))+geom_point(aes(colour = color))+ggtitle("Diamonds-Weight to Price by Color")+theme(plot.title=element_text(color="blue"))
#use aes() to indicate that carat is on x-axis, and price is on y-axis
#use geom_point() to plot points, and set color inside geom_point
#use ggtitle() to name the plot, and apply theme() to change the 
#color of the title of the plot.

#3

ggplot(diamonds, aes(x = log(carat), y = log(price)))+geom_point(aes(colour = factor(color)))+ggtitle("Diamonds-Weight to Price by Color(Linear)")+labs(x = "Weight", y = "Price")+theme(plot.title=element_text(color="blue"))
#use aes() to indicate that x-axis and y-axis.
#use geom_point() to plot points, and set color inside geom_point
#use ggtitle() to name the plot, and use labs() to name the x,y axis.
#apply theme() to change the color of the title of the plot.

#4
price_resid = summary(lm(log(price)~log(carat)))$resid
#take out the residual out of the linear regression model
newdata = data.frame(log(carat), price_resid)
#make our new dataset which includes log(carat) and the residual.
ggplot(newdata, aes(x = log(carat), y = price_resid))+geom_point(aes(colour = factor(color)))+ggtitle("Diamonds-Weight to Price by Color")+labs(x = "Weight", y = "Price Residuals")+theme(plot.title=element_text(color="blue"), legend.position="top")
#use aes() to indicate that x-axis and y-axis.
#use geom_point() to plot points, and set color inside geom_point
#use ggtitle() to name the plot, and use labs() to name the x,y axis. 
#apply theme() to change the color of the title of the plot.
#apply the legend.position() insisde theme() to change the position
#of the legend, here, we change the position to the top.

#5
library(grid)
#use the library of grid.
ggplot(newdata, aes(x = log(carat), y = price_resid))+geom_point(aes(colour = factor(color)))+ggtitle("Diamonds-Weight to Price by Color")+labs(x = "Weight", y = "Price Residuals")+theme(plot.title=element_text(color="blue"), legend.position="top")+guides(col = guide_legend(nrow = 1))
#use new data here, and use aes() to indicate that x-axis and y-axis.
#use geom_point() to plot points, and set color inside geom_point
#use ggtitle() to name the plot, and use labs() to name the x,y axis. 
#apply theme() to change the color of the title of the plot.
#apply the legend.position() insisde theme() to change the position
#of the legend, here, we change the position to the top.
#use the col = guide_legend(nrow = 1) inside guides() to 
#order the factors of legend as one row.

leftview = viewport(x=0.315, y = 0.22, width = 0.46, height = 0.2)
#use viewport() to set the position of the plot on the bottom left

leftplot = ggplot(diamonds, aes(x = price))+geom_histogram(aes(y =..density.., fill = color), binwidth = 400)+theme(legend.position="none", axis.title=element_blank(), plot.margin = unit(c(0, 0, 0, 0),"cm"))

#plot the plot on the bottom left, and name it as "leftplot"
#use aes() to indicate that price is on the x-axis.
#use geom_histgram() to plot a histgram, and use aes() inside to
#indicate that we want to plot a density histogram of price
#use color to fill the histgram, and let binwidth = 400
#apply legend.position = "none" in theme() to remove the legend
#apply axis.title = element_blank() to remove the names of axis.
#set the plot.margin as 0, 0, 0, 0 to remove the margin of the plot.

rightview = viewport(x = 0.79, y = 0.65, width = 0.4, height = 0.2)
#use viewport() to set the position of the plot on the upper right
rightplot = ggplot(diamonds, aes(x = carat))+geom_histogram(aes(y = ..density.., col = color), binwidth = 0.03)+theme(legend.position="none", axis.title=element_blank(), plot.margin = unit(c(0, 0, 0, 0),"cm"))
#plot the plot on the upper right, and name it as "rightplot"
#use aes() to indicate that carat is on the x-axis.
#use geom_histgram() to plot a histgram, and use aes() inside to
#indicate that we want to plot a density histogram of carat
#use color to fill the histgram, and let binwidth = 0.03
#apply legend.position = "none" in theme() to remove the legend
#apply axis.title = element_blank() to remove the names of axis.
#set the plot.margin as 0, 0, 0, 0 to remove the margin of the plot.

print(leftplot, vp = leftview)
#print the leftplot on top of main plot
print(rightplot, vp = rightview)
#print the rightplot on top of main plot.





