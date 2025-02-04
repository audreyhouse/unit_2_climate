# AH 2025-01-30
# Count the number of lines in the HDR of the txt file 
# read in data

ant_ice_loss = read.table(file="data/antarctica_mass_200204_202408.txt", skip=31, sep="", header=FALSE, 
           col.names=c("decimal_date", "mass_Gt", "sigma_Gt")) #Antarctica ice mass loss
head(ant_ice_loss) #Shows the first six rows of a data frame
tail(ant_ice_loss) #Shows the last six rows
dim(ant_ice_loss) #Shows the dimensions of a data frame

grn_ice_loss = read.table(file="data/greenland_mass_200204_202408.txt", skip=31, sep="", header=FALSE, 
                          col.names=c("decimal_date", "mass_Gt", "sigma_Gt")) #Greenland ice mass loss
head(grn_ice_loss)
tail(grn_ice_loss)
dim(grn_ice_loss)[1]
summary(grn_ice_loss) #Summary info for data frame each column, Min, Max, Mean, Median, Qu.

plot(x=ant_ice_loss$decimal_date, y=ant_ice_loss$mass_Gt, data=ant_ice_loss)
plot(mass_Gt ~ decimal_date, data=ant_ice_loss) # y as a formula of (~) x
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, type="l")
plot(mass_Gt ~ decimal_date, data=grn_ice_loss, type="l") #l=line

plot(mass_Gt ~ decimal_date, data=ant_ice_loss, type="l")
lines(mass_Gt ~ decimal_date, data=grn_ice_loss, type="l", col="red") #plot window is sized based off of ant data, and grn goes out of that window 

plot(mass_Gt ~ decimal_date, data=ant_ice_loss, type="l", ylim=c(-5000,0)) #expand limit of plot
lines(mass_Gt ~ decimal_date, data=grn_ice_loss, type="l", col="red")

range(grn_ice_loss$mass_Gt)

plot(mass_Gt ~ decimal_date, data=ant_ice_loss, type="l", ylim=range(grn_ice_loss$mass_Gt))
lines(mass_Gt ~ decimal_date, data=grn_ice_loss, type="l", col="red")

plot(mass_Gt ~ decimal_date, data=ant_ice_loss, type="l",
     ylim=range(grn_ice_loss$mass_Gt),
     ylab = "Mass loss (Gt)",
     xlab= "Year") # Adding labels to the plot
lines(mass_Gt ~ decimal_date, data=grn_ice_loss, type="l", col="red") # Slight problem, R filled in June 2017-2018 when there isn't data there

# Creating a row of data to insert into existing data frames- exactly mimic column names

data_break = data.frame(decimal_date=2018.0, mass_Gt=NA, sigma_Gt=NA)
data_break

#Adding fake data point to data, creating new data set to keep a copy of the original, binding rows of two data frames

ant_ice_loss_with_NA = rbind(ant_ice_loss, data_break)
head(ant_ice_loss_with_NA)
tail(ant_ice_loss_with_NA)

plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, type="l",
     ylim=range(grn_ice_loss$mass_Gt),
     ylab = "Mass loss (Gt)",
     xlab= "Year") # Doesn't create the gap beacause it does the line in order, must insert the data point in the right spot of the time series- requires sorting

ordered_rows = order(ant_ice_loss_with_NA$decimal_date)

ant_ice_loss_with_NA = ant_ice_loss_with_NA[ordered_rows, ] # Way to get every row is to not clarify rows, this is permanenetly making the NA data set ordered correctly

ant_ice_loss_with_NA = ant_ice_loss_with_NA[ordered_rows, ]

plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, type="l",
     ylim=range(grn_ice_loss$mass_Gt),
     ylab = "Mass loss (Gt)",
     xlab= "Year")

grn_ice_loss_with_NA = rbind(grn_ice_loss, data_break)

grn_ice_loss_with_NA = grn_ice_loss_with_NA[order(grn_ice_loss_with_NA$decimal_date), ]

plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, type="l",
     ylim=range(grn_ice_loss$mass_Gt),
     ylab = "Mass loss (Gt)",
     xlab= "Year") 
lines(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, type="l", col="red")


# Adding uncertainty
head(ant_ice_loss)


pdf("figures/my_first_figure.pdf", width=7, height=5) 
plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, type="l",
     ylim=range(grn_ice_loss$mass_Gt),
     ylab = "Mass loss (Gt)",
     xlab= "Year", lwd=2) # Line width
lines((mass_Gt + 2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, lty="dashed") # lty making the line dashed instead of solid
lines((mass_Gt - 2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, lty="dashed") # lty making the line dashed instead of solid
lines(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, type="l", lwd=2, col="red")
lines((mass_Gt + 2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, lty="dashed", col="red") 
lines((mass_Gt - 2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, lty="dashed", col="red")
dev.off #Closes the pdf opened above to make it accessible

min(ant_ice_loss$mass_Gt)
barplot(height=c(min(ant_ice_loss$mass_Gt), min(grn_ice_loss$mass_Gt)))
barplot(height=c(min(ant_ice_loss$mass_Gt)*(-1), min(grn_ice_loss$mass_Gt)*(-1)), names.arg=c("Antarctica","Greenland"), ylim=c(0,6000), ylab="Ice loss in Gt") #Flip to negative to positive, add x-axis labels, add more tick marks on y-axis, add y-axis title

