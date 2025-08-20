data = mtcars
data

summary(data)

# Density Plot - MPG Distribution
plot(density(mtcars$mpg), main="Density Plot of MPG", col="blue", lwd = 2)
#conclusion:- The highest density of MPG values is around 15-25 MPG, aligning with the histogram.

# Bar Chart - Cylinders
cylinder_count <- table(data$cyl)
labels_cylinder <- paste(names(cylinder_count), "(", cylinder_count, ")")
pie(cylinder_count, labels = labels_cylinder, main = "How many Vehicles have how many Cylinders")
# Conclusion:- 7 Vehicles have 6 cylinders, 11 vehicles have 4 cylinders, 14 vehicles have 8 cylinders

# Scatter Plot - Relationship between MPG and Horsepower
hist(data$hp, data$mpg,main="MPG vs Horsepower", xlab="Horsepower", ylab="MPG", col="blue", pch=19)
#conclusion :- As horsepower increases, fuel efficiency (MPG) decreases, suggesting that more powerful cars consume more fuel.

# Pie Chart - Gears
gear_counts <- table(data$gear)
labels_gear <- paste(names(gear_counts), "(", gear_counts, ")")
pie(gear_counts, labels = labels_gear, main = "How many Vehicles have how many Gears")
# Conclusion:- There are 15 vehicles that have 3 gears, 12 Vehicles that have 4 gears, 5 Vehicles that have 5 gears

# Pie Chart - Number of Vehicles with V shaped Engine and Straight engine
vs_counts <- table(data$vs)
labels_vs <- paste(c('Straight Engine', 'V-Shaped Engine'), "(", vs_counts, ")")
pie(vs_counts, labels = labels_vs, main = "Types of Engine in cars")
# Conclusion:- There are 18 vehicles with Straight Engine and 14 Engines with V-Shaped Engine

# Pie Chart - Number of Vehicles with Automatic and Manual Transmission
am_counts <- table(data$am)
labels_am <- paste(c('Automatic Transmission', 'Manual Transmission'), "(", am_counts, ")")
pie(am_counts, labels = labels_am, main = "Type of transmission in car")
# Conclusion:- There are 19 vehicles with Automatic transmission and 13 Vehicles with Manual Transmission

# Bar Chart - Number of Carburetors
barplot(table(data$carb))
# Conclusion:- Most numbers of cars have 2 or 4 carburetors
