#import utilities.df
utilities.df <- read.csv("Utilities.csv")
View(utilities.df)
# set row names in utilities.df to values stored in the Company name column
row.names(utilities.df) <- utilities.df[,1]
View(utilities.df)
# remove the Company name column
utilities.df <- utilities.df[,-1]
View(utilities.df)

#get the first seven rows of the data frame to illustrate calculations
utilities.df.first <- utilities.df[1:7,]

# compute Euclidean distance using all seven variables
d.all <- dist(utilities.df.first, method = "euclidean")
print(d.all)

#compute Euclidean distance using only Sales and Fuel_Cost
d.sales.fuel <- dist(utilities.df.first[,c(6,8)], method = "euclidean")
print(d.sales.fuel)

# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)

View(utilities.df.norm)
# set row names in utilities.df.norm to the company names column of utilities.df
row.names(utilities.df.norm) <- row.names(utilities.df)
View(utilities.df.norm)


# compute normalized distance based on variables Sales and FuelCost for the first seven rows
d.norm.first <- utilities.df.norm[1:7,]
d.norm.sales.fuel <- dist(d.norm.first[,c(6,8)], method = "euclidean")
print(d.norm.sales.fuel)

# compute normalized distance based on all seven variables
d.norm <- dist(d.norm.first, method = "euclidean")
print(d.norm)


# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
km$cluster
cluster.df <- data.frame(km$cluster)
View(cluster.df)

# centroids
km$centers

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df.norm))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))


#distance between centers
dist(km$centers)


