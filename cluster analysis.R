## Load libraries ----
library(reshape2)
library(dplyr)
library(skmeans)
library(fpc)

## Import order data from csv file ----
orders <- read.csv("~/Desktop/export_for_federico_1percent_sampling.csv")
orders$INDIV_ID <- as.factor(orders$INDIV_ID) # converts customers ID to factor
orders$DATE <- as.Date(orders$DATE, "%d/%m/%y") # converts to Date format

## Product type classification ----
orders[grep("SOCK", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "SOCKS"
orders[grep("SHOE|BOOT|SLIDE", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "SHOE"
orders[grep("SHORTS", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "SHORTS"
orders[grep("TROUSERS|SWEATPANTS|PANTS", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "TROUSERS"
orders[grep("SHIRT|TOP||HOODIE|TANK.*TOP|JERSEY", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "TOP"
orders[grep("BRA", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "BRA"
orders[grep("TIGHTS|LEGGING|CAPRIS|CROPS", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "TIGHTS"
orders[grep("JACKET|CAPE", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "JACKET"
orders[grep("SWEATER|SWEATSHIRT|PARKA", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "HEAVY_TOP"
orders[grep("KIT|HAT|BAG|FUELBAND|GIFT.*CARD|SPIKES|ARMBAND|HEADBAND|GLOVE|SENSOR|STRAPS|STUDS|BOTTLE|BACKPACK|SACK|GUARDS|SHIELDS|VEST|BASKETBALL.*SLEEVE", orders$DISPLAY_NAME, ignore.case = TRUE), "type"] <- "OTHER"
orders[is.na(orders$type), "type"] <- "UNCLASSIFIED"
orders$type <- as.factor(orders$type)
## ---

sum(orders$DEMAND) # total Demand

## STYLE based clustering ----
## create array with indivID and STYLES
indiv_orders <- orders %>%
  filter(STYLE != "VAS0020") %>%
  filter(CAL_YEAR == "2015", CAL_MONTH_DESC == "JULY", DAY_OF_WEEK_DESC == "SUNDAY") %>% # temp filter to trim down data
  select(INDIV_ID, STYLE, UNITS) %>%
  group_by(INDIV_ID, STYLE) %>% # aggregate multiple occurrencies of the same INDIV_ID and STYLE
  summarize(UNITS = sum(UNITS))

#length(unique(indiv_orders$STYLE)) # how many unique styles = 8235
#length(unique(indiv_orders$INDIV_ID)) # how many unique indiv_ID = 34193

## OPTIONAL - simplify and trim down data: remove styles with only 1 occurrence
more_styles <- indiv_orders %>%
  group_by(STYLE) %>%
  summarize(OCCURRENCIES = n()) %>%
  filter(OCCURRENCIES > 1) %>%
  select(STYLE)
indiv_orders <- indiv_orders %>%
  filter(STYLE %in% more_styles$STYLE)

## cast into a matrix INDIV_ID by STYLE combinations
indiv_orders.m <- acast(indiv_orders, INDIV_ID ~ STYLE, value.var = "UNITS")
indiv_orders.m[is.na(indiv_orders.m)] <- 0 # replace NAs with 0
## ---

## Multiple product dimensions based clustering ----
## create data frame with indivID and product dimensions, and add up multiple units
indiv_orders <- orders %>%
  filter(STYLE != "VAS0020") %>%
  select(INDIV_ID, IL.ID, FP.CL, CATEGORY, DIVISION, GENDER, type, UNITS) %>%
  group_by(INDIV_ID, IL.ID, FP.CL, CATEGORY, DIVISION, GENDER, type) %>%
  summarize(UNITS = sum(UNITS))

length(unique(indiv_orders$INDIV_ID)) # how many unique indiv_ID = 34193

## (OPTIONAL) transform in numeric and scale variables
indiv_orders$IL.ID <- scale(as.numeric(indiv_orders$IL.ID))
indiv_orders$FP.CL <- scale(as.numeric(indiv_orders$FP.CL))
indiv_orders$CATEGORY <- scale(as.numeric(indiv_orders$CATEGORY))
indiv_orders$DIVISION <- scale(as.numeric(indiv_orders$DIVISION))
indiv_orders$GENDER <- scale(as.numeric(indiv_orders$GENDER))
indiv_orders$type <- scale(as.numeric(indiv_orders$type))

## cast into a matrix INDIV_ID by product dimensions

# ver.1 all combination (BIG! matrix: 34193 x 383)
indiv_orders.m <- acast(indiv_orders, INDIV_ID ~ IL.ID + FP.CL + CATEGORY + DIVISION + GENDER + type, value.var = "UNITS")
# ver.2 some combinations (mid matrix: 34193 x 88)
indiv_orders.m <- acast(indiv_orders, INDIV_ID ~ CATEGORY + DIVISION + GENDER, fun.aggregate = sum, value.var = "UNITS")
# ver.3 no combinations, all vectors (small matrix: )
#### NEED TO BE BINARIES
indiv_orders.m <- indiv_orders %>%
  group_by(INDIV_ID) %>%
  summarize(UNITS = sum(UNITS))

indiv_orders.m[is.na(indiv_orders.m)] <- 0 # replace NAs with 0

## Clustering algorithms ----

## generate the clusters and optimize k with k-means
clustering.k <- kmeansruns(indiv_orders.m, krange = 1:10, criterion = "asw") # too heavy!!

## generate the clusters with basic k-means
indiv.k <- list()
for (i in 2:15) {
  cat("Generating k = ", i, " clusters \n", sep = "")
  indiv.k[[i]] <- kmeans(indiv_orders.m, i)
}
rm(i)
indiv.k

## generate the clusters with spherical k-means
indiv.sk <- list()
for (i in 2:15) {
  cat("Generating k = ", i, " clusters \n", sep = "")
  indiv.sk[[i]] <- skmeans(indiv_orders.m, i, method = "genetic", control = list(verbose = F))
}
rm(i)
indiv.sk

## Extract results ----
# extract cluster structure for the list object
indiv_cluster <- data.frame(INDIV_ID = attr(indiv.k$cluster, "names"), CLUSTER = indiv.k$cluster) # kmeans

values <- data.frame(k = 1:15, values = NA)
indiv_cluster <- data.frame(INDIV_ID = row.names(as.data.frame(indiv.sk[[2]]["cluster"])))
for (i in 2:15) { # skmeans
  # print(paste("For k =", i, "value =", indiv.sk[[i]]["value"]), quote = F)
  indiv_cluster[, i] <- indiv.sk[[i]]["cluster"]
  indiv_cluster[, i] <- as.factor(indiv_cluster[, i])
  values[i,"values"] <- indiv.sk[[i]]["value"]
}
colnames(indiv_cluster) <- c("INDIV_ID", paste("k", seq(from=2, to=15), sep = ""))
plot(x= values$k, y= values$values)

## merge cluster information to orders data frame for analysis
orders_cluster <- merge(orders, indiv_cluster, by = "INDIV_ID", all.x = TRUE)
write.csv(orders_cluster, file = "skmeans_clusters.csv")
write.csv(values, file = "values.csv")
