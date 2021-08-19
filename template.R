## load required packages
library(ggplot2)

## download NOAA storm database

if(!file.exists("./dataset")){dir.create("./dataset")}
downloadUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(downloadUrl, destfile = "./dataset/StormData.csv.bz2") 

storm_data <- read.table("./dataset/StormData.csv.bz2", sep = ",", header = TRUE, na.strings = "")

dim(storm_data)
str(storm_data)

## create new storm data with only required variables
new_storm_data <- storm_data[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

## check NA vslues in any column
apply(new_storm_data, 2, function(x) any(is.na(x)))

for(i in 1:nrow(new_storm_data)) {
        index_unit <- 0
        if(!is.na(new_storm_data$CROPDMGEXP[i])) {
                temp <- tolower(new_storm_data$CROPDMGEXP[i])
                if (grepl("h", temp) == TRUE) {
                        index_unit <- 100
                } else if (grepl("k", temp) == TRUE) {
                        index_unit <- 1000
                } else if (grepl("m", temp) == TRUE) {
                        index_unit <- 1000000
                } else if (grepl("b", temp) == TRUE) {
                        index_unit <- 1000000000
                } else if (grepl("^[+-]", temp) == TRUE) {
                        index_unit <- 0
                } else {
                        index_unit <- strtoi(temp)
                }
        }
        new_storm_data$crop_dmg_unit[i] <- index_unit
}

for(i in 1:nrow(new_storm_data)) {
        index_unit <- 0
        if(!is.na(new_storm_data$PROPDMGEXP[i])) {
                temp <- tolower(new_storm_data$PROPDMGEXP[i])
                if (grepl("h", temp) == TRUE) {
                        index_unit <- 100
                } else if (grepl("k", temp) == TRUE) {
                        index_unit <- 1000
                } else if (grepl("m", temp) == TRUE) {
                        index_unit <- 1000000
                } else if (grepl("b", temp) == TRUE) {
                        index_unit <- 1000000000
                } else if (grepl("^[+-]", temp) == TRUE) {
                        index_unit <- 0
                } else {
                        index_unit <- strtoi(temp)
                }
        }
        new_storm_data$prop_dmg_unit[i] <- index_unit
}


## Fatalities

storm_fatalities <- aggregate(FATALITIES ~ EVTYPE, data = new_storm_data, FUN = sum)
storm_fatalities <- storm_fatalities[order(-storm_fatalities$FATALITIES),]

new_storm_fatalities <- storm_fatalities[1:10, ]

new_storm_fatalities

plot1 <- ggplot(new_storm_fatalities, aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES, fill = EVTYPE)) +
        geom_bar(stat = "identity") + 
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =8)) +
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_text(face = "bold")) +
        geom_text(label = new_storm_fatalities$FATALITIES, vjust = -.3, size =3) +
        ylab("Fatalities") +
        ggtitle("Top 10 Most Harmful Storm Event Types (Fatalities Vs Injuries)") +
        scale_y_continuous(limits = c(0, 6000))

## Injuries

storm_injuries <- aggregate(INJURIES ~ EVTYPE, data = new_storm_data, FUN = sum)
storm_injuries <- storm_injuries[order(-storm_injuries$INJURIES),]

new_storm_injuries <- storm_injuries[1:10,]

new_storm_injuries

plot2 <- ggplot(new_storm_injuries, aes(x = reorder(EVTYPE, -INJURIES), y = INJURIES, fill = EVTYPE)) +
        geom_bar(stat = "identity") + 
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        theme(axis.title.x = element_text(face = "bold")) +
        theme(axis.title.y = element_text(face = "bold")) +
        geom_text(label = new_storm_injuries$INJURIES, vjust = -.3, size =3) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =8)) +
        xlab("Storm Event Type") + 
        ylab("Injuries") +
        scale_y_continuous(limits = c(0, 95000))

grid.arrange(plot1, plot2, nrow =2)

## injuries + fatalities

new_storm_data$INJU_FATAL <- new_storm_data$INJURIES + new_storm_data$FATALITIES

storm_injuries_fatalities <- aggregate(INJU_FATAL ~ EVTYPE, data = new_storm_data, FUN = sum)
storm_injuries_fatalities <- storm_injuries_fatalities[order(-storm_injuries_fatalities$INJU_FATAL),]

new_storm_injuries_fatalities <- storm_injuries_fatalities[1:10, ]

new_storm_injuries_fatalities 

ggplot(new_storm_injuries_fatalities, aes(x = reorder(EVTYPE, -INJU_FATAL), y = INJU_FATAL, fill = EVTYPE)) +
        geom_bar(stat = "identity") + 
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        theme(axis.title.x = element_text(face = "bold")) +
        theme(axis.title.y = element_text(face = "bold")) +
        geom_text(label = new_storm_injuries_fatalities$INJU_FATAL, vjust = -.3, size =3) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab("Storm Event Type") + 
        ylab("Injuries and Fatalities") +
        ggtitle("Top 10 Most Harmful Storm Event Types")

## Damage on properties
storm_prop <- aggregate((PROPDMG * prop_dmg_unit) ~ EVTYPE, data = new_storm_data, FUN = sum)
names(storm_prop) <- c("EVTYPE", "total_prop")
storm_prop <- storm_prop[order(-storm_prop$total_prop),]

new_storm_prop <- storm_prop[1:10,]

new_storm_prop

plot3 <- ggplot(new_storm_prop, aes(x = reorder(EVTYPE, -total_prop), y = total_prop, fill = EVTYPE)) +
        geom_bar(stat = "identity") + 
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =6)) +
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_text(face = "bold")) +
        geom_text(label = paste(round(new_storm_prop$total_prop/1000000000, 2), "B"), vjust = -.3, size =3) +
        ylab("Property Damage") +
        ggtitle("Top 10 Most Damaging Storm Event Types (Properties and Crops)") +
        scale_y_continuous(limits = c(0, 154657709800))

## damage on crops

storm_crop <- aggregate((PROPDMG * crop_dmg_unit) ~ EVTYPE, data = new_storm_data, FUN = sum)
names(storm_crop) <- c("EVTYPE", "total_crop")
storm_crop <- storm_crop[order(-storm_crop$total_crop),]

new_storm_crop <- storm_crop[1:10,]

new_storm_crop

plot4 <- ggplot(new_storm_crop, aes(x = reorder(EVTYPE, -total_crop), y = total_crop, fill = EVTYPE)) +
        geom_bar(stat = "identity") + 
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        theme(axis.title.x = element_text(face = "bold")) +
        theme(axis.title.y = element_text(face = "bold")) +
        geom_text(label = paste(round(new_storm_crop$total_crop/1000000000, 2), "B"), vjust = -.3, size =3) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =6)) +
        xlab("Storm Event Type") + 
        ylab("Crop Damage") +
        scale_y_continuous(limits = c(0, 530228721840))

grid.arrange(plot3, plot4, nrow =2)