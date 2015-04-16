#Method to normalize GIMMS historic record - KFW
library(GISTools)
library(reshape)
library(splitstackshape)
library(ggplot2)

#File for the KFW analysis
shpfile = "Input_Data/KFW/Matched_Indigenous_Lands_id.shp"
src_Shp = readShapePoly(shpfile)

#Clean the source Shapefile to remove extra columns of data.
cln_Shp <- src_Shp[,c("terrai_nom","terrai_are","reu_id","id")]

#Load in the data to join to the shapefile
#======================================================
#Historic GIMMS -------------------------------------------
HistGIMMS <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/historic_ndvi/historic_ndvi_extract_merge.csv"
HistGIMMS <- read.csv(HistGIMMS)
#Copy only relevant columns of overlap...
HistGIMMS_df <- HistGIMMS[,c(1,226:259)]

#Rename the columns for easier interpretation later...
for (i in 2:35)
{
  splt <- strsplit(colnames(HistGIMMS_df)[i],"_")
  splt[[1]][1] <- sub("X","",splt[[1]][1])
  month = splt[[1]][2]
  year = splt[[1]][1]
  dt = paste(year,"-",month,sep="")
  colnames(HistGIMMS_df)[i] <- dt
}

HistGIMMS_ts <- melt(HistGIMMS_df,id="id")
HistGIMMS_ts <- cSplit(HistGIMMS_ts, "variable", "-")

colnames(HistGIMMS_ts)[1] <- "id"
colnames(HistGIMMS_ts)[2] <- "NDVI_AVH"
colnames(HistGIMMS_ts)[3] <- "Year"
colnames(HistGIMMS_ts)[4] <- "Month"

  
#Contemporary GIMMS ---------------------------------------
ContGIMMS <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/ndvi/ndvi_extract_merge.csv"
ContGIMMS <- read.csv(ContGIMMS)

#Copy only relevant dates of overlap into a new DF
ContGIMMS_df <- ContGIMMS[,c(1,3:132)]
for (i in 2:131)
{
  splt <- strsplit(colnames(ContGIMMS_df)[i],"_")
  splt[[1]][1] <- sub("X","",splt[[1]][1])
  day = splt[[1]][2]
  year = splt[[1]][1]
  month = strsplit(toString(as.Date((as.numeric(day)-1),origin=paste("1-1-",toString(year),sep=""))),"-")[[1]][2]
  day = strsplit(toString(as.Date((as.numeric(day)-1),origin=paste("1-1-",toString(year),sep=""))),"-")[[1]][3]
  dt = paste(year,"-",month,"-",day,sep="")
  colnames(ContGIMMS_df)[i] <- dt
}

#Reshape the ContGIMMS_df into a long form to enable monthly averaging.
ContGIMMS_ts <- melt(ContGIMMS_df,id="id")
#Add a month and year column for later merging...
ContGIMMS_ts <- cSplit(ContGIMMS_ts, "variable", "-")

#Aggregate and rename columns for use
ContGIMMS_ts <- aggregate(value ~ variable_1 + variable_2 + id, ContGIMMS_ts, FUN=mean)
colnames(ContGIMMS_ts)[1] <- "Year"
colnames(ContGIMMS_ts)[2] <- "Month"
colnames(ContGIMMS_ts)[3] <- "id"
colnames(ContGIMMS_ts)[4] <- "NDVI_MOD"

HC_GIMMS <- merge(HistGIMMS_ts, ContGIMMS_ts, by=c("id","Year","Month"))

HC_GIMMS[HC_GIMMS == 0] <- NA
plot(HC_GIMMS$NDVI_AVH, HC_GIMMS$NDVI_MOD)

abline(lm(NDVI_MOD ~ NDVI_AVH, data=HC_GIMMS), col='red')

FElm <- lm(NDVI_MOD ~ NDVI_AVH + factor(id), data=HC_GIMMS)

abline(FElm, col="blue")

ggplot() + geom_point(aes(x=Month,y=NDVI_MOD), data=HC_GIMMS)

#Merge it in
#kfw.SPDF <- merge(cln_Shp, HistGIMMS, by.x="id", by.y="id")
