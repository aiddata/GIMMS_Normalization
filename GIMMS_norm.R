#Method to normalize GIMMS historic record - KFW
library(GISTools)
library(reshape)

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
for (i in 2:34)
{
  splt <- strsplit(colnames(HistGIMMS_df)[i],"_")
  splt[[1]][1] <- sub("X","",splt[[1]][1])
  day = 1
  month = splt[[1]][2]
  year = splt[[1]][1]
  dt = paste(year,"-",month,"-",day,sep="")
  colnames(HistGIMMS_df)[i] <- dt
}
  
#Contemporary GIMMS ---------------------------------------
ContGIMMS <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/ndvi/ndvi_extract_merge.csv"
ContGIMMS <- read.csv(ContGIMMS)

#Copy only relevant dates of overlap into a new DF
ContGIMMS_df <- ContGIMMS[,c(1,3:132)]
for (i in 2:34)
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

#Add a month column for the merge
ContGIMMS_ts["month"] <- NA
ContGIMMS_ts["year"] <- NA
for (i in 1:nrow(ContGIMMS_ts))
{
  strspl <- strsplit(toString(ContGIMMS_ts[1,]["variable"][[1]]),"-")
  ContGIMMS_ts[i,]["month"] <- strspl[[1]][2]
  ContGIMMS_ts[i,]["year"] <- strspl[[1]][1]
  print(i)
}
#Merge by Month


#Merge it in
#kfw.SPDF <- merge(cln_Shp, HistGIMMS, by.x="id", by.y="id")
