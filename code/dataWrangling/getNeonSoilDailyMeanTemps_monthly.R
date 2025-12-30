# Example script of how to download and calculate daily mean temps at sites.
# not fully reproducible, but the concept is there to adapt for your own use
#GJR 4/1/2025
#Download soil temp data (30 minute) for all NEON sites in the beetle data
#calculate daily means for the ~5 and ~15cm probes, discard the finer reso data
#requires the list object of beetle data for all sites from neonUtilities 'loadByProduct'

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library('neonUtilities')

#function to calculate mean depth (zoffset in cm) for the designated probes (here 502 and 503)
# probe is the probe number code, sensorPositions is the sensor position data frame from the downloaded NEON list
getProbeDepths<-function(probe,sensorPositions) {
  ind<-grepl(probe,sensorPositions$HOR.VER)
  meanOffset<-mean(sensorPositions$zOffset[ind])
}


setwd('/media/raglandlab/ExtraDrive1/neonData/groundBeetle')
load(file = "allNeonBeetleDataFeb25.RData")

#get data frame, one row per site, including site lat/lon
#here, just using the unique site names for the loop below
fieldDat<-beetlesAll$bet_fielddata
fieldDat<- fieldDat %>% distinct(siteID,.keep_all=T)
sites<-fieldDat$siteID
#Sites to excluce
# no soil temperatures data for PUUM, 
# four Alaskan sites experience soil heave and thus unreliable sensor positions: HEAL, BONA, TOOL, and BARR
# should exclude non-continental sites, soil temperature is unlikely to dictate seasonality: GUAN, LAJA
exclude<-c('PUUM',"HEAL","BONA","TOOL","BARR","GUAN","LAJA","DEJU")

sites<-sites[!(sites %in% exclude)]

i=1
for (site in sites) {
  
  #download per site 30 minute data
  soilTemps30min <- loadByProduct(dpID="DP1.00041.001", 
                                  site = site,
                                  package = "basic",
                                  include.provisional=T,
                                  tabl = 'ST_30_minute',
                                  check.size = F,
                                  token='eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJncmFnbGFuZHVjZGVudmVyQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkwMDg5NTkwMiwiaWF0IjoxNzQzMjE1OTAyLCJlbWFpbCI6ImdyYWdsYW5kdWNkZW52ZXJAZ21haWwuY29tIn0.8JLlRhodiyqhmWozmYYJIh1Sm4jZu2_vK9NCrfwMrHLF9LChju4SQt-dGBZUXrVZgg-7X7TX5UChe8WPb5NOkg',
                                  nCores = 4)
  
  #reduce to only 2nd and 3rd vertical positions (coded 502 and 503) that should correspond to 5 and 15 cm soil depth
  ind<-(soilTemps30min$ST_30_minute$verticalPosition == 502 | soilTemps30min$ST_30_minute$verticalPosition == 503) & soilTemps30min$ST_30_minute$finalQF == 0
  #exclude sensor array 001 from TREE, see issue log
  if (site=="TREE") {
    ind<-ind & soilTemps30min$ST_30_minute$horizontalPosition != "001"
  }
  soilTemps30min$ST_30_minute <- soilTemps30min$ST_30_minute[ind,]
  #converts date-time to just date; need to add one second because ymd_hms fails to parse midnight times (00:00:00)
  soilTemps30min$ST_30_minute$date<-as.Date(ymd_hms(soilTemps30min$ST_30_minute$endDateTime + seconds(1)))
  soilTemps30min$ST_30_minute$month<-month(soilTemps30min$ST_30_minute$date)
  
  
  #calculate daily means for each retained probe depth
  out<-data.frame(soilTemps30min$ST_30_minute %>% 
                    #group_by(date, verticalPosition) %>%
                    group_by()
                    summarise(meanTemp = mean(soilTempMean, na.rm = TRUE) ) )
  out$site<-site
  
  #get mean depth of probes across the multipe (xy coord) instruments at each site
  depths<-c(
    getProbeDepths('502',soilTemps30min$sensor_positions_00041),
    getProbeDepths('503',soilTemps30min$sensor_positions_00041)
  )
  outMeta<-data.frame(site=site,probeDepths=depths)
  
  #output data frames
  if (i==1) {
    dailySoilTempMeansAll<-out
    soilTempProbeDepths<-outMeta
  } else {
    dailySoilTempMeansAll<-rbind(dailySoilTempMeansAll,out)
    soilTempProbeDepths<-rbind(soilTempProbeDepths,outMeta)
  }
  i=i+1
  
  #clean up
  rm(soilTemps30min)
  gc()
}

write.table(dailySoilTempMeansAll,
            "/media/raglandlab/ExtraDrive1/neonData/groundBeetle/soilTempData/dailySoilMeansAll.csv",
            sep=",",
            quote=F,
            row.names=F)

write.table(soilTempProbeDepths,
            "/media/raglandlab/ExtraDrive1/neonData/groundBeetle/soilTempData/soilTempProbeDepths.csv",
            sep=",",
            quote=F,
            row.names=F)

dailySoilMeansAll<-read.csv("/media/raglandlab/ExtraDrive1/neonData/groundBeetle/soilTempData/dailySoilMeansAll.csv")
dailySoilMeansAll$verticalPosition<-as.factor(dailySoilMeansAll$verticalPosition)
dailySoilMeansAll$date<-ymd(dailySoilMeansAll$date)

dailySoilMeansAll$month<-month(dailySoilMeansAll$date)
dailySoilMeansAll$year<-year(dailySoilMeansAll$date)

monthlySoilTemps<-dailySoilMeansAll %>% 
  #group_by(date, verticalPosition) %>%
  group_by(site,month,year) %>%
  summarise(montlyMeanTemp = mean(meanTemp, na.rm = TRUE) ) 

write.table(monthlySoilTemps,
            "/media/raglandlab/ExtraDrive1/neonData/groundBeetle/soilTempData/monthlySoilMeansAll.csv",
            sep=",",
            quote=F,
            row.names=F)



#plot data from all sites
ggplot(dailySoilMeansAll,aes(x=date,y=meanTemp,color=verticalPosition,group=verticalPosition)) + geom_line() +
  facet_wrap(vars(site))

#JORN looks like it's missing data from the deeper sensor (503) from sometime in 2020 to 2021
# might need to exclude that time frame, or just use the shallower sensor (502)
ind<-dailySoilMeansAll$site=="JORN"
ggplot(dailySoilMeansAll[ind,],aes(x=date,y=meanTemp,color=verticalPosition,group=verticalPosition)) + geom_line()

ind<-dailySoilMeansAll$site=="ABBY"
ggplot(dailySoilMeansAll[ind,],aes(x=date,y=meanTemp,color=verticalPosition,group=verticalPosition)) + geom_line()



#get data for minimum interval (one month) to check the sensor positions file
getSensorPos<-function(site) {
  soilTemps30min <- loadByProduct(dpID="DP1.00041.001", 
                                  site = site,
                                  package = "basic",
                                  include.provisional=T,
                                  tabl = 'ST_30_minute',
                                  check.size = F,
                                  token='eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJncmFnbGFuZHVjZGVudmVyQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkwMDg5NTkwMiwiaWF0IjoxNzQzMjE1OTAyLCJlbWFpbCI6ImdyYWdsYW5kdWNkZW52ZXJAZ21haWwuY29tIn0.8JLlRhodiyqhmWozmYYJIh1Sm4jZu2_vK9NCrfwMrHLF9LChju4SQt-dGBZUXrVZgg-7X7TX5UChe8WPb5NOkg',
                                  nCores = 4,
                                  startdate="2020-05", 
                                  enddate="2020-06")
}

#BART
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("BART")
sensorPos<-soilTemps30min$sensor_positions_00041

#HARV
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("HARV")
sensorPos<-soilTemps30min$sensor_positions_00041

#BLAN
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("BLAN")
sensorPos<-soilTemps30min$sensor_positions_00041

#SCBI
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("SCBI")
sensorPos<-soilTemps30min$sensor_positions_00041

#SERC
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("SERC")
sensorPos<-soilTemps30min$sensor_positions_00041

#DSNY
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("DSNY")
sensorPos<-soilTemps30min$sensor_positions_00041

#JERC
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("JERC")
sensorPos<-soilTemps30min$sensor_positions_00041

#OSBS
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("OSBS")
sensorPos<-soilTemps30min$sensor_positions_00041

#STEI
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("STEI")
sensorPos<-soilTemps30min$sensor_positions_00041

#TREE
#Starts 2010
#Sensor 001 repositioned in 2018, exclude 001 from daily mean estimates
# all other sensor positions static and at expected depths
soilTemps30min<-getSensorPos("TREE")
sensorPos<-soilTemps30min$sensor_positions_00041

#UNDE
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("UNDE")
sensorPos<-soilTemps30min$sensor_positions_00041

#KONA
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("KONA")
sensorPos<-soilTemps30min$sensor_positions_00041

#KONZ
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("KONZ")
sensorPos<-soilTemps30min$sensor_positions_00041

#UKFS
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("UKFS")
sensorPos<-soilTemps30min$sensor_positions_00041

#GRSM
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("GRSM")
sensorPos<-soilTemps30min$sensor_positions_00041

#MLBS
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("MLBS")
sensorPos<-soilTemps30min$sensor_positions_00041

#ORNL
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("ORNL")
sensorPos<-soilTemps30min$sensor_positions_00041

#DELA
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("DELA")
sensorPos<-soilTemps30min$sensor_positions_00041

#LENO
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("LENO")
sensorPos<-soilTemps30min$sensor_positions_00041

#TALL
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("TALL")
sensorPos<-soilTemps30min$sensor_positions_00041

#DCFS
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("DCFS")
sensorPos<-soilTemps30min$sensor_positions_00041

#NOGP
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("NOGP")
sensorPos<-soilTemps30min$sensor_positions_00041

#WOOD
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("WOOD")
sensorPos<-soilTemps30min$sensor_positions_00041

#CPER
#Starts 2010
#ISSUE: Two different soil plot reference corner records were created for each CPER soil plot, which resulted in two partially different 
#sets of sensor location data being reported in the sensor_positions file. Affected variables were referenceLatitude, referenceLongitude, 
#referenceElevation, eastOffset, northOffset, xAzimuth, and yAzimuth. Other sensor location metadata, including sensor height/depth (zOffset),
#were unaffected but were still reported twice for each sensor.
#So, soil depth is consistent, can still use for averaging soil temps
soilTemps30min<-getSensorPos("CPER")
sensorPos<-soilTemps30min$sensor_positions_00041

#RMNP
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("RMNP")
sensorPos<-soilTemps30min$sensor_positions_00041

#STER
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("STER")
sensorPos<-soilTemps30min$sensor_positions_00041

#CLBJ
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("CLBJ")
sensorPos<-soilTemps30min$sensor_positions_00041

#OAES
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("OAES")
sensorPos<-soilTemps30min$sensor_positions_00041

#YELL
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("YELL")
sensorPos<-soilTemps30min$sensor_positions_00041

#MOAB
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("MOAB")
sensorPos<-soilTemps30min$sensor_positions_00041

#NIWO
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("NIWO")
sensorPos<-soilTemps30min$sensor_positions_00041

#JORN
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("JORN")
sensorPos<-soilTemps30min$sensor_positions_00041

#SRER
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("SRER")
sensorPos<-soilTemps30min$sensor_positions_00041

#ONAQ
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("ONAQ")
sensorPos<-soilTemps30min$sensor_positions_00041

#ABBY
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("ABBY")
sensorPos<-soilTemps30min$sensor_positions_00041

#WREF
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("WREF")
sensorPos<-soilTemps30min$sensor_positions_00041

#SJER
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("SJER")
sensorPos<-soilTemps30min$sensor_positions_00041

#SOAP
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("SOAP")
sensorPos<-soilTemps30min$sensor_positions_00041

#TEAK
#Starts 2010,Sensor positions static and at expected depths
soilTemps30min<-getSensorPos("TEAK")
sensorPos<-soilTemps30min$sensor_positions_00041

############ EXCLUDE ###################
#DEJU
#all sorts of sensos/flagging issues
soilTemps30min<-getSensorPos("DEJU")
sensorPos<-soilTemps30min$sensor_positions_00041

site="BART"
soilTemps30min <- loadByProduct(dpID="DP1.00041.001", 
                                site = site,
                                package = "basic",
                                include.provisional=T,
                                tabl = 'ST_30_minute',
                                check.size = F,
                                token='eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJncmFnbGFuZHVjZGVudmVyQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkwMDg5NTkwMiwiaWF0IjoxNzQzMjE1OTAyLCJlbWFpbCI6ImdyYWdsYW5kdWNkZW52ZXJAZ21haWwuY29tIn0.8JLlRhodiyqhmWozmYYJIh1Sm4jZu2_vK9NCrfwMrHLF9LChju4SQt-dGBZUXrVZgg-7X7TX5UChe8WPb5NOkg',
                                nCores = 4,
                                startdate="2020-05", 
                                enddate="2020-06")
sensorPos<-soilTemps30min$sensor_positions_00041



ggplot(b,aes(x=date,y=meanTemp,color=verticalPosition)) + geom_line()

