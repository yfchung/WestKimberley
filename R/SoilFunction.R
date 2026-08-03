########################################################################################
###   This script is provided by the Soil & Landscape Grid of Australia to           ###
###   demonstrate accessing the soil and landscape data sets using OGC web services  ###
###   OGC web services.                                                              ###
###                                                                                  ###
###   URL for Soil and Landscape Grid is www.csiro.au/soil-and-landscape-grid        ###
###                                                                                  ###
###   Author : Ross Searle - email: ross.searle@csiro.au                             ###
###                                                                                  ###
########################################################################################


library(RCurl)
library(rgdal)
library(raster)
library(sp)
library(XML)
library(maptools)
library(hash)
library(plyr)
library(GSIF)
library(aqp)
library(epiR)

###################  Required functions     ############################################

urlRoot = "http://www.asris.csiro.au/ArcGis/services/"

iniAttributeInfo <- function(){
  #cat("Generating attribute info dictionary")
  dict <- hash()
  assign("Depth_to_Rock", c("None", "Meters", "DER"), dict)
  assign("Rooting_Depth", c("None", "Meters", "DPE"), dict)
  assign("Organic_Carbon", c("Log", "%", "SOC"), dict)
  assign("pH_Soil_Water", c("None", "", "PHW"), dict)  
  assign("pH_Soil_CaCl2", c("None", "", "PHC"), dict)
  assign("Clay", c("None", "%", "CLY"), dict)
  assign("Silt", c("None", "%", "SLT"), dict)
  assign("Sand", c("None", "%", "SND"), dict)
  assign("ECEC", c("Log", "meq/100g", "ECE"), dict)
  assign("Bulk_Density", c("None", "g/cm", "BDW"), dict)
  assign("Available_Water_Capacity", c("None", "%", "AWC"), dict)
  assign("EC", c("Log", "dS/m", "ECD"), dict) 
  assign("Total_P", c("Log", "%", "PTO"), dict)
   assign("Total_Nitrogen", c("Log", "%", "NTO"), dict)  
  return (dict)
}

iniDepthInfo <- function(){
  #cat("Generating Depth info dictionary")
  dict <- hash()
  assign("0-5", c(1), dict)
  assign("5-15", c(4), dict)
  assign("15-30", c(7), dict)
  assign("30-60", c(10), dict)
  assign("60-100", c(13), dict)
  assign("100-200", c(16), dict) 
  return (dict)
}

iniComponentInfo <- function(){
  #cat("Generating Depth info dictionary")
  dict <- hash()
  assign("value", c(0), dict)
  assign("lower_ci", c(2), dict)
  assign("upper_ci", c(1), dict)
  return (dict)
}

iniProductInfo <- function(){
  
  dict <- hash()
  assign("National", c("TERN", "ACLEP_AU_NAT_C"), dict)
  assign("WA", c("TERN", "ACLEP_AU_WAT_D"), dict)
  assign("SA", c("TERN", "ACLEP_AU_SAT_D"), dict)
  assign("TAS", c("TERN", "ACLEP_AU_TAS_N"), dict)
  assign("Australian-Wide", c("TERN", "ACLEP_AU_TRN_N"), dict)
  return (dict)
}

getServiceInfo <- function(region, attribute, component, depth){
  #cat("Getting Layer Info")
  
  attributeInfo <- iniAttributeInfo()
  depthInfo <- iniDepthInfo()
  componentInfo <- iniComponentInfo()
  productInfo <- iniProductInfo()
  
 ainfo<- get(attribute, attributeInfo)
 attCode = ainfo[3]
 
 cinfo = get(component, componentInfo)
 compCode = cinfo[1]
 
 pinfo = get(region, productInfo)
 
 
 dinfo = get(depth, depthInfo)
 depthCode = dinfo[1]
 layerId <- depthCode + compCode

 URLp1 = paste(urlRoot, pinfo[1],"/", attCode, "_", pinfo[2], "/MapServer/WCSServer?", sep="") 
  return (list(URLp1, layerId))
}

getWCSURL <- function(product, attribute, component, depth, extents){
  
  cols = as.integer((maxX - minX) / 0.000833333);
  rows = as.integer((maxY - minY) / 0.000833333);
  
  id = getServiceInfo(product, attribute, component, depth )
  urlBase = id[[1]][1]
  layer = id[[2]][1]
  
  wcs = paste(urlBase, "REQUEST=GetCoverage&SERVICE=WCS&VERSION=1.0.0&COVERAGE=", layer, "&CRS=EPSG:4283&BBOX=", extents[[1]][1], ",", extents[[3]][1], ",", extents[[2]][1],",", extents[[4]][1], "&WIDTH=", cols, "&HEIGHT=", rows, "&FORMAT=GeoTIFF",sep="")
  
  return (wcs)
}


getANZSoilWFSasDT <- function(url, region, extents, attribute, localData){
  
  xml.request = paste('<?xml version="1.0"?><wfs:GetFeature xmlns:om="http://www.opengis.net/om/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:anzsmlss="http://anzsoil.org/ns/soilsample/2.0.0" xmlns:sam="http://www.opengis.net/sampling/2.0" xmlns:wfs="http://www.opengis.net/wfs" xmlns:fes="http://www.opengis.net/fes/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ogc="http://www.opengis.net/ogc" service="WFS" version="1.1.0" outputFormat="application/gml+xml; version=3.2" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd http://anzsoil.org/ns/soilsample/2.0.0 http://www.clw.csiro.au/aclep/ANZSoilML/trunk/ANZSoilML_SoilSample/xsd/anzsoilml-soilsample.xsd" resultType="results">
  <wfs:Query typeName="om:OM_Observation">
    <ogc:Filter>
<ogc:And>
<ogc:PropertyIsEqualTo>
<!--<ogc:PropertyName>om:procedure/gsmlla:AnalyticalProcess/gml:name</ogc:PropertyName>-->
<ogc:PropertyName>om:observedProperty/@xlink:title</ogc:PropertyName>
<ogc:Literal>DENSITY</ogc:Literal>
</ogc:PropertyIsEqualTo>
<ogc:BBOX>
<gml:Envelope srsName="EPSG:4283">
<gml:lowerCorner>' , extents[[1]][1]  , ' ' , extents[[3]][1] ,'</gml:lowerCorner>
<gml:upperCorner>' , extents[[2]][1]  , ' ' , extents[[4]][1] ,'</gml:upperCorner>
</gml:Envelope>
</ogc:BBOX>
</ogc:And>
</ogc:Filter>
</wfs:Query>
</wfs:GetFeature>', sep='')
  
 # cat(xml.request)
  if(localData)
    {
        xmltext  <- xmlTreeParse('c:/temp/sali.xml',useInternalNodes=T)
    }
    else
    {    
      myheader=c(Connection="close", 'Content-Type' = "application/xml", 'Content-length'=nchar(xml.request))
      data =  getURL(url = url, postfields=xml.request, httpheader=myheader, verbose=TRUE)
      xmltext  <- xmlTreeParse(data, asText = TRUE,useInternalNodes=T)
      sink(paste('c:/temp/', region, '.xml', sep=''))
      print(xmltext)
      sink()
    }
 
print(xmltext)
 
#writeLines(xmltext, "c:/temp/natsoil.xml")
  
  valsFilt = '[../om:featureOfInterest/sams:SF_SpatialSamplingFeature/sams:shape/gml:Point/gml:pos]'
  Vals <- as.numeric(unlist(xpathApply(xmltext, paste('//wfs:FeatureCollection/wfs:member/om:OM_Observation/om:result', valsFilt , sep=''),xmlValue)) )
  idsFilt = '[../om:featureOfInterest/sams:SF_SpatialSamplingFeature/sams:shape/gml:Point/gml:pos]'
  ids <- unlist(xpathApply(xmltext, paste('//wfs:FeatureCollection/wfs:member/om:OM_Observation/@gml:id', valsFilt , sep=''))) 
  methodsFilt = '[../../../om:featureOfInterest/sams:SF_SpatialSamplingFeature/sams:shape/gml:Point/gml:pos]'
  methods <- unlist(xpathApply(xmltext, paste('//wfs:FeatureCollection/wfs:member/om:OM_Observation/om:procedure/gsmlla:AnalyticalProcess/gml:name', methodsFilt , sep=''),xmlValue)) 
  datesFilt = '[../om:featureOfInterest/sams:SF_SpatialSamplingFeature/sams:shape/gml:Point/gml:pos]'
  dates <- unlist(xpathApply(xmltext,paste('//wfs:FeatureCollection/wfs:member/om:OM_Observation/om:resultTime', datesFilt , sep=''),xmlValue)) 
  locFilt = '[../om:featureOfInterest/sams:SF_SpatialSamplingFeature/sams:shape/gml:Point/gml:pos]'
  locs <- unlist(xpathApply(xmltext,'//wfs:FeatureCollection/wfs:member/om:OM_Observation/om:featureOfInterest/sams:SF_SpatialSamplingFeature/sams:shape/gml:Point/gml:pos',xmlValue))  
  upper_depths <- as.numeric(unlist(xpathApply(xmltext,'//wfs:FeatureCollection/wfs:member/om:OM_Observation/om:featureOfInterest/sams:SF_SpatialSamplingFeature/sam:parameter/om:NamedValue/om:value/anzsml:DepthQuantityRange/anzsml:upperBoundary/swe:Quantity/swe:value',xmlValue)))
  lower_depths <- as.numeric(unlist(xpathApply(xmltext,'//wfs:FeatureCollection/wfs:member/om:OM_Observation/om:featureOfInterest/sams:SF_SpatialSamplingFeature/sam:parameter/om:NamedValue/om:value/anzsml:DepthQuantityRange/anzsml:lowerBoundary/swe:Quantity/swe:value',xmlValue)))
  
  lats <- numeric()
  longs<- numeric()
  IDcomps<-character()
  regionID<-character()
  for (i in 1:length(locs) ) { 
    s =  strsplit(locs[i], " ")
    lats<- append(lats, as.numeric(s[[1]][2]))
    longs<- append(longs, as.numeric(s[[1]][1]))
    
    idc = strsplit(ids[i], "_")
    idc2 = strsplit(idc[[1]][3], "\\.")
    IDcomps<-append(IDcomps, paste(idc2[[1]][1], idc2[[1]][2], idc2[[1]][3], sep="_"))
    regionID<-append(IDcomps, region)
    
  }
  
  
  # just for checking purposes
  length(methods)
  length(IDcomps)
  length(ids)
  length(dates)
  length(lats)
  length(longs)
  length(upper_depths)
  length(lower_depths)
  length(Vals)
  
  
  
  dt <- data.frame(region, IDcomps,lats, longs, methods, dates, upper_depths, lower_depths, Vals)
    
  return (dt)
}

###################  End of Required functions    ######################################

###################  Set these parameters to meet your requirements  ###################
###                                                                                  ###
###  PLEASE NOTE : the current Web Coverage Service (WCS) is a restricted download   ###
###               size limit. It is limited to 3x3 arc seconds. If you try to        ###
###               a larger area R will hang.                                         ###

Region = "National"  #  see 'iniProductInfo' function for choices
Attribute = "Bulk_Density"  #  see 'iniProductInfo' function for choices
Component = "value"   #  see 'iniDepthInfo' function for choices
DepthInterval = "100-200"  #  see 'iniDepthInfo' function for choices
DownloadDirectory = 'c:/temp'

# extent to extract in WGS 84 coordinates - refer note above about max size
minX = 149
maxX = 151
minY = -27
maxY = -25



###################  End of parameters section   ######################################

###################  Run the commands below to extract and download the raster data ###

setwd(DownloadDirectory)
extents = list(minX, maxX, minY, maxY)
downloadName = paste(DownloadDirectory,'/', Region,'_', Attribute,'_', Component,'_', DepthInterval, '.tif', sep='')


url = getWCSURL(Region, Attribute, Component, DepthInterval, extents)
url

bin = getBinaryURL(url)
con <- file(downloadName, open = "wb")
writeBin(bin, con)
close(con)

r <- raster(downloadName)
attributeInfo <- iniAttributeInfo()
ainfo<- get(Attribute, attributeInfo)
attUnits = ainfo[2]
plot(r, main=paste(Region, Attribute, Component, DepthInterval, 'cm (', attUnits ,')', sep=' '))

###################  End of raster data extraction routines  ###########################



###################  Code from here on is realively experimental and systems  #########
###################  will change in the future so don't set anything up that  #########
################### relies on this code or code derived from this             #########


###################  Extract soil site data using ANZSoilML web services  #############

urlNatsoil = "http://www.clw.csiro.au/aclep/resources/wfs"
DT_Natsoil = getANZSoilWFSasDT (urlNatsoil, "NatSoil", extents, "DENSITY", F)
url_QLD = "http://qld.anzsoil.org/geoserver/wfs/"
DT_SALI = getANZSoilWFSasDT (url_QLD, "QLD", extents, "DENSITY", F)

locs = rbind(DT_Natsoil,DT_SALI)
write.table(locs, paste(DownloadDirectory,'/', Attribute,'_labData.csv', sep=''), sep=",",row.names=F)
locs

coordinates(locs) <- ~ longs + lats
proj4string(locs) <- CRS("+proj=longlat +datum=WGS84")

plot(r)
classcol <- c("red", "blue")
points(locs, pch=16, col=classcol[locs$region])

###################  End of extracting date using ANZSoilML web services  #############



###################  Generate Splines and do residuals analysis on WCS raster  #######


#cant get 2 data frames to work
#,DT_SALI
alocs = data.frame(rbind(DT_Natsoil))
#alocs = data.frame(rbind(DT_SALI))
alocs$upper_depths <- alocs$upper_depths * 100
alocs$lower_depths <- alocs$lower_depths * 100
#alocs <- data.frame(locs$IDcomps, locs$lats, locs$longs, locs$methods, locs$dates, ud, ld, locs$Vals)
depths(alocs) <- IDcomps ~ upper_depths + lower_depths
#coordinates(alocs) <- ~ longs + lats

print(alocs)
par(mar=c(0,0,3,0))
#classcol <- c("red", "blue", "green")
plot(alocs, name='name', color='Vals')


alocs$hcnt = profileApply(alocs, FUN=nrow)
loc4spline = subsetProfiles(alocs, s='hcnt>2')
spln <- mpspline(loc4spline, var.name="Vals", mxd = 200, lam = 0.1, d = t(c(0,5,15,30,60,100,200)), vlow = 0, vhigh = 1000)

par(mar=c(2,2,2,2))
par(mfrow=c(4,6))
for (i in 1:length(spln$idcol) ) 
{ 
  if(!(all(is.na(spln$var.1cm[,i])))){
    d = spln$var.1cm[,i]
    plot( d, 1:length(d), type="n", main=paste("Site No .", i), yaxs = "i", xaxs = "i", xlim = c(1, 2), ylim = rev(range(c(0,200))))
    #plot( d, 1:length(d), type="n", main=paste("Site No .", i))
    lines( d, 1:length(d), type="l") 
    
  }
}
par(mfrow=c(1,1))

#d = spln$var.1cm[,1]
#plot( d, 1:length(d), type="l", main=paste("Site No .", i), yaxs = "i", xaxs = "i", xlim = c(1, 2), ylim = rev(range(c(0,200))))


stdDepths = spln$var.std[1,]
sites = spln$idcol
dfstd = data.frame(spln$idcol, spln$var.std[,1], spln$var.std[,2], spln$var.std[,3], spln$var.std[,4], spln$var.std[,5], spln$var.std[,6])
colnames(dfstd) = c("SiteID", "L1", "L2", "L3", "L4", "L5", "L6")


resdf <- data.frame(locs$IDcomps,locs$lats, locs$longs)
colnames(resdf) <- c("IDcomps", "lats","longs")
nodups <- resdf[!duplicated(resdf[,c("IDcomps")]),]
mergeddf <- merge(nodups, dfstd, by.x = "IDcomps", by.y = "SiteID")
coordinates(mergeddf) <- ~ longs + lats
proj4string(mergeddf) <- CRS("+proj=longlat +datum=WGS84")


s<-stack()
s<-addLayer(s,r) 
modelledVals<-as.data.frame(extract(s,mergeddf,method="simple")) 
residuals <- cbind(mergeddf, modelledVals)
colnames(residuals) = c("SiteID","Lats", "Longs", "L1", "L2", "L3", "L4", "L5", "L6", "modelledVal")
residuals <- within(residuals, resid <- L1 - modelledVal )
residuals2 = na.omit(residuals)
coordinates(residuals2) = ~Longs+Lats
proj4string(residuals2) <- CRS("+proj=longlat +datum=WGS84")

plot(r)
rbPal <- colorRampPalette(c('red','blue'))
ramp <- rbPal(10)[as.numeric(cut(residuals2$resid,breaks = 10))]
points(residuals2, pch=16,col = ramp)


cccC <- epi.ccc(residuals2$L1, residuals2$modelledVal, ci = "z-transform",conf.level = 0.95)
r.sqC <- cor(residuals2$L1, residuals2$modelledVal)^2
fitC <- lm(L1 ~ modelledVal-1, data=residuals2)
validC = data.frame(residuals2$L1, residuals2$modelledVal )
plot(validC, main=paste( 'Model Fit'), xlab='Observed', ylab = 'predicted', pch=3, cex =0.5, xlim = c(1, 2), ylim = c(1, 2))
abline(fitC, col="red")
abline(0,1, col="green")
text(0, 120, paste("R2 = ",round(r.sqC, digits = 2)), pos=4)
text(0, 110, paste("LCCC = ", round(cccC$rho.c[,1], digits = 2)), pos=4)

print( paste("R2 = ", round(r.sqC, digits = 2)))
print(  paste("LCCC = ", round(cccC$rho.c[,1], digits = 2)))

plot(density(residuals2$resid))
polygon(density(residuals2$resid),col="purple", border="purple", xlab="Depth", main="Regolith Depth")
