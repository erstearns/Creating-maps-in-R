##############################################
# Code author: Erin Stearns
# Code objective: Getting familiar with geo data and making maps in R
# Date: 12.14.2017
#############################################

rm(list = ls()) 

# ---------------------------------------- Setting up environment --------------------------------------------#
# Load necessary packages
x <- c("reshape2","data.table","ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap","sf")
lapply(x, require, character.only = TRUE)


# --------------------------------------- Part II: Spatial data in R ----------------------------------------- #
# load london sport data shapefile - pop of London boroughs in 2001 and % pop participating in sporting activities
lnd <- readOGR("data/london_sport.shp")

# ---- structure of spatial data ----
#access data slot: non-geographic attribute data
head(lnd@data,n=2)
mean(lnd$Partic_Per) # short for mean(lnd@data$Partic_Per) 

# check classes of all variables 
sapply(lnd@data, class)

# need to coerce pop to numeric
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001))

#check CRS
lnd@proj4string

# ---- basic plotting ---- 
plot(lnd)

# select rows of lnd@data where sports participation is less than 13
lnd@data[lnd$Partic_Per < 13, 1:3]

# Select zones where sports participation is between 20 and 25%
sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25
plot(lnd[sel, ]) 
head(sel) # test output of previous selection (not shown)

plot(lnd, col = "lightgrey") # plot the london_sport object
sel <- lnd$Partic_Per > 25
plot(lnd[sel, ], col = "turquoise", add = TRUE) # add selected zones to map

#challenge
    plot(lnd,col="lightgrey")
    # find London's geographic centroid (add ", byid = T" for all)
    cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
    points(cent_lnd, cex = 3)
    # set 10 km buffer
    lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 
    
    # method 1 of subsetting selects any intersecting zones
    lnd_central <- lnd[lnd_buffer,] # the selection is too big!
    # test the selection for the previous method - uncomment below
    plot(lnd_central, col = "lightblue", add = T)
    plot(lnd_buffer, add = T) # some areas just touch the buffer
    
    # method2 of subsetting selects only points within the buffer
    lnd_cents <- SpatialPoints(coordinates(lnd),
                               proj4string = CRS(proj4string(lnd))) # create spatialpoints
    sel <- lnd_cents[lnd_buffer,] # select points inside buffer
    points(sel) # show where the points are located
    lnd_central <- lnd[sel,] # select zones intersecting w. sel
    plot(lnd_central, add = T, col = "lightslateblue", 
         border = "grey")
    plot(lnd_buffer, add = T, border = "red", lwd = 2)
    
    # Add text to the plot!
    text(coordinates(cent_lnd), "Central\nLondon")

  # ---- selecting quadrants ----
  # Find the centre of the london area
  #grabs x coordinate
  easting_lnd <- coordinates(gCentroid(lnd))[[1]]
  #grabs y coordinate
  northing_lnd <- coordinates(gCentroid(lnd))[[2]]
  
  # arguments to test whether or not a coordinate is east or north of the centre
  # x coordinate col
  east <- sapply(coordinates(lnd)[,1], function(x) x > easting_lnd)
  # y coordinate col
  north <- sapply(coordinates(lnd)[,2], function(x) x > northing_lnd)
  
  # test if the coordinate is east and north of the centre
  lnd$quadrant <- "unknown" # prevent NAs in result
  lnd$quadrant[east & north] <- "northeast"
  lnd$quadrant[east & !north] <- "southeast"
  lnd$quadrant[!east & north] <- "northwest"
  lnd$quadrant[!east & !north] <- "southwest"
  
  plot(lnd)
  plot(lnd[east & north,], add = TRUE, col = "red" ) #plots northeast quadrant as red polygons
  llgridlines(lnd, lty= 3, side ="EN", offset = -0.5) #adds gridlines to map
  
  lnd_disolved = rgeos::gUnaryUnion(spgeom = lnd, id = lnd$quadrant) #dissolve borough lines and leave only quadrant polygons
  #gUnaryUnion joins intersecting geometries
  
  library(tmap)
  qtm(lnd, fill = "quadrant") +
    tm_shape(lnd_disolved) +
    tm_borders(lwd = 9)
  
  
# --------------------------------------- Part III: Creating and manipulating spatial data ------------------------------ #
  
# ---- creating new spatial data ----
vec <- vector(mode = "numeric", length = 3)
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4))

#to creata  spatial object, must be a numeric matrix or data frame
sp1 <- SpatialPoints(coords = df)

#check class
class(sp1)

#creat spatial object with data frame added -- allows for non-spatial data to be added
spdf <- SpatialPointsDataFrame(sp1, data = df)
class(spdf)

# ---- projections: setting and transforming CRS in R ----

#spatial data should always have a CRS -- if no CRS information provided, and correct CRS known, can be set:
proj4string(lnd) <- NA_character_ #remove CRS info from lnd
proj4string(lnd) <- CRS("+init=epsg:27700") #assign a new CRS

#the above code changes the CRS but does not reproject the data
#EPSG codes best way to refer to different projections. the following code searchs the list of available EPSG codesa and creates
#a new version of lnd in WSG84

EPSG <- make_EPSG() #create data frame of available EPSG codes
#search for WGS 84 code
# a planar CRS is defined by a projection, datum and a set of parameters
#   - parameters describe things like where the center of the map is; number of parameters depends on the projectsion -- important to document
EPSG[grepl("WGS 84$", EPSG$note),]
# * Geographic CRS: A coordinate reference system based on a geodetic datum and using an ellipsoidal (including spherical) model 
#                of the Earth. This provides an accurate representation of the geometry of geographic features for a large portion 
#                of the Earth’s surface. Geographic coordinate reference systems can be two- or three-dimensional. A Geographic 2D 
#                CRS is used when positions of features are described on the surface of the ellipsoid through latitude and longitude 
#                coordinates; a Geographic 3D CRS is used when positions are described on, above or below the ellipsoid and includes 
#                height above the ellipsoid. [...]
# * Geocentric CRS: A coordinate reference system based on a geodetic datum that deals with the Earth’s curvature by taking the 3D 
#                spatial view, which obviates the need to model the curvature. The origin of a geocentric CRS is at the centre of mass 
#                of the Earth.

#convert coordinates of lnd into WGS 84 CRS, reproject & save
lnd84 <- spTransform(lnd, CRS("+init=epsg:4326"))
#works as well?
lnd84_e <- spTransform(lnd, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#save as .Rds bc .RData is more restrictive and maintains object's name
saveRDS(object = lnd84, file = "data/lnd84.Rds") #will use in part IV
saveRDS(object = lnd84_e, file = "data/lnd84_e.Rds") #will use in part IV -- my version

#now we can remove the lnd84 object
rm(lnd84)
rm(lnd84_e)

# ---- attribute joins ----

#print attribute names
names(lnd)

#re-read in london sport file
lnd <- readOGR("data/london_sport.shp")
plot(lnd)

# going to join non-spatial crime data stored in a csv
# each row in csv represents a single crime, thus going to aggregate crimes to the borough level to prep for join to lnd
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",stringsAsFactors = F)

#extract theft & handling crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling",]

#calculate sum of crime count for each district and save
crime_ag <- aggregate(CrimeCount ~ Borough, FUN=sum, data=crime_theft)

#prior to joining on borough names, compare name columns
lnd$name %in% crime_ag$Borough

#grab rows that don't match
lnd$name[!lnd$name %in% crime_ag$Borough]

#left join to lnd bc want the length of df to remain unchanged, with vars from new data appended in cols
lnd@data <- left_join(lnd@data, crime_ag, by=c('name' = 'Borough'))

#plot using tmap function 'quick thematic map plot' (qtm)
qtm(lnd, "CrimeCount") #plot basic map

# ---- clipping and spatial joins ----
