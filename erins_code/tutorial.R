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
lnd <- readOGR(dsn = "data/london_sport.shp")

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
