##############################################
# Code author: Erin Stearns
# Code objective: Getting familiar with geo data and making maps in R
# Date: 12.14.2017
#############################################

# ---------------------------------------- summary notes of most helpfiul bits ------------------#

#package: sp
bbox() #retrieves spatial bounding from spatial data

#package: tmap
tmap_mode("view") #set tmap mode to interactive viewing

#package: ggplot2
fortify() #converts spatial data to a dataframe, which is required format for using ggmap

#package: OpenStreetMap
#  -- tricky installation, set java path:
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_144\\')

# -----------------------------------------------------------------------------------------------#
rm(list = ls()) 

# ---------------------------------------- Setting up environment --------------------------------------------#
# Load necessary packages
#x <- c("reshape2","data.table","ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap","sf")
#lapply(x, require, character.only = TRUE)
pacman::p_load(reshape2,data.table,ggmap, rgdal, rgeos, maptools, dplyr, tidyr, tmap,sf,
               leaflet,tmaptools,OpenStreetMap)
#need to checkout java library to get OpenStreetMap pkg working properly

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

#trying to to make it a bit different
qtm(lnd, fill="CrimeCount", fill.n = 8, fill.palette = "div",
    fill.title = "Thefts", fill.position="topright",
    format = "World", style="gray")
#from r code on repo:
lnd$Thefts = lnd$CrimeCount / 10000
qtm(lnd, "Thefts", fill.title = "Thefts\n(10000)", fill.n = 8,fill.palette = "div",scale = 0.8) +
   tm_layout(legend.position = c(0.89,0.02))

# -- From QTM help examples:
data("World","metro")
qtm(World)
qtm(World, fill = "economy", format="World", style="col_blind")

qtm(World, fill="HPI", fill.n=9, fill.palette="div", fill.auto.palette.mapping=FALSE, 
    fill.title="Happy Planet Index", fill.id="name", format="World", style="gray")

# bubble map
qtm(World, borders = NULL) + 
  qtm(metro, symbols.size = "pop2010", 
      symbols.title.size= "Metropolitan Areas", 
      symbols.id= "name",
      format = "World")


# ---- clipping and spatial joins ----
# create new stations object - transport infrastructure points -- using the "lnd-stns" shapefile.
stations <- readOGR(dsn = "data/lnd-stns.shp")
# stations = read_shape("data/lnd-stns.shp") # from tmap

# comparing the CRS of both lnd and stations objects prior to attempting a spatial join
proj4string(stations) # this is the full geographical detail.
proj4string(lnd) # what's the coordinate reference system (CRS)
bbox(stations) # the extent, 'bounding box' of stations
bbox(lnd) # return the bounding box of the lnd object

# the CRS differs between the two objects, the lnd is the official CRS for the UK, thus will set
# stations to that CRS; also, stations is unprojected, so wil project as well
stations <- spTransform(stations, CRS(proj4string(lnd))) #proj4string sets the CRS and spTransform projects it

#plot
plot(lnd)
points(stations)

#spatial extent of stations is beyond lnd
#Want to clip the stations so that only those falling within London boroughs are retained 
#we can use sp::over,or simply the square bracket notation for subsetting tabular data 
#(enter ?gIntersects to find out another way to do this):
stations <- stations[lnd,]
plot(lnd)
points(stations)

#plotting other things - not in pdf, found in intro-spatial.R script in repo
#aggregate stations to borough level
stations_agg = aggregate(x = stations["CODE"], by = lnd, FUN = length)
head(stations_agg@data)

#create new var in lnd to match borough count of transport infrastructure points code in stations_agg
lnd$n_points = stations_agg$CODE

#get average number of stations per borough
lnd_n = aggregate(stations["NUMBER"], by = lnd, FUN = mean)

## ----fig.cap="Choropleth map of mean values of stations in each borough"----
#create quantiles
brks = quantile(lnd_n$NUMBER)
labs = grey.colors(n = 4) # shouldn't this be 5 vals? 0,25,50,75,100? -- cutting below does not work with n=5
#convert numeric to factor -- divide the range of NUMBER into intervals and codes in NUMBER 
#  according to which interval they fall
q = cut(lnd_n$NUMBER, brks, labels = labs, 
        include.lowest = T)
summary(q) # check what we've created - 4 factors containing counts of the boroughs in each

#convert and plot
qc = as.character(q) # convert to character class to plot
plot(lnd_n, col = qc) # plot (not shown in printed tutorial)
legend(legend = paste0("Q", 1:4), fill = levels(q), "topright")
areas = sapply(lnd_n@polygons, function(x) x@area) #this creates a vector of areas for each borough

#
levels(stations$LEGEND) # see A roads and rapid transit stations (RTS) (not shown)
sel = grepl("A Road Sing|Rapid", stations$LEGEND) # selection for plotting
sym = as.integer(stations$LEGEND[sel]) # subset to selection aboce and convert to symbols
points(stations[sel,], pch = sym) #plot
legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym))

## ---- echo=FALSE, eval=FALSE---------------------------------------------
## # , fig.cap="Symbol levels for train station types in London"
## q = cut(lnd_n$NUMBER, breaks= c(quantile(lnd_n$NUMBER)), include.lowest=T)
## clr = as.character(factor(q, labels = paste0("grey", seq(20, 80, 20))))
## plot(lnd_n, col = clr)
## legend(legend = paste0("q", 1:4), fill = paste0("grey", seq(20, 80, 20)), "topright")
## sel = grepl("A Road Sing|Rapid", stations$LEGEND) # selection for plotting
## sym = as.integer(stations$LEGEND[sel]) # symbols
## points(stations[sel,], pch = sym)
## legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym))

# --------------------------------------- Part IV: Making maps with tmap, ggplot2 and leaflet ---- #
# ---- tmap ----
#code from pdf
qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues") # not shown
#code from pdf
qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = "Blues", ncol = 2)

## ---- fig.cap="Facetted map of London Boroughs created by tmap"----------
#from pdf
tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords = TRUE, drop.units = TRUE)
#from intro-spatial.R
tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords=TRUE, drop.shapes=TRUE) +
  tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)


# to create a basemap in R, you can use the read_osm function, from the tmaptools package as follows.
# * Note that you must first transform the data into a geographical CRS:*

# Transform the coordinate reference system
lnd_wgs = spTransform(lnd, CRS("+init=epsg:4326"))
if(curl::has_internet()) {
  osm_tiles = tmaptools::read_osm(bbox(lnd_wgs)) # download images from OSM with the same boundaries as our london layer
  tm_shape(osm_tiles) + tm_raster() +
    tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02))
} else {
  tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02))
}

#Another way to make tmap maps have a basemap is by entering tmap_mode("view"). 
#This will make the maps appear on a zoomable webmap powered by leaflet.
tmap_mode("view")
if(curl::has_internet()) {
  osm_tiles = tmaptools::read_osm(bbox(lnd_wgs)) # download images from OSM with the same boundaries as our london layer
  tm_shape(osm_tiles) + tm_raster() +
    tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02))
} else {
  tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02))
}


# ---- ggmap ----

#convert pop from factor to numeric var
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001))

#create a scatter plot from attribute data
p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))

# add layers - e.g. add text to the plot
p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) +
  geom_text(size = 2, aes(label = name))

# will create a map to show the percentage of the population in each London Borough
#  who regularly participate in sports activities

# IMPORTANT NOTE: ggmap requires spatial data to be supplied as data.frame, using fortify()
# The generic plot() function can use Spatial* objects directly; ggplot2 cannot.   
#  Therefore we need to extract them as a data frame. The fortify function was written 
#  specifically for this purpose. For this to work, either the maptools or rgeos
#  packages must be installed.

#fortify london shapefile
lnd_f <- fortify(lnd)

#The fortify() application step has lost the attribute information associated with the lnd object
#We can add it back using the left_join function from the dplyr package 
head(lnd_f, n = 2) # peak at the fortified data
lnd$id <- row.names(lnd) # allocate an id variable to the sp data
head(lnd@data, n = 2) # final check before join (requires shared variable name)
lnd_f <- left_join(lnd_f, lnd@data) # join the data

#now data ready
map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
  geom_polygon() + coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "% Sports\nParticipation") +
  ggtitle("London Sports Participation")

#the map above in black and white:
map + scale_fill_gradient(low = "white", high = "black")


# ---- creating interactive maps with leaflet ----

#leaflet has tight integration with RStudio for making interactive online visualizations
#load the lnd dataset we reprojected to WGS 84 in Part III
lnd84 <- readRDS('data/lnd84.Rds')
#load my test one
lnd84_e <- readRDS('data/lnd84_e.Rds')

leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)

# ---- advanced task: faceting for maps ----

#bring in london historic pop (1801-2001) data and tidy up
london_data <- read.csv("data/census-historic-population-borough.csv")
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)

#merge historic pop data with london borough spatial object
ltidy <- rename(ltidy, ons_label = Area.Code) # rename Area.code variable
lnd_f <- left_join(lnd_f, ltidy)

#rename date var
lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)

#using faceting, can produce map for each year
ggplot(data = lnd_f, # the input data
       aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ date) + # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

#save 
# ggsave("figure/facet_london.png", width = 9, height = 9) # save figure

