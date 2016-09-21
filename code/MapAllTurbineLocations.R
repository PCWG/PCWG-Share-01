MapAllTurbineLocations <- function(df,
                                   sw.version = "",
                                   sw.version.logic = "equals",
                                   code.dir,
                                   ouput.dir = file.path(getwd(),'analysis','all'),
                                   made.by = ""){
  
  # plots baseline errors
  #
  # Args:
  # *.error summary of errors
  #
  # Returns:
  # a ggplot2 plot item
  
  require(rgdal)        # for readOGR(...)
  require(RColorBrewer) # for brewer.pal(...)
  require(ggplot2)
  
  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # Check to see if we have country data
  if (NROW(df[!(is.na(df$Geography.country)),])>0){
    # get the maximum number of tests in any one country
    nmax <- max(aggregate(cbind(count = data.type)~Geography.country,
                          data = df[!(is.na(df$Geography.country)),],
                          FUN = length)$count,
                na.rm = TRUE)
    
    # filter by software version
    if (sw.version == ""){
      # get all versions
    } else {
      # get specific software version
      df <- SelectDatabySWVersion(df,
                                  sw.version,
                                  sw.version.logic)
    }
    
    # continue if we have data
    if (NROW(df)>0){
      
      # get the number of datasets
      n.lines <- NROW(unique(df$data.file))
      # create the plot title
      plot.title <- "Locations of Turbines"
      # create the plot subtitle
      plot.subtitle <- paste0(n.lines,
                              " data sets found.")
      # create the plot caption
      plot.caption <- labelAggregate(as.character(NROW(df)),
                                     df$sw.version,
                                     made.by)
      
      # read world map
      world <- readOGR(dsn=file.path(code.dir,"worldmap"),
                       layer="TM_WORLD_BORDERS_SIMPL-0.3",
                       verbose=FALSE)
      # get countries
      countries <- world@data
      countries <- cbind(id=rownames(countries),countries)
      
      # get the data count
      counts <- aggregate(cbind(count = data.type)~Geography.country,
                          data = df[!(is.na(df$Geography.country)),],
                          FUN = length)
      # change the names of some of the countries to correspond to the 
      counts <- data.frame(sapply(counts,
                                  FUN = function(x){
                                    x<-gsub("US","United States",x)
                                    x<-gsub("UK","United Kingdom",x)
                                    return(x)
                                  },
                                  simplify = FALSE))
      counts$count <- as.numeric(levels(counts$count))[counts$count]
      counts$Geography.country <- as.character(levels(counts$Geography.country))[counts$Geography.country]
      
      # combine the data count with the country outlines
      countries <- merge(countries,
                         counts,
                         by.x="NAME", 
                         by.y="Geography.country", 
                         all.x=T)
      
      #countries$count(is.na(countries$count)) <- o
      
      # now create the data frame to plot
      map.df <- fortify(world)
      map.df <- merge(map.df,
                      countries,
                      by="id")
      
      # create the plot
      p <- ggplot(map.df, 
                  aes(x=long,
                      y=lat,
                      group=group)) +
        geom_polygon(aes(fill=count),
                     size = 0.125)+
        geom_path(colour="grey50",
                  size = 0.125)+
        scale_fill_gradientn(name="N. Data Sets",
                             colours=rev(brewer.pal(9,"Spectral")),
                             limits =c(0,nmax),
                             na.value="white")+
        coord_fixed() +
        scale_x_continuous(name = "",
                           breaks = c(-180,-90,0,90,180),
                           labels = c("180 E","90 E","0","90 W","180 W")) +
        scale_y_continuous(name = "",
                           breaks = c(-90,-45,0,45,90),
                           labels = c("90 S","45 S","0","45 N","90 N")) +
        labs(title = plot.title) +
        labs(subtitle = plot.subtitle) +
        labs(caption=plot.caption) 
      
      print(p)
      
      
      if (sw.version == ""){
        filename = paste0("AllTurbineLocations_Map_allSWversions.png")
      } else {
        filename = paste0("AllTurbineLocations_Map_SWVersion",sw.version,".png")
      }
      
      ggsave(filename = file.path(output.dir,
                                  filename),
             plot = p,
             width = 6, 
             height = 4, 
             units = "in", 
             dpi = 300)
      
    } else {
      message("No data found with the requested software version")
    }
  } else {
    message("No country data found in this data set")
  }
  
  # turn warnings back on
  options(warn = oldw)
}
