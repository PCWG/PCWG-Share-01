PlotAllTurbineLocations <- function(df,
                                    sw.version = "",
                                    sw.version.logic = "equals",
                                    ouput.dir = file.path(getwd(),'analysis','all'),
                                    made.by = ""){
  
  # plots baseline errors
  #
  # Args:
  # *.error summary of errors
  #
  # Returns:
  # a ggplot2 plot item
  
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
    if ((NROW(df)>0) & 
        (any(nchar(df$Geography.country)>0,na.rm = TRUE))){
      
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
      
      # aggregate the 
      
      # plot
      p <- ggplot(data = df,
                  aes(x = factor(Geography.country))) +
        geom_bar() +
        scale_x_discrete(drop = FALSE,
                         name = "Country") +
        scale_y_continuous(name = "Count") +
        labs(title = plot.title) +
        labs(subtitle = plot.subtitle) +
        labs(caption=plot.caption) 
      
      if (sw.version.logic == "equals"){
        
      } else {
        p <- p + 
          aes(fill = sw.version) +
          scale_fill_discrete(name = "Software\nVersion")
      }
      
      print(p)
      
      if (sw.version == ""){
        filename = paste0("AllTurbineLocations_allSWversions.png")
      } else {
        filename = paste0("AllTurbineLocations_SWVersion",
                          sw.version.logic,
                          sw.version,
                          ".png")
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