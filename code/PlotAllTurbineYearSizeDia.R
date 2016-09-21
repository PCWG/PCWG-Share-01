PlotAllTurbineYearSizeDia <- function(df,
                                      sw.version = "",
                                      sw.version.logic = "equals",
                                      ouput.dir = file.path(getwd(),'analysis','all'),
                                      made.by = ""){
  # plots metadata for PCWG Share 01 
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
      (any(nchar(df$turbine.height)>0,na.rm = TRUE)) &
      (any(nchar(df$year.of.measurement)>0,na.rm = TRUE))){
    
    # get the number of datasets
    n.lines <- NROW(unique(df$data.file))
    # create the plot title
    plot.title <- "Turbines in the Data Sets"
    # create the plot subtitle
    plot.subtitle <- paste0(n.lines,
                            " data sets found.")
    # create the plot caption
    plot.caption <- labelAggregate(as.character(NROW(unique(df$data.file))),
                                 df$sw.version,
                                 made.by)
    
    # plot the results
    p <- ggplot(df,
                aes(x = year.of.measurement,
                    y = turbine.height,
                    size = turbine.dia,
                    fill = sw.version,
                    group = sw.version)) + 
      geom_point(alpha = 0.25,
                 colour ='black',
                 shape = 21) +
      geom_jitter(position = position_jitter(width = .125),
                  colour ='black',
                  shape = 21,
                  alpha = 0.25) + 
      scale_size_continuous(breaks=c(40, 60, 80, 100, 120),
                            range = c(min(df$turbine.dia,na.rm=TRUE)/10, 
                                      max(df$turbine.dia,na.rm=TRUE)/10),
                            name = "Diameter (m)") +
      labs(x = "Year of Measurement",
           y = "Turbine Hub Height (m)") +
      labs(title = plot.title) +
      labs(subtitle = plot.subtitle) +
      labs(caption=plot.caption) 
    
    if (sw.version.logic == "equals"){
      p <- p + scale_fill_discrete(guide=FALSE)
    } else {
      p <- p + scale_fill_discrete(name = "Software\nVersion")
    }
    
    #+ fig.height = 4, fig.width = 6
    print(p)
    
    if (sw.version == ""){
      filename = paste0("TurbineYearSizeDia_allSWversions.png")
    } else {
      filename = paste0("TurbineYearSizeDia_SWVersion",
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
    message("No suitable data found with the requested software version")
  }
  
  # turn warnings back on
  options(warn = oldw)
}