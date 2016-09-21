PlotAllDataSuppliers <- function(df,
                                 sw.version = "",
                                 sw.version.logic = "equals",
                                 ouput.dir = file.path(getwd(),'analysis','all'),
                                 made.by = ""){
  
  # plots summary of who contributed data
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
  
  # get the maximum number of tests for any one data supplier
  nmax <- max(aggregate(cbind(count = data.type)~data.supplier.type,
                        data = df[!(is.na(df$data.supplier.type)),],
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
      (any(nchar(df$data.supplier.type)>0,na.rm = TRUE))){
    
    # get the number of datasets
    n.lines <- NROW(unique(df$data.file))
    # create the plot title
    plot.title <- "Data Suppliers"
    # create the plot subtitle
    plot.subtitle <- paste0(n.lines,
                            " data sets found.")
    # create the plot caption
    plot.caption <- labelAggregate(as.character(NROW(df)),
                                 df$sw.version,
                                 made.by)
    # plot
    p <- ggplot(data = df,
                aes(x = data.supplier.type)) +
      geom_bar(stat="count") +
      scale_x_discrete(drop = FALSE,
                       name = "Data Suppliers") +
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
    
    # need to generate plot for knitr to work.
    print(p)
    
    if (sw.version == ""){
      filename = paste0("DataSources_allSWversions.png")
    } else {
      filename = paste0("DataSources_SWVersion",
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
    message("No usable data found with the requested software version")
  }
  
  # turn warnings back on
  options(warn = oldw)
}