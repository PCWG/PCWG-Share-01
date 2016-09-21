PlotAllErrorsByWSBin_Lines <- function(df,
                                       data.range = "all",
                                       sw.version = "",
                                       sw.version.logic = "equals",
                                       ouput.dir = file.path(getwd(),'analysis','all'),
                                       filename.suffix = ""){
  # plots errors in a PCWG Share 01 file by normalized wind speed bin
  #
  # Args:
  # *.error summary of errors
  #
  # Returns:
  # a ggplot2 plot item
  
  require(ggplot2)
  require(RColorBrewer)
  
  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # filter by Range (all, inner, outer)
  df <- df[(df$range == data.range),]
  
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
    
    # create the plot caption
    plot.caption <- labelAggregate(as.character(NROW(unique(df$data.file))),
                                   df$sw.version,
                                   made.by)
    
    # create a bin label
    df$x.label <- factor(x = paste0(df$x.min, "-",df$x.max),
                         ordered = TRUE)
    
    # work out if the errors are 
    
    # figure out the corrections that we have
    corrections = levels(df$correction)
    
    for (correction in corrections){
      # subset the data
      sub <- df[((df$correction == correction) &
                   (df$error.name == "NME")),]
      sub <- sub[!is.na(sub$error.val.pc),]
      
      # figure out how many series we have to plot
      n.lines <- NROW(unique(sub$data.file))
      lines.palette <- colorRampPalette(brewer.pal(8,"Paired"))(n.lines)
      
      # plot boxplots
      plot.title <- paste0("Error By Wind Speed Bin for ",
                           capFirst(data.range),
                           " Data")
      plot.subtitle <- paste0("Using ",
                              correction,
                              ". ",
                              n.lines,
                              " data sets found.")
      
      if(all((sub$error.val.pc)==0)){
        # generate dummy data
        dummy <- data.frame(data.file = "No data",
                            x.label = factor(x = levels(df$x.label),
                                             levels = levels(df$x.label),
                                             ordered = TRUE),
                            error.val.pc = 0,
                            range = factor(data.range,
                                           levels = levels(df$range)),
                            correction = factor(correction,
                                                levels = levels(df$correction)))
        
        # no data to plot; need empty axes
        # working on https://github.com/hadley/ggplot2/pull/1582; requires the development version of ggplot2
        p <- ggplot(data = dummy,
                    aes(x = x.label,
                        y = error.val.pc)) + 
          geom_hline(yintercept = 0) +
          geom_blank() + 
          scale_x_discrete(name = "Normalized Wind Speed (binned)",
                           drop = FALSE) +
          scale_y_continuous(name = "Normalized Mean Error (Predicted - Actual, %)",
                             limits =c(-15,15)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = plot.title) +
          labs(subtitle = plot.subtitle) + 
          labs(caption=plot.caption)
      } else {
        p <- ggplot(data = sub,
                    aes(x = x.label,
                        y = error.val.pc,
                        group = data.file,
                        colour = data.file)) + 
          geom_hline(yintercept = 0) +
          geom_line() + 
          scale_color_manual(values = lines.palette) +
          guides(colour = FALSE) +
          scale_x_discrete(name = "Normalized Wind Speed (binned)",
                           drop = FALSE) +
          scale_y_continuous(name = "Normalized Mean Error (Predicted - Actual, %)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = plot.title) +
          labs(subtitle = plot.subtitle) + 
          labs(caption=plot.caption)
      }
      
      #+ fig.height = 4, fig.width = 6
      print(p)
      
      if (sw.version == ""){
        filename = paste0("AllErrorsByWSBin_Lines_", 
                          correction,
                          "_",
                          data.range,
                          "_allSWversions",
                          filename.suffix,
                          ".png")
      } else {
        filename = paste0("AllErrorsByWSBin_Lines_", 
                          correction,
                          "_",
                          data.range,
                          "_SWVersion",
                          sw.version.logic,
                          sw.version,
                          filename.suffix,
                          ".png")
      }
      # save the figure
      ggsave(filename = file.path(output.dir,
                                  filename),
             plot = p,
             width = 6, 
             height = 4, 
             units = "in", 
             dpi = 300)
    }
  } else {
    message("No data found with the requested software version")
  }
  
  # turn warnings back on
  options(warn = oldw)
}