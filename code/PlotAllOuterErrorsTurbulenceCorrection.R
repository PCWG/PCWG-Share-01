PlotAllOuterErrorsTurbulenceCorrection <- function(df,
                                                   sw.version = "",
                                                   sw.version.logic = "equals",
                                                   output.dir = getwd()){
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
    
    # create the plot labels
    plot.title <- "Outer Range Normalized Error"
    n.lines <- NROW(unique(df$data.file))
    plot.subtitle <- paste0(n.lines, " data sets found.")
    plot.caption <- labelAggregate(as.character(NROW(unique(df$data.file))),
                                   df$sw.version,
                                   made.by)
    
    # get the data we want
    df <- df[((df$correction == "Baseline") | (df$correction == "Turbulence Correction")) &
               (df$range == "Outer") & 
               (df$error.name == "NME"),]
    
    df$error.val.pc.cut <- cut(x = df$error.val.pc,
                               breaks = c(-100,
                                          seq(from = -2.75,
                                              to = 2.75,
                                              by = 0.5),
                                          100),
                               labels = c("< -2.75",
                                          paste(seq(from = -2.5,
                                                    to = 2.5,
                                                    by = 0.5)),
                                          ">2.75"))
    
    # plot the NME
    p <- ggplot(data = as.data.frame(table(df$error.val.pc.cut,
                                           droplevels(df$correction))),
                aes(x = Var1,
                    y = Freq,
                    fill = Var2)) +
      geom_vline(aes(xintercept = which(levels(Var1) %in% '0'))) +
      geom_bar(stat = 'identity',
               position = "dodge") +
      scale_x_discrete(drop = FALSE,
                       name = "Error (Predicted - Actual, %)") +
      scale_y_continuous(name = "Count") +
      scale_fill_brewer(type = "qual",
                        palette= 7,
                        drop = TRUE,
                        name = "Corrections") +
      labs(title = plot.title) +
      labs(subtitle = plot.subtitle) + 
      labs(caption=plot.caption)
    
    
    print(p)
    
    if (sw.version == ""){
      filename = paste0("OuterRangeTurbulenceCorrectionHistogram_allSWversions.png")
    } else {
      filename = paste0("OuterRangeTurbulenceCorrectionHistogram_SWVersion",
                        sw.version.logic,
                        sw.version,
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
    
  } else {
    message("No data found with the requested software version")
  }
  
  # turn warnings back on
  options(warn = oldw)
  
}