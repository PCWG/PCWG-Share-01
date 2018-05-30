ReadErrorData <- function(wb,
                          sw.version,
                          data){

  sheets = c("Baseline",
            "Den & Turb",
            "Den & Aug Turb (Relaxed)",
            "Den & 2D PDM",
            "Den & 3D PDM",
            "Den & REWS (S)",
            "Den & REWS (S+V)",
            "Den & REWS (S+V+U)",
            "Den & RAWS (S)",
            "Den & RAWS (S+V)",
            "Den & RAWS (S+V+U)",
            "Den & P by H",
            "Den & REWS (S) & Turb",
            "Den & REWS (S+V) & Turb",
            "Den & REWS (S+V+U) & Turb"
            )

  # get errors
  by.WS <- NULL
  by.TOD <- NULL
  by.CM <- NULL
  by.WD <- NULL
  by.Range <- NULL
  by.4CM <- NULL
  for (sheet in sheets){
    
    if (existsSheet(wb, sheet)){
  
      print(paste("Found: ", sheet))
      
      # by wind speed
      by.WS <- rbind(by.WS,
                     data.frame(ExpandErrorByWSDF(ReadWSErrorData(wb,sheet,data),
                                                  sheets),
                                range = "all"))
  
      # Add error binned by WS in the inner and outer range (only for version > 0.5.8)
      if (compareVersion(VersionStr(sw.version),
                         "0.5.8") > 0){
        by.WS <- rbind(by.WS,
                       data.frame(ExpandErrorByWSDF(ReadWSInnerRangeErrorData(wb,sheet,data),
                                                    sheets),
                                  range = "Inner"),
                       data.frame(ExpandErrorByWSDF(ReadWSOuterRangeErrorData(wb,sheet,data),
                                                    sheets),
                                  range = "Outer"))
      }
      # by time of day
      by.TOD <- rbind(by.TOD,
                      ExpandErrorByTODDF(ReadTODErrorData(wb,sheet,sw.version,data),
                                         sheets))
      # by calendar month
      by.CM <- rbind(by.CM,
                     ExpandErrorByCMDF(ReadCMErrorData(wb,sheet,sw.version,data),
                                       sheets))
      # by wind direction
      by.WD <- rbind(by.WD,
                     ExpandErrorByWDDF(ReadWDErrorData(wb,sheet,sw.version,data),
                                       sheets))
      # by range
      by.Range <- rbind(by.Range,
                        ExpandErrorByRangeDF(ReadRangeErrorData(wb,sheet,sw.version,data),
                                             sheets))
      # from four-cell matrix
      by.4CM <- rbind(by.4CM,
                      ExpandErrorBy4CMDF(Read4CMErrorData(wb,sheet,sw.version,data),
                                         sheets))
    }else{
      print(paste("Not Found: ", sheet))
    }
    
  }
  
  # pack up the errors
  errors <- list(by.WS = data.frame(by.WS,
                                    sw.version),
                 by.TOD = data.frame(by.TOD,
                                     sw.version),
                 by.CM = data.frame(by.CM,
                                    sw.version),
                 by.WD = data.frame(by.WD,
                                    sw.version),
                 by.Range = data.frame(by.Range,
                                       sw.version),
                 by.4CM = data.frame(by.4CM,
                                     sw.version))
}