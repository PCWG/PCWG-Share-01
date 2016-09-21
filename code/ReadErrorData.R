ReadErrorData <- function(wb,
                          sw.version){
  
  sheets = c("Baseline",
             "REWS",
             "Turbulence Correction",
             "REWS & Turbulence Correction",
             "Power Deviation Matrix")
  
  # get errors
  by.WS <- NULL
  by.TOD <- NULL
  by.CM <- NULL
  by.WD <- NULL
  by.Range <- NULL
  by.4CM <- NULL
  for (sheet in sheets){
    # by wind speed
    by.WS <- rbind(by.WS,
                   data.frame(ExpandErrorByWSDF(ReadWSErrorData(wb,sheet),
                                                sheets),
                              range = "all"))
    # Add error binned by WS in the inner and outer range (only for version > 0.5.8)
    if (compareVersion(VersionStr(sw.version),
                       "0.5.8") > 0){
      by.WS <- rbind(by.WS,
                     data.frame(ExpandErrorByWSDF(ReadWSInnerRangeErrorData(wb,sheet),
                                                  sheets),
                                range = "Inner"),
                     data.frame(ExpandErrorByWSDF(ReadWSOuterRangeErrorData(wb,sheet),
                                                  sheets),
                                range = "Outer"))
    }
    # by time of day
    by.TOD <- rbind(by.TOD,
                    ExpandErrorByTODDF(ReadTODErrorData(wb,sheet,sw.version),
                                       sheets))
    # by calendar month
    by.CM <- rbind(by.CM,
                   ExpandErrorByCMDF(ReadCMErrorData(wb,sheet,sw.version),
                                     sheets))
    # by wind direction
    by.WD <- rbind(by.WD,
                   ExpandErrorByWDDF(ReadWDErrorData(wb,sheet,sw.version),
                                     sheets))
    # by range
    by.Range <- rbind(by.Range,
                      ExpandErrorByRangeDF(ReadRangeErrorData(wb,sheet,sw.version),
                                           sheets))
    # from four-cell matrix
    by.4CM <- rbind(by.4CM,
                    ExpandErrorBy4CMDF(Read4CMErrorData(wb,sheet,sw.version),
                                       sheets))
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