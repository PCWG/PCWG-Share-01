ReadMetaData <- function(wb){
  # reads a PCWG Share 01 file and returns meta data about the submission
  #
  # Args:
  # data.file: name of the data file to be read
  #
  # Returns:
  # data.frame containing data from the "Meta Data" tab
  
  require(XLConnect)
  # initalize the data frame
  data.meta = data.frame(data.type = NullToNA(readWorksheet(wb,
                                                            sheet = "Meta Data",
                                                            region = "C8",
                                                            simplify = TRUE,
                                                            header = FALSE),
                                              xAlt = "Not Reported"),
                         REWS.n.heights = NullToNA(readWorksheet(wb,
                                                                 sheet = "Meta Data",
                                                                 region = "C9",
                                                                 simplify = TRUE,
                                                                 header = FALSE,
                                                                 colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         REWS.inc.veer = NullToNA(readWorksheet(wb,
                                                                sheet = "Meta Data",
                                                                region = "C10",
                                                                simplify = TRUE,
                                                                header = FALSE)),
                         inner.range.def = NullToNA(readWorksheet(wb,
                                                                  sheet = "Meta Data",
                                                                  region = "C11",
                                                                  simplify = TRUE,
                                                                  header = FALSE)),
                         Site.outline.class = NullToNA(readWorksheet(wb,
                                                                     sheet = "Meta Data",
                                                                     region = "C12",
                                                                     simplify = TRUE,
                                                                     header = FALSE)),
                         Site.forestry.class = NullToNA(readWorksheet(wb,
                                                                      sheet = "Meta Data",
                                                                      region = "C13",
                                                                      simplify = TRUE,
                                                                      header = FALSE)),
                         Site.IEC.class = NullToNA(readWorksheet(wb,
                                                                 sheet = "Meta Data",
                                                                 region = "C14",
                                                                 simplify = TRUE,
                                                                 header = FALSE)),
                         Geography.latitude = NullToNA(readWorksheet(wb,
                                                                     sheet = "Meta Data",
                                                                     region = "C15",
                                                                     simplify = TRUE,
                                                                     header = FALSE,
                                                                     colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         Geography.continent = NullToNA(readWorksheet(wb,
                                                                      sheet = "Meta Data",
                                                                      region = "C16",
                                                                      simplify = TRUE,
                                                                      header = FALSE)),
                         Geography.country = NullToNA(readWorksheet(wb,
                                                                    sheet = "Meta Data",
                                                                    region = "C17",
                                                                    simplify = TRUE,
                                                                    header = FALSE)),
                         Geography.elevation = NullToNA(readWorksheet(wb,
                                                                      sheet = "Meta Data",
                                                                      region = "C18",
                                                                      simplify = TRUE,
                                                                      header = FALSE,
                                                                      colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         Meas.IEC61400 = NullToNA(readWorksheet(wb,
                                                                sheet = "Meta Data",
                                                                region = "C19",
                                                                simplify = TRUE,
                                                                header = FALSE)),
                         Meas.ano.type = NullToNA(readWorksheet(wb,
                                                                sheet = "Meta Data",
                                                                region = "C20",
                                                                simplify = TRUE,
                                                                header = FALSE)),
                         Meas.ano.heated = NullToNA(readWorksheet(wb,
                                                                  sheet = "Meta Data",
                                                                  region = "C21",
                                                                  simplify = TRUE,
                                                                  header = FALSE)),
                         Meas.turb.type = NullToNA(readWorksheet(wb,
                                                                 sheet = "Meta Data",
                                                                 region = "C22",
                                                                 simplify = TRUE,
                                                                 header = FALSE)),
                         Meas.power.type = NullToNA(readWorksheet(wb,
                                                                  sheet = "Meta Data",
                                                                  region = "C23",
                                                                  simplify = TRUE,
                                                                  header = FALSE)),
                         turbine.dia = NullToNA(readWorksheet(wb, 
                                                              sheet = "Meta Data",
                                                              region = "C24",
                                                              simplify = TRUE,
                                                              header = FALSE,
                                                              colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         turbine.height = NullToNA(readWorksheet(wb, 
                                                                 sheet = "Meta Data",
                                                                 region = "C25",
                                                                 simplify = TRUE,
                                                                 header = FALSE,
                                                                 colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         turbine.spec.power = NullToNA(readWorksheet(wb,
                                                                     sheet = "Meta Data",
                                                                     region = "C26",
                                                                     simplify = TRUE,
                                                                     header = FALSE,
                                                                     colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         turbine.control.type = NullToNA(readWorksheet(wb,
                                                                       sheet = "Meta Data",
                                                                       region = "C27",
                                                                       simplify = TRUE,
                                                                       header = FALSE)),
                         year.of.measurement = NullToNA(readWorksheet(wb,
                                                                      sheet = "Meta Data",
                                                                      region = "C28",
                                                                      simplify = TRUE,
                                                                      header = FALSE,
                                                                      colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         year.of.first.operation = NullToNA(readWorksheet(wb,
                                                                          sheet = "Meta Data",
                                                                          region = "C29",
                                                                          simplify = TRUE,
                                                                          header = FALSE,
                                                                          colTypes = c(XLC$DATA_TYPE.NUMERIC))),
                         Meas.timezone = NullToNA(readWorksheet(wb,
                                                                sheet = "Meta Data",
                                                                region = "C28",
                                                                simplify = TRUE,
                                                                header = FALSE)),
                         stringsAsFactors=FALSE)
}

# note that there are some problems with XLConnect on a mac: see 
# https://github.com/s-u/rJava/issues/37 for details of how to fix them
# (download the Apple Java 1.6 release and install it)