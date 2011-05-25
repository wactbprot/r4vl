refreshAnalysis <- function(cdb,ccc){


  ## Analysis immer neu anfangen!!
  ccc$Calibration$Analysis <- list()
  ccc$Calibration$Analysis$Values <- list()

  ccc$Calibration$Analysis$Maintainer <- toString(Sys.getenv("USER"))
  ## demnächst über eine Sys.--- function
  ## done!

    ccc$Calibration$Analysis$Date   <-
      list(
           Type="calculated",
           Value=toString(format(Sys.time(), "%Y-%m-%d %H:%M" ) )
           )

  return( ccc )
}
