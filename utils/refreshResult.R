refreshResult <- function(cdb,ccc){

  ## Result immer neu anfangen!!
  ccc$Calibration$Result <- list()
  ccc$Calibration$Result$Table <- list()

  ccc$Calibration$Result$Maintainer <- toString(Sys.getenv("USER"))
  ## demnächst über eine Sys.--- function
  ## done!

    ccc$Calibration$Result$Date   <-
      list(
           Type="calculated",
           Value=toString(format(Sys.time(), "%Y-%m-%d %H:%M" ) )
           )

  return( ccc )
}
