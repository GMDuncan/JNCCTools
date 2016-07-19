#' randomWeightedPoints
#'
#' @author Graeme Duncan
#' @param inputProbRaster An r RasterLayer containing values 0-1 determining 'weighting' of final point distribution. Areas with higher values are more likely to have a point allocated.
#' @param vectorMask An r SpatialPolygonsDataFrame containing a vector mask polygon inside of which points will be created.
#' @param numRequiredPoints Number (integer) of required points in the output.
#' @param minDist Number describing the minimum distance allowed between points in the output.
#' @param absMinDist (Optional) Number. If used, value should be lower than minDist and enables the inputProbRaster to also affect minimum point spacing. Areas of higher raster values will have minimum distances tending towards this value.
#' @param rasterPower (Optiona) Number. If used, sets the influence of the inputProbRaster in determining probability. In levels of power. Defaults to 1.
#' @details This function creates random points within a vector mask with distribution weighted dependending on an underlying raster of values 0 to 1. Users can determine the input probability raster, vector mask, number of desired output points, (high and low) minimum point spacing and the overall influence of the raster layer. ALL INPUTS MUST BE IN THE SAME PROJECTED CRS. Outputs an r SpatialPointsDataFrame object.
#' @export
#'


randomWeightedPoints <- function(inputProbRaster, vectorMask, numRequiredPoints, minDist, absMinDist, rasterPower){
  library(raster)
  library(rgdal)
  library(sp)
  library(rgeos)
  #Value missing catch, allows absolute minimum distance to be optional. If not set, uses minDist
  pb <- txtProgressBar(min=0,max=numRequiredPoints,initial=0, width = NA, title = "Point generation progress", label = "Number of points generated")
  if(missing(absMinDist)){
    absMinDist <- minDist
  }
  if(missing(rasterPower)){
    rasterPower <- 1
  }
  #generate initial empty SPDF
  outputPoints <- spsample(vectorMask,1,"random")[F,]
  
  #make a nice plot
  plot(inputProbRaster)
  lines(vectorMask)
  
  #breaks when we have enough points
  while(length(outputPoints) < numRequiredPoints){
    selectedPoint <- spsample(vectorMask,1,"random", iter=10)
    pointVal <- extract(inputProbRaster, selectedPoint)
    #Check if point is within threshold distance from already extracted points
    if (length(outputPoints) > 0){
      #Scales the allowed minimum distance based on the probability raster (i.e high probability areas allow closer points)
      #If no absolute minimum distance set, results in standard minimum distance due to missing argument catch at top of function
      selectedMinDist <- absMinDist + (minDist-absMinDist)*(1-pointVal)
      distCheck <- gDistance(selectedPoint, outputPoints)
      if (distCheck > selectedMinDist){
        #See if we should move it across
        if (0.5*(pointVal^rasterPower) > runif(1,0,1)){
          #Move the point to the output data frame
          outputPoints <- rbind(outputPoints, selectedPoint)
          points(selectedPoint, pch="+")
          setTxtProgressBar(pb, length(outputPoints))
          #print(sprintf("Generated %.0f out of %.0f points",length(outputPoints),numRequiredPoints))
        }
      }  
    } else {
      if (0.5*(pointVal^rasterPower) > runif(1,0,1)){
        #Move the point to the output data frame
        outputPoints <- rbind(outputPoints, selectedPoint)
        points(selectedPoint, pch="+")
        setTxtProgressBar(pb, length(outputPoints))
        #print(sprintf("Generated %.0f out of %.0f points",length(outputPoints),numRequiredPoints))
      }
    } 
  }
  df <- data.frame(STATION=row.names(outputPoints),row.names=row.names(outputPoints))
  return(SpatialPointsDataFrame(outputPoints, df))
}
