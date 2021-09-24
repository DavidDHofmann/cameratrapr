# ################################################################################
# #### TESTING
# ################################################################################
# library(cameratrapr)
# setwd("/home/david/Schreibtisch")
# cameras1 <- paste0("R", sprintf("%02d", 1:30))
# cameras2 <- paste0("S", sprintf("%02d", 1:30))
# cameras <- c(cameras1, cameras2)
# locations <- paste0("L", sprintf("%03d", 1:6))
# name <- "Garden"
# library(exifr)
# # dir <- "/home/david/Schreibtisch"
#
# # Create proejct folder
# cam_project(name, cameras = cameras, locations = locations)
# cam_move(project = "/home/david/Schreibtisch/Garden")

################################################################################
#### Load Dependencies
################################################################################
#' @importFrom exifr read_exif
#' @importFrom lubridate parse_date_time
#' @importFrom reticulate source_python
NULL

################################################################################
#### Level 1 Functions
################################################################################
#' Prepare folder Structure
#'
#' Function to create all necessary folders to work with the cameratrapr package
#' @export
#' @param name character. Name of the project. This will become the name of the
#' main folder
#' @param dir character. directory to the folder where the project should be
#' located
#' @param cameras character vector. CameraIDs that will be used for the study.
#' @param locations (OPTIONAL) character vector. LocationIDs at which the
#' cameras will be placed.
#' @examples
#' Coming
cam_project <- function(name = NULL, dir = NULL, cameras = NULL, locations = NULL){

  # Make sure a name is provided
  if (is.null(name)){
    stop("Please provide a project name")
  }

  # If no directory is provided, assume its going into the working directory
  if (is.null(dir)){
    dir <- getwd()
  }

  # Specify project folder
  main_folder <- file.path(dir, name)

  # Check if project already exists in the directory
  exists <- dir.exists(main_folder)

  # Break if already exists and should not be overwritten
  if (exists){
    stop("Project already exists")
  }

  # Create all required folders
  dir.create(main_folder, showWarnings = F)
  dir.create(file.path(main_folder, "Cameras"), showWarnings = F)
  dir.create(file.path(main_folder, "Locations"), showWarnings = F)

  # If cameras are provided, prepare camera folders already
  if (!is.null(cameras)){
    for (i in 1:length(cameras)){
      dir.create(file.path(main_folder, "Cameras", cameras[i]), showWarnings = F)
    }
  }

  # If locations are provided, prepare location folders already
  if (!is.null(locations)){
    for (i in 1:length(locations)){
      dir.create(file.path(main_folder, "Locations", locations[i]), showWarnings = F)
    }
  }

  # Create csv files that are required to collect data
  deployments <- .deploymentsCSV(file.path(dir, name), locations = locations)
  cameras     <- .camerasCSV(file.path(dir, name), cameras = cameras)

  # Print success message
  cat("Project successfully created in", main_folder)

}

#' Move files from the camera folders to the location folders and rename images
#' automatically
#'
#' This function takes the images assigned to each camera and copies them to the
#' folder of the location at which the camera was setup. For this, the
#' 'Deployments.csv' file has to be duly filled out.
#' @export
#' @param project character. Directory to the project folder.
#' @param overwrite logical. In case the renamed file already exists in the
#' locations folders. Should it be overwritten?
#' @param date_format character. Format in which the dates in the
#' 'Deployments.csv' file are entered. If not provided, it will be guessed.
#' @param tz character. Time zone of the dates. Set to UTC by default.
#' @examples
#' Coming
cam_move <- function(project = NULL, overwrite = F, date_format = NULL, tz = "UTC"){

  # If no project directory is provided, assume that it should be the working
  # directory
  if (is.null(project)){
    project <- getwd()
  }

  # Make sure the directory contains the correct folders
  check1 <- dir.exists(file.path(project, "Cameras"))
  check2 <- dir.exists(file.path(project, "Locations"))
  if (!check1 & !check2){
    stop("Project does not contain correct folders. Did you create the project
    using new_project?")
  }

  # Identify all files that need to be moved/renamed
  files <- dir(file.path(project, "Cameras")
    , recursive  = T
    , full.names = T
  )

  # If there are no files, stop
  if (length(files) == 0){
    stop("There are no files in the 'Cameras' folder or subfolders")
  }

  # Load the deployments.csv file
  deployments <- file.path(project, "Deployments.csv")
  if (file.exists(deployments)){
    deployments <- read.csv(deployments, stringsAsFactors = F)
  } else {
    stop("Could not find 'deployments.csv' file.")
  }

  # Try to parse the dates if no format is provided
  if (is.null(date_format)){
    cat("Parsing deployment dates...\n")
    deployments$StartDate <- parse_date_time(deployments$StartDate
      , order = c("dmYHMS", "mdY")
      , tz    = tz
      , quiet = T
    )
    deployments$EndDate <- parse_date_time(deployments$EndDate
      , order = c("dmYHMS", "mdY")
      , tz    = tz
      , quiet = T
    )
  } else {
    deployments$StartDate <- as.POSIXct(deployments$StartDate
      , format = date_format
      , tz     = tz
    )
    deployments$EndDate <- as.POSIXct(deployments$EndDate
      , format = date_format
      , tz     = tz
    )
  }

  # If there are no rows in this table, terminate
  if (nrow(deployments) == 0){
    stop("The 'Deployments.csv' file is empty. Cannot proceed without this
    information.")
  }

  # Create a dataframe with information for each image
  info <- data.frame(
      Filename         = basename(files)
    , Fileext          = unlist(lapply(files, .fileExt))
    , CameraID         = basename(dirname(files))
    , Path             = files
    , stringsAsFactors = F
  )

  # Also read the image metadata
  cat("Reading image metadata...\n")
  meta <- read_exif(info$Path)

  # Add relevant columns to the information table
  info$Timestamp <- as.POSIXct(meta$CreateDate, format = "%Y:%m:%d %H:%M:%S")
  info$Type <- meta$MIMEType

  # Subset to images/videos
  info <- info[info$Type %in% c("image/jpeg", "video/mp4"), ]

  # If there are no files, stop
  if (nrow(info) == 0){
    stop("There are no files that need to be copied in the 'Cameras' folder or
    subfolders")
  }

  # Determine the new location of each image
  for (i in 1:nrow(info)){

    # Subset to corresponding deployment
    sub <- subset(deployments
      , CameraID == info$CameraID[i]
      & StartDate <= info$Timestamp[i]
      & EndDate >= info$Timestamp[i]
    )

    # In case there are no corresponding rows, it could be that EndDate is not
    # set yet
    if (nrow(sub) == 0){
      sub <- subset(deployments
        , CameraID == info$CameraID[i]
        & StartDate <= info$Timestamp[i]
      )
    }

    # If there are still no corresponding rows, something has to be missing
    if (nrow(sub) == 0){
      warning("Could not find deployment period for image '", info$Path[i],
      "'. File will not be copied.")
    } else {
      info$LocationID[i] = sub$LocationID
    }
  }

  # Remove files for which the locationID is not known
  info <- info[!is.na(info$LocationID), ]

  # Stop if there are no files left
  if (nrow(info) == 0){
    stop("There are no files that need to be copied in the 'Cameras' folder or
    subfolders")
  }

  # Prepare new directories and filenames
  info$NewDirectory <- file.path(project, "Locations", info$LocationID, info$CameraID)
  info$NewFilename <- paste0(info$LocationID, "_", info$CameraID, "_", info$Timestamp, ".", info$Fileext)
  info$NewFilename <- gsub(info$NewFilename, pattern = " ", replacement = "")
  info$NewFilename <- gsub(info$NewFilename, pattern = "-", replacement = "")
  info$NewFilename <- gsub(info$NewFilename, pattern = ":", replacement = "")
  info$NewPath <- file.path(info$NewDirectory, info$NewFilename)

  # Check which of the files already exist
  info$Exists <- file.exists(info$NewPath)

  # If overwrite is not desired, remove existing files from info frame
  if (!overwrite){
    info <- info[!info$Exists, ]
  }

  # If no files are left, break
  if (nrow(info) == 0){
    stop("All files already exist in the output directories")
  }

  # Create missing directories
  missing <- unique(info$NewDirectory[!dir.exists(info$NewDirectory)])
  if (length(missing) != 0){
    cat("Creating missing directories...\n")
    for (i in 1:length(missing)){
      dir.create(missing[i], showWarnings = F, recursive = T)
    }
  }

  # Move and rename the files
  cat("Copying files to 'Locations' directory...\n")
  file.copy(from = info$Path, to = info$NewPath, overwrite = overwrite)

}

#' Convert video to jpeg
#'
#' Function to convert a video into frames
#' @export
#' @param filepath filepath(s) to the video file(s) that need to be converted
#' @param outdir character vector, either of length 1 or same length as
#' \code{file} directory or directories to the folder(s) where the converted
#' file(s) should be stored. By default, this is set to the directory of the
#' input file(s).
#' @param fps numeric frames per second that should be stored
#' @param overwrite logical should frames that already exist in the output
#' directory be overwritten?
#' @examples
#' Coming
video2pic <- function(file = NULL, outdir = NULL, fps = NULL, overwrite = F){

  # If no file is provided, stop
  if (is.null(file)){
    stop("Please provide at least one file")
  }

  # Check if all files exist
  exist <- file.exists(file)
  if (!all(exist)){
    stop("Some of the specified files do not exist")
  }

  # If no output directory is provided, set it to the directory of the input
  # file
  if (is.null(outdir)){
    outdir <- dirname(file)
  } else {
    if (!all(dir.exists(outdir))){
      stop("specificied output directory does not exist")
    }
  }

  # Make sure a correct number of directories is provided
  if (length(outdir) != length(file) & length(outdir) != 1){
    stop("outdir has to be of length 1 or same length as file")
  }

  # If a signle output directory is provided, repead it for all files
  if (length(outdir) == 1){
    outdir <- rep(outdir, length(file))
  }

  # If no fps is desired, stop
  if (is.null(fps)){
    stop("Please provide a valid fps")
  }

  # Source the python script
  pypath <- system.file("/python/video2pic.py", package = "cameratrapr")
  source_python(pypath)

  # Prepare progress bar
  cat("Extracting frames...\n")
  pb <- txtProgressBar(
      min     = 0
    , max     = length(file)
    , initial = 0
    , style   = 3
    , width   = 55
  )

  # Loop through each file and extract the frames
  for (i in 1:length(file)){

    # Check if output files alraedy exist
    base <- strsplit(basename(file[i]), split="\\.")[[1]][-2]
    base <- paste0(base, "_Frame_0.JPG")
    outname <- file.path(outdir[i], base)

    # If output already exists, skip
    if (file.exists(outname) & !overwrite){

      warning("Frames for ", basename(file)," already exist in the output directory... Skipping\n")
      next

    # Otherwise, run extraction
    } else {

      # Run the function
      video2pic_py(file[i], outdir[i], fps)

    }
    # Print progress bar update
    setTxtProgressBar(pb, i)
  }
  cat("\nExtracting done...\n")
}

# #' Install Python Dependencies
# #'
# #' Function to install the python dependencies that are required for this package
# #' @export
# #' @examples
# #' Coming
# cam_install <- function(method = "auto", conda = "auto") {
#   reticulate::py_install(c("sys", "argparse", "cv2", "math", "os", "numpy")
#     , method = method
#     , conda  = conda
#   )
# }

################################################################################
#### Level 2 Functions
################################################################################
# Helper function to create the deployments csv file
.deploymentsCSV <- function(dir = NULL, locations = NULL){

  # Prepare an empty dataframe
  if (!is.null(locations)) {
    deployments <- data.frame(
        LocationID = locations
      , CameraID        = NA
      , CardID          = NA
      , StartDate       = NA
      , EndDate         = NA
      , Height          = NA
      , HorizontalAngle = NA
      , VerticalAngle   = NA
      , MountingMethod  = NA
      , Notes           = NA
    )
  } else {
    deployments <- na.omit(data.frame(
        LocationID      = NA
      , CameraID        = NA
      , CardID          = NA
      , StartDate       = NA
      , EndDate         = NA
      , Height          = NA
      , HorizontalAngle = NA
      , VerticalAngle   = NA
      , MountingMethod  = NA
      , Notes           = NA
    ))
  }

  # Write it to file
  filename <- file.path(dir, "Deployments.csv")
  write.csv(deployments, file = filename, row.names = F)
}

# Helper function to create the cameras csv file
.camerasCSV <- function(dir = NULL, cameras = NULL){

  # Prepare an empty dataframe
  if (!is.null(cameras)) {
    cameras <- data.frame(
        CameraID     = cameras
      , ModelType    = NA
      , ModelNo      = NA
      , DateAcquired = NA
      , Notes        = NA
    )
  } else {
    cameras <- na.omit(data.frame(
        CameraID     = NA
      , ModelType    = NA
      , ModelNo      = NA
      , DateAcquired = NA
      , Notes        = NA
    ))
  }

  # Write it to file
  filename <- file.path(dir, "Cameras.csv")
  write.csv(cameras, file = filename, row.names = F)
}

# Helper function to determine the file extension
.fileExt <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
}
