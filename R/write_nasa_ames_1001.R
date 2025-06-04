#' Write NASA Ames FFI 1001
#'
#' @description {
#' Creates a NASA Ames 1001 file in the format \cr
#' \cr
#' \code{HL FFI} Header Length -  File Format Index (supports 1001 only) \cr
#' \code{ONAME} Name of File Originator \cr
#' \code{ORG} Name of Organisation \cr
#' \code{SNAME} Name of Instrumentation \cr
#' \code{MNAME} Campagin Name \cr
#' \code{IVOL NVOL} Data Volume number - total number of volumes \cr
#' \code{DATE RDATE} Date of data collection - data of file creation \cr
#' \code{DX} Interval in independant variable, 0 if interval is not consistant
#' \code{XNAME} Name of independant variable \cr
#' \code{NV} number of primary variables  \cr
#' \code{VSCAL1...VSCALn} scaling factors for variables \cr
#' \code{VMISS1...VMISSn} missing flag for variables \cr
#' \code{VNAME1} first long name of variables \cr
#' \code{...}  \cr
#' \code{VNAMEn} last long name of variables \cr
#' \code{NSCOML} number of special comment lines \cr
#' \code{SCOM1} first special comment line \cr
#' \code{...} \cr
#' \code{SCOMn} last special comment line \cr
#' \code{NNCOML} number of normal comment lines \cr
#' \code{NCOM1} first normal comment line \cr
#' \code{...} \cr
#' \code{NCOMn} last normal comment line \cr
#' }
#'
#' @param d data frame of data to have header attached, column names should be simple, no need to match long names in header
#' @param ONAME string containing lastname, firstname of file originator max length 132 characters
#' @param ORG string containing name of organisation max length 132 characters
#' @param SNAME string containing name of instrumentation max length 132 characters
#' @param MNAME string containing name of mission/campaign/project max length 132 characters
#' @param IVOL integer This files volume index
#' @param NVOL integer Total number of files in volume (i.e. files that share ONAME, ORG, SNAME and MNAME)
#' @param DATE string of date when data was taken in format to be parsed by \code{lubridate::ymd()}
#' @param DX interval of independent variable, zero if not consistent
#' @param XNAME string containing name of independent variable max length 132 characters
#' @param X_col column number containing independent variable
#' @param NV_col integer or vector of integers corresponding to column number containing primary variables
#' @param VSCALn integer or vector of integers corresponding to scaling factors of primary variables, default all 1
#' @param VMISSn integer or vector of integers corresponding to missing values of primary variables, default all 99999
#' @param VNAMEn string or vector of string containing long names of primary variables, each should not exceed 132 characters
#' @param SCOMn string or vector of string containing each special comment line
#' @param NCOMn string or vector of string containing each normal comment line
#' @param file save file here
#' @param col.names include the column headers on the data table T/F. If true they will be added as the final normal comments line
#'
#' @author W. S. Drysdale
#' @export

write_nasa_ames = function(d,
                           ONAME,
                           ORG,
                           SNAME,
                           MNAME,
                           IVOL = 1,
                           NVOL = 1,
                           DATE,
                           DX,
                           XNAME,
                           X_col = 1,
                           NV_col,
                           VSCALn = NULL,
                           VMISSn = NULL,
                           VNAMEn,
                           SCOMn = NULL,
                           NCOMn = NULL,
                           file,
                           col.names = FALSE
){

  #Check strings don't exceed 132 characters
  if(nchar(ONAME) > 132){
    stop("ONAME is longer than 132 characters")
  }

  if(nchar(ORG) > 132){
    stop("ORG is longer than 132 characters")
  }

  if(nchar(SNAME) > 132){
    stop("SNAME is longer than 132 characters")
  }

  if(nchar(MNAME) > 132){
    stop("MNAME is longer than 132 characters")
  }

  if(nchar(XNAME) > 132){
    stop("XNAME is longer than 132 characters")
  }

  for(i in 1:length(VNAMEn)){
    if(nchar(VNAMEn[i]) > 132)
      stop(paste0("A VNAMEn:", VNAMEn[i], " is longer than 132 characters"))
  }

  #format date
  DATE = stringr::str_replace_all(as.character(lubridate::ymd(DATE)), "-", " ")
  RDATE = stringr::str_replace_all(as.character(Sys.Date()), "-", " ")

  #Collect number of variable line lengths
  NSCOM = length(SCOMn)
  NNCOM = length(NCOMn)

  if(col.names)
    NNCOM = NNCOM + 1

  NV = length(NV_col)
  if((ncol(d) - 1) != NV){
    stop("Number of VNAMEn entries does not match number of primary variables (NV or NV_col)")
  }

  #If no scale factors are supplied, set them all to 1
  if(length(VSCALn) == 0){
    VSCALn = rep(1, NV)
  }

  #If no missing value values are supplied, set them all to 99999
  if(length(VMISSn) == 0){
    VMISSn = rep(99999999, NV)
  }

  #If VSCALn,VMISSn,VNAMEn,NV_col lengths dont match, stop
  if(!all(sapply(list(length(VSCALn), length(VMISSn), length(VNAMEn)), FUN = identical, NV))){
    stop("VSCALn,VMISSn,VNAMEn,NV_col lengths differ")
  }

  #Determine Header Length
  HL = 14 + NSCOM + NNCOM + NV

  #open connection to output file
  data_file = file(file, open = "wt")

  #close on exit
  on.exit(close(data_file))

  #write file
  writeLines(paste0(HL, " 1001"), con = data_file)
  writeLines(ONAME, con = data_file)
  writeLines(ORG, con = data_file)
  writeLines(SNAME, con = data_file)
  writeLines(MNAME, con = data_file)
  writeLines(paste(IVOL, NVOL, sep = " "), con = data_file)
  writeLines(paste(DATE, RDATE, sep = " "), con = data_file)
  writeLines(paste0(DX), con = data_file)
  writeLines(XNAME, con = data_file)
  writeLines(paste0(NV), con = data_file)
  writeLines(paste(VSCALn, collapse = " "), con = data_file)
  writeLines(paste(VMISSn, collapse = " "), con = data_file)

  for(i in 1:length(VNAMEn)){
    writeLines(VNAMEn[i], con = data_file)
  }

  writeLines(paste0(NSCOM), con = data_file)

  if(NSCOM != 0){
    for(i in 1:length(SCOMn))
      writeLines(SCOMn[i], con = data_file)
  }

  writeLines(paste0(NNCOM), con = data_file)

  if(!is.null(NCOMn)){ # test is.null rather than NNCOM == 0, col.names == T; NNCOM is 1, even if NCOMn is null
    for(i in 1:length(NCOMn))
      writeLines(NCOMn[i], con = data_file)
  }

  #suppressWarnings(
  write.table(d,
              data_file,
              append = T,
              row.names = F,
              col.names = col.names,
              quote = F)
  #  )
}
