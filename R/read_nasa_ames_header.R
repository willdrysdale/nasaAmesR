#' Read NASA Ames header
#'
#' Reads and extracts file information from NASA Ames headers formatted as described in:
#' http://cedadocs.ceda.ac.uk/73/4/index.html
#' Returns a list of header length, flags, file date and column names for use with other
#' file readers.
#'
#' @param file  NASA Ames file
#'
#' @return List of header length, flags, date and column names
#' @author Freya Squires
#'
#' @export

read_nasa_ames_header <- function(file){

  # return the header length
  hl <- readLines(file, n = 1) |> 
    stringr::str_replace_all("\t"," ") |> 
    stringr::str_split(" ") |> 
    unlist() |> 
    as.numeric() |> 
    stats::na.omit() |> 
    purrr::pluck(1)

  # Read the header
  # Commonly in NASA Ames files tested the header length given does not include the
  # column names so header length is 1 more than the length given in the file
  header<- base::readLines(file,n = hl+1)

  # get header shape info
  # number of unique elements per line of header
  header_element_length = stringr::str_split(header,"\\s+")  |> 
    purrr::map_int(length)

  # header element length will be 1 and can be coerced to numeric class if it is a number
  # defining the length of comments in a header. If these are pulled out we can split
  # header into useful elements
  suppressWarnings({
    comment_lengths = header[header_element_length == 1]  |> 
      as.numeric() |> 
      stats::na.omit()
  })
  
  suppressWarnings({
    numericHeader = as.numeric(header)
  })
  
  #Which lines describe number of lines in each section.
  linebreaks <- which(numericHeader %in% comment_lengths)

  #get date column name
  datename <- header[linebreaks[2]-1]

  #second index of linebreaks should include comments which describe which each column
  #of data is. First and second comments after the number stating how many variables
  #are in the file give a scaling factor and a missing flag.
  longnames <- header[seq(linebreaks[2]+3, linebreaks[2]+2+comment_lengths[2])]

  longnames <- c(datename, longnames)

  #return the misssing data flags
  data_flags <- header[linebreaks[2]+2]

  # date appears on line 7 according to CEDA convention - nervous about this!!
  date_row <- 7

  #select and parse date
  date = header[date_row]  |> 
    stringr::str_split(" ")

  date = lubridate::ymd(paste(date[[1]][1:3],collapse = ""),tz = "UTC")
  
  # Special Comments
  
  special_comments = header[(hl-sum((comment_lengths[3:4]))):(hl-(comment_lengths[4]+1))]
  
  # Normal Comments
  
  normal_comments = header[(hl-(comment_lengths[4]-1)):hl]
  
  # Return
  list(date = date,
       data_flags = data_flags,
       long_names = longnames,
       header_length = hl,
       special_comments = special_comments,
       normal_comments = normal_comments
       )

}
