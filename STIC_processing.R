##### setup #####
library(tidyverse)
library(lubridate)
library(zoo)
library(devtools)

# generating a custom theme to get rid of the ugly ggplot defaults 
theme_cust <- function(base_size = 11, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black")
    )
}

##### loading Sam Zipper's STICKr package 

install_github("samzipper/STICr")
library(STICr)

#id <- "0B-wuZ2XMFIBUd09Ob0pKVkRzQTA" # google file ID
#read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

#FT4_STIC <- read.csv("FT4_T_2022.csv")
#FT4_STIC <- tidy_hobo_data("FT4_T_2022.csv",outfile= FALSE)


tidy_hobo_data <- function(infile, outfile = FALSE) {
# read in file
raw_data <- read.csv(infile,
                     skip = 1)

# tidy columns
tidy_data <-
  raw_data |>
  dplyr::rename_with(.cols = contains("Temp"),
                     .fn = function(x){"temperature"}) |>
  dplyr::rename_with(.cols = contains("Lux"),
                     .fn = function(x){"conductivity_uncal"}) |>
  dplyr::rename_with(.cols = contains("Date"),
                     .fn = function(x){"datetime"}) |>
  dplyr::select(datetime, conductivity_uncal, temperature) |>
  dplyr::mutate(datetime = lubridate::mdy_hm(datetime),
                temperature = as.numeric(temperature),
                conductivity_uncal = as.numeric(conductivity_uncal))

# save data if needed
if (outfile != FALSE) {
  write.csv(tidy_data, outfile, row.names = FALSE)
}
return(tidy_data)
}

classify_wetdry <- function(stic_data, classify_var = "spc", threshold = 200, wetval = 1, dryval= 0) {
  
  # extract classify variable
  if (!(classify_var %in% names(stic_data))) stop(paste0("classify_var input (", classify_var, ") is not present in stic_data"))
  class_var <- stic_data[ ,classify_var]
  
  # classify and add to data frame
  stic_data$wetdry <- if_else(class_var >= threshold, wetval, dryval)
  
  return(stic_data)
}

FT4_STIC <-tidy_hobo_data("FT4_T_2022.csv", outfile = FALSE)
FT4_STIC <- classify_wetdry(FT4_STIC, classify_var = "conductivity_uncal", threshold = 5000, wetval = 1.1, dryval = 0.1)

write.csv(FT4_STIC,"FT4_STIC.csv", row.names = FALSE)

ggplot(FT4_STIC, aes(x=datetime , y= wetdry)) +
  geom_point(aes(color= factor(wetdry)))+
  scale_colour_brewer(palette = "Paired")+
  xlab(expression(paste("Date")))+
  ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")")))+
  theme_cust()
