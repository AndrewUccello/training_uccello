# <function_name> <- function(arg1, arg2, ...) {
#   < code for the funciton to run>
#   return (<something>)
# }

interior_f <- c(186, 175, 172)
interior_c <- (interior_f -32) * 5 / 9

exterior_f <- c(75,78,80)
exterior_c <- (interior_f -32) * 5 / 9

surface_f <- c(103,109,98)
surface_c <- (surface_f - 32) * 5 / 9

convert_f_to_c <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  return(celsius)
}

convert_c_to_f <- function(celsius) {
  fahr <- celsius * 9/5 + 32
  return(fahr)
}

surface_f_v2 <- convert_c_to_f(surface_c)
identical(surface_f, surface_f_v2)

convert_temps <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  kelvin <- celsius + 273.15
  return(list(fahr = fahr, celsius = celsius, kelvin = kelvin))
}

surface_temps_df <- data.frame(convert_temps(fahr = surface_f))

convert_temps <- function(temp, unit = "F") {
  
  ### Error checking:
  unit <- toupper(unit)  ## try to anticipate common mistakes! toupper tranlsates lowercase to uppercase
  if (!unit %in% c("F", "C")) {
    stop("The units must be either F or C")
  }
  
  if (unit == "F") {
    fahr <- temp
    celsius <- (fahr - 32) * 5/9
  } else {
    celsius <- temp
    fahr <- celsius * 9 / 5 + 32
  }
  kelvin <- celsius + 273.15
  
  out_df <- data.frame(fahr, celsius, kelvin)
  return(out_df)
}
 library(dplyr)

data.frame(f = surface_f) %>%
  mutate(c = convert_f_to_c(f))

add_hot_or_cold <- function(df, thresh = 70) {
  if (!"fahr" %in% names(df)) {
    stop("data frame must have fahr column!")
  }
  out_df <- df %>%
    mutate(hotcold = ifelse(fahr>thresh, "hot","cold"))
  return(out_df)
}

data.frame(fahr = surface_f) %>%
  mutate(c = convert_f_to_c(fahr)) %>%
  add_hot_or_cold(thresh=105)


