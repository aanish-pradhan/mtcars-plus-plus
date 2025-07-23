#' Script for cleaning The Car Connection dataset.
#' 
#' @author Aanish Pradhan
library(dplyr)
library(forcats)
library(readr)
library(stringr)

CAR_MANUFACTURERS <- c("Acura, Alfa Romeo, Audi, BMW, Buick, Cadillac, 
	Chevrolet, Chrysler, Dodge, FIAT, Ford, Genesis, GMC, Honda, Hyundai, 
	INFINITI, Jaguar, Jeep, Kia, Land Rover, Lexus, Lincoln, Lucid, Mazda, 
	Mercedes-Benz, MINI, Mitsubishi, Nissan, Polestar, Porsche, Ram, Rivian, 
	Subaru, Tesla, Toyota, Volkswagen, Volvo")

data <- readr::read_csv(file = "fullspecs_transpose.csv")


data <- data |> 
	dplyr::rename(car_name = Car) |>
	dplyr::mutate(year = stringr::str_sub(car_name, start = 1, end = 4)) |>
	dplyr::mutate(car_name = stringr::str_sub(car_name, start = 6))
	
	
data <- data |> 
	dplyr::rename(msrp_usd = MSRP) |> 
	dplyr::mutate(msrp_usd = readr::parse_number(msrp_usd, na = "N/A"))

data <- data |>
	dplyr::mutate(
		city_mileage_mpg = stringr::str_extract(`Gas Mileage`, "-?\\d+(\\.\\d+)? mpg City"),
		hwy_mileage_mpg = stringr::str_extract(`Gas Mileage`, "-?\\d+(\\.\\d+)? mpg Hwy")
	) |>
	dplyr::mutate(
		city_mileage_mpg = readr::parse_number(city_mileage_mpg),
		hwy_mileage_mpg = readr::parse_number(hwy_mileage_mpg)
	) |>
	dplyr::select(-`Gas Mileage`)

data <- data |>
	dplyr::rename(
		passenger_capacity = `Passenger Capacity`, 
		num_passenger_doors = `Passenger Doors`, 
		base_curb_wt_lbs = `Base Curb Weight (lbs)`,
		front_hip_room_in = `Front Hip Room (in)`,
		front_leg_room_in = `Front Leg Room (in)`,
		second_shoulder_room_in = `Second Shoulder Room (in)`,
		
	)



