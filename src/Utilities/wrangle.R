# ------------------------------------------------------------------------------
# File: mod_faq.R
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: Script for wrangling The Car Connection dataset.
# ------------------------------------------------------------------------------
library(dplyr)
library(forcats)
library(purrr)
library(readr)
library(stringr)

CAR_MANUFACTURERS <- c("Acura", "Alfa Romeo", "Aston Martin", "Audi", "BMW", 
					   "Bentley", "Buick", "Cadillac", "Chevrolet", "Chrysler", 
					   "Dodge", "Ferrari", "FIAT", "Ford", "Genesis", "GMC", 
					   "Honda", "Hyundai", "INFINITI", "Jaguar", "Jeep", "Kia", 
					   "Lamborghini", "Land Rover", "Lexus", "Lincoln", 
					   "Lotus", "Lucid", "Maserati", "Mazda", "McLaren", 
					   "Mercedes-Benz", "MINI", "Mitsubishi", "Nissan", 
					   "Polestar", "Porsche", "Ram", "Rivian", "Rolls-Royce", 
					   "Subaru", "smart", "Tesla", "Toyota", "Volkswagen", 
					   "Volvo")

tcc_raw <- readRDS(file = "data/tcc_raw.rds")
tcc_raw <- tcc_raw |>
	dplyr::select(where(~ !all(is.na(.))))

# FORMATTING, RENAMING AND TYPECASTING

colnames(tcc_raw) <- colnames(tcc_raw) |>
	stringr::str_to_lower() |>
	stringr::str_remove_all(pattern = "[\\(\\)]") |>
	stringr::str_remove_all(pattern = ",") |>
	stringr::str_remove_all(pattern = ":1") |> # All gear ratios are "X gear ratio (:1)"
	stringr::str_replace_all(pattern = "@", replacement = "at") |>
	stringr::str_replace_all(pattern = "ft³", replacement = "ft3") |>
	stringr::str_replace_all(pattern = "wt.", replacement = "weight") |>
	stringr::str_replace_all(pattern = "w/o", replacement = "without") |>
	stringr::str_replace_all(pattern = " {2,}", replacement = ' ') |>
	stringr::str_trim(side = c("right")) |>
	stringr::str_replace_all(pattern = ' ', replacement = '_')
colnames(tcc_raw)
	



# Year, Make and Model
extract_make_model <- function(car_name) {
	make <- sapply(CAR_MANUFACTURERS, function(car_make) ifelse(stringr::str_detect(car_name, car_make), car_make, NA))
	make[!is.na(make)][1]
}

tcc_raw <- tcc_raw |> 
	dplyr::rename(car_name = car) |>
	dplyr::mutate(year = stringr::str_sub(car_name, start = 1, end = 4), 
				  year = as.integer(readr::parse_number(year))) |>
	dplyr::mutate(car_name = stringr::str_sub(car_name, start = 6)) |>
	dplyr::mutate(car_specs = stringr::str_extract(car_name, "Specs:.*$"), 
				  car_specs = stringr::str_remove(car_specs, "^Specs:\\s*"), 
				  car_specs = stringr::str_trim(car_specs), 
				  car_name = stringr::str_remove(car_name, " Specs:.*$")) |>	
	dplyr::mutate(make = sapply(car_name, extract_make_model), 
				  make = unname(make),
				  make = forcats::as_factor(make),
				  model = stringr::str_remove(car_name, paste0("^", make, ' '))) |>
	dplyr::select(-c(car_name, car_specs))
tcc_clean <- dplyr::select(tcc_raw, c(year, make, model, style_name))
tcc_raw <- dplyr::select(tcc_raw, -c(year, make, model, style_name))

# MSRP
tcc_raw <- tcc_raw |>
	dplyr::rename(msrp_usd = msrp) |>
	dplyr::mutate(msrp_usd = readr::parse_number(msrp_usd, na = "N/A"))
tcc_clean <- tcc_clean |>
	dplyr::mutate(msrp_usd = tcc_raw$msrp_usd)
tcc_raw <- dplyr::select(tcc_raw, -c(msrp_usd))

# Gas Mileage
tcc_raw <- tcc_raw |>
	dplyr::mutate(
		city_mileage_mpg = stringr::str_extract(gas_mileage, "-?\\d+(\\.\\d+)? mpg City"),
		hwy_mileage_mpg = stringr::str_extract(gas_mileage, "-?\\d+(\\.\\d+)? mpg Hwy")
	) |>
	dplyr::mutate(
		city_mileage_mpg = readr::parse_number(city_mileage_mpg),
		hwy_mileage_mpg = readr::parse_number(hwy_mileage_mpg)
	) |>
	dplyr::select(-gas_mileage)
tcc_clean <- tcc_clean |>
	dplyr::mutate(city_mileage_mpg = tcc_raw$city_mileage_mpg, 
				  hwy_mileage_mpg = tcc_raw$hwy_mileage_mpg, 
				  battery_range_miles = tcc_raw$battery_range_mi)
tcc_raw <- dplyr::select(tcc_raw, -c(city_mileage_mpg, hwy_mileage_mpg, battery_range_mi))

# Engine
fuel_types <- c(
	"Gas/Plug-in Electric" = "Regular Unleaded",
	"Gas/Ethanol" = "Regular Unleaded",
	"Electric" = "Electricity",
	"FFV" = "Flexible",
	"Flexible" = "Flexible",
	"Natural Gas" = "Compressed Natural Gas",
	"Compressed Natural Gas" = "Compressed Natural Gas",
	"CNG" = "Compressed Natural Gas",
	"Regular Unleaded" = "Regular Unleaded",
	"\\bGAS\\b" = "Regular Unleaded",
	"\\bGas\\b" = "Regular Unleaded",
	"\\bgas\\b" = "Regular Unleaded",
	"Premium Unleaded" = "Premium Unleaded",
	"Diesel" = "Diesel"
)


fuel_pattern <- names(fuel_types) %>%
	paste0(collapse = "|") %>%
	regex(ignore_case = TRUE)



extract_cylinders <- function(x) {
	if (is.na(x)) return(NA_integer_)
	
	# e.g. "4-Cyl", "6 Cylinder"
	match <- str_extract(x, "(?i)(\\d+)[- ]?(Cyl|Cylinder|Cylinders)?")
	if (!is.na(match)) return(as.integer(str_extract(match, "\\d+")))
	
	# e.g. "V8", "I4", "W12", etc.
	match2 <- str_extract(x, "(?i)(V-?\\d+|I-?\\d+|i\\d+|H-?\\d+|W-?\\d+|L\\d+|l\\d+)")
	if (!is.na(match2)) return(as.integer(str_extract(match2, "\\d+")))
	
	if (str_to_lower(str_trim(x)) == "electric") return(0)
	
	return(NA_integer_)
}

tcc_raw <- tcc_raw %>%
	dplyr::select(-c(engine_type)) |> # engine_type is the same as engine
	# 1) Strip off the displacement at the end
	mutate(
		engine = engine %>%
			str_remove(",?\\s*\\d+(?:\\.\\d+)?\\s*[lL]?$") %>%
			str_remove(",\\s*$") %>%
			str_trim()
	) %>%
	# 2) Extract configuration & cylinder count
	mutate(
		engine_configuration = case_when(
			str_to_lower(str_trim(engine)) == "electric" ~ "Electric",
			str_detect(engine, regex("Flat[- ]?[46]|H-?[46]", ignore_case = TRUE)) ~ "Flat",
			str_detect(engine, regex("I-?\\d+|L4", ignore_case = TRUE))                ~ "Inline",
			str_detect(engine, regex("V-?\\d+", ignore_case = TRUE))                   ~ "V",
			str_detect(engine, regex("W-?\\d+", ignore_case = TRUE))                   ~ "W",
			str_detect(engine, regex("(?:\\d+[- ]?)?Cyl(?:inder)?s?", ignore_case = TRUE)) ~ NA_character_,
			TRUE ~ NA_character_
		),
		num_cylinders = sapply(engine, extract_cylinders)
	) %>%
	# 3) _Remove_ any leftover config/cyl tokens (using the NEW regex)
	mutate(
		engine = engine %>%
			str_remove_all(
				regex(
					paste0(
						"Flat[- ]?[46]",
						"|H-?[46]",
						"|I-?\\d+",
						"|L4",
						"|V-?\\d+",
						"|W-?\\d+",
						"|(?:\\d+[- ]?)?Cyl(?:inder)?s?"    # <— catches “-cyl”, “4 Cyl”, “Cylinders”, or just “Cyl”
					),
					ignore_case = TRUE
				)
			) %>%
			str_remove_all("[(),]+") %>%
			str_squish()
	) |>
	mutate(
		is_hybrid = str_detect(engine, regex("Gas/Electric|Electric/Gas|Gas/Plug-in Electric", ignore_case = TRUE))
	) |> 
	mutate(
		engine = case_when(
			is_hybrid ~ str_replace(engine, regex("Gas/Plug-in Electric|Electric/Gas|Gas/Electric", ignore_case = TRUE), "Gas"),
			TRUE ~ engine
		) %>%
			str_squish()
	) |> 
	mutate(
		fuel_type = case_when(
			is.na(engine) ~ NA_character_,
			TRUE ~ map_chr(engine, function(x) {
				if (is.na(x)) return(NA_character_)
				matched <- names(fuel_types)[str_detect(x, regex(names(fuel_types), ignore_case = TRUE))]
				if (length(matched) == 0) return(NA_character_)
				fuel_types[matched[1]]  # take the first match
			})
		),
		engine = if_else(
			is.na(engine),
			engine,
			str_remove(engine, fuel_pattern) %>%
				str_remove("/Ethanol") %>%  # just in case
				str_squish()
		)
	) |>
	mutate(
		is_turbocharged = if_else(
			is.na(engine), FALSE,
			str_detect(engine, regex("turbo|bi-turbo|twin-turbo|turbocharged", ignore_case = TRUE))
		),
		is_supercharged = if_else(
			is.na(engine), FALSE,
			str_detect(engine, regex("supercharged|supercharger", ignore_case = TRUE))
		)
	) |>
	mutate(
		engine_configuration = forcats::as_factor(engine_configuration), 
		num_cylinders = as.integer(unname(num_cylinders)),
		fuel_type = forcats::as_factor(fuel_type)
	) |>
	dplyr::select(-c(engine))
tcc_clean <- tcc_clean |>
	dplyr::mutate(engine_configuration = tcc_raw$engine_configuration, 
				  num_cylinders = tcc_raw$num_cylinders, 
				  is_hybrid = tcc_raw$is_hybrid, 
				  fuel_type = tcc_raw$fuel_type, 
				  is_turbocharged = tcc_raw$is_turbocharged, 
				  is_supercharged = tcc_raw$is_supercharged)
tcc_raw <- dplyr::select(tcc_raw, -c(engine_configuration, 
									 num_cylinders, 
									 is_hybrid, 
									 fuel_type, 
									 is_turbocharged, 
									 is_supercharged))

# Drivetrain
drivetrain_map <- c(
	"^2"     = "2WD",
	"^4"     = "4WD",
	"FOUR" = "4WD",
	"ALL"    = "AWD",
	"FRONT"  = "FWD",
	"REAR"   = "RWD",
	"AWD"    = "AWD",
	"RWD"    = "RWD"
)
tcc_raw <- tcc_raw |>
	dplyr::mutate(
		drivetrain = stringr::str_replace_all(drivetrain, '-', ' '), 
		drivetrain = stringr::str_to_upper(drivetrain)
	) |>
	mutate(
		drivetrain = map_chr(drivetrain, function(x) {
			if (is.na(x)) return(NA_character_)
			match <- names(drivetrain_map)[str_detect(x, names(drivetrain_map))]
			if (length(match) == 0) return(x)  # fallback to original if no match
			drivetrain_map[match[1]]
		}),
		drivetrain = forcats::as_factor(drivetrain)
	)
tcc_clean <- tcc_clean |>
	dplyr::mutate(
		drivetrain = tcc_raw$drivetrain
	)
tcc_raw <- dplyr::select(tcc_raw, -c(drivetrain))

# Passenger doors and passenger capacity
tcc_raw <- tcc_raw |> 
	dplyr::rename(num_passenger_doors = passenger_doors) |>
	dplyr::mutate(
		num_passenger_doors = if_else(!is.na(number_of_passenger_doors) & number_of_passenger_doors != num_passenger_doors, NA_integer_, num_passenger_doors),
		num_passenger_doors = replace(num_passenger_doors, num_passenger_doors == 0, NA),
		passenger_capacity = replace(passenger_capacity, passenger_capacity == 0 | 
									 	passenger_capacity < num_passenger_doors, NA)		
	) |> 
	dplyr::select(-c(number_of_passenger_doors))

tcc_clean <- tcc_clean |>
	dplyr::mutate(
		passenger_capacity = as.integer(tcc_raw$passenger_capacity),
		num_passenger_doors = as.integer(tcc_raw$num_passenger_doors)
	)

tcc_raw <- dplyr::select(tcc_raw, -c(passenger_capacity, num_passenger_doors))

# Body Style
body_styles <- c(
	"2dr Car" = "2-Door Car",
	"3dr Car" = "3-Door Car",
	"4dr Car" = "4-Door Car",
	"Mini-van, Cargo" = "Cargo Mini-van",
	"Mini-van, Passenger" = "Passenger Mini-van",
	"Crew Cab Pickup" = NA_character_,
	"Extended Cab Pickup" = NA_character_,
	"Regular Cab Chassis-Cab" = NA_character_
)

tcc_raw <- tcc_raw %>%
	mutate(
		body_style = recode(body_style, !!!body_styles),
		body_style = as_factor(body_style)
	)

tcc_clean <- tcc_clean %>%
	mutate(body_style = tcc_raw$body_style)

tcc_raw <- select(tcc_raw, -body_style)

# EPA Classification
# epa_classes <- c(
# 	"2WD Special Purpose|2WD Special Purpose Vehicle|2WD Special Purpose Vehicles|4WD Special Purpose|4WD Special Purpose Vehicle|4WD Special Purpose Vehicles" = "Special Purpose Vehicle",
# 	"2WD Sport Utililty|2WD Sport Utility|2WD sport Utility Vehicle|2WD Sport Utility Vehicle|2WD Sport Utility Vehicles|4WD Sport Utility|4WD sport Utility Vehicle|4WD Sport Utility Vehicle|4WD Sport Utility Vehicles|AWD Sport Utility|AWD Sport Utility Vehicle|AWD Sport Utility Vehicles|FWD Sport Utility|FWD Sport Utility Vehicle" = "Sport Utility Vehicle",
# 	"2WD Van" = "Van",
# 	"4WD Minivan|4WD Minivans|Minivan - SWD" = "Minivan",
# 	"4WD Pickup Trucks|4WD Standard Pickup Truck|Light-Duty Truck|Pickup Trucks" = "Truck",
# 	"Compact Sedan|Mid-Size Sedan|Midsize sedan" = "Sedan",
# 	"Compact Car|Compact Cars" = "Compact",
# 	"Mini-compact|Mini-Compact|Mini-Compact Car|Minicompact|Minicompact Car|Minicompact Cars" = "Mini-Compact",
# 	"large|Large car|Large Car|Large Cars" = "Large",
# 	"Mid-size|Mid-Size|Mid-Size Cars|Midsize|Midsize Car|Midsize cars|Midsize Cars|MidSize Cars" = "Mid Size",
# 	"Mid-Size Station Wagon|Mid-Size Wagon|Midsize S/W|Midsize Station Wagon|Midsize Station Wagons" = "Station Wagon",
# 	"No Data" = NA,
# 	
# )
tcc_raw <- tcc_raw |>
	dplyr::select(-c(epa_class)) # epa_class is the same as epa_classification

# Maximum width w/o mirrors
tcc_raw <- tcc_raw |>
	dplyr::rename(
		max_width_without_mirrors_in = width_max_without_mirrors_in
	)
tcc_clean <- tcc_clean |>
	dplyr::mutate(
		max_width_without_mirrors_in = tcc_raw$max_width_without_mirrors_in
	)
tcc_raw <- dplyr::select(tcc_raw, -c(max_width_without_mirrors_in))

# Approximate fuel tank capacity
tcc_raw <- tcc_raw |>
	dplyr::rename(
		approx_fuel_tank_capacity_gal = fuel_tank_capacity_approx_gal
	)
tcc_clean <- tcc_clean |>
	dplyr::mutate(
		approx_fuel_tank_capacity_gal = tcc_raw$approx_fuel_tank_capacity_gal
	)
tcc_raw <- dplyr::select(tcc_raw, -c(approx_fuel_tank_capacity_gal, aux_fuel_tank_location))

# Displacement
tcc_raw <- tcc_raw |>
	dplyr::rename(displacement_L = displacement) |>
	dplyr::mutate(
		displacement_L = stringr::str_extract(displacement_L, "\\d+(\\.\\d+)?\\s*[lL]"),
		displacement_L = readr::parse_number(displacement_L)
	)

tcc_clean <- tcc_clean |>
	dplyr::mutate(
		displacement_L = tcc_raw$displacement_L
	)

tcc_raw <- dplyr::select(tcc_raw, -c(displacement_L))

# Transmission Type (rename to speeds)
tcc_raw <- tcc_raw |>
	dplyr::rename(transmission_num_speeds = trans_type)

# Air Bags
tcc_raw <- tcc_raw |>
	dplyr::rename(
		has_frontal_driver_air_bag = `air_bag-frontal-driver`,
		has_frontal_passenger_air_bag = `air_bag-frontal-passenger`,
		has_frontal_passenger_air_bag_switch = `air_bag-passenger_switch_on/off`,
		has_side_body_front_air_bag = `air_bag-side_body-front`,
		has_side_body_rear_air_bag = `air_bag-side_body-rear`,
		has_side_head_front_air_bag = `air_bag-side_head-front`, 
		has_side_head_rear_air_bag = `air_bag-side_head-rear`
	) |>
	mutate(across(
		c(
			has_frontal_driver_air_bag,
			has_frontal_passenger_air_bag,
			has_frontal_passenger_air_bag_switch,
			has_side_body_front_air_bag,
			has_side_body_rear_air_bag,
			has_side_head_front_air_bag,
			has_side_head_rear_air_bag
		),
		~ .x == "Yes"
	))

tcc_clean <- tcc_clean |>
	dplyr::mutate(
		has_frontal_driver_air_bag = tcc_raw$has_frontal_driver_air_bag,
		has_frontal_passenger_air_bag = tcc_raw$has_frontal_passenger_air_bag,
		has_frontal_passenger_air_bag_switch = tcc_raw$has_frontal_passenger_air_bag_switch,
		has_side_body_front_air_bag = tcc_raw$has_side_body_front_air_bag,
		has_side_body_rear_air_bag = tcc_raw$has_side_body_rear_air_bag,
		has_side_head_front_air_bag = tcc_raw$has_side_head_front_air_bag,
		has_side_head_rear_air_bag = tcc_raw$has_side_head_rear_air_bag
	)
tcc_raw <- dplyr::select(tcc_raw, -c(
	has_frontal_driver_air_bag,
	has_frontal_passenger_air_bag,
	has_frontal_passenger_air_bag_switch,
	has_side_body_front_air_bag,
	has_side_body_rear_air_bag,
	has_side_head_front_air_bag,
	has_side_head_rear_air_bag
))

# Brakes
tcc_raw <- tcc_raw |>
	dplyr::rename(has_abs_brakes = `brakes-abs`) |>
	dplyr::mutate(has_abs_brakes = (has_abs_brakes == "Yes"))
tcc_clean <- tcc_clean |>
	dplyr::mutate(has_abs_brakes = tcc_raw$has_abs_brakes)
tcc_raw <- dplyr::select(tcc_raw, -c(has_abs_brakes, brake_abs_system_second_line))

# Safety Features
tcc_raw <- tcc_raw |>
	dplyr::rename(
		has_child_safety_rear_door_locks = child_safety_rear_door_locks,
		has_daytime_running_lights = daytime_running_lights,
		has_traction_control = traction_control,
		has_night_vision = night_vision,
		has_rollover_protection_bars = rollover_protection_bars,
		has_fog_lamps = fog_lamps,
		has_parking_aid = parking_aid,
		has_tire_pressure_monitor = tire_pressure_monitor,
		has_backup_camera = `back-up_camera`,
		has_stability_control = stability_control
	) |>
	dplyr::mutate(across(c(
		has_child_safety_rear_door_locks,
		has_daytime_running_lights,
		has_traction_control,
		has_night_vision,
		has_rollover_protection_bars,
		has_fog_lamps,
		has_parking_aid,
		has_tire_pressure_monitor,
		has_backup_camera,
		has_stability_control
	), ~ .x == "Yes"))
tcc_clean <- tcc_clean |>
	dplyr::mutate(
		has_child_safety_rear_door_locks = tcc_raw$has_child_safety_rear_door_locks,
		has_daytime_running_lights = tcc_raw$has_daytime_running_lights,
		has_traction_control = tcc_raw$has_traction_control,
		has_night_vision = tcc_raw$has_night_vision,
		has_rollover_protection_bars = tcc_raw$has_rollover_protection_bars,
		has_fog_lamps = tcc_raw$has_fog_lamps,
		has_parking_aid = tcc_raw$has_parking_aid,
		has_tire_pressure_monitor = tcc_raw$has_tire_pressure_monitor,
		has_backup_camera = tcc_raw$has_backup_camera,
		has_stability_control = tcc_raw$has_stability_control
	)
tcc_raw <- dplyr::select(tcc_raw, -c(
	has_child_safety_rear_door_locks,
	has_daytime_running_lights,
	has_traction_control,
	has_night_vision,
	has_rollover_protection_bars,
	has_fog_lamps,
	has_parking_aid,
	has_tire_pressure_monitor,
	has_backup_camera,
	has_stability_control
))

# Warranty
tcc_raw <- tcc_raw |>
	dplyr::rename(
		basic_warranty_num_years = basic_years,
		basic_warranty_num_miles = `basic_miles/km`,
		corrosion_warranty_num_years = corrosion_years,
		corrosion_warranty_num_miles = `corrosion_miles/km`,
		drivetrain_warranty_num_years = drivetrain_years,
		drivetrain_warranty_num_miles = `drivetrain_miles/km`,
		emissions_warranty_num_years = emissions_years,
		emissions_warranty_num_miles = `emissions_miles/km`,
		roadside_assistance_num_years = roadside_assistance_years,
		roadside_assistance_num_miles = `roadside_assistance_miles/km`,
		maintenance_warranty_num_years = maintenance_years,
		maintenance_warranty_num_miles = `maintenance_miles/km`
	) |>
	dplyr::mutate(
		# Create is_unlimited flags
		is_basic_warranty_miles_unlimited = basic_warranty_num_miles == "Unlimited",
		is_corrosion_warranty_miles_unlimited = corrosion_warranty_num_miles == "Unlimited",
		is_drivetrain_warranty_miles_unlimited = drivetrain_warranty_num_miles == "Unlimited",
		is_emissions_warranty_miles_unlimited = emissions_warranty_num_miles == "Unlimited",
		is_roadside_assistance_miles_unlimited = roadside_assistance_num_miles == "Unlimited",
		is_maintenance_warranty_miles_unlimited = maintenance_warranty_num_miles == "Unlimited",
		
		# Replace "Unlimited" with NA, remove commas, convert to numeric
		basic_warranty_num_miles = as.numeric(gsub(",", "", ifelse(basic_warranty_num_miles == "Unlimited", NA, basic_warranty_num_miles))),
		corrosion_warranty_num_miles = as.numeric(gsub(",", "", ifelse(corrosion_warranty_num_miles == "Unlimited", NA, corrosion_warranty_num_miles))),
		drivetrain_warranty_num_miles = as.numeric(gsub(",", "", ifelse(drivetrain_warranty_num_miles == "Unlimited", NA, drivetrain_warranty_num_miles))),
		emissions_warranty_num_miles = as.numeric(gsub(",", "", ifelse(emissions_warranty_num_miles == "Unlimited", NA, emissions_warranty_num_miles))),
		roadside_assistance_num_miles = as.numeric(gsub(",", "", ifelse(roadside_assistance_num_miles == "Unlimited", NA, roadside_assistance_num_miles))),
		maintenance_warranty_num_miles = as.numeric(gsub(",", "", ifelse(maintenance_warranty_num_miles == "Unlimited", NA, maintenance_warranty_num_miles))),
		
		# Convert years to integer
		basic_warranty_num_years = as.integer(basic_warranty_num_years),
		corrosion_warranty_num_years = as.integer(corrosion_warranty_num_years),
		drivetrain_warranty_num_years = as.integer(drivetrain_warranty_num_years),
		emissions_warranty_num_years = as.integer(emissions_warranty_num_years),
		roadside_assistance_num_years = as.integer(roadside_assistance_num_years),
		maintenance_warranty_num_years = as.integer(maintenance_warranty_num_years)
	)
tcc_clean <- tcc_clean |>
	dplyr::mutate(
		# Basic warranty
		basic_warranty_num_miles = tcc_raw$basic_warranty_num_miles,
		basic_warranty_num_years = tcc_raw$basic_warranty_num_years,
		is_basic_warranty_miles_unlimited = tcc_raw$is_basic_warranty_miles_unlimited,
		
		# Corrosion warranty
		corrosion_warranty_num_miles = tcc_raw$corrosion_warranty_num_miles,
		corrosion_warranty_num_years = tcc_raw$corrosion_warranty_num_years,
		is_corrosion_warranty_miles_unlimited = tcc_raw$is_corrosion_warranty_miles_unlimited,
		
		# Drivetrain warranty
		drivetrain_warranty_num_miles = tcc_raw$drivetrain_warranty_num_miles,
		drivetrain_warranty_num_years = tcc_raw$drivetrain_warranty_num_years,
		is_drivetrain_warranty_miles_unlimited = tcc_raw$is_drivetrain_warranty_miles_unlimited,
		
		# Emissions warranty
		emissions_warranty_num_miles = tcc_raw$emissions_warranty_num_miles,
		emissions_warranty_num_years = tcc_raw$emissions_warranty_num_years,
		is_emissions_warranty_miles_unlimited = tcc_raw$is_emissions_warranty_miles_unlimited,
		
		# Roadside assistance
		roadside_assistance_num_miles = tcc_raw$roadside_assistance_num_miles,
		roadside_assistance_num_years = tcc_raw$roadside_assistance_num_years,
		is_roadside_assistance_miles_unlimited = tcc_raw$is_roadside_assistance_miles_unlimited,
		
		# Maintenance warranty
		maintenance_warranty_num_miles = tcc_raw$maintenance_warranty_num_miles,
		maintenance_warranty_num_years = tcc_raw$maintenance_warranty_num_years,
		is_maintenance_warranty_miles_unlimited = tcc_raw$is_maintenance_warranty_miles_unlimited
	)
tcc_raw <- dplyr::select(tcc_raw, -c(
	# Basic warranty
	basic_warranty_num_miles,
	basic_warranty_num_years,
	is_basic_warranty_miles_unlimited,
	basic_note,
	
	# Corrosion warranty
	corrosion_warranty_num_miles,
	corrosion_warranty_num_years,
	is_corrosion_warranty_miles_unlimited,
	corrosion_note,
	
	# Drivetrain warranty
	drivetrain_warranty_num_miles,
	drivetrain_warranty_num_years,
	is_drivetrain_warranty_miles_unlimited,
	drivetrain_note,
	
	# Emissions warranty
	emissions_warranty_num_miles,
	emissions_warranty_num_years,
	is_emissions_warranty_miles_unlimited,
	emissions_note,
	
	# Roadside assistance
	roadside_assistance_num_miles,
	roadside_assistance_num_years,
	is_roadside_assistance_miles_unlimited,
	roadside_assistance_note,
	
	# Maintenance warranty
	maintenance_warranty_num_miles,
	maintenance_warranty_num_years,
	is_maintenance_warranty_miles_unlimited,
	maintenance_note, 
	
	warranty_note, 
	`hybrid/electric_components_note`, 
))
	
# hybrid/electric_components_miles/km
# hybrid/electric_components_years

# Cargo volume
tcc_clean <- tcc_clean |>
	dplyr::mutate(
		cargo_volume_with_rear_seat_up_ft3 = tcc_raw$cargo_volume_with_rear_seat_up_ft3,
		cargo_box_length_at_floor_in = tcc_raw$cargo_box_length_at_floor_in
	)
tcc_raw <- dplyr::select(tcc_raw, -c(cargo_volume_with_rear_seat_up_ft3, cargo_box_length_at_floor_in))

# Dimensions
tcc_raw <- tcc_raw |>
	dplyr::rename(
		overall_length_with_rear_bumper_in = `length_overall_w/rear_bumper_in`
	)
tcc_clean <- tcc_clean |>
	dplyr::mutate(
		overall_length_with_rear_bumper_in = tcc_raw$overall_length_with_rear_bumper_in 
	)
tcc_raw <- dplyr::select(tcc_raw, -c(overall_length_with_rear_bumper_in))



tcc <- tcc_clean
rm(tcc_clean)

