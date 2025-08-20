tbd_variables <- rlang::quos(
	# EPA Classification / Volume
	epa_classification,
	passenger_volume_ft3,
	
	# Dimensions / Weight
	base_curb_weight_lbs,
	wheelbase_in,
	length_overall_in,
	height_overall_in,
	liftover_height_in,
	min_ground_clearance_in,
	front_track_width_in,
	track_width_rear_in,
	
	# Front Seat
	front_head_room_in,
	front_leg_room_in,
	front_hip_room_in,
	front_shoulder_room_in,
	
	# Second Row
	second_head_room_in,
	second_leg_room_in,
	second_hip_room_in,
	second_shoulder_room_in,
	
	# Third Row
	third_shoulder_room_in,
	third_head_room_in,
	third_hip_room_in,
	third_leg_room_in,
	
	# Fourth row
	fourth_head_room_in,
	fourth_shoulder_room_in,
	fourth_leg_room_in,
	fourth_hip_room_in,
	
	
	# Cargo Area
	cargo_volume_to_seat_1_ft3,
	cargo_volume_to_seat_2_ft3,
	cargo_volume_to_seat_3_ft3,
	`cargo_volume_to_seat_4_ft3`,
	cargo_area_length_at_floor_to_seat_1_in,
	cargo_area_length_at_floor_to_seat_2_in,
	cargo_area_length_at_floor_to_seat_3_in,
	`cargo_area_length_at_floor_to_seat_4_in`,
	`cargo_area_length_at_floor_to_console_in`,
	cargo_area_width_at_beltline_in,
	cargo_box_width_at_wheelhousings_in,
	cargo_box_area_height_in,
	rear_door_opening_height_in,
	rear_door_opening_width_in,
	`cargo_volume_with_rear_seat_down_ft3`,
	trunk_volume_ft3,
	
	# Fuel Economy / Emissions
	`epa_fuel_economy_est_-_city_mpg`,
	`epa_fuel_economy_est_-_hwy_mpg`,
	`fuel_economy_est-combined_mpg`,
	`epa_mpg_equivalent_-_combined`,
	`epa_mpg_equivalent_-_city`,
	`epa_mpg_equivalent_-_hwy`,
	epa_air_pollution_score,
	
	# Engine & Electrical
	fuel_system,
	sae_net_horsepower_at_rpm,
	sae_net_torque_at_rpm,
	engine_order_code,
	engine_oil_cooler,
	maximum_alternator_capacity_amps,
	`cold_cranking_amps_at_0°_f_primary`,
	`cold_cranking_amps_at_0°_f_2nd`,
	
	# Transmission / Gear Ratios
	first_gear_ratio,
	second_gear_ratio,
	third_gear_ratio,
	fourth_gear_ratio,
	fifth_gear_ratio,
	sixth_gear_ratio,
	seventh_gear_ratio,
	eighth_gear_ratio,
	ninth_gear_ratio,
	tenth_gear_ratio,
	final_drive_axle_ratio,
	reverse_ratio,
	trans_order_code,
	clutch_size_in,
	trans_pto_access,
	`trans_power_take_off`,
	
	# Transfer Case / Axles
	transfer_case_model,
	`transfer_case_power_take_off`,
	transfer_case_gear_ratio_high,
	transfer_case_gear_ratio_low,
	`axle_type_-_rear`,
	`axle_type_-_front`,
	`axle_ratio_-_rear`,
	`axle_ratio_-_front`,
	
	# Towing / Trailering
	`weightdistributing_hitch_-_max_tongue_weight_lbs`,
	`weightdistributing_hitch_-_max_trailer_weight_lbs`,
	maximum_trailering_capacity_lbs,
	`dead_weight_hitch_-_max_trailer_weight_lbs`,
	`dead_weight_hitch_-_max_tongue_weight_lbs`,
	
	# Braking
	brake_type,
	front_brake_rotor_diam_x_thickness_in,
	rear_brake_rotor_diam_x_thickness_in,
	rear_drum_diam_x_width_in,
	`disc_-_rear_yes_or`,
	`drum_-_rear_yes_or`,
	brake_abs_system,
	
	# Suspension
	`suspension_type_-_front`,
	`suspension_type_-_front_cont.`,
	`suspension_type_-_rear`,
	`suspension_type_-_rear_cont.`,
	`shock_absorber_diameter_-_front_mm`,
	`shock_absorber_diameter_-_rear_mm`,
	`stabilizer_bar_diameter_-_front_in`,
	`stabilizer_bar_diameter_-_rear_in`,
	total_cooling_system_capacity_qts,
	
	# Steering
	steering_type,
	steering_ratio_overall,
	lock_to_lock_turns_steering,
	`turning_diameter_-_curb_to_curb_ft`,
	`turning_diameter_-_wall_to_wall_ft`,
	
	# Tires
	front_tire_order_code,
	front_tire_size,
	rear_tire_order_code,
	rear_tire_size,
	spare_tire_order_code,
	spare_tire_size,
	
	# Wheels
	front_wheel_size_in,
	front_wheel_material,
	rear_wheel_size_in,
	rear_wheel_material,
	spare_wheel_size_in,
	spare_wheel_material,
	
	# Doors & Openings
	rear_door_type,
	side_door_opening_height_in,
	side_door_opening_width_in,
	`step_up_height_-_front_in`,
	`step_up_height_-_side_in`,
	`ground_to_top_of_load_floor_in`,
	
	# Overhangs & Clearances
	overhang_front_in,
	`overhang_rear_w/bumper_in`,
	ground_clearance_rear_in,
	ground_clearance_front_in,
	
	# Weights
	gross_vehicle_weight_rating_cap_lbs,
	`gross_axle_weightrating_-_front_lbs`,
	`gross_axle_weightrating_-_rear_lbs`,
	gross_combined_weightrating_lbs,
	`curb_weight_-_rear_lbs`,
	`curb_weight_-_front_lbs`,
	
	# Fuel System
	aux_fuel_tank_capacity_approx_gal,
	fuel_tank_location,
	
	tailgate_width_in,
	`ext'd_cab_cargo_volume_ft3`,
	steering_ratio_on_center, 
	steering_ratio_at_lock,
	spare_tire_capacity_lbs,
	front_tire_capacity_lbs, 
	rear_tire_capacity_lbs,
	`revolutions/mile_at_45_mph_-_front`,
	`revolutions/mile_at_45_mph_-_rear`,
	`revolutions/mile_at_45_mph_-_spare`,
	`axle_capacity_-_rear_lbs`,
	`axle_capacity_-_front_lbs`,
	`spring_capacity_-_front_lbs`,
	`spring_capacity_-_rear_lbs`,
	maximum_alternator_watts,
	`fifth_wheel_hitch_-_max_trailer_weight_lbs`, 
	`fifth_wheel_hitch_-_max_tongue_weight_lbs`, 
	length_overall_without_rear_bumper_in,
	front_bumper_to_back_of_cab_in,
	frame_width_rear_in,
	cab_to_axle_in,
	overhang_rear_without_bumper_in,
	ground_to_top_of_frame_in,
	cab_to_end_of_frame_in,
	cargo_box_width_at_top_rear_in,
	cargo_volume_ft3,
	cargo_box_width_at_floor_in
)

tbd_colnames <- purrr::map_chr(tbd_variables, rlang::as_name)

# 3. Select those columns into tcc_tbd_raw
tcc_tbd_raw <- dplyr::select(tcc_raw, all_of(tbd_colnames))

# 4. Drop those columns from tcc_raw
tcc_raw <- dplyr::select(tcc_raw, -all_of(tbd_colnames))
