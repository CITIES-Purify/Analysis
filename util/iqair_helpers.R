standardize_iqair_colnames <- function(df) {
  return(
    df <- df |>
        rename(
        pm1 = any_of("PM1 (ug/m3)"),
        pm25 = any_of("PM2.5 (ug/m3)"),
        pm10 = any_of("PM10 (ug/m3)"),
        temperature = any_of("Temperature (Celsius)"),
        humidity = any_of("Humidity (%)"),
        co2 = any_of("CO2 (ppm)"),
        fan_speed_level = any_of("Fan Speed Level")
        )
    )
}
