WHO_PM25_GUIDELINE_VALUE = 5 # ug/m3

SIG_LEVEL = 0.05

BOXPLOT_FOLDER = "boxplot"

MEAN_PM_SD = "Mean $\\pm$ SD"
MEDIAN_Q1_Q3 = "Median [Q1, Q3]"

# ----- SENSOR ENVIRONMENTAL METRICS ------
SENSOR_METRICS <- c("pm1", "pm25", "pm10", "co2", "temperature", "humidity")

# ----- APPLE WATCH HEALTH METRICS ------
HR = "heart-rate"
RHR = "resting-heart-rate"
HRV = "hrv"
RR = "respiratory-rate"
SPO2 = "oxygen-saturation"
SLEEP = "sleep"

HEALTH_METRICS <- c(HR, RHR, HRV, RR, SPO2)
HEALTH_METRIC_NAMES <- setNames(
  c("HR", "RHR", "HRV", "Sleep RR", "SpO2"),
  c(HR, RHR, HRV, RR, SPO2)
)

# ----- APPLE WATCH SLEEP STAGES ------
IN_BED = "in_bed" # non-sleep time flagged by the iPhone, not by the Apple Watch
ASLEEP_UNSPECIFIED = "alseep_unspecified"
AWAKE = "awake" # non-sleep time flagged by Apple Watch
ASLEEP_CORE = "asleep_core"
ASLEEP_DEEP = "asleep_deep"
ASLEEP_REM = "asleep_rem"
ASLEEP_ALL = "alseep_all" # not returned by Apple Watch, our custom flag to calculate the total sleep time (IN_BED and AWKAKE doesn't count towards total sleep time)

ALL_SLEEP_STAGES <- c(IN_BED, ASLEEP_UNSPECIFIED, AWAKE, ASLEEP_CORE, ASLEEP_DEEP, ASLEEP_REM, ASLEEP_ALL) # array of sleep values with index value correspond to the value of the category, https://learn.microsoft.com/en-us/dotnet/api/healthkit.hkcategoryvaluesleepanalysis?view=net-ios-26.2-10.0

SLEEP_STAGES_FOR_ANALYSIS <- c(AWAKE, ASLEEP_CORE, ASLEEP_DEEP, ASLEEP_REM, ASLEEP_ALL)

SLEEP_STAGE_NAMES <- setNames(
  c("Total", "Unspecified", "Awake", "Core", "Deep",  "REM"),
  c(ASLEEP_ALL, ASLEEP_UNSPECIFIED, AWAKE, ASLEEP_CORE, ASLEEP_DEEP, ASLEEP_REM)
)

# ----- SURVEY -----
SURVEY = "survey"

# ----- SAMPLES AND LOCATIONS ------
START_DATE = "start_date"
END_DATE = "end_date"

# ----- PERIODS AND TREATMENTS ------
TREATMENT_1_NAME = "treatment1"
TREATMENT_2_NAME = "treatment2"

PERIOD_INDICES <- c(1, 2, 3)

# ----- LABELS FOR LATEX CODES -----
METRIC_LABELS_LATEX <- c(
  pm1 = "PM1 ($\\mu$g/m$^3$)",
  pm25 = "PM2.5 ($\\mu$g/m$^3$)",
  pm10 = "PM10 ($\\mu$g/m$^3$)",
  co2 = "CO$_2$ (ppm)",
  temperature = "T ($^\\circ$C)",
  humidity = "RH (\\%)"
)

METRIC_TYPE_LABELS_LATEX <- c(raw = "Raw", corrected = "Corrected")

LOCATION_TYPE_LABELS_LATEX <- c(home = "Home", indoors = "Indoors elsewhere", outdoors = "Outdoors", all = "All")

HEALTH_METRIC_LABELS_LATEX <- setNames(
  c("HR (beats/min)", "RHR (beats/min)", "HRV (ms)", "Sleep RR (breaths/min)", "SpO$_2$ (\\%)"),
  c(HR, RHR, HRV, RR, SPO2)
)
SLEEP_STAGE_LABELS_LATEX <- setNames(
  c("Awake (h)", "Core (h)", "Deep (h)",  "REM (h)", "Unspecified (h)", "Total (h)"),
  c(AWAKE, ASLEEP_CORE, ASLEEP_DEEP, ASLEEP_REM, ASLEEP_UNSPECIFIED, ASLEEP_ALL)
)

TREATMENT_LABELS <- setNames(
                c("Sham Filter", "Real Filter", "Outdoors", "Washout"),
                c(TREATMENT_1_NAME, TREATMENT_2_NAME, "outdoors", "washout")
)
TREATMENT_LABELS_2 <- setNames(
                c("Sham Filter (Home)", "Real Filter (Home)", "Outdoors"),
                c(TREATMENT_1_NAME, TREATMENT_2_NAME, "outdoors")
)
TREATMENT_LABELS_3 <- setNames(
                c("Sham Filter (Fan On)", "Real Filter (Fan On)", "Washout (Fan Off)"),
                c(TREATMENT_1_NAME, TREATMENT_2_NAME, "washout")
)

# ----- COLORS ------
TREATMENT_FILL_VALUES <- setNames(c("orange", "lightblue", "gray", "gray"), c(TREATMENT_1_NAME, TREATMENT_2_NAME, "outdoors", "washout"))