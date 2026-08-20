library(data.table)

file_path <- "C:/Users/tahoang/Downloads/2024-05-07T11-53-47McsRecording_E-00190_Recording-0_(Data Acquisition (1);MEA2100-Mini; Electrode Raw Data1)_Analog.csv"

df <- fread(file_path, skip = 6)

# Time in seconds
time <- df[[1]] / 1e6

# Check sampling interval
dt <- median(diff(time))

# Calculate sampling frequency
fs <- 1 / dt

cat("Sampling interval:", dt, "seconds\n")
cat("Sampling frequency:", fs, "Hz\n")
