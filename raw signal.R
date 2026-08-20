library(data.table)
library(ggplot2)

# 1. Read CSV

file_path <- "C:/Users/tahoang/Downloads/2024-05-07T11-53-47McsRecording_E-00190_Recording-0_(Data Acquisition (1);MEA2100-Mini; Electrode Raw Data1)_Analog.csv"

df <- fread(file_path, skip = 6)

# Time in seconds
time <- df[[1]] / 1e6

# Electrode signals
signals <- df[, -1, with = FALSE]

# 2. Choose one electrode

electrode <- names(signals)[45]

cat("Selected electrode:", electrode, "\n")

# 3. Select a short time window

start_time <- 0      # seconds
duration <- 5        # 5 seconds

idx <- time >= start_time &
  time <= (start_time + duration)

raw_signal <- data.frame(
  Time = time[idx],
  Voltage = signals[[electrode]][idx]
)

# 4. Plot raw signal

ggplot(raw_signal, aes(x = Time, y = Voltage)) +
  geom_line(linewidth = 0.3) +
  labs(
    title = paste("Signal électrique brut —", electrode),
    x = "Temps (s)",
    y = "Potentiel électrique"
  ) +
  theme_classic()

