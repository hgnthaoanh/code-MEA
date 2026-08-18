
library(data.table)

# Read file

file_path <- "C:/Users/tahoang/Downloads/2024-05-07T11-53-47McsRecording_E-00190_Recording-0_(Data Acquisition (1);MEA2100-Mini; Electrode Raw Data1)_Analog.csv"

df <- fread(file_path, skip = 6)


# Time and signals,

time <- df[[1]] / 1e6

signals <- df[, -1, with = FALSE]

electrodes <- names(signals)

cat("Number of electrodes:",
    length(electrodes),
    "\n")


recording_time <- max(time)

cat("Recording duration:",
    recording_time,
    "seconds\n")


# Choose electrode

library(data.table)

# Read file

file_path <- "C:/Users/tahoang/Downloads/2024-05-07T11-53-47McsRecording_E-00190_Recording-0_(Data Acquisition (1);MEA2100-Mini; Electrode Raw Data1)_Analog.csv"

df <- fread(file_path, skip = 6)


# Time and signals,

time <- df[[1]] / 1e6

signals <- df[, -1, with = FALSE]

electrodes <- names(signals)

cat("Number of electrodes:",
    length(electrodes),
    "\n")


recording_time <- max(time)

cat("Recording duration:",
    recording_time,
    "seconds\n")


# Choose electrode

electrode <- electrodes[1]

cat("Electrode:",
    electrode,
    "\n")

signal <- signals[[electrode]]


# Parameters

sampling_rate <- 20000

refractory_ms <- 1

refractory <- round(
  refractory_ms / 1000 * sampling_rate
)


# Spike detection function

detect_spikes <- function(signal, threshold_factor){
  
  sigma <- mad(signal, constant = 1.4826)
  
  threshold <- median(signal) -
    threshold_factor * sigma
  
  
  crossings <- which(
    diff(signal < threshold) == 1
  ) + 1
  
  
  spike_idx <- c()
  
  last_spike <- -Inf
  
  
  for(idx in crossings){
    
    if(idx - last_spike > refractory){
      
      window <- idx:min(
        idx + refractory,
        length(signal)
      )
      
      
      peak <- window[
        which.min(signal[window])
      ]
      
      
      spike_idx <- c(
        spike_idx,
        peak
      )
      
      
      last_spike <- peak
    }
  }
  
  
  return(spike_idx)
}

spike_counts <- sapply(signals, function(x)
  length(detect_spikes(x, 4))
)

best_electrode <- names(which.max(spike_counts))

cat("Best electrode:", best_electrode, "\n")

electrode <- best_electrode
signal <- signals[[electrode]]

# Compare threshold 4 and 4.5

sigma <- mad(signal, constant = 1.4826)

median_signal <- median(signal)


threshold35 <- median_signal - 3.5*sigma
threshold40 <- median_signal - 4*sigma
threshold45 <- median_signal - 4.5*sigma


spike_idx_4 <- detect_spikes(
  signal,
  4
)


spike_idx_45 <- detect_spikes(
  signal,
  4.5
)


cat("\n====================\n")
cat("Threshold factor: 4\n")
cat("Spikes:",
    length(spike_idx_4),
    "\n")


cat("\n====================\n")
cat("Threshold factor: 4.5\n")
cat("Spikes:",
    length(spike_idx_45),
    "\n")


retention <- length(spike_idx_45) /
  length(spike_idx_4)


cat("\nRetention 4.5 vs 4:",
    round(retention*100,2),
    "%\n")


# Choose threshold 4 for visualization

spike_idx <- spike_idx_4


cat("\nFiring rate:",
    length(spike_idx)/recording_time,
    "Hz\n")



# Find most active 1-second window

spike_times <- time[spike_idx]


bins <- seq(
  0,
  recording_time + 1,
  by = 1
)


hist_spikes <- hist(
  spike_times,
  breaks = bins,
  plot = FALSE
)


best_window <- which.max(
  hist_spikes$counts
)


start_time <- hist_spikes$breaks[best_window]

end_time <- start_time + 1


cat("Most active window:",
    start_time,
    "-",
    end_time,
    "seconds\n")


# Plot

idx <- which(
  time >= start_time &
    time <= end_time
)


zoom_spikes <- spike_idx[
  time[spike_idx] >= start_time &
    time[spike_idx] <= end_time
]


plot(
  time[idx],
  signal[idx],
  type="l",
  xlab="Time (s)",
  ylab="Voltage",
  main=paste(
    electrode,
    "- Most active 1 second"
  )
)


abline(
  h=threshold35,
  col="green",
  lty=2
)

abline(
  h=threshold40,
  col="red",
  lty=2
)

abline(
  h=threshold45,
  col="blue",
  lty=2
)


points(
  time[zoom_spikes],
  signal[zoom_spikes],
  col="magenta",
  pch=16
)


legend(
  "bottomleft",
  legend=c(
    "3.5 MAD",
    "4 MAD",
    "4.5 MAD",
    "Spikes (4 MAD)"
  ),
  col=c(
    "green",
    "red",
    "blue",
    "magenta"
  ),
  lty=c(2,2,2,NA),
  pch=c(NA,NA,NA,16),
  bty="n"
)


# ==============================
# Raster plot for selected electrode
# ==============================

# Spike times in seconds
spike_times <- time[spike_idx]
# ==========================================
# Raster plot - Last 60 seconds
# ==========================================
# Raster plot - 1062 to 1063 seconds
# Keep ORIGINAL recording time
# ==========================================

raster_start <- 1062
raster_end   <- 1063

# Spikes detected using the same 4 MAD detection
raster_spikes <- spike_idx_4[
  time[spike_idx_4] >= raster_start &
    time[spike_idx_4] <= raster_end
]

# KEEP ORIGINAL TIME
raster_spike_times <- time[raster_spikes]

cat("\n====================\n")
cat("Raster window:",
    raster_start,
    "-",
    raster_end,
    "seconds\n")

cat("Number of spikes:",
    length(raster_spikes),
    "\n")


# ==========================================
# Plot
# ==========================================

plot(
  c(raster_start, raster_end),
  c(0.5, 1.5),
  type = "n",
  xlim = c(raster_start, raster_end),
  ylim = c(0.5, 1.5),
  
  xlab = "Time (s)",
  ylab = "",
  
  yaxt = "n",
  
  main = paste(
    electrode,
    "- Spike Raster Plot"
  )
)


# Draw spikes at ORIGINAL recording time
segments(
  x0 = raster_spike_times,
  y0 = 0.8,
  x1 = raster_spike_times,
  y1 = 1.2
)


# Y-axis
axis(
  2,
  at = 1,
  labels = electrode,
  las = 1
)


# X-axis: explicitly show 1062 -> 1063
axis(
  1,
  at = seq(1062, 1063, by = 0.1),
  labels = seq(1062, 1063, by = 0.1)
)
