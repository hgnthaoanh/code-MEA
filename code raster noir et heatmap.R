library(data.table)

# Load data


file_path <- "C:/Users/tahoang/Downloads/2024-05-07T11-53-47McsRecording_E-00190_Recording-0_(Filter (2);Filter; Filter Data2)_Analog.csv"

df <- fread(file_path, skip = 6)

# Time and signals

time <- df[[1]] / 1e6

signals <- df[, -1, with = FALSE]

electrodes <- names(signals)

n_channels <- length(electrodes)

cat("Number of electrodes:", n_channels, "\n")


electrode_labels <- paste0("ID=", 0:(n_channels-1))


# Parameters

sampling_rate <- 20000

threshold_factor <- 4

refractory_ms <- 1

refractory <- round(refractory_ms / 1000 * sampling_rate)


# Spike detection

detect_spikes <- function(signal){
  
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
  
  return(time[spike_idx])
}


# Detect spikes

all_spikes <- vector("list", n_channels)

for(i in seq_len(n_channels)){
  
  cat("Processing", electrode_labels[i], "\n")
  
  all_spikes[[i]] <- detect_spikes(
    signals[[i]]
  )
  
}

names(all_spikes) <- electrode_labels


recording_time <- max(time)

window_size <- 60

starts <- seq(0, recording_time, by = window_size)

n_windows <- length(starts)

#== heatmap circle
#======================
# Heatmap of firing rate
#======================

library(ggplot2)

# Firing rate (Hz)
recording_time <- max(time)

firing_rate <- sapply(all_spikes, length) / recording_time

heatmap_data <- data.frame(
  electrode = electrode_labels,
  firing_rate = firing_rate
)

# ----- Layout -----

n_col <- 10
n_row <- ceiling(n_channels / n_col)

heatmap_data$x <- rep(1:n_col, each = n_row)[1:n_channels]
heatmap_data$y <- rep(n_row:1, times = n_col)[1:n_channels]

# ----- Text colour -----

mid <- median(heatmap_data$firing_rate, na.rm = TRUE)

heatmap_data$text_colour <- ifelse(
  heatmap_data$firing_rate > mid,
  "white",
  "black"
)

# ----- Plot -----

p <- ggplot(
  heatmap_data,
  aes(x = x, y = y)
) +
  
  geom_point(
    aes(fill = firing_rate),
    shape = 21,
    size = 16,
    colour = "black",
    stroke = 0.8
  ) +
  
  geom_text(
    aes(
      label = electrode,
      colour = text_colour
    ),
    size = 3,
    fontface = "bold"
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "#313695",
      "#74add1",
      "#ffffbf",
      "#f46d43",
      "#a50026"
    ),
    name = "Firing rate (Hz)"
  ) +
  
  scale_colour_identity() +
  
  coord_equal(
    xlim = c(0.2, 10.8),
    ylim = c(0.0, 7.0),
    clip = "off"
  ) +
  
  labs(
    title = "MEA Electrode Activity Heatmap",
    subtitle = paste(
      "Threshold =", threshold_factor, "× MAD"
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

print(p)

# Save heatmap
ggsave(
  "MEA_Heatmap.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 600
)

# Continuous raster plot
png(
  filename = "MEA4_Raster_Continuous.png",
  width = 4800,
  height = 3600,
  res = 600
)


par(
  mar=c(5,8,4,2)
)

plot(
  NA,
  xlim = c(0, recording_time),
  ylim = c(0.5, n_channels+0.5),
  xlab = "Time (s)",
  ylab = "Electrode",
  main = paste0(
    "MEA Raster Plot - ",
    round(recording_time/60,1),
    " min"
  ),
  yaxt = "n",
  xaxt = "n"
)


# X axis every 60 seconds
axis(
  1,
  at = seq(0, recording_time, by = 60),
  labels = seq(0, recording_time, by = 60)
)


axis(
  2,
  at=1:n_channels,
  labels=electrode_labels,
  las=2,
  cex.axis=0.5
)


for(i in seq_len(n_channels)){
  
  spikes <- all_spikes[[i]]
  
  if(length(spikes)>0){
    
    segments(
      x0=spikes,
      y0=i-0.35,
      x1=spikes,
      y1=i+0.35,
      lwd=0.5
    )
    
  }
  
}


box()

dev.off()


browseURL("MEA4_Raster_Continuous.png")


