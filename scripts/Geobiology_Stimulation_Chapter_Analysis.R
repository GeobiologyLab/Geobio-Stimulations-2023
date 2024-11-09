# Note: ChatGPT (version 4) was used to help clean up and comment parts of the code.

library(reshape2)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(readr)
library(stats)
library(ggpattern)
library(grid)
library(lubridate)
library(plotly)
library(jsonlite)


# Script for Geobiology Stimulations Thesis Chapter Figures

########### FIGURE 4.4 - Stimulation Procedure ###########

###########A###########
geomonitor <- read_table("../data/DAT-861.DAT")
baseline <- geomonitor[-c(1:4),c(1:2,43)]
baseline <- baseline[c(5041:13681),]


# time elapsed
baseline$Time <- as.POSIXct(baseline$Time, format="%H:%M:%S")
tdiff_base <- as.numeric(difftime(baseline$Time, baseline$Time[1], units = "mins"))
baseline$Time_Elapsed <- tdiff_base

names(baseline)[names(baseline) == "Sensor40"] <- "Pressure"

# Ensure Pressure is numeric, with error handling for non-numeric values
baseline$Pressure <- as.numeric(baseline$Pressure)

#plot
s <- ggplot(baseline, aes(Time_Elapsed)) +
  geom_line(aes(y = Pressure, color = "Pressure"), size = 2) +
  labs(x = "Time (minutes)", y = "Pressure (MPa)") +  # Set the y-axis label to "Pressure (MPa)"
  theme(
    axis.title.y.left = element_text(margin = margin(r = 20)),
    axis.title.x.bottom = element_text(margin = margin(t = 10)),
    legend.position = "top",
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)) +  # Adjust the legend text size here
  scale_color_manual(values = c("Pressure" = "black")) +
  labs(color = "Baseline") +
  ylim(0,205)

###########B###########
dataGeoAug3 <- read.csv("../data/geomonitor_aug3.csv")
# time elapsed
dataGeoAug3$Time <- as.POSIXct(dataGeoAug3$Time, format="%H:%M:%S")
tdiff_aug3 <- as.numeric(difftime(dataGeoAug3$Time, dataGeoAug3$Time[1], units = "mins"))
dataGeoAug3$Time_Elapsed <- tdiff_aug3
# subset dataframes for just stimulation and flowback
aug3_stim <- dataGeoAug3[1761:5644,]
# time elapsed
tdiff_aug3 <- as.numeric(difftime(aug3_stim$Time, aug3_stim$Time[1], units = "mins"))
aug3_stim$Time_Elapsed <- tdiff_aug3

ylim.prim <- c(0, 220)   # Pressure
ylim.sec <- c(0, 100)    # flowrate

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] 

ggplot(aug3_stim, aes(Time_Elapsed)) +
  geom_line(aes(y = Pressure, color = "Pressure"), size = 2) +
  geom_line(aes(y = a + Flowrate * b, color = "Flowrate"), size = 2) +
  labs(x = "Time (minutes)") +
  scale_y_continuous("Pressure (MPa)", sec.axis = sec_axis(~ (. - a) / b, name = "Flowrate (lpm)")) +
  theme(axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red", margin = margin(l = 20)), 
        axis.title.y.left = element_text(margin = margin(r = 20)),
        axis.title.x.bottom = element_text(margin = margin(t = 10)),
        legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +  # Adjust the legend text size here
  scale_color_manual(values = c("Pressure" = "black", "Flowrate" = "red")) +
  labs(color = "Low Seismicity", )


###########C###########
dataGeoAug10 <- read.csv("../data/geomonitor_aug10.csv")
# time elapsed
dataGeoAug10$Time <- as.POSIXct(dataGeoAug10$Time, format="%H:%M:%S")
tdiff_aug10 <- as.numeric(difftime(dataGeoAug10$Time, dataGeoAug10$Time[1], units = "mins"))
dataGeoAug10$Time_Elapsed <- tdiff_aug10
# subset dataframes for just stimulation and flowback
aug10_stim <- dataGeoAug10[1291:5412,]
# time elapsed
tdiff_aug10 <- as.numeric(difftime(aug10_stim$Time, aug10_stim$Time[1], units = "mins"))
aug10_stim$Time_Elapsed <- tdiff_aug10

ggplot(aug10_stim, aes(Time_Elapsed)) +
  geom_line(aes(y = Pressure, color = "Pressure"), size = 2) +
  geom_line(aes(y = a + Flowrate * b, color = "Flowrate"), size = 2) +
  labs(x = "Time (minutes)") +
  scale_y_continuous("Pressure (MPa)", sec.axis = sec_axis(~ (. - a) / b, name = "Flowrate (lpm)")) +
  theme(axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red", margin = margin(l = 20)), 
        axis.title.y.left = element_text(margin = margin(r = 20)),
        axis.title.x.bottom = element_text(margin = margin(t = 10)),
        legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +  # Adjust the legend text size here
  scale_color_manual(values = c("Pressure" = "black", "Flowrate" = "red")) +
  labs(color = "High Seismicity")


########### FIGURE 4.5 - H2O2 levels of interval 11 water ###########
cbPalette <- c( "#CC79A7","#D55E00", "#0072B2")

h2o2_all <- read.csv("../data/h2o2_all2.csv")
h2o2_stim <- h2o2_all[1:44,]
# Replace negative values in the H2O2 column with 0.00625
h2o2_all$H2O2[h2o2_all$H2O2 < 0] <- 0.00625

ggplot(h2o2_all, aes(x=Liters, y=H2O2, color=Experiment)) + 
  geom_point(size = 4) + 
  theme_bw(base_size = 18) + 
  #ggtitle ("H2O2 in ST1-int11 flowback waters") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title.y.left = element_text(margin = margin(r = 20)),
        axis.title.x.bottom = element_text(margin = margin(t = 10)),) +
  ylab(expression(paste("Hydrogen Peroxide (", mu, "M)"))) +
  xlab("Outflow (liters)") +
  scale_colour_manual(values=cbPalette) +  
  geom_errorbar(
    aes(ymin = H2O2 - StdDev, ymax = H2O2 + StdDev),  
    width = 14) 


########### FIGURE 4.6 - Redox potential of ST1 interval 11 water ###########
# Baseline
dataORPbase <- read.csv("../data/ORP_base.csv")
# Add day to Time
dataORPbase$Time <- paste0("2023-07-27 ", dataORPbase$Time)
# reformat time column
dataORPbase$Time <- as.POSIXct(dataORPbase$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_base <- as.numeric(difftime(dataORPbase$Time, dataORPbase$Time[1], units = "hours"))
dataORPbase$Time_Elapsed <- tdiff_base

# Aug 3
dataORP1_1 <- read.csv("../data/Day1ORP_s1.csv")
# Add day to Time
dataORP1_1$Time <- paste0("2023-08-03 ", dataORP1_1$Time)
# Data Day 2
dataORP2_1 <- read.csv("../data/Day2ORP_s1.csv")
# Add day to Time
dataORP2_1$Time <- paste0("2023-08-04 ", dataORP2_1$Time)
# Append Day 2 to Day 1
dataORP_1 <- rbind(dataORP1_1, dataORP2_1)
# reformat time column
dataORP_1$Time <- as.POSIXct(dataORP_1$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_st1 <- as.numeric(difftime(dataORP_1$Time, dataORP_1$Time[1], units = "hours"))
dataORP_1$Time_Elapsed <- tdiff_st1

# Aug 10
dataORP1_2 <- read.csv("../data/Day1ORP_s2.csv")
# Add day to Time
dataORP1_2$Time <- paste0("2023-08-10 ", dataORP1_2$Time)
# Data Day 2
dataORP2_2 <- read.csv("../data/Day2ORP_s2.csv")
# Add day to Time
dataORP2_2$Time <- paste0("2023-08-11 ", dataORP2_2$Time)
# Append Day 2 to Day 1
dataORP_2 <- rbind(dataORP1_2, dataORP2_2)
# reformat time column
dataORP_2$Time <- as.POSIXct(dataORP_2$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_st2 <- as.numeric(difftime(dataORP_2$Time, dataORP_2$Time[1], units = "hours"))
dataORP_2$Time_Elapsed <- tdiff_st2

# Append
dataORP <- rbind(dataORPbase,dataORP_1, dataORP_2)

# Convert to numeric
dataORPbase$ORP <- as.numeric(dataORPbase$ORP)

# Round time to the nearest 2 minutes
dataORPbase <- dataORPbase %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average pH and Time_Elapsed for every 2-minute interval
dataORP_avg <- dataORPbase %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(
    avg_ORP = mean(ORP, na.rm = TRUE),
    avg_Time_Elapsed = mean(Time_Elapsed, na.rm = TRUE)
  )

# Convert to numeric
dataORP_1$ORP <- as.numeric(dataORP_1$ORP)

# Round time to the nearest 2 minutes
dataORP_1 <- dataORP_1 %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average orp for every 2-minute interval
dataORP_avg2 <- dataORP_1 %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(avg_ORP = mean(ORP, na.rm = TRUE))

# Convert to numeric
dataORP_2$ORP <- as.numeric(dataORP_2$ORP)

# Round time to the nearest 2 minutes
dataORP_2 <- dataORP_2 %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average orp for every 2-minute interval
dataORP_avg3 <- dataORP_2 %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(avg_ORP = mean(ORP, na.rm = TRUE))

dataORP_avg <- dataORP_avg[,-4]

# interpolate liters outflow so I can combine all 3 plots and plot by outflow

# first for high seismicity
outflow_high <- read.csv("../data/high_liters.csv")
# reformat time column
outflow_high$Time <- as.POSIXct(outflow_high$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for dataORP_avg3
dataORP_avg3$Liters_Outflow <- approx(outflow_high$Time, outflow_high$Liters, xout = dataORP_avg3$Time_rounded)$y

# add Water column
dataORP_avg3$Water <- "Formation"
dataORP_avg3$Water[1:69] <- "Borehole"

# low seismicity
outflow_low <- read.csv("../data/low_liters.csv")
# reformat time column
outflow_low$Time <- as.POSIXct(outflow_low$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for dataORP_avg3
dataORP_avg2$Liters_Outflow <- approx(outflow_low$Time, outflow_low$Liters, xout = dataORP_avg2$Time_rounded)$y

# add Water column
dataORP_avg2$Water <- "Formation"
dataORP_avg2$Water[1:100] <- "Borehole"

# baseline
outflow_base <- read.csv("../data/base_liters.csv")
# reformat time column
outflow_base$Time <- as.POSIXct(outflow_base$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for dataORP_avg3
dataORP_avg$Liters_Outflow <- approx(outflow_base$Time, outflow_base$Liters, xout = dataORP_avg$Time_rounded)$y

# add Water column
dataORP_avg$Water <- "Formation"
dataORP_avg$Water[1:130] <- "Borehole"


# Combine the dataframes
combined_data <- rbind(dataORP_avg, dataORP_avg2, dataORP_avg3)

# Create a new column that combines Experiment and Water
combined_data$Experiment_Water <- interaction(combined_data$Experiment, combined_data$Water)

cbPalette <- c( "#CC79A7", "#D55E00", "#0072B2")

# Define custom colors for the combinations of Experiment and Water
color_palette <- c(
  "Baseline.Formation" = "#CC79A7",    # Blue for Formation
  "Baseline.Borehole" = "#E6A9C5",     # Lighter Blue for Borehole
  "Low Seismicity.Formation" = "#0072B2",    # Orange for Formation
  "Low Seismicity.Borehole" = "#56B4E9",     # Lighter Orange for Borehole
  "High Seismicity.Formation" = "#D55E00",    # Green for Formation
  "High Seismicity.Borehole" = "#E69F00"      # Lighter Green for Borehole
)

# plot
ggplot(combined_data, aes(x=Liters_Outflow, y=avg_ORP, color=Experiment_Water)) + 
  geom_line(size = 1) +  
  geom_point(size = 1) +  
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none") +
  scale_y_continuous(breaks = pretty(combined_data$avg_ORP, n = 10)) +
  ylab("Redox Potential (mV)") +
  xlab("Outflow (liters)") +
  scale_colour_manual(values=color_palette) +
  guides(color = guide_legend(override.aes = list(size = 3)))

########### FIGURE 4.7 - EC of ST1 interval 11 water ###########
# Baseline
dataECbase <- read.csv("../data/EC_base.csv")
# Add day to Time
dataECbase$Time <- paste0("2023-07-27 ", dataECbase$Time)
# reformat time column
dataECbase$Time <- as.POSIXct(dataECbase$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_base <- as.numeric(difftime(dataECbase$Time, dataECbase$Time[1], units = "hours"))
dataECbase$Time_Elapsed <- tdiff_base


# Aug 3
dataEC1_1 <- read.csv("../data/Day1EC_s1.csv")
# Add day to Time
dataEC1_1$Time <- paste0("2023-08-03 ", dataEC1_1$Time)
# Data Day 2
dataEC2_1 <- read.csv("../data/Day2EC_s1.csv")
# Add day to Time
dataEC2_1$Time <- paste0("2023-08-04 ", dataEC2_1$Time)
# Append Day 2 to Day 1
dataEC_1 <- rbind(dataEC1_1, dataEC2_1)
# reformat time column
dataEC_1$Time <- as.POSIXct(dataEC_1$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_st1 <- as.numeric(difftime(dataEC_1$Time, dataEC_1$Time[1], units = "hours"))
dataEC_1$Time_Elapsed <- tdiff_st1

# Aug 10
dataEC1_2 <- read.csv("../data/Day1EC_s2.csv")
# Add day to Time
dataEC1_2$Time <- paste0("2023-08-10 ", dataEC1_2$Time)
# Data Day 2
dataEC2_2 <- read.csv("../data/Day2EC_s2.csv")
# Add day to Time
dataEC2_2$Time <- paste0("2023-08-11 ", dataEC2_2$Time)
# Append Day 2 to Day 1
dataEC_2 <- rbind(dataEC1_2, dataEC2_2)
# reformat time column
dataEC_2$Time <- as.POSIXct(dataEC_2$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_st2 <- as.numeric(difftime(dataEC_2$Time, dataEC_2$Time[1], units = "hours"))
dataEC_2$Time_Elapsed <- tdiff_st2

# Append
dataEC <- rbind(dataECbase,dataEC_1, dataEC_2)

# Round time to the nearest 2 minutes
dataECbase <- dataECbase %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average EC and Time_Elapsed for every 2-minute interval
dataEC_avg <- dataECbase %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(
    avg_EC = mean(EC, na.rm = TRUE),
    avg_Time_Elapsed = mean(Time_Elapsed, na.rm = TRUE)
  )

# Round time to the nearest 2 minutes
dataEC_1 <- dataEC_1 %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average EC for every 2-minute interval
dataEC_avg2 <- dataEC_1 %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(avg_EC = mean(EC, na.rm = TRUE))

# Round time to the nearest 2 minutes
dataEC_2 <- dataEC_2 %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average EC for every 2-minute interval
dataEC_avg3 <- dataEC_2 %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(avg_EC = mean(EC, na.rm = TRUE))

dataEC_avg <- dataEC_avg[,-4]

# interpolate liters outflow so I can combine all 3 plots and plot by outflow

# first for high seismicity
outflow_high <- read.csv("../data/high_liters.csv")
# reformat time column
outflow_high$Time <- as.POSIXct(outflow_high$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for dataEC_avg3
dataEC_avg3$Liters_Outflow <- approx(outflow_high$Time, outflow_high$Liters, xout = dataEC_avg3$Time_rounded)$y

# add Water column
dataEC_avg3$Water <- "Formation"
dataEC_avg3$Water[1:69] <- "Borehole"

# low seismicity
outflow_low <- read.csv("../data/low_liters.csv")
# reformat time column
outflow_low$Time <- as.POSIXct(outflow_low$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for dataEC_avg3
dataEC_avg2$Liters_Outflow <- approx(outflow_low$Time, outflow_low$Liters, xout = dataEC_avg2$Time_rounded)$y

# add Water column
dataEC_avg2$Water <- "Formation"
dataEC_avg2$Water[1:100] <- "Borehole"

# baseline
outflow_base <- read.csv("../data/base_liters.csv")
# reformat time column
outflow_base$Time <- as.POSIXct(outflow_base$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for dataEC_avg3
dataEC_avg$Liters_Outflow <- approx(outflow_base$Time, outflow_base$Liters, xout = dataEC_avg$Time_rounded)$y

# add Water column
dataEC_avg$Water <- "Formation"
dataEC_avg$Water[1:130] <- "Borehole"


# Combine the dataframes
combined_data <- rbind(dataEC_avg, dataEC_avg2, dataEC_avg3)

# Create a new column that combines Experiment and Water
combined_data$Experiment_Water <- interaction(combined_data$Experiment, combined_data$Water)

cbPalette <- c( "#CC79A7", "#D55E00", "#0072B2")

# Define custom colors for the combinations of Experiment and Water
color_palette <- c(
  "Baseline.Formation" = "#CC79A7",    # Blue for Formation
  "Baseline.Borehole" = "#E6A9C5",     # Lighter Blue for Borehole
  "Low Seismicity.Formation" = "#0072B2",    # Orange for Formation
  "Low Seismicity.Borehole" = "#56B4E9",     # Lighter Orange for Borehole
  "High Seismicity.Formation" = "#D55E00",    # Green for Formation
  "High Seismicity.Borehole" = "#E69F00"      # Lighter Green for Borehole
)

# plot
ggplot(combined_data, aes(x=Liters_Outflow, y=avg_EC, color=Experiment_Water)) + 
  geom_line(size = 2) +  # Increase the size value to make the lines thicker
  geom_point(size = 2) +  # Add points with differentiation by Water
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none") +
  scale_y_continuous(breaks = pretty(combined_data$avg_EC, n = 10)) +
  ylab(expression(paste("Electrical Conductivity ( ", mu, "S cm"^-1, ")"))) +
  xlab("Outflow (liters)") +
  scale_colour_manual(values=color_palette) +
  guides(color = guide_legend(override.aes = list(size = 3)))

########### FIGURE 4.8 - pH of ST1 interval 11 water ###########
# Baseline
datapHbase <- read.csv("../data/pH_base.csv")
# Add day to Time
datapHbase$Time <- paste0("2023-07-27 ", datapHbase$Time)
# reformat time column
datapHbase$Time <- as.POSIXct(datapHbase$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_base <- as.numeric(difftime(datapHbase$Time, datapHbase$Time[1], units = "hours"))
datapHbase$Time_Elapsed <- tdiff_base


# Aug 3
datapH1_1 <- read.csv("../data/Day1pH_s1.csv")
# Add day to Time
datapH1_1$Time <- paste0("2023-08-03 ", datapH1_1$Time)
# Data Day 2
datapH2_1 <- read.csv("../data/Day2pH_s1.csv")
# Add day to Time
datapH2_1$Time <- paste0("2023-08-04 ", datapH2_1$Time)
# Append Day 2 to Day 1
datapH_1 <- rbind(datapH1_1, datapH2_1)
# reformat time column
datapH_1$Time <- as.POSIXct(datapH_1$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_st1 <- as.numeric(difftime(datapH_1$Time, datapH_1$Time[1], units = "hours"))
datapH_1$Time_Elapsed <- tdiff_st1

# Aug 10
datapH1_2 <- read.csv("../data/Day1pH_s2.csv")
# Add day to Time
datapH1_2$Time <- paste0("2023-08-10 ", datapH1_2$Time)
# Data Day 2
datapH2_2 <- read.csv("../data/Day2pH_s2.csv")
# Add day to Time
datapH2_2$Time <- paste0("2023-08-11 ", datapH2_2$Time)
# Append Day 2 to Day 1
datapH_2 <- rbind(datapH1_2, datapH2_2)
# reformat time column
datapH_2$Time <- as.POSIXct(datapH_2$Time, format="%Y-%m-%d %H:%M:%S")
# time elapsed
tdiff_st2 <- as.numeric(difftime(datapH_2$Time, datapH_2$Time[1], units = "hours"))
datapH_2$Time_Elapsed <- tdiff_st2

# Append
datapH <- rbind(datapHbase,datapH_1, datapH_2)

# Rename experiments
datapH <- datapH %>%
  mutate(Experiment = case_when(
    Experiment == "July 27" ~ "Baseline",
    Experiment == "August 3-4" ~ "Low Seismicity",
    Experiment == "August 10-11" ~ "High Seismicity",
    TRUE ~ Experiment  # Keep other values unchanged
  ))

# Round time to the nearest 2 minutes
datapHbase <- datapHbase %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average pH and Time_Elapsed for every 2-minute interval
datapH_avg <- datapHbase %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(
    avg_pH = mean(pH, na.rm = TRUE),
    avg_Time_Elapsed = mean(Time_Elapsed, na.rm = TRUE)
  )

# Round time to the nearest 2 minutes
datapH_1 <- datapH_1 %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average pH for every 2-minute interval
datapH_avg2 <- datapH_1 %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(avg_pH = mean(pH, na.rm = TRUE))

# Round time to the nearest 2 minutes
datapH_2 <- datapH_2 %>%
  mutate(Time_rounded = floor_date(Time, unit = "2 minutes"))

# Calculate the average pH for every 2-minute interval
datapH_avg3 <- datapH_2 %>%
  group_by(Time_rounded, Experiment) %>%
  summarise(avg_pH = mean(pH, na.rm = TRUE))

datapH_avg <- datapH_avg[,-4]

# interpolate liters outflow so I can combine all 3 plots and plot by outflow

# first for high seismicity
outflow_high <- read.csv("../data/high_liters.csv")
# reformat time column
outflow_high$Time <- as.POSIXct(outflow_high$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for datapH_avg3
datapH_avg3$Liters_Outflow <- approx(outflow_high$Time, outflow_high$Liters, xout = datapH_avg3$Time_rounded)$y

# add Water column
datapH_avg3$Water <- "Formation"
datapH_avg3$Water[1:69] <- "Borehole"

# low seismicity
outflow_low <- read.csv("../data/low_liters.csv")
# reformat time column
outflow_low$Time <- as.POSIXct(outflow_low$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for datapH_avg3
datapH_avg2$Liters_Outflow <- approx(outflow_low$Time, outflow_low$Liters, xout = datapH_avg2$Time_rounded)$y

# add Water column
datapH_avg2$Water <- "Formation"
datapH_avg2$Water[1:100] <- "Borehole"

datapH_avg2 <- datapH_avg2[-c(1:2),]

# baseline
outflow_base <- read.csv("../data/base_liters.csv")
# reformat time column
outflow_base$Time <- as.POSIXct(outflow_base$Time, format="%Y-%m-%d %H:%M:%S")

# Interpolate Liters_Outflow for datapH_avg3
datapH_avg$Liters_Outflow <- approx(outflow_base$Time, outflow_base$Liters, xout = datapH_avg$Time_rounded)$y

# add Water column
datapH_avg$Water <- "Formation"
datapH_avg$Water[1:130] <- "Borehole"


# Combine the dataframes
combined_data <- rbind(datapH_avg, datapH_avg2, datapH_avg3)

# Create a new column that combines Experiment and Water
combined_data$Experiment_Water <- interaction(combined_data$Experiment, combined_data$Water)

cbPalette <- c( "#CC79A7", "#D55E00", "#0072B2")

# Define custom colors for the combinations of Experiment and Water
color_palette <- c(
  "July 27.Formation" = "#CC79A7",    # Blue for Formation
  "July 27.Borehole" = "#E6A9C5",     # Lighter Blue for Borehole
  "August 3-4.Formation" = "#0072B2",    # Orange for Formation
  "August 3-4.Borehole" = "#56B4E9",     # Lighter Orange for Borehole
  "August 10-11.Formation" = "#D55E00",    # Green for Formation
  "August 10-11.Borehole" = "#E69F00"      # Lighter Green for Borehole
)

# plot
ggplot(combined_data, aes(x=Liters_Outflow, y=avg_pH, color=Experiment_Water)) + 
  geom_line(size = 1) +  # Increase the size value to make the lines thicker
  geom_point(size = 1) +  # Add points with differentiation by Water
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none") +
  scale_y_continuous(breaks = pretty(combined_data$avg_pH, n = 10)) +
  ylab("pH") +
  xlab("Outflow (liters)") +
  scale_colour_manual(values=color_palette) +
  guides(color = guide_legend(override.aes = list(size = 3)))

########### FIGURE 4.9 - Seismic activity resulting from hydraulic stimulation ###########

###########A###########
epm <- read.csv("../data/epm.csv")

# August 3
ggplot(epm, aes(x = Time_Elapsed, y = EPM_Aug3)) +
  geom_bar(stat = "identity", fill = "#0072B2", color = "black") +
  labs(x = "Time (minutes)", y = "Events Per 10 Minutes") +
  theme(axis.title.y.left = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  ylim(0,750) 

# August 10
ggplot(epm, aes(x = Time_Elapsed, y = EPM_Aug10)) +
  geom_bar(stat = "identity", fill = "#D55E00", color = "black") +
  labs(x = "Time (minutes)", y = "Events Per 10 Minutes") +
  theme(axis.title.y.left = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) 

# Both

ggplot() +
  geom_bar(data = epm, aes(x = Time_Elapsed, y = EPM_Aug10, fill = "High Seismicity"), 
           stat = "identity", color = "black", alpha = 0.8) +  # August 10 with transparency
  geom_bar(data = epm, aes(x = Time_Elapsed, y = EPM_Aug3, fill = "Low Seismicity"), 
           stat = "identity", color = "black", alpha = 0.8) +  # August 3 with transparency
  labs(x = "Time (minutes)", y = "Events Per 10 Minutes") +
  theme(axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.position = "top",                # Position legend at the top
        legend.direction = "horizontal",        # Arrange legend items horizontally
        legend.title = element_blank(),         # Remove the legend title
        legend.text = element_text(size = 18)) +# Adjust legend text size
  scale_fill_manual(values = c("Low Seismicity" = "#0072B2", "High Seismicity" = "#D55E00")) +
  ylim(0,750)

###########B###########
s1 <- read.csv('../data/Aug3.csv')
s2 <- read.csv('../data/Aug10.csv')

# Parse the isotime column to POSIXct format
s1 <- s1 %>%
  mutate(isotime = ymd_hms(isotime) + hours(2))

# Define the limits for the x-axis
start_time <- ymd_hms("2023-08-03 11:00:00")
end_time <- ymd_hms("2023-08-03 15:00:00")

# Create the ggplot
ggplot(s1, aes(x = isotime, y = magnitude)) +
  geom_point(aes(size = magnitude), color = '#0072B2', alpha=0.6) +
  theme_bw(base_size = 22) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Magnitude") +
  xlab("Time") +
  scale_size_continuous(range = c(1, 3)) + 
  scale_x_datetime(date_labels = "Aug. 3 %H:%M", date_breaks = "1 hour", limits = c(start_time, end_time)) +
  ylim(-4.8,-2.5)


###########C###########
s2 <- s2 %>%
  mutate(isotime = ymd_hms(isotime) + hours(2))

# Define the limits for the x-axis
start_time <- ymd_hms("2023-08-10 11:00:00")
end_time <- ymd_hms("2023-08-10 16:00:00")

# Create the ggplot
ggplot(s2, aes(x = isotime, y = magnitude)) +
  geom_point(aes(size = magnitude), color = '#D55E00', alpha=0.6) +
  theme_bw(base_size = 22) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Magnitude") +
  xlab("Time") +
  scale_size_continuous(range = c(1, 3)) + 
  scale_x_datetime(date_labels = "Aug. 10 %H:%M", date_breaks = "1 hour", limits = c(start_time, end_time)) +
  ylim(-4.8,-2.5)

###########D###########
# Load borehole coordinates dictionary
data <- readLines('../data/borehole-coordinates-dictionary.txt')
d <- jsonlite::fromJSON(data)

# Combine the coordinates from the JSON data into a single data frame
coordinates_df <- data.frame(d$ST1)

cbPalette <- c( "#CC79A7", "#D55E00", "#0072B2")

# Create a 3D scatter plot with plotly
plot.new()
p <- plot_ly() %>%
  add_trace(data = s1, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers',
            marker = list(size = 2, opacity = 0.8, color = '#0072B2'), name = 'Low Seismicity') %>%
  add_trace(data = s2, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers',
            marker = list(size = 2, opacity = 0.3, color = '#D55E00'), name = 'High Seismicity') %>%
  add_trace(data = coordinates_df, x = ~Easting, y = ~Northing, z = ~Z, type = 'scatter3d', mode = 'markers',
            marker = list(size = 3, opacity = 0.6, color = 'gray'), name = 'ST1') %>%
  layout(scene = list(title = "Stimulation of ST1 Borehole, August 2023",
                      xaxis = list(title = "Easting"),
                      yaxis = list(title = "Northing"),
                      zaxis = list(title = "Z"))) 
p

########### FIGURE S1.6 - Grain-size dependent mechanoradical ROS production ###########

###########A###########
df6 <- read.csv("../data/table_granite.csv")
df6$Sample <- gsub("um", "μm", df6$Sample)
df6$Time <- as.factor(df6$Time)
df6$Sample <- factor(df6$Sample,levels = c("Granite + Water (<45 μm)","Granite + Water (45-106 μm)","Granite + Water (106-150 μm)"))
cbbbPalette <- c("springgreen4","darkslateblue",  "darkturquoise")

ggplot(df6, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), size =0.7) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9), 
                width = 0.25,  
                size = 0.8) +  
  theme_bw() + 
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2] ~ (mu*M))) +
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),  
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  scale_fill_manual(values=cbbbPalette) + ylim(0,15)


###########B###########
granite <- read.csv("../data/table4.csv")
granite$Sample <- gsub("um", "μm", granite$Sample)
granite$Time <- as.factor(granite$Time)

granite$Sample <- factor(granite$Sample,levels = c("Granite + Water (<45 μm)","Granite + Water (45-106 μm)","Granite + Water (106-150 μm)"))

cbbbPalette <- c("springgreen4","darkslateblue",  "darkturquoise")

ggplot(granite, aes(x=Time, y=H2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), size=0.7) +
  geom_errorbar(aes(ymin=H2-Std_Dev, ymax=H2+Std_Dev),
                position=position_dodge(.9), 
                width = 0.25,  # Adjusts the width of the error bars
                size = 0.8) +  # Adjusts the thickness of the error bars
  theme_bw() + 
  xlab("Time (hours)") +
  ylab(expression(H[2] ~ "(nmol/g rock)")) +
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),  
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "top") +
  scale_fill_manual(values=cbbbPalette)


