library(plotly)  # load plotly first to avoid it overriding mutate!
library(sf)
library(stats)
library(tidyverse)
library(lme4)
library(MuMIn)
library(ordinal)
library(PMCMRplus) # for friedman analysis - likert scales
library(afex) # for repeated measures anova - perf. measures
library(emmeans) # for rmanova post-hoc analysis
library(MANOVA.RM)

#source("utils/clmcalcutils.R")
options(scipen = 999) # disable scientific notation

#load('data_feedback.rda')
load('data_all_actions.rda')
load('data_patterns.rda')
load('data_all_experiential.rda')
source("utils/visutils.R")
source("utils/lmecalcutils.R")
fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))



# Plotting speed data
#fig %>% add_trace(data=Sa %>% filter(Participant == 4, HitOrder == 16), type='scattergl',mode='markers', color=I('darkgrey'),
#                                     x=~bind_rows(speed_data)$t, y=~bind_rows(speed_data)$x) %>%
#  add_trace(data=Sa %>% filter(Participant == 4, HitOrder == 16), type='scattergl',mode='lines', color=I('darkgrey'),
#            x=~bind_rows(speed_smooth_data)$t, y=~bind_rows(speed_smooth_data)$x) %>%
#                           layout(showlegend=F,
#                                  yaxis=list( zeroline=F, tickfont=list(size=15), showticklabels=T))

var_names = c("Correspondence (1-7)" = "AlgoCorrespondFastSlow.f", 
              "Action Arm Travel (meter)" = "travel_arm", 
              "Action Duration (ms)" = "duration_ms", 
              "Straightness (0-1)" = "straightness", 
              "Peak Speed (m/s)" = "peak_speed_smooth",
              "Time to Peak Speed (ms)" = "time_to_peak_speed_smooth_ms",
              "Peak Speed to Target (\\%)" = "peak_speed_smooth_to_target_pct",
              "Fitts ID" = "fittsID",
              "Throughput (bits/s)" = "throughput",
              "Feedback" = "PerformanceFeedback.f",
              "Metric" = "JudgementType.f",
              "Learning" = "ActionOrderFB")

###
# Create Aggregate Summaries
###
Sa <- Sa %>% rowwise() %>% mutate(
  speed = st_length(
    st_cast(
      st_line_sample(
        st_linestring(data.matrix(data.frame(RightControllerPosWorld))),
        sample = c(0,1)),
      "LINESTRING")
  )
)

###
# Create a global speed trajectory
###

speed_data_list <- Sa$speed_smooth_data

Sa = Sa %>% mutate(duration_ms = duration * 1000)

# use the mean time, so the speed represents the mean duration of an action.
timemean = mean(Sa$duration_ms,na.rm=T)

# convert timestamps to relative time.
speed_data_list2 <- lapply(speed_data_list, function(df) {
  df$time = as.numeric(difftime(df$t,min(df$t,na.rm=T)))*1000
  timemax = max(df$time)
  df$time = scales::rescale(df$time,from=c(0,timemax), to=c(0,timemean))
  return(df)
})
                         
max_time <- max(sapply(speed_data_list2, function(df) max(df$time)))
min_time <- min(sapply(speed_data_list2, function(df) min(df$time)))

common_time_grid <- seq(min_time, max_time, length.out = 300)
interpolated_matrix <- matrix(NA, nrow = length(speed_data_list), ncol = length(common_time_grid))

for (i in 1:length(speed_data_list2)) {
  speed_values <- speed_data_list2[[i]]$x
  time_values <- speed_data_list2[[i]]$time
  if (length(time_values) > 1) {
  interpolated_speeds <- approx(time_values, speed_values, xout = common_time_grid)$y
  interpolated_matrix[i, ] <- interpolated_speeds
  }
}  

mean_trajectory <- colMeans(interpolated_matrix, na.rm = TRUE)

mean_trajectory_df <- data.frame(time = common_time_grid, mean_speed = mean_trajectory)

plot_ly(mean_trajectory_df, x = ~time, y = ~mean_speed, type = 'scatter', mode = 'lines') %>%
  layout(title = "Mean Trajectory of Speed Data",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Mean Speed"))

# Attempt a global average
# we use medians for our aproximators for now because there are some pretty big outliers.
Sa_a = Sa %>% ungroup() %>% summarise(
  ControllerLeaveTarget_ms = median(ControllerLeaveTarget_ms),
  ControllerHoverTarget_ms = median(ControllerHoverTarget_ms),
  duration_ms = mean(duration_ms),
)

Sa_a = mean_trajectory_df %>% summarise(
  peak_speed_smooth = max(mean_speed,na.rm=T),
  time_to_peak_speed_smooth_total_ms = na.omit(time[mean_speed == peak_speed_smooth]),
  peak_speed_smooth_to_target_total_ms = max(time) - time_to_peak_speed_smooth_total_ms,
) %>% bind_cols(Sa_a)

# Subtract Controller hover target from peak speed to target
Sa_a = Sa_a %>% mutate(
  peak_speed_smooth_to_target_ms = peak_speed_smooth_to_target_total_ms - ControllerHoverTarget_ms,
  time_to_peak_speed_smooth_ms = time_to_peak_speed_smooth_total_ms - ControllerLeaveTarget_ms,
)

# todo: remove arbitrarily high values in ControllerHoverTarget/LeaveTarget

# Re-calculate values based on the mean speed trajectory.

#peak_speed_index = first(rowid[speed==max(speed,na.rm=T)]), # first() takes care of NAs
#time_to_peak_speed_smooth = sum(time_delta[rowid < peak_speed_index],na.rm=T),
#peak_speed_smooth = max(speed_smooth,na.rm=T),
#peak_speed_smooth_index = first(rowid[speed_smooth==max(speed_smooth,na.rm=T)]), # first() takes care of NAs
#time_to_peak_speed_smooth = sum(time_delta[rowid < peak_speed_smooth_index],na.rm=T),
#peak_speed_smooth_to_target = sum(time_delta[rowid > peak_speed_index],na.rm=T),
#peak_speed_smooth_to_target_pct = (peak_speed_smooth_to_target / duration) * 100,




# recalculate fittsID for laser euc
Sa = Sa %>% rowwise %>% mutate(
  laserR_travel_euc = st_length(st_linestring(data.matrix(data.frame(RightControllerLaserPosWorld_euc))))
)

###
# Remove trajectories - causes issues with joining.
###
traj = c("RightControllerLaserPosWorldL","RightControllerPosWorldL","HeadCameraRotEuler","RightControllerLaserPosWorldR",
         "RightControllerPosWorldR","RightControllerLaserPosWorld","RightControllerPosWorld","RightControllerLaserPosWorld_euc",
         "RightControllerPosWorld_euc","RightControllerLaserPosWorld_norm","RightControllerPosWorld_norm",
         "MolePositionWorld_euc","MolePositionMS_euc","Event")
Sa <- Sa %>% select(-any_of(traj))


# Remove calculations of last hits, as they are vrey long.

# overshooting errors: Overshooting errors, referred to by Mandryk and Lough as exit errors, are errors in which the
# participant exited and re-entered the primary target prior to mouse-down.
#overshoot_error = NA,
# peak speed: Peak speed is simply the maximum speed reached during the primary task
#peak_speed_smooth = NA,
# Time to peak speed is the temporal measure taken to reach peak speed and represents the acceleration phase of the motion
#time_to_peak_speed_smooth = NA
#Percent after peak speed (%afterS max ): Percent after peak speed is the amount of time that occurs after peak speed is reached as a percentage of total
# movement time and represents the deceleration phase of the motion
#percent_after_peak_speed_smooth = NA

condition_order = c("OperationTime","OperationSpeed","OperationDistance","ActionTime","ActionSpeed","ActionDistance","TaskTime","TaskSpeed","TaskDistance")
condition_order_short = c("O-T","O-S","O-D","A-T","A-S","A-D","T-T","T-S","T-D")
names(condition_order_short) <- condition_order
feedback_order = c("Operation","Action","Task")
judgement_order = c("Time","Speed","Distance")

Sa = Sa %>% filter(FeedbackJudge != "NoneNone") %>%  mutate(
  straightness = travel_arm_euc / travel_arm,
  straightness_pct = straightness * 100,
  FeedbackJudge.f = factor(FeedbackJudge,levels=condition_order),
  FeedbackJudge.fs = FeedbackJudge,
  FeedbackJudge.fs = str_replace_all(FeedbackJudge.fs, condition_order_short),
  FeedbackJudge.fs = factor(FeedbackJudge.fs,levels=unname(condition_order_short)),
  time_to_peak_speed_smooth_ms = time_to_peak_speed_smooth * 1000,
  ControllerHoverTarget_ms = ControllerHoverTarget_ms * 1000,
  ControllerLeaveTarget_ms = ControllerLeaveTarget_ms * 1000,
  peak_speed_smooth_to_target_ms = (peak_speed_smooth_to_target * 1000) - ControllerHoverTarget_ms,
  PerformanceFeedback.f = factor(PerformanceFeedback,levels=feedback_order, ordered=T),
  JudgementType.f = factor(JudgementType,levels=judgement_order),
  )

#Sd <- Sd %>% select(distance,distanceMS, Participant,SessionProgram,MiniPatternLabel,PatternSegmentLabel)

#Sal <- Sa %>% select(Participant, HitOrder, FeedbackJudge,duration,
#                      travel_arm,travel_arm_euc,time_to_peak_speed_smooth,peak_speed_smooth,
#                      peak_speed_smooth_to_target_pct,JudgementType,PerformanceFeedback)

# Combine with Experiential Measures (Lf) and base pattern measures (Sd)

####
# Merge Sd and Sa
####
Sa = Sa %>% left_join(Sd, by=c("Participant" = "Participant", "HitOrder"))
Sa = Sa %>% rename("MoleIdStart" = "MoleIdStart.x", "MoleIdToHit" = "MoleIdToHit.x")

#Sa <- Sa %>% left_join(Sd, by=c("Participant" = "Participant", "FeedbackJudge" = "PatternSegmentLabel"))
Sa <- Sa %>% left_join(Lf)

# fittsID for arm euc
Sa = Sa %>% mutate(
  fittsID_arm = log2(travel_arm_euc / MoleSize + 1)
)

# fittsID for laser euc
Sa = Sa %>% mutate(
  fittsID_laser = log2(laserR_travel_euc/ MoleSize + 1)
)


# Create variable to keep track of order
Sa <- Sa %>% group_by(Participant, FeedbackJudge) %>% mutate(
  Orderflag = row_number(),
  Orderflag = ifelse(Orderflag < 2, Orderflag, 0)
) %>% ungroup() %>% group_by(Participant) %>% mutate(
  PlayOrder = cumsum(Orderflag),
  PlayOrder = as.factor(PlayOrder)
)

# Create variable to keep track of judge order
Sa <- Sa %>% group_by(Participant, JudgementType) %>% mutate(
  Orderflag = row_number(),
  Orderflag = ifelse(Orderflag < 2, Orderflag, 0)
) %>% ungroup() %>% group_by(Participant) %>% mutate(
  JudgeOrder = cumsum(Orderflag),
  JudgeOrder = factor(PlayOrder, levels=c(1:9),ordered=T)
)

Sa = Sa %>% ungroup() %>% mutate(
  JudgementOrder = str_remove(SessionProgram, "D3-3-2 Performance Feedback - "),
  fittsID.f = as.factor(fittsID),
  Participant.f = as.factor(Participant),
  throughput = fittsID / duration,
  Gender.f = as.factor(Gender),
  Age.f = factor(Age, ordered=T),
  VRExperience.f = as.factor(VRExperience),
  GameExperience.f = as.factor(GameExperience),
  PlayedBefore.f = as.factor(PlayedBefore),
  VRSickness.f = as.factor(VRSickness),
  PredictedPattern.f = as.factor(PredictedPattern),
  Glasses.f = as.character(Glasses),
  Glasses.f = case_when(Glasses.f == "FALSE" ~  "TookThemOff",
                        Glasses.f == "TRUE" ~  "WornWithinHeadset",
                        is.na(Glasses.f) ~ "DidNotUse"),
  Glasses.f = as.factor(Glasses.f)
)

# Create a variable check for ordering effects.
Sa = Sa %>% ungroup() %>% group_by(Participant) %>%
  mutate(ActionOrder = HitOrder-min(HitOrder))

# Create variables we can use to check for learning effects, within each feedback.
Sa = Sa %>% ungroup() %>% group_by(Participant, PerformanceFeedback) %>%
  mutate(PlayOrderFB = as.numeric(PlayOrder),
         PlayOrderFB = PlayOrderFB - min(PlayOrderFB),
         ActionOrderFB = ActionOrder-min(ActionOrder))

###
# Remove First Five Actions Where Feedback System Gets Calibrated
###
# 2025.09.03: We DONT do this as we dont have time re-run the analysis and there is little gain.
#hitnumber, from mole 1 to mole 20
#Sa = Sa %>% group_by(Participant.f, FeedbackJudge.f) %>% 
#  mutate(counter = 1,
#         hitnumber = cumsum(counter)) %>%
#  filter(hitnumber > 5)
  
  

# Scfm: Summary of metric per feedback condition
Scfm <- Sa %>% group_by(FeedbackJudge) %>% 
  summarize(
    `Correspondence (1-7)` = mean(as.numeric(AlgoCorrespondFastSlow.f)),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Action Duration (ms)`= mean(duration_ms),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Fitts ID` = mean(fittsID),
    `Throughput (bits/s)` = mean(throughput),
)

Scfm_sd <- Sa %>% group_by(FeedbackJudge) %>% 
  summarize(
    `Correspondence (1-7)` = sd(as.numeric(AlgoCorrespondFastSlow.f)),
    `Action Arm Travel (meter)` = sd(travel_arm),
    `Action Duration (ms)`= sd(duration_ms),
    `Straightness (0-1)` = sd(straightness),
    `Peak Speed (m/s)` = sd(peak_speed_smooth),
    `Time to Peak Speed (ms)` = sd(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = sd(peak_speed_smooth_to_target_pct),
    `Fitts ID` = sd(fittsID),
    `Throughput (bits/s)` = sd(throughput),
  )

Scmm <- Sa %>% group_by(JudgementType) %>% 
  summarize(
    `Correspondence (1-7)` = mean(as.numeric(AlgoCorrespondFastSlow.f)),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Action Duration (ms)`= mean(duration_ms),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Fitts ID` = mean(fittsID),
    `Throughput (bits/s)` = mean(throughput),
  )

Scmm_sd <- Sa %>% group_by(JudgementType) %>% 
  summarize(
    `Correspondence (1-7)` = sd(as.numeric(AlgoCorrespondFastSlow.f)),
    `Action Arm Travel (meter)` = sd(travel_arm),
    `Action Duration (ms)`= sd(duration_ms),
    `Straightness (0-1)` = sd(straightness),
    `Peak Speed (m/s)` = sd(peak_speed_smooth),
    `Time to Peak Speed (ms)` = sd(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = sd(peak_speed_smooth_to_target_pct),
    `Fitts ID` = sd(fittsID),
    `Throughput (bits/s)` = sd(throughput),
  )


# Scm: Summary of metric condition


# Scf: Summary of feedback condition
  
  
  # Aggregate how much travel

sum_cols = c("AlgoCorrespondFastSlow.f", "HowMuchFeedback.f", "FeedbackQuality.f", "OverallExperience.f", "FeedbackOverallFeel.f", "FeedbackNotice.f","FeedbackEncourage.f","FeedbackAssessPerf.f","FeedbackDistract.f","FeedbackSenseDiff.f")
sum_cols_sa = c("travel", "duration","travel_head")

# Summarize means for conditions
#Lf %>% group_by(Condition) %>% select(all_of(sum_cols)) %>%
#summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>% view()

#Sa %>% group_by(PerformanceFeedback) %>% select(all_of(sum_cols_sa)) %>%
#  summarise(across(everything(), ~ sum(., na.rm = TRUE))) %>% view()


# Sc: Summary of Condition
Sc = Sa %>% group_by(Participant, FeedbackJudge.f) %>%
  dplyr::summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (ms)` = mean(duration_ms),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Correspondence` = unique(AlgoCorrespondFastSlow.f),
     MiniPatternLabel = paste(unique(MiniPatternLabel),collapse=", "),
    `Performance Feedback` = unique(PerformanceFeedback.f),
    `Performance Metric` = unique(JudgementType),
    `Throughput (bits/s)` = mean(throughput),
    `Fitts ID` = mean(fittsID),
    PlayOrder = as.numeric(unique(PlayOrder)),
    PlayOrderFB = as.numeric(unique(PlayOrderFB))
  ) %>% mutate(
    Participant.f = as.factor(Participant),
  )

Sc = Sc %>% ungroup() %>% 
  mutate(max_speed = max(`Peak Speed (m/s)`),
         max_time = max(`Action Duration (ms)`),
         max_straight = max(`Straightness (0-1)`),
  ) %>% group_by(Participant.f, FeedbackJudge.f) %>%
  mutate(speed_perf = `Peak Speed (m/s)` / max_speed, # this creates a value between 0 and 1 where higher values equal better performance, compared to everyone.
         time_perf = 1 - (`Action Duration (ms)` / max_time),
         straight_perf = `Straightness (0-1)` / max_straight)  %>%
  summarise(speed_perf = mean(speed_perf),
            time_perf = mean(time_perf),
            straight_perf = mean(straight_perf),
            overall_perf = speed_perf + time_perf + straight_perf) %>% right_join(Sc)

fig %>% add_trace(data=Sc %>% group_by(`Performance Feedback`), x=~PlayOrderFB, y=~`Peak Speed (m/s)`, mode='markers',type='scatter') %>%
  add_trace(data=p_lin(Sc, "Peak Speed (m/s)", "PlayOrderFB"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 120, 1)"))
  
fm <- lm(`Action Duration (ms)` ~ PlayOrderFB, data = Sc)
fm <- lm(`Peak Speed (m/s)` ~ PlayOrderFB, data = Sc) # 4%
fm <- lm(`Action Duration (ms)` ~ PlayOrderFB, data = Sc) # -12%
fm <- lm(`Straightness (0-1)` ~ PlayOrderFB, data = Sc) # 0% 

fig %>% add_trace(data=Sc %>% group_by(PlayOrder) %>% summarise(time_perf = mean(time_perf)) , x=~PlayOrder, y=~time_perf, mode='markers',type='scatter')


# Scf: Summary of Condition-Feedback
Scf = Sa %>% group_by(Participant, PerformanceFeedback.f) %>%
  dplyr::summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (ms)` = mean(duration_ms),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Correspondence` = mean(as.numeric(AlgoCorrespondFastSlow.f)),
    `Overall Feel` = unique(FeedbackOverallFeel.f),
    `Distraction` = unique(FeedbackDistract.f),
    `Quality` = unique(FeedbackQuality.f),
    `Quantity` = unique(FeedbackQuantity.f),
    `Sensing Algorithm` = unique(FeedbackSenseDiff.f),
    `Assess Performance` = unique(FeedbackAssessPerf.f),
    `Notice` = unique(FeedbackNotice.f),
    `Distraction` = unique(FeedbackDistract.f),
    `Encouragement` = unique(FeedbackEncourage.f),
    MiniPatternLabel = paste(unique(MiniPatternLabel),collapse=", "),
    `Performance Feedback` = unique(PerformanceFeedback.f),
    `Performance Metric` = paste(unique(JudgementType),collapse=", "),
    `Throughput (bits/s)` = mean(throughput),
  ) %>% mutate(
    Participant.f = as.factor(Participant),
    `Performance Feedback` = as.factor(`Performance Feedback`)
  )

# Scm: Summary of Condition-Metric
Scm = Sa %>% group_by(Participant, JudgementType.f) %>%
  dplyr::summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (ms)` = mean(duration_ms),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Correspondence` = mean(as.numeric(AlgoCorrespondFastSlow.f)),
    `Performance Metric` = paste(unique(JudgementType),collapse=", "),
    `Throughput (bits/s)` = mean(throughput),
    JudgementOrder = paste(unique(str_remove(SessionProgram, "D3-3-2 Performance Feedback - "))),
  ) %>% mutate(
    Participant.f = as.factor(Participant),
    `Performance Metric` = as.factor(`Performance Metric`),
    JudgementOrder = as.numeric(as.factor(JudgementOrder)),
  ) 

Scm = Scm %>% ungroup() %>% 
  mutate(max_speed = max(`Peak Speed (m/s)`),
         max_time = max(`Action Duration (ms)`),
         max_straight = max(`Straightness (0-1)`),
  ) %>% group_by(Participant.f) %>%
  mutate(speed_perf = `Peak Speed (m/s)` / max_speed, # this creates a value between 0 and 1 where higher values equal better performance, compared to everyone.
         time_perf = 1 - (`Action Duration (ms)` / max_time),
         straight_perf = `Straightness (0-1)` / max_straight)  %>%
  summarise(speed_perf = mean(speed_perf),
            time_perf = mean(time_perf),
            straight_perf = mean(straight_perf),
            overall_perf = speed_perf + time_perf + straight_perf) %>%
  arrange(-overall_perf) %>% select(Participant.f, overall_perf, straight_perf, time_perf, speed_perf) %>% ungroup() %>%
  rownames_to_column("Place") %>% mutate(Place = as.numeric(Place)) %>% right_join(Scm)




Scm = Scm %>% group_by(Participant.f) %>% filter(`Performance Metric` == "Time") %>%
  summarise(duration_timeperf = mean(`Action Duration (ms)`),unique(Participant.f)) %>% right_join(Scm)

Scm = Scm %>% ungroup() %>% filter(`Performance Metric` %in% c("Speed","Distance")) %>% group_by(Participant.f) %>%
  summarise(duration_otherperf = mean(`Action Duration (ms)`),unique(Participant.f)) %>% right_join(Scm)

Scm = Scm %>% group_by(Participant.f) %>% filter(`Performance Metric` == "Speed") %>%
  summarise(speed_speedperf = mean(`Peak Speed (m/s)`),unique(Participant.f)) %>% right_join(Scm)

Scm = Scm %>% ungroup() %>% filter(`Performance Metric` %in% c("Time","Distance")) %>% group_by(Participant.f) %>%
  summarise(speed_otherperf = mean(`Peak Speed (m/s)`),unique(Participant.f)) %>% right_join(Scm)

Scm = Scm %>% group_by(Participant.f) %>% filter(`Performance Metric` == "Distance") %>%
  summarise(distance_distanceperf = mean(`Straightness (0-1)`),unique(Participant.f)) %>% right_join(Scm)

Scm = Scm %>% ungroup() %>% filter(`Performance Metric` %in% c("Time","Speed")) %>% group_by(Participant.f) %>%
  summarise(distance_otherperf = mean(`Straightness (0-1)`),unique(Participant.f)) %>% right_join(Scm)

Scm %>% group_by(Participant.f) %>% dplyr::slice(1) %>% group_by(JudgementType.f) %>% summarise(distperf = mean(distance_distanceperf), otherdist = mean(distance_otherperf), diff = distperf - otherdist)
Scm %>% group_by(Participant.f) %>% dplyr::slice(1) %>% group_by(JudgementType.f) %>% mutate(diff = distance_distanceperf - distance_otherperf) %>% summarise(sd(diff))

Scm %>% group_by(Participant.f) %>% dplyr::slice(1) %>% group_by(JudgementType.f) %>% summarise(perf = mean(speed_speedperf), other = mean(speed_otherperf), diff = perf - other)
Scm %>% group_by(Participant.f) %>% dplyr::slice(1) %>% group_by(JudgementType.f) %>% mutate(diff = speed_speedperf - speed_otherperf) %>% summarise(sd(diff))

Scm = Scm %>% ungroup() %>% mutate(speed_adaption = speed_speedperf / speed_otherperf,
               time_adaption = duration_timeperf / duration_otherperf,
               distance_adaption = distance_distanceperf / distance_otherperf,
               best_adaption = (speed_adaption + time_adaption + distance_adaption) / 3) %>%
  arrange(-best_adaption) %>% select(Participant.f, best_adaption, time_adaption, speed_adaption, distance_adaption) %>% group_by(Participant.f) %>% 
  summarise(across(everything(), ~ unique(.))) %>%
  rownames_to_column("AdaptionRank") %>% mutate(AdaptionRank = as.numeric(AdaptionRank)) %>% right_join(Scm)



# Sp: Summary of Participants

Sp = Sa %>% group_by(Participant) %>%
  summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (ms)` = mean(duration_ms),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Fitts ID` = mean(fittsID),
    `Preferred` = unique(PreferedFeedback)
  )

# Spf: Summary of Participants in the feedback space.
Spf = Sa %>% group_by(Participant, PerformanceFeedback.f) %>%
  summarise(
    `Correspondence` = mean(as.numeric(AlgoCorrespondFastSlow.f)),
    `Overall Feel` = unique(FeedbackOverallFeel.f),
    `Distraction` = unique(FeedbackDistract.f),
    `Quality` = unique(FeedbackQuality.f),
    `Quantity` = unique(FeedbackQuantity.f),
    `Sensing Algorithm` = unique(FeedbackSenseDiff.f),
    `Assess Performance` = unique(FeedbackAssessPerf.f),
    `Notice` = unique(FeedbackNotice.f),
    `Distraction` = unique(FeedbackDistract.f),
    `Encouragement` = unique(FeedbackEncourage.f),
  )

Sf = Sa %>% group_by(Participant, FeedbackJudge) %>%
  summarise(
    `Actions (n)` = max(HitOrder) - min(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (ms)` = mean(duration_ms),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Fitts ID` = mean(fittsID),
    `Feedback` = unique(PerformanceFeedback),
    `JudgementType` = unique(JudgementType),
  ) %>% filter(FeedbackJudge != "NoneNone")

# Sp: Summary of Participant standard deviation
Sp_sd = Sa %>% group_by(Participant) %>%
  summarise(
    `Actions (n)` = NA,
    `Actions Analyzed (n)` = NA,
    `Action Duration (ms)` = sd(duration_ms),
    `Action Arm Travel (meter)` = sd(travel_arm),
    `Total Arm Travel (meter)` = NA,
    `Euc. Arm Travel (meter)` = NA,
    `Straightness (0-1)` = sd(straightness),
    `Peak Speed (m/s)` = sd(peak_speed_smooth),
    `Time to Peak Speed (ms)` = sd(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = sd(peak_speed_smooth_to_target_pct),
    `Fitts ID` = sd(fittsID),
  )

#Spl: Summary of Playorder
Spl = Sa %>% group_by(Participant,PlayOrder) %>% 
  summarise(`Throughput` = mean(throughput),
            `Action Duration (ms)` = mean(duration_ms),
            `Action Arm Travel (meter)` = mean(travel_arm),
            `Straightness (0-1)` = mean(straightness),
            `Peak Speed (m/s)` = mean(peak_speed_smooth),
            `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
            `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct))

# ,
#PlayOrder = as.numeric(unique(PlayOrder))) %>% ungroup() %>%
#  group_by(Participant) %>%
#  mutate(across(any_of(matches("^(?!PlayOrder$).*",perl=T)), ~ .x-.x[PlayOrder==1]))

#Spl %>% group_by(Participant) %>%
#  summarise(across(any_of(matches("^(?!PlayOrder$).*",perl=T)), ~ predict(lm(.x ~ PlayOrder, data=Spl)))) %>%
#  rename_with(~ paste0(.x,"_reg"), matches("^(?!PlayOrder$).*",perl=T)) %>%
#  rename("Participant" = "Participant_reg")

###
# Calculating Performance cost/proficiency
###

non_straight = Sc %>% group_by(`Performance Feedback`) %>% filter(`Performance Metric` %in% c("Speed","Time")) %>%
  summarise(nondistance_straightness = mean(`Straightness (0-1)`))

straight = Sc %>% group_by(`Performance Feedback`) %>% filter(`Performance Metric` %in% c("Distance")) %>%
  summarise(distance_straightness = mean(`Straightness (0-1)`))

straight %>% left_join(non_straight) %>%
  mutate(increase = (distance_straightness - nondistance_straightness))

non_speed = Sc %>% group_by(`Performance Feedback`) %>% filter(`Performance Metric` %in% c("Distance","Time")) %>%
  summarise(non_speed = mean(`Peak Speed (m/s)`))

speed = Sc %>% group_by(`Performance Feedback`) %>% filter(`Performance Metric` %in% c("Speed")) %>%
  summarise(speed = mean(`Peak Speed (m/s)`))

speed %>% left_join(non_speed) %>%
  mutate(increase = (non_speed - speed),
         percentage = (speed / non_speed)-1)

non_time = Sc %>% group_by(`Performance Feedback`) %>% filter(`Performance Metric` %in% c("Distance","Speed")) %>%
  summarise(non_time = mean(`Action Duration (ms)`))

time = Sc %>% group_by(`Performance Feedback`) %>% filter(`Performance Metric` %in% c("Time")) %>%
  summarise(time = mean(`Action Duration (ms)`))

time %>% left_join(non_time) %>%
  mutate(increase = (non_time - time),
         percentage = (non_time / time)-1 )


non_speed = Sc %>% ungroup() %>% filter(`Performance Metric` %in% c("Distance")) %>%
  summarise(non_speed = mean(`Peak Speed (m/s)`))

speed = Sc %>% ungroup() %>% filter(`Performance Metric` %in% c("Speed")) %>%
  summarise(speed = mean(`Peak Speed (m/s)`))

speed %>% bind_cols(non_speed) %>%
  mutate(increase = (speed - non_speed),
         percentage = (speed / non_speed)-1)


Sc = Sc %>% ungroup() %>% 
 mutate(max_speed = max(`Peak Speed (m/s)`),
        max_time = max(`Action Duration (ms)`),
        max_straight = max(`Straightness (0-1)`),
 ) %>% group_by(Participant.f) %>%
 mutate(speed_perf = `Peak Speed (m/s)` / max_speed,
        time_perf = 1 - (`Action Duration (ms)` / max_time),
        straight_perf = `Straightness (0-1)` / max_straight)  %>%
 summarise(speed_perf = mean(speed_perf),
           time_perf = mean(time_perf),
           straight_perf = mean(straight_perf),
           overall_perf = speed_perf + time_perf + straight_perf) %>%
 arrange(-overall_perf) %>% select(Participant.f, overall_perf) %>% ungroup() %>%
 rownames_to_column("Place") %>% mutate(Place = as.numeric(Place)) %>% right_join(Sc)


non_speed = Sc %>% group_by(Place, `Performance Metric`) %>% filter(`Performance Metric` %in% c("Time","Distance")) %>%
  summarise(non_speed = mean(`Peak Speed (m/s)`)) %>%
  pivot_wider(names_from = `Performance Metric`, values_from = non_speed)

speed = Sc %>% group_by(Place, `Performance Metric`) %>% filter(`Performance Metric` %in% c("Speed")) %>%
  summarise(speed = mean(`Peak Speed (m/s)`))

Speed_Pct = speed %>% left_join(non_speed) %>%
  mutate(increase_time = (speed - Time),
         percentage_time = ((speed / Time)-1) * 100,
         increase_distance = (speed - Distance),
         percentage_distance = ((speed / Distance)-1) * 100,
         increase_all = speed - ((Time+Distance)/2),
         ) %>% arrange(Place)

Speed_Pct = Speed_Pct %>% select(PID = Place, `Peak Speed` = speed, Distance, percentage_distance, Time, percentage_time) %>%
  mutate(
    across(everything(), ~ format(round(.x,2),nsmall=2)),
    percentage_distance = paste0(percentage_distance,"\\%"),
    percentage_time = paste0(percentage_time,"\\%"),
    across(everything(), ~ as.character(.x)),
  ) %>% arrange(PID) #pivot_longer(cols=-c(PID), names_to = "Variables") %>% 
  #pivot_wider(names_from = `PID`, values_from = value)

paste(colnames(Speed_Pct), collapse=" & ")
writeLines(paste(Speed_Pct %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ \n"), "table.txt")

pivot_longer(cols=-c(Condition), names_to = "Variables") %>%
  pivot_wider(names_from = Condition, values_from = value)

# Group-level
non_speed = Sc %>% group_by(`Performance Metric`) %>% filter(`Performance Metric` %in% c("Time","Distance")) %>%
  summarise(non_speed = mean(`Peak Speed (m/s)`)) %>%
  pivot_wider(names_from = `Performance Metric`, values_from = non_speed)

speed = Sc %>% group_by(`Performance Metric`) %>% filter(`Performance Metric` %in% c("Speed")) %>%
  summarise(speed = mean(`Peak Speed (m/s)`))

Speed_Pct = speed %>% bind_cols(non_speed) %>%
  mutate(increase_time = (speed - Time),
         percentage_time = ((speed / Time)-1) * 100,
         increase_distance = (speed - Distance),
         percentage_distance = ((speed / Distance)-1) * 100,
         increase_all = speed - ((Time+Distance)/2),
  )
Speed_Pct = Speed_Pct %>% select(`Peak Speed` = speed, Distance, percentage_distance, Time, percentage_time) %>%
  mutate(
    across(everything(), ~ round(.x,2)),
    percentage_distance = paste0(percentage_distance,"\\%"),
    percentage_time = paste0(percentage_time,"\\%"),
  ) 

#%>%
#  summarise(max_pct = max(percentage),
#            min_pct = min(percentage))

Spd_long = Speed_Pct %>% mutate(Distance_norm = scales::rescale(Distance, from=c(min(Distance),max(Distance)))),
                                
                                (max(Distance) - min(Distance))  Avg = Distance+Time+`Peak Speed` )
%>% select(c(-percentage_time, -percentage_distance)) %>% pivot_longer(cols=-c(PID), names_to = "Variables") 

fig %>% add_trace(data=Spd_long, 
              x=~value, y=~Variables, type='scatter',mode='markers+text',
              mode='markers+text', marker=list(size=20, color=I('black')), hoverinfo="text", text=~PID, color=I('white'))
fig %>%
  add_trace(data=Spc, x=~OrderTotal, y=~acc_rate, color=~Participant, opacity=.6,
            type='scattergl', mode='markers+text', marker=list(size=20), hoverinfo="text", text=paste(Spc$Participant, Spc$Condition)) %>%
  add_trace(name="Frust. (1-7)", x=jitter(Spc$OrderTotal,amount=0.05), y=jitter(Spc$FrustNormalized,amount=0.02), color=I('blue'), opacity=.6,
            type='scattergl', mode='markers+lines') %>%
  layout(yaxis=list(range=c(-0.1,1.1)))




###
# Reporting various means in Results
###

# Who had the lowest throughput of all conditions?

Sa %>% group_by(FeedbackJudge.f) %>% summarise(
  throughput_mean = mean(throughput),
  throughput_sd = sd(throughput)
)

# Who received lowest correspondence ratings?
Sa %>% group_by(JudgementType.f) %>% summarise(
  correspondence = mean(as.numeric(AlgoCorrespondFastSlow.f))
)

###
# Bartlett's Spherity test
###

psych::KMO(Spf %>% select(-PerformanceFeedback.f) %>% mutate(across(everything(), ~ as.numeric(.x))))

psych::cortest.bartlett(Spf %>% select(-PerformanceFeedback.f) %>% mutate(across(everything(), ~ as.numeric(.x))))

Spf_num = Spf %>% select(-PerformanceFeedback.f) %>% mutate(across(everything(), ~ as.numeric(.x))) %>% select(-Distraction)

#KMO(Stf %>% select(-Condition.f))$MSAi>0.50

# Bartletts test of spherity indicated that variables were related and suitable for factor analysis.
# KMO indicated that for most variables except distraction, sampling was adequate to perform factor analysis.
# We therefore removed distraction.

psych::fa.parallel(Spf_num, fa="minres")

efa_result <- psych::fa(Spf_num %>% ungroup %>% select(-Participant, -`Overall Feel`), nfactors = 4, rotate = "oblimin", fm="minres")
efa_result
print(efa_result$loadings,cutoff = 0.3)
psych::fa.diagram(efa_result) 

###
# Bartlett's Spherity test
###

###
# Plots of self-report scales
###

plot_violin <- function(dataset, xlabel, ylabel, desc) {
df = data.frame(x = dataset[[xlabel]],
                y = dataset[[ylabel]])
fig_c <- fig %>%
  add_trace(data=df, x=~x, 
            y=~jitter(y,amount=.2),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4,color="rgba(0, 0, 0, 255)"),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.5, color=I("darkgrey")) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                  text=desc), showlegend=F,
         xaxis=list(range=c(-0.45,~length(unique(x)) - 0.45), title=" ", zeroline=F, tickfont=list(size=15)),
         yaxis=list(range=c(-0.52,7.52), title=" ", zeroline=F, dtick=1, tickformat = ".0", tickfont=list(size=15), showticklabels=T))

return(fig_c)
}


fig_c = plot_violin(Lf %>% distinct(Participant,PerformanceFeedback,.keep_all=T), "PerformanceFeedback","FeedbackSenseDiff.f",
            "“With this feedback, I sensed the \n difference between the three algorithms.”")
orca(fig_c, "fig/condition_feedbackSenseDiff_violin.pdf", width=325, height=355)

fig_c = plot_violin(Lf %>% distinct(Participant,PerformanceFeedback,.keep_all=T), "PerformanceFeedback","FeedbackDistract.f",
            "“How much did you feel that the \n feedback distracted you?”")
orca(fig_c, "fig/condition_feedbackDistract_violin.pdf", width=325, height=355)

fig_c = plot_violin(Lf %>% distinct(Participant,PerformanceFeedback,.keep_all=T), "PerformanceFeedback","FeedbackQuality.f",
            "Overall, How good did the feedback feel?")
orca(fig_c, "fig/condition_feedbackQuality_violin.pdf", width=325, height=355)

fig_c = plot_violin(Lf, "FeedbackJudge","AlgoCorrespondFastSlow.f",
            "With this algorithm, the feedback clearly \n corresponded to whether I was fast or slow.")
orca(fig_c, "fig/condition_feedbackAlgoCorrespond_violin.pdf", width=725, height=555)


fig_c <- fig %>%
  add_trace(data=Sa, x=~PerformanceFeedback, 
            y=~jitter(as.numeric(travel),amount=.2),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.5, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),title=list(font=list(size=15),xanchor="center",xref="paper",
                                                  text="“How much did you feel that the \n [blue tail/checkmark/heatmap] feedback distracted you?”"), showlegend=F,
         xaxis=list(range=c(-0.45,2.55), title=" ", zeroline=F, tickfont=list(size=15)),
         yaxis=list(range=c(-0.52,7.52), title=" ", zeroline=F, dtick=1, tickformat = ".0", tickfont=list(size=15), showticklabels=T))
fig_c
orca(fig_c, "fig/condition_feedbackDistract_violin.pdf", width=285, height=325)

###
# Simple mode: Non-parametric tests for likert scale data
###

# Friedman test
tables_friedman <- function(df, measlabel, treatlabel, plabel) {
  dataset = data.frame(measurement = df[[measlabel]],
                  treatment = df[[treatlabel]],
                  participant = df[[plabel]])
result <- friedman.test(measurement ~ treatment | participant, data = dataset)
dataset_measurement = dataset %>% group_by(treatment) %>% summarise(
  `Mean Score` = mean(as.numeric(measurement)),
  `SD` = sd(as.numeric(measurement))
) %>% ungroup() %>% mutate(across(all_of(c("Mean Score", "SD")), ~ format(round(.x,2), nsmall = 2))) %>%
  rename(Condition = treatment) %>% 
  pivot_longer(cols=-c(Condition), names_to = "Variables") %>%
  pivot_wider(names_from = Condition, values_from = value)

s1 = paste(colnames(dataset_measurement), collapse=" & ")
s2 = paste(dataset_measurement %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ ")

dataset_measurement_stat <- data.frame(
  `chisquared` =  result$statistic,
  `df` = result$parameter,
  p = result$p.value
) %>% mutate(across(all_of(c("chisquared","df")), ~ format(round(.x,2), nsmall = 2))) %>%
  mutate(`p-value` = format(round(p,3), nsmall = 3),
         `p-value` = ifelse(`p-value` == "0.000", "$<$0.001", `p-value`),
         `p-value` = ifelse(p < 0.05, paste0(`p-value`,"*"))) %>% select(-p)


s3 = paste(colnames(dataset_measurement_stat), collapse=" & ")
s4 = paste(dataset_measurement_stat %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ ")

# re-arrange distance variable so conover's F shows the right p-values for reporting.
#dataset = dataset %>% mutate(across(everything(), ~ str_replace_all(.x, c("Action" = "UAction", "Operation" = "WOperation")))) %>%
  #arrange(participant, treatment)

# Perform Conover's post-hoc test
conover_result <- frdAllPairsConoverTest(y = dataset$measurement,
                                         groups = dataset$treatment,
                                         blocks = dataset$participant)

#conover_summary = summary(conover_result)

pvalues = as.data.frame(conover_result$p.value) %>%
  mutate(across(everything(), ~ format(round(.x,3), nsmall = 3)),
         across(everything(), ~ ifelse(as.numeric(.x) < 0.05, paste0('\\cellcolor{g2}',.x,"*"),.x)),
         across(everything(), ~ ifelse(.x == "\\cellcolor{g2}0.000*", "\\cellcolor{g2}$<$0.001*", .x)),
         across(everything(), ~ ifelse(is.na(.x), " ",.x))) %>% rownames_to_column()

s5 = paste(colnames(pvalues), collapse=" & ")
writeLines(paste(pvalues %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")

return(list(result, conover_result, s1,s2,s3,s4,s5))
}

#condition-wise: correspondence
tables_friedman(Sc, "Correspondence","FeedbackJudge.f","Participant")
tables_friedman(Scm %>% arrange(Participant, desc(JudgementType.f)), "Correspondence","Performance Metric","Participant")
tables_friedman(Scm, "JudgementOrder","Performance Metric","Participant")

# feedback-wise: distract, ..
tables_friedman(Scf, "Distraction","Performance Feedback","Participant")
tables_friedman(Scf, "Encouragement","Performance Feedback","Participant")
tables_friedman(Scf, "Sensing Algorithm","Performance Feedback","Participant")
tables_friedman(Scf, "Assess Performance","Performance Feedback","Participant")
tables_friedman(Scf, "Notice","Performance Feedback","Participant")
tables_friedman(Scf, "Quality","Performance Feedback","Participant")
tables_friedman(Scf, "Quantity","Performance Feedback","Participant")
tables_friedman(Scf, "Overall Feel","Performance Feedback","Participant")

# Todo: Test Overall Preference

###
# Pre-test filtering
###

# 1) 

###
# Simple mode: Repeated Measures Multivariate Anova
###
# The repeated-measures part does not refer to that we took multiple measures of actions, but instead to that
# we measured participants multiple times throughout the experiment.
# tables_rmanova is unaware of possible differences in FittsID between conditions and unaware of relationships
# between the different dependent variables measured.
# Simple Two-way ANOVA
tables_rmanova <- function(df, plabel, measlabel,treatlabel1, treatlabel2, control1, control2) {
  #browser()
  dataset = data.frame(measurement = df[[measlabel]],
                       treat1 = df[[treatlabel1]],
                       treat2 = df[[treatlabel2]],
                       participant = df[[plabel]])
  
  aov_result <- aov_ez(id="participant", dv="measurement", dataset, within = c("treat2")) #treat1 is performance feedback, but we dont 
  aov_summary = summary(aov_result, effect_size ="partial")
  
  
  aov_table = tibble(
    colnames = names(aov_summary[[4]][1,]),
    colvals = aov_summary[[4]][1,],
    Variable = names(aov_summary[[4]][,1][1])
  )
  
  aov_table = tibble(
    colnames = names(aov_summary[[4]][2,]),
    colvals = aov_summary[[4]][2,],
    Variable = names(aov_summary[[4]][,1][2])
  )  %>% bind_rows(aov_table)
  
  aov_table = tibble(
    colnames = names(aov_summary[[4]][3,]),
    colvals = aov_summary[[4]][3,],
    Variable = names(aov_summary[[4]][,1][3])
  )  %>% bind_rows(aov_table)
  
  aov_table = aov_table %>% mutate(
    colnames = case_when(colnames == "Sum Sq" ~ "SS",
                         colnames == "num Df" ~ "Df",
                         colnames == "F value" ~ "F",
                         colnames == "Pr(>F)" ~ "p")
  ) %>% filter(!is.na(colnames))
  
  aov_table = aov_table %>% pivot_wider(names_from="colnames", values_from="colvals")
  
  aov_table = aov_table %>% mutate(across(all_of(c("SS","F")), ~ format(round(.x,2), nsmall = 2))) %>%
    mutate(`p-value` = format(round(p,3), nsmall = 3),
           `p-value` = ifelse(`p-value` == "0.000", "$<$0.001", `p-value`),
           `p-value` = ifelse(p < 0.05, paste0(`p-value`,"*"),`p-value`)) %>% select(-p)
  
  s1 = paste(colnames(aov_table), collapse=" & ")
  s2 = paste(aov_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\ ")
  
  # Obtain estimated marginal means
  emm <- emmeans(aov_result, ~ treat1:treat2)
  
  # Perform pairwise comparisons with Bonferroni adjustment
  pairwise_comparisons <- pairs(emm, adjust = "bonferroni")
  pairwisedf = summary(pairwise_comparisons)
  pairwisedf = pairwisedf %>% filter(p.value < 0.055)
  
  pairwisedf = pairwisedf %>% rename(Contrast = contrast, Estimate = estimate,
                                     Df = df,`t Ratio` = t.ratio,p = p.value)
  
  pairwisedf = pairwisedf %>% mutate(across(all_of(c("Estimate","SE","t Ratio")), ~ format(round(.x,2), nsmall = 2))) %>%
    mutate(`p-value` = format(round(p,3), nsmall = 3),
           `p-value` = ifelse(`p-value` == "0.000", "$<$0.001", `p-value`),
           `p-value` = ifelse(p < 0.05, paste0(`p-value`,"*"),`p-value`)) %>% select(-p)
  
  s3 = paste(colnames(pairwisedf), collapse=" & ")
  writeLines(paste(pairwisedf %>% arrange(`p-value`) %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")
  return(list(s1,s2,s3))
}

aov_ez(id="Participant.f", dv="straightness_pct", within = c("JudgementType"), data=Sa)

tables_rmanova(Sa, "Participant.f", "duration_ms","PerformanceFeedback", "JudgementType") 
tables_rmanova(Sa, "Participant.f", "travel_arm","PerformanceFeedback", "JudgementType") 
tables_rmanova(Sa, "Participant.f", "peak_speed_smooth","PerformanceFeedback", "JudgementType") 
tables_rmanova(Sa, "Participant.f", "peak_speed_smooth_to_target_pct","PerformanceFeedback", "JudgementType")
tables_rmanova(Sa, "Participant.f", "time_to_peak_speed_smooth_ms","PerformanceFeedback", "JudgementType")
tables_rmanova(Sa, "Participant.f", "straightness_pct","PerformanceFeedback", "JudgementType")
tables_rmanova(Sa, "Participant.f", "fittsID","PerformanceFeedback", "JudgementType")
tables_rmanova(Sa, "Participant.f", "throughput","PerformanceFeedback", "JudgementType")


tables_rmanova_single <- function(df, plabel, measlabel,treatlabel1) {
  #browser()
  dataset = data.frame(measurement = df[[measlabel]],
                       treat1 = df[[treatlabel1]],
                       participant = df[[plabel]])
  
  aov_result <- aov_ez(id="participant", dv="measurement", dataset, within = c("treat1"))
  aov_summary = summary(aov_result)
  
  aov_table = tibble(
    colnames = names(aov_summary[[4]][1,]),
    colvals = aov_summary[[4]][1,],
    Variable = names(aov_summary[[4]][,1][1])
  )
  
  aov_table = tibble(
    colnames = names(aov_summary[[4]][2,]),
    colvals = aov_summary[[4]][2,],
    Variable = names(aov_summary[[4]][,1][2])
  )  %>% bind_rows(aov_table)
  
  #aov_table = tibble(
  #  colnames = names(aov_summary[[4]][3,]),
  #  colvals = aov_summary[[4]][3,],
  #  Variable = names(aov_summary[[4]][,1][3])
  #)  %>% bind_rows(aov_table)
  
  aov_table = aov_table %>% mutate(
    colnames = case_when(colnames == "Sum Sq" ~ "SS",
                         colnames == "num Df" ~ "Df",
                         colnames == "F value" ~ "F",
                         colnames == "Pr(>F)" ~ "p")
  ) %>% filter(!is.na(colnames))
  
  aov_table = aov_table %>% pivot_wider(names_from="colnames", values_from="colvals")
  
  aov_table = aov_table %>% mutate(across(all_of(c("SS","F")), ~ format(round(.x,2), nsmall = 2))) %>%
    mutate(`p-value` = format(round(p,3), nsmall = 3),
           `p-value` = ifelse(`p-value` == "0.000", "$<$0.001", `p-value`),
           `p-value` = ifelse(p < 0.05, paste0(`p-value`,"*"),`p-value`)) %>% select(-p)
  
  s1 = paste(colnames(aov_table), collapse=" & ")
  s2 = paste(aov_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\ ")
  
  # Obtain estimated marginal means
  emm <- emmeans(aov_result, ~ treat1)
  
  # Perform pairwise comparisons with Bonferroni adjustment
  pairwise_comparisons <- pairs(emm, adjust = "bonferroni")
  pairwisedf = summary(pairwise_comparisons)
  pairwisedf = pairwisedf %>% filter(p.value < 0.055)
  
  pairwisedf = pairwisedf %>% rename(Contrast = contrast, Estimate = estimate,
                                     Df = df,`t Ratio` = t.ratio,p = p.value)
  
  pairwisedf = pairwisedf %>% mutate(across(all_of(c("Estimate","SE","t Ratio")), ~ format(round(.x,2), nsmall = 2))) %>%
    mutate(`p-value` = format(round(p,3), nsmall = 3),
           `p-value` = ifelse(`p-value` == "0.000", "$<$0.001", `p-value`),
           `p-value` = ifelse(p < 0.05, paste0(`p-value`,"*"),`p-value`)) %>% select(-p)
  
  s3 = paste(colnames(pairwisedf), collapse=" & ")
  writeLines(paste(pairwisedf %>% arrange(`p-value`) %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")
  return(list(s1,s2,s3))
}

tables_rmanova_single(Scm, "Participant.f", "Action Duration (ms)","JudgementType.f") 

tables_rmanova_single(Scm, "Participant.f", "Action Arm Travel (meter)","JudgementType.f") 
tables_rmanova_single(Scm, "Participant.f", "Peak Speed (m/s)","JudgementType.f") 
tables_rmanova_single(Scm, "Participant.f", "Peak Speed to Target (\\%)","JudgementType.f")
tables_rmanova_single(Scm, "Participant.f", "Time to Peak Speed (ms)","JudgementType.f")
tables_rmanova_single(Scm, "Participant.f", "Straightness (0-1)","JudgementType.f")
tables_rmanova_single(Scm, "Participant.f", "Throughput (bits/s)","JudgementType.f")
tables_rmanova_single(Scm, "Participant.f", "Correspondence","JudgementType.f")

tables_rmanova_single(Scf, "Participant.f", "Action Duration (ms)","PerformanceFeedback.f") 
tables_rmanova_single(Scf, "Participant.f", "Action Arm Travel (meter)","PerformanceFeedback.f") 
tables_rmanova_single(Scf, "Participant.f", "Peak Speed (m/s)","PerformanceFeedback.f") 
tables_rmanova_single(Scf, "Participant.f", "Peak Speed to Target (\\%)","PerformanceFeedback.f")
tables_rmanova_single(Scf, "Participant.f", "Time to Peak Speed (ms)","PerformanceFeedback.f")
tables_rmanova_single(Scf, "Participant.f", "Straightness (0-1)","PerformanceFeedback.f")
tables_rmanova_single(Scf, "Participant.f", "Throughput (bits/s)","PerformanceFeedback.f")
tables_rmanova_single(Scf, "Participant.f", "Correspondence","PerformanceFeedback.f")

tables_rmanova_single(Sc, "Participant.f", "Throughput (bits/s)","FeedbackJudge.f")

fit <- multRM(cbind(travel_arm, duration_ms, peak_speed_smooth, peak_speed_smooth_to_target_pct, time_to_peak_speed_smooth_ms) ~ PerformanceFeedback.f * JudgementType.f *  order, data = Sa_test,
              subject = "Participant.f", within = c("PerformanceFeedback.f","JudgementType.f","order"))
summary(fit)



Sa_test = Sa %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f,fittsID.f) %>% 
  summarise(across(all_of(c("travel_arm", "duration_ms", "peak_speed_smooth", "peak_speed_smooth_to_target_pct", "time_to_peak_speed_smooth_ms")), ~ mean(.x)))


Sa_test = Sa %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f) %>% dplyr::slice(1:min(19, n()))
Sa_test = Sa_test %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f) %>% mutate(order = 1, order = cumsum(order))

fittsIDs = unique(round(Sa$fittsID,2))

missingIDs = Sa %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f) %>% 
  summarise(n(), missingIDs = paste0(setdiff(fittsIDs, round(fittsID,2)))) %>% 
  ungroup() %>% summarise(unique(missingIDs))

Sa_test = Sa %>% filter(round(fittsID,2) %in% missingIDs)



tables_rmanova(Sc,"Participant", "Action Arm Travel (meter)", "FeedbackJudge")

tables_rmanova(Sc,"Participant", "Action Arm Travel (meter)", Sc, within = "FeedbackJudge")

manova_result <- manova(cbind(travel_arm, duration_ms, peak_speed_smooth, peak_speed_smooth_to_target_pct, time_to_peak_speed_smooth_ms) ~ PerformanceFeedback.f + JudgementType.f + Error(PerformanceFeedback.f/JudgementType.f/fittsID), data = Sa %>% filter(fittsID.f != 0))
summary(manova_result)

Sa = Sa %>% filter(fittsID != 0)

manova_result <- manova(cbind(`Action Arm Travel (meter)`, `Action Duration (ms)`, `Peak Speed (m/s)`) ~ 
                          as.factor(Feedback) + as.factor(JudgementType) + 
                          Error(as.factor(Feedback)/as.factor(JudgementType)/`Fitts ID`), 
                        data = Sf %>% filter(`Fitts ID` != 0))

anova_travel_arm <- aov(peak_speed_smooth ~ PerformanceFeedback.f + JudgementType.f + Error(PerformanceFeedback.f/JudgementType.f/fittsID), data = Sa %>% filter(fittsID.f != 0) %>% as.data.frame(.))

posthoc_travel_arm <- emmeans(anova_travel_arm, ~ PerformanceFeedback.f + JudgementType.f)
pairwise_travel_arm <- pairs(posthoc_travel_arm)

pairwise_travel_arm <- pairs(posthoc_t)

summary(anova_travel_arm)

# Sa %>% select(travel_arm, duration_ms, peak_speed_smooth, peak_speed_smooth_to_target_pct, time_to_peak_speed_smooth_ms, PerformanceFeedback.f,fittsID,JudgementType) %>% view()

independent_var <- Sa$FeedbackJudge.f

manova_model <- manova(dependent_vars ~ PerformanceFeedback.f * JudgementType.f, data = Sa)
summary(manova_model)



manova_model <- lm(cbind(travel_arm, duration_ms, peak_speed_smooth, straightness_pct, peak_speed_smooth_to_target_pct, time_to_peak_speed_smooth_ms) ~ FeedbackJudge , data = Sa %>% filter(fittsID.f ))
within <- data.frame(FeedbackJudge = Sa$FeedbackJudge)
manova_result <- car::Anova(manova_model, idata = within, idesign = ~FeedbackJudge, type = "III")





aov_result = summary(aov_result)[[4]]

dataset_measurement = dataset %>% group_by(treatment) %>% summarise(
  `Mean Score` = mean(as.numeric(measurement)),
  `SD` = sd(as.numeric(measurement))
) %>% ungroup() %>% mutate(across(all_of(c("Mean Score", "SD")), ~ format(round(.x,2), nsmall = 2))) %>%
  rename(Condition = treatment) %>% 
  pivot_longer(cols=-c(Condition), names_to = "Variables") %>%
  pivot_wider(names_from = Condition, values_from = value)

aov_result <- aov_ez("Participant", "Action Arm Travel (meter)", Sc, within = "FeedbackJudge")

manova_result <- manova(cbind(DV1, DV2, DV3) ~ Time + Error(Participant/Time), data = data)


# Perform pairwise comparisons with Bonferroni adjustment
pairwise_comparisons <- pairs(emm, adjust = "bonferroni")
pairwisedf = summary(pairwise_comparisons)
writeLines(paste(pairwisedf %>% arrange(p.value) %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")
###
# Linear Mixed Models (LMM): Check significant differences
###

# Variables affecting our model: Pattern used (random), Participant (random), distance (for MT/speed/etc.), time (for travel_length)

# OK, so after trying manova.. i believe linear mixed model is still the best tool we have available/ready at hand.

# Null model: Expecting variance from participant and from pattern
m.p = lmer(data=Sa, duration_ms ~ (1|Participant.f))
m.pf = lmer(data=Sa, duration_ms ~ fittsID + (1|Participant))
(anova(m.p,m.pf))
m.pff = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + (1|Participant))
(anova(m.pf,m.pff))
m.pfo = lmer(data=Sa, duration_ms ~ fittsID + PlayOrder + (1|Participant))
(anova(m.pf,m.pfo))
(anova(m.pff,m.pfo))
m.pfj = lmer(data=Sa, duration_ms ~ fittsID + JudgementType.f + (1|Participant))
(anova(m.pf,m.pfj))
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
(anova(m.pf,m.pffj))
m.pffjo = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + PlayOrder + (1|Participant))
(anova(m.pffj,m.pffjo))
summary(m.pffjo)
m.pffj.summary <- summary(m.pffj)
m.pffj.summary

# todo include straightness
#travel_arm, duration_ms, peak_speed_smooth, peak_speed_smooth_to_target_pct, time_to_peak_speed_smooth_ms, PerformanceFeedback.f,fittsID,JudgementType
lmes = list(predictors = c("travel_arm", "duration_ms", "peak_speed_smooth","peak_speed_smooth_to_target","time_to_peak_speed_smooth_ms"),
            random = c("GameExperience.f","Gender.f","Age.f","VRExperience.f","PlayedBefore.f","VRSickness.f","PredictedPattern.f","Glasses.f"),
            fixed = c("fittsID.f","JudgementType.f", "PerformanceFeedback.f","ActionOrder"),
            null = c("Participant.f"),
            threshold = 0.05,
            df = Sa)
# , 
table = g_lme_table(lmes)

# Gender p = 0.1856, chisquared = 1.7521
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
m.pffjg = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Gender.f) + (1|Participant))
(anova(m.pffj,m.pffjg))

# Age p = 0.656, chisquared = 7.7218
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
m.pffja = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Age.f) + (1|Participant))
(anova(m.pffj,m.pffja))

# VRExperience.f p = 0.4155, chisquared = 0.663
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
m.pffjv = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|VRExperience.f) + (1|Participant))
(anova(m.pffj,m.pffjv))

# GameExperience.f p = 0.0759, chisquared = 3.1506
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
m.pffjp = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|GameExperience.f) + (1|Participant))
(anova(m.pffj,m.pffjp))

# PlayedBefore.f p=0.6237, chisquared=0.9441
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
m.pffjp = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|PlayedBefore.f) + (1|Participant))
(anova(m.pffj,m.pffjp))

# PredictedPattern.f p=0.7925, chisquared=0.0692
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
m.pffjp = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + PredictedPattern.f + (1|Participant))
(anova(m.pffj,m.pffjp))

# Glasses.f p=0.2568, chisquared=2.7187
m.pffj = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + (1|Participant))
m.pffjg = lmer(data=Sa, duration_ms ~ fittsID + PerformanceFeedback.f + JudgementType.f + Glasses.f + (1|Participant))
(anova(m.pffj,m.pffjg))

test_learning <- function(dataset, basemodel="fittsID + PerformanceFeedback.f + JudgementType.f",testterm="ActionOrderFB", responselabel) {
  #browser()
  m.b = lmer(data=dataset, as.formula(paste(responselabel, "~", basemodel,"+", "(1|Participant)")))
  m.t = lmer(data=dataset, as.formula(paste(responselabel, "~", basemodel,"+", testterm,"+","(1|Participant)")))
  
  #randomeffectsize = as.data.frame(VarCorr(m.t))
  # Calculate the square root of the total variance of random effects
  #sqrt_total_variance_random_effects <- sqrt(randomeffectsize$vcov)
    # Calculate the standardized effect size
  #fixed_effects <- fixef(m.t)
  #  latex_table = tibble(rowname = names(fixed_effects),
  #                      values = fixed_effects)
  #  latex_table %>% mutate(values = values / sqrt_total_variance_random_effects)
  #)
  #standardized_effect_size <- fixed_effect_estimate / response_sd
  #confint(m.t, parm = "beta_", method = "profile")
  
  s1 = anova(m.b,m.t)
  s3 = anova(m.t, type='III')
  s4 = VarCorr(m.t)
  m.t.summary <- summary(m.t)
  fixedtable = as.data.frame(m.t.summary$coefficients) %>% 
    rownames_to_column("Fixed Effect")
  s2 = fixedtable
  
  s3 = rownames_to_column(s3)
  
  latex_table = s3 %>% select(rowname, `F value`, `Pr(>F)`) %>%
    mutate(pvalue = case_when(`Pr(>F)` < 0.001 ~ '***',
                              `Pr(>F)` < 0.01 ~ '**\\hphantom{*}',
                              `Pr(>F)` < 0.05 ~ '*\\hphantom{**}',
                              TRUE ~ '\\hphantom{***}'),
           pcolor = case_when(`Pr(>F)` < 0.001 ~ 'g3',
                              `Pr(>F)` < 0.01 ~ 'g4',
                              `Pr(>F)` < 0.05 ~ 'g5',
                              TRUE ~ 'g6'),
           values = paste0("\\cellcolor{",pcolor, "} ", format(round(`F value`,2), nsmall = 2), pvalue)) %>% 
    select(rowname, values) %>%
    pivot_wider(names_from=rowname, values_from = values) %>%
    mutate(`Response Variable` = responselabel,
           `Response Variable` = names(var_names)[var_names == responselabel]) %>%
    relocate(`Response Variable`)
    
    names(latex_table) = str_replace_all(names(latex_table), setNames(names(var_names),var_names))
    
#    latex_table = latex_table %>% mutate(`Participant (SD)` = sqrt(s4$Participant["(Intercept)", "(Intercept)"]),
#                           `Participant (SD)` = format(round(`Participant (SD)`,2), nsmall = 2))

  return(latex_table)
}

table = tibble() %>% 
  bind_rows(test_learning(Sa,responselabel="peak_speed_smooth",)) %>%
  bind_rows(test_learning(Sa,responselabel="travel_arm")) %>%
  bind_rows(test_learning(Sa,responselabel="duration_ms")) %>%
  bind_rows(test_learning(Sa,responselabel="peak_speed_smooth_to_target_pct")) %>%
  bind_rows(test_learning(Sa,responselabel="throughput")) %>%
  bind_rows(test_learning(Sa,responselabel="time_to_peak_speed_smooth_ms")) %>%
  bind_rows(test_learning(Sa,responselabel="straightness"))

paste(colnames(table), collapse=" & ")
writeLines(paste(table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ \n"), "table.txt")

#Gender.f = as.factor(Gender),
#Age.f = factor(Age, ordered=T),
#VRExperience.f = as.factor(VRExperience),
#GameExperience.f = as.factor(GameExperience),
#PlayedBefore.f = as.factor(PlayedBefore),
#VRSickness.f = as.factor(VRSickness),
#PredictedPattern.f = as.factor(PredictedPattern),
#Glasses.f = as.character(Glasses),


lme_table <- table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         across(everything(), ~ str_replace_all(.x, c("JudgementType.f" = "Performance Metric",
                                                      "PerformanceFeedback" = "Performance Feedback",
                                                      "travel_arm" = "Arm Travel",
                                                      "duration_ms" = "Duration (ms)",
                                                      "peak_speed_smooth_to_target" = "Peak Speed To Target",
                                                      "time_to_peak_speed_smooth_ms" = "Time To Peak Speed (ms)",
                                                      "peak_speed_smooth" = "Peak Speed (m/s)")))
  ) %>% select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)


message(paste(colnames(lme_table), collapse=" & "))
message(paste(lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))


m.pp = lmer(data=Sa, duration_ms ~ (1|Participant) + (1|MiniPatternLabel))
(anova(m.p,m.pp))

# 1) Testing combined feedback on movement time
m.pc = lmer(data=Sa, duration_ms ~ FeedbackJudge + (1|Participant) + (1|MiniPatternLabel))
(anova(m.pp,m.pc))
m.pc.summary <- summary(m.pc)
fixedtable = as.data.frame(m.pc.summary$coefficients) %>% 
  rownames_to_column("Fixed Effect") %>%
  mutate(Predicted = "Movement Time") 

# 2) Testing feedback isolated on movement time
m.pf = lmer(data=Sa, duration_ms ~ PerformanceFeedback + (1|Participant) + (1|MiniPatternLabel))
(anova(m.pp,m.pf))
m.pf.summary <- summary(m.pf)
fixedtable = as.data.frame(m.pf.summary$coefficients) %>% 
  rownames_to_column("Fixed Effect") %>%
  mutate(Predicted = "Movement Time") 

# 3) Testing judgement type isolated on movement time
m.pp = lmer(data=Sa, straightness_pct ~ (1+FeedbackJudge|Participant) + (1+FeedbackJudge|MiniPatternLabel), REML=F)
m.pj = lmer(data=Sa, straightness_pct ~ FeedbackJudge + (1+FeedbackJudge|Participant) + (1+FeedbackJudge|MiniPatternLabel), REML=F)
(anova(m.pp,m.pj))
m.pj.summary <- summary(m.pj)
fixedtable = as.data.frame(m.pj.summary$coefficients) %>% 
  rownames_to_column("Fixed Effect") %>%
  mutate(Predicted = "Movement Time") 

# Extract variances from VarCorr output
variance_components <- VarCorr(m.pj)
random_effects_variance <- as.data.frame(variance_components)$vcov
residual_variance <- attr(variance_components, "sc")^2

# Print the variances - if the random effects variance is orders of magnitude smaller than residual variance, the random effects might not contribute substantially.
cat("Random Effects Variance:\n")
print(random_effects_variance)
cat("\nResidual Variance:\n")
print(residual_variance)


# Null model: Expecting variance from participant and from pattern
m.p = clmm(data=Sc, as.ordered(Correspondence) ~ (1|Participant))
m.pp = clmm(data=Sc, as.ordered(Correspondence) ~ (1|Participant) + (1|MiniPatternLabel))
(anova(m.p,m.pp)) # significant, so we do need minipattern.

m.pj = clmm(data=Sc, as.ordered(Correspondence) ~ `Performance Feedback` * `Performance Metric` + (1|Participant) + (1|MiniPatternLabel))
(anova(m.pp,m.pj))
m.pj.summary <- summary(m.pj)
fixedtable = as.data.frame(m.pj.summary$coefficients) %>% 
  rownames_to_column("Fixed Effect") %>%
  mutate(Predicted = "AlgoCorrespondFastSlow.f") 



# ancova? correlation follows the trial block.



car::vif(m.pc)
anova(m.p,m.pf)
anova(m.p,m.pj)




fixedtable = as.data.frame(modelperc.summary$coefficients) %>% 
  rownames_to_column("Fixed Effect") %>%
  filter(`Fixed Effect` %in% c("pam_rate", "fishLost")) %>%
  mutate(Predicted = "Movement Time") %>% bind_rows(fixedtable)


fixedtable = fixedtable %>%
  rename(`p` = `Pr(>|z|)`) %>%
  mutate(Estimate = format(round(Estimate,2), nsmall = 2),
         `Std. Error` = format(round(`Std. Error`,2), nsmall = 2),
         `z value` = format(round(`z value`,2), nsmall = 2),
         `p` = format(round(`p`,3), nsmall = 3),
         `p` = ifelse(`p` == "0.000", "$<$0.001", `p`),
         across(everything(), ~ str_replace_all(.x, c("fishLost" = "Fish Lost",
                                                      "pam_rate" = "PAM Rate"))))

fixedtable <- fixedtable %>% group_by(Predicted) %>%
  mutate(Predictor = " ") %>%
  group_modify(~ add_row(Predictor=paste("\\underline{",.y,"}"),.before=0, .x)) %>%
  ungroup() %>% replace(is.na(.)," ") %>% arrange(desc(Predicted)) %>%
  select(Predicted = Predictor, `Fixed Effect`, `Estimate`, `Std. Error`, `z value`, `p`)

###
# Plots of quantitative measures
###

# ways participants reported it affected their performance:
# action feedback could delay participants' trip to the next mole
# 

# todo: I think the very low distance coverage i saw, might come from actions that dont belong to the task measure??
# todo: verify that the euclidean distance to travel in each condition in fact is also the same?
# todo: read from the segment code, what kind of patterns were being used - check the way they are distributed in the data.

# Steps to unpack this further:
# Divide the actions into operations, use the other Fitts paper as reference for how to make the estimations.
# 1) Visual Search
# 2) Going up to peak velocity
# 3) Slowing down/approaching target

fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~duration, type='box')
fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~straightness, type='box')
fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~travel_arm, type='box')
fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~peak_speed_smooth, type='box')
fig %>% add_trace(data=Sa, x=~Participant, y=~, type='box')
fig %>% add_trace(data=Sa, x=~Participant,color=~PerformanceFeedback,y=~peak_speed_smooth, type='box', color=I("rgba(50, 50, 50, 1)"))


plot_bar <- function(dataset, xlabel, ylabel, desc,xtitle,ytitle,miny,maxy,lwidth,overlay = "") {
  #browser()
  df = data.frame(x = dataset[[xlabel]],
                  y = as.numeric(dataset[[ylabel]]),
                  p = dataset$Participant)

  # Simple SD
  #df = df %>% group_by(x) %>% summarise(mean = mean(y), sd=sd(y))
  
  # 95% Confidence intervals
  df = df %>%
    group_by(p,x) %>%
    summarise(y = mean(y)) %>%
    group_by(x) %>%
    summarise(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE),
              n.y = n()) %>%
    mutate(se.y = sd.y / sqrt(n.y),
           lower.ci.y = mean.y - qt(1 - (0.05 / 2), n.y - 1) * se.y,
           upper.ci.y = mean.y + qt(1 - (0.05 / 2), n.y - 1) * se.y)
  
  fig_c = fig %>%
    add_trace(data=df,
              x=~x, y=~mean.y, color=I("grey"), #barmode='group', colors = c("lightgrey", "darkgrey", "grey"),
              error_y=~list(symmetric=FALSE, array=upper.ci.y - mean.y, arrayminus=mean.y-lower.ci.y, color = '#000000'), type='bar') %>%
    layout(margin=list(l=55,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                     text=desc), showlegend=F,
           xaxis=list(linewidth=lwidth, range=c(-0.45,~length(unique(x)) - 0.45), title=xtitle, zeroline=F, tickfont=list(size=15)),
           yaxis=list(linewidth=lwidth, range=c(miny,maxy), title=ytitle, zeroline=F, tickfont=list(size=15), showticklabels=T))
  
  if (overlay != "") {
    svg_file_path = paste0('fig/',overlay)
    svg_file <- readBin(svg_file_path, what="raw",n = file.info(svg_file_path)$size)
    base64_svg <- base64enc::base64encode(svg_file)
    base64_svg <- paste0("data:image/svg+xml;base64,", base64_svg)
    
    fig_c = fig_c %>%
      layout(images = list(
        list(
          source = base64_svg,
          x = 0, y = 0,  # Image position in plot coordinates (0 to 1)
          sizex = 1, sizey = 1,  # Size of the image in plot coordinates
          xanchor = "left", yanchor = "bottom",  # Anchor the image to the plot center
          xref = "paper", yref = "paper"  # Referencing the image position relative to the plot area
        )
      ))
  }
  
  return(fig_c)
}

plot_violin <- function(dataset, xlabel, ylabel,plabel, desc,xtitle,ytitle,miny,maxy,minx = NA,maxx = NA,bwidth,jit,overlay = "") {
  #browser()
  df = data.frame(x = dataset[[xlabel]],
                  y = as.numeric(dataset[[ylabel]]),
                  pid = dataset[[plabel]])
  
  df = df %>% group_by(x,pid) %>% summarise(y = mean(y))
  
  if (is.na(minx)) {
    minx = -0.45
  }
  if (is.na(maxx)) {
    maxx = length(unique(df$x))-0.45
  }
  
  # 95% Confidence intervals
  # The intervals are currently based on a median value from each participants, because
  # they need to be independent and are supposed to show where future participants
  # performance are expected to be.
  dfe = df %>%
    group_by(pid,x) %>%
    summarise(y = mean(y)) %>%
    group_by(x) %>%
    summarise(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE),
              n.y = n()) %>%
    mutate(se.y = sd.y / sqrt(n.y),
           lower.ci.y = mean.y - qt(1 - (0.05 / 2), n.y - 1) * se.y,
           upper.ci.y = mean.y + qt(1 - (0.05 / 2), n.y - 1) * se.y)
  
  fig_c <- fig %>%
    add_trace(data=df, x=~x, 
              y=~jitter(y,amount=jit),
              scalemode='width', points='all', pointpos=0,name='C', jitter=.65, #meanline=list(visible=T,width=4,color="rgba(0, 0, 0, 255)"),
              marker=list(size=3,color="rgba(120,120,120, 0.35)",line=list(width=1.5,color="rgba(120,120,120, 0.15)")),
              scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=bwidth, color=I("rgba(120, 120, 120, 1)")) %>%
    add_trace(data=dfe, x=~x,y=~mean.y, type='scatter',mode='markers', color=I("rgba(50, 50, 50, 1)"), 
              error_y=~list(symmetric=FALSE, array=dfe$upper.ci.y - dfe$mean.y, arrayminus=dfe$mean.y-dfe$lower.ci.y) # todo: get lmer4 SE
    )  %>%
    layout(margin=list(l=15,r=0,t=55,b=70),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                    text=desc), showlegend=F,
           xaxis=list(range=c(minx,maxx), title=xtitle, zeroline=F, tickfont=list(size=15)),
           yaxis=list(range=c(miny,maxy), title=ytitle, zeroline=F, tickfont=list(size=15), showticklabels=T,dtick=1) )

  if (overlay != "") {
    svg_file_path = paste0('fig/',overlay)
    svg_file <- readBin(svg_file_path, what="raw",n = file.info(svg_file_path)$size)
    base64_svg <- base64enc::base64encode(svg_file)
    base64_svg <- paste0("data:image/svg+xml;base64,", base64_svg)
    
    
    fig_c = fig_c %>%
      layout(images = list(
        list(
          source = base64_svg,
          #source =  "https://raw.githubusercontent.com/cldougl/plot_images/add_r_img/vox.png",  
          xref = "paper",  
          yref = "paper",  
          x = 0,  
          y = -0.41,  
          sizex = 1,  
          sizey = 1,  
          xanchor="left",  
          yanchor="bottom"
          #x = -0.1, y = 0.91,  # Image position in plot coordinates (0 to 1)
          #sizex = 1, sizey = 1,  # Size of the image in plot coordinates
          #xanchor = "left", yanchor = "bottom",  # Anchor the image to the plot center
          #xref = "paper", yref = "paper"  # Referencing the image position relative to the plot area
        )
      ))
  }  
    
  return(fig_c)
}

# Performance plots
fig_plot = read.csv("wam_plot.csv", sep=";")

# Initialize an empty list to store the plots
figs <- list()

# Use a for loop to iterate over each row in the data frame
for (i in 1:nrow(fig_plot)) {
  row <- fig_plot[i, ]
  figs[[i]] <- plot_violin(dataset = Sa %>% ungroup(), 
                        xlabel = row$x, 
                        ylabel = row$y, 
                        plabel = "Participant",
                        desc = row$desc, 
                        xtitle = row$xtitle, 
                        ytitle = row$ytitle, 
                        miny = row$miny, 
                        maxy = row$maxy,
                        bwidth = row$bandwidth,
                        jit = row$jitter)
  orca(figs[[i]], paste('fig/violin_error',row$annotation,row$y,'.pdf',sep="_"), width=row$width, height=row$height)
}


# Use a for loop to iterate over each row in the data frame
for (i in 1:nrow(fig_plot)) {
  row <- fig_plot[i, ]
  figs[[i]] <- plot_bar(dataset = Sa %>% ungroup(), 
                           xlabel = row$x, 
                           ylabel = row$y, 
                           desc = row$desc, 
                           xtitle = row$xtitle, 
                           ytitle = row$ytitle, 
                           miny = row$miny, 
                           maxy = row$maxy,
                           lwidth = row$lwidth,
                           overlay = row$overlay)
  orca(figs[[i]], paste('fig/bar_error',row$annotation,row$y,'.pdf',sep="_"), width=row$width, height=row$height)
}




# Likert Plots
fig_plot = read.csv("wam_plot_lf.csv", sep=";")

# Initialize an empty list to store the plots
figs <- list()

# Use a for loop to iterate over each row in the data frame
for (i in 1:nrow(fig_plot)) {
  row <- fig_plot[i, ]
  figs[[i]] <- plot_violin(dataset = Sa %>% ungroup(), 
                           xlabel = row$x, 
                           ylabel = row$y, 
                           plabel = "Participant",
                           desc = str_replace(row$desc,"\\\\n","\n"), 
                           xtitle = row$xtitle, 
                           ytitle = row$ytitle, 
                           miny = row$miny, 
                           maxy = row$maxy,
                           minx = row$minx, 
                           maxx = row$maxx,
                           bwidth = row$bandwidth,
                           jit = row$jitter,
                           overlay = row$overlay)
  figs[[i]]
  orca(figs[[i]], paste('fig/violin_error',row$annotation,row$y,'.pdf',sep="_"), width=row$width, height=row$height)
}


plot_playorder <- function(dataset, ylabel, xlabel = "PlayOrder", plabel="Participant") {
  #browser()
  df = data.frame(x = dataset[[xlabel]],
                  y = as.numeric(dataset[[ylabel]]),
                  pid = as.character(dataset[[plabel]]))
  df = df %>% mutate(x2 = as.numeric(x),
                     x2 = (x2-1) %/% 3,
                     x = x2,
                     x2 = NULL)
  
  df = df %>% arrange(pid,x) %>% group_by(x) %>%
    summarise(y = mean(y))

fig %>%
  add_trace(data=df, x=~x, 
            y=~y,
            marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 120, 1)")) %>%
  add_trace(data=p_lin(df, "y", "x"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 120, 1)")) %>%
  layout(margin=list(l=55,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                   text=p_lin_coef(df, "y", "x")), showlegend=T,
         xaxis=list(title=' ', zeroline=F, tickfont=list(size=15)),
         yaxis=list(title=' ', zeroline=F, tickfont=list(size=15), showticklabels=T))
}
plot_playorder(Spl,"Throughput")
plot_playorder(Spl,"Action Duration (ms)")
plot_playorder(Spl,"Action Arm Travel (meter)")
plot_playorder(Spl,"Straightness (0-1)")
plot_playorder(Spl,"Peak Speed (m/s)")
plot_playorder(Spl,"Time to Peak Speed (ms)")
plot_playorder(Spl,"Peak Speed to Target (\\%)")



orca(fig_c, "fig/throughput-visual-analysis.pdf", width=395, height=355)

fig_c <- fig %>%
  add_trace(data=Sa %>% group_by(Participant,OverallExperience.f) %>% summarise(duration_ms = median(duration_ms)), x=~OverallExperience.f, 
            y=~jitter(duration_ms,amount=2),
            marker=list(size=3,color="rgba(120,120,120, 0.80)",line=list(width=1.5,color="rgba(120,120,120, 0.15)")),
            type='scatter',mode='marker',color=I("rgba(120, 120, 120, 1)")) %>%
  layout(margin=list(l=55,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                   text='Overall Experience vs duration'), showlegend=F,
         xaxis=list(range=c(-0.45,7.45), title=' ', zeroline=F, tickfont=list(size=15)),
         yaxis=list(range=c(1000,1600), title=' ', zeroline=F, tickfont=list(size=15), showticklabels=T))

orca(fig_c, "fig/control_overallexp_vs_duration.pdf", width=325, height=355)


horfig = fig %>%
  add_trace(data=df, x=~jitter(y,amount=jit),
            y=~factor(x,levels=rev(levels(x))), orientation='h',
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, #meanline=list(visible=T,width=4,color="rgba(0, 0, 0, 255)"),
            marker=list(size=4,color="rgba(90,90,90, 0.35)",line=list(width=1.5,color="rgba(120,120,120, 0.15)")),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=bwidth, color=I("rgba(120, 120, 120, 1)")) %>%
  add_trace(data=dfe, x=~mean.y,y=~x, type='scatter',mode='markers', color=I("rgba(50, 50, 50, 1)"), 
            error_x=~list(symmetric=FALSE, array=dfe$upper.ci.y - dfe$mean.y, arrayminus=dfe$mean.y-dfe$lower.ci.y) # todo: get lmer4 SE
  )  %>%
  layout(margin=list(l=0,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                   text=desc), showlegend=F,
         xaxis=list(range=c(miny,maxy), title=xtitle, zeroline=F, tickfont=list(size=15), dtick=1),
         yaxis=list(range=c(-0.45,8.5), title=ytitle, zeroline=F, tickfont=list(size=15), showticklabels=F))


orca(horfig, "fig/correspondence-horisontal.pdf", width=393, height=500)

#############
# Latex Table: Participants
#############

# g0 = min, g1 = 1st Qu, g2 = median, g3 = 3rd Qu, g4 = Max.

cri = tibble(`Action Duration (ms)` = rev(c(1.0, 1.25,1.353,1.45,1.6,1.8,5.2)), # 1.353 is the median
             `Participant` = c(30,100,100,100,100,100,100), # always g0
             `Actions (n)` = c(80,100,210,300,300,300,300), # always g0
             `Actions Analyzed (n)` = c(30,100,210,300,300,300,300), # always g0
             `Action Arm Travel (meter)` = c(0.15,0.25,0.3,0.35,0.4,0.45,0.5), # 0.2839 is the median
             `Total Arm Travel (meter)` = c(45,50,55,60,65,70,75), # 55 is the median
             `Euc. Arm Travel (meter)` =  c(35,40,45,50,55,60,65), #46 median
             `Straightness (0-1)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.82 median
             `Peak Speed (m/s)` =  c(0.5,1.0,1.5,2.0,2.5,3.0,3.5), # 1.41 median
             `Time to Peak Speed (ms)` =  rev(c(0.35,0.45,0.55,0.65,0.75,0.85,0.95)),
             `Peak Speed to Target (\\%)` =  c(45,55,60,65,70,75,80),
             `Fitts ID` = c(2.00,2.20,2.40,2.60,2.80,3.00,3.20), #median 2.75
             colors = c("g0","g1", "g2", "g3", "g4","g4","g4"),)

Sp_f = read.csv("wam_col_latex.csv", sep=";")

# Define columns to color
Sp_col = Sp_f %>% filter(color) %>% pull(name)
# Define which cols should have rounded numbers.
Sp_numcols = Sp_f %>% filter(dec) %>% pull(name)
Sp_perccols = Sp_f %>% filter(perc) %>% pull(name)
# Define which cols should have standard deviation.
Sp_sdcols = Sp_f %>% filter(sd) %>% pull(name)


Sp_table = Sp %>% mutate(FeedbackJudge = "All")
Sp_table = Sp_table %>% bind_rows(Sf)

# Generate colors from col values
Sp_table <- Sp_table %>% mutate(across(all_of(Sp_col), ~ t_color(.x,  cri[[cur_column()]],cri$colors), .names = "{.col}_c"))


# Apply number rounding
Sp_table <- Sp_table %>% mutate(across(all_of(Sp_numcols), ~ format(round(.x,2), nsmall = 2)))
Spsd_table <- Sp_sd %>% mutate(across(all_of(Sp_sdcols), ~ format(round(.x,2), nsmall = 2)))

# apply percentage
# already applied now
Sp_table <- Sp_table %>% mutate(across(all_of(Sp_perccols), ~ round(.x,0)))
Sp_table <- Sp_table %>% mutate(across(all_of(Sp_perccols), ~ paste0(as.character(.x),"\\%")))

# Add colors to numbers
Sp_table <- Sp_table %>% mutate(across(all_of(Sp_col), ~ paste0("\\cellcolor{", get(paste0(cur_column(), "_c")), "}", as.character(.x))))

# Add standard deviation to numbers
#Sp_table <- Sp_table %>% mutate(across(all_of(Sp_sdcols), ~ ifelse(cur_column() %in% Sp_sdcols,
#                                                                      paste0(.x, "(", Spsd_table[[cur_column()]],")"),
#                                                                      .x)))
# Cleanup color cols and format all as strings
Sp_table <- Sp_table %>% select(-ends_with("_c")) %>%
  mutate(across(everything(), as.character))

Sp_table = Sp_table %>% select(-Feedback, -JudgementType)

# Pivot
Sp_table <- Sp_table %>% pivot_longer(cols=-c(Participant,FeedbackJudge), names_to = "Variable")  %>%
  pivot_wider(names_from = Participant, values_from = value)

#Sp_table <- Sp_table %>% group_by(Variable) %>%
#  group_modify(~ add_row(`1`=paste("\\multicolumn{5}{l}{ \\underline{",.y,"} }"),.before=0, .x)) %>%
#  ungroup() %>% replace(is.na(.)," ") %>%
#  select(-Variable)

condition_order = c("OperationTime","OperationSpeed","OperationDistance","ActionTime","ActionSpeed","ActionDistance","TaskTime","TaskSpeed","TaskDistance")
Sp_table$FeedbackJudge = factor(Sp_table$FeedbackJudge,levels=c("All",condition_order))

Sp_table <- Sp_table %>% group_by(FeedbackJudge) %>%
 group_modify(~ add_row(`Variable`=paste("\\multicolumn{5}{l}{ \\underline{", levels(Sp_table$FeedbackJudge)[.y$FeedbackJudge],"} }"),.before=0, .x)) %>%
 ungroup() %>% replace(is.na(.)," ") %>% 
 select(-FeedbackJudge)

# Export
paste(colnames(Sp_table), collapse=" & ")
writeLines(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ \n"), "table.txt")

#############
# Latex Table: Condition
#############

cri = tibble(`Action Duration (ms)` = rev(c(1000, 1150,1250,1350,1450,1550,1650)), # 1.353 is the median
             `Participant` = c(30,100,100,100,100,100,100), # always g0
             `Actions (n)` = c(80,100,210,300,300,300,300), # always g0
             `Actions Analyzed (n)` = c(30,100,210,300,300,300,300), # always g0
             `Action Arm Travel (meter)` = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45), # 0.2839 is the median
             `Total Arm Travel (meter)` = c(45,50,55,60,65,70,75), # 55 is the median
             `Euc. Arm Travel (meter)` =  c(35,40,45,50,55,60,65), #46 median
             `Straightness (0-1)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.82 median
             `Peak Speed (m/s)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.83 median
             `Time to Peak Speed (ms)` =  rev(c(450,500,550,600,650,700,750)),
             `Peak Speed to Target (\\%)` =  c(35,45,55,65,75,85,95),
             `Correspondence (1-7)` =  c(3.0,3.5,4.0,4.5,5.0,5.5,6.0),
             `Fitts ID` = c(2.5,2.6,2.7,2.8,2.9,3.0,3.1), #median 2.75
             `Throughput (bits/s)` = c(2.13,2.18,2.23,2.28,2.33,2.38,2.43), #median 2.23
             colors = c("g3","g4", "g5", "g6", "g5","g4","g3"),)

Scfm_f = read.csv("wam_col_latex.csv", sep=";")

# filter Scfm_f to only have the columns Scfm has
Scfm_f = Scfm_f %>% filter(name %in% colnames(Scfm))

# Define columns to color
Scfm_col = Scfm_f %>% filter(color) %>% pull(name)
# Define which cols should have rounded numbers.
Scfm_numcols = Scfm_f %>% filter(dec) %>% pull(name)
Scfm_nodeccols = Scfm_f %>% filter(nodec) %>% pull(name)
Scfm_perccols = Scfm_f %>% filter(perc) %>% pull(name)
# Define which cols should have standard deviation.
Scfm_sdcols = Scfm_f %>% filter(sd) %>% pull(name)

Scfm_table = Scfm

# Generate colors from col values
Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_col), ~ t_color(.x,  cri[[cur_column()]],cri$colors), .names = "{.col}_c"))


# Apply number rounding
Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_numcols), ~ format(round(.x,2), nsmall = 2)))

# apply percentage
# already applied now
Scfm_table <- Scfm_table %>% mutate(across(all_of(c(Scfm_perccols,Scfm_nodeccols)), ~ round(.x,0)))
Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_perccols), ~ paste0(as.character(.x),"\\%")))

Scfmsd_table <- Scfm_sd %>% mutate(across(all_of(c(Scfm_perccols,Scfm_nodeccols)), ~ round(.x,0)))
Scfmsd_table <- Scfmsd_table %>% mutate(across(all_of(Scfm_numcols), ~ format(round(.x,1), nsmall = 1)))

# Add colors to numbers
Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_col), ~ paste0("\\cellcolor{", get(paste0(cur_column(), "_c")), "}", as.character(.x))))

# Add standard deviation to numbers
Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_col), ~ paste0(as.character(.x), " {\\color{sd} ", Scfmsd_table[[cur_column()]], "}")))

#Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_sdcols), ~ ifelse(cur_column() %in% Scfm_sdcols,
#                                                                      paste0(.x, " (", Scfmsd_table[[cur_column()]],")"),
#                                                                      .x)))
# Cleanup color cols and format all as strings
Scfm_table <- Scfm_table %>% select(-ends_with("_c")) %>%
  mutate(across(everything(), as.character))

condition_order = c("OperationTime","OperationSpeed","OperationDistance","ActionTime","ActionSpeed","ActionDistance","TaskTime","TaskSpeed","TaskDistance")
Scfm_table$FeedbackJudge = factor(Scfm_table$FeedbackJudge,levels=c("All",condition_order))
Scfm_table = Scfm_table %>% arrange(FeedbackJudge)

# Pivot
Scfm_table <- Scfm_table %>% pivot_longer(cols=-c(FeedbackJudge), names_to = "Variable")  %>%
  pivot_wider(names_from = FeedbackJudge, values_from = value)

#Sp_table <- Sp_table %>% group_by(Variable) %>%
#  group_modify(~ add_row(`1`=paste("\\multicolumn{5}{l}{ \\underline{",.y,"} }"),.before=0, .x)) %>%
#  ungroup() %>% replace(is.na(.)," ") %>%
#  select(-Variable)

# Export
paste(colnames(Scfm_table), collapse=" & ")
writeLines(paste(Scfm_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ \n"), "table.txt")

#############
# Latex Table: Metrics
#############

cri = tibble(`Action Duration (ms)` = rev(c(1000, 1150,1250,1350,1450,1550,1650)), # 1.353 is the median
             `Participant` = c(30,100,100,100,100,100,100), # always g0
             `Actions (n)` = c(80,100,210,300,300,300,300), # always g0
             `Actions Analyzed (n)` = c(30,100,210,300,300,300,300), # always g0
             `Action Arm Travel (meter)` = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45), # 0.2839 is the median
             `Total Arm Travel (meter)` = c(45,50,55,60,65,70,75), # 55 is the median
             `Euc. Arm Travel (meter)` =  c(35,40,45,50,55,60,65), #46 median
             `Straightness (0-1)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.82 median
             `Peak Speed (m/s)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.83 median
             `Time to Peak Speed (ms)` =  rev(c(450,500,550,600,650,700,750)),
             `Peak Speed to Target (\\%)` =  c(35,45,55,65,75,85,95),
             `Correspondence (1-7)` =  c(3.0,3.5,4.0,4.5,5.0,5.5,6.0),
             `Fitts ID` = c(2.5,2.6,2.7,2.8,2.9,3.0,3.1), #median 2.75
             `Throughput (bits/s)` = c(2.13,2.18,2.23,2.28,2.33,2.38,2.43), #median 2.23
             colors = c("g3","g4", "g5", "g6", "g5","g4","g3"),)

Scmm_f = read.csv("wam_col_latex.csv", sep=",")

# filter Scfm_f to only have the columns Scfm has
Scmm_f = Scmm_f %>% filter(name %in% colnames(Scmm))

# Define columns to color
Scmm_col = Scmm_f %>% filter(color) %>% pull(name)
# Define which cols should have rounded numbers.
Scmm_numcols = Scmm_f %>% filter(dec) %>% pull(name)
Scmm_nodeccols = Scmm_f %>% filter(nodec) %>% pull(name)
Scmm_perccols = Scmm_f %>% filter(perc) %>% pull(name)
# Define which cols should have standard deviation.
Scmm_sdcols = Scmm_f %>% filter(sd) %>% pull(name)

Scmm_table = Scmm

# Generate colors from col values
#Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_col), ~ t_color(.x,  cri[[cur_column()]],cri$colors), .names = "{.col}_c"))


# Apply number rounding
Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_numcols), ~ format(round(.x,2), nsmall = 2)))

# apply percentage
# already applied now
#Scmm_table <- Scmm_table %>% mutate(across(all_of(c(Scmm_perccols,Scmm_nodeccols)), ~ round(.x,0)))
#Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_perccols), ~ paste0(as.character(.x),"\\%")))

Scmmsd_table <- Scmm_sd %>% mutate(across(all_of(c(Scmm_perccols,Scmm_nodeccols)), ~ round(.x,0)))
Scmmsd_table <- Scmmsd_table %>% mutate(across(all_of(Scmm_numcols), ~ format(round(.x,1), nsmall = 1)))

# Add colors to numbers
#Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_col), ~ paste0("\\cellcolor{", get(paste0(cur_column(), "_c")), "}", as.character(.x))))

# Add standard deviation to numbers
Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_col), ~ paste0(as.character(.x), " {\\color{sd} ", Scmmsd_table[[cur_column()]], "}")))

#Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_sdcols), ~ ifelse(cur_column() %in% Scfm_sdcols,
#                                                                      paste0(.x, " (", Scfmsd_table[[cur_column()]],")"),
#                                                                      .x)))
# Cleanup color cols and format all as strings
Scmm_table <- Scmm_table %>% select(-ends_with("_c")) %>%
  mutate(across(everything(), as.character))

#condition_order = c("OperationTime","OperationSpeed","OperationDistance","ActionTime","ActionSpeed","ActionDistance","TaskTime","TaskSpeed","TaskDistance")
#Scmm_table$FeedbackJudge = factor(Scm_table$FeedbackJudge,levels=c("All",condition_order))
#Scmm_table = Scmm_table %>% arrange(FeedbackJudge)

# Pivot
Scmm_table <- Scmm_table %>% pivot_longer(cols=-c(JudgementType), names_to = "Variable")  %>%
  pivot_wider(names_from = Variable, values_from = value)

#Sp_table <- Sp_table %>% group_by(Variable) %>%
#  group_modify(~ add_row(`1`=paste("\\multicolumn{5}{l}{ \\underline{",.y,"} }"),.before=0, .x)) %>%
#  ungroup() %>% replace(is.na(.)," ") %>%
#  select(-Variable)

# Remove unneeded cols
Scmm_table = Scmm_table %>% select(-`Fitts ID`, `Actions (n)`)

# Export
paste(colnames(Scmm_table), collapse=" & ")
writeLines(paste(Scmm_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ \n"), "table.txt")

#############
# Latex Table: Feedback
#############

cri = tibble(`Action Duration (ms)` = rev(c(1000, 1150,1250,1350,1450,1550,1650)), # 1.353 is the median
             `Participant` = c(30,100,100,100,100,100,100), # always g0
             `Actions Analyzed (n)` = c(30,100,210,300,300,300,300), # always g0
             `Action Arm Travel (meter)` = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45), # 0.2839 is the median
             `Total Arm Travel (meter)` = c(45,50,55,60,65,70,75), # 55 is the median
             `Euc. Arm Travel (meter)` =  c(35,40,45,50,55,60,65), #46 median
             `Straightness (0-1)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.82 median
             `Peak Speed (m/s)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.83 median
             `Time to Peak Speed (ms)` =  rev(c(450,500,550,600,650,700,750)),
             `Peak Speed to Target (\\%)` =  c(35,45,55,65,75,85,95),
             `Correspondence (1-7)` =  c(3.0,3.5,4.0,4.5,5.0,5.5,6.0),
             `Fitts ID` = c(2.5,2.6,2.7,2.8,2.9,3.0,3.1), #median 2.75
             `Throughput (bits/s)` = c(2.13,2.18,2.23,2.28,2.33,2.38,2.43), #median 2.23
             colors = c("g3","g4", "g5", "g6", "g5","g4","g3"),)

Scmm_f = read.csv("wam_col_latex.csv", sep=",")

# filter Scfm_f to only have the columns Scfm has
Scmm_f = Scmm_f %>% filter(name %in% colnames(Scmm))

# Define columns to color
Scmm_col = Scmm_f %>% filter(color) %>% pull(name)
# Define which cols should have rounded numbers.
Scmm_numcols = Scmm_f %>% filter(dec) %>% pull(name)
Scmm_nodeccols = Scmm_f %>% filter(nodec) %>% pull(name)
Scmm_perccols = Scmm_f %>% filter(perc) %>% pull(name)
# Define which cols should have standard deviation.
Scmm_sdcols = Scmm_f %>% filter(sd) %>% pull(name)

Scmm_table = Scmm

# Generate colors from col values
#Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_col), ~ t_color(.x,  cri[[cur_column()]],cri$colors), .names = "{.col}_c"))


# Apply number rounding
Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_numcols), ~ format(round(.x,2), nsmall = 2)))

# apply percentage
# already applied now
Scmm_table <- Scmm_table %>% mutate(across(all_of(c(Scmm_perccols,Scmm_nodeccols)), ~ round(.x,0)))
Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_perccols), ~ paste0(as.character(.x),"\\%")))

Scmmsd_table <- Scmm_sd %>% mutate(across(all_of(c(Scmm_perccols,Scmm_nodeccols)), ~ round(.x,0)))
Scmmsd_table <- Scmmsd_table %>% mutate(across(all_of(Scmm_numcols), ~ format(round(.x,1), nsmall = 1)))

# Add colors to numbers
#Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_col), ~ paste0("\\cellcolor{", get(paste0(cur_column(), "_c")), "}", as.character(.x))))

# Add standard deviation to numbers
Scmm_table <- Scmm_table %>% mutate(across(all_of(Scmm_col), ~ paste0(as.character(.x), " {\\color{sd} ", Scmmsd_table[[cur_column()]], "}")))

#Scfm_table <- Scfm_table %>% mutate(across(all_of(Scfm_sdcols), ~ ifelse(cur_column() %in% Scfm_sdcols,
#                                                                      paste0(.x, " (", Scfmsd_table[[cur_column()]],")"),
#                                                                      .x)))
# Cleanup color cols and format all as strings
Scmm_table <- Scmm_table %>% select(-ends_with("_c")) %>%
  mutate(across(everything(), as.character))

#condition_order = c("OperationTime","OperationSpeed","OperationDistance","ActionTime","ActionSpeed","ActionDistance","TaskTime","TaskSpeed","TaskDistance")
#Scmm_table$FeedbackJudge = factor(Scm_table$FeedbackJudge,levels=c("All",condition_order))
#Scmm_table = Scmm_table %>% arrange(FeedbackJudge)

# Pivot
Scmm_table <- Scmm_table %>% pivot_longer(cols=-c(JudgementType), names_to = "Variable")  %>%
  pivot_wider(names_from = Variable, values_from = value)

#Sp_table <- Sp_table %>% group_by(Variable) %>%
#  group_modify(~ add_row(`1`=paste("\\multicolumn{5}{l}{ \\underline{",.y,"} }"),.before=0, .x)) %>%
#  ungroup() %>% replace(is.na(.)," ") %>%
#  select(-Variable)

Scmm_table = Scmm_table %>% select(-`Fitts ID`)

# Export
paste(colnames(Scmm_table), collapse=" & ")
writeLines(paste(Scmm_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ \n"), "table.txt")



#############
# Correlations
#############

corr_colvars = Scf %>% ungroup() %>% select(-Participant, -`Actions (n)`, -`Actions Analyzed (n)`, -MiniPatternLabel, -PerformanceFeedback.f, -`Performance Metric`) %>% colnames(.)

Scf_cor <- Scf %>% ungroup() %>% mutate(across(all_of(corr_colvars), ~ as.numeric(.x))) %>% select(all_of(corr_colvars)) %>% as.data.frame(.)
Scm_cor <- Scm %>% ungroup() %>% mutate(across(everything(), ~ as.numeric(.x))) %>% select(-Participant) %>% as.data.frame(.) 

cor.test(Scm_cor$best_adaption, Scm_cor$overall_perf, method="spearman")
cor.test(Scm_cor$speed_adaption, Scm_cor$speed_perf, method="spearman")

cor.test(Scf_cor$Correspondence, Scf_cor$`Straightness (0-1)`, method="spearman")
cor.test(Scf_cor$Correspondence, Scf_cor$Distraction, method="spearman")

PerformanceAnalytics::table.Correlation(Scf_cor, method="spearman")
PerformanceAnalytics::chart.Correlation(Scm_cor, method="spearman")

PerformanceAnalytics::chart.Correlation(Spf_num, method="spearman")


tstfun <- function (Ra, Rb, ...) 
{
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  columnnames.a = colnames(Ra)
  columnnames.b = colnames(Rb)
  for (column.a in 1:columns.a) {
    for (column.b in 1:columns.b) {
      merged.assets = merge(Ra[, column.a, drop = FALSE], 
                            Rb[, column.b, drop = FALSE])
      merged.assets = na.omit(merged.assets)
      htest = cor.test(as.numeric(merged.assets[, 1]), 
                       as.numeric(merged.assets[, 2]), ...)
      values = cbind(htest$estimate, htest$p.value, htest$conf.int[1], 
                     htest$conf.int[2])
      if (column.a == 1 & column.b == 1) {
        result.df = data.frame(Value = values)
        rownames(result.df) = paste(columnnames.a[column.a], 
                                    columnnames.b[column.b], sep = " to ")
      }
      else {
        nextrow = data.frame(Value = values)
        rownames(nextrow) = paste(columnnames.a[column.a], 
                                  columnnames.b[column.b], sep = " to ")
        result.df = rbind(result.df, nextrow)
      }
    }
  }
  colnames(result.df) = c("Correlation", "p-value", "Lower CI", 
                          "Upper CI")
  result.df
}


#############
# Latex Table: Post-Experiment Questionnaire
#############


###
# How peak speed rose over the course of the study.
###
library(lubridate)

#hitnumber, from mole 1 to mole 20
Sa = Sa %>% group_by(Participant.f, FeedbackJudge.f) %>% 
  mutate(counter = 1,
         hitnumber = cumsum(counter))

Sa = Scm %>% select(Participant, Place, AdaptionRank) %>% right_join(Sa)

Sa2 = Sa %>% filter(Place > 11, hitnumber > 5)  
Sa2 = Sa %>% filter(AdaptionRank < 12, hitnumber > 5)  

Scm %>% filter(ad)

# Sc: Summary of Condition
Sc2 = Sa2 %>% group_by(Participant, FeedbackJudge.f) %>%
  dplyr::summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (ms)` = mean(duration_ms),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed_smooth),
    `Time to Peak Speed (ms)` = mean(time_to_peak_speed_smooth_ms),
    `Peak Speed to Target (\\%)` = mean(peak_speed_smooth_to_target_pct),
    `Correspondence` = unique(AlgoCorrespondFastSlow.f),
    MiniPatternLabel = paste(unique(MiniPatternLabel),collapse=", "),
    `Performance Feedback` = unique(PerformanceFeedback.f),
    `Performance Metric` = unique(JudgementType),
    `Throughput (bits/s)` = mean(throughput),
    `Fitts ID` = mean(fittsID),
    PlayOrder = unique(PlayOrder)
  ) %>% mutate(
    Participant.f = as.factor(Participant),
  )



fig %>% add_trace(data=Sa2 %>% ungroup() %>% filter(PerformanceFeedback.f %in% c("Operation")) %>% group_by(fittsID_laser) %>% summarise(peak_speed=mean(peak_speed_smooth)), opacity=0.95, marker=list(size=5,color="rgba(255,0,0, 1.0)"),
                  x=~fittsID_laser, y=~peak_speed, type='scatter',mode='markers') %>%
  add_trace(data=Sa2 %>% ungroup() %>% filter(PerformanceFeedback.f %in% c("Action")) %>% group_by(fittsID_laser) %>% summarise(peak_speed=mean(peak_speed_smooth)), opacity=0.95, marker=list(size=5,color="rgba(0,0,255, 1.0)"),
                x=~fittsID_laser, y=~peak_speed, type='scatter',mode='markers') %>%
        add_trace(data=Sa2 %>% ungroup() %>% group_by(fittsID_laser), opacity=0.3, marker=list(size=5,color="rgba(60,120,60, 0.5.0)"),
            x=~fittsID_laser, y=~peak_speed_smooth, type='scatter',mode='markers') %>%
        add_trace(data=p_lin(Sa2 %>% filter(PerformanceFeedback.f %in% c("Operation")), "peak_speed_smooth", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
          type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)")) %>%
  add_trace(data=p_lin(Sa2 %>% filter(PerformanceFeedback.f %in% c("Action")), "peak_speed_smooth", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 255, 1)")) %>%
        add_trace(data=p_lin(Sa2 %>% filter(PerformanceFeedback.f %in% c("Task")), "peak_speed_smooth", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,255,120, 0.0)",line=list(width=1.5,color="rgba(120,255,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(20, 200, 20, 1)"))

fig %>% add_trace(data=Sa2 %>% ungroup() %>% filter(PerformanceFeedback.f %in% c("Operation")) %>% group_by(fittsID_laser) %>% summarise(duration_ms=mean(duration_ms)), opacity=0.95, marker=list(size=5,color="rgba(255,0,0, 1.0)"),
                  x=~fittsID_laser, y=~duration_ms, type='scatter',mode='markers') %>%
  add_trace(data=Sa2 %>% ungroup() %>% filter(PerformanceFeedback.f %in% c("Action")) %>% group_by(fittsID_laser) %>% summarise(duration_ms=mean(duration_ms)), opacity=0.95, marker=list(size=5,color="rgba(0,0,255, 1.0)"),
            x=~fittsID_laser, y=~duration_ms, type='scatter',mode='markers') %>%
  add_trace(data=Sa2 %>% ungroup() %>% group_by(fittsID_laser), opacity=0.3, marker=list(size=5,color="rgba(60,120,60, 0.5.0)"),
            x=~fittsID_laser, y=~duration_ms, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa2 %>% filter(PerformanceFeedback.f %in% c("Operation")), "duration_ms", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)")) %>%
  add_trace(data=p_lin(Sa2 %>% filter(PerformanceFeedback.f %in% c("Action")), "duration_ms", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 255, 1)")) %>%
  add_trace(data=p_lin(Sa2 %>% filter(PerformanceFeedback.f %in% c("Task")), "duration_ms", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,255,120, 0.0)",line=list(width=1.5,color="rgba(120,255,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(20, 200, 20, 1)"))


fig %>% add_trace(data=Sa2 %>% ungroup() %>% filter(JudgementType.f %in% c("Time"), PerformanceFeedback.f %in% c("Operation","Action")) %>% group_by(fittsID_laser) %>% summarise(duration=mean(duration_ms)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 1.0)"),
                  x=~fittsID_laser, y=~duration, type='scatter',mode='markers') %>%
  add_trace(data=Sa2 %>% ungroup() %>% group_by(fittsID_laser), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~fittsID_laser, y=~duration_ms, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa2 %>% filter(JudgementType.f %in% c("Time"), PerformanceFeedback.f %in% c("Operation","Action")), "duration_ms", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)")) %>%
  add_trace(data=p_lin(Sa2 %>% filter(JudgementType.f %in% c("Speed","Distance"), PerformanceFeedback.f %in% c("Operation","Action")), "duration_ms", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(20, 20, 20, 1)"))

fig %>% add_trace(data=Sa2 %>% ungroup() %>% filter(JudgementType.f %in% c("Distance"), PerformanceFeedback.f %in% c("Operation","Action")) %>% group_by(fittsID_laser) %>% summarise(straightness=mean(straightness)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 1.0)"),
                  x=~fittsID_laser, y=~straightness, type='scatter',mode='markers') %>%
  add_trace(data=Sa2 %>% ungroup() %>% group_by(fittsID_laser), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~fittsID_laser, y=~straightness, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa2 %>% filter(JudgementType.f %in% c("Distance"), PerformanceFeedback.f %in% c("Operation","Action")), "straightness", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)")) %>%
  add_trace(data=p_lin(Sa2 %>% filter(JudgementType.f %in% c("Time","Speed"), PerformanceFeedback.f %in% c("Operation","Action")), "straightness", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(20, 20, 20, 1)"))

fig %>% add_trace(data=Sa %>% ungroup() %>% filter(JudgementType.f %in% c("Distance")) %>% group_by(fittsID_laser) %>% summarise(travel_laser=mean(travel_laser)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 1.0)"),
                  x=~fittsID_laser, y=~travel_laser, type='scatter',mode='markers') %>%
  add_trace(data=Sa %>% ungroup() %>% group_by(fittsID_laser), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~fittsID_laser, y=~travel_laser, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa %>% filter(JudgementType.f %in% c("Distance")), "travel_laser", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)")) %>%
  add_trace(data=p_lin(Sa %>% filter(JudgementType.f %in% c("Time","Speed")), "travel_laser", "fittsID_laser"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(20, 20, 20, 1)"))

fig %>% add_trace(data=Sa %>% ungroup() %>% filter(JudgementType.f %in% c("Time")) %>% group_by(fittsID_laser) %>% summarise(duration=mean(duration_ms)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 1.0)"),
                  x=~fittsID_laser, y=~duration, type='scatter',mode='markers') %>%
  add_trace(data=Sa %>% ungroup() %>% group_by(fittsID_laser), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~fittsID_laser, y=~duration_ms, type='scatter',mode='markers')

fig %>% add_trace(data=Sa %>% ungroup() %>% filter(JudgementType.f %in% c("Time")) %>% group_by(fittsID.f) %>% summarise(duration=mean(duration_ms)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~fittsID, y=~duration, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sa, opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~fittsID, y=~duration_ms, type='scatter',mode='markers')


fig %>% add_trace(data=Sa %>% ungroup() %>% filter(JudgementType.f %in% c("Time")) %>% group_by(hitnumber) %>% summarise(duration=mean(duration_ms / fittsID_laser)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~hitnumber, y=~duration, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sa %>% ungroup() %>% filter(JudgementType.f %in% c("Time")) %>% group_by(hitnumber), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~hitnumber, y=~duration_ms, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa, "duration_ms", "hitnumber"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)"))

fig %>% add_trace(data=Sa %>% ungroup() %>% filter(`Performance Metric` %in% c("Distance")) %>% group_by(hitnumber) %>% summarise(straightness=mean(straightness / fittsID_laser)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~hitnumber, y=~straightness, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sa %>% ungroup() %>% group_by(hitnumber), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~hitnumber, y=~straightness, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa, "straightness", "hitnumber"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)"))

fig %>% add_trace(data=Sa %>% ungroup() %>% group_by(hitnumber) %>% summarise(peakspeed=mean(peak_speed_smooth / fittsID_laser)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~hitnumber, y=~peakspeed, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sa %>% ungroup() %>% group_by(hitnumber), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~hitnumber, y=~peak_speed_smooth, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa, "peak_speed_smooth", "hitnumber"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)"))


fig %>% add_trace(data=Sa %>% ungroup() %>% group_by(hitnumber) %>% summarise(throughput=mean(throughput)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~hitnumber, y=~throughput, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sa %>% ungroup() %>% group_by(hitnumber), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~hitnumber, y=~throughput, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa, "throughput", "hitnumber"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)"))

fig %>% add_trace(data=Sc2 %>% filter(`Performance Metric` %in% c("Distance","Time")) %>% group_by(`Performance Feedback`) %>% summarise(peak_speed=mean(`Peak Speed (m/s)`)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~as.numeric(`Performance Feedback`), y=~peak_speed, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Speed")) %>% group_by(`Performance Feedback`) %>% summarise(peak_speed=mean(`Peak Speed (m/s)`)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~`peak_speed`, type='scatter',mode='lines') %>%
  add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Speed")) %>% group_by(`Performance Feedback`), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~`Peak Speed (m/s)`, type='scatter',mode='markers')

# 4-5% for peak speed
fig %>% add_trace(data=Sc2 %>% filter(`Performance Metric` %in% c("Distance","Time")) %>% group_by(`Performance Feedback`) %>% summarise(peak_speed=mean(`Peak Speed (m/s)`)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~as.numeric(`Performance Feedback`), y=~peak_speed, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Speed")) %>% group_by(`Performance Feedback`) %>% summarise(peak_speed=mean(`Peak Speed (m/s)`)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~`peak_speed`, type='scatter',mode='lines') %>%
        add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Speed")) %>% group_by(`Performance Feedback`), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~`Peak Speed (m/s)`, type='scatter',mode='markers')

# 0-1% for straightness
fig %>% add_trace(data=Sc2 %>% filter(`Performance Metric` %in% c("Speed","Time")) %>% group_by(`Performance Feedback`) %>% summarise(straightness=mean(`Straightness (0-1)`)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~as.numeric(`Performance Feedback`), y=~straightness, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Distance")) %>% group_by(`Performance Feedback`) %>% summarise(straightness=mean(`Straightness (0-1)`)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~straightness, type='scatter',mode='lines') %>%
  add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Distance")) %>% group_by(`Performance Feedback`), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~`Straightness (0-1)`, type='scatter',mode='markers')

# 1-2% for movement speed
fig %>% add_trace(data=Sc2 %>% filter(`Performance Metric` %in% c("Speed","Distance")) %>% group_by(`Performance Feedback`) %>% summarise(duration=mean(`Action Duration (ms)`)), opacity=0.5, marker=list(size=10,color="rgba(60,60,60, 1.0)"),
                  x=~as.numeric(`Performance Feedback`), y=~duration, type='scatter',mode='markers+lines') %>%
  add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Time")) %>% group_by(`Performance Feedback`) %>% summarise(duration=mean(`Action Duration (ms)`)), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~duration, type='scatter',mode='lines') %>%
  add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Time")) %>% group_by(`Performance Feedback`), opacity=0.5, marker=list(size=5,color="rgba(60,60,60, 0.5.0)"),
            x=~as.numeric(`Performance Feedback`), y=~`Action Duration (ms)`, type='scatter',mode='markers')

fig %>% add_trace(data=Sa, color=~Participant.f, opacity=0.5,
                  x=~PerformanceFeedback, y=~peak_speed, type='scatter',mode='markers')

fig %>% add_trace(data=Sa, color=~Participant.f, opacity=0.5,
                  x=~time_start, y=~peak_speed, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa, "peak_speed", "Timestamp"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 120, 1)"))

fig %>% add_trace(data=Sa, color=~Participant.f, opacity=0.5,
                  x=~HitOrder, y=~straightness, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa, "straightness", "HitOrder"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 120, 1)"))

fig %>% add_trace(data=Sa, color=~Participant.f, opacity=0.5,
                  x=~HitOrder, y=~duration_ms, type='scatter',mode='markers') %>%
  add_trace(data=p_lin(Sa, "duration_ms", "HitOrder"), x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(120, 120, 120, 1)"))

fig %>% add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Speed")) %>% group_by(Participant.f), opacity=0.5, marker=list(size=7,color="rgba(60,60,60, 1.0)"),
                  text=~paste(`Performance Feedback`), x=~as.numeric(`Performance Feedback`), y=~`Peak Speed (m/s)`, type='scatter',mode='markers+lines') %>%
        add_trace(data=p_lin(Sc %>% filter(`Performance Metric` %in% c("Speed")), "Peak Speed (m/s)", "Performance Feedback"), 
                  x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)"))

fig %>% add_trace(data=Sc %>% filter(`Performance Metric` %in% c("Time")) %>% group_by(Participant.f), opacity=0.5, marker=list(size=7,color="rgba(60,60,60, 1.0)"),
                  text=~paste(`Performance Feedback`), x=~as.numeric(`Performance Feedback`), y=~`Action Duration (ms)`, type='scatter',mode='markers+lines') %>%
  add_trace(data=p_lin(Sc %>% filter(`Performance Metric` %in% c("Time")), "Action Duration (ms)", "Performance Feedback"), 
            x=~x,y=~y, marker=list(size=3,color="rgba(120,120,120, 0.0)",line=list(width=1.5,color="rgba(120,120,120, 0.0)")),
            type='scatter',mode='lines+markers',color=I("rgba(255, 120, 120, 1)"))


fig %>% add_trace(data=Scm %>% filter(JudgementType.f %in% c("Speed")), opacity=0.5, marker=list(size=15,color="rgba(60,60,60, 1.0)"),
                  x=~Participant.f, y=~`Peak Speed (m/s)`, type='scatter',mode='markers') %>%
        add_trace(data=Scm %>% filter(JudgementType.f %in% c("Time","Distance")), opacity=0.5, marker=list(size=7,color="rgba(120,120,120, 1.0)"),
            x=~Participant.f, y=~`Peak Speed (m/s)`, type='scatter',mode='markers') %>%
  add_trace(data=Scm %>% group_by(Participant.f) %>% filter(JudgementType.f %in% c("Time","Distance", "Speed")), opacity=0.5, marker=list(size=5,color="rgba(120,120,120, 0.0)"),
            color=I("rgba(120, 120, 120, 1)"), x=~Participant.f, y=~`Peak Speed (m/s)`, type='scatter',mode='lines')

fig %>% add_trace(data=Scm %>% filter(JudgementType.f %in% c("Time")), opacity=0.5, marker=list(size=20,color="rgba(120,120,120, 1.0)"),
                  x=~Participant.f, y=~`Action Duration (ms)`, type='scatter',mode='markers') %>%
  add_trace(data=Scm %>% filter(JudgementType.f %in% c("Speed","Distance")), opacity=0.5, marker=list(size=10,color="rgba(120,120,120, 1.0)"),
            x=~Participant.f, y=~`Action Duration (ms)`, type='scatter',mode='markers') %>%
  add_trace(data=Scm %>% group_by(Participant.f) %>% filter(JudgementType.f %in% c("Time","Distance", "Speed")), opacity=0.5, marker=list(size=5,color="rgba(120,120,120, 0.0)"),
            color=I("rgba(120, 120, 120, 1)"), x=~Participant.f, y=~`Action Duration (ms)`, type='scatter',mode='lines')

Scm = Scm %>% group_by(Participant.f) %>% select(Participant.f,`Peak Speed (m/s)`, JudgementType.f) %>%
  filter(JudgementType.f == "Speed") %>%
  summarise(peak_speed_rel = `Peak Speed (m/s)`) %>% right_join(Scm)

Scm = Scm %>% group_by(Participant.f) %>% select(Participant.f,`Peak Speed (m/s)`, JudgementType.f) %>%
  filter(JudgementType.f == "Speed") %>%
  summarise(peak_speed_avg = `Peak Speed (m/s)`) %>% right_join(Scm)

Scm = Scm %>% group_by(Participant.f) %>% select(Participant.f,`Straightness (0-1)`, JudgementType.f) %>%
  filter(JudgementType.f == "Distance") %>%
  summarise(straightness_rel = `Straightness (0-1)`) %>% right_join(Scm)

Scm = Scm %>% group_by(Participant.f) %>% select(Participant.f,`Action Duration (ms)`, JudgementType.f) %>%
  filter(JudgementType.f == "Time") %>%
  summarise(duration_rel = `Action Duration (ms)`) %>% right_join(Scm)

fig %>% add_trace(data=Scm %>% filter(JudgementType.f %in% c("Speed")), opacity=0.5, marker=list(size=20,color="rgba(120,120,120, 1.0)"),
                  x=~Participant.f, y=~100, type='scatter',mode='markers') %>%
  add_trace(data=Scm %>% filter(JudgementType.f %in% c("Time","Distance")), opacity=0.5, marker=list(size=10,color="rgba(120,120,120, 1.0)"),
            x=~Participant.f, y=~(`Peak Speed (m/s)`/peak_speed_rel)*100, type='scatter',mode='markers')

Scm %>% mutate(improve =  straightness_rel / `Straightness (0-1)`) %>% view()
Scm %>% mutate(improve =  peak_speed_rel / `Peak Speed (m/s)`) %>% view()
Scm %>% mutate(improve =  duration_rel / `Action Duration (ms)`) %>% view()

Scm %>% mutate(improve =  peak_speed_rel / `Peak Speed (m/s)`) %>% summarise(sdimprove = sd(improve))
Scm %>% mutate(improve =  duration_rel / `Action Duration (ms)`) %>% summarise(sdimprove = sd(improve))
Scm %>% mutate(improve =  peak_speed_rel / `Peak Speed (m/s)`) %>% summarise(meanimprove = mean(improve))

###
# Dashboard prototyping
###
# Bars only
fig_b = fig %>% add_trace(data=Sa %>% head(1), y=-0.5, x =~ControllerLeaveTarget_ms, text=' ', textposition='inside',
                          insidetextanchor='middle',type = 'bar', orientation = 'h', hoverinfo='text',
                          hovertext=~paste0('Leaving Previous Target\n','Start: 0 ms', '\n','End: ', round(ControllerLeaveTarget_ms,0),'ms'),
                          marker = list(color = '#d3ebfaff', line = list(color = '#1e7bb7ff', width = 3))) %>%
  add_trace(data=Sa %>% head(1), y=-0.5, x =~time_to_peak_speed_smooth_ms, 
            text=~paste('Acceleration','\n',round(time_to_peak_speed_smooth_ms,0),'ms'), textposition='inside', hoverinfo='text',
            hovertext=~paste0('Initial Impulse Phase\n','Start: ',round(ControllerLeaveTarget_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms,0),'ms'),
            marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#1e7bb7ff', width = 3)),
            insidetextanchor='middle',type = 'bar', color=I('#000000'), orientation = 'h') %>%
  add_trace(data=Sa %>% head(1), y=-0.5, x =~peak_speed_smooth_to_target_ms, 
            text=~paste('Deceleration','\n',round(peak_speed_smooth_to_target_ms,0),'ms'), textposition='inside', hoverinfo='text',
            hovertext=~paste0('Deceleration Phase\n','Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms+peak_speed_smooth_to_target_ms,0),'ms'),
            insidetextanchor='middle',type = 'bar', orientation = 'h', 
            marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#8ad2f3ff', width = 3))) %>% 
  add_trace(data=Sa %>% head(1), y=-0.5, x =~ControllerHoverTarget_ms, hoverinfo='text',
            hovertext=~paste0('Dwell Phase\n', 'Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms+peak_speed_smooth_to_target_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms+peak_speed_smooth_to_target_ms+ControllerHoverTarget_ms,0),'ms'),
            text=~paste('Hover','\n',round(ControllerHoverTarget_ms,0),'ms'), textposition='inside', 
            insidetextanchor='middle',type = 'bar', orientation = 'h',
            marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#a4de7fff', width = 3))) %>%
  layout(showlegend=F, barmode='stack', hoverlabel=list(font=list(size=15)),
         xaxis=list(title=' ',range=~c(-50,duration_ms+50),zeroline=F,showtitle=F,mirror=F),
         yaxis=list(title=' ',dtick=NULL,ticks=NULL,zeroline=F,mirror=F, showline=F,showticklabels=F,range=c(-1.0,1.0)))

# bars + speed chart
fig_b %>% add_trace(data=Sa %>% head(1) %>% pull(speed_data) %>% as.data.frame(),
                    x=~as.numeric(difftime(t,min(t)))*1000, y=~x, type='scatter',mode='lines', hoverinfo='text',
                    hovertext=~paste0('Speed: ',round(x*100,2),'cm/s', '\n','Time: ', round(as.numeric(difftime(t,min(t)))*1000,0),'ms'),
                    line = list(color = '#8d9096ff', width = 3))

plot_action <- function(df, speed_df) {
  #browser()
# Bars only
fig_b = fig %>% add_trace(data=df, y=-0.5, x =~ControllerLeaveTarget_ms, text=' ', textposition='inside',
                  insidetextanchor='middle',type = 'bar', orientation = 'h', hoverinfo='text',
                  hovertext=~paste0('Leaving Previous Target\n','Start: 0 ms', '\n','End: ', round(ControllerLeaveTarget_ms,0),'ms'),
                  marker = list(color = '#d3ebfaff', line = list(color = '#1e7bb7ff', width = 3))) %>%
        add_trace(data=df, y=-0.5, x =~time_to_peak_speed_smooth_ms, 
                  text=~paste('Acceleration','\n',round(time_to_peak_speed_smooth_ms,0),'ms'), textposition='inside', hoverinfo='text',
                  hovertext=~paste0('Initial Impulse Phase\n','Start: ',round(ControllerLeaveTarget_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms,0),'ms'),
                  marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#1e7bb7ff', width = 3)),
                  insidetextanchor='middle',type = 'bar', color=I('#000000'), orientation = 'h') %>%
        add_trace(data=df, y=-0.5, x =~peak_speed_smooth_to_target_ms, 
                  text=~paste('Deceleration','\n',round(peak_speed_smooth_to_target_ms,0),'ms'), textposition='inside', hoverinfo='text',
                  hovertext=~paste0('Deceleration Phase\n','Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms+peak_speed_smooth_to_target_ms,0),'ms'),
                  insidetextanchor='middle',type = 'bar', orientation = 'h', 
                  marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#8ad2f3ff', width = 3))) %>% 
        add_trace(data=df, y=-0.5, x =~ControllerHoverTarget_ms, hoverinfo='text',
                  hovertext=~paste0('Dwell Phase\n', 'Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms+peak_speed_smooth_to_target_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_smooth_ms+peak_speed_smooth_to_target_ms+ControllerHoverTarget_ms,0),'ms'),
                  text=~paste('Hover','\n',round(ControllerHoverTarget_ms,0),'ms'), textposition='inside', 
                  insidetextanchor='middle',type = 'bar', orientation = 'h',
            marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#a4de7fff', width = 3))) %>%
  layout(showlegend=F, barmode='stack', hoverlabel=list(font=list(size=15)),
         xaxis=list(title=' ',range=~c(-50,duration_ms+50),zeroline=F,showtitle=F,mirror=F,ticksuffix = " ms"),
         yaxis=list(title=' ',dtick=NULL,ticks=NULL,zeroline=F,mirror=F, showline=F,showticklabels=F,range=c(-1.0,1.0)))

# bars + speed chart
fig_b %>% add_trace(data=speed_df,
            x=~time, y=~mean_speed, type='scatter',mode='lines', hoverinfo='text',
            hovertext=~paste0('Speed: ',round(mean_speed*100,2),'cm/s', '\n','Time: ', round(time,2),'ms'),
            line = list(color = '#8d9096ff', width = 3)) %>%
          add_trace(data=speed_df, x=~c(first(time),last(time)),y=~c(mean(na.omit(mean_speed)),mean(na.omit(mean_speed))),type='scatter',mode='lines',
                    line = list(color = '#8d9096ff', width = 1,dash='dash',hoverinfo='none',hovertext=' ')) %>%
  add_trace(data=speed_df, x=~c(first(time),last(time)),y=~c(0,0),type='scatter',mode='lines',
            line = list(color = '#8d9096ff', width= 1),hoverinfo='none',hovertext=' ') %>%
  layout(hoverlabel=list(bgcolor = '#eaeaeaff')) %>%
  add_annotations(ax=0,ay=-20,arrowhead=7,data=df,y=~peak_speed_smooth,x=~time_to_peak_speed_smooth_ms,text=~paste0('Peak Speed: ',round(peak_speed*100,2),'cm/s')) %>%
  add_annotations(ax=0,ay=-50,arrowhead=6,xanchor = "center",data=speed_df,y=~last(mean_speed),x=~last(time),text=~paste0('Action End: ',round(last(time),0),'ms                           ')) %>%
  add_annotations(xshift=25,yshift=15,showarrow=F,align="left",data=speed_df,y=~mean(na.omit(mean_speed)),x=~first(time),text=~paste0('Mean Speed:\n',round(mean(na.omit(mean_speed*100)),2),'cm/s'))

}

plot_action(Sa_a, mean_trajectory_df)
  
# Search Phase - approximated by when does the target leave the mole.
# Coarse Movement Acceleration Phase
# Fine Movement Deceleration Phase
# Hover Movement Activation Phase
# Corrective feedback is the last 10% of the motion


### Estimate how much time participants spent.
Sa %>% group_by(Participant, FeedbackJudge) %>% summarise(mintime = min(time_start), maxtime = max(time_end), duration = maxtime - mintime)

Sa %>% group_by(Participant) %>% summarise(mintime = min(time_start), maxtime = max(time_end), duration = maxtime - mintime) %>% summarise(dur = mean(as.numeric(duration)), sd = sd(as.numeric(duration)))
Sa %>% group_by(Participant, FeedbackJudge) %>% summarise(mintime = min(time_start), maxtime = max(time_end), duration = maxtime - mintime) %>% ungroup() %>% summarise(dur = mean(duration), sd = sd(duration))
