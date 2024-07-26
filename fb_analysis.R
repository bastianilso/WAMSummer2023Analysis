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

speed_data_list <- Sa$speed_data

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
  peak_speed = max(mean_speed,na.rm=T),
  time_to_peak_speed_total_ms = na.omit(time[mean_speed == peak_speed]),
  peak_speed_to_target_total_ms = max(time) - time_to_peak_speed_total_ms,
) %>% bind_cols(Sa_a)

# Subtract Controller hover target from peak speed to target
Sa_a = Sa_a %>% mutate(
  peak_speed_to_target_ms = peak_speed_to_target_total_ms - ControllerHoverTarget_ms,
  time_to_peak_speed_ms = time_to_peak_speed_total_ms - ControllerLeaveTarget_ms,
)

# todo: remove arbitrarily high values in ControllerHoverTarget/LeaveTarget

# Re-calculate values based on the mean speed trajectory.

#peak_speed_index = first(rowid[speed==max(speed,na.rm=T)]), # first() takes care of NAs
#time_to_peak_speed = sum(time_delta[rowid < peak_speed_index],na.rm=T),
#peak_speed_smooth = max(speed_smooth,na.rm=T),
#peak_speed_smooth_index = first(rowid[speed_smooth==max(speed_smooth,na.rm=T)]), # first() takes care of NAs
#time_to_peak_speed_smooth = sum(time_delta[rowid < peak_speed_smooth_index],na.rm=T),
#peak_speed_to_target = sum(time_delta[rowid > peak_speed_index],na.rm=T),
#peak_speed_to_target_pct = (peak_speed_to_target / duration) * 100,

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
#peak_speed = NA,
# Time to peak speed is the temporal measure taken to reach peak speed and represents the acceleration phase of the motion
#time_to_peak_speed = NA
#Percent after peak speed (%afterS max ): Percent after peak speed is the amount of time that occurs after peak speed is reached as a percentage of total
# movement time and represents the deceleration phase of the motion
#percent_after_peak_speed = NA

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
  time_to_peak_speed_ms = time_to_peak_speed * 1000,
  ControllerHoverTarget_ms = ControllerHoverTarget_ms * 1000,
  ControllerLeaveTarget_ms = ControllerLeaveTarget_ms * 1000,
  peak_speed_to_target_ms = (peak_speed_to_target * 1000) - ControllerHoverTarget_ms,
  PerformanceFeedback.f = factor(PerformanceFeedback,levels=feedback_order, ordered=T),
  JudgementType.f = factor(JudgementType,levels=judgement_order),
  )

#Sd <- Sd %>% select(distance,distanceMS, Participant,SessionProgram,MiniPatternLabel,PatternSegmentLabel)

#Sal <- Sa %>% select(Participant, HitOrder, FeedbackJudge,duration,
#                      travel_arm,travel_arm_euc,time_to_peak_speed,peak_speed,
#                      peak_speed_to_target_pct,JudgementType,PerformanceFeedback)

# Combine with Experiential Measures (Lf) and base pattern measures (Sd)

####
# Merge Sd and Sa
####
Sa = Sa %>% left_join(Sd, by=c("Participant" = "Participant", "HitOrder"))
Sa = Sa %>% rename("MoleIdStart" = "MoleIdStart.x", "MoleIdToHit" = "MoleIdToHit.x")

#Sa <- Sa %>% left_join(Sd, by=c("Participant" = "Participant", "FeedbackJudge" = "PatternSegmentLabel"))
Sa <- Sa %>% left_join(Lf)

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

Sa = Sa %>% mutate(
  JudgementOrder = str_remove(SessionProgram, "D3-3-2 Performance Feedback - "),
  fittsID.f = as.factor(fittsID),
  Participant.f = as.factor(Participant),
)




# Scfm: Summary of metric per feedback condition
Scfm <- Sa %>% group_by(FeedbackJudge) %>% 
  summarize(
    travel_arm_mean = mean(travel_arm),
    travel_arm_sd = sd(travel_arm),
    duration_mean= mean(duration),
    duration_sd= sd(duration),
    straightness_mean = mean(straightness),
    straightness_sd = sd(straightness),
    peak_speed_mean = mean(peak_speed),
    peak_speed_sd = sd(peak_speed),
    time_to_peak_speed_mean = mean(time_to_peak_speed),
    time_to_peak_speed_sd = sd(time_to_peak_speed),
    peak_speed_to_target_pct_mean = mean(time_to_peak_speed),
    peak_speed_to_target_pct_sd = sd(time_to_peak_speed),
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
Sc = Sa %>% group_by(Participant, FeedbackJudge) %>%
  dplyr::summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (sec)` = mean(duration),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed),
    `Time to Peak Speed (s)` = mean(time_to_peak_speed),
    `Peak Speed to Target (\\%)` = mean(peak_speed_to_target_pct),
    `Correspondence` = unique(AlgoCorrespondFastSlow.f),
     MiniPatternLabel = paste(unique(MiniPatternLabel),collapse=", "),
    `Performance Feedback` = unique(PerformanceFeedback.f),
    `Performance Metric` = unique(JudgementType),
    `Fitts ID` = mean(fittsID),
  ) %>% mutate(
    Participant = as.factor(Participant),
    FeedbackJudge = as.factor(FeedbackJudge)
  )

# Scf: Summary of Condition-Feedback
Scf = Sa %>% group_by(Participant, PerformanceFeedback.f) %>%
  dplyr::summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (sec)` = mean(duration),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed),
    `Time to Peak Speed (s)` = mean(time_to_peak_speed),
    `Peak Speed to Target (\\%)` = mean(peak_speed_to_target_pct),
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
  ) %>% mutate(
    Participant = as.factor(Participant),
    `Performance Feedback` = as.factor(`Performance Feedback`)
  )


# Sp: Summary of Participants

Sp = Sa %>% group_by(Participant) %>%
  summarise(
    `Actions (n)` = max(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (sec)` = mean(duration),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed),
    `Time to Peak Speed (s)` = mean(time_to_peak_speed),
    `Peak Speed to Target (\\%)` = mean(peak_speed_to_target_pct),
  )

Sf = Sa %>% group_by(Participant, FeedbackJudge) %>%
  summarise(
    `Actions (n)` = max(HitOrder) - min(HitOrder),
    `Actions Analyzed (n)` = length(HitOrder)-1,
    `Action Duration (sec)` = mean(duration),
    `Action Arm Travel (meter)` = mean(travel_arm),
    `Total Arm Travel (meter)` = sum(travel_arm),
    `Euc. Arm Travel (meter)` = sum(travel_arm_euc),
    `Straightness (0-1)` = mean(straightness),
    `Peak Speed (m/s)` = mean(peak_speed),
    `Time to Peak Speed (s)` = mean(time_to_peak_speed),
    `Peak Speed to Target (\\%)` = mean(peak_speed_to_target_pct),
    `Fitts ID` = mean(fittsID),
    `Feedback` = unique(PerformanceFeedback),
    `JudgementType` = unique(JudgementType),
  ) %>% filter(FeedbackJudge != "NoneNone")

# Sp: Summary of Participant standard deviation
Sp_sd = Sa %>% group_by(Participant) %>%
  summarise(
    `Actions (n)` = NA,
    `Actions Analyzed (n)` = NA,
    `Action Duration (sec)` = sd(duration),
    `Action Arm Travel (meter)` = sd(travel_arm),
    `Total Arm Travel (meter)` = NA,
    `Euc. Arm Travel (meter)` = NA,
    `Straightness (0-1)` = sd(straightness),
    `Peak Speed (m/s)` = sd(peak_speed),
    `Time to Peak Speed (s)` = sd(time_to_peak_speed),
    `Peak Speed to Target (\\%)` = sd(peak_speed_to_target_pct),
  )


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

return(list(s1,s2,s3,s4,s5))
}

#condition-wise: correspondence
tables_friedman(Sc, "Correspondence","FeedbackJudge","Participant")

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
# Simple mode: Repeated Measures Multivariate Anova
###
# The repeated-measures part does not refer to that we took multiple measures of actions, but instead to that
# we measured participants multiple times throughout the experiment.
# tables_rmanova is unaware of possible differences in FittsID between conditions and unaware of relationships
# between the different dependent variables measured.
# Simple Two-way ANOVA
tables_rmanova <- function(df, plabel, measlabel,treatlabel1, treatlabel2) {
  dataset = data.frame(measurement = df[[measlabel]],
                       treat1 = df[[treatlabel1]],
                       treat2 = df[[treatlabel2]],
                       participant = df[[plabel]])
  
  aov_result <- aov_ez(id="participant", dv="measurement", dataset, within = c("treat1","treat2"))
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

aov_ez(id="Participant.f", dv="duration_ms", within = c("PerformanceFeedback","JudgementType"), data=Sa)

tables_rmanova(Sa, "Participant.f", "duration_ms","PerformanceFeedback", "JudgementType") 
tables_rmanova(Sa, "Participant.f", "travel_arm","PerformanceFeedback", "JudgementType") 
tables_rmanova(Sa, "Participant.f", "peak_speed","PerformanceFeedback", "JudgementType") 
tables_rmanova(Sa, "Participant.f", "peak_speed_to_target_pct","PerformanceFeedback", "JudgementType")
tables_rmanova(Sa, "Participant.f", "time_to_peak_speed_ms","PerformanceFeedback", "JudgementType")
tables_rmanova(Sa, "Participant.f", "straightness_pct","PerformanceFeedback", "JudgementType")
tables_rmanova(Sa, "Participant.f", "fittsID","PerformanceFeedback", "JudgementType")



fit <- multRM(cbind(travel_arm, duration_ms, peak_speed, peak_speed_to_target_pct, time_to_peak_speed_ms) ~ PerformanceFeedback.f * JudgementType.f *  order, data = Sa_test,
              subject = "Participant.f", within = c("PerformanceFeedback.f","JudgementType.f","order"))
summary(fit)



Sa_test = Sa %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f,fittsID.f) %>% 
  summarise(across(all_of(c("travel_arm", "duration_ms", "peak_speed", "peak_speed_to_target_pct", "time_to_peak_speed_ms")), ~ mean(.x)))


Sa_test = Sa %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f) %>% dplyr::slice(1:min(19, n()))
Sa_test = Sa_test %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f) %>% mutate(order = 1, order = cumsum(order))

fittsIDs = unique(round(Sa$fittsID,2))

missingIDs = Sa %>% group_by(Participant.f, PerformanceFeedback.f,JudgementType.f) %>% 
  summarise(n(), missingIDs = paste0(setdiff(fittsIDs, round(fittsID,2)))) %>% 
  ungroup() %>% summarise(unique(missingIDs))

Sa_test = Sa %>% filter(round(fittsID,2) %in% missingIDs)



tables_rmanova(Sc,"Participant", "Action Arm Travel (meter)", "FeedbackJudge")

tables_rmanova(Sc,"Participant", "Action Arm Travel (meter)", Sc, within = "FeedbackJudge")

manova_result <- manova(cbind(travel_arm, duration_ms, peak_speed, peak_speed_to_target_pct, time_to_peak_speed_ms) ~ PerformanceFeedback.f + JudgementType.f + Error(PerformanceFeedback.f/JudgementType.f/fittsID), data = Sa %>% filter(fittsID.f != 0))
summary(manova_result)

Sa = Sa %>% filter(fittsID != 0)

manova_result <- manova(cbind(`Action Arm Travel (meter)`, `Action Duration (sec)`, `Peak Speed (m/s)`) ~ 
                          as.factor(Feedback) + as.factor(JudgementType) + 
                          Error(as.factor(Feedback)/as.factor(JudgementType)/`Fitts ID`), 
                        data = Sf %>% filter(`Fitts ID` != 0))

anova_travel_arm <- aov(peak_speed ~ PerformanceFeedback.f + JudgementType.f + Error(PerformanceFeedback.f/JudgementType.f/fittsID), data = Sa %>% filter(fittsID.f != 0) %>% as.data.frame(.))

posthoc_travel_arm <- emmeans(anova_travel_arm, ~ PerformanceFeedback.f + JudgementType.f)
pairwise_travel_arm <- pairs(posthoc_travel_arm)

pairwise_travel_arm <- pairs(posthoc_t)

summary(anova_travel_arm)

# Sa %>% select(travel_arm, duration_ms, peak_speed, peak_speed_to_target_pct, time_to_peak_speed_ms, PerformanceFeedback.f,fittsID,JudgementType) %>% view()

independent_var <- Sa$FeedbackJudge.f

manova_model <- manova(dependent_vars ~ PerformanceFeedback.f * JudgementType.f, data = Sa)
summary(manova_model)



manova_model <- lm(cbind(travel_arm, duration_ms, peak_speed, straightness_pct, peak_speed_to_target_pct, time_to_peak_speed_ms) ~ FeedbackJudge , data = Sa %>% filter(fittsID.f ))
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
#travel_arm, duration_ms, peak_speed, peak_speed_to_target_pct, time_to_peak_speed_ms, PerformanceFeedback.f,fittsID,JudgementType
lmes = list(predictors = c("travel_arm", "duration_ms", "peak_speed","peak_speed_to_target","time_to_peak_speed_ms"),
            random = c("Gender"),
            fixed = c("fittsID.f","JudgementType.f", "PerformanceFeedback.f","PlayOrder"),
            null = c("Participant.f"),
            threshold = 0.05,
            df = Sa)

table = g_lme_table(lmes)


lme_table <- table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         across(everything(), ~ str_replace_all(.x, c("JudgementType.f" = "Performance Metric",
                                                      "PerformanceFeedback" = "Performance Feedback",
                                                      "travel_arm" = "Arm Travel",
                                                      "duration_ms" = "Duration (ms)",
                                                      "peak_speed_to_target" = "Peak Speed To Target",
                                                      "time_to_peak_speed_ms" = "Time To Peak Speed (ms)",
                                                      "peak_speed" = "Peak Speed (m/s)")))
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
fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~time_to_peak_speed, type='box')
fig %>% add_trace(data=Sa, x=~Participant, y=~, type='box')
fig %>% add_trace(data=Sa, x=~Participant,color=~PerformanceFeedback,y=~peak_speed, type='box', color=I("rgba(50, 50, 50, 1)"))


plot_bar <- function(dataset, xlabel, ylabel, desc,xtitle,ytitle,miny,maxy) {
  #browser()
  df = data.frame(x = dataset[[xlabel]],
                  y = as.numeric(dataset[[ylabel]]))

  # Simple SD
  #df = df %>% group_by(x) %>% summarise(mean = mean(y), sd=sd(y))
  
  # 95% Confidence intervals
  df = df %>%
    group_by(x) %>%
    summarise(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE),
              n.y = n()) %>%
    mutate(se.y = sd.y / sqrt(n.y),
           lower.ci.y = mean.y - qt(1 - (0.05 / 2), n.y - 1) * se.y,
           upper.ci.y = mean.y + qt(1 - (0.05 / 2), n.y - 1) * se.y)
  
  fig_c <- fig %>%
    add_trace(data=df,
              x=~x, y=~mean.y, color=I('darkgrey'),
              error_y=~list(symmetric=FALSE, array=upper.ci.y - mean.y, arrayminus=mean.y-lower.ci.y, color = '#000000'), type='bar') %>%
    layout(margin=list(l=55,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                     text=desc), showlegend=F,
           xaxis=list(range=c(-0.45,~length(unique(x)) - 0.45), title=xtitle, zeroline=F, tickfont=list(size=15)),
           yaxis=list(range=c(miny,maxy), title=ytitle, zeroline=F, tickfont=list(size=15), showticklabels=T))
  
  return(fig_c)
}

plot_violin <- function(dataset, xlabel, ylabel,plabel, desc,xtitle,ytitle,miny,maxy,minx = NA,maxx = NA,bwidth,jit) {
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
    layout(margin=list(l=55,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                    text=desc), showlegend=F,
           xaxis=list(range=c(minx,maxx), title=xtitle, zeroline=F, tickfont=list(size=15)),
           yaxis=list(range=c(miny,maxy), title=ytitle, zeroline=F, tickfont=list(size=15), showticklabels=T))
  
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
                           maxy = row$maxy)
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
                           jit = row$jitter)
  orca(figs[[i]], paste('fig/violin_error',row$annotation,row$y,'.pdf',sep="_"), width=row$width, height=row$height)
}


fig_c <- fig %>%
  add_trace(data=Sa %>% group_by(Participant,Age) %>% summarise(duration_ms = median(duration_ms)), x=~Age, 
            y=~jitter(duration_ms,amount=2),
            marker=list(size=3,color="rgba(120,120,120, 0.80)",line=list(width=1.5,color="rgba(120,120,120, 0.15)")),
            type='scatter',mode='marker',color=I("rgba(120, 120, 120, 1)")) %>%
  layout(margin=list(l=55,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                   text='Age vs duration'), showlegend=F,
         xaxis=list(range=c(21,42), title=' ', zeroline=F, tickfont=list(size=15)),
         yaxis=list(range=c(1000,1600), title=' ', zeroline=F, tickfont=list(size=15), showticklabels=T))
orca(fig_c, "fig/control_age_vs_duration.pdf", width=325, height=355)

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

#############
# Latex Table: Participants
#############

# g0 = min, g1 = 1st Qu, g2 = median, g3 = 3rd Qu, g4 = Max.

cri = tibble(`Action Duration (sec)` = rev(c(1.0, 1.25,1.353,1.45,1.6,1.8,5.2)), # 1.353 is the median
             `Participant` = c(30,100,100,100,100,100,100), # always g0
             `Actions (n)` = c(80,100,210,300,300,300,300), # always g0
             `Actions Analyzed (n)` = c(30,100,210,300,300,300,300), # always g0
             `Action Arm Travel (meter)` = c(0.15,0.25,0.3,0.35,0.4,0.45,0.5), # 0.2839 is the median
             `Total Arm Travel (meter)` = c(45,50,55,60,65,70,75), # 55 is the median
             `Euc. Arm Travel (meter)` =  c(35,40,45,50,55,60,65), #46 median
             `Straightness (0-1)` =  c(0.6,0.7,0.8,0.9,1.0,1.1,1.2), # 0.82 median
             `Peak Speed (m/s)` =  c(0.5,1.0,1.5,2.0,2.5,3.0,3.5), # 1.41 median
             `Time to Peak Speed (s)` =  rev(c(0.4,0.475,0.55,0.625,0.7,0.775,0.85)),
             `Peak Speed to Target (\\%)` =  c(0.45,0.55,0.60,0.65,0.70,0.75,0.80),
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
#Sp_table <- Sp_table %>% mutate(across(all_of(Sp_perccols), ~ round(.x * 100)))
#Sp_table <- Sp_table %>% mutate(across(all_of(Sp_perccols), ~ paste0(as.character(.x),"\\%")))

# Add colors to numbers
Sp_table <- Sp_table %>% mutate(across(all_of(Sp_col), ~ paste0("\\cellcolor{", get(paste0(cur_column(), "_c")), "}", as.character(.x))))

# Add standard deviation to numbers
#Sp_table <- Sp_table %>% mutate(across(all_of(Sp_sdcols), ~ ifelse(cur_column() %in% Sp_sdcols,
#                                                                      paste0(.x, "(", Spsd_table[[cur_column()]],")"),
#                                                                      .x)))
# Cleanup color cols and format all as strings
Sp_table <- Sp_table %>% select(-ends_with("_c")) %>%
  mutate(across(everything(), as.character))

# Pivot
Sp_table <- Sp_table %>% pivot_longer(cols=-c(Participant,FeedbackJudge), names_to = "Variable")  %>%
  pivot_wider(names_from = Participant, values_from = value)

#Sp_table <- Sp_table %>% group_by(Variable) %>%
#  group_modify(~ add_row(`1`=paste("\\multicolumn{5}{l}{ \\underline{",.y,"} }"),.before=0, .x)) %>%
#  ungroup() %>% replace(is.na(.)," ") %>%
#  select(-Variable)

condition_order = c("OperationTime","OperationMaxSpeed","OperationDistance","ActionTime","ActionMaxSpeed","ActionDistance","TaskTime","TaskMaxSpeed","TaskDistance")
Sp_table$FeedbackJudge = factor(Sp_table$FeedbackJudge,levels=c("All",condition_order))

Sp_table <- Sp_table %>% group_by(FeedbackJudge) %>%
 group_modify(~ add_row(`Variable`=paste("\\multicolumn{5}{l}{ \\underline{", levels(Sp_table$FeedbackJudge)[.y$FeedbackJudge],"} }"),.before=0, .x)) %>%
 ungroup() %>% replace(is.na(.)," ") %>% 
 select(-FeedbackJudge)

# Export
paste(colnames(Sp_table), collapse=" & ")
writeLines(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")


###
# Dashboard prototyping
###
# Bars only
fig_b = fig %>% add_trace(data=Sa %>% head(1), y=-0.5, x =~ControllerLeaveTarget_ms, text=' ', textposition='inside',
                          insidetextanchor='middle',type = 'bar', orientation = 'h', hoverinfo='text',
                          hovertext=~paste0('Leaving Previous Target\n','Start: 0 ms', '\n','End: ', round(ControllerLeaveTarget_ms,0),'ms'),
                          marker = list(color = '#d3ebfaff', line = list(color = '#1e7bb7ff', width = 3))) %>%
  add_trace(data=Sa %>% head(1), y=-0.5, x =~time_to_peak_speed_ms, 
            text=~paste('Acceleration','\n',round(time_to_peak_speed_ms,0),'ms'), textposition='inside', hoverinfo='text',
            hovertext=~paste0('Initial Impulse Phase\n','Start: ',round(ControllerLeaveTarget_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms,0),'ms'),
            marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#1e7bb7ff', width = 3)),
            insidetextanchor='middle',type = 'bar', color=I('#000000'), orientation = 'h') %>%
  add_trace(data=Sa %>% head(1), y=-0.5, x =~peak_speed_to_target_ms, 
            text=~paste('Deceleration','\n',round(peak_speed_to_target_ms,0),'ms'), textposition='inside', hoverinfo='text',
            hovertext=~paste0('Deceleration Phase\n','Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms,0),'ms'),
            insidetextanchor='middle',type = 'bar', orientation = 'h', 
            marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#8ad2f3ff', width = 3))) %>% 
  add_trace(data=Sa %>% head(1), y=-0.5, x =~ControllerHoverTarget_ms, hoverinfo='text',
            hovertext=~paste0('Dwell Phase\n', 'Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms+ControllerHoverTarget_ms,0),'ms'),
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
        add_trace(data=df, y=-0.5, x =~time_to_peak_speed_ms, 
                  text=~paste('Acceleration','\n',round(time_to_peak_speed_ms,0),'ms'), textposition='inside', hoverinfo='text',
                  hovertext=~paste0('Initial Impulse Phase\n','Start: ',round(ControllerLeaveTarget_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms,0),'ms'),
                  marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#1e7bb7ff', width = 3)),
                  insidetextanchor='middle',type = 'bar', color=I('#000000'), orientation = 'h') %>%
        add_trace(data=df, y=-0.5, x =~peak_speed_to_target_ms, 
                  text=~paste('Deceleration','\n',round(peak_speed_to_target_ms,0),'ms'), textposition='inside', hoverinfo='text',
                  hovertext=~paste0('Deceleration Phase\n','Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms,0),'ms'),
                  insidetextanchor='middle',type = 'bar', orientation = 'h', 
                  marker = list(color = 'rgba(255, 255, 255, 0.6)', line = list(color = '#8ad2f3ff', width = 3))) %>% 
        add_trace(data=df, y=-0.5, x =~ControllerHoverTarget_ms, hoverinfo='text',
                  hovertext=~paste0('Dwell Phase\n', 'Start: ',round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms,0), 'ms\n','End: ', round(ControllerLeaveTarget_ms+time_to_peak_speed_ms+peak_speed_to_target_ms+ControllerHoverTarget_ms,0),'ms'),
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
            line = list(color = '#8d9096ff', width f= 1),hoverinfo='none',hovertext=' ') %>%
  layout(hoverlabel=list(bgcolor = '#eaeaeaff')) %>%
  add_annotations(ax=0,ay=-20,arrowhead=7,data=df,y=~peak_speed,x=~time_to_peak_speed_ms,text=~paste0('Peak Speed: ',round(peak_speed*100,2),'cm/s')) %>%
  add_annotations(ax=0,ay=-50,arrowhead=6,xanchor = "center",data=speed_df,y=~last(mean_speed),x=~last(time),text=~paste0('Action End: ',round(last(time),0),'ms                           ')) %>%
  add_annotations(xshift=25,yshift=15,showarrow=F,align="left",data=speed_df,y=~mean(na.omit(mean_speed)),x=~first(time),text=~paste0('Mean Speed:\n',round(mean(na.omit(mean_speed*100)),2),'cm/s'))

}

plot_action(Sa_a, mean_trajectory_df)
  
# Search Phase - approximated by when does the target leave the mole.
# Coarse Movement Acceleration Phase
# Fine Movement Deceleration Phase
# Hover Movement Activation Phase
# Corrective feedback is the last 10% of the motion