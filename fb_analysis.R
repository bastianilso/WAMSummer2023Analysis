library(plotly)  # load plotly first to avoid it overriding mutate!
library(sf)
library(tidyverse)


#load('data_feedback.rda')
load('data_all_actions.rda')
source("utils/visutils.R")
fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

###
# Create Aggregate Summaries
###

# Calculate travel_euc
Sa <- Sa %>% rowwise() %>% mutate(
  travel_arm_euc = st_length(
    st_cast(
      st_line_sample(
        st_linestring(data.matrix(data.frame(RightControllerPosWorld))),
        sample = c(0,1)),
      "LINESTRING")
  )
)

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

condition_order = c("OperationTime","OperationMaxSpeed","OperationDistance","ActionTime","ActionMaxSpeed","ActionDistance","TaskTime","TaskMaxSpeed","TaskDistance")
condition_order_short = c("O-T","O-S","O-D","A-T","A-S","A-D","T-T","T-S","T-D")
names(condition_order_short) <- condition_order
feedback_order = c("Operation","Action","Task")
judgement_order = c("Time","MaxSpeed","Distance")

Sa = Sa %>% filter(FeedbackJudge != "NoneNone") %>%  mutate(
  straightness = travel_arm_euc / travel_arm,
  FeedbackJudge.f = factor(FeedbackJudge,levels=condition_order),
  FeedbackJudge.fs = FeedbackJudge,
  FeedbackJudge.fs = str_replace_all(FeedbackJudge.fs, condition_order_short),
  FeedbackJudge.fs = factor(FeedbackJudge.fs,levels=unname(condition_order_short)),
  time_to_peak_speed_ms = time_to_peak_speed * 1000,
  duration_ms = duration * 1000,
  PerformanceFeedback.f = factor(PerformanceFeedback,levels=feedback_order),
  JudgementType.f = factor(JudgementType,levels=judgement_order),
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

fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~duration, type='scatter')
fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~straightness, type='box')
fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~travel_arm, type='box')
fig %>% add_trace(data=Sa, x=~FeedbackJudge, y=~time_to_peak_speed, type='box')
fig %>% add_trace(data=Sa, x=~Participant, y=~, type='box')
orca(fig_c, "fig/condition_feedbackSenseDiff_violin.pdf", width=325, height=355)

Sa %>% group_by(FeedbackJudge) %>% summarise(feedbackJudge_sd = sd())

plot_bar <- function(dataset, xlabel, ylabel, desc,xtitle,ytitle,miny,maxy) {
 #browser()
  df = data.frame(x = dataset[[xlabel]],
                  y = dataset[[ylabel]])
  
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

# Movement Time (ms)
fig_c <- plot_bar(Sa, "FeedbackJudge.fs","duration_ms","Movement Time per Task Condition", 
         " ","Movement Time (ms)",1200,1600)
fig_c
orca(fig_c, "fig/bar_error_condition_duration.pdf", width=425, height=425)

# Straightness
fig_c <- plot_bar(Sa, "FeedbackJudge.fs","straightness","Straightness per Task Condition", 
                  " ","Straightness(0-1)",0.75,1)
fig_c
orca(fig_c, "fig/bar_error_condition_straightness.pdf", width=425, height=425)

# Peak Velocity
fig_c <- plot_bar(Sa, "FeedbackJudge.fs","peak_speed","Peak Speed per Task Condition", 
                  " ","Peak Speed (m/s)",0.75,2.5)
fig_c
orca(fig_c, "fig/bar_error_condition_peakspeed.pdf", width=425, height=425)
# Todo: is Peak velocity negative ??

# % After Peak Speed
fig_c <- plot_bar(Sa, "FeedbackJudge.fs","peak_speed_to_target_pct","% After Peak Speed per Task Condition", 
                  " ","% After Peak Speed",0.5,0.65)
fig_c
orca(fig_c, "fig/bar_error_condition_pct_after_peak_speed.pdf", width=425, height=425)
# Todo: Re-run vr_preprocess to get proper pct

# Time to Peak Speed
fig_c <- plot_bar(Sa, "FeedbackJudge.fs","time_to_peak_speed_ms","Time to Peak Speed per Task Condition", 
                  " ","Time to Peak Speed (ms)",400,700)
fig_c
orca(fig_c, "fig/bar_error_condition_time_to_peak_speed.pdf", width=425, height=425)


# Miniplot per feedback type
fig_bars = read.csv("wam_plot_bars.csv", sep=";")
fig_bars$dataset = list(Sa)

lapply(Sa,plot_bar,xlabel=fig_bars$x, ylabel=fig_bars$y, desc=fig_bars$desc,xtitle,ytitle,miny,maxy)

# Movement Time
fig_c <- plot_bar(Sa, "PerformanceFeedback.f","duration_ms","Movement Time (ms)", 
                  " "," ",1200,1600)
fig_c
orca(fig_c, "fig/bar_error_feedback_duration.pdf", width=265, height=265)



###
# Latex Table of Participants
###

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
# Save Aggregate Summaries
###