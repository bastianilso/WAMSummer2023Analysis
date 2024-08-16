library(tidyverse)
library(plotly)
library(sf)
library(gsheet)
library(interp)
source("utils/loadrawdata.R")
options("digits.secs"=6)

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

# Load All data from the following directory

# Load all metadata
M <- LoadFromDirectory("data_20231127", event = NULL, sample = NULL)


####
# Use Meta-data to aggregate each dataset
####

M = M %>% rename(Participant = i1,
                 Session = i2)

M = M %>% mutate(Participant = as.numeric(Participant))

# Filter out participant 0, who is just a test round.
excluded_participants = c(0)
M = M %>% filter(!Participant %in% excluded_participants)
M = M %>% filter(Session == "feedback")

M = M %>% mutate(i = paste0("data_20231127","/",Participant,"/",Session))

# Summary of actions
Sa = NULL
# Summary of treatment programs
St = NULL

# Summary of distances in each pattern for each participant
Sd = NULL

count = 0



debug_flag = F
#M = M %>% filter(Participant %in% c(7))
# We should log when people manage to identify the target, e.g. their eyes look at the right target.
# we should also log at the very least, a direct laser coming from the persons head orientation onto the board, so get an approximation of where the person is looking.
# Pointer Hover Begin/End seems to be swapped around and ControllerHover is sometimes reporting the wrong MoleIds ??
# ControllerHover problems seem to be case-by-case dependent, sometimes it is fine, other times its problematic.
for (folderpath in M$i) {
  
  D = LoadFromDirectory(folderpath)
  print(nrow(D))
  count = sum(count, nrow(D))
  
  ####
  # Format columns
  #### 
  D = D %>% rename(Participant = i1,
                   Session = i2)
  
  
  col_formats = read.csv("wam_column.csv", sep=";")
  col_formats = col_formats %>% filter(name %in% colnames(D))
  
  D = D %>% 
    mutate_at(col_formats %>% pull(name), 
              ~ifelse(.x == "NULL", NA, .x)) %>%
    mutate_at(col_formats %>% filter(type=="numeric") %>% pull(name), 
              ~as.numeric(.x)) %>%
    mutate_at(col_formats %>% filter(type=="int") %>% pull(name), 
              ~as.integer(.x)) %>%
    mutate_at(col_formats %>% filter(type=="time") %>% pull(name), 
              ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%OS")) 
  
  # Ensure D is arranged by Timestamp
  D = D %>% arrange(Timestamp)
  
  # Ensure tracking columns are not NA in-game.
  tracking_cols = colnames(D %>% select(contains("LaserPosWorld"), contains("RotEuler"),contains("Viewport")))
  D = D %>% tidyr::fill(all_of(tracking_cols), .direction = c("downup"))
  
  # At some point 
  # we introduce SessionProgram as both an Event column and a Meta column,
  # but as a result we get SessionProgram.x and SessionProgram.y
  # Same for ParticipantID, but we dont use that.
  # todo: add to github issue.
  
  if ( !("SessionProgram" %in% colnames(D)) ) {
    D = D %>% mutate(SessionProgram = SessionProgram.y)
  }
  
  # Ensure MotorSpace related column information is available throughout the dataset
  motorspace_cols = colnames(D %>% select(contains("MotorSpace")))
  D = D %>% tidyr::fill(all_of(motorspace_cols), .direction = c("down"))
  
  ####
  # Add Convenience Columns
  ####

  # Calculate time_delta for each row to estimate sample rate.
  # the first part ensures that Sample events' timestamps are considered next to each other.
  # the second part overrides the non-sample events with NA values.
  # Event columns are NA in their time_delta.
  D = D %>% group_by(Event) %>% arrange(Timestamp) %>%
    mutate(time_delta = Timestamp - lag(Timestamp)) %>%
    ungroup() %>% 
    mutate(time_delta = case_when(Event == "Sample" ~ time_delta))
  
  # Create unique ID for each row index. Will be used for filter-join operations.
  # rowindex is guaranteed to follow chronological progression of time.
  D = D %>% tibble::rownames_to_column("rowindex")
  
  # Create a "PlayPeriod" column to indicate: "PreGame", "Game" and "PostGame".
  D = D %>% arrange(Timestamp) %>%
    mutate(indication = ifelse(Event == "Game Started", 1, 0),
           indication = ifelse(Event %in% c("Game Finished","Game Stopped"), 1, indication),
           indication = cumsum(indication),
           indication = ifelse(Event %in% c("Game Finished","Game Stopped"), 1, indication),
           PlayPeriod = ifelse(indication < 1, "PreGame", "Game"),
           PlayPeriod = ifelse(indication > 1, "PostGame", PlayPeriod),
           indication = NULL)
  browser()
  # Fill MoleSpawnOrder and PointerShootOrder during the PlayPeriod.
  # ShootOrder reports number until
  #MoleOrder = ifelse(Event == "Mole Spawned", 1,0),
  #MoleOrder = cumsum(MoleOrder),
  #MoleHitOrder = ifelse(Event %in% c("Mole Hit", "Mole Expired"),1,0),
  #MoleHitOrder = lag(cumsum(MoleHitOrder)),
  #ActionOrder = ifelse(MoleOrder == MoleHitOrder, NA, MoleOrder),
  #ActionOrder = ifelse(PlayPeriod %in% c("PreGame","PostGame"), NA, ActionOrder),
  D = D %>% arrange(Timestamp) %>%
    mutate(ActionOrder = ifelse(Event == "Pointer Shoot", 1, 0),
           ActionOrder = lag(ActionOrder),
           MoleOrder = ifelse(Event == "Mole Spawned", 1,0),
           MoleOrder = cumsum(MoleOrder),
           MoleOrder = ifelse(Event == "Mole Spawned", MoleOrder, NA),
           ActionOrder = case_when(MoleOrder == 1 ~ 1, TRUE ~ ActionOrder),
           ActionOrder = ifelse(is.na(ActionOrder),0,ActionOrder),
           ActionOrder = cumsum(ActionOrder),
           ActionOrder = ifelse(PlayPeriod %in% c("PreGame","PostGame"), NA, ActionOrder),
           PerformanceFeedback = ifelse(grepl("Performance Feedback Set", Event), Event, NA),
           PerformanceFeedback = str_remove(PerformanceFeedback, "Performance Feedback Set "),
           )
  
  # 'GameStarted' event might contain 'CalibrationPoint' during which is wrong.
  # clear any of that.
  # We generally need to clear CalibrationPoint for all events except "Pointer Shoot" and "Mole Spawned"
  D = D %>% mutate(PatternSegmentLabel = ifelse(Event == "Game Started", NA, PatternSegmentLabel),
                   PatternSegmentLabel = ifelse(PlayPeriod == "PreGame", NA, PatternSegmentLabel))
  
  D = D %>% mutate(bad_labels = PatternSegmentLabel == "CalibrationPoint" & Event != "Mole Spawned",
                   PatternSegmentLabel = ifelse(bad_labels, NA, PatternSegmentLabel))
  
  # experiment: see whether separating calibration point into its own column and using the patternsegmentlabel
  # helps the distance equivation.
  D = D %>% mutate(CalibrationPoint = FALSE,
                   CalibrationPoint = case_when(PatternSegmentLabel == "CalibrationPoint" ~ TRUE),
                   PatternSegmentLabel = case_when(PatternSegmentLabel != "CalibrationPoint" ~ PatternSegmentLabel))
  # D %>% filter(Event != "Sample") %>% select(PatternSegmentLabel, CalibrationPoint, Event) %>% view()
  
  # Use 'CountDown 3' to choose an end-point for the segment, removing movement during countdown/game pause.
  #D = D %>% mutate(PatternSegmentLabel = ifelse(Event == "CountDown 3", "None", PatternSegmentLabel))
  # Ensure all cols has labels from JudgementType and PerformanceFeedback 
  # only do it downwards - warmup has no judgement/fb
  label_cols = c("JudgementType", "PerformanceFeedback", "PatternSegmentLabel","MoleSize")
  D = D %>% tidyr::fill(all_of(label_cols), .direction = c("down"))
  
  # Replace NA with None
  D = D %>% replace_na(list(JudgementType = "None", PerformanceFeedback = "None", PatternSegmentLabel = "None"))

  # Create a new column JudgementOrder
  D = D %>% mutate(
    JudgementOrder = str_remove(SessionProgram, "D3-3-2 Performance Feedback - ")
  )
  
  ####
  # Determine true size of Motorspace and Wall
  ####
  
  # Use "CountDown 0" to detect state of game before it begins.
  W = D %>% filter(Event == "CountDown 0") %>% head(1) %>%
    summarize(x = c(unique(WallBoundsXMin),unique(WallBoundsXMin), unique(WallBoundsXMax), unique(WallBoundsXMax),unique(WallBoundsXMin)),
              y = c(unique(WallBoundsYMin),unique(WallBoundsYMax), unique(WallBoundsYMax), unique(WallBoundsYMin),unique(WallBoundsYMin)))
  MS = D %>% filter(Event == "MotorSpace Size Update", MotorSpaceName=="MotorSpaceR",PatternSegmentLabel=="Warmup") %>%
    summarize(width = last(MotorSpaceWidth),
              height = last (MotorSpaceHeight),
              x = last(MotorSpaceCenterPositionX),
              y = last(MotorSpaceCenterPositionY),
              x0 = x - (width), # width is actually just half of the width, not full width.
              y0 = y - (height),  #height is just half of actual height, not full height.
              x1 = x + (width),
              y1 = y + (height),
              gainx = last(MotorSpaceGainX),
              gainy = last(MotorSpaceGainY),
              wx0 = x0 * gainx,
    )
  WS = D %>% filter(Event == "CountDown 0") %>% head(1) %>%
    summarize(x0 = last(WallBoundsXMin),
              y0 = last(WallBoundsYMin),
              x1 = last(WallBoundsXMax),
              y1 = last(WallBoundsXMax),
              width = last(WallBoundsXMax) -last(WallBoundsXMin),
              height = last(WallBoundsYMax) -last(WallBoundsYMin))
  
  # Convert Mole locations to equivalent locations in motorspace
  D = D %>% mutate(
    MolePositionMSX = scales::rescale(MolePositionWorldX, 
                                      from=c(first(WS$x0),last(WS$x1)), to=c(MS$x0,MS$x1)),
    MolePositionMSY = scales::rescale(MolePositionWorldY, 
                                      from=c(first(WS$y0),last(WS$y1)), to=c(MS$y0,MS$y1))
    )
  
  ####
  # HitOrder: Determine what constitutes each player action
  ####
  D = D %>% arrange(Timestamp) %>%
    dplyr::mutate(HitOrder = ifelse(Event %in% c("Mole Hit","Mole Expired"), 1, 0),
                  HitOrder = lag(HitOrder),
                  MoleOrder = ifelse(Event == "Mole Spawned", 1,0),
                  MoleOrder = cumsum(MoleOrder),
                  MoleOrder = ifelse(Event == "Mole Spawned", MoleOrder, NA),
                  HitOrder = case_when(MoleOrder == 1 ~ 1, TRUE ~ HitOrder),
                  HitOrder = ifelse(is.na(HitOrder),0,HitOrder),
                  HitOrder = cumsum(HitOrder),
                  HitOrder = ifelse(Event == "Mole Spawned", MoleOrder, HitOrder),
                  HitOrder = ifelse(PlayPeriod %in% c("PreGame","PostGame"), NA, HitOrder),
    )
  
  
  # flag the first action (mole to hit) after each break, so they can be filtered out.
  # important to do before we filter out cols coming before mole spawned, otherwise
  # they cant be identified.
  D = D %>% group_by(HitOrder) %>% dplyr::mutate(
    flag = any(Event %in% c("CountDown 0")),
    HitOrder = ifelse(flag, NA, HitOrder),
  ) %>% select(-flag)
  
  # filter out cols that come before Mole Spawned
  D = D %>% group_by(HitOrder) %>% dplyr::mutate(
    flag = ifelse(Event %in% c("Mole Spawned"), 1, 0),
    hit_flag = cumsum(flag),
    HitOrder = ifelse(hit_flag == 0, NA, HitOrder),
  ) %>% select(-flag, -hit_flag)
  
  # filter out actions that never complete (mole spawn, but never is hit)
  D = D %>% group_by(HitOrder) %>% dplyr::mutate(
    flag = any(Event %in% c("Mole Spawned")),
    flag2 = any(Event %in% c("Mole Hit","Mole Expired")),
    HitOrder = ifelse(flag == flag2, HitOrder, NA),
  ) %>% select(-flag, -flag2)
  
  # filter out actions that were interrupted (game paused)
  D = D %>% group_by(HitOrder) %>% dplyr::mutate(
    flag = any(Event %in% c("Game Paused")),
    HitOrder = ifelse(flag, NA, HitOrder),
  ) %>% select(-flag)
  

  
  # add timestamps for when hit started and ended
  D = D %>% group_by(HitOrder) %>% dplyr::mutate(
    HitStartTimestamp = first(Timestamp),
    MoleIdStart = ifelse(Event == "Mole Spawned", MoleId,NA),
    HitEndTimestamp = last(Timestamp)
  ) %>% tidyr::fill(MoleIdStart, .direction="downup")
  
  # We will just do it at the summary stage.
  #D = D %>% ungroup() %>%
  #  mutate(MoleIdToHit = ifelse(!is.na(HitOrder), MoleIdStart, NA)) %>%
  #  tidyr::fill(MoleIdToHit, .direction="up") %>%
  #  mutate(MoleIdToHit = lead(MoleIdToHit)) %>%
  #  group_by(HitOrder) %>% 
  #  mutate(MoleIdToHit = last(MoleIdToHit))
  
  #browser()
  #D %>% filter(!is.na(HitOrder)) %>% select(Framecount,Timestamp, Event, HitOrder,ActionOrder, MoleOrder, MoleIdStart,MoleIdToHit) %>% view()
  
  ####
  # Verify pattern distances
  ####
  # grouped by patternsegmentlabel to exclude distances forming across breaks.
  # todo: verify whether we also consider patternsegmentlabel (e.g. exclude breaks) when calculating action trajectories.
  
  # Note: Plotting of base pattern moved to plot_basepatterns.R
  
  # Import actual mini-patterns
  
  patterns = read.csv("wam_pattern.csv", sep=";")
  
  D = D %>% left_join(patterns)
  
  Sdd = D %>% filter(Event == "Mole Spawned") %>% select(Participant, MiniPatternLabel, SessionProgram,Framecount,MoleId, 
                                                               MolePositionWorldX, MolePositionWorldY, PatternSegmentLabel,
                                                               MolePositionMSX, MolePositionMSY, MoleSize,HitOrder,MoleIdStart) %>%
    #group_by(PatternSegmentLabel) %>%
    ungroup() %>%
    mutate (NextMolePositionWorldX = lead(MolePositionWorldX),
            NextMolePositionWorldY = lead(MolePositionWorldY),
            NextMolePositionMSX = lead(MolePositionMSX),
            NextMolePositionMSY = lead(MolePositionMSY)) %>%
    filter(!is.na(NextMolePositionWorldX)) %>% rowwise() %>%
    mutate(MolePositionWorld = list(data.frame('x'=c(MolePositionWorldX,NextMolePositionWorldX),
                                               'y'=c(MolePositionWorldY,NextMolePositionWorldY))),
           MolePositionMS = list(data.frame('x'=c(MolePositionMSX,NextMolePositionMSX),
                                            'y'=c(MolePositionMSY,NextMolePositionMSY))),
           dist_moleMS = st_length(st_linestring(data.matrix(data.frame(MolePositionMS)))),
           dist_mole = st_length(st_linestring(data.matrix(data.frame(MolePositionWorld)))),
           fittsID = log2(dist_mole / MoleSize + 1),
    ) 
  
  Sdd = Sdd %>% ungroup() %>% mutate(
    MoleIdToHit = lead(MoleIdStart),
    MoleCombo = paste(MoleIdStart,MoleIdToHit,sep="-"),
  )
  #save(Sdd, file = 'plot_patterns.rda', compress=TRUE)
  
  #Sdd = Distances %>% group_by(PatternSegmentLabel) %>% 
  #  summarise(
  #    distance = sum(dist_mole),
  #    MoleSize = unique(MoleSize),
  #    distanceMS = sum(dist_moleMS),
  #    Participant = unique(Participant),
  #    SessionProgram = unique(SessionProgram),
  #    MiniPatternLabel = unique(MiniPatternLabel),
  #    MoleId = paste(MoleId, collapse=" "),
  #    dist_mole = paste(dist_mole, collapse=" ")
  #  )
  
  #Sd %>% group_by(Participant,SessionProgram) %>% summarise(dist_prog = sum(distance)) %>% view()
  #Sd %>% group_by(PatternSegmentLabel) %>% summarise(dist_prog = sum(distance), dist_prog_sd = sd(distance), dist_prog_mean = mean(distance)) %>% view() # check whether, after the shuffle, any segments had higher weight.
  
  if (!is.null(Sd)) {
    Sd = Sd %>% bind_rows(Sdd)
  } else {
    Sd = Sdd
  }
  
  
  ####
  # ControllerHover: Restore broken ControllerHover column and calcualate hover time and hover leaving time.
  ####
  #browser()
  
  # Fix ControllerHover, by ensuring that any Pointer Hover event before a hit has same ID as the hit.
  # this fix only works in our case because we have a single target to hit - hard to generalize for WhackVR dashboard.
  # Remove Pointer Hover Begin that has lower timestamp.
  D = D %>% group_by(HitOrder) %>% mutate(
      hasPointerHover = any(Event == "Pointer Hover Begin"),
      firstMoleHit = first(MoleId[Event %in% c("Mole Hit","Mole Expired")]),
      ControllerHover.fix = ifelse(Event == "Pointer Hover Begin", firstMoleHit,NA),
      flag = max(rowindex[Event == "Pointer Hover Begin"]),
      ControllerHover.fix = ifelse(hasPointerHover & rowindex == flag, ControllerHover.fix,NA),
      flag = ifelse(is.na(ControllerHover.fix) & Event == "Pointer Hover Begin", T,F)
  ) %>% filter(!flag)
  
  # Do a filldown to catch all Pointer Hover End, and then cleanup
  # Also, if the mole was hovered multiple times (hover-in,hover-out), count the earliest hover and remove later occurences
  D = D %>% ungroup() %>% tidyr::fill(ControllerHover.fix, .direction="down") %>%
    mutate(
      ControllerHover.fix = case_when(Event %in% c("Pointer Hover Begin", "Pointer Hover End") ~ ControllerHover.fix)
    ) %>% group_by(HitOrder) %>%
    mutate(
      flag = min(rowindex[Event == "Pointer Hover Begin"]),
      ControllerHover.fix = case_when(Event == "Pointer Hover Begin" & rowindex == flag ~  ControllerHover.fix,
                                      Event == "Pointer Hover End" ~ ControllerHover.fix),
      flag = ifelse(is.na(ControllerHover.fix) & Event == "Pointer Hover Begin", T,F)
    ) %>% filter(!flag)
    
  
  # Remove unnecessary Pointer Hover Ends - choose the Pointer Hover end furthest away from Pointer Hover Begin.
  # This mean Begin/End dont necessarily represent the same hover point, however, we do ensure they are the same moleID.
  D = D %>% mutate(
    MoleIdHover = ifelse(is.na(MoleId), ControllerHover.fix, MoleId),
  ) %>% group_by(MoleIdHover) %>% 
    mutate(
      flag = ifelse(Event == "Pointer Hover Begin",rowindex,NA),
      flagmole = ifelse(Event == "Pointer Hover Begin",MoleIdHover,NA),
      flaghit = ifelse(Event == "Mole Hit",rowindex,NA),
      flaghitmole = ifelse(Event == "Mole Hit",MoleIdHover,NA),
    ) %>% ungroup() %>% tidyr::fill(flag,flagmole,flaghit,flaghitmole, .direction="downup")  %>%
    mutate(
      #exclude hovers attributed to other moles.
      #exclude hover ends that come before the hit - the minimum happened end hover is always after a hit like 27706 P1, 3754 P1, 
      flag = ifelse(Event == "Pointer Hover End" & MoleIdHover == flagmole & rowindex > flaghit & MoleIdHover == flaghitmole,flag,NA),
    ) %>%
    group_by(flag) %>% mutate(
      # use minimum pointer hover end coming after hit - solves situations with multiple hover ends. 
      flag2 = min(rowindex[Event == "Pointer Hover End"]),
      ControllerHover.fix = case_when(Event == "Pointer Hover End" & rowindex == flag2 ~  ControllerHover.fix,
                                      Event == "Pointer Hover Begin" ~ ControllerHover.fix),
      flag3 = ifelse(is.na(ControllerHover.fix) & Event == "Pointer Hover End", T,F)
    ) %>% ungroup() %>% filter(!flag3) %>% select(-flag,-flag2,-flag3,-flagmole)
  
  # Calculate Hover Leave Phase
  D = D %>% mutate(
    flag = ifelse(Event == "Pointer Hover Begin",rowindex,NA),
  ) %>% ungroup() %>% tidyr::fill(flag, .direction="downup") %>%
    group_by(flag) %>% mutate(
      time_hover_end = first(Timestamp[Event == "Pointer Hover End"]),
      time_mole_hit = first(Timestamp[Event == "Mole Hit"]), # there may be more hits without corresponding hovers.
      ControllerLeaveTarget_ms = difftime(time_hover_end,time_mole_hit),
      ControllerLeaveTarget_ms = ifelse(Event %in% c("Pointer Hover Begin","Pointer Hover End","Mole Hit"),ControllerLeaveTarget_ms,NA)
    )
  
  # Calculate Hover Arrive Phase
  D = D %>% mutate(
    flag = ifelse(Event == "Pointer Hover Begin",rowindex,NA),
    flagmole = ifelse(Event == "Pointer Hover Begin",MoleIdHover,NA),
  ) %>% ungroup() %>% tidyr::fill(flag,flagmole, .direction="downup") %>%
    mutate(
      flag = ifelse(Event == "Mole Hit" & MoleIdHover != flagmole,NA,flag), # ensure we dont include hits with other ID than our mole.
    ) %>% 
    group_by(flag) %>% mutate(
      time_hover_begin = first(Timestamp[Event == "Pointer Hover Begin"]),
      time_mole_hit = first(Timestamp[Event == "Mole Hit"]), # there may be more hits without corresponding hovers.
      ControllerHoverTarget_ms = difftime(time_mole_hit,time_hover_begin),
      ControllerHoverTarget_ms = ifelse(Event %in% c("Pointer Hover Begin","Pointer Hover End","Mole Hit"),ControllerHoverTarget_ms,NA)
    )
  
  # total_time = difftime(Timestamp[Event %in% c("Game Stopped", "Game Finished")], 
  # Timestamp[Event == "Game Started"], units="secs"),
  # total_time = as.integer(total_time),
  if (debug_flag) {
    print(last(D$SessionProgram))
    print(last(D$Participant))
    # visualize/debug   
    D %>% filter(Event %in% c("Mole Hit","Pointer Hover Begin","Pointer Hover End")) %>% select(ControllerHover.fix,hasPointerHover,Timestamp, Event, ControllerHover, MoleId, HitOrder,ActionOrder, MoleOrder,PatternSegmentLabel) %>% view()
    D %>% filter(Event %in% c("Mole Spawned","Mole Hit","Pointer Hover Begin","Pointer Hover End")) %>% select(flag,flagmole,rowindex,Timestamp, Event,time_mole_hit,time_hover_begin,ControllerLeaveTarget_ms,ControllerHoverTarget_ms,ControllerHover.fix, MoleIdHover,MoleId, HitOrder,ActionOrder, MoleOrder,PatternSegmentLabel) %>% view()
    D %>% filter(HitOrder == 1) %>% select(Framecount,Timestamp, Event, ControllerHover.fix,hasPointerHover,ControllerHover,HitOrder,ActionOrder, MoleOrder, HitStartTimestamp,HitEndTimestamp,MoleId,RightControllerLaserPosWorldX,RightControllerLaserPosWorldY) %>% view()
    # visualize single mole life
    fig %>% add_trace(data = D %>% filter(HitOrder == 3) %>% tidyr::fill(RightControllerPosWorldX, .direction='downup'),
                      type='scatter',mode='markers+text', 
                      x=~RightControllerPosWorldX, y=~Framecount, text=~as.character(ControllerHover)) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper",
                        text=~difftime(last(Timestamp),first(Timestamp))),
             xaxis=list(range=c(-5,5), zeroline=F, tickfont=list(size=15)),
             yaxis=list(zeroline=F, tickfont=list(size=15), showticklabels=T))
    # summarize lengths, check they have realistic durations
    D %>% filter(!is.na(HitOrder)) %>% group_by(HitOrder) %>% summarise(
      duration = difftime(last(Timestamp),first(Timestamp))
    ) %>% view()
    #visualize each hit's time usage over time of the game.
    fig %>% add_trace(data = D %>% filter(Event == "Mole Hit"), type='scatter',mode='markers', 
                      x=~difftime(Timestamp,first(Timestamp)), y=~difftime(HitEndTimestamp,HitStartTimestamp)) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper",
                        text=~difftime(last(Timestamp),first(Timestamp))),
             xaxis=list( zeroline=F, tickfont=list(size=15)),
             yaxis=list(range=c(0,5.5), zeroline=F, tickfont=list(size=15), showticklabels=T))
  }

  # If a pointer shot happens and a hit follows in the same frame, assume they are linked.
  D = D %>% ungroup() %>%
    group_by(Framecount) %>%
    mutate(ActionOrder = ifelse(Event %in% c("Mole Hit", "Fake Mole Hit"), 
                                ActionOrder[Event=="Pointer Shoot"], ActionOrder)) %>%
    ungroup()
  
  # Provide positions of the previously hit mole.
   D = D %>% filter(Event %in% c("Mole Hit", "Fake Mole Hit")) %>%
     mutate(MolePrevHitPositionWorldX = lag(MolePositionWorldX),
            MolePrevHitPositionWorldY = lag(MolePositionWorldY),
            MoleHitPositionWorldX = ifelse(MoleType=="Target", MolePositionWorldX,NA),
            MoleHitPositionWorldY = ifelse(MoleType=="Target", MolePositionWorldY,NA),
            DistRHitPositionWorldX = ifelse(MoleType=="DistractorRight", MolePositionWorldX,NA),
            DistRHitPositionWorldY = ifelse(MoleType=="DistractorRight", MolePositionWorldY,NA),
            DistLHitPositionWorldX = ifelse(MoleType=="DistractorLeft", MolePositionWorldX,NA),
            DistLHitPositionWorldY = ifelse(MoleType=="DistractorLeft", MolePositionWorldY,NA),
            MoleHitType = MoleType) %>%
     select(rowindex, MoleHitType, MoleHitPositionWorldX,MoleHitPositionWorldY, 
            MolePrevHitPositionWorldX, MolePrevHitPositionWorldY,
            DistRHitPositionWorldX, DistRHitPositionWorldY,
            DistLHitPositionWorldX, DistLHitPositionWorldY) %>%
     right_join(D, by="rowindex")
  
   
  #M = tibble(ActionOrder, MoleType, PosWorldX, PosWorldY, visible)
  # Add PositionWorld for DistR, DistL, and Mole for each action
   
  # 1: make MoleId actually unique by combining framecount and MoleId.

  moleSpawnEvents = c("DistractorLeft Mole Spawned","Mole Spawned",
                 "DistractorRight Mole Spawned")
  
  moleHitEvents = c("Mole Hit", "DistractorLeft Mole Expired",  "DistractorRight Mole Expired",
                    "Mole Expired", "Fake Mole Hit")
  
  
  
  moleAlive = D %>% select(Participant,Session,ActionOrder,Framecount,Event, MoleId, MoleType,MolePositionWorldX, MolePositionWorldY) %>% 
     filter(Event %in% c(moleSpawnEvents,moleHitEvents)) %>% 
    # Create Unique ID that encodes the time which the mole spawned into it.
     mutate(moleIDUnique = paste(Framecount,MoleId)) %>% group_by(MoleId) %>%
     mutate(moleIDUniqueLag = lag(moleIDUnique)) %>% ungroup() %>%
    # Apply unique ID also to rows where moles are hit/expire
     mutate(moleSpawnID = ifelse(Event %in% moleHitEvents, moleIDUniqueLag,moleIDUnique),
            moleIDUnique = NULL, moleIDUniqueLag = NULL) %>% ungroup() %>%
     group_by(moleSpawnID) %>% 
    # Use Unique ID to identify the earliest/latest action that the mole was active.
     mutate(lowestActionOrder = min(ActionOrder, na.rm=T),
            highestActionOrder = max(ActionOrder, na.rm=T)) %>%
     filter(Event %in% moleSpawnEvents) %>% ungroup() %>%
    # Grow dataset to contain all 12 actions for each unique mole ID, then filter.
     complete(ActionOrder,moleSpawnID) %>% group_by(moleSpawnID) %>%
     mutate(ActionOrder = ifelse(ActionOrder < min(lowestActionOrder,na.rm=T), NA, ActionOrder),
            ActionOrder = ifelse(ActionOrder > max(highestActionOrder,na.rm=T), NA, ActionOrder),
            MoleId = min(MoleId, na.rm=T),
            MoleAlivePositionWorldX = min(MolePositionWorldX, na.rm=T),
            MoleAlivePositionWorldY = min(MolePositionWorldY, na.rm=T),
            MoleType = min(MoleType, na.rm=T),
            PlayPeriod = "Game") %>%
     filter(!is.na(ActionOrder)) %>% 
    select(Participant,Session,ActionOrder, PlayPeriod, MoleId, MoleType, MoleAlivePositionWorldX, MoleAlivePositionWorldY) %>%
    ungroup()
  
  D = D %>% bind_rows(moleAlive)
  #D %>% summarize(ActionOrderN = unique(ActionOrder)) %>% view()
  
   
   #values_from =Framecount,ActionOrder,Event) %>% view()
   
  #D %>% group_by(ActionOrder) %>% 
  #  mutate(group = n()) %>% select(ActionOrder, group) %>% view()

    
   
  #D %>% filter(Event %in% c("Mole Spawned", "Mole Expired")) %>%
  #    mutate(MoleId_ = ifelse(Event == "Mole Spawned", MoleId, NA),
  #           MoleId_e = ifelse(Event == "Mole Expired", MoleId, NA),
  #           activeMoles = ifelse(Event == "Mole Spawned", 
  #                                Reduce(paste, paste0(as.character(MoleId_),","), accumulate = TRUE),NA),
  #           activeMoles = str_remove_all(activeMoles,"NA"),
  #           deadMoles = ifelse(Event == "Mole Expired", 
  #                              Reduce(paste, paste0(as.character(MoleId_e),","), accumulate = TRUE),NA),
  #           deadMoles = str_remove_all(deadMoles,"NA"),) %>% 
  #   select(Event,activeMoles, deadMoles) %>% view()
  # 
  # 
  # 
  # D %>% filter(Event %in% c("Mole Spawned", "Mole Expired")) %>%
  #   select(Event,MoleId,MolePositionWorldX,MolePositionWorldY) %>% view()
  
  
  
  
  # Debug view:
  #D %>% filter(Event != "Sample") %>% select(SessionProgram, PlayPeriod,Timestamp, Event, MoleId, MoleOrder,ActionOrder,MoleSpawnOrder,PointerShootOrder) %>% view()
    
  # Debug view of action trajectories:
  #D %>% filter(Event != "Sample", ActionOrder==12) %>% select(MoleHitPositionWorldX, SessionProgram, PlayPeriod,Timestamp, Event, MoleId, MoleOrder,ActionOrder,MoleSpawnOrder,PointerShootOrder) %>% view()
  

  # Six Quadrants
  # 
  
  # Create column in "D" to tell whether laser position is within boundaries of wall.
  D = D %>% 
    mutate(LaserWithinWallBounds = case_when(RightControllerLaserPosWorldX < na.omit(WallBoundsXMin[Event=="CountDown 0"]) ~ FALSE,
                                             RightControllerLaserPosWorldX > na.omit(WallBoundsXMax[Event=="CountDown 0"]) ~ FALSE,
                                             RightControllerLaserPosWorldY < na.omit(WallBoundsYMin[Event=="CountDown 0"]) ~ FALSE,
                                             RightControllerLaserPosWorldY > na.omit(WallBoundsYMax[Event=="CountDown 0"]) ~ FALSE,
                                             TRUE ~ TRUE))

  # # Plot motorspace movements with moles (converted to MS space)
  # 
  # D %>% filter(PlayPeriod == "Game", !is.na(ActionOrder), LaserWithinWallBounds) %>%
  #   plot_ly(name='markers', ., color=I('rgba(0.8,0.8,0.8,0.70)'), type='scatter', mode="markers",
  #           x=~RightControllerPosWorldX, y=~RightControllerPosWorldY,
  #           marker=list(size=2)) %>%
  #   #add_trace(name='markers', ., color=I('purple'), type='scatter', mode="markers",
  #   #           x=~LeftControllerLaserPosWorldX, y=~LeftControllerLaserPosWorldY,
  #   #           marker=list(size=2)) %>%
  #   add_trace(data=MS,name='line', x = ~c(x0,x1,x1,x0,x0),
  #             y=~c(y0,y0,y1,y1,y0), mode='lines', line=list(width=1.15), color=I('black'))  %>%
  #   add_trace(data=D,name="MoleTargets", x=~MolePositionMSX,
  #             y=~MolePositionMSY,symbol=I('o'),marker=list(size=12))
  # 
  # # Plot laser target movements - beware, this includes Unity's positional glitches and sticky wall boundaries.
  #   
  # fig_c <- D %>% filter(PlayPeriod == "Game", !is.na(ActionOrder), LaserWithinWallBounds) %>%
  #   plot_ly(name='markers', ., color=I('rgba(0.8,0.8,0.8,0.70)'), type='scattergl', mode="markers",
  #           x=~RightControllerLaserPosWorldX, y=~RightControllerLaserPosWorldY,
  #           marker=list(size=2)) %>%
  #   #add_trace(name='markers', ., color=I('purple'), type='scatter', mode="markers",
  #   #           x=~LeftControllerLaserPosWorldX, y=~LeftControllerLaserPosWorldY,
  #   #           marker=list(size=2)) %>%
  #   add_trace(name='line', ., x = W$x,
  #             y=W$y, mode='lines', line=list(width=1.15), color=I('black')) %>%
  #   add_trace(name="MoleTargets", ., x=~MoleHitPositionWorldX,
  #             y=~MoleHitPositionWorldY,symbol=I('o'),marker=list(size=12)) %>%
  #   add_trace(name="MoleHit", ., x=~MoleHitPositionWorldX,
  #             y=~MoleHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=15)) %>%
  #   add_trace(name="DistRHit", ., x=~DistRHitPositionWorldX, color=I('red'),
  #             y=~DistRHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=12)) %>%
  #   add_trace(name="DistLHit", ., x=~DistLHitPositionWorldX, color=I('blue'),
  #             y=~DistLHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=12)) %>%
  #   add_trace(name="PrevMoleHit", ., x=~MolePrevHitPositionWorldX,
  #             y=~MolePrevHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=12)) %>%
  #   add_trace(name="MoleAlive", ., x=~MoleAlivePositionWorldX[MoleType=="Target"],
  #             y=~MoleAlivePositionWorldY[MoleType=="Target"],symbol=I('o'),marker=list(size=12)) %>%
  #   layout(showlegend=F, xaxis=list(showticklabels=F),yaxis=list(showticklabels=F,zeroline=F))
  
  
  
  # D %>% filter(PlayPeriod == "Game", !is.na(ActionOrder)) %>%
  #   group_by(ActionOrder) %>%
  #   plot_ly(name='markers', ., color=I('rgba(0.8,0.8,0.8,0.70)'), type='scatter', mode="markers",
  #           x=~MoleAlivePositionWorldX, y=~MoleAlivePositionWorldX,
  #           marker=list(size=2)) %>%
  #   subplot(nrows = 4, margin=0.01) %>%
  #   layout(showlegend=F)
  # 
  
  # fig <- D %>% filter(PlayPeriod == "Game", !is.na(ActionOrder), LaserWithinWallBounds) %>%
  #   group_by(ActionOrder) %>%
  #   do(p = plot_ly(name='markers', ., color=I('rgba(0.8,0.8,0.8,0.70)'), type='scatter', mode="markers",
  #                  x=~RightControllerLaserPosWorldX, y=~RightControllerLaserPosWorldY,
  #                  marker=list(size=2)) %>%
  #       #add_trace(name='markers', ., color=I('purple'), type='scatter', mode="markers",
  #       #           x=~LeftControllerLaserPosWorldX, y=~LeftControllerLaserPosWorldY,
  #       #           marker=list(size=2)) %>%
  #        add_trace(name='line', ., x = W$x,
  #                  y=W$y, mode='lines', line=list(width=1.15), color=I('black')) %>%
  #        add_trace(name="MoleTargets", ., x=~MoleHitPositionWorldX,
  #                  y=~MoleHitPositionWorldY,symbol=I('o'),marker=list(size=12)) %>%
  #        add_trace(name="MoleHit", ., x=~MoleHitPositionWorldX,
  #                  y=~MoleHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=15)) %>%
  #        add_trace(name="DistRHit", ., x=~DistRHitPositionWorldX, color=I('red'),
  #                  y=~DistRHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=12)) %>%
  #        add_trace(name="DistLHit", ., x=~DistLHitPositionWorldX, color=I('blue'),
  #                  y=~DistLHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=12)) %>%
  #        add_trace(name="PrevMoleHit", ., x=~MolePrevHitPositionWorldX,
  #                  y=~MolePrevHitPositionWorldY,symbol=I('circle-x-open'),marker=list(size=12)) %>%
  #        add_trace(name="MoleAlive", ., x=~MoleAlivePositionWorldX[MoleType=="Target"],
  #                  y=~MoleAlivePositionWorldY[MoleType=="Target"],symbol=I('o'),marker=list(size=12)) %>%
  #        add_trace(name="DistLAlive", ., x=~MoleAlivePositionWorldX[MoleType=="DistractorLeft"], color=I('red'),
  #                  y=~MoleAlivePositionWorldY[MoleType=="DistractorLeft"],symbol=I('o'),marker=list(size=12)) %>%
  #        add_trace(name="DistRAlive", ., x=~MoleAlivePositionWorldX[MoleType=="DistractorRight"], color=I('blue'),
  #                  y=~MoleAlivePositionWorldY[MoleType=="DistractorRight"],symbol=I('o'),marker=list(size=12)) %>%
  #        layout(xaxis=list(showticklabels=F),yaxis=list(showticklabels=F,zeroline=F)) %>%
  #        add_annotations(
  #          text = ~unique(paste(ActionOrder)),
  #          x = 0.1,
  #          y = 0.95,
  #          yref = "paper",
  #          xref = "paper",
  #          xanchor = "left",
  #          yanchor = "top",
  #          showarrow = FALSE,
  #          font = list(size = 10)
  #        )) %>%
  #   subplot(nrows=8, margin=0.01) %>%
  #   layout(showlegend=F, title=list(pad=list(b=10),text=paste0("Target-to-Target Player Movement - Action Analysis<br><sup>",folderpath,"</sup>")))

  # todo: show active effects in action-overview, prism, fov, mirror..
  # todo: show eye-tracking data
  # todo: show how much was visible to people in the headset.
  
  #fig
  #orca(fig, paste0("figures/",str_remove_all(folderpath,"/"),".pdf"), width=750, height=750)
  

  ####
  # Arrange again
  #### 
  
  D = D %>% arrange(Timestamp)
  
  # Cleanup Pointer Hover End and Begin by swapping them.
  D = D %>% mutate(
    Event = str_replace_all(Event, c("Pointer Hover End" = "Hover Begin", "Pointer Hover Begin" = "Hover End")),
  )
  
  ####
  # Interpolate position and speed for every 'Sample' event, to create time-equidistant sampling.
  #### 
  
  # todo: estimate also hovering time using the Mole Hover/Unhover events?
  # todo: estimate also search/initial reaction using the point in which the target leaves the previous target?
  
  Di = D %>% filter(PlayPeriod == "Game", Event=="Sample", !is.na(HitOrder)) %>% group_by(Participant, HitOrder) %>%
    summarise(
      PlayPeriod = unique(PlayPeriod),
      Event = unique(Event),
      Session = unique(Session),
      JudgementType = unique(JudgementType,na.rm=T),
      JudgementOrder = unique(JudgementOrder,na.rm=T),
      PerformanceFeedback = unique(PerformanceFeedback,na.rm=T),
      FeedbackJudge = paste0(PerformanceFeedback,JudgementType),
      MoleSpawnOrder = unique(MoleSpawnOrder),
      timestampmin = min(Timestamp),
      timestampmax = max(Timestamp),
      movement_time = timestampmax-timestampmin,
      hertz = 1 / as.numeric(movement_time),
      time_delta = 0.01, # every row is now 10ms
      timestamp_interp = seq(timestampmin, timestampmax, by=0.01),
      RightControllerPosWorldX = approx(Timestamp, RightControllerPosWorldX, xout = timestamp_interp)$y,
      RightControllerPosWorldY = approx(Timestamp, RightControllerPosWorldY, xout = timestamp_interp)$y,
      RightControllerLaserPosWorldX = approx(Timestamp, RightControllerLaserPosWorldX, xout = timestamp_interp)$y,
      RightControllerLaserPosWorldY = approx(Timestamp, RightControllerLaserPosWorldY, xout = timestamp_interp)$y,
      HeadCameraRotEulerX = approx(Timestamp, HeadCameraRotEulerX, xout = timestamp_interp)$y,
      HeadCameraRotEulerY = approx(Timestamp, HeadCameraRotEulerY, xout = timestamp_interp)$y,
      HeadCameraRotEulerZ = approx(Timestamp, HeadCameraRotEulerZ, xout = timestamp_interp)$y,
    ) %>% rename(Timestamp = timestamp_interp) %>% group_by(Participant,HitOrder) %>% 
    mutate(
      dx = c(diff(RightControllerPosWorldX),NA),
      dy = c(diff(RightControllerPosWorldY),NA),
      dt = 0.01,
      speed = sqrt(dx^2 + dy^2) / dt,
      timestampi_max = max(Timestamp),
      timestampi_min = min(Timestamp),
      timestamp_rel = as.numeric(Timestamp - timestampi_min)
      # normalize coordinates and time 
    )
  
  # Create a normalized speed, we can use while predicting.
  Di = Di %>% group_by(Participant,HitOrder) %>% filter(!is.na(speed), !is.na(timestamp_rel)) %>%
    mutate(
      speed_norm = scales::rescale(speed, from=c(min(speed),max(speed)), to=c(0,1)),
      timestamp_rel_norm = scales::rescale(timestamp_rel, from=c(min(timestamp_rel),max(timestamp_rel)), to=c(0,1))
    ) %>% ungroup() %>% tibble::rownames_to_column("rowindex")
  
  #browser()
  # Smooth the speed using a spline with degree 2 polynomial
  # TODO: calculate smooth speed using values normalized to 0-1
  # TODO: figure out how to set the right smoothing coefficient.
  # TODO: Fit portions of the gesture, specifically the first 
  #Di = Di %>% group_by(Participant,HitOrder) %>% filter(!is.na(speed)) %>%
  #  mutate(
  #    speed_smooth = predict(lm(speed_norm ~ poly(timestamp_rel_norm, 2, raw=T)))
      #speed_smooth = predict(loess(speed_norm ~ timestamp_rel, data = ., degree = 2, span = 0.3), timestamp_rel)
  #  ) %>% right_join(Di)

  #for some reason calling predict() inside a mutate doesnt really work..
  
  predictions = lapply(split(Di, list(Di$Participant, Di$HitOrder)), function(df) {
    # Apply LOESS smoothing
    # It's not clear from Ruiz et al if they applied a specific kernel, so we use the default Tricubic kernel.
    
    #df$speed_smooth <- predict(loess(speed_norm ~ timestamp_rel_norm, data = df, degree = 2, span = 0.05, control=loess.control(cell=0.9)), df$timestamp_rel_norm)
    df$speed_smooth_norm <- predict(locfit::locfit(df$speed_norm ~ df$timestamp_rel_norm, family = "gaussian", deg = 2, alpha = 0.2, kern = "rectangular"), df$timestamp_rel_norm)
    #df$speed_smooth <- predict(lm(speed_norm ~ poly(timestamp_rel_norm, 2, raw = TRUE), data = df))
    #df$speed_smooth <- interp::locpoly(x = df$timestamp_rel_norm, y = df$speed_norm, output="points", h=0.05, kernel = "uniform", degree=2)
    
    df = df %>% select(Participant, HitOrder, speed_smooth_norm, timestamp_rel_norm, rowindex)
    return(df)
  })
  
  preds <- do.call(rbind, predictions)
  Di = Di %>% left_join(preds)

  # Bring smoothed speed out of normalized space and back into unit space.
  Di = Di %>% group_by(Participant,HitOrder) %>%
    mutate(
      speed_smooth = scales::rescale(speed_smooth_norm, from=c(min(speed_norm),max(speed_norm)), to=c(min(speed),max(speed))),
    ) %>% ungroup()
  
    
  # Extract the smoothed speed values
  #interp_data$smoothed_speed <- predict(smooth_speed, interp_data$timestamp)$y
  
  if (debug_flag) {
    browser()
    D %>% filter(HitOrder == 2) %>% select(Framecount,Timestamp, Event, HitOrder,ActionOrder, MoleOrder, HitStartTimestamp,HitEndTimestamp,MoleId,RightControllerLaserPosWorldX,RightControllerLaserPosWorldY) %>% view()

    
    fig %>% add_trace(name="raw",data = preds %>% filter(HitOrder == 20), type='scattergl',mode='markers', 
                      x=~timestamp_rel_norm, y=~speed_smooth) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper"),
             xaxis=list( zeroline=F, tickfont=list(size=15)),
             yaxis=list( zeroline=F, tickfont=list(size=15), showticklabels=T))    
    
   fig %>% add_trace(name="raw",data = Di %>% filter(HitOrder == 10), type='scattergl',mode='markers', 
                              x=~timestamp_rel_norm, y=~speed) %>%
      add_trace(name="interpolated",data = Di %>% filter(HitOrder == 10), type='scattergl',mode='lines', 
                x=~timestamp_rel_norm, y=~speed_smooth) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper",
                        text=~movement_time),
             xaxis=list( zeroline=F, tickfont=list(size=15)),
             yaxis=list( zeroline=F, tickfont=list(size=15), showticklabels=T))
    
    # visualize speed and speed_smoothed
    fig_c = fig %>% add_trace(name="real",data = Di %>% filter(Participant==9, HitOrder %in% c(100)), type='scatter',mode='markers', 
                              x=~timestamp_rel, y=~speed) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper",
                        text=~movement_time),
             xaxis=list(zeroline=F, tickfont=list(size=15)),
             yaxis=list(zeroline=F, tickfont=list(size=15), showticklabels=T))
    
    fig_d = fig %>% add_trace(name="interpolated",data = Di %>% filter(Participant==9, HitOrder %in% c(100)), type='scatter',mode='markers', 
                              x=~timestamp_rel, y=~speed_smooth) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper",
                        text=~movement_time),
             xaxis=list(zeroline=F, tickfont=list(size=15)),
             yaxis=list(zeroline=F, tickfont=list(size=15), showticklabels=T))
    subplot(fig_c,fig_d)
    orca(subplot(fig_c,fig_d), "fig/speed_smoothed_timeequidistant.pdf", width=1024, height=512)
    # visualize trajectory
    fig_c = fig %>% add_trace(name="interpolated",data = Di %>% filter(Participant==9, HitOrder %in% c(187,188,189)), type='scatter',mode='markers', 
                      x=~RightControllerPosWorldX, y=~RightControllerPosWorldY) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper",
                        text=~movement_time),
             xaxis=list(range=c(0.2,0.6), zeroline=F, tickfont=list(size=15)),
             yaxis=list(range=c(0.8,1.2), zeroline=F, tickfont=list(size=15), showticklabels=T))
    
    fig_d = fig %>% add_trace(name="raw", data = D %>% filter(Participant==9, HitOrder %in% c(187,188,189)), type='scatter',mode='markers', 
                      x=~RightControllerPosWorldX, y=~RightControllerPosWorldY) %>%
      layout(title=list(font=list(size=15), xanchor="center", xref="paper",
                        text=" "),
             xaxis=list(range=c(0.2,0.6), zeroline=F, tickfont=list(size=15)),
             yaxis=list(range=c(0.8,1.2), zeroline=F, tickfont=list(size=15), showticklabels=T))
    # Plot comparison of interpolated movement vs raw movement.
    subplot(fig_c,fig_d)
    orca(subplot(fig_c,fig_d), "fig/movement_interpolated_timeequidistant.pdf", width=1024, height=512)
  }
  ####
  # Summarize to Actions
  #### 
  
  # Filter out Warmup and Breaks.
  invalid_segments = c("Break","BreakBig","Warmup","None")
  D = D %>% filter(!PatternSegmentLabel %in% invalid_segments)
  
  
  # we need to ensure no NA values to do any calculations with sf.
  # This only extracts features from the 'Sample' events - otherwise we might get
  # weird sampling rate/values (? need to investigate further)
  S = Di %>% ungroup() %>% filter(Event == "Sample", PlayPeriod == "Game", !is.na(RightControllerLaserPosWorldX),
                   !is.na(HitOrder)) %>% group_by(Participant,HitOrder) %>% rownames_to_column("rowid") %>%
    summarize(eventCount = length(Event),
              Participant = unique(Participant),
              Session = unique(Session),
              #time_delta_mean = mean(time_delta, na.rm=T),
              #time_delta_sd = sd(time_delta, na.rm=T),
              time_start = first(Timestamp),
              time_end = last(Timestamp),
              #duration = last(Timestamp) - first(Timestamp),
              #PointerShootOrder = paste(unique(PointerShootOrder, na.rm=T)),
              HitOrder = unique(HitOrder,na.rm=T),
              JudgementType = unique(JudgementType,na.rm=T),
              PerformanceFeedback = unique(PerformanceFeedback,na.rm=T),
              FeedbackJudge = paste0(PerformanceFeedback,JudgementType),
              MoleSpawnOrder = unique(MoleSpawnOrder),
              # Save the line trajectory
              RightControllerLaserPosWorldL = list(data.frame('x'=RightControllerLaserPosWorldX[RightControllerLaserPosWorldX < 0],
                                                              'y'=RightControllerLaserPosWorldY[RightControllerLaserPosWorldX < 0])),
              RightControllerLaserPosWorldR = list(data.frame('x'=RightControllerLaserPosWorldX[RightControllerLaserPosWorldX > 0],
                                                              'y'=RightControllerLaserPosWorldY[RightControllerLaserPosWorldX > 0])),
              RightControllerLaserPosWorld = list(data.frame('x'=RightControllerLaserPosWorldX,
                                                             'y'=RightControllerLaserPosWorldY,
                                                             't'=Timestamp)),
              RightControllerLaserPosWorld_euc = list(data.frame('x'=c(first(RightControllerLaserPosWorldX),last(RightControllerLaserPosWorldX)),
                                                                 'y'=c(first(RightControllerLaserPosWorldY),last(RightControllerLaserPosWorldY)))),
              RightControllerLaserPosWorld_norm = list(data.frame('x'=scales::rescale(RightControllerLaserPosWorldX, from=c(first(RightControllerLaserPosWorldX),last(RightControllerLaserPosWorldX)), to=c(0,1)),
                                                                  'y'=scales::rescale(RightControllerLaserPosWorldY, from=c(first(RightControllerLaserPosWorldY),last(RightControllerLaserPosWorldY)), to=c(0,1)))),
              RightControllerPosWorldL = list(data.frame('x'=RightControllerLaserPosWorldX[RightControllerPosWorldX < 0],
                                                              'y'=RightControllerPosWorldY[RightControllerPosWorldX < 0])),
              RightControllerPosWorldR = list(data.frame('x'=RightControllerPosWorldX[RightControllerPosWorldX > 0],
                                                              'y'=RightControllerPosWorldY[RightControllerPosWorldX > 0])),
              RightControllerPosWorld = list(data.frame('x'=RightControllerPosWorldX,
                                                             'y'=RightControllerPosWorldY,
                                                             't'=Timestamp)),
              RightControllerPosWorld_euc = list(data.frame('x'=c(first(RightControllerPosWorldX),last(RightControllerPosWorldX)),
                                                                 'y'=c(first(RightControllerPosWorldY),last(RightControllerPosWorldY)))),
              RightControllerPosWorld_norm = list(data.frame('x'=scales::rescale(RightControllerPosWorldX, from=c(first(RightControllerPosWorldX),last(RightControllerPosWorldX)), to=c(0,1)),
                                                                  'y'=scales::rescale(RightControllerPosWorldY, from=c(first(RightControllerPosWorldY),last(RightControllerPosWorldY)), to=c(0,1)))),
              HeadCameraRotEuler = list(data.frame('x'=HeadCameraRotEulerX,'y'=HeadCameraRotEulerY,'z'=HeadCameraRotEulerZ)),
              travel_head = st_length(st_linestring(data.matrix(data.frame(HeadCameraRotEuler)))),
              travel_arm = st_length(st_linestring(data.matrix(data.frame(RightControllerPosWorld)))),
              travel_arm_euc = st_length(st_linestring(data.matrix(data.frame(RightControllerPosWorld_euc)))),
              travel_laser = st_length(st_linestring(data.matrix(data.frame(RightControllerLaserPosWorld)))),
              travelR_laser = st_length(st_linestring(data.matrix(data.frame(RightControllerLaserPosWorldR)))),
              travelL_laser = st_length(st_linestring(data.matrix(data.frame(RightControllerLaserPosWorldL)))),
              duration = sum(time_delta, na.rm=T),
              speed_data = list(data.frame('x'=speed,
                                      't'=Timestamp)),
              speed_smooth_data = list(data.frame('x'=speed_smooth,
                                           't'=Timestamp)),
              peak_speed = max(speed,na.rm=T),
              peak_speed_index = first(rowid[speed==max(speed,na.rm=T)]), # first() takes care of NAs
              time_to_peak_speed = sum(time_delta[rowid < peak_speed_index],na.rm=T),
              peak_speed_smooth = max(speed_smooth,na.rm=T),
              peak_speed_smooth_index = first(rowid[speed_smooth==max(speed_smooth,na.rm=T)]), # first() takes care of NAs
              time_to_peak_speed_smooth = sum(time_delta[rowid < peak_speed_index],na.rm=T),
              peak_speed_to_target = sum(time_delta[rowid > peak_speed_index],na.rm=T),
              peak_speed_to_target_pct = (peak_speed_to_target / duration) * 100,
              peak_speed_smooth_to_target = sum(time_delta[rowid > peak_speed_smooth_index],na.rm=T),
              peak_speed_smooth_to_target_pct = (peak_speed_smooth_to_target / duration) * 100,
              # Calculate straightness trajectory
              # 
              )

  
  # fig_c <- fig %>% add_trace(data=S, type='scatter',mode='marker', color=I('darkgrey'),
  #                   x=~bind_rows(RightControllerPosWorld)$x, y=~bind_rows(RightControllerPosWorld)$y) %>%
  #         add_trace(data=S, x=~bind_rows(RightControllerPosWorld_euc)$x, y=~bind_rows(RightControllerPosWorld_euc)$y) %>%
  #         layout(showlegend=F, xaxis=list(showticklabels=F),yaxis=list(showticklabels=F,zeroline=F))
  # orca(fig_c, "fig/movement_cleaned.pdf", width=756, height=756)
  # # should also render an equivalent "dirty" one.
  # fig_c <- fig %>% add_trace(data=D, type='scatter',mode='marker', color=I('darkgrey'),
  #                            x=~RightControllerPosWorldX, y=~RightControllerPosWorldY) %>%
  #   layout(showlegend=F, xaxis=list(showticklabels=F),yaxis=list(showticklabels=F,zeroline=F))
  # orca(fig_c, "fig/movement_dirty.pdf", width=756, height=756)
  
  
  S = W %>% summarize(
    W_widthMin = min(x),
    W_widthMax = max(x),
    W_heightMin = min(y),
    W_heightMax = max(y),
    W_width = abs(W_widthMax) + abs(W_widthMin),
    W_height = abs(W_heightMin) + abs(W_heightMax),
  ) %>% bind_cols(S)
  
  
  S = D %>% ungroup() %>% filter(PlayPeriod == "Game", !is.na(HitOrder),!is.na(Event), Event != "Sample", LaserWithinWallBounds) %>% 
    group_by(Participant,HitOrder) %>%
    summarize(
      Event = list(data.frame('Event'=Event, 'Type'=EventType,'MoleId'=MoleId, 'MoleOrder' = MoleOrder, 'Time' = Timestamp)),
      MolePositionMS_euc = list(data.frame('x'=last(MolePositionMSX),
                                           'y'=last(MolePositionMSY))),
      MolePositionWorld_euc = list(data.frame('x'=last(MolePositionWorldX),
                                              'y'=last(MolePositionWorldY))),
      travel_mole_euc = st_length(st_linestring(data.matrix(data.frame(MolePositionWorld_euc)))),
      ControllerLeaveTarget_ms = unique(na.omit(ControllerLeaveTarget_ms)),
      ControllerHoverTarget_ms = unique(na.omit(ControllerHoverTarget_ms)),
      MoleIdStart = unique(MoleIdStart)
    ) %>% right_join(S)
  
  
  S = S %>% ungroup() %>% mutate(MoleIdToHit = lead(MoleIdStart))
  
  # Save to Summary of Actions
  if (!is.null(Sa)) {
    Sa = Sa %>% bind_rows(S)
  } else {
    Sa = S
  }
  
}


# Unify MaxSpeed to Speed
Sa = Sa %>% mutate(
  FeedbackJudge = str_replace_all(FeedbackJudge, c("OperationMaxSpeed" = "OperationSpeed", "ActionMaxSpeed" = "ActionSpeed","TaskMaxSpeed" = "TaskSpeed")),
  JudgementType = str_replace_all(JudgementType, c("MaxSpeed" = "Speed")),
)


# Save Dataset as RDA
save(Sa, file = 'data_all_actions.rda', compress=TRUE)
save(Sd, file = 'data_patterns.rda', compress=TRUE)
####
# Export to Python for machine learning
####

Sae = Sa %>% select(Participant, HitOrder, duration, travel, travel_head, PerformanceFeedback, JudgementType, FeedbackJudge)

# todo, ensure data is between 0 and 1 for ML
# todo, if we include positional data, it should be normalized so each action starts at 0.
# todo, 

Sae_clean = data.frame(Participant = as.integer(Sae$Participant),
                       HitOrder = as.integer(Sae$HitOrder),
                       duration = as.numeric(Sae$duration),
                       travel = as.numeric(Sae$travel),
                       travel_head = as.numeric(Sae$travel_head),
                       PerformanceFeedback = as.character(Sae$PerformanceFeedback),
                       JudgementType = as.character(Sae$JudgementType),
                       FeedbackJudge = as.character(Sae$FeedbackJudge))
save(Sae_clean, file = 'data_machine_learning.rda')

####
# Format columns
####

#D = D %>% rename(Participant = i1,
#                Study = i2)

#Sf <- Sf %>% left_join()

#writeLines(colnames(D), "colnames.txt")
#col_formats = read.csv("wam_column.csv", sep=";")

#D = D %>% 
#  mutate_at(col_formats %>% pull(name), 
#            ~ifelse(.x == "NULL", NA, .x)) %>%
#  mutate_at(col_formats %>% filter(type=="numeric") %>% pull(name), 
#            ~as.numeric(.x)) %>%
#  mutate_at(col_formats %>% filter(type=="int") %>% pull(name), 
#            ~as.integer(.x)) %>%
#  mutate_at(col_formats %>% filter(type=="time") %>% pull(name), 
#            ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%OS")) 

####
# Divide into Signifier and Feedback Datasets
####

#Df = D %>% filter(Study == "feedback")
#Ds = D %>% filter(Study == "signifier")

####
# Combine with Likert Scale Data
###

Lf <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zIO96Miqkcs8eVEhIOl4ZNAv9UEz6eLxXjbFwD0R_rY/edit?gid=1857813124#gid=1857813124')

valid_pids = 24

Lf = Lf %>% filter(Participant <= valid_pids)

# Mutate VR Experience, Game Experience
Lf = Lf %>% mutate(VRExperience = ifelse(VRExperience == "yes","yes","no"),
                 GameExperience = ifelse(GameExperience == "yes","yes","no"),
                 PredictedPattern = ifelse(PredictedPattern == "yes","yes","no"))

# Check whether we can filldown without issues
#Lf %>% group_by(Participant) %>% 
#  summarise(PredictedPattern = paste(unique(PredictedPattern),collapse=" "),
#            GameExp = paste(unique(GameExperience),collapse=" ")) %>% view()

# Create Normalized versions of pacing, how much help, liked help

# Cleanup JudgementType
Lf = Lf %>% mutate(
  Algorithm = str_replace_all(Algorithm, c("MaxSpeed" = "Speed")),
  Condition = str_remove(Condition, "FB")
)

# Replace NA values with a '0' rating for help variables.
Lf = Lf %>% mutate(PerformanceFeedback = Condition,
                   JudgementType = Algorithm,
                   FeedbackJudge = paste0(PerformanceFeedback,JudgementType),
                 AlgoCorrespondFastSlow.f = `With this algorithm, the feedback clearly corresponded to whether I was fast or slow.`,
                 HowMuchFeedback.f = `How much feedback did you get?`,
                 FeedbackQuality.f = `Overall, How good did the feedback feel?`,
                 FeedbackQuantity.f = `How much feedback did you get?`,
                 OverallExperience.f = `OverallExperience`,
                 FeedbackOverallFeel.f = `Overall, how did the [blue tail/Checkmark/HeatMap] feedback make the you feel?`,
                 FeedbackNotice.f = `Overall, how much did you notice the [blue tail/checkmark/heatmap] feedback?`,
                 FeedbackEncourage.f = `“The [blue tail/checkmark/heatmap] feedback encouraged me to play faster.”`,
                 FeedbackAssessPerf.f = `“With the [blue tail/checkmark/heatmap] feedback I could easily assess how well I was performing.”`,
                 FeedbackDistract.f = `How much did you feel that the [blue tail/checkmark/heatmap] feedback distracted you?`,
                 FeedbackSenseDiff.f = `“With the [blue tail/checkmark/heatmap] feedback, I sensed the difference between the three algorithms.”`) %>%
  mutate(across(ends_with(".f"), ~ factor(.,levels=c(1:7))))


# Filling

# columns which need filling:
fill_per_participant = c('VRExperience', 'VRSickness','GameExperience','OverallExperience.f','PredictedPattern','Age','Gender','Glasses','PlayedBefore', 'PreferedFeedback')
fill_per_feedback = c('FeedbackSenseDiff.f','FeedbackDistract.f','FeedbackAssessPerf.f','FeedbackEncourage.f','FeedbackNotice.f','FeedbackOverallFeel.f','FeedbackQuality.f','FeedbackQuantity.f')

Lf = Lf %>% group_by(Participant) %>% 
  tidyr::fill(all_of(fill_per_participant), .direction='down')

Lf = Lf %>% group_by(Participant,PerformanceFeedback) %>% 
  tidyr::fill(all_of(fill_per_feedback), .direction='down')

# Save Likert responses as RDA
save(Lf, file = 'data_all_experiential.rda', compress=TRUE)


####
# Save Final Data
####
#save(D, file = 'data_all.rda', compress=TRUE)
save(Df, file = 'data_feedback.rda', compress=TRUE)
save(Ds, file = 'data_signifier.rda', compress=TRUE)
