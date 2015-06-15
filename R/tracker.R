# Tracker contains function that deals with the task of "tracking action after X time periods".
# There are two interpretations:
# (1) Relative time: standardize start time to 0, calculate values corresponding to time delta 0-X.
# (2) Calendar time: From calendar time, calculate values corresponding to X time periods later
#
# The necessary inputs are start time, action time, all POSIXct objects, and track length
# The optional inputs are time unit and seq_override. The last parameter filters action that precedes the start time

library(dplyr)

matrix_rowsum<-function(in_matrix,ncol){
  temp = apply(in_matrix,1, cumsum)
  # for some reason, cumsum messed up the the dimension
  if (dim(temp)[1] == ncol){
    rowsum_matrix = t(temp)
  }else{
    rowsum_matrix = temp
  }
  return(rowsum_matrix)
}


track_relative_time <- function(start_time_data, act_time_data, track_length, end_time,
                                time_unit = 'days', seq_override=FALSE){
  # start_time_data, act_time_data has field id and time. act_time_data has additional field of num
  # track length is time periods tracked since the start time. The 1st cell is time 0, so nth day actually tracks n-1 day
  # end_time handles cases where start_time_data does not survive for the whole duration of track length, which is a POSIXct object
  # If pad with 0s, it will result in bias.


  # The function implicitly assumes that the spell is alive since the start time, hence one unique start time.
  # If the actor could have several active spells, the results will be wrong.
  num_id = nrow(start_time_data)
  if (length(unique(start_time_data$id)) != num_id){
    stop('Start time is not unique!')
  }

  # make sure that the tracking activity is within the range

  # rename
  start_time_data = start_time_data %>% rename(stime=time)
  act_time_data = act_time_data %>% rename(atime=time)
  #process
  meta_data = merge(start_time_data, act_time_data, all.x = T) # the use of merge assumes unique start time and common id!
  # compute
  time_diff_stat = meta_data %>%
    mutate(time_dist = floor(difftime(atime, stime, units=time_unit))+1) %>% # +1 in timedelta because idx starts at 1
    filter(time_dist <= track_length) %>%
    group_by(id, time_dist) %>% summarize(n=sum(num))


  # check for timeline anomaly
  is_invalid_time_distance = sum(time_diff_stat$time_dist<0)>0
  if (is_invalid_time_distance){
    if (seq_override){
      # in some cases, the 2nd activity will precede the start_time_data date
      # such as homework assignment after authentication
      time_diff_stat = time_diff_stat %>% filter(time_dist>0)
    }else{
      stop('There exist actions that preceds start time.')
    }
  }

  # fill the time flow
  # If use cumsum, then has to worry about the gap days.
  pathway = matrix(data=0, ncol = track_length, nrow = num_id)
  # vectorize to speed up
  sids = start_time_data$id
  aids = time_diff_stat$id
  ts = time_diff_stat$time_dist
  ns = time_diff_stat$n

  for (t in seq(track_length)){
    sample_idx = which(ts==t)
    if (length(sample_idx)>0){
      active_id_idx = sids %in% aids[sample_idx]
      pathway[active_id_idx,t] = ns[sample_idx]
    }
  }
  timeline = matrix_rowsum(pathway, track_length)

  # Fill invalid entries with NA
  # filter for spells whose max length is smaller than track_length
  invalid_data = start_time_data %>% mutate(max_length = floor(as.numeric( difftime(end_time, stime ,time_unit) ))+1) %>%
    filter(max_length<track_length)

  invalid_ids = invalid_data$id
  invalid_idx = invalid_data$max_length + 1
  if (sum(invalid_idx<=0)>0){
    stop('Invalid stop gap measures')
  }

  num_invalid_obs = length(invalid_ids)
  if (num_invalid_obs>0){
    for (j in seq(num_invalid_obs)){
      timeline[invalid_ids[j], invalid_idx[j]:track_length] = NA
    }
  }

  #output
  list(id = sids, track=timeline)
}


track_calendar_time <- function(start_time_data, action_time_data, track_length, end_time,
                                time_unit = 'days', aggregate_unit = 'month'){

  # the input descriptions are the same as the track relative time
  num_id = nrow(start_time_data)
  if (length(unique(start_time_data$id)) != num_id){
    stop('Start time is not unique!')
  }

  # transform track length
  if (time_units=='days'){
    track_length_insec = track_length*86400
  }else if(time_units=='weeks'){
    track_length_insec = track_length*604800
  }else{
    stop('unsupported time unit.')
  }


  # set the end time by the etime, then filter out all spells whose etime exceeds endtime
  valid_start_data = start_time_data %>% transform(etime = time+track_length_insec) %>%
    select(id, stime=time, etime) %>%
    filter(etime < end_time)


  # merge with data
  meta_data = merge(valid_start_data, action_time_data, all.x=T)


  # Filter action events only within the time interval and aggregate
  calendar_aggregate = meta_data %>%
    filter(time >= stime & time <= etime) %>%
    group_by(id) %>% summarize(num = n())


  return (calendar_aggregate)
}
