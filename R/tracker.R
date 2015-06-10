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


track_relative_time <- function(start_time_data, act_time_data, track_length,
                                time_unit = 'days', seq_override=FALSE){
  # start_time_data, act_time_data has field id and time. act_time_data has additional field of num

  # The function implicitly assumes that the spell is alive since the start time, hence one unique start time.
  # If the actor could have several active spells, the results will be wrong.
  num_id = nrow(start_time_data)
  if (length(unique(start_time_data$id)) != num_id){
    stop('Start time is not unique!')
  }

  # the 1st cell is time 0, so nth day tracking requires n+1 cells
  t_track_length = track_length+1


  # make sure that the tracking activity is within the range

  # rename
  start_time_data = start_time_data %>% rename(stime=time)
  act_time_data = act_time_data %>% rename(atime=time)
  #process
  meta_data = merge(start_time_data, act_time_data, all.x = T) # the use of merge assumes unique start time and common id!
  # compute
  time_diff_stat = meta_data %>%
    mutate(time_dist = floor(difftime(atime, stime, units=time_unit))+1) %>% # +1 in timedelta because idx starts at 1
    filter(time_dist <= t_track_length) %>%
    group_by(id, time_dist) %>% summarize(n=sum(num))


  # check for timeline anomaly
  is_invalid_time_distance = sum(time_diff_stat$time_dist<0)>0
  if (is_invalid_time_distance){
    if (seq_override){
      # in some cases, the 2nd activity will precede the benchmark date
      # such as homework assignment after authentication
      time_diff_stat = time_diff_stat %>% filter(time_dist>0)
    }else{
      stop('There exist actions that preceds start time.')
    }
  }

  # fill the time flow
  pathway = matrix(data=0, ncol = t_track_length, nrow = num_id)
  # vectorize to speed up
  sids = start_time_data$id
  aids = time_diff_stat$id
  ts = time_diff_stat$time_dist
  ns = time_diff_stat$n

  for (t in seq(t_track_length)){
    sample_idx = which(ts==t)
    if (length(sample_idx)>0){
      active_id_idx = sids %in% aids[sample_idx]
      pathway[active_id_idx,t] = ns[sample_idx]
    }
  }

  timeline = matrix_rowsum(pathway, t_track_length)
  # for all 0s, convert into NA
  timeline[which(rowSums(timeline)==0),]=NA
  return(timeline)
}


track_calendar_time <- function(benchmark, action_time_data, track_length){

  num_id = length(benchmark$id)
  t_track_length = track_length + 1
  time_unit = 'days'

  # 根据benchmark的起始日期和追踪时长，确定终结日期
  benchmark_end = transform(benchmark, edate = get_dateint_in_k_days(date, track_length)) %>% select(id, sdate=date, edate)
  # 把act数据和benchmark联系起来
  meta_data = merge(benchmark_end, action_time_data, all=T)
  meta_data$date[is.na(meta_data$date)]=99999999  # 将无效数据设置为极大，这样就肯定不在范围内

  #第一步：如果在benchmark中是无效的，去掉
  #第二步: 找到符合追踪条件的活动数据，
  #第三步：按月和主ID汇总, 如果主Key在追踪时间段内无活动，sum(n[flag]) = 0；这对于百分比计算有用
  #第四步：按月汇总

  by_month_path = filter(meta_data, !is.na(edate)) %>%
    transform(flag = (date>=sdate & date<=edate)) %>%
    transform(mid=get_month(sdate)) %>% group_by(id,mid) %>% summarize(num = sum(n[flag]))
  return (by_month_path)
}
