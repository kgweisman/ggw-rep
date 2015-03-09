# calculate length of study for pilot-b

# library
library("chron")

# set working directory for india
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/turk/run-01_2015-03-09/")

# mike's json for-loop
files <- dir("production-results/")

d.raw <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
      data = c("AcceptTime", "SubmitTime"),
      nrow = 1, ncol = 0))
  
  id$AcceptTime = jd$AcceptTime  
  id$SubmitTime = jd$SubmitTime
  
  # bind into same dataframe
  d.raw <- bind_rows(d.raw, id)
}

glimpse(d.raw)

d_times = d.raw %>%
  mutate(startTime = chron(times = substr(AcceptTime, 12, 19), format = 'h:m:s'),
         endTime = chron(times = substr(SubmitTime, 12, 19), format = 'h:m:s'),
         duration = endTime - startTime,
         duration_mins = as.numeric(substr(duration, 4,5)) + as.numeric(substr(duration, 7,8))/60)

summary(d_times)

qplot(d_times$duration_mins)
