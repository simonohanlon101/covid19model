require(lubridate)

intervention_schedule <- read.csv(file = textConnection(
'region,start_date,end_date,lockdown\n
United_Kingdom,2020-03-23,2020-05-31,True\n
United_Kingdom,2020-06-01,2020-07-31,False\n
United_Kingdom,2020-08-01,2020-09-30,False\n'
  )
  , stringsAsFactors = FALSE
)

origin <- dmy('31/12/2019')

update_table <- function(arr, i, start, end, val){
  arr[i,start:end,] <- val * 1L
  return(arr)
}

alter_interventions <- function(processed_data, intervention_schedule){
  
  newX <- processed_data$stan_data$X
  countries <- names(processed_data$dates)
  
  for(i in 1:nrow(intervention_schedule)){
    data <- intervention_schedule[i,]
    idx <- which(data[,1] == countries)
    start <- as.integer(ymd(data[,2]) - origin)
    end <- as.integer(ymd(data[,3]) - origin)
    val = as.logical(data[,4])
    newX <- update_table(newX, idx, start, end, val)
  }
  
  processed_data$stan_data$X <- newX
  
  return(processed_data)

}



