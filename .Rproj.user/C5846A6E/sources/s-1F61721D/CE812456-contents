#import(data.table)

load_data <- function() {
  dt <- fread("data/top50.csv",
                 stringsAsFactors = F, nrows=1000)
  dt
}

select_features <- function(dt) {
  dt <- dt[,Beats.Per.Minute:Popularity]
  dt
}