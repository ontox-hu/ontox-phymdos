## function to fix table output of rsr::list_answers()
fix_tables <- function(df){
  # unlist all column items
  # loop over columns
  for(n in names(df)){
    # create empty vector for column values
    vector <- vector()
    # loop over rows
    for(c in 1:length(df[[n]])){
      #check if cell is null -> TRUE = append NA, FALSE = append column value
      if(is.null(df[[c,n]][[1]][[1]])){
        vector <- append(vector, NA)
        # check if cell is list -> TRUE = unlist, FALSE = continue
      }else if(is.list(df[[c,n]][[1]][[1]])){
        # check if listed cell is null -> TRUE = append NA, FALSE = append column value
        if(is.null(unlist(df[[c,n]][[1]][[1]]))){
          vector <- append(vector, NA)
        }else{
          vector <- append(vector, paste(unlist(df[[c,n]][[1]][[1]]), collapse = ", "))
        }
      }else{
        vector <- append(vector, paste(df[[c,n]][[1]][[1]], collapse = ", "))
      }
    }
    # paste column values back into column
    df[n] <- vector 
  }
  df <- df |> mutate_all(na_if,"")
}

## get answer data from Sysrev, optionally filter by aid
# input pid_data is output of rsr::get_answers()
rsr_as_tables <- function(pid_data, p_aid = NULL, token){
  # if provided, filter data on specific aid
  if(!is.null(p_aid)){
    pid_data <- pid_data |> filter(aid == aid[which(aid == p_aid)])
  }
  # extract SBtab labelled answers from data
  pid_data |> rsr::list_answers(token = token) |>
    # clean inconsistent data from tables
    lapply(function(x){fix_tables(x)})
}