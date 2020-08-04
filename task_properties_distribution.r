#' Count how many tasks involved the provided combination of props (and no
#' others) for at least one of the provided motor regions (body parts). This can
#' be used to determine, for example, how many of the tasks focussed on the
#' combination of strength and precision only (with no other properties)
#'
#' @param motor_regions which motor regions (body parts) to include in the count
#' @param included_props the combination of props for which to count the number
#'   of tasks matching exactly these props (for at least one of the given motor
#'   regions)
#' @param data the dataframe containing all the task count data.
count_matching_tasks <-
  function(motor_regions, included_props, task_properties, task_counts) {
    matching_tasks <- rep(FALSE, ncol(task_properties) - 2)
    exclusions <- rep(TRUE, ncol(task_properties) - 2)
    for (mr in motor_regions) {
      all_mr_props <- task_properties[task_properties$body_part == mr, ]
      included_mr_props <-
        all_mr_props[all_mr_props$property %in% included_props, ]
      excluded_mr_props <-
        all_mr_props[!(all_mr_props$property %in% included_props) & !(all_mr_props$property == "Task Count"), ]
      
      # Which tasks involve all of the included properties
      matching_included_mr_props <-
        colSums(included_mr_props[, -2:0]) >= length(included_props)
      
      # Which tasks involve any of the excluded properties
      matching_excluded_mr_props <-
        colSums(excluded_mr_props[, -2:0]) >= 1
      
      exclusions <- exclusions & !matching_excluded_mr_props
      matching_tasks <- matching_tasks | matching_included_mr_props
    }
    qualifying_task_counts <- (task_counts[, -2:0][matching_tasks & exclusions])
    if (length(qualifying_task_counts) == 0)
    {
      return(0)
    }
    else if (length(qualifying_task_counts) == 1)
    {
      return(as.integer(qualifying_task_counts))
    }
    else{
      return(sum(qualifying_task_counts))
    }
  }

#' Count the number of tasks involving each combination of gross motor
#' precision, strength and coordination.
#' @param data_filepath the string path to the .csv data file
get_task_types_gross <- function(data_filepath){
  gross_motor_bodyparts <- c("Arms", "Legs", "Feet", "Torso", "Head/Neck")
  p <- c("Precision")
  ps <- c("Precision", "Strength")
  s <- c("Strength")
  sco <- c("Strength", "Coordination")
  co <- c("Coordination")
  pco <- c("Precision", "Coordination")
  psco <- c("Precision", "Strength", "Coordination")
  
  task_properties <- read.csv(data_filepath)
  task_counts <- task_properties[1, ]
  
  gross_data <- data.frame()[1,]
  
  gross_data$s <- count_matching_tasks(gross_motor_bodyparts, s, task_properties, task_counts)
  gross_data$co <- count_matching_tasks(gross_motor_bodyparts, co, task_properties, task_counts)
  gross_data$p <- count_matching_tasks(gross_motor_bodyparts, p, task_properties, task_counts)
  gross_data$pco <- count_matching_tasks(gross_motor_bodyparts, pco, task_properties, task_counts)
  gross_data$sco <- count_matching_tasks(gross_motor_bodyparts, sco, task_properties, task_counts)
  gross_data$ps <- count_matching_tasks(gross_motor_bodyparts, ps, task_properties, task_counts)
  gross_data$psco <- count_matching_tasks(gross_motor_bodyparts, psco, task_properties, task_counts)
  
  return(gross_data)
}

#' Count the number of tasks involving each combination of fine motor
#' precision, strength and coordination.
#' @param data_filepath the string path to the .csv data file
get_task_types_fine <- function(data_filepath){
  fine_motor_bodyparts <- c("Fingers", "Hands")
  p <- c("Precision")
  ps <- c("Precision", "Strength")
  s <- c("Strength")
  sco <- c("Strength", "Coordination")
  co <- c("Coordination")
  pco <- c("Precision", "Coordination")
  psco <- c("Precision", "Strength", "Coordination")
  
  task_properties <- read.csv(data_filepath)
  task_counts <- task_properties[1, ]
  
  fine_data <- data.frame()[1,]

  fine_data$s <- count_matching_tasks(fine_motor_bodyparts, s, task_properties, task_counts)
  fine_data$co <- count_matching_tasks(fine_motor_bodyparts, co, task_properties, task_counts)
  fine_data$p <- count_matching_tasks(fine_motor_bodyparts, p, task_properties, task_counts)
  fine_data$pco <- count_matching_tasks(fine_motor_bodyparts, pco, task_properties, task_counts)
  fine_data$sco <- count_matching_tasks(fine_motor_bodyparts, sco, task_properties, task_counts)
  fine_data$ps <- count_matching_tasks(fine_motor_bodyparts, ps, task_properties, task_counts)
  fine_data$psco <- count_matching_tasks(fine_motor_bodyparts, psco, task_properties, task_counts)
  
  return(fine_data)
}

#' Count the number of tasks involving each combination of
#' sensory information
#' @param data_filepath the string path to the .csv data file
get_task_types_sensory <-function(data_filepath){
  p <- "Proprioceptive"
  th <- "Tactile (Hands)"
  tnh <- "Tactile (Non-Hands)"
  ve <- "Vestibular"
  vi <- "Visual"
  all_sense <- c(p, th, tnh, ve, vi)
  
  task_properties <- read.csv(data_filepath)
  task_counts <- task_properties[1, ]
  
  sensory_data <- data.frame()[1,]
  
  for(i in c(1:length(all_sense))){
    combinations <- combn(all_sense, i)
    for(j in c(1:length(combinations[1,]))){
      sensory_data[paste(combinations[,j], collapse=" + ")] <- count_matching_tasks(c("N/A"), combinations[,j], task_properties, task_counts)
    }
  }
  return(sensory_data)
}


# Asakawa, et al (2019)
filepath_asa <- "./data/task_properties-asakawa.csv"
task_types_gross_asa <- get_task_types_gross(filepath_asa)
task_types_fine_asa <- get_task_types_fine(filepath_asa)
task_types_sensory_asa <- get_task_types_sensory(filepath_asa)

# Gracia-Bafalluy and Noel (2008)
filepath_gra <- "./data/task_properties-graciabafalluy.csv"
task_types_gross_gra <- get_task_types_gross(filepath_gra)
task_types_fine_gra <- get_task_types_fine(filepath_gra)
task_types_sensory_gra <- get_task_types_sensory(filepath_gra)

# Erasmus, et al (2016)
filepath_era <- "./data/task_properties-erasmus.csv"
task_types_gross_era <- get_task_types_gross(filepath_era)
task_types_fine_era <- get_task_types_fine(filepath_era)
task_types_sensory_era <- get_task_types_sensory(filepath_era)

# Katsipataki (2013)
filepath_kat <- "./data/task_properties-katsipataki.csv"
task_types_gross_kat <- get_task_types_gross(filepath_kat)
task_types_fine_kat <- get_task_types_fine(filepath_kat)
task_types_sensory_kat <- get_task_types_sensory(filepath_kat)
