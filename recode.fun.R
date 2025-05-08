recode.fun <- function(q, scale = 1){
  if(scale == 1){
    q.recoded = recode(q, "1 – 5 hours" = 1, "6 – 10 hours" = 2,
                       "11 – 15 hours" = 3, "16 – 20 hours" = 4,
                       "More than 20 hours" = 5)
  }
  else if(scale == 2){
    q.recoded = recode(q, "1 – 3 hours" = 1, "4 – 6 hours" = 2,
                       "7 – 9 hours" = 3, "More than 9 hours" = 4)
  }
  else if(scale == 3){
    q.recoded = recode(q, "0 hours (don’t work)" = 1, "1 – 5 hours" = 2, 
                       "6 – 10 hours" = 3, "11 – 15 hours" = 4, 
                       "16 – 20 hours" = 5, "More than 20 hours" = 6)
  }
  else if(scale == 4){
    q.recoded = recode(q, "0 hours (don’t engage in extracurricular activities)" = 1, "1 – 5 hours" = 2, 
                       "6 – 10 hours" = 3, "11 – 15 hours" = 4, 
                       "16 – 20 hours" = 5, "More than 20 hours" = 6)
  }
  else if(scale == 5){
    q.recoded = recode(q, "Poor (1)" = 1, "Fair" = 2, "Okay" = 3, "Good" = 4, "Excellent (5)" = 5)
  }
  else if(scale == 6){
    q.recoded = recode(q, "Yes" = 1, "No" = 0)
  }
  else if(scale == 7){
    q.recoded = recode(q, "(a) Both statements are true, or neither statement is true." = 1, 
                      "(b) Only one statement is true." = 0)
  }
  else if (scale == 8){
    q.recoded = recode(q, "Never" = 1, "Rarely" = 2, 
                       "Sometimes" = 3, "Often" = 4, "Very Often" = 5)
  }
  else if (scale == 15){
    q.recoded = recode(q, "NOT a first-generation college student" = "No", "a first-generation college student" = "Yes" )
  }
  else if (scale == 16){
    q.recoded = recode(q, "a Science, Technology, Engineering, or Mathematics (STEM) major" = "Yes",
                       "NOT a Science, Technology, Engineering, or Mathematics (STEM) major" = "No")
  }
  return(q.recoded)
}