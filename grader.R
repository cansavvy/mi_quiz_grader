# C. Savonen
# Grade answers

# Install googlesheets if not installed
if (!("googlesheets" %in% installed.packages())) {
  install.packages("googlesheets")
}

# Magrittr pipe
`%>%` <- dplyr::`%>%`

# Get Answers sheet
gsheet <- googlesheets::gs_title("Ultimate_Michigan_Trivia_Quiz")

# Make this into a data.frame
answers_df <- googlesheets::gs_read(ss = gsheet, ws = 1) %>%
  as.data.frame()

# Get the emails separated
emails <- answers_df[, grep("email", colnames(answers_df))] 
answers_df <- answers_df[, grep("email", colnames(answers_df), invert = TRUE)] 

# Answer template
the_answers <- dplyr::filter(answers_df, Timestamp == "answers") %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::transmute("answer" = as.character(V1), 
                   "question" = rownames(t(answers_df))) %>%
  dplyr::filter(question != "Timestamp") 

# Prep answers
some_answers <- answers_df %>% 
  dplyr::filter(Timestamp != "answers") %>%
  t() %>%
  as.data.frame() %>%
  dplyr::transmute("answer" = as.character(V1), 
                   "question" = rownames(t(answers_df))) %>%
  dplyr::filter(question != "Timestamp") 
  
# Check answers function 

string_prepper <- function(string) {
  string <- tolower(string) 
  string <- gsub("the| a |'|,|\\.|!|state", "", string)
  return(string)
}

# Hard code which questions need to
list_questions <- c("Name the Great Lakes.", 
                    "What are the four major professional sports teams located in Michigan?")
                    
multi_correct_questions <- c("What’s a nickname for Detroit?", 
                             "What’s the state’s official nickname(s)?")
                    
q_num <- 6

total_correct <- c()
for (q_num in 1:nrow(some_answers)) {
  the_question <- some_answers$question[q_num]
  an_answer <- some_answers$answer[q_num]
  the_answer <- the_answers$answer[q_num]
  
  if (length(an_answer) < 1) {
    total_correct <- c(total_correct, 0)
  }
  
  if (the_question == "What is Euchre?") {
     num_correct <- ifelse(grepl("card", an_answer), 1, 0)
     total_correct <- c(total_correct, num_correct)
  }
  
  if (any(grepl(the_question, list_questions))) {
    an_answer <- gsub("detroit", "", an_answer)
    an_answer <- string_prepper(unlist(strsplit(an_answer, " ")))
    the_answer_list <- string_prepper(unlist(strsplit(the_answer, ",")))
    
    num_correct <- length(agrep(an_answer, the_answer, .20))
    total_correct <- c(total_correct, num_correct)
    next
  }
  if (any(grepl(the_question, multi_correct_questions))) {
    an_answer <- string_prepper(an_answer)
    the_answer_list <- string_prepper(unlist(strsplit(the_answer, ",")))
    
    num_correct <- length(agrep(an_answer, the_answer, .20))
    total_correct <- c(total_correct, num_correct)
    next
  }
  if (length(the_answer) == 1) {
    num_correct <- length(agrep(an_answer, the_answer, .15))
    total_correct <- c(total_correct, num_correct)
    next
  }
}
