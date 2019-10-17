# C. Savonen

# Grade answers from Michigan Quiz

# Establish base dir
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

# Load library:
library(optparse)

#--------------------------------Set up options--------------------------------#
# Set up optparse options
option_list <- list(
  make_option(
    opt_str = c("-p", "--participant"), type = "character",
    default = "most_recent", help = "What participant is to be graded?")
)

# Parse options
opt <- parse_args(OptionParser(option_list = option_list))

#################################### Set up  ###################################
# Install googlesheets if not installed
if (!("googlesheets" %in% installed.packages())) {
  install.packages("googlesheets")
}

# Magrittr pipe
`%>%` <- dplyr::`%>%`

# String prepping function 
string_prepper <- function(string) {
  string <- tolower(string) 
  string <- gsub("the| a |'|,|\\.|!|state", "", string)
  return(string)
}

############################### Get answers sheet ##############################
gsheet <- googlesheets::gs_title("Ultimate_Michigan_Trivia_Quiz")

# Make this into a data.frame
answers_df <- googlesheets::gs_read(ss = gsheet, ws = 1) %>%
  as.data.frame()

# Get the emails separated
emails <- answers_df[, grep("email", colnames(answers_df))] 
answers_df <- answers_df[, grep("email", colnames(answers_df), invert = TRUE)] 

################################ Prep answers ##################################
# Answer template
the_answers <- dplyr::filter(answers_df, Timestamp == "answers") %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::transmute("answer" = tolower(as.character(V1)), 
                   "question" = rownames(t(answers_df))) %>%
  dplyr::filter(question != "Timestamp") 

# Prep answers
some_answers <- answers_df %>% 
  dplyr::filter(Timestamp != "answers") %>%
  t() %>%
  as.data.frame() 

# If default is used
if (opt$participant == "most_recent") {
  opt$participant <- ncol(some_answers)
}

# Just select one of the participants' answers
original_answers <- as.character(some_answers[, opt$participant])[-1]

# Format it better
some_answers <- data.frame("answer" = tolower(as.character(original_answers)), 
                           "question" = rownames(t(answers_df))[-1])

###################### Categorize special questions ############################
# Hard code which questions need to treated differently
list_questions <- c("Name the Great Lakes.", 
                    "What are the four major professional sports teams located in Michigan?",
                    "What animals are represented on Michigan’s state flag?",
                    "Which of the following are real cities in Michigan?")
                    
multi_correct_questions <- c("What’s a nickname for Detroit?", 
                             "What’s the state’s official nickname(s)?",
                             "How many miles long is the Mackinac Bridge?")

###################### Do the grading of each question #########################

# Store points in this vector
total_correct <- c()
for (q_num in 1:nrow(some_answers)) {
  
  # Parse the questions and answers 
  the_question <- some_answers$question[q_num]
  an_answer <- some_answers$answer[q_num]
  the_answer <- the_answers$answer[q_num]
  
  # Put 0 if there is no answer
  if (length(an_answer) < 1) {
    total_correct <- c(total_correct, 0)
    next
  }
  
  # Check Euchre question
  if (the_question == "What is Euchre?") {
     num_correct <- ifelse(grepl("card", an_answer), 1, 0)
     total_correct <- c(total_correct, num_correct)
     next
  }
  
  # Check the list questions
  if (any(grepl(the_question, list_questions))) {
    an_answer <- gsub("detroit|lake", "", an_answer)
    an_answer <- string_prepper(unlist(strsplit(an_answer, ",| ")))
    the_answer_list <- string_prepper(unlist(strsplit(the_answer, ", |,| ")))
    
    # Get rid of empty strings
    an_answer <- an_answer[which(nchar(an_answer) > 0)]
    
    # Find how many are correct
    num_correct <- lapply(an_answer, function(answer) {
      agrep(answer, the_answer, .20)
      })
    # Count how many are correct
    num_correct <- length(which(unlist(num_correct) == 1))
    total_correct <- c(total_correct, num_correct)
    next
  }
  # If there are multiple correct answers use this:
  if (any(grepl(the_question, multi_correct_questions))) {
    an_answer <- string_prepper(an_answer)
    an_answer <- gsub("mi|miles|mile", "", an_answer)
    the_answer_list <- string_prepper(unlist(strsplit(the_answer, ",")))
    
    num_correct <- length(agrep(an_answer, the_answer_list, 0.20))
    total_correct <- c(total_correct, num_correct)
    next
  }
  # If question has one answer, use this:
  if (length(the_answer) == 1) {
    num_correct <- length(agrep(the_answer, an_answer, .15))
    total_correct <- c(total_correct, num_correct)
    next
  }
}

# Tally it
final_grade <- paste0(sum(total_correct), " out of 119 possible points")
percent <- (sum(total_correct)/119)*100

# Save this all to a data.frame
output <- data.frame("Question" = some_answers$question,
                     "Your_Answer" = original_answers, 
                     "Points_Awarded" = total_correct)

# Make a summary report about the variant caller and strategy
output_file <- file.path(root_dir, 
                         "reports", 
                         paste0(opt$participant, "_report.Rmd"))

# Path to the template file
template_file <- file.path(root_dir, "template", 
                           "template_report.Rmd")

# Make copy of template
if (file.exists(template_file)) {
  file.copy(from = template_file, to = output_file, overwrite = TRUE)
} else {
  stop(cat("The Rmd template file ", template_file, " does not exist."))
}

# Run this notebook
rmarkdown::render(output_file, "html_document")
