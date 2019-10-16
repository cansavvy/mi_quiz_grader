# C. Savonen
# Grade answers

# Install googlesheets if not installed
if (!("googlesheets" %in% installed.packages())) {
  install.packages("googlesheets")
}

# Magrittr pipe
`%>%` <- dplyr::`%>%`

# Get Answers sheet
answers <- googlesheets::gs_title("Ultimate_Michigan_Trivia_Quiz")

# Make this into a data.frame
answers_df <- googlesheets::gs_read(ss = answers, ws = 1) %>%
  as.data.frame()

# Answer template
answers <- list(dplyr::filter(answers_df, Timestamp == "answers"))
