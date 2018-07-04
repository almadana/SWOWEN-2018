# Post-hoc cue checking for the SWOWES-UY project
#
# this checks the cues and tries to match them with the items on the word list
# uses the raw, unprocessed response data

data.file    = './data/raw/SWOWES-UY.complete.csv'
output.file  ='./data/dictionaries/cuesSoFar.csv'

X            = read.table(data.file, header = TRUE, sep = ",", dec = ".", quote = "\"",stringsAsFactors = FALSE,
                          encoding = 'UTF-8')

#print all the cues, and the number of responses for each cue
cues = X %>% 
  group_by(cue) %>% 
  summarize(n=n()) 

