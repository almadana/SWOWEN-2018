require(rstudioapi)
require(stringr)
require(tidyverse)

rm(list = ls())

# Set your working direct
  current_path = getActiveDocumentContext()$path
setwd(dirname(current_path ))
message('Current working directory: ', getwd() )

# Determine unknown and missing response tokens
unknown.Token = 'No conozco la palabra'
missing.Token = c("No más respuestas","No more responses")

listlength.Min = 14
listlength.Max = 18
listlength.default = 14
age.Min        = 16

# Participants who tested the experiments (will be excluded)
#testsubjects = c(1,2,71,7334,7336,36869,60804,76083,76308,83324,89552,89569,99569,100429,112713,122019,122857)

# Languages considered native
nativeLanguages = c('Argentina Cordobés','Argentina Rioplatense','Uruguay Rioplatense','Argentina Nor-oriental')

responseCountTreshold = 300


# Criteria for removing participants with
# 1. over 60% missing or unknown responses
criteria.X = 0.6

# 2. less than 30% of responses in Spanish lexicon
criteria.Spanish = 0.3

# 3. more than 20% of responses not unique (sex,sex,sex)
criteria.Repeat = 0.4

# 4. more than 30% of responses are multi-word
criteria.Ngram = 0.3

# more than 15% of responses are ALL CAPS -> serial ALL-CAPPER
criteria.allCaps = 0.15

#more than 30% of respones are three words for R1 -> serial three-responder
criteria.nWords = 0.3
