# Preprocessing pipeline for SWOWES-UY project
#
# Note: For the english project, a similar pipeline is implemented server side, which is used to determine
# the total number of valid responses per cue and determine the snowball sampling
# This pipeline reimplements some of its principles in a more systematic way, in the ES-UY project.
#
# The script writes a fixed number of participants per cue (here set to 60)
# to an output file used for further processing
# To determine which data to retain, a number of checks are performed to see if participants
# are fluent speakers (knowing most cues, responding with words part of the language' lexicon), etc.
# In this script, we also prioritise native speakers if sufficient data is available for a specific cue.
#
# Author: Simon De Deyne, simon2d@gmail.com
# Adaptation for ES-UY data: Álvaro Cabana, almadana@gmail.com

require(stringi)
require(stringr)
require(hunspell)
require(tidyverse)

results      = list()

# Import the raw dataset
data.file    = './data/raw/SWOWES-UY.complete.csv'
output.file  = './data/processed/SWOWES-UY.R60.csv'
lexicon.file = './data/dictionaries/wordlist.txt'
dictionary.file=('./data/dictionaries/Spanish2.dic')
cueCorrections.file = './data/dictionaries/cueCorrections.txt'
responseCorrections.file = './data/dictionaries/responseCorrections.csv'
responseExceptions.file = './data/dictionaries/customCorrections.csv'
report.file  = './output/reports/preprocessing.SWOWES-UY.rds'

X            = read.table(data.file, header = TRUE, sep = ",", dec = ".", quote = "\"",stringsAsFactors = FALSE,
                          encoding = 'UTF-8')
Lexicon      = read.csv(lexicon.file, header = TRUE,stringsAsFactors = FALSE, encoding = 'UTF-8')

# If participants repeat the same response to a specific cue, recode as no more responses
doubles = X %>% filter(R1 == R2, R1 != unknown.Token) %>% select(participantID,cue,R1,R2) %>% nrow()
doubles = doubles + X %>% filter(R2 == R3, R2 %in% missing.Token,R2 != unknown.Token) %>%
          select(participantID,cue,R1,R2)  %>% nrow()
X       = X %>% mutate(R2 = ifelse(R1 == R2 & R1 != unknown.Token, missing.Token,R2))
X       = X %>% mutate(R3 = ifelse(R2 == R3 & R2 != missing.Token & R2 != unknown.Token, missing.Token,R3))

# Swap inconsistent missing responses coded in R2 but not present in R3
inconsistent        = (! (X$R2  %in% missing.Token)) & (! (X$R3 %in% missing.Token))
X$R2[inconsistent]  = X$R3[inconsistent]
X$R3[inconsistent]  = missing.Token[1]

results$responses$doubles = doubles
message('Removed ',doubles, ' repeated responses for a single cue')

# Before convertin to long format
# Check for consistent patterns of multiWord responses at R1... and replace R2 and R2 accordingly
source('./R/checkMultiWordResponses.R')



# Convert data to long format
X           = gather(X,RPOS,response,R1,R2,R3,factor_key = FALSE)


# Original number of responses (unbalanced data)
results$responses$N.original = X %>% nrow()
message('Original number of responses: ', results$responses$N.original)


# Mark both unknown and missing responses. replace them by NA's for the remaining data
X           = X %>% mutate(isMissing = as.numeric(response %in% missing.Token))
X           = X %>% mutate(isUnknown = as.numeric(response %in% unknown.Token))
#X           = X %>% na_if(unknown.Token)

X           = X %>% mutate(response = ifelse(isMissing | isUnknown ,NA,response) )


# Convert dates
X$created_at = as.POSIXct(strptime(X$created_at, format = "%Y-%m-%d %H:%M:%S",tz ='UTC'))

#### STRIP WHITESPACE - FIX COMMON TYPING ERRORS (UNINTENDED SYMBOLS)
X = X %>% mutate(response=stri_replace(response,"",regex="[ç}]$"))
X = X %>% mutate(response=stri_trim(response))


# check capitalization -> convert to lowercase serial CAPLOCKERS
source("./R/checkCapitalization.R")

View(X)

# Lookup for exceptions in the exception list. DO NOT correct these
X$spellingOk=0
responseExceptions = read.csv(responseExceptions.file,stringsAsFactors = F)

# generate cue_response pairs to check for excpetions - KIND OF A HACK/WORKAROUND so mapvalues can be used (requires 1 column, not two)
X$cue_response = paste(X$cue,X$response,sep="__#__") 
responseExceptions$cue_response = paste(responseExceptions$cue,responseExceptions$response,sep="__#__") 

#mark the rows to be corrected as spellingOk -> no further corrections
exceptionCorrections = X$cue_response %in% responseExceptions$cue_response
#correct the responses in the responseExceptions list
X$cue_response=plyr::mapvalues(X$cue_response,responseExceptions$cue_response,responseExceptions$correction,warn_missing = F)
X$spellingOk[exceptionCorrections] = 1
X$response[exceptionCorrections] = X$cue_response[exceptionCorrections]

# Fix alternative spellings for responses
responseCorrections = read.csv(responseCorrections.file,stringsAsFactors = F,encoding = 'UTF-8')
# correct the response using the response correction list, ONLY if spelling is not OK
X = X %>% 
  mutate(response = ifelse(spellingOk,response,  plyr::mapvalues(response,responseCorrections$response,responseCorrections$substitution,warn_missing=F)    ) ) 

#plyr::mapvalues(X$response,)

# Fix alternative spellings for cues
spelling.words  = read.table(cueCorrections.file,sep = '\t',header=TRUE,stringsAsFactors = FALSE, encoding = 'UTF-8')
X$cue           = plyr::mapvalues(X$cue,spelling.words$original,spelling.words$correction,warn_missing = FALSE)

# Count spaces and commas in responses, to figure out of n-grams with n > 1 are used. Ignore missing responses (Unknown word, No more responses")
X               = X %>% mutate(nWords = ifelse(isMissing >  0, NA, ifelse(str_count(response,"\\S+") + str_count(response,",|;") > 1,1,0)))

# Calculate presence of response in wordlist # OR # Hunspell dictionary
spanishDictionary=dictionary(dictionary.file)
X               = X %>%  mutate(inLexicon = ifelse(isMissing > 0, NA, ifelse( spellingOk>0, 1, as.numeric( (response %in% Lexicon$Word) | hunspell_check(response,dict=spanishDictionary) )  )))
#X               = X %>%  mutate(inLexicon = ifelse(isMissing > 0, NA, as.numeric( (response %in% Lexicon$Word ) )  ))
message('Proportion of responses in lexicon: ',  sum(X$inLexicon,na.rm=T)/results$responses$N.original )

# Calculate participant characteristics
PP              = X %>% group_by(participantID,nativeLanguage,gender,age,education) %>%
                    summarise(N = n(),
                    Unknown = sum(isUnknown),
                    Missing = sum(isMissing),
                    C.Response = n_distinct(response),
                    F.Spanish = sum(inLexicon,na.rm = TRUE),
                    F.words = sum(nWords,na.rm = TRUE))

# Convert to proportions, note corrected for unknown and missing responses
PP$Prop.Unknown = PP$Unknown / PP$N
PP$Prop.Repeat  = 1 - (PP$C.Response - as.numeric(PP$Unknown>0) - as.numeric(PP$Missing>0)) / (PP$N - PP$Unknown - PP$Missing)
PP$Prop.X       = (PP$Missing + PP$Unknown) / PP$N
PP$Prop.Spanish = PP$F.Spanish / (PP$N - PP$Unknown - PP$Missing)
PP$Prop.Ngram   = PP$F.words / (PP$N - PP$Unknown - PP$Missing)

PP              = PP %>% mutate(Status = ifelse(Prop.X >  criteria.X, 'X',
                  ifelse(Prop.Spanish < criteria.Spanish,'Non-native',
                         ifelse(Prop.Repeat > criteria.Repeat, 'Perseveration',
                                ifelse(Prop.Ngram > criteria.Ngram, 'Verbose','Valid')))))


## Calculate the breakdown of valid and removed participants
results$pp$N     = dim(PP)[1]
results$pp$N.invalid.X          = sum(PP$Prop.X > criteria.X)
results$pp$N.invalid.nonnative  = sum(PP$Prop.Spanish < criteria.Spanish, na.rm = TRUE)
results$pp$N.invalid.persever   = sum(PP$Prop.Repeat > criteria.Repeat, na.rm  = TRUE)
results$pp$N.invalid.ngram      = sum(PP$Prop.Ngram > criteria.Ngram, na.rm = TRUE)



# Gender and age stats
results$pp$female    = PP %>% filter(gender=='Fe') %>% nrow()
results$pp$male      = PP %>% filter(gender=='Ma') %>% nrow()
results$pp$X         = PP %>% filter(gender=='X') %>% nrow()
results$pp$age.M     = round(mean(PP$age))
results$pp$age.SD    = round(sd(PP$age),1)


# Language stats (I know, this is lazy...)
# results$pp$N.native    = round(PP %>% filter(nativeLanguage %in% nativeLanguages) %>% nrow() / results$pp$N * 100)
# results$pp$N.america   = round(PP %>% filter(nativeLanguage == 'United States') %>% nrow() / results$pp$N * 100)
# results$pp$N.canada    = round(PP %>% filter(nativeLanguage == 'Canada') %>% nrow() / results$pp$N * 100)
# results$pp$N.uk        = round(PP %>% filter(nativeLanguage == 'United Kingdom') %>% nrow() / results$pp$N * 100)
# results$pp$N.australia = round(PP %>% filter(nativeLanguage == 'Australia') %>% nrow() / results$pp$N * 100)


# Education
results$pp$N.education = PP %>% group_by(education) %>% summarise(Freq = n())


# Percentage removed
nPP         = PP %>% group_by(Status) %>% summarise(Freq = n())
results$pp$N.invalid = round(100 * nPP %>% filter(!Status=='Valid') %>% summarise(Freq = sum(Freq)))


# Remove from data
X           = X %>% filter(participantID %in% PP$participantID[PP$Status=='Valid'])

# Verify sufficient responses in the set of cues after removal of invalid pp's
Cues          = X %>% group_by(cue) %>% summarise(Freq = n())
missing       = Cues %>% filter(Freq < responseCountTreshold) %>% arrange(Freq)
missing$Freq  = (responseCountTreshold - missing$Freq)/3

# Sanity check
message('Number of participants missing: ', ceiling(sum(missing$Freq)/listlength.default))

# Convert to wide again
# Select 100 responses per row, by considering first: native American, Australian - Canadian,  - Irish - UK, others
# next by considering the date (most recent first), but always ordered by participants to include complete response sets at the participant level where possible
# Use top N
X_wide    = X %>%  select(id,participantID,age,gender,nativeLanguage,country,education,created_at,cue,response,RPOS) %>% spread(RPOS,response)

# Add a selection variable to favor native speakers
X_wide    = X_wide %>% mutate(Native = ifelse(nativeLanguage == 'United States', 3,
                                    ifelse(nativeLanguage %in% c('Canada','Australia','New Zealand','Jamaica','Puerto Rico'),2,
                                    ifelse(nativeLanguage %in% c('Ireland','United Kingdom'), 1,0)))) %>%
                                  arrange(participantID)

# Create a sample key to avoid ties in top_n, but sample weighted on native language
d = as.double(max(X_wide$participantID))
X_wide$SampleKey = X_wide$Native + (X_wide$participantID/d)

# Collect 100 participants depending on Native and date created (this could be improved)
X_set     = X_wide %>% group_by(cue) %>% top_n(100,SampleKey) %>% arrange(participantID) %>%
            select(id,participantID,age,gender,nativeLanguage,country,education,created_at,cue,R1,R2,R3)


#message('Final number of responses: ', X_set %>% nrow())
results$responses$N.set100 =  X_set %>% nrow()
PP.set    = X_set %>% group_by(participantID,nativeLanguage,gender,age,education) %>%
              summarise(N = n_distinct(participantID)) %>% nrow()

# Uncomment the following to check whether sufficient participants per cue are available
#PP.validset = nPP %>% filter(Status == 'Valid') %>% select(Freq) - PP.set
#message('Set 100 Participants removed: ',PP.validset, ' (',round(PP.validset / nPP %>% filter(Status == 'Valid') %>% select(Freq) * 100,2), '%)')
results$responses$N.valid = X_wide %>% nrow() * 3
results$responses$N.set100 = X_set %>% nrow() * 3
results$responses$N.valid - results$responses$N.set100
results$pp$N.set100  = X_set %>% group_by(participantID) %>% summarise(n_distinct(participantID)) %>% nrow()

# Write the dataset with 100 responses per cue
write.csv(X_set,output.file)

# Write a summary of the output to an rds file
saveRDS(results,report.file,ascii=TRUE)

