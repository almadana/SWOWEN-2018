# TO BE RUN WITHIN preprocessData.R

# find out which proportion of an individual's responses are capitalized
X =  X %>% 
  mutate(isAllCaps = !is.na( stri_match(response,regex="^[A-ZÁÑÉÍÚÓ\\s]+$")), 
         isFirstCaps = !is.na(stri_match(response,regex="^[A-ZÁÑÉÍÚÓ][^A-ZÁÑÉÍÚÓ]+$"))) %>% 
  group_by(participantID) %>% 
  mutate(prop.isAllCaps = sum(isAllCaps/  sum(!is.na(response)) ), 
         prop.isFirstCaps = sum(isFirstCaps/  sum(!is.na(response)) )) 

# Participans either capitalize all responses, or do not capitalize responses at all
# 0.15 seems a a good threshold to assume serial capitalization -> responses need to be converted to lowercase

# X %>% 
#   filter(prop.isAllCaps > 0.01) %>% 
#   ggplot(aes(x=prop.isAllCaps)) + geom_histogram(bin=50)

X =  X %>% 
  mutate(response = ifelse( (prop.isAllCaps > criteria.allCaps) | (prop.isFirstCaps > criteria.allCaps) , stri_trans_tolower(response), response))

responsesConverted = sum((X$prop.isAllCaps & X$isAllCaps ) > criteria.allCaps,na.rm = T)
message("Number of responses converted to lowercase: ", responsesConverted)
