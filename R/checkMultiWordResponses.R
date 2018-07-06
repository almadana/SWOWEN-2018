# SWOWES-UY 
############
# Checks multi-word responses... 
# ------------------------------
# If the responses consist of a list of comma- (or other symbol) separated values, or
#If the participant has a serial pattern of coming up with three words in the first response slot (usually with no reponses on the other slots)
# ONLY THEN split up
#
# This script is meant to be called from within preprocessData.R
#

getMulti <- function(response1,number) {
  result=unlist(stri_split(response1,regex="[\\s,-]+"))[number]
  return( ifelse(result=="",NA,result))
}

# are responses separated by a blank, a comma, or a pipe ?

X               = X %>% mutate(isUnknown = (R1 %in% unknown.Token),isMissing = as.numeric(R1 %in% missing.Token),nWords = ifelse((isMissing | isUnknown) >  0, NA, ifelse(stri_count(R1,regex="[\\s,-]+")>0  , stri_count(R1,regex="[\\s,-]+") ,0)))

# proportion of first responses that have three words
X = X %>% 
  group_by(participantID) %>% 
  mutate(propThreeResponses = sum(nWords == 3), serialResponder = propThreeResponses > criteria.nWords) 

X = X %>% 
  group_by(X) %>% 
  mutate(R2 = ifelse(serialResponder & nWords>1, getMulti(R1,2),R2), 
         R3 = ifelse(serialResponder & nWords>1, getMulti(R1,3),R3),
         R1 = ifelse(serialResponder & nWords>1, getMulti(R1,1),R1))    #Beware, if you mutate R1 first, this won't work!

nMultiResponsesChanged = sum(X$serialResponder & X$nWords>1,na.rm = T)
message("Number of multiple responses split: ",nMultiResponsesChanged)
# %>% 
  # filter(propThreeResponses>0) %>% 
  # ggplot(aes(x=propThreeResponses))+geom_histogram()

