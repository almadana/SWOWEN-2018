# Create a word list from spelling corrections, SUBTLEX and VARCON project
# Custom corrections are included as well as a custom list with proper nouns and acronyms

rm(list = ls())

# Word list with SUBTLEX-ESP (min freq = 2) and CUSTOM DICTIONARY EXTENSIONS, some generic Acronyms as well (PC, CD, etc)
wordDict.file     = './data/dictionaries/spanishWordList.txt'

# Proper nouns, Acronyms
proper.file     = './data/dictionaries/spanishProperNames.txt'

# Spelling corrections
spelling.file   = './data/dictionaries/responseCorrections.csv'


# Output file that combines the various list of valid lexical entries
output.file     = './data/dictionaries/wordlist.txt'

X.varcon        = read.table(wordDict.file, header = TRUE, sep=",", dec=".", quote = "\"",stringsAsFactors = FALSE,
                             encoding = 'UTF-8')
X.proper        = read.table(proper.file, header = T, col.names = c('Word'), sep=",", dec=".", quote = "\"",
                             stringsAsFactors = FALSE, encoding = 'UTF-8')
X.spelling      = read.csv(spelling.file, header = T, col.names = c('Original','Correction'),as.is=T, encoding = 'UTF-8')
X.spelling      = X.spelling %>% select(Correction) %>% rename(Word = Correction)
X               = bind_rows(X.varcon,X.proper,X.spelling) %>% distinct() %>% arrange(Word)

write.csv(X,output.file,fileEncoding = 'UTF-8',row.names = FALSE)

