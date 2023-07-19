library(readr)
library(GGally)




# Importons nos donnees
diabetes <- read_delim("diabetes.csv", delim = ";", 
                       escape_double = FALSE, col_types = cols(Outcome = col_factor(levels = c("0", 
                                                                                               "1"))), trim_ws = TRUE)
View(diabetes)

ggpai
