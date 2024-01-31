stopwords <- readr::read_lines("datos/stopwords_español.txt")
sdal <- readr::read_rds("datos/sdal_diccionario_afectos_español.rds")

# stopwords ----
stopwords_extra = c("mui", "porque", "cuando", "fué", "hoi", "hai", "despues", "voi", "dió", "aunque", "soi", "digo", "tambien", "siempre", "estaba")

stopwords_cortas <- stopwords[nchar(stopwords) < 5]
stopwords_2 <- c(stopwords_cortas, stopwords_extra)


# simbolos ----
# "\\.|\\,|\\¿|\\?|\\!|\\¡|\\*|\\+|\\-"
simbolos = c(".", "…", ",", "¿", "?", "!", "¡", "*", "+", "-", "—", "–", "_", "%", ":", ";", "º", 
             "[", "]", "(", ")", "«", "»", "<", ">", "“", "”"
)
simbolos_regex = paste0("\\", simbolos, collapse = "|")

numeros = "[0-9]"
