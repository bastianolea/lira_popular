

# stopwords ----
stopwords_extra = c("mui", "porque", "cuando", "fué", "hoi", "hai", "despues", "voi", "dió", "aunque", "soi", "digo", "tambien", "siempre", "estaba")

if (exists("stopwords")) {
stopwords_cortas <- stopwords[nchar(stopwords) < 5]
stopwords_2 <- c(stopwords_cortas, stopwords_extra)
}


# simbolos ----
# "\\.|\\,|\\¿|\\?|\\!|\\¡|\\*|\\+|\\-"
simbolos = c(".", "…", ",", "¿", "?", "!", "¡", "*", "+", "-", "—", "–", "_", "%", ":", ";", "º", 
             "[", "]", "(", ")", "«", "»", "<", ">", "“", "”"
)
simbolos_regex = paste0("\\", simbolos, collapse = "|")

numeros = "[0-9]"


#verbos ----
irregulares <- c("acertar", "adquirir", "agradecer", "andar", "asir", "caber", "caer", "ceñir",
                 "conducir", "construir", "contar", "dar", "decir", "discernir", "dormir", "entender", "erguir", "errar", "estar", "haber", "hacer", "ir",
                 "jugar", "leer", "lucir", "mover", "mullir", "oír", "oler", "pedir", "poder", "poner", "pudrir", "podrir", "querer", "roer",
                 "saber", "salir", "sentir", "ser", "sonreír", "tañer", "tener", "traer", "valer", "venir", "ver", "yacer")
