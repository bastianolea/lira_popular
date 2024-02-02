# usethis::use_git()
# usethis::use_gpl3_license()

#obtener enlaces a liras, obtener textos y metadatos de liras
source("lira_descargar.R")

#procesar (tokenizar) en párrafos, líneas y palabras
source("lira_procesar.R")

#descargar diccionario de sentimientos en español y stopwords
source("obtener_sdal.R")
source("obtener_stopwords.R")


source("lira_calcular_sentimiento.R")
source("lira_comparar.R")
source("lira_modelar_topicos.R")