# DVORAK
# www.overfitting.net
# https://www.overfitting.net/


analyze_letters_spaces_numbers <- function(file_path) {
    # Leer archivo con codificación Windows
    text <- tolower(paste(readLines(file_path, warn = FALSE,
                                    encoding = "latin1"), collapse = ""))
    
    # Reemplazar vocales acentuadas y diéresis por su base
    text <- chartr(
        "áàäâãéèëêíìïîóòöôõúùüû",
        "aaaaaeeeeiiiiooooouuuu",
        text
    )
    
    # Conservar solo letras a-z, ñ, espacios y números 0–9
    filtered_text <- gsub("[^a-z ñ0-9]", "", text)
    
    # Separar caracteres
    chars <- strsplit(filtered_text, "")[[1]]
    
    # Contar frecuencias
    freq_table <- table(chars)
    
    # Convertir a data.frame ordenado
    df <- as.data.frame(freq_table, stringsAsFactors = FALSE)
    colnames(df) <- c("char", "count")
    df$percent <- df$count / sum(df$count)
    df$value <- df$count / max(df$count)
    
    # Ordenar por frecuencia descendente
    # df <- df[order(-df$count), ]
    
    return(df)
}


###########################

names=c("elfindelaeternidad.txt", "citaconrama.txt")
for (i in 1:length(names)) {
    result=analyze_letters_spaces_numbers(names[i])
    print(result)
    write.csv2(result, paste0(names[i], "_stats.csv"), row.names=FALSE)
}
