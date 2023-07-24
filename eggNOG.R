library(stringr)
library(data.table)

parse_go_obo <- function(input_file, output_file) {
  lev_dict <- list('molecular_function' = 'MF', 'biological_process' = 'BP', 'cellular_component' = 'CC')
  
  dict_list <- list()
  
  lines <- readLines(input_file)
  
  a <- b <- c <- NULL
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    if (line == '') {
      if (!is.null(a) && !is.null(b) && !is.null(c)) {
        dict_list[[length(dict_list) + 1]] <- list('GO' = a, 'Description' = b, 'level' = lev_dict[[c]])
      }
      
      a <- b <- c <- NULL
      
    } else {
      if (str_detect(line, '^id:')) {
        a <- str_extract(line, 'GO:[0-9]{7}')
      } else if (str_detect(line, '^name:')) {
        b <- str_sub(line, start = 7)
      } else if (str_detect(line, '^namespace:')) {
        c <- str_sub(line, start = 12)
      } else if (str_detect(line, '^alt_id:')) {
        alt_a <- str_extract(line, 'GO:[0-9]{7}')
        if (!is.null(alt_a) && !is.null(b) && !is.null(c)) {
          dict_list[[length(dict_list) + 1]] <- list('GO' = alt_a, 'Description' = b, 'level' = lev_dict[[c]])
        }
      }
    }
  }
  
  go_annotation <- rbindlist(dict_list)
  go_annotation <- go_annotation[!is.na(GO) & !is.na(Description) & !is.na(level)]
  
  fwrite(go_annotation, file = output_file, sep = '\t', quote = FALSE, row.names = FALSE)
}

# Usage:
# parse_go_obo('input.obo', 'output.tsv')
