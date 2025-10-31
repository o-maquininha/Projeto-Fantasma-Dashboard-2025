if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, data.table, pROC,
  readxl, readr, ggcorrplot, cowplot,
  RColorBrewer, scales, nortest, xlsx,
  skimr, xtable, geobr, sf, ggrepel,
  abjutils, grDevices, wordcloud
)


#windowsFonts(Arial=windowsFont("sans"))

options(scipen=999)
options(OutDec = ",")

# Definindo paleta de cores da Estat
cores_estat <- c(
  "#A11D21", "#003366", "#663333", "#CC9900", "#006606",
  "#CC9966", "#999966", "#FF6600", "#008091", "#041835",
  "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat),
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',',
                                       #accuracy = 0.01,
                                       big.mark = "."))
    )
  )
}

# Definindo função que retorna frequências relativas de um vetor
percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

# Definindo função que retorna banco de dados com frequências
# relativas e absolutas de uma variável categórica
vector_frequencies <- function(vector) {
  frequency <- vector %>%
    table() %>%
    as_tibble() %>%
    mutate(
      rel = n %>%
        percent() %>%
        paste("%", sep = "")
    )
  colnames(frequency) <- c("groups", "absolute", "relative")
  return(frequency)
}



# Remover eixos do gráfico coroplético
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


## Nova função de quadro resumo
print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\setlength{\t\\tabcolsep}{9pt}
\t\\renewcommand{\t\\arraystretch}{1.20}
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular} {", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\hline\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\hline\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  
  latex <- str_c(latex, "\t\\hline
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

# Quadro simples

quadro_simples <- function(data, var_name, digits = 2) {
  var_name <- substitute(var_name)
  
  data <- data %>%
    summarize(
      `Média` = round(mean(!!sym(var_name)), digits),
      `Desvio Padrão` = round(sd(!!sym(var_name)), digits),
      `Variância` = round(var(!!sym(var_name)), digits),
      `Mínimo` = round(min(!!sym(var_name)), digits),
      `1º Quartil` = round(quantile(!!sym(var_name), probs = .25), digits),
      `Mediana` = round(quantile(!!sym(var_name), probs = .5), digits),
      `3º Quartil` = round(quantile(!!sym(var_name), probs = .75), digits),
      `Máximo` = round(max(!!sym(var_name)), digits)
    )
  
  col_count <- ncol(data)
  
  latex <- str_c(
    "|Estatística       |        Valor|\n",
    "|:-------------|---------:|\n"
  )
  
  for (i in seq_len(col_count)) {
    latex <- str_c(
      latex,
      "|", colnames(data)[i], " | ", data[[i]], "|\n"
    )
  }
  
  writeLines(latex)
}

print_quadro_simples <- function(data, var_name,
                                   title = "Principais métricas do(a) [nome da variável]",
                                   digits = 2, cb = FALSE) {
  var_name <- deparse(substitute(var_name))
  title <- str_replace(title, "\\[nome da variável\\]", var_name)
  
  chunk <- str_c(
    "```{r}\n",
    "#| echo: false\n",
    "#| results: asis\n\n",
    "cat(\"Quadro: ", title, "\\n\\n\")\n\n",
    "quadro_simples(", deparse(substitute(data)), ", ", var_name, ", digits = ", digits, ")\n",
    "```"
  )
  
  if (!cb) {
    writeLines(chunk)
  } 
  
  else {
    writeClipboard(chunk)
    message("o chunck está no Cntrl + V")
  }
}