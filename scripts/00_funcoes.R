# =============================================================
# 00_funcoes.R
# Pacotes e funções pequenas usadas para montar as tabelas em LaTeX
# =============================================================

library(AER)     # ivreg(), coeftest(), vcovHC()
library(car)     # linearHypothesis()
library(lmtest)  # bptest(), coeftest()

# número com vírgula decimal (padrão brasileiro)
num <- function(x, d = 3) {
  s <- formatC(abs(x), format = "f", digits = d, decimal.mark = ",")
  ifelse(x < 0, paste0("$-$", s), s)
}

estrelas <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
}

# Para cada variável de "vars": coeficiente (com estrelas) e, na linha de baixo,
# o erro padrão entre colchetes. ct é a saída de coeftest().
celulas <- function(ct, vars) {
  saida <- c()
  for (v in vars) {
    if (v %in% rownames(ct)) {
      saida <- c(saida,
                 paste0(num(ct[v, 1]), estrelas(ct[v, 4])),
                 paste0("[", num(ct[v, 2]), "]"))
    } else {
      saida <- c(saida, "", "")
    }
  }
  saida
}

# Grava uma matriz de texto como tabular do LaTeX.
# Uma linha cuja primeira célula é "\\midrule" vira um traço horizontal e "\\addlinespace",
# um espaço; uma linha de título de painel (multicolumn) é escrita sozinha.
grava_tabela <- function(tab, cabecalho, colunas, arquivo) {
  corpo <- c()
  for (i in 1:nrow(tab)) {
    if (tab[i, 1] %in% c("\\midrule", "\\addlinespace")) {
      corpo <- c(corpo, tab[i, 1])
    } else if (grepl("multicolumn", tab[i, 1])) {
      corpo <- c(corpo, paste(tab[i, 1], "\\\\"))
    } else {
      corpo <- c(corpo, paste(paste(tab[i, ], collapse = " & "), "\\\\"))
    }
  }
  writeLines(c(paste0("\\begin{tabular}{", colunas, "}"), "\\toprule",
               cabecalho, "\\midrule", corpo, "\\bottomrule", "\\end{tabular}"),
             arquivo)
}
