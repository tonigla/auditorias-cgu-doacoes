# =============================================================
# analise.R
# Trabalho final de Econometria I (FGV EESP, 2026-2)
# Auditorias da CGU e doações de campanha
#
# Reproduz todas as tabelas (pasta tabelas/) e figuras (pasta figuras/)
# a partir de dados/base_final.csv. Rodar com a pasta do repositório
# como diretório de trabalho.
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

base <- read.csv("dados/base_final.csv")
dim(base)

# corrupção centrada na média: com a interação, o coeficiente de "antes"
# passa a ser o efeito para um município com corrupção média
base$corr_c <- base$corr - mean(base$corr)

# =============================================================
# 1. Estatísticas descritivas: Tabelas 1 e 2, balanceamento, Figuras 1 e 2
# =============================================================

table(base$eleicao, base$antes)

# doações em R$ mil, para a tabela
base$pj_inc_mil   <- base$pj_inc/1000
base$pj_total_mil <- base$pj_total/1000

# -------------------------------------------------------------
# Tabela 1 — sorteios, datas e municípios na amostra
# -------------------------------------------------------------
cron <- read.csv("dados/cgu_cronograma_sorteios.csv")
cron <- cron[!is.na(cron$eleicao), ]
cron <- cron[order(cron$sorteio), ]

n_amostra <- table(base$sorteio)                          # municípios na amostra final
n_doacao  <- table(base$sorteio[!is.na(base$y_share)])    # com alguma doação de empresa

tab1 <- cbind(cron$sorteio,
              format(as.Date(cron$data_sorteio), "%d/%m/%Y"),
              format(as.Date(cron$data_relatorio), "%d/%m/%Y"),
              cron$eleicao,
              ifelse(cron$antes == 1, "Tratado", "Controle"),
              n_amostra[as.character(cron$sorteio)],
              n_doacao[as.character(cron$sorteio)])

tab1 <- rbind(tab1[cron$eleicao == 2008, ],
              c("\\midrule", rep("", 6)),
              tab1[cron$eleicao == 2012, ],
              c("\\midrule", rep("", 6)),
              c("Total", "", "", "", "", nrow(base), sum(!is.na(base$y_share))))

cabecalho <- c(" & Data do & Publicação & & & Municípios & Com doação \\\\",
               "Sorteio & sorteio & do relatório & Eleição & Grupo & na amostra & de empresa \\\\",
               " & (1) & (2) & (3) & (4) & (5) & (6) \\\\")

grava_tabela(tab1, cabecalho, "ccccccc", "tabelas/tabela1.tex")

# -------------------------------------------------------------
# Tabela 2 — médias por grupo e diferença
# -------------------------------------------------------------
# A diferença de médias entre tratados e controles é o coeficiente da dummy
# numa regressão simples (Tutorial 3). O erro padrão é robusto (HC1).

variaveis <- c("y_share", "y_bin", "pj_inc_mil", "pj_total_mil", "tem_pj", "concorreu", "pref_cand",
               "falha_total", "no_os", "corr",
               "lpop", "lrenda_pc", "shurb", "gini", "tx_analf18m", "lpib_pc")

nomes <- c("Parcela do partido do prefeito",
           "Partido do prefeito recebeu doação (0/1)",
           "Valor ao partido do prefeito (R\\$ mil)",
           "Valor a todos os partidos (R\\$ mil)",
           "Município teve doação (0/1)",
           "Partido do prefeito tem candidato (0/1)",
           "Prefeito é candidato (0/1)",
           "Irregularidades médias ou graves",
           "Ordens de serviço",
           "Irregularidades por ordem de serviço",
           "População (log)",
           "Renda per capita (log)",
           "População urbana (proporção)",
           "Índice de Gini",
           "Analfabetismo adulto (\\%)",
           "PIB per capita (log)")

casas <- c(3, 3, 1, 1, 3, 3, 3, 1, 1, 3, 3, 3, 3, 3, 1, 3)

tab2 <- c()
for (i in 1:length(variaveis)) {
  v   <- variaveis[i]
  reg <- lm(base[, v] ~ base$antes)
  ct  <- coeftest(reg, vcov. = vcovHC, type = "HC1")
  y0  <- base[base$antes == 0, v]
  y1  <- base[base$antes == 1, v]
  linha <- c(paste("\\quad", nomes[i]),
             num(mean(y0, na.rm = TRUE), casas[i]),
             num(mean(y1, na.rm = TRUE), casas[i]),
             paste0(num(ct[2, 1], casas[i]), estrelas(ct[2, 4])),
             paste0("[", num(ct[2, 2], casas[i]), "]"),
             num(sd(base[, v], na.rm = TRUE), casas[i]))
  tab2 <- rbind(tab2, linha)
  cat(v, ": controle =", mean(y0, na.rm = TRUE), " tratado =", mean(y1, na.rm = TRUE),
      " dif =", ct[2, 1], " p =", ct[2, 4], "\n")
}

painel  <- function(titulo) c(paste0("\\multicolumn{6}{l}{\\textit{", titulo, "}}"), rep("", 5))
espaco  <- c("\\addlinespace", rep("", 5))

tab2 <- rbind(painel("Painel A: Doações de empresas na eleição"), tab2[1:7, ], espaco,
              painel("Painel B: Resultado da auditoria"), tab2[8:10, ], espaco,
              painel("Painel C: Características do município"), tab2[11:16, ],
              c("\\midrule", rep("", 5)),
              c("Municípios", sum(base$antes == 0), sum(base$antes == 1), "", "", nrow(base)))

cabecalho <- c(" & Relatório & Relatório & & & Desvio \\\\",
               " & depois & antes & & Erro & padrão \\\\",
               " & (controle) & (tratado) & Diferença & padrão & (amostra) \\\\",
               " & (1) & (2) & (3) & (4) & (5) \\\\")

grava_tabela(tab2, cabecalho, "lccccc", "tabelas/tabela2.tex")

sum(!is.na(base$y_share))                           # municípios com alguma doação de empresa
table(base$antes[!is.na(base$y_share)])

# -------------------------------------------------------------
# Balanceamento: as características pré-tratamento explicam "antes"?
# -------------------------------------------------------------
bal <- lm(antes ~ lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012, data = base)
coeftest(bal, vcov. = vcovHC, type = "HC1")

# H0: todos os coeficientes das características municipais são zero (teste F robusto)
teste_bal <- linearHypothesis(bal, c("lpop = 0", "lrenda_pc = 0", "shurb = 0", "gini = 0",
                                     "tx_analf18m = 0", "lpib_pc = 0"),
                              vcov. = vcovHC(bal, type = "HC1"), test = "F")
teste_bal

# a medida de corrupção (que depende do protocolo de cada sorteio) é diferente entre os grupos
summary(lm(corr ~ antes + e2012, data = base))
summary(lm(no_os ~ antes + e2012, data = base))

# -------------------------------------------------------------
# Figura 1 — linha do tempo: eleições e publicação dos relatórios
# -------------------------------------------------------------
options(OutDec = ",")

datas <- as.Date(cron$data_relatorio)
ini <- as.Date(tapply(datas, list(cron$eleicao, cron$antes), min), origin = "1970-01-01")   # ordem: 2008 controle,
fim <- as.Date(tapply(datas, list(cron$eleicao, cron$antes), max), origin = "1970-01-01")   # 2012 controle, 2008 tratado, 2012 tratado
ini; fim

rotulo <- c("Sorteios 26 a 28\n(controles)", "Sorteios 36 a 38\n(controles)",
            "Sorteios 22 a 25\n(tratados)", "Sorteios 31 a 35\n(tratados)")
cor    <- c("gray60", "gray60", "black", "black")
eleicoes <- as.Date(c("2004-10-03", "2008-10-05", "2012-10-07"))

pdf("figuras/figura1.pdf", width = 7.2, height = 2.3)
par(mar = c(0.2, 0.2, 0.2, 0.2), family = "serif", cex = 0.85)
plot(NA, xlim = as.Date(c("2004-05-01", "2014-02-01")), ylim = c(-1.25, 1.35), axes = FALSE, xlab = "", ylab = "")
arrows(as.Date("2004-05-01"), 0, as.Date("2014-02-01"), 0, length = 0.08, lwd = 1.5)
anos <- as.Date(paste0(2005:2013, "-01-01"))
segments(anos, -0.07, anos, 0.07)
segments(eleicoes, -0.25, eleicoes, 0.25, lwd = 2.5)
text(eleicoes, -0.52, c("Eleição de 2004", "Eleição de 2008", "Eleição de 2012"))
# período em que os relatórios de cada grupo foram publicados
segments(ini, 0.42, fim, 0.42, lwd = 6, col = cor, lend = 1)
text(ini + (fim - ini)/2, 0.92, rotulo, cex = 0.9)
# mandatos
arrows(as.Date(c("2005-01-01", "2009-01-01")), -0.95, as.Date(c("2008-12-31", "2012-12-31")), -0.95,
       code = 3, angle = 90, length = 0.04)
text(as.Date(c("2007-01-01", "2011-01-01")), -1.15, c("Mandato 2005-2008", "Mandato 2009-2012"), cex = 0.9)
dev.off()

# -------------------------------------------------------------
# Figura 2 — média de Y por quartil de corrupção e grupo
# -------------------------------------------------------------
media_ic <- function(y) {
  y <- y[!is.na(y)]
  m <- mean(y)
  ep <- sd(y)/sqrt(length(y))
  c(m, m - 1.96*ep, m + 1.96*ep)
}

base$quartil <- cut(base$corr, breaks = quantile(base$corr, c(0, 0.25, 0.50, 0.75, 1)),
                    labels = 1:4, include.lowest = TRUE)
table(base$quartil, base$antes)

desenha2 <- function(v, titulo, eixo) {
  g0 <- c()
  g1 <- c()
  for (q in 1:4) {
    g0 <- rbind(g0, media_ic(base[base$quartil == q & base$antes == 0, v]))
    g1 <- rbind(g1, media_ic(base[base$quartil == q & base$antes == 1, v]))
  }
  x0 <- 1:4 - 0.07
  x1 <- 1:4 + 0.07
  plot(1:4, g0[, 1], xlim = c(0.6, 4.4), ylim = c(0, 0.7), type = "n", xaxt = "n", las = 1,
       xlab = "Quartil de irregularidades por ordem de serviço", ylab = eixo,
       main = titulo, font.main = 1, cex.main = 1, bty = "l")
  abline(h = seq(0.1, 0.7, 0.1), col = "gray88", lwd = 0.6)
  arrows(x0, g0[, 2], x0, g0[, 3], angle = 90, code = 3, length = 0.03, lwd = 0.8, col = "gray45")
  arrows(x1, g1[, 2], x1, g1[, 3], angle = 90, code = 3, length = 0.03, lwd = 0.8, col = "gray45")
  lines(x0, g0[, 1], lty = 2)
  lines(x1, g1[, 1], lty = 1)
  points(x0, g0[, 1], pch = 21, bg = "white", cex = 1.4)
  points(x1, g1[, 1], pch = 16, cex = 1.4)
  axis(1, at = 1:4, labels = c("1º", "2º", "3º", "4º"), tick = FALSE, line = -0.6)
  print(round(cbind(g0[, 1], g1[, 1]), 3))
}

pdf("figuras/figura2.pdf", width = 7.2, height = 3.2)
par(mfrow = c(1, 2), mar = c(3.6, 4.2, 2.5, 0.8), mgp = c(2.4, 0.8, 0), family = "serif", cex = 0.85)
desenha2("y_share", "A. Parcela das doações de empresas\nrecebida pelo partido do prefeito", "Parcela (0 a 1)")
legend("topright", legend = c("Relatório depois da eleição (controle)", "Relatório antes da eleição (tratado)"),
       pch = c(21, 16), lty = c(2, 1), pt.bg = "white", pt.cex = 1.3, bty = "n", cex = 0.9)
desenha2("y_bin", "B. Partido do prefeito recebeu\ndoação de empresa", "Proporção de municípios")
dev.off()

options(OutDec = ".")

# =============================================================
# 2. MQO: Tabela 3
# =============================================================

mean(base$corr)
quantile(base$corr, c(0.25, 0.50, 0.75))

# a centralização também reduz a correlação entre "antes" e a interação
cor(base$antes, base$antes*base$corr)
cor(base$antes, base$antes*base$corr_c)

# -------------------------------------------------------------
# Y = parcela das doações de empresas que vai ao partido do prefeito
# -------------------------------------------------------------
# (1) diferença de médias
ols1 <- lm(y_share ~ antes, data = base)
summary(ols1)

# (2) + controles
ols2 <- lm(y_share ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
           data = base)

# (3) + corrupção e interação
ols3 <- lm(y_share ~ antes + corr_c + antes:corr_c +
             lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012, data = base)

# (4) + dummies de estado
ols4 <- lm(y_share ~ antes + corr_c + antes:corr_c +
             lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012 + as.factor(uf),
           data = base)

# heterocedasticidade
bptest(ols1); bptest(ols2); bptest(ols3); bptest(ols4)

# erros padrão robustos
coeftest(ols1, vcov. = vcovHC, type = "HC1")
coeftest(ols2, vcov. = vcovHC, type = "HC1")
coeftest(ols3, vcov. = vcovHC, type = "HC1")
coeftest(ols4, vcov. = vcovHC, type = "HC1")[1:11, ]

# intervalo de confiança de 95% do efeito na coluna (2)
ct2 <- coeftest(ols2, vcov. = vcovHC, type = "HC1")
ct2["antes", 1] - 1.96*ct2["antes", 2]
ct2["antes", 1] + 1.96*ct2["antes", 2]
sd(base$y_share, na.rm = TRUE)
2.8*ct2["antes", 2]     # efeito mínimo detectável com o erro padrão estimado

# -------------------------------------------------------------
# Y = log(1 + doações de empresas ao partido do prefeito)
# -------------------------------------------------------------
ols5 <- lm(y_log ~ antes + corr_c + antes:corr_c +
             lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012, data = base)
ols6 <- lm(y_log ~ antes + corr_c + antes:corr_c +
             lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012 + as.factor(uf),
           data = base)

bptest(ols5); bptest(ols6)
coeftest(ols5, vcov. = vcovHC, type = "HC1")
coeftest(ols6, vcov. = vcovHC, type = "HC1")[1:11, ]

# -------------------------------------------------------------
# Testes
# -------------------------------------------------------------
# H0: beta_antes = beta_antes:corr = 0 (a divulgação não tem efeito em nenhum nível de corrupção)
conjunto <- function(m) {
  linearHypothesis(m, c("antes = 0", "antes:corr_c = 0"), vcov. = vcovHC(m, type = "HC1"), test = "F")
}
conjunto(ols3); conjunto(ols4); conjunto(ols5); conjunto(ols6)

# efeito de "antes" para um município no percentil 75 de corrupção:
# beta_antes + beta_interação*(p75 - média), com a variância da combinação linear
d75 <- quantile(base$corr, 0.75) - mean(base$corr)
d75

efeito75 <- function(m) {
  V  <- vcovHC(m, type = "HC1")
  ef <- coef(m)["antes"] + d75*coef(m)["antes:corr_c"]
  ep <- sqrt(V["antes", "antes"] + d75^2*V["antes:corr_c", "antes:corr_c"] + 2*d75*V["antes", "antes:corr_c"])
  c(ef, ep)
}
efeito75(ols3); efeito75(ols4); efeito75(ols5); efeito75(ols6)

# heterogeneidade por eleição
ols_het <- lm(y_share ~ antes + e2012 + antes:e2012 +
                lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc, data = base)
coeftest(ols_het, vcov. = vcovHC, type = "HC1")

# o tratamento não muda a chance de haver doação de empresa no município
# (a amostra com y_share definido não é selecionada pelo tratamento)
coeftest(lm(tem_pj ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012, data = base),
         vcov. = vcovHC, type = "HC1")

# resultado secundário: o partido do prefeito lançou candidato?
ols_conc <- lm(concorreu ~ antes + corr_c + antes:corr_c +
                 lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012, data = base)
coeftest(ols_conc, vcov. = vcovHC, type = "HC1")

# -------------------------------------------------------------
# Tabela 3
# -------------------------------------------------------------
modelos <- list(ols1, ols2, ols3, ols4, ols5, ols6)
vars    <- c("antes", "corr_c", "antes:corr_c")

tab3 <- c()
p_bp <- c(); p_f <- c(); ef <- c(); ep <- c(); n <- c(); r2 <- c()
for (m in modelos) {
  ct   <- coeftest(m, vcov. = vcovHC, type = "HC1")
  tab3 <- cbind(tab3, celulas(ct, vars))
  n    <- c(n, nobs(m))
  r2   <- c(r2, num(summary(m)$r.squared))
  p_bp <- c(p_bp, num(bptest(m)$p.value))
  if ("antes:corr_c" %in% rownames(ct)) {
    p_f <- c(p_f, num(conjunto(m)[2, "Pr(>F)"]))
    ef  <- c(ef, num(efeito75(m)[1]))
    ep  <- c(ep, paste0("[", num(efeito75(m)[2]), "]"))
  } else {
    p_f <- c(p_f, ""); ef <- c(ef, ""); ep <- c(ep, "")
  }
}

tab3 <- rbind(cbind(c("Relatório antes da eleição", "", "Corrupção", "", "Relatório antes $\\times$ corrupção", ""), tab3),
              c("\\midrule", rep("", 6)),
              c("Efeito no percentil 75 de corrupção", ef),
              c("", ep),
              c("$H_0$: sem efeito em qualquer nível ($p$-valor)", p_f),
              c("Breusch-Pagan ($p$-valor)", p_bp),
              c("\\midrule", rep("", 6)),
              c("Controles municipais", "Não", "Sim", "Sim", "Sim", "Sim", "Sim"),
              c("\\textit{Dummy} de eleição", "Não", "Sim", "Sim", "Sim", "Sim", "Sim"),
              c("\\textit{Dummies} de estado", "Não", "Não", "Não", "Sim", "Não", "Sim"),
              c("Observações", n),
              c("$R^2$", r2))

cabecalho <- c(" & \\multicolumn{4}{c}{Parcela das doações} & \\multicolumn{2}{c}{log(1 + doações)} \\\\",
               "\\cmidrule(lr){2-5} \\cmidrule(lr){6-7}",
               " & (1) & (2) & (3) & (4) & (5) & (6) \\\\")

grava_tabela(tab3, cabecalho, "lcccccc", "tabelas/tabela3.tex")

# =============================================================
# 3. Resposta binária (MPL, probit e logit): Tabela 4
# =============================================================

mean(base$y_bin)

# -------------------------------------------------------------
# Modelo de probabilidade linear
# -------------------------------------------------------------
mpl <- lm(y_bin ~ antes + corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
          data = base)
coeftest(mpl, vcov. = vcovHC, type = "HC1")

# o MPL pode prever probabilidades fora do intervalo [0, 1]
summary(mpl$fitted.values)
fora <- sum(mpl$fitted.values < 0 | mpl$fitted.values > 1)
fora

# -------------------------------------------------------------
# Probit e logit (máxima verossimilhança)
# -------------------------------------------------------------
probit <- glm(y_bin ~ antes + corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
              family = binomial(link = "probit"), data = base)
logit  <- glm(y_bin ~ antes + corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
              family = binomial(link = "logit"), data = base)
summary(probit)
summary(logit)

# -------------------------------------------------------------
# Município típico: controles na média, eleição de 2008
# -------------------------------------------------------------
tipico0 <- data.frame(antes = 0, corr_c = 0,
                      lpop = mean(base$lpop), lrenda_pc = mean(base$lrenda_pc), shurb = mean(base$shurb),
                      gini = mean(base$gini), tx_analf18m = mean(base$tx_analf18m),
                      lpib_pc = mean(base$lpib_pc), e2012 = 0)
tipico1 <- tipico0
tipico1$antes <- 1

p0 <- c(predict(mpl, newdata = tipico0),
        predict(probit, newdata = tipico0, type = "response"),
        predict(logit, newdata = tipico0, type = "response"))
p1 <- c(predict(mpl, newdata = tipico1),
        predict(probit, newdata = tipico1, type = "response"),
        predict(logit, newdata = tipico1, type = "response"))
p0; p1
p1 - p0     # efeito de "antes" sobre a probabilidade, para o município típico

# efeito de um desvio padrão a mais de população (para comparar magnitudes)
tipico_pop <- tipico0
tipico_pop$lpop <- tipico0$lpop + sd(base$lpop)
predict(probit, newdata = tipico_pop, type = "response") - p0[2]

# teste conjunto no probit: "antes" e a interação com corrupção
probit_int <- glm(y_bin ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini +
                    tx_analf18m + lpib_pc + e2012, family = binomial(link = "probit"), data = base)
summary(probit_int)
linearHypothesis(probit_int, c("antes = 0", "antes:corr_c = 0"))

# -------------------------------------------------------------
# Tabela 4
# -------------------------------------------------------------
vars <- c("antes", "corr_c", "lpop")

tab4 <- cbind(c("Relatório antes da eleição", "", "Corrupção", "", "População (log)", ""),
              celulas(coeftest(mpl, vcov. = vcovHC, type = "HC1"), vars),
              celulas(coeftest(probit), vars),
              celulas(coeftest(logit), vars))

tab4 <- rbind(tab4,
              c("\\midrule", "", "", ""),
              c("\\multicolumn{4}{l}{\\textit{Probabilidade prevista para o município típico}}", "", "", ""),
              c("\\quad Relatório depois da eleição", num(p0)),
              c("\\quad Relatório antes da eleição", num(p1)),
              c("\\quad Diferença (efeito de \\textit{antes})", num(p1 - p0)),
              c("\\midrule", "", "", ""),
              c("Controles municipais e \\textit{dummy} de eleição", "Sim", "Sim", "Sim"),
              c("Previsões fora de $[0,1]$", fora, "--", "--"),
              c("Log-verossimilhança", "--", num(as.numeric(logLik(probit)), 1), num(as.numeric(logLik(logit)), 1)),
              c("Observações", nobs(mpl), nobs(probit), nobs(logit)))

cabecalho <- c(" & MPL & Probit & Logit \\\\",
               " & (1) & (2) & (3) \\\\")

grava_tabela(tab4, cabecalho, "lccc", "tabelas/tabela4.tex")

# =============================================================
# 4. Variável instrumental: Tabela 5
# =============================================================

dados <- base[!is.na(base$y_share), ]

table(dados$sorteado_antes, dados$antes)

# -------------------------------------------------------------
# Parte 1 — na mão, sem controles (como no Tutorial 18)
# -------------------------------------------------------------
reg_1 <- lm(antes ~ sorteado_antes, data = dados)      # primeiro estágio
summary(reg_1)
reg_fr <- lm(y_share ~ sorteado_antes, data = dados)   # forma reduzida
summary(reg_fr)

# estimador de Wald = forma reduzida / primeiro estágio
b_iv <- cov(dados$sorteado_antes, dados$y_share)/cov(dados$sorteado_antes, dados$antes)
b_iv
reg_fr$coefficients[2]/reg_1$coefficients[2]

# -------------------------------------------------------------
# Parte 2 — com controles
# -------------------------------------------------------------
mqo <- lm(y_share ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
          data = dados)

fr <- lm(y_share ~ sorteado_antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
         data = dados)

iv1 <- ivreg(y_share ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012 |
               sorteado_antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
             data = dados)

iv2 <- ivreg(y_share ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012 |
               sorteado_antes + meses_ate_eleicao + lpop + lrenda_pc + shurb + gini + tx_analf18m +
               lpib_pc + e2012,
             data = dados)

coeftest(mqo, vcov. = vcovHC, type = "HC1")
coeftest(fr,  vcov. = vcovHC, type = "HC1")
coeftest(iv1, vcov. = vcovHC, type = "HC1")
coeftest(iv2, vcov. = vcovHC, type = "HC1")

# -------------------------------------------------------------
# Relevância — primeiro estágio
# -------------------------------------------------------------
pe1 <- lm(antes ~ sorteado_antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012,
          data = dados)
pe2 <- lm(antes ~ sorteado_antes + meses_ate_eleicao + lpop + lrenda_pc + shurb + gini + tx_analf18m +
            lpib_pc + e2012, data = dados)
coeftest(pe1, vcov. = vcovHC, type = "HC1")
coeftest(pe2, vcov. = vcovHC, type = "HC1")

F1 <- linearHypothesis(pe1, "sorteado_antes = 0", vcov. = vcovHC(pe1, type = "HC1"), test = "F")
F2 <- linearHypothesis(pe2, c("sorteado_antes = 0", "meses_ate_eleicao = 0"),
                       vcov. = vcovHC(pe2, type = "HC1"), test = "F")
F1; F2

# -------------------------------------------------------------
# Exogeneidade — teste J (sobreidentificação: m - k = 2 - 1 = 1 gl)
# -------------------------------------------------------------
reg_residuos <- lm(residuals(iv2) ~ sorteado_antes + meses_ate_eleicao + lpop + lrenda_pc + shurb +
                     gini + tx_analf18m + lpib_pc + e2012, data = dados)

J <- linearHypothesis(reg_residuos, c("sorteado_antes = 0", "meses_ate_eleicao = 0"), test = "Chisq")
J
p_J <- pchisq(J[2, "Chisq"], df = 1, lower.tail = FALSE)
p_J

# -------------------------------------------------------------
# Tabela 5
# -------------------------------------------------------------
tab5 <- cbind(c("Relatório antes da eleição", "", "Sorteado antes da eleição", ""),
              celulas(coeftest(mqo, vcov. = vcovHC, type = "HC1"), c("antes", "sorteado_antes")),
              celulas(coeftest(fr,  vcov. = vcovHC, type = "HC1"), c("antes", "sorteado_antes")),
              celulas(coeftest(iv1, vcov. = vcovHC, type = "HC1"), c("antes", "sorteado_antes")),
              celulas(coeftest(iv2, vcov. = vcovHC, type = "HC1"), c("antes", "sorteado_antes")))

tab5 <- rbind(tab5,
              c("\\midrule", rep("", 4)),
              c("Instrumentos", "--", "--", "$Z_1$", "$Z_1$, $Z_2$"),
              c("Primeiro estágio: coeficiente de $Z_1$", "--", "--", num(coef(pe1)["sorteado_antes"]), num(coef(pe2)["sorteado_antes"])),
              c("Primeiro estágio: estatística $F$", "--", "--", num(F1[2, "F"], 1), num(F2[2, "F"], 1)),
              c("Teste $J$ ($p$-valor)", "--", "--", "--", num(p_J)),
              c("Controles municipais e \\textit{dummy} de eleição", "Sim", "Sim", "Sim", "Sim"),
              c("Observações", nobs(mqo), nobs(fr), nobs(iv1), nobs(iv2)))

cabecalho <- c(" & MQO & Forma reduzida & MQ2E & MQ2E \\\\",
               " & (1) & (2) & (3) & (4) \\\\")

grava_tabela(tab5, cabecalho, "lcccc", "tabelas/tabela5.tex")

# =============================================================
# 5. Robustez: Tabela 6
# =============================================================

base$lfalha_c <- base$lfalha - mean(base$lfalha)

# (1) especificação principal (coluna 4 da Tabela 3)
# (2) sem os sorteios 26 e 36: sorteados antes da eleição, com relatório depois
# (3) corrupção medida como desvio em relação à média do próprio sorteio
# (4) medida do AFF: log das irregularidades, controlando o log das ordens de serviço
# (5) só a eleição de 2008
# (6) só a eleição de 2012

sem2636 <- base[!(base$sorteio %in% c(26, 36)), ]
so2008  <- base[base$eleicao == 2008, ]
so2012  <- base[base$eleicao == 2012, ]
table(sem2636$antes); table(so2008$antes); table(so2012$antes)

# ---------------- Painel A: parcela das doações ----------------
a1 <- lm(y_share ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012 + as.factor(uf), data = base)
a2 <- lm(y_share ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012 + as.factor(uf), data = sem2636)
a3 <- lm(y_share ~ antes + corr_rel + antes:corr_rel + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012 + as.factor(uf), data = base)
a4 <- lm(y_share ~ antes + lfalha_c + antes:lfalha_c + lno_os + lpop + lrenda_pc + shurb + gini +
           tx_analf18m + lpib_pc + e2012 + as.factor(uf), data = base)
a5 <- lm(y_share ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + as.factor(uf), data = so2008)
a6 <- lm(y_share ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + as.factor(uf), data = so2012)

# ---------------- Painel B: recebeu doação (MPL) ----------------
b1 <- lm(y_bin ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012 + as.factor(uf), data = base)
b2 <- lm(y_bin ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012 + as.factor(uf), data = sem2636)
b3 <- lm(y_bin ~ antes + corr_rel + antes:corr_rel + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012 + as.factor(uf), data = base)
b4 <- lm(y_bin ~ antes + lfalha_c + antes:lfalha_c + lno_os + lpop + lrenda_pc + shurb + gini +
           tx_analf18m + lpib_pc + e2012 + as.factor(uf), data = base)
b5 <- lm(y_bin ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + as.factor(uf), data = so2008)
b6 <- lm(y_bin ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + as.factor(uf), data = so2012)

# Linhas da tabela: "antes" e a interação com a medida de corrupção
# (a interação é sempre o último coeficiente do modelo)
linhas <- function(m) {
  ct <- coeftest(m, vcov. = vcovHC, type = "HC1")
  print(ct[c(2, 3, nrow(ct)), ])
  inter <- rownames(ct)[nrow(ct)]
  celulas(ct, c("antes", inter))
}

painelA <- cbind(linhas(a1), linhas(a2), linhas(a3), linhas(a4), linhas(a5), linhas(a6))
painelB <- cbind(linhas(b1), linhas(b2), linhas(b3), linhas(b4), linhas(b5), linhas(b6))

nA <- c(nobs(a1), nobs(a2), nobs(a3), nobs(a4), nobs(a5), nobs(a6))
nB <- c(nobs(b1), nobs(b2), nobs(b3), nobs(b4), nobs(b5), nobs(b6))

rotulos <- c("\\quad Relatório antes da eleição", "", "\\quad Relatório antes $\\times$ corrupção", "")

tab6 <- rbind(c("\\multicolumn{7}{l}{\\textit{Painel A: Parcela das doações ao partido do prefeito}}", rep("", 6)),
              cbind(rotulos, painelA),
              c("\\quad Observações", nA),
              c("\\addlinespace", rep("", 6)),
              c("\\multicolumn{7}{l}{\\textit{Painel B: Partido do prefeito recebeu doação (MPL)}}", rep("", 6)),
              cbind(rotulos, painelB),
              c("\\quad Observações", nB))

cabecalho <- c(" & Especificação & Sem sorteios & Corrupção rel. & Medida & Só & Só \\\\",
               " & principal & 26 e 36 & ao sorteio & do AFF & 2008 & 2012 \\\\",
               " & (1) & (2) & (3) & (4) & (5) & (6) \\\\")

grava_tabela(tab6, cabecalho, "lcccccc", "tabelas/tabela6.tex")

# sem os municípios com eleição suplementar no mandato (prefeito trocado)
sem_sup <- base[base$suplementar == 0, ]
coeftest(lm(y_share ~ antes + corr_c + antes:corr_c + lpop + lrenda_pc + shurb + gini + tx_analf18m +
              lpib_pc + e2012 + as.factor(uf), data = sem_sup),
         vcov. = vcovHC, type = "HC1")[c("antes", "corr_c", "antes:corr_c"), ]

# =============================================================
# 6. Explicações alternativas: Tabela 7
# =============================================================

# -------------------------------------------------------------
# (1) O prefeito é candidato: afastamento e lealdade deveriam ser mais fortes
# -------------------------------------------------------------
table(base$pref_cand, base$concorreu)    # 88 prefeitos concorrem por outra legenda

# a decisão de concorrer não muda com a divulgação
coeftest(lm(pref_cand ~ antes + e2012, data = base), vcov. = vcovHC, type = "HC1")

m1 <- lm(y_share ~ antes + antes:pref_cand + pref_cand + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012, data = base)
ct1 <- coeftest(m1, vcov. = vcovHC, type = "HC1")
ct1

# efeito quando o prefeito é candidato: antes + antes:pref_cand
linearHypothesis(m1, "antes + antes:pref_cand = 0", vcov. = vcovHC(m1, type = "HC1"), test = "F")

# -------------------------------------------------------------
# (2) O partido não é o prefeito: parcela do partido pelo qual o prefeito concorre
# -------------------------------------------------------------
m2 <- lm(y_share_pref ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc + e2012, data = base)
ct2 <- coeftest(m2, vcov. = vcovHC, type = "HC1")
ct2

# -------------------------------------------------------------
# (3) A informação envelheceu: relatórios publicados até 12 meses antes da eleição e antes disso
# -------------------------------------------------------------
dia_eleicao <- as.Date(ifelse(base$eleicao == 2008, "2008-10-05", "2012-10-07"))
base$meses_relatorio <- as.numeric(dia_eleicao - as.Date(base$data_relatorio))/30
round(tapply(base$meses_relatorio, base$sorteio, mean), 1)

base$antes_recente <- ifelse(base$antes == 1 & base$meses_relatorio <= 12, 1, 0)
base$antes_antigo  <- ifelse(base$antes == 1 & base$meses_relatorio > 12, 1, 0)
table(base$antes_recente, base$antes_antigo)

m3 <- lm(y_share ~ antes_recente + antes_antigo + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012, data = base)
ct3 <- coeftest(m3, vcov. = vcovHC, type = "HC1")
ct3
linearHypothesis(m3, "antes_recente = antes_antigo", vcov. = vcovHC(m3, type = "HC1"), test = "F")

# -------------------------------------------------------------
# (4) A doação não era visível: em 2008 o nome do doador só saía depois da eleição;
#     em 2012 passou a sair durante a campanha
# -------------------------------------------------------------
m4 <- lm(y_share ~ antes + antes:e2012 + lpop + lrenda_pc + shurb + gini + tx_analf18m +
           lpib_pc + e2012, data = base)
ct4 <- coeftest(m4, vcov. = vcovHC, type = "HC1")
ct4

# -------------------------------------------------------------
# (5) e (6) Forças opostas: construtoras (dependem da prefeitura) e demais empresas, só 2012
# -------------------------------------------------------------
so2012 <- base[base$eleicao == 2012, ]
so2012$y_bin_obras  <- ifelse(so2012$obras_inc > 0, 1, 0)
so2012$y_bin_outros <- ifelse(so2012$pj_inc - so2012$obras_inc > 0, 1, 0)
mean(so2012$y_bin_obras); mean(so2012$y_bin_outros)

m5 <- lm(y_bin_obras ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc, data = so2012)
m6 <- lm(y_bin_outros ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc, data = so2012)
ct5 <- coeftest(m5, vcov. = vcovHC, type = "HC1")
ct6 <- coeftest(m6, vcov. = vcovHC, type = "HC1")
ct5; ct6

# a mesma comparação com a parcela das doações de cada grupo de empresas (amostras pequenas)
sum(!is.na(so2012$y_share_obras)); sum(!is.na(so2012$y_share_outros))
coeftest(lm(y_share_obras ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc, data = so2012),
         vcov. = vcovHC, type = "HC1")
coeftest(lm(y_share_outros ~ antes + lpop + lrenda_pc + shurb + gini + tx_analf18m + lpib_pc, data = so2012),
         vcov. = vcovHC, type = "HC1")

# -------------------------------------------------------------
# Tabela 7
# -------------------------------------------------------------
vars <- c("antes", "antes:pref_cand", "antes:e2012", "antes_recente", "antes_antigo")

tab7 <- cbind(c("Relatório antes da eleição", "",
                "Relatório antes $\\times$ prefeito candidato", "",
                "Relatório antes $\\times$ eleição de 2012", "",
                "Relatório até 12 meses antes", "",
                "Relatório mais de 12 meses antes", ""),
              celulas(ct1, vars), celulas(ct2, vars), celulas(ct3, vars),
              celulas(ct4, vars), celulas(ct5, vars), celulas(ct6, vars))

media_y <- c(mean(base$y_share, na.rm = TRUE), mean(base$y_share_pref, na.rm = TRUE),
             mean(base$y_share, na.rm = TRUE), mean(base$y_share, na.rm = TRUE),
             mean(so2012$y_bin_obras), mean(so2012$y_bin_outros))

tab7 <- rbind(tab7,
              c("\\midrule", rep("", 6)),
              c("Média da variável dependente", num(media_y)),
              c("Amostra", "Todas", "Todas", "Todas", "Todas", "2012", "2012"),
              c("Observações", nobs(m1), nobs(m2), nobs(m3), nobs(m4), nobs(m5), nobs(m6)))

cabecalho <- c(" & & Parcela do & & & Recebeu de & Recebeu de \\\\",
               "Variável dependente: & Parcela & partido atual & Parcela & Parcela & construtora & outra empresa \\\\",
               " & (1) & (2) & (3) & (4) & (5) & (6) \\\\")

grava_tabela(tab7, cabecalho, "lcccccc", "tabelas/tabela7.tex")
