# =============================================================
# analise.R — Auditorias da CGU e doações de campanha (Econometria I, FGV EESP)
# Gera as Tabelas 1 a 7 e as Figuras 1 e 2 a partir de dados/base_final.csv.
# Rodar com a pasta do repositório como diretório de trabalho.
# =============================================================
library(AER); library(car); library(lmtest)

# controles municipais (Censo 2000 e PIB 2005), usados em quase todas as regressões
ctrl <- c("lpop", "lrenda_pc", "shurb", "gini", "tx_analf18m", "lpib_pc")
X <- paste(ctrl, collapse = " + ")
# atalhos: reg() roda MQO a partir de uma fórmula em texto; rob() dá os erros padrão robustos (HC1)
reg <- function(f, d = base) lm(as.formula(f), data = d)
rob <- function(m) coeftest(m, vcov. = vcovHC, type = "HC1")
# funções das tabelas em LaTeX: número com vírgula, estrelas de significância,
# coeficiente com o erro padrão embaixo entre colchetes e o tabular completo
num <- function(x, d = 3) { s <- formatC(abs(x), format = "f", digits = d, decimal.mark = ","); ifelse(x < 0, paste0("$-$", s), s) }
estrelas <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
celulas <- function(ct, vars) unlist(lapply(vars, function(v) if (v %in% rownames(ct))
  c(paste0(num(ct[v, 1]), estrelas(ct[v, 4])), paste0("[", num(ct[v, 2]), "]")) else c("", "")))
grava_tabela <- function(tab, cab, col, arq) {
  a <- tab[, 1]
  corpo <- ifelse(a %in% c("\\midrule", "\\addlinespace"), a, paste(ifelse(grepl("multicolumn", a), a, apply(tab, 1, paste, collapse = " & ")), "\\\\"))
  writeLines(c(paste0("\\begin{tabular}{", col, "}"), "\\toprule", cab, "\\midrule", corpo, "\\bottomrule", "\\end{tabular}"), arq)
}

# ---------------- Dados ----------------
dir.create("tabelas", showWarnings = FALSE); dir.create("figuras", showWarnings = FALSE)
base <- read.csv("dados/base_final.csv")
# corrupção centrada na média: com a interação, o coeficiente de antes é o efeito
# no município de corrupção média. C = controles + dummy de eleição
base$corr_c <- base$corr - mean(base$corr); C <- paste(X, "+ e2012")
base$pj_inc_mil <- base$pj_inc/1000; base$pj_total_mil <- base$pj_total/1000

# ---------------- Tabela 1: sorteios, datas e amostra ----------------
cron <- read.csv("dados/cgu_cronograma_sorteios.csv")
cron <- cron[!is.na(cron$eleicao), ]; cron <- cron[order(cron$sorteio), ]
s <- as.character(cron$sorteio); mid <- c("\\midrule", rep("", 6))
tab1 <- cbind(cron$sorteio, format(as.Date(cron$data_sorteio), "%d/%m/%Y"), format(as.Date(cron$data_relatorio), "%d/%m/%Y"),
              cron$eleicao, ifelse(cron$antes == 1, "Tratado", "Controle"), table(base$sorteio)[s], table(base$sorteio[!is.na(base$y_share)])[s])
tab1 <- rbind(tab1[cron$eleicao == 2008, ], mid, tab1[cron$eleicao == 2012, ], mid, c("Total", "", "", "", "", nrow(base), sum(!is.na(base$y_share))))
grava_tabela(tab1, c(" & Data do & Publicação & & & Municípios & Com doação \\\\",
                     "Sorteio & sorteio & do relatório & Eleição & Grupo & na amostra & de empresa \\\\",
                     " & (1) & (2) & (3) & (4) & (5) & (6) \\\\"), "ccccccc", "tabelas/tabela1.tex")

# ---------------- Tabela 2: médias por grupo ----------------
# a diferença de médias é o coeficiente da dummy antes numa regressão simples (Tutorial 3)
v <- c("y_share", "y_bin", "pj_inc_mil", "pj_total_mil", "tem_pj", "concorreu", "pref_cand", "falha_total", "no_os", "corr", ctrl)
nomes <- c("Parcela do partido do prefeito", "Partido do prefeito recebeu doação (0/1)", "Valor ao partido do prefeito (R\\$ mil)",
           "Valor a todos os partidos (R\\$ mil)", "Município teve doação (0/1)", "Partido do prefeito tem candidato (0/1)",
           "Prefeito é candidato (0/1)", "Irregularidades médias ou graves", "Ordens de serviço", "Irregularidades por ordem de serviço",
           "População (log)", "Renda per capita (log)", "População urbana (proporção)", "Índice de Gini", "Analfabetismo adulto (\\%)", "PIB per capita (log)")
casas <- c(3, 3, 1, 1, 3, 3, 3, 1, 1, 3, 3, 3, 3, 3, 1, 3)
tab2 <- t(sapply(1:16, function(i) {
  x <- base[, v[i]]; d <- casas[i]; ct <- rob(lm(x ~ base$antes))
  c(paste("\\quad", nomes[i]), num(mean(x[base$antes == 0], na.rm = TRUE), d), num(mean(x[base$antes == 1], na.rm = TRUE), d),
    paste0(num(ct[2, 1], d), estrelas(ct[2, 4])), paste0("[", num(ct[2, 2], d), "]"), num(sd(x, na.rm = TRUE), d))
}))
# a tabela é dividida em três painéis: doações, auditoria e município
painel <- function(t) c(paste0("\\multicolumn{6}{l}{\\textit{", t, "}}"), rep("", 5))
esp <- c("\\addlinespace", rep("", 5))
tab2 <- rbind(painel("Painel A: Doações de empresas na eleição"), tab2[1:7, ], esp, painel("Painel B: Resultado da auditoria"), tab2[8:10, ], esp,
              painel("Painel C: Características do município"), tab2[11:16, ], c("\\midrule", rep("", 5)),
              c("Municípios", sum(base$antes == 0), sum(base$antes == 1), "", "", nrow(base)))
grava_tabela(tab2, c(" & Relatório & Relatório & & & Desvio \\\\", " & depois & antes & & Erro & padrão \\\\",
                     " & (controle) & (tratado) & Diferença & padrão & (amostra) \\\\", " & (1) & (2) & (3) & (4) & (5) \\\\"), "lccccc", "tabelas/tabela2.tex")

# ---------------- Balanceamento ----------------
# se o sorteio funcionou, as características do município não explicam o tratamento (teste F robusto)
table(base$antes[!is.na(base$y_share)])
bal <- reg(paste("antes ~", X, "+ e2012"))
rob(bal)
linearHypothesis(bal, paste(ctrl, "= 0"), vcov. = vcovHC(bal, type = "HC1"), test = "F")
# o número de ordens de serviço caiu nos sorteios recentes, o que muda a medida de corrupção
summary(lm(corr ~ antes + e2012, data = base))
summary(lm(no_os ~ antes + e2012, data = base))
# média de ordens de serviço antes e a partir do 34º sorteio
tapply(base$no_os, base$sorteio >= 34, mean)

# ---------------- Figura 1: linha do tempo ----------------
options(OutDec = ",")
# período em que saíram os relatórios de cada grupo, em cada eleição
datas <- as.Date(cron$data_relatorio); g <- list(cron$eleicao, cron$antes)
ini <- as.Date(tapply(datas, g, min), origin = "1970-01-01"); fim <- as.Date(tapply(datas, g, max), origin = "1970-01-01")
eleicoes <- as.Date(c("2004-10-03", "2008-10-05", "2012-10-07"))
pdf("figuras/figura1.pdf", width = 7.2, height = 2.3)
par(mar = c(0.2, 0.2, 0.2, 0.2), family = "serif", cex = 0.85)
plot(NA, xlim = as.Date(c("2004-05-01", "2014-02-01")), ylim = c(-1.25, 1.35), axes = FALSE, xlab = "", ylab = "")
arrows(as.Date("2004-05-01"), 0, as.Date("2014-02-01"), 0, length = 0.08, lwd = 1.5)
anos <- as.Date(paste0(2005:2013, "-01-01")); segments(anos, -0.07, anos, 0.07)
segments(eleicoes, -0.25, eleicoes, 0.25, lwd = 2.5)
text(eleicoes, -0.52, c("Eleição de 2004", "Eleição de 2008", "Eleição de 2012"))
segments(ini, 0.42, fim, 0.42, lwd = 6, col = c("gray60", "gray60", "black", "black"), lend = 1)
text(ini + (fim - ini)/2, 0.92, c("Sorteios 26 a 28\n(controles)", "Sorteios 36 a 38\n(controles)", "Sorteios 22 a 25\n(tratados)", "Sorteios 31 a 35\n(tratados)"), cex = 0.9)
arrows(as.Date(c("2005-01-01", "2009-01-01")), -0.95, as.Date(c("2008-12-31", "2012-12-31")), -0.95, code = 3, angle = 90, length = 0.04)
text(as.Date(c("2007-01-01", "2011-01-01")), -1.15, c("Mandato 2005-2008", "Mandato 2009-2012"), cex = 0.9)
dev.off()

# ---------------- Figura 2: médias por quartil de corrupção ----------------
# média e IC de 95% de Y por quartil, tratados e controles separados (como em Ferraz e Finan, 2008)
media_ic <- function(y) { y <- y[!is.na(y)]; m <- mean(y); ep <- sd(y)/sqrt(length(y)); c(m, m - 1.96*ep, m + 1.96*ep) }
base$quartil <- cut(base$corr, breaks = quantile(base$corr, c(0, 0.25, 0.50, 0.75, 1)), labels = 1:4, include.lowest = TRUE)
desenha2 <- function(v, titulo, eixo) {
  g <- lapply(0:1, function(a) t(sapply(1:4, function(q) media_ic(base[base$quartil == q & base$antes == a, v]))))
  x <- list(1:4 - 0.07, 1:4 + 0.07)
  plot(1:4, g[[1]][, 1], xlim = c(0.6, 4.4), ylim = c(0, 0.7), type = "n", xaxt = "n", las = 1, xlab = "Quartil de irregularidades por ordem de serviço",
       ylab = eixo, main = titulo, font.main = 1, cex.main = 1, bty = "l")
  abline(h = seq(0.1, 0.7, 0.1), col = "gray88", lwd = 0.6)
  for (k in 1:2) arrows(x[[k]], g[[k]][, 2], x[[k]], g[[k]][, 3], angle = 90, code = 3, length = 0.03, lwd = 0.8, col = "gray45")
  lines(x[[1]], g[[1]][, 1], lty = 2); lines(x[[2]], g[[2]][, 1], lty = 1)
  points(x[[1]], g[[1]][, 1], pch = 21, bg = "white", cex = 1.4); points(x[[2]], g[[2]][, 1], pch = 16, cex = 1.4)
  axis(1, at = 1:4, labels = c("1º", "2º", "3º", "4º"), tick = FALSE, line = -0.6)
  print(round(cbind(g[[1]][, 1], g[[2]][, 1]), 3))
}
pdf("figuras/figura2.pdf", width = 7.2, height = 3.2)
par(mfrow = c(1, 2), mar = c(3.6, 4.2, 2.5, 0.8), mgp = c(2.4, 0.8, 0), family = "serif", cex = 0.85)
desenha2("y_share", "A. Parcela das doações de empresas\nrecebida pelo partido do prefeito", "Parcela (0 a 1)")
legend("topright", legend = c("Relatório depois da eleição (controle)", "Relatório antes da eleição (tratado)"),
       pch = c(21, 16), lty = c(2, 1), pt.bg = "white", pt.cex = 1.3, bty = "n", cex = 0.9)
desenha2("y_bin", "B. Partido do prefeito recebeu\ndoação de empresa", "Proporção de municípios")
dev.off()
options(OutDec = ".")

# ---------------- Tabela 3: MQO ----------------
# centralizar corr reduz a correlação entre antes e a interação
mean(base$corr)
c(cor(base$antes, base$antes*base$corr), cor(base$antes, base$antes*base$corr_c))
# colunas: (1) diferença de médias, (2) + controles, (3) + corrupção e interação, (4) + dummies de estado;
# (5) e (6) repetem (3) e (4) com log(1 + valor doado)
I <- paste("antes + corr_c + antes:corr_c +", C)
ols <- list(lm(y_share ~ antes, data = base), reg(paste("y_share ~ antes +", C)), reg(paste("y_share ~", I)),
            reg(paste("y_share ~", I, "+ as.factor(uf)")), reg(paste("y_log ~", I)), reg(paste("y_log ~", I, "+ as.factor(uf)")))
# IC de 95% do efeito e efeito mínimo detectável (2,8 erros padrão), coluna 2
ct2 <- rob(ols[[2]])
ct2["antes", 1] + c(-1.96, 1.96)*ct2["antes", 2]
2.8*ct2["antes", 2]
# a divulgação muda a chance de haver doação no município? e de o partido lançar candidato?
rob(reg(paste("tem_pj ~ antes +", C)))["antes", ]
rob(reg(paste("concorreu ~", I)))["antes", ]
# teste F conjunto: a divulgação não tem efeito em nenhum nível de corrupção
conjunto <- function(m) linearHypothesis(m, c("antes = 0", "antes:corr_c = 0"), vcov. = vcovHC(m, type = "HC1"), test = "F")
# efeito no percentil 75 de corrupção, com o erro padrão da combinação linear
(d75 <- quantile(base$corr, 0.75) - mean(base$corr))
efeito75 <- function(m) {
  V <- vcovHC(m, type = "HC1")
  c(coef(m)["antes"] + d75*coef(m)["antes:corr_c"], sqrt(V["antes", "antes"] + d75^2*V["antes:corr_c", "antes:corr_c"] + 2*d75*V["antes", "antes:corr_c"]))
}
e75 <- sapply(ols[3:6], efeito75)
# monta a tabela: coeficientes, testes e linhas de especificação
tab3 <- rbind(cbind(c("Relatório antes da eleição", "", "Corrupção", "", "Relatório antes $\\times$ corrupção", ""),
                    sapply(ols, function(m) celulas(rob(m), c("antes", "corr_c", "antes:corr_c")))),
              c("\\midrule", rep("", 6)),
              c("Efeito no percentil 75 de corrupção", "", "", num(e75[1, ])),
              c("", "", "", paste0("[", num(e75[2, ]), "]")),
              c("$H_0$: sem efeito em qualquer nível ($p$-valor)", "", "", sapply(ols[3:6], function(m) num(conjunto(m)[2, "Pr(>F)"]))),
              c("Breusch-Pagan ($p$-valor)", sapply(ols, function(m) num(bptest(m)$p.value))),
              c("\\midrule", rep("", 6)),
              c("Controles municipais", "Não", rep("Sim", 5)),
              c("\\textit{Dummy} de eleição", "Não", rep("Sim", 5)),
              c("\\textit{Dummies} de estado", "Não", "Não", "Não", "Sim", "Não", "Sim"),
              c("Observações", sapply(ols, nobs)),
              c("$R^2$", sapply(ols, function(m) num(summary(m)$r.squared))))
grava_tabela(tab3, c(" & \\multicolumn{4}{c}{Parcela das doações} & \\multicolumn{2}{c}{log(1 + doações)} \\\\",
                     "\\cmidrule(lr){2-5} \\cmidrule(lr){6-7}", " & (1) & (2) & (3) & (4) & (5) & (6) \\\\"), "lcccccc", "tabelas/tabela3.tex")

# ---------------- Tabela 4: resposta binária ----------------
# Y = partido do prefeito recebeu doação; MPL por MQO, probit e logit por máxima verossimilhança
f <- as.formula(paste("y_bin ~ antes + corr_c +", X, "+ e2012"))
mpl <- lm(f, data = base)
probit <- glm(f, family = binomial(link = "probit"), data = base)
logit <- glm(f, family = binomial(link = "logit"), data = base)
# o MPL pode prever probabilidades fora de [0, 1]
summary(mpl$fitted.values)
fora <- sum(mpl$fitted.values < 0 | mpl$fitted.values > 1)
# probabilidade prevista para um município típico (controles na média, 2008), com e sem divulgação
t0 <- data.frame(antes = 0, corr_c = 0, lapply(base[ctrl], mean), e2012 = 0); t1 <- t0; t1$antes <- 1
pr <- function(d) c(predict(mpl, d), predict(probit, d, type = "response"), predict(logit, d, type = "response"))
p0 <- pr(t0); p1 <- pr(t1)
# para comparar: efeito de um desvio padrão a mais de população no probit
tp <- t0; tp$lpop <- t0$lpop + sd(base$lpop)
predict(probit, tp, type = "response") - p0[2]
# teste conjunto no probit, com a interação
linearHypothesis(glm(as.formula(paste("y_bin ~ antes + corr_c + antes:corr_c +", X, "+ e2012")), family = binomial(link = "probit"), data = base),
                 c("antes = 0", "antes:corr_c = 0"))
v <- c("antes", "corr_c", "lpop"); l <- c("\\midrule", "", "", "")
tab4 <- rbind(cbind(c("Relatório antes da eleição", "", "Corrupção", "", "População (log)", ""),
                    celulas(rob(mpl), v), celulas(coeftest(probit), v), celulas(coeftest(logit), v)), l,
              c("\\multicolumn{4}{l}{\\textit{Probabilidade prevista para o município típico}}", "", "", ""),
              c("\\quad Relatório depois da eleição", num(p0)), c("\\quad Relatório antes da eleição", num(p1)),
              c("\\quad Diferença (efeito de \\textit{antes})", num(p1 - p0)), l,
              c("Controles municipais e \\textit{dummy} de eleição", "Sim", "Sim", "Sim"),
              c("Previsões fora de $[0,1]$", fora, "--", "--"),
              c("Log-verossimilhança", "--", num(as.numeric(logLik(probit)), 1), num(as.numeric(logLik(logit)), 1)),
              c("Observações", nobs(mpl), nobs(probit), nobs(logit)))
grava_tabela(tab4, c(" & MPL & Probit & Logit \\\\", " & (1) & (2) & (3) \\\\"), "lccc", "tabelas/tabela4.tex")

# ---------------- Tabela 5: variável instrumental ----------------
# Z1 = sorteado antes da eleição; Z2 = meses entre o sorteio e a eleição. Os sorteios 26 e 36
# foram sorteados antes, mas publicados depois, por isso Z1 e antes não coincidem
dados <- base[!is.na(base$y_share), ]
table(dados$sorteado_antes, dados$antes)
# estimador de Wald na mão (Tutorial 18): forma reduzida / primeiro estágio
cov(dados$sorteado_antes, dados$y_share)/cov(dados$sorteado_antes, dados$antes)
Z <- "sorteado_antes + meses_ate_eleicao"
mqo <- reg(paste("y_share ~ antes +", C), dados)
fr <- reg(paste("y_share ~ sorteado_antes +", C), dados)
iv1 <- ivreg(as.formula(paste("y_share ~ antes +", C, "| sorteado_antes +", C)), data = dados)
iv2 <- ivreg(as.formula(paste("y_share ~ antes +", C, "|", Z, "+", C)), data = dados)
# estimativa com cada instrumento separado (o teste J compara as duas)
iv_z2 <- ivreg(as.formula(paste("y_share ~ antes +", C, "| meses_ate_eleicao +", C)), data = dados)
c(coef(iv1)["antes"], coef(iv_z2)["antes"])
# relevância: F do primeiro estágio
pe1 <- reg(paste("antes ~ sorteado_antes +", C), dados)
pe2 <- reg(paste("antes ~", Z, "+", C), dados)
F1 <- linearHypothesis(pe1, "sorteado_antes = 0", vcov. = vcovHC(pe1, type = "HC1"), test = "F")
F2 <- linearHypothesis(pe2, c("sorteado_antes = 0", "meses_ate_eleicao = 0"), vcov. = vcovHC(pe2, type = "HC1"), test = "F")
# exogeneidade: teste J (resíduo do MQ2E contra os instrumentos, 1 grau de liberdade)
dados$u <- residuals(iv2)
J <- linearHypothesis(reg(paste("u ~", Z, "+", C), dados), c("sorteado_antes = 0", "meses_ate_eleicao = 0"), test = "Chisq")
v <- c("antes", "sorteado_antes")
tab5 <- rbind(cbind(c("Relatório antes da eleição", "", "Sorteado antes da eleição", ""), sapply(list(mqo, fr, iv1, iv2), function(m) celulas(rob(m), v))),
              c("\\midrule", rep("", 4)),
              c("Instrumentos", "--", "--", "$Z_1$", "$Z_1$, $Z_2$"),
              c("Primeiro estágio: coeficiente de $Z_1$", "--", "--", num(coef(pe1)["sorteado_antes"]), num(coef(pe2)["sorteado_antes"])),
              c("Primeiro estágio: estatística $F$", "--", "--", num(F1[2, "F"], 1), num(F2[2, "F"], 1)),
              c("Teste $J$ ($p$-valor)", "--", "--", "--", num(pchisq(J[2, "Chisq"], df = 1, lower.tail = FALSE))),
              c("Controles municipais e \\textit{dummy} de eleição", "Sim", "Sim", "Sim", "Sim"),
              c("Observações", nobs(mqo), nobs(fr), nobs(iv1), nobs(iv2)))
grava_tabela(tab5, c(" & MQO & Forma reduzida & MQ2E & MQ2E \\\\", " & (1) & (2) & (3) & (4) \\\\"), "lcccc", "tabelas/tabela5.tex")

# ---------------- Tabela 6: robustez ----------------
base$lfalha_c <- base$lfalha - mean(base$lfalha)
# esp() roda a especificação completa trocando a medida de corrupção e a amostra. Cada item de cfg
# é uma coluna: principal, sem sorteios 26 e 36, corrupção relativa ao sorteio, medida do AFF, só 2008, só 2012
esp <- function(y, c, d, extra = "", e = " + e2012") reg(paste0(y, " ~ antes + ", c, " + antes:", c, extra, " + ", X, e, " + as.factor(uf)"), d)
cfg <- list(list("corr_c", base), list("corr_c", base[!(base$sorteio %in% c(26, 36)), ]), list("corr_rel", base),
            list("lfalha_c", base, " + lno_os"), list("corr_c", base[base$eleicao == 2008, ], e = ""), list("corr_c", base[base$eleicao == 2012, ], e = ""))
rot <- c("\\quad Relatório antes da eleição", "", "\\quad Relatório antes $\\times$ corrupção", "")
# painel(): roda as 6 colunas para um Y e guarda antes e a interação (último coeficiente)
painel <- function(y, titulo) {
  ms <- lapply(cfg, function(k) do.call(esp, c(y, k)))
  rbind(c(paste0("\\multicolumn{7}{l}{\\textit{", titulo, "}}"), rep("", 6)),
        cbind(rot, sapply(ms, function(m) { ct <- rob(m); celulas(ct, c("antes", rownames(ct)[nrow(ct)])) })),
        c("\\quad Observações", sapply(ms, nobs)))
}
tab6 <- rbind(painel("y_share", "Painel A: Parcela das doações ao partido do prefeito"), c("\\addlinespace", rep("", 6)),
              painel("y_bin", "Painel B: Partido do prefeito recebeu doação (MPL)"))
grava_tabela(tab6, c(" & Especificação & Sem sorteios & Corrupção rel. & Medida & Só & Só \\\\", " & principal & 26 e 36 & ao sorteio & do AFF & 2008 & 2012 \\\\",
                     " & (1) & (2) & (3) & (4) & (5) & (6) \\\\"), "lcccccc", "tabelas/tabela6.tex")
# sem os 17 municípios com eleição suplementar
rob(esp("y_share", "corr_c", base[base$suplementar == 0, ]))[c("antes", "corr_c", "antes:corr_c"), ]

# ---------------- Tabela 7: explicações alternativas ----------------
# (1) prefeito candidato (88 prefeitos concorrem por outra legenda)
table(base$pref_cand, base$concorreu)
rob(lm(pref_cand ~ antes + e2012, data = base))
m1 <- reg(paste("y_share ~ antes + antes:pref_cand + pref_cand +", C))
linearHypothesis(m1, "antes + antes:pref_cand = 0", vcov. = vcovHC(m1, type = "HC1"), test = "F")
# (2) parcela do partido pelo qual o prefeito concorre
m2 <- reg(paste("y_share_pref ~ antes +", C))
# (3) informação envelhecida: relatório até 12 meses antes da eleição contra mais antigo
meses <- as.numeric(as.Date(ifelse(base$eleicao == 2008, "2008-10-05", "2012-10-07")) - as.Date(base$data_relatorio))/30
base$antes_recente <- ifelse(base$antes == 1 & meses <= 12, 1, 0)
base$antes_antigo <- ifelse(base$antes == 1 & meses > 12, 1, 0)
m3 <- reg(paste("y_share ~ antes_recente + antes_antigo +", C))
linearHypothesis(m3, "antes_recente = antes_antigo", vcov. = vcovHC(m3, type = "HC1"), test = "F")
# (4) visibilidade: em 2008 o doador só era divulgado depois da eleição; em 2012, durante a campanha
m4 <- reg(paste("y_share ~ antes + antes:e2012 +", C))
# (5) e (6) construtoras contra outras empresas, só 2012 (o TSE informa o setor do doador)
s12 <- base[base$eleicao == 2012, ]
s12$y_bin_obras <- ifelse(s12$obras_inc > 0, 1, 0)
s12$y_bin_outros <- ifelse(s12$pj_inc - s12$obras_inc > 0, 1, 0)
m5 <- reg(paste("y_bin_obras ~ antes +", X), s12)
m6 <- reg(paste("y_bin_outros ~ antes +", X), s12)
rob(reg(paste("y_share_obras ~ antes +", X), s12))
rob(reg(paste("y_share_outros ~ antes +", X), s12))
# monta a tabela
ms <- list(m1, m2, m3, m4, m5, m6)
y <- mean(base$y_share, na.rm = TRUE)
media <- c(y, mean(base$y_share_pref, na.rm = TRUE), y, y, mean(s12$y_bin_obras), mean(s12$y_bin_outros))
tab7 <- rbind(cbind(c("Relatório antes da eleição", "", "Relatório antes $\\times$ prefeito candidato", "", "Relatório antes $\\times$ eleição de 2012", "",
                      "Relatório até 12 meses antes", "", "Relatório mais de 12 meses antes", ""),
                    sapply(ms, function(m) celulas(rob(m), c("antes", "antes:pref_cand", "antes:e2012", "antes_recente", "antes_antigo")))),
              c("\\midrule", rep("", 6)),
              c("Média da variável dependente", num(media)),
              c("Amostra", rep("Todas", 4), "2012", "2012"),
              c("Observações", sapply(ms, nobs)))
grava_tabela(tab7, c(" & & Parcela do & & & Recebeu de & Recebeu de \\\\",
                     "Variável dependente: & Parcela & partido atual & Parcela & Parcela & construtora & outra empresa \\\\",
                     " & (1) & (2) & (3) & (4) & (5) & (6) \\\\"), "lcccccc", "tabelas/tabela7.tex")

# números citados no texto da Seção 7: efeito para prefeito candidato e meses entre relatório e eleição
k <- c("antes", "antes:pref_cand"); V <- vcovHC(m1, type = "HC1")
c(sum(coef(m1)[k]), sqrt(sum(V[k, k])))
range(meses[base$antes == 1])
# Seção 7.5: entre os controles, sorteados antes da eleição (26 e 36) contra sorteados depois
controles <- base[base$antes == 0, ]
table(controles$sorteado_antes)
rob(reg(paste("y_share ~ sorteado_antes +", C), controles))["sorteado_antes", ]
rob(reg(paste("y_bin ~ sorteado_antes +", C), controles))["sorteado_antes", ]
