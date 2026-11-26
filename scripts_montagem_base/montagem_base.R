# =============================================================
# montagem_base.R — monta dados/base_final.csv (uma linha por município)
# Lê as bases de origem em dados/ (saída do montagem_base.py).
# =============================================================
base <- merge(read.csv("dados/aff_corrupcao.csv"), read.csv("dados/cgu_cronograma_sorteios.csv"), by = "sorteio")
# amostra: sorteios das janelas de 2008 e 2012, município identificado, corrupção medida e só a primeira auditoria
base <- base[!is.na(base$eleicao) & base$pareamento_ok == "True" & !is.na(base$falha_total) & base$auditado_antes == 0, ]
base <- base[order(base$sorteio), ]; base <- base[!duplicated(base$codigo_ibge), ]
# confere a regra da primeira auditoria com o histórico completo da CGU (tem que dar 0)
cgu <- read.csv("dados/cgu_sorteios.csv")
sum(tapply(cgu$sorteio, cgu$codigo_ibge, min)[as.character(base$codigo_ibge)] < base$sorteio, na.rm = TRUE)
# efeito mínimo detectável, em desvios padrão de Y (5% de significância, 80% de poder)
2.8*sqrt(1/sum(base$antes == 1) + 1/sum(base$antes == 0))


# partido do prefeito: o do eleito na eleição anterior (2004 ou 2008)
# siglas que mudaram de nome entre 2004 e 2008
rec <- c(PFL = "DEM", PL = "PR", PRONA = "PR", PAN = "PTB")
inc <- function(ano) {
  v <- read.csv(paste0("dados/tse_votacao_prefeito_", ano, ".csv"))
  if (ano == 2004) v$SG_PARTIDO <- ifelse(v$SG_PARTIDO %in% names(rec), rec[v$SG_PARTIDO], v$SG_PARTIDO)
  # onde houve eleição suplementar (tipo 1), vale o eleito nela
  v <- v[v$DS_SIT_TOT_TURNO == "ELEITO", ]; v <- v[order(v$codigo_ibge, v$CD_TIPO_ELEICAO), ]; v <- v[!duplicated(v$codigo_ibge), ]
  data.frame(codigo_ibge = v$codigo_ibge, eleicao = ano + 4, partido_inc = v$SG_PARTIDO, prefeito = v$NM_CANDIDATO, suplementar = ifelse(v$CD_TIPO_ELEICAO == 1, 1, 0))
}
base <- merge(base, rbind(inc(2004), inc(2008)), by = c("codigo_ibge", "eleicao"), all.x = TRUE)
table(base$suplementar)


# doações: só receitas de pessoas jurídicas. Repasses de comitês e partidos ficam fora
# para não contar o mesmo dinheiro duas vezes
le <- function(f, col) { d <- read.csv(paste0("dados/tse_receitas_", f, ".csv")); d[grepl("pessoas jur", d[[col]], ignore.case = TRUE), ] }
rec08 <- le("cand_prefeito_2008", "DS_TITULO"); com08 <- le("comites_2008", "DS_TITULO")
# comitê só de vereador não entra (ficam o de prefeito e o único)
com08 <- com08[!grepl("Vereador", com08$DS_ORGAO), ]
com12 <- le("comites_2012", "Tipo.receita"); com12 <- com12[!grepl("Vereador", com12$Tipo.comite), ]
# em 2012 o TSE informa o setor econômico do doador
d12 <- function(d) data.frame(codigo_ibge = d$codigo_ibge, partido = d$Sigla..Partido, valor = d$Valor.receita, setor = d[, grep("^Setor", names(d))])
pj12 <- rbind(d12(le("cand_prefeito_2012", "Tipo.receita")), d12(com12), d12(le("partidos_2012", "Tipo.receita")))
pj <- rbind(data.frame(codigo_ibge = c(rec08$codigo_ibge, com08$codigo_ibge), partido = c(rec08$SG_PARTIDO, com08$SG_PARTIDO),
                       valor = c(rec08$VR_RECEITA, com08$VR_RECEITA), eleicao = 2008), data.frame(pj12[1:3], eleicao = 2012))
# soma por município e partido: o do prefeito (pj_inc) e o total do município (pj_total)
pj_partido <- setNames(aggregate(valor ~ codigo_ibge + eleicao + partido, data = pj, FUN = sum), c("codigo_ibge", "eleicao", "partido_inc", "pj_inc"))
base <- merge(base, pj_partido, by = c("codigo_ibge", "eleicao", "partido_inc"), all.x = TRUE)
base <- merge(base, setNames(aggregate(valor ~ codigo_ibge + eleicao, data = pj, FUN = sum), c("codigo_ibge", "eleicao", "pj_total")), by = c("codigo_ibge", "eleicao"), all.x = TRUE)
# construtoras e obras contra as demais empresas (só 2012)
pj12$obras <- ifelse(grepl("^Constru|^Obras|^Incorpora|engenharia|^Perfura|^Demoli", pj12$setor) & !grepl("embarca", pj12$setor), 1, 0)
round(tapply(pj12$valor, pj12$obras, sum)/sum(pj12$valor), 3)
o <- pj12[pj12$obras == 1, ]
base <- merge(base, setNames(aggregate(valor ~ codigo_ibge + partido, data = o, FUN = sum), c("codigo_ibge", "partido_inc", "obras_inc")), by = c("codigo_ibge", "partido_inc"), all.x = TRUE)
base <- merge(base, setNames(aggregate(valor ~ codigo_ibge, data = o, FUN = sum), c("codigo_ibge", "obras_total")), by = "codigo_ibge", all.x = TRUE)
# quem não aparece nas receitas não recebeu doação de empresa
for (v in c("pj_inc", "pj_total", "obras_inc", "obras_total")) base[is.na(base[[v]]), v] <- 0
base$y_share_obras <- ifelse(base$eleicao == 2012 & base$obras_total > 0, base$obras_inc/base$obras_total, NA)
base$y_share_outros <- ifelse(base$eleicao == 2012 & base$pj_total - base$obras_total > 0, (base$pj_inc - base$obras_inc)/(base$pj_total - base$obras_total), NA)
# variáveis dependentes: parcela, recebeu doação (0/1) e log do valor
base$y_share <- ifelse(base$pj_total > 0, base$pj_inc/base$pj_total, NA)
base$y_bin <- ifelse(base$pj_inc > 0, 1, 0)
base$y_log <- log(1 + base$pj_inc)
base$tem_pj <- ifelse(base$pj_total > 0, 1, 0)


# o partido lançou candidato? o próprio prefeito é candidato de novo?
cand <- lapply(c(2008, 2012), function(a) { d <- read.csv(paste0("dados/tse_candidatos_prefeito_", a, ".csv"))
  d <- d[d$CD_TIPO_ELEICAO == 2 & d$DS_SITUACAO_CANDIDATURA == "APTO", ]; d$eleicao <- a; d })
cand <- rbind(cand[[1]][c("codigo_ibge", "eleicao", "NM_CANDIDATO", "SG_PARTIDO")], cand[[2]][c("codigo_ibge", "eleicao", "NM_CANDIDATO", "SG_PARTIDO")])
base$concorreu <- ifelse(paste(base$codigo_ibge, base$eleicao, base$partido_inc) %in% paste(cand$codigo_ibge, cand$eleicao, cand$SG_PARTIDO), 1, 0)
# o TSE grava o mesmo nome com e sem acento conforme o ano, então tiramos os acentos
sem_acento <- function(x) toupper(iconv(x, to = "ASCII//TRANSLIT"))
cands <- data.frame(codigo_ibge = cand$codigo_ibge, eleicao = cand$eleicao, nome = sem_acento(cand$NM_CANDIDATO), partido_cand = cand$SG_PARTIDO)
base$nome <- sem_acento(base$prefeito)
base <- merge(base, cands[!duplicated(cands[1:3]), ], by = c("codigo_ibge", "eleicao", "nome"), all.x = TRUE)
base$pref_cand <- ifelse(is.na(base$partido_cand), 0, 1)
# partido pelo qual o prefeito concorre (se não concorre, vale o da eleição anterior)
base$partido_pref <- ifelse(base$pref_cand == 1, base$partido_cand, base$partido_inc)
base <- merge(base, setNames(pj_partido, c("codigo_ibge", "eleicao", "partido_pref", "pj_pref")), by = c("codigo_ibge", "eleicao", "partido_pref"), all.x = TRUE)
base$pj_pref[is.na(base$pj_pref)] <- 0
base$y_share_pref <- ifelse(base$pj_total > 0, base$pj_pref/base$pj_total, NA)


# corrupção: irregularidades por ordem de serviço auditada, e medidas alternativas
base$corr <- base$falha_total/base$no_os
base$corr_irr <- base$nirregular/base$no_os
base$corr_mis <- base$mismanagement/base$no_os
base$lfalha <- log(base$falha_total)
round(tapply(base$no_os, base$sorteio, mean), 1)
# desvio em relação à média do próprio sorteio (o escopo das auditorias mudou com o tempo)
base$corr_rel <- base$corr - ave(base$corr, base$sorteio)
# instrumentos da Tabela 5: sorteado antes da eleição e meses até a eleição
dia <- as.Date(ifelse(base$eleicao == 2008, "2008-10-05", "2012-10-07"))
base$sorteado_antes <- ifelse(as.Date(base$data_sorteio) < dia, 1, 0)
base$meses_ate_eleicao <- as.numeric(dia - as.Date(base$data_sorteio))/30

# controles: PIB per capita de 2005 e dummy de eleição
base <- merge(base, read.csv("dados/pib_2005.csv"), by = "codigo_ibge", all.x = TRUE)
base$lpib_pc <- log(base$pib_2005_mil_reais*1000/exp(base$lpop))
base$e2012 <- ifelse(base$eleicao == 2012, 1, 0)
base$uf <- base$uf_municipio
# salva só as colunas usadas na análise
base <- base[order(base$codigo_ibge), c("codigo_ibge", "uf", "sorteio", "data_sorteio", "data_relatorio", "eleicao", "e2012", "antes",
  "sorteado_antes", "meses_ate_eleicao", "partido_inc", "suplementar", "concorreu", "pref_cand", "pj_inc", "pj_total", "y_share", "y_bin",
  "y_log", "tem_pj", "y_share_pref", "obras_inc", "obras_total", "y_share_obras", "y_share_outros", "falha_total", "nirregular",
  "mismanagement", "no_os", "lno_os", "corr", "corr_irr", "corr_mis", "corr_rel", "lfalha", "lpop", "lrenda_pc", "shurb", "gini", "tx_analf18m", "lpib_pc")]
write.csv(base, "dados/base_final.csv", row.names = FALSE)
