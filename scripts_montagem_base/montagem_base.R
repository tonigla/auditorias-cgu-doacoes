# =============================================================
# montagem_base.R
# Monta a base final do trabalho: uma linha por município
# Auditorias da CGU e doações de empresas ao partido do prefeito
# =============================================================

# Lê as bases de origem em dados/ (saída do montagem_base.py) e grava dados/base_final.csv.
# Rodar com a pasta do repositório como diretório de trabalho.

# -------------------------------------------------------------
# 1. Amostra: auditorias (AFF) + cronograma dos sorteios
# -------------------------------------------------------------
aff  <- read.csv("dados/aff_corrupcao.csv")
cron <- read.csv("dados/cgu_cronograma_sorteios.csv")

base <- merge(aff, cron, by = "sorteio")
nrow(base)

base <- base[!is.na(base$eleicao), ]           # sorteios 29 e 30 ficam fora (cobrem duas gestões)
base <- base[base$pareamento_ok == "True", ]   # município identificado com segurança
base <- base[!is.na(base$falha_total), ]       # corrupção medida
base <- base[base$auditado_antes == 0, ]       # só a primeira auditoria do município

# cada município aparece uma única vez
base <- base[order(base$sorteio), ]
base <- base[!duplicated(base$codigo_ibge), ]

table(base$eleicao, base$antes)
table(base$sorteio)

# conferindo a regra da primeira auditoria com o histórico completo da CGU
cgu <- read.csv("dados/cgu_sorteios.csv")
primeiro <- aggregate(sorteio ~ codigo_ibge, data = cgu, FUN = min)
names(primeiro)[2] <- "primeiro_sorteio"
confere <- merge(base, primeiro, by = "codigo_ibge")
sum(confere$primeiro_sorteio < confere$sorteio)   # tem que dar 0

# efeito mínimo detectável (em desvios padrão de Y): 5% de significância, 80% de poder
n1 <- sum(base$antes == 1)
n0 <- sum(base$antes == 0)
2.8*sqrt(1/n1 + 1/n0)

# -------------------------------------------------------------
# 2. Partido do prefeito em exercício (incumbente)
# -------------------------------------------------------------
vot04 <- read.csv("dados/tse_votacao_prefeito_2004.csv")
vot08 <- read.csv("dados/tse_votacao_prefeito_2008.csv")

# partidos que mudaram de nome ou se fundiram entre 2004 e 2008
vot04$SG_PARTIDO[vot04$SG_PARTIDO == "PFL"] <- "DEM"
vot04$SG_PARTIDO[vot04$SG_PARTIDO %in% c("PL", "PRONA")] <- "PR"
vot04$SG_PARTIDO[vot04$SG_PARTIDO == "PAN"] <- "PTB"

# "ELEITO" já é o vencedor final (quem só passou ao 2º turno aparece como "2º TURNO")
eleito04 <- vot04[vot04$DS_SIT_TOT_TURNO == "ELEITO", ]
eleito08 <- vot08[vot08$DS_SIT_TOT_TURNO == "ELEITO", ]

# CD_TIPO_ELEICAO: 1 = suplementar, 2 = ordinária.
# Onde houve eleição suplementar, quem está no cargo é o eleito nela,
# por isso ela vem primeiro na ordenação
eleito04 <- eleito04[order(eleito04$codigo_ibge, eleito04$CD_TIPO_ELEICAO), ]
eleito08 <- eleito08[order(eleito08$codigo_ibge, eleito08$CD_TIPO_ELEICAO), ]
eleito04 <- eleito04[!duplicated(eleito04$codigo_ibge), ]
eleito08 <- eleito08[!duplicated(eleito08$codigo_ibge), ]

inc04 <- data.frame(codigo_ibge = eleito04$codigo_ibge,
                    eleicao     = 2008,
                    partido_inc = eleito04$SG_PARTIDO,
                    prefeito    = eleito04$NM_CANDIDATO,
                    suplementar = ifelse(eleito04$CD_TIPO_ELEICAO == 1, 1, 0))
inc08 <- data.frame(codigo_ibge = eleito08$codigo_ibge,
                    eleicao     = 2012,
                    partido_inc = eleito08$SG_PARTIDO,
                    prefeito    = eleito08$NM_CANDIDATO,
                    suplementar = ifelse(eleito08$CD_TIPO_ELEICAO == 1, 1, 0))

base <- merge(base, rbind(inc04, inc08), by = c("codigo_ibge", "eleicao"), all.x = TRUE)
sum(is.na(base$partido_inc))
table(base$suplementar)

# -------------------------------------------------------------
# 3. Doações de pessoas jurídicas por partido e município
# -------------------------------------------------------------
rec08  <- read.csv("dados/tse_receitas_cand_prefeito_2008.csv")
com08  <- read.csv("dados/tse_receitas_comites_2008.csv")
rec12  <- read.csv("dados/tse_receitas_cand_prefeito_2012.csv")
com12  <- read.csv("dados/tse_receitas_comites_2012.csv")
part12 <- read.csv("dados/tse_receitas_partidos_2012.csv")

table(rec08$DS_TITULO)
table(rec12$Tipo.receita)

# Só a doação original da empresa. Repasses ("Recursos de outros candidatos/comitês",
# "Recursos de partido político") ficam de fora para não contar o mesmo dinheiro duas vezes.
rec08  <- rec08[grepl("pessoas jur", rec08$DS_TITULO, ignore.case = TRUE), ]
com08  <- com08[grepl("pessoas jur", com08$DS_TITULO, ignore.case = TRUE), ]
rec12  <- rec12[grepl("pessoas jur", rec12$Tipo.receita, ignore.case = TRUE), ]
com12  <- com12[grepl("pessoas jur", com12$Tipo.receita, ignore.case = TRUE), ]
part12 <- part12[grepl("pessoas jur", part12$Tipo.receita, ignore.case = TRUE), ]

# comitê exclusivo de vereador não entra (ficam o de prefeito e o único)
table(com08$DS_ORGAO)
table(com12$Tipo.comite)
com08 <- com08[!grepl("Vereador", com08$DS_ORGAO), ]
com12 <- com12[!grepl("Vereador", com12$Tipo.comite), ]

pj08 <- rbind(data.frame(codigo_ibge = rec08$codigo_ibge, partido = rec08$SG_PARTIDO, valor = rec08$VR_RECEITA),
              data.frame(codigo_ibge = com08$codigo_ibge, partido = com08$SG_PARTIDO, valor = com08$VR_RECEITA))
pj08$eleicao <- 2008

# em 2012 o TSE informa o setor econômico (CNAE) do doador; a coluna começa com "Setor"
pj12 <- rbind(data.frame(codigo_ibge = rec12$codigo_ibge,  partido = rec12$Sigla..Partido,  valor = rec12$Valor.receita,
                         setor = rec12[, grep("^Setor", names(rec12))]),
              data.frame(codigo_ibge = com12$codigo_ibge,  partido = com12$Sigla..Partido,  valor = com12$Valor.receita,
                         setor = com12[, grep("^Setor", names(com12))]),
              data.frame(codigo_ibge = part12$codigo_ibge, partido = part12$Sigla..Partido, valor = part12$Valor.receita,
                         setor = part12[, grep("^Setor", names(part12))]))
pj12$eleicao <- 2012

pj <- rbind(pj08, pj12[, c("codigo_ibge", "partido", "valor", "eleicao")])

pj_partido <- aggregate(valor ~ codigo_ibge + eleicao + partido, data = pj, FUN = sum)
pj_total   <- aggregate(valor ~ codigo_ibge + eleicao, data = pj, FUN = sum)
names(pj_partido) <- c("codigo_ibge", "eleicao", "partido_inc", "pj_inc")
names(pj_total)   <- c("codigo_ibge", "eleicao", "pj_total")

base <- merge(base, pj_partido, by = c("codigo_ibge", "eleicao", "partido_inc"), all.x = TRUE)
base <- merge(base, pj_total,   by = c("codigo_ibge", "eleicao"), all.x = TRUE)

# município/partido que não aparece nas receitas de PJ não recebeu doação de empresa
base$pj_inc[is.na(base$pj_inc)]     <- 0
base$pj_total[is.na(base$pj_total)] <- 0

# Doadores de 2012 separados em dois grupos: construção civil e obras
# (setor que mais depende de contratos com a prefeitura) e os demais
pj12$obras <- ifelse(grepl("^Constru|^Obras|^Incorpora|engenharia|^Perfura|^Demoli", pj12$setor) &
                       !grepl("embarca", pj12$setor), 1, 0)
round(tapply(pj12$valor, pj12$obras, sum)/sum(pj12$valor), 3)

obras_partido <- aggregate(valor ~ codigo_ibge + partido, data = pj12[pj12$obras == 1, ], FUN = sum)
obras_total   <- aggregate(valor ~ codigo_ibge, data = pj12[pj12$obras == 1, ], FUN = sum)
names(obras_partido) <- c("codigo_ibge", "partido_inc", "obras_inc")
names(obras_total)   <- c("codigo_ibge", "obras_total")

base <- merge(base, obras_partido, by = c("codigo_ibge", "partido_inc"), all.x = TRUE)
base <- merge(base, obras_total,   by = "codigo_ibge", all.x = TRUE)
base$obras_inc[is.na(base$obras_inc)]     <- 0
base$obras_total[is.na(base$obras_total)] <- 0

# parcela do partido do prefeito entre os doadores de cada grupo (só 2012)
base$y_share_obras  <- ifelse(base$eleicao == 2012 & base$obras_total > 0, base$obras_inc/base$obras_total, NA)
base$y_share_outros <- ifelse(base$eleicao == 2012 & base$pj_total - base$obras_total > 0,
                              (base$pj_inc - base$obras_inc)/(base$pj_total - base$obras_total), NA)
summary(base$y_share_obras); summary(base$y_share_outros)

# -------------------------------------------------------------
# 4. Variáveis dependentes
# -------------------------------------------------------------
base$y_share <- ifelse(base$pj_total > 0, base$pj_inc/base$pj_total, NA)   # parcela do partido do prefeito
base$y_bin   <- ifelse(base$pj_inc > 0, 1, 0)                              # recebeu alguma doação de PJ
base$y_log   <- log(1 + base$pj_inc)
base$tem_pj  <- ifelse(base$pj_total > 0, 1, 0)                            # houve doação de PJ no município

# o partido do prefeito lançou candidato a prefeito na eleição seguinte?
cand08 <- read.csv("dados/tse_candidatos_prefeito_2008.csv")
cand12 <- read.csv("dados/tse_candidatos_prefeito_2012.csv")
cand08 <- cand08[cand08$CD_TIPO_ELEICAO == 2 & cand08$DS_SITUACAO_CANDIDATURA == "APTO", ]
cand12 <- cand12[cand12$CD_TIPO_ELEICAO == 2 & cand12$DS_SITUACAO_CANDIDATURA == "APTO", ]

chave_cand <- c(paste(cand08$codigo_ibge, 2008, cand08$SG_PARTIDO),
                paste(cand12$codigo_ibge, 2012, cand12$SG_PARTIDO))
base$concorreu <- ifelse(paste(base$codigo_ibge, base$eleicao, base$partido_inc) %in% chave_cand, 1, 0)

# o próprio prefeito é candidato de novo? (mesmo nome na lista de candidatos do município)
# o TSE grava o mesmo nome com e sem acento conforme o ano, então tiramos os acentos antes de comparar
sem_acento <- function(x) toupper(iconv(x, to = "ASCII//TRANSLIT"))

cands <- rbind(data.frame(codigo_ibge = cand08$codigo_ibge, eleicao = 2008,
                          nome = sem_acento(cand08$NM_CANDIDATO), partido_cand = cand08$SG_PARTIDO),
               data.frame(codigo_ibge = cand12$codigo_ibge, eleicao = 2012,
                          nome = sem_acento(cand12$NM_CANDIDATO), partido_cand = cand12$SG_PARTIDO))
cands <- cands[!duplicated(cands[, c("codigo_ibge", "eleicao", "nome")]), ]

base$nome <- sem_acento(base$prefeito)
base <- merge(base, cands, by = c("codigo_ibge", "eleicao", "nome"), all.x = TRUE)
base$pref_cand <- ifelse(is.na(base$partido_cand), 0, 1)

# Parte dos prefeitos concorre por outra legenda. Medida alternativa: parcela das doações
# que vai para o partido pelo qual o prefeito concorre (se ele não concorre, vale o partido da eleição anterior)
base$partido_pref <- ifelse(base$pref_cand == 1, base$partido_cand, base$partido_inc)
table(base$partido_pref == base$partido_inc, base$pref_cand)

names(pj_partido) <- c("codigo_ibge", "eleicao", "partido_pref", "pj_pref")
base <- merge(base, pj_partido, by = c("codigo_ibge", "eleicao", "partido_pref"), all.x = TRUE)
base$pj_pref[is.na(base$pj_pref)] <- 0
base$y_share_pref <- ifelse(base$pj_total > 0, base$pj_pref/base$pj_total, NA)
table(base$pref_cand, base$concorreu)

# -------------------------------------------------------------
# 5. Corrupção
# -------------------------------------------------------------
# irregularidades por ordem de serviço auditada
base$corr      <- base$falha_total/base$no_os
base$corr_irr  <- base$nirregular/base$no_os       # todas as irregularidades (robustez)
base$corr_mis  <- base$mismanagement/base$no_os    # só falhas de gestão
base$lfalha    <- log(base$falha_total)            # especificação do AFF (com lno_os de controle)

# o número de ordens de serviço por auditoria caiu ao longo dos sorteios,
# então a média de corr muda de sorteio para sorteio
round(tapply(base$corr, base$sorteio, mean), 2)
round(tapply(base$no_os, base$sorteio, mean), 1)
base$corr_rel <- base$corr - ave(base$corr, base$sorteio)   # desvio em relação à média do próprio sorteio

# -------------------------------------------------------------
# 5b. Variáveis do exercício de variável instrumental (seção 4 do analise.R)
# -------------------------------------------------------------
dia_eleicao <- as.Date(ifelse(base$eleicao == 2008, "2008-10-05", "2012-10-07"))
base$sorteado_antes    <- ifelse(as.Date(base$data_sorteio) < dia_eleicao, 1, 0)
base$meses_ate_eleicao <- as.numeric(dia_eleicao - as.Date(base$data_sorteio))/30

table(base$sorteado_antes, base$antes)   # sorteios 26 e 36: sorteados antes, relatório depois

# -------------------------------------------------------------
# 6. Controles
# -------------------------------------------------------------
pib <- read.csv("dados/pib_2005.csv")
base <- merge(base, pib, by = "codigo_ibge", all.x = TRUE)

# PIB de 2005 (em mil R$) por habitante do Censo 2000
base$lpib_pc <- log(base$pib_2005_mil_reais*1000/exp(base$lpop))

base$e2012 <- ifelse(base$eleicao == 2012, 1, 0)
base$uf    <- base$uf_municipio

# -------------------------------------------------------------
# 7. Salvar
# -------------------------------------------------------------
base <- base[, c("codigo_ibge", "uf", "sorteio", "data_sorteio", "data_relatorio", "eleicao", "e2012", "antes",
                 "sorteado_antes", "meses_ate_eleicao",
                 "partido_inc", "suplementar", "concorreu", "pref_cand",
                 "pj_inc", "pj_total", "y_share", "y_bin", "y_log", "tem_pj",
                 "y_share_pref", "obras_inc", "obras_total", "y_share_obras", "y_share_outros",
                 "falha_total", "nirregular", "mismanagement", "no_os", "lno_os",
                 "corr", "corr_irr", "corr_mis", "corr_rel", "lfalha",
                 "lpop", "lrenda_pc", "shurb", "gini", "tx_analf18m", "lpib_pc")]
base <- base[order(base$codigo_ibge), ]

dim(base)
summary(base)

write.csv(base, "dados/base_final.csv", row.names = FALSE)
