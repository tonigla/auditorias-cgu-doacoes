# =============================================================
# montagem_base.py — consolida as bases brutas (etapa anterior ao montagem_base.R)
# Uso: python montagem_base.py <pasta_brutos> <pasta_saida> [pasta_tse]
# =============================================================
import sys, os, re, unicodedata
import numpy as np, pandas as pd
from scipy.optimize import linear_sum_assignment
RAW, OUT = sys.argv[1:3]; TSE = sys.argv[3] if len(sys.argv) > 3 else RAW; os.makedirs(OUT, exist_ok=True)

# nome em maiúsculas, sem acento nem pontuação, para cruzar CGU e TSE pelo nome
def norm(s):
    s = unicodedata.normalize('NFKD', str(s).upper())
    return ' '.join(re.sub(r"[^A-Z ]", ' ', ''.join(c for c in s if not unicodedata.combining(c))).split())

# ---------------- chave TSE <-> IBGE ----------------
chave = pd.read_csv(f'{RAW}/municipios_tse_ibge.csv')
chave['nome_norm'] = chave.nome_municipio.map(norm)
chave[['codigo_ibge', 'codigo_tse', 'uf', 'nome_municipio', 'capital']].to_csv(f'{OUT}/chave_municipios.csv', index=False)

# ---------------- sorteios da CGU (a lista só traz nome e UF) ----------------
cgu = pd.read_csv(f'{RAW}/cgu_sorteios.csv')
cgu['municipio_cgu'] = cgu.municipio
# no sorteio 1 o nome vem como "Região X - Município"
cgu['municipio'] = cgu.municipio.str.replace(r'^Região .*? - ', '', regex=True)
# grafias da CGU que diferem da tabela do TSE, conferidas uma a uma
correcoes = {('Boninau', 'BA'): 'BONINAL', ('Pedra Branca do Amapari', 'AP'): 'AGUA BRANCA DO AMAPARI',
             ('Pedra Branca do Amaparí', 'AP'): 'AGUA BRANCA DO AMAPARI', ('Caldas Brandrão', 'PB'): 'CALDAS BRANDAO',
             ('Gracho Cardoso', 'SE'): 'GRACCHO CARDOSO', ('Açu', 'RN'): 'ASSU', ('Carreiro da Várzea', 'AM'): 'CAREIRO DA VARZEA',
             ('Presidente Juscelino', 'RN'): 'SERRA CAIADA', ('São Caetano', 'PE'): 'SAO CAITANO', ('Jardim dos Angicos', 'RN'): 'JARDIM DE ANGICOS',
             ('Muquém de São Francisco', 'BA'): 'MUQUEM DO SAO FRANCISCO', ('Campo de Santana', 'PB'): 'TACIMA',
             ('Estrela dOeste', 'SP'): 'ESTRELA D OESTE', ('Florínia', 'SP'): 'FLORINEA', ('Santa Teresinha', 'BA'): 'SANTA TEREZINHA',
             ('São João do Meriti', 'RJ'): 'SAO JOAO DE MERITI', ('Piçarras', 'SC'): 'BALNEARIO PICARRAS',
             ('Armação de Búzios', 'RJ'): 'ARMACAO DOS BUZIOS', ('Goianópolis', 'GO'): 'GOIANAPOLIS',
             ('Balneário Arroio do Silva', 'SC'): 'BALNEARIO ARROIO DO SILVA'}
cgu['nome_norm'] = [correcoes.get((m, u), norm(m)) for m, u in zip(cgu.municipio, cgu.uf)]
# Rondônia: a CGU escreve "D'Oeste" e o TSE "DO OESTE"
ro = cgu.uf.eq('RO')
cgu.loc[ro, 'nome_norm'] = cgu.loc[ro, 'nome_norm'].str.replace(r'\bD OESTE$', 'DO OESTE', regex=True)
cgu = cgu.merge(chave[['nome_norm', 'uf', 'codigo_ibge']], on=['nome_norm', 'uf'], how='left')
cgu['data_sorteio'] = pd.to_datetime(cgu.data_sorteio, dayfirst=True).dt.date
cgu['codigo_ibge'] = cgu.codigo_ibge.astype('Int64')
cgu[['codigo_ibge', 'sorteio', 'data_sorteio', 'ordem', 'uf', 'municipio_cgu', 'url']].to_csv(f'{OUT}/cgu_sorteios.csv', index=False)

# ---------------- Atlas 2000/2010 (uma coluna por variável e ano) ----------------
at = pd.read_csv(f'{RAW}/atlas_ipeadata_2000_2010.csv')
at['col'] = at.serie.str.replace('ADH_', '').str.lower() + '_' + at.ano_censo.astype(str)
atw = at.pivot(index='codigo_ibge', columns='col', values='valor').reset_index()
atw.to_csv(f'{OUT}/atlas_2000_2010.csv', index=False)

# ---------------- PIB 2005 (SIDRA) ----------------
pib = pd.read_excel(f'{RAW}/SIDRA_PIB.xlsx', header=None, skiprows=5).iloc[:-1]
pib.columns = ['codigo_ibge', 'nome_sidra', 'pib_2005_mil_reais']
pib = pib[pib.codigo_ibge.astype(str).str.len() == 7].copy()
pib['codigo_ibge'] = pib.codigo_ibge.astype(int)
pib['pib_2005_mil_reais'] = pd.to_numeric(pib.pib_2005_mil_reais, errors='coerce')
pib[['codigo_ibge', 'pib_2005_mil_reais']].to_csv(f'{OUT}/pib_2005.csv', index=False)

# ---------------- AFF: recuperar o município de cada auditoria ----------------
# Os arquivos de Avis, Ferraz e Finan (2018) são anonimizados: só têm sorteio, código de estado e Censo 2000.
# uf_id segue a ordem alfabética das UFs; o 10 junta toda a região Norte
aff = pd.read_stata(f'{RAW}/tableA2.dta', convert_categoricals=False)
UFS = ['AL', 'BA', 'CE', 'ES', 'GO', 'MA', 'MG', 'MS', 'MT', 'PA', 'PB', 'PE', 'PI', 'PR', 'RJ', 'RN', 'RS', 'SC', 'SE', 'SP']
aff['uf'] = aff.uf_id.astype(int).map(lambda i: UFS[i - 1])
aff['grupo_uf'] = aff.uf.where(aff.uf != 'PA', 'NORTE')
aff['id_aff'] = np.arange(len(aff))
cgu['grupo_uf'] = cgu.uf.where(~cgu.uf.isin({'PA', 'AC', 'AM', 'AP', 'RO', 'RR', 'TO'}), 'NORTE')
cand = cgu.dropna(subset=['codigo_ibge']).merge(atw[['codigo_ibge', 'pesotot_2000', 'gini_2000', 'rdpc_2000', 'pesourb_2000']], on='codigo_ibge', how='left')
# em cada sorteio e estado, pareia cada auditoria a um município sorteado, sem repetição (algoritmo húngaro)
pares = []
for (s, u), g in aff.groupby(['sorteio', 'grupo_uf']):
    c = cand[(cand.sorteio == s) & (cand.grupo_uf == u)].reset_index(drop=True)
    if c.empty: continue
    sh = c.pesourb_2000.values / c.pesotot_2000.values
    # custo: distância em log da população, Gini e participação urbana
    C = np.nan_to_num(np.abs(np.log(c.pesotot_2000.values)[None, :] - g.lpop.values[:, None]) / 0.05
                      + np.abs(c.gini_2000.values[None, :] - g.gini.values[:, None]) / 0.01 + np.abs(sh[None, :] - g.shurb.values[:, None]) / 0.01, nan=999)
    pares += [(g.id_aff.iloc[a], int(c.codigo_ibge.iloc[b]), c.uf.iloc[b], C[a, b], c.pesourb_2000.iloc[b] / c.pesotot_2000.iloc[b])
              for a, b in zip(*linear_sum_assignment(C))]
aff = aff.merge(pd.DataFrame(pares, columns=['id_aff', 'codigo_ibge', 'uf_municipio', 'dist_pareamento', 'shurb_atlas']), on='id_aff', how='left')
# validação: a participação urbana vem do mesmo Censo nas duas fontes, então o par certo é idêntico
aff['dif_shurb'] = (aff.shurb - aff.shurb_atlas).abs()
aff['pareamento_ok'] = aff.dif_shurb < 1e-4
aff['codigo_ibge'] = aff.codigo_ibge.astype('Int64')
aff = aff.rename(columns={'treatment': 'auditado_antes'})
aff[['codigo_ibge', 'uf_municipio', 'pareamento_ok', 'dist_pareamento', 'dif_shurb', 'sorteio', 'auditado_antes', 'no_os', 'lno_os', 'falha_total',
     'nirregular', 'mismanagement', 'lpop', 'lrenda_pc', 'shurb', 'gini', 'tx_analf18m']].to_csv(f'{OUT}/aff_corrupcao.csv', index=False)
print('AFF: total', len(aff), '| com código', aff.codigo_ibge.notna().sum(), '| pareamento_ok', aff.pareamento_ok.sum())


# ---------------- TSE: filtrar prefeito, tirar dados pessoais e pôr o código IBGE ----------------
# grava(): cruza o código TSE do município com a chave e descarta diretórios estaduais e nacionais
ch = pd.read_csv(f'{OUT}/chave_municipios.csv')[['codigo_tse', 'codigo_ibge']]
def grava(d, f, col):
    d['codigo_tse'] = pd.to_numeric(d[col], errors='coerce')
    d = d.merge(ch, on='codigo_tse', how='left'); d = d[d.codigo_ibge.notna()].copy(); d['codigo_ibge'] = d.codigo_ibge.astype(int)
    d[['codigo_ibge', 'codigo_tse'] + [c for c in d.columns if c not in ('codigo_ibge', 'codigo_tse')]].to_csv(f'{OUT}/{f}', index=False)
# rd(): lê os arquivos grandes do TSE em pedaços, ficando só com o cargo de prefeito
def rd(path, usecols=None, cargo=None):
    return pd.concat([x if cargo is None else x[x[cargo].str.upper().str.strip() == 'PREFEITO'] for x in
                      pd.read_csv(path, sep=';', encoding='latin1', dtype=str, usecols=usecols, chunksize=200000, on_bad_lines='skip')], ignore_index=True)
# valores com vírgula decimal viram número
def val(s):
    s = s.fillna('').str.strip()
    return pd.to_numeric(s.where(~s.str.contains(','), s.str.replace('.', '', regex=False).str.replace(',', '.', regex=False)), errors='coerce')
# limpa(): mantém só CNPJ (pessoa jurídica); CPF e nome de pessoa física são apagados
def limpa(d, v, doc, nomes):
    d[v] = val(d[v]); d['CNPJ_DOADOR'] = d[doc].fillna('').str.replace(r'\D', '', regex=True)
    d['CNPJ_DOADOR'] = d.CNPJ_DOADOR.where(d.CNPJ_DOADOR.str.len() == 14, '')
    d.loc[d.CNPJ_DOADOR == '', nomes] = ''
    return d.drop(columns=doc)

# votação de prefeito, somada por município (2004 e 2008)
base = ['ANO_ELEICAO', 'CD_TIPO_ELEICAO', 'NR_TURNO', 'SG_UF', 'SG_UE', 'NM_UE', 'DS_CARGO', 'SQ_CANDIDATO', 'NR_CANDIDATO', 'NM_CANDIDATO', 'NM_URNA_CANDIDATO', 'DS_SITUACAO_CANDIDATURA']
for ano in ['2004', '2008']:
    k = base + ['NR_PARTIDO', 'SG_PARTIDO', 'DS_COMPOSICAO_COLIGACAO', 'DS_SIT_TOT_TURNO']
    d = rd(f'{TSE}/votacao_candidato_munzona_{ano}/votacao_candidato_munzona_{ano}_BRASIL.csv', k + ['QT_VOTOS_NOMINAIS'], 'DS_CARGO')
    d['QT_VOTOS_NOMINAIS'] = pd.to_numeric(d.QT_VOTOS_NOMINAIS)
    grava(d.groupby(k, dropna=False, as_index=False).QT_VOTOS_NOMINAIS.sum(), f'tse_votacao_prefeito_{ano}.csv', 'SG_UE')
# candidatos a prefeito (2008 e 2012)
for ano in ['2008', '2012']:
    k = base + ['DS_DETALHE_SITUACAO_CAND', 'NR_PARTIDO', 'SG_PARTIDO', 'DS_COMPOSICAO_COLIGACAO', 'DS_GENERO', 'DS_GRAU_INSTRUCAO', 'DS_OCUPACAO', 'DS_SIT_TOT_TURNO', 'ST_REELEICAO']
    grava(rd(f'{TSE}/consulta_cand_{ano}/consulta_cand_{ano}_BRASIL.csv', k, 'DS_CARGO'), f'tse_candidatos_prefeito_{ano}.csv', 'SG_UE')

# receitas de 2008: candidatos a prefeito e comitês
P = f'{TSE}/prestacao_contas_2008/prestacao_contas_2008'
r = ['NR_PARTIDO', 'SG_PARTIDO', 'SG_UE_SUPERIOR', 'SG_UE', 'NM_UE', 'VR_RECEITA', 'DT_RECEITA']
d = rd(f'{P}/receitas_candidatos_2008_brasil.csv', ['SEQUENCIAL_CANDIDATO', 'DS_CARGO', 'NR_CANDIDATO'] + r + ['DS_TITULO', 'DS_ESP_RECURSO', 'NM_DOADOR', 'CD_CPF_CNPJ_DOADOR'], 'DS_CARGO')
grava(limpa(d, 'VR_RECEITA', 'CD_CPF_CNPJ_DOADOR', 'NM_DOADOR'), 'tse_receitas_cand_prefeito_2008.csv', 'SG_UE')
d = rd(f'{P}/receitas_comites_2008_brasil.csv', ['DS_ORGAO'] + r + ['RTRIM(LTRIM(DR.DS_TITULO))', 'DS_ESP_RECURSO', 'NM_DOADOR', 'CD_CPF_CNPJ_DOADOR'])
grava(limpa(d.rename(columns={'RTRIM(LTRIM(DR.DS_TITULO))': 'DS_TITULO'}), 'VR_RECEITA', 'CD_CPF_CNPJ_DOADOR', 'NM_DOADOR'), 'tse_receitas_comites_2008.csv', 'SG_UE')

# receitas de 2012: candidatos a prefeito, comitês e diretórios municipais
P = f'{TSE}/prestacao_final_2012'
d = rd(f'{P}/receitas_candidatos_2012_brasil.txt', ['Sequencial Candidato', 'UF', 'Numero UE', 'Municipio', 'Sigla  Partido', 'Numero candidato', 'Cargo', 'CPF/CNPJ do doador',
       'Nome do doador', 'Setor econômico do doador', 'Data da receita', 'Valor receita', 'Tipo receita', 'Fonte recurso', 'Especie recurso'], 'Cargo')
grava(limpa(d, 'Valor receita', 'CPF/CNPJ do doador', 'Nome do doador'), 'tse_receitas_cand_prefeito_2012.csv', 'Numero UE')
for tipo in ['comites', 'partidos']:
    d = rd(f'{P}/receitas_{tipo}_2012_brasil.txt')
    doc = [x for x in d.columns if 'CPF/CNPJ' in x]
    d = limpa(d, 'Valor receita', doc[0], ['Nome do doador', 'Nome do doador (Receita Federal)']).drop(columns=doc[1:])
    grava(d, f'tse_receitas_{tipo}_2012.csv', 'Número UE')
