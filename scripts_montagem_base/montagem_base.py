"""
montagem_base.py — consolidação das bases brutas (etapa anterior ao montagem_base.R)

1. Chave TSE <-> IBGE, sorteios da CGU, Atlas, PIB e identificação dos municípios
   nos arquivos anonimizados de Avis, Ferraz e Finan (2018)
2. Bases do TSE: filtra o cargo de prefeito e remove dados pessoais
3. Bases do TSE: acrescenta o código IBGE

Uso: python montagem_base.py <pasta_brutos> <pasta_saida>
"""
import sys, os, re, unicodedata
import numpy as np, pandas as pd
from scipy.optimize import linear_sum_assignment

RAW, OUT = sys.argv[1], sys.argv[2]
M, O = RAW, OUT   # nomes usados na parte 2

# =============================================================
# 1. Chave de municípios, CGU, Atlas, PIB e pareamento do AFF
# =============================================================
def norm(s):
    """Maiúsculas, sem acento, sem apóstrofo/hífen, espaços simples."""
    s = unicodedata.normalize('NFKD', str(s).upper())
    s = ''.join(c for c in s if not unicodedata.combining(c))
    return ' '.join(re.sub(r"[^A-Z ]", ' ', s).split())

# ---------- 1. chave TSE <-> IBGE ----------
chave = pd.read_csv(f'{RAW}/municipios_tse_ibge.csv')
chave['nome_norm'] = chave.nome_municipio.map(norm)
chave[['codigo_ibge', 'codigo_tse', 'uf', 'nome_municipio', 'capital']].to_csv(f'{OUT}/chave_municipios.csv', index=False)

# ---------- 2. sorteios da CGU ----------
cgu = pd.read_csv(f'{RAW}/cgu_sorteios.csv')
cgu['municipio_cgu'] = cgu.municipio
cgu['municipio'] = cgu.municipio.str.replace(r'^Região .*? - ', '', regex=True)   # sorteio 1
# grafias da CGU que diferem da tabela TSE/IBGE (verificadas uma a uma)
correcoes = {
    ('Boninau', 'BA'): 'BONINAL',
    ('Pedra Branca do Amapari', 'AP'): 'AGUA BRANCA DO AMAPARI',
    ('Pedra Branca do Amaparí', 'AP'): 'AGUA BRANCA DO AMAPARI',
    ('Caldas Brandrão', 'PB'): 'CALDAS BRANDAO',
    ('Gracho Cardoso', 'SE'): 'GRACCHO CARDOSO',
    ('Açu', 'RN'): 'ASSU',
    ('Carreiro da Várzea', 'AM'): 'CAREIRO DA VARZEA',
    ('Presidente Juscelino', 'RN'): 'SERRA CAIADA',
    ('São Caetano', 'PE'): 'SAO CAITANO',
    ('Jardim dos Angicos', 'RN'): 'JARDIM DE ANGICOS',
    ('Muquém de São Francisco', 'BA'): 'MUQUEM DO SAO FRANCISCO',
    ('Campo de Santana', 'PB'): 'TACIMA',
    ('Estrela dOeste', 'SP'): 'ESTRELA D OESTE',
    ('Florínia', 'SP'): 'FLORINEA',
    ('Santa Teresinha', 'BA'): 'SANTA TEREZINHA',
    ('São João do Meriti', 'RJ'): 'SAO JOAO DE MERITI',
    ('Piçarras', 'SC'): 'BALNEARIO PICARRAS',
    ('Armação de Búzios', 'RJ'): 'ARMACAO DOS BUZIOS',
    ('Goianópolis', 'GO'): 'GOIANAPOLIS',
    ('Balneário Arroio do Silva', 'SC'): 'BALNEARIO ARROIO DO SILVA',
}
cgu['nome_norm'] = [correcoes.get((m, u), norm(m)) for m, u in zip(cgu.municipio, cgu.uf)]
# RO: CGU escreve "D'Oeste", TSE escreve "DO OESTE"
ro = cgu.uf.eq('RO')
cgu.loc[ro, 'nome_norm'] = cgu.loc[ro, 'nome_norm'].str.replace(r'\bD OESTE$', 'DO OESTE', regex=True)
cgu = cgu.merge(chave[['nome_norm', 'uf', 'codigo_ibge']], on=['nome_norm', 'uf'], how='left')
faltam = cgu[cgu.codigo_ibge.isna()]
print('CGU sem código IBGE:', len(faltam)); print(faltam[['sorteio', 'municipio', 'uf']].to_string())
cgu['data_sorteio'] = pd.to_datetime(cgu.data_sorteio, dayfirst=True).dt.date
cgu['codigo_ibge'] = cgu.codigo_ibge.astype('Int64')
cgu[['codigo_ibge', 'sorteio', 'data_sorteio', 'ordem', 'uf', 'municipio_cgu', 'url']] \
    .to_csv(f'{OUT}/cgu_sorteios.csv', index=False)

# ---------- 3. Atlas (largo: uma coluna por variável-ano) ----------
at = pd.read_csv(f'{RAW}/atlas_ipeadata_2000_2010.csv')
at['col'] = at.serie.str.replace('ADH_', '').str.lower() + '_' + at.ano_censo.astype(str)
atw = at.pivot(index='codigo_ibge', columns='col', values='valor').reset_index()
atw.to_csv(f'{OUT}/atlas_2000_2010.csv', index=False)

# ---------- 4. PIB 2005 ----------
pib = pd.read_excel(f'{RAW}/SIDRA_PIB.xlsx', header=None, skiprows=5).iloc[:-1]
pib.columns = ['codigo_ibge', 'nome_sidra', 'pib_2005_mil_reais']
pib = pib[pib.codigo_ibge.astype(str).str.len() == 7].copy()     # tira a linha "Brasil"
pib['codigo_ibge'] = pib.codigo_ibge.astype(int)
pib['pib_2005_mil_reais'] = pd.to_numeric(pib.pib_2005_mil_reais, errors='coerce')  # "..." -> NA
pib[['codigo_ibge', 'pib_2005_mil_reais']].to_csv(f'{OUT}/pib_2005.csv', index=False)

# ---------- 5. AFF: recuperar o município de cada auditoria ----------
aff = pd.read_stata(f'{RAW}/tableA2.dta', convert_categoricals=False)
UFS = ['AL','BA','CE','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RS','SC','SE','SP']
aff['uf'] = aff.uf_id.astype(int).map(lambda i: UFS[i - 1])        # uf_id = ordem alfabética
# uf_id 10 junta o Norte inteiro (PA + AC, AM, AP, RO, RR, TO): o AFF agrupa os estados pequenos
NORTE = {'PA', 'AC', 'AM', 'AP', 'RO', 'RR', 'TO'}
aff['grupo_uf'] = aff.uf.where(aff.uf != 'PA', 'NORTE')
aff['id_aff'] = np.arange(len(aff))
cgu['grupo_uf'] = cgu.uf.where(~cgu.uf.isin(NORTE), 'NORTE')
cand = cgu.dropna(subset=['codigo_ibge']).merge(
    atw[['codigo_ibge', 'pesotot_2000', 'gini_2000', 'rdpc_2000', 'pesourb_2000']], on='codigo_ibge', how='left')
pares = []
for (s, u), g in aff.groupby(['sorteio', 'grupo_uf']):
    c = cand[(cand.sorteio == s) & (cand.grupo_uf == u)].reset_index(drop=True)
    if c.empty:
        continue
    # custo: |dif. log pop.|/0,05 + |dif. Gini|/0,01 + |dif. part. urbana|/0,01
    shurb_c = c.pesourb_2000.values / c.pesotot_2000.values
    C = (np.abs(np.log(c.pesotot_2000.values)[None, :] - g.lpop.values[:, None]) / 0.05
         + np.abs(c.gini_2000.values[None, :] - g.gini.values[:, None]) / 0.01
         + np.abs(shurb_c[None, :] - g.shurb.values[:, None]) / 0.01)
    C = np.nan_to_num(C, nan=999)
    ri, ci = linear_sum_assignment(C)                                # pareamento 1 a 1
    for a, b in zip(ri, ci):
        pares.append((g.id_aff.iloc[a], int(c.codigo_ibge.iloc[b]), c.uf.iloc[b], C[a, b],
                      c.pesourb_2000.iloc[b] / c.pesotot_2000.iloc[b]))
p = pd.DataFrame(pares, columns=['id_aff', 'codigo_ibge', 'uf_municipio', 'dist_pareamento', 'shurb_atlas'])
aff = aff.merge(p, on='id_aff', how='left')
aff['dif_shurb'] = (aff.shurb - aff.shurb_atlas).abs()
# o par é aceito se a participação urbana do AFF é idêntica à do Atlas 2000 (até 4 casas)
aff['pareamento_ok'] = aff.dif_shurb < 1e-4
aff['codigo_ibge'] = aff.codigo_ibge.astype('Int64')
aff = aff.rename(columns={'treatment': 'auditado_antes'})
cols = ['codigo_ibge', 'uf_municipio', 'pareamento_ok', 'dist_pareamento', 'dif_shurb', 'sorteio', 'auditado_antes',
        'no_os', 'lno_os', 'falha_total', 'nirregular', 'mismanagement',
        'lpop', 'lrenda_pc', 'shurb', 'gini', 'tx_analf18m']
aff[cols].to_csv(f'{OUT}/aff_corrupcao.csv', index=False)
print('AFF: total', len(aff), '| com código', aff.codigo_ibge.notna().sum(), '| pareamento_ok', aff.pareamento_ok.sum())
print(aff.groupby('sorteio').pareamento_ok.mean().round(2).to_string())

# =============================================================
# 2. TSE: filtrar prefeito e remover dados pessoais
# =============================================================
def rd(path, usecols, filt=None, chunks=200000):
    out=[]
    for ch in pd.read_csv(path, sep=';', encoding='latin1', dtype=str, usecols=usecols, chunksize=chunks, on_bad_lines='skip'):
        if filt is not None: ch=ch[filt(ch)]
        out.append(ch)
    return pd.concat(out, ignore_index=True)
def val(s): 
    s=s.fillna('').str.strip()
    s=s.where(~s.str.contains(','), s.str.replace('.','',regex=False).str.replace(',','.',regex=False))
    return pd.to_numeric(s, errors='coerce')
def pj_id(doc, tipo):
    doc=doc.fillna('').str.replace(r'\D','',regex=True)
    return doc.where(doc.str.len()==14, '')   # mantém só CNPJ; CPF de pessoa física é descartado

# 2a. votação de prefeito, 2004 e 2008
for ano in ['2004','2008']:
    f=f'{M}/votacao_candidato_munzona_{ano}/votacao_candidato_munzona_{ano}_BRASIL.csv'
    cols=['ANO_ELEICAO','CD_TIPO_ELEICAO','NR_TURNO','SG_UF','SG_UE','NM_UE','DS_CARGO','SQ_CANDIDATO','NR_CANDIDATO','NM_CANDIDATO','NM_URNA_CANDIDATO','DS_SITUACAO_CANDIDATURA','NR_PARTIDO','SG_PARTIDO','DS_COMPOSICAO_COLIGACAO','DS_SIT_TOT_TURNO','QT_VOTOS_NOMINAIS']
    d=rd(f,cols,lambda c:c.DS_CARGO.str.upper().str.strip()=='PREFEITO')
    d['QT_VOTOS_NOMINAIS']=pd.to_numeric(d.QT_VOTOS_NOMINAIS)
    k=[c for c in cols if c!='QT_VOTOS_NOMINAIS']
    d=d.groupby(k,dropna=False,as_index=False).QT_VOTOS_NOMINAIS.sum()
    d.to_csv(f'{O}/tse_votacao_prefeito_{ano}.csv',index=False); print(ano,d.shape)
# 2b. candidatos a prefeito, 2008 e 2012
cols=['ANO_ELEICAO','CD_TIPO_ELEICAO','NR_TURNO','SG_UF','SG_UE','NM_UE','DS_CARGO','SQ_CANDIDATO','NR_CANDIDATO','NM_CANDIDATO','NM_URNA_CANDIDATO','DS_SITUACAO_CANDIDATURA','DS_DETALHE_SITUACAO_CAND','NR_PARTIDO','SG_PARTIDO','DS_COMPOSICAO_COLIGACAO','DS_GENERO','DS_GRAU_INSTRUCAO','DS_OCUPACAO','DS_SIT_TOT_TURNO','ST_REELEICAO']
for ano in ['2008','2012']:
    d=rd(f'{M}/consulta_cand_{ano}/consulta_cand_{ano}_BRASIL.csv',cols,lambda c:c.DS_CARGO.str.upper().str.strip()=='PREFEITO')
    d.to_csv(f'{O}/tse_candidatos_prefeito_{ano}.csv',index=False); print(ano,d.shape)
# 2c. receitas de 2008 (candidatos e comitês)
P=f'{M}/prestacao_contas_2008/prestacao_contas_2008'
c=['SEQUENCIAL_CANDIDATO','DS_CARGO','NR_CANDIDATO','NR_PARTIDO','SG_PARTIDO','SG_UE_SUPERIOR','SG_UE','NM_UE','VR_RECEITA','DT_RECEITA','DS_TITULO','DS_ESP_RECURSO','NM_DOADOR','CD_CPF_CNPJ_DOADOR']
d=rd(f'{P}/receitas_candidatos_2008_brasil.csv',c,lambda x:x.DS_CARGO.str.upper().str.strip()=='PREFEITO')
d['VR_RECEITA']=val(d.VR_RECEITA); d['CNPJ_DOADOR']=pj_id(d.CD_CPF_CNPJ_DOADOR,d.DS_TITULO)
d.loc[d.CNPJ_DOADOR=='','NM_DOADOR']=''; d=d.drop(columns='CD_CPF_CNPJ_DOADOR')
d.to_csv(f'{O}/tse_receitas_cand_prefeito_2008.csv',index=False); print(d.shape); print(d.DS_TITULO.value_counts().head(10))
c=['DS_ORGAO','NR_PARTIDO','SG_PARTIDO','SG_UE_SUPERIOR','SG_UE','NM_UE','VR_RECEITA','DT_RECEITA','RTRIM(LTRIM(DR.DS_TITULO))','DS_ESP_RECURSO','NM_DOADOR','CD_CPF_CNPJ_DOADOR']
d=rd(f'{P}/receitas_comites_2008_brasil.csv',c).rename(columns={'RTRIM(LTRIM(DR.DS_TITULO))':'DS_TITULO'})
d['VR_RECEITA']=val(d.VR_RECEITA); d['CNPJ_DOADOR']=pj_id(d.CD_CPF_CNPJ_DOADOR,d.DS_TITULO)
d.loc[d.CNPJ_DOADOR=='','NM_DOADOR']=''; d=d.drop(columns='CD_CPF_CNPJ_DOADOR')
d.to_csv(f'{O}/tse_receitas_comites_2008.csv',index=False); print(d.shape); print(d.DS_ORGAO.value_counts())
# 2d. receitas de 2012 (candidatos, comitês e partidos)
P=f'{M}/prestacao_final_2012'
c=['Sequencial Candidato','UF','Numero UE','Municipio','Sigla  Partido','Numero candidato','Cargo','CPF/CNPJ do doador','Nome do doador','Setor econômico do doador','Data da receita','Valor receita','Tipo receita','Fonte recurso','Especie recurso']
d=rd(f'{P}/receitas_candidatos_2012_brasil.txt',c,lambda x:x.Cargo.str.upper().str.strip()=='PREFEITO')
d['Valor receita']=val(d['Valor receita']); d['CNPJ_DOADOR']=pj_id(d['CPF/CNPJ do doador'],d['Tipo receita'])
d.loc[d.CNPJ_DOADOR=='','Nome do doador']=''; d=d.drop(columns='CPF/CNPJ do doador')
d.to_csv(f'{O}/tse_receitas_cand_prefeito_2012.csv',index=False); print(d.shape); print(d['Tipo receita'].value_counts())
for tipo,fn,seq in [('comites','receitas_comites_2012_brasil.txt','Sequencial Comite'),('partidos','receitas_partidos_2012_brasil.txt',None)]:
    d=pd.read_csv(f'{P}/{fn}',sep=';',encoding='latin1',dtype=str,on_bad_lines='skip')
    print(tipo, list(d.columns))
    drop=[x for x in d.columns if 'CPF/CNPJ' in x]
    d['Valor receita']=val(d['Valor receita']); d['CNPJ_DOADOR']=pj_id(d[drop[0]],d['Tipo receita'])
    nm=[x for x in d.columns if x=='Nome do doador']; 
    d.loc[d.CNPJ_DOADOR=='',['Nome do doador','Nome do doador (Receita Federal)']]=''
    d=d.drop(columns=drop)
    d.to_csv(f'{O}/tse_receitas_{tipo}_2012.csv',index=False); print(d.shape)

# =============================================================
# 3. TSE: acrescentar o código IBGE
# =============================================================
chave = pd.read_csv(f'{OUT}/chave_municipios.csv')[['codigo_tse', 'codigo_ibge']]
arquivos = {
    'tse_votacao_prefeito_2004.csv': 'SG_UE', 'tse_votacao_prefeito_2008.csv': 'SG_UE',
    'tse_candidatos_prefeito_2008.csv': 'SG_UE', 'tse_candidatos_prefeito_2012.csv': 'SG_UE',
    'tse_receitas_cand_prefeito_2008.csv': 'SG_UE', 'tse_receitas_comites_2008.csv': 'SG_UE',
    'tse_receitas_cand_prefeito_2012.csv': 'Numero UE', 'tse_receitas_comites_2012.csv': 'Número UE',
    'tse_receitas_partidos_2012.csv': 'Número UE',
}
for f, col in arquivos.items():
    d = pd.read_csv(f'{O}/{f}', dtype=str)
    d['codigo_tse'] = pd.to_numeric(d[col], errors='coerce')
    n0 = len(d)
    d = d.merge(chave, on='codigo_tse', how='left')
    sem = d.codigo_ibge.isna()
    print(f'{f}: {n0} linhas | sem município: {sem.sum()} ({d.loc[sem, col].nunique()} UEs distintas, ex.: {list(d.loc[sem, col].unique()[:5])})')
    d = d[~sem].copy()
    d['codigo_ibge'] = d.codigo_ibge.astype(int)
    d = d[['codigo_ibge', 'codigo_tse'] + [c for c in d.columns if c not in ('codigo_ibge', 'codigo_tse')]]
    d.to_csv(f'{OUT}/{f}', index=False)
