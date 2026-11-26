# Auditorias da CGU e doações de campanha

Código e dados do trabalho final de Econometria I (FGV EESP, 2026-2), de Antonio Gama e Leonardo Sørensen.
Pergunta: quando a corrupção de uma gestão municipal vem a público antes da eleição, as empresas reduzem as doações ao partido do prefeito? (eleições de 2008 e 2012)

- `scripts/analise.R`: gera as Tabelas 1 a 7 e as Figuras 1 e 2 a partir de `dados/base_final.csv`. Pacotes: `AER`, `car`, `lmtest`.
- `scripts_montagem_base/`: `montagem_base.py` consolida as bases brutas e `montagem_base.R` monta a base final (opcional).
- `dados/`: `base_final.csv` (uma linha por município, 678 municípios) e o calendário dos sorteios da CGU.
- Bases de origem (CGU, Avis, Ferraz e Finan, TSE, Censo e PIB): link no Apêndice A do trabalho.

Para reproduzir os resultados, rode `scripts/analise.R` no R com a pasta do repositório como diretório de trabalho.
