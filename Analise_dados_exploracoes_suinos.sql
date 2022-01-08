-- selecionar schema
USE siss; 



-- QUE EXPLORACOES ESTAO NA TABELA DAS EXPLORACOES MAS NAO ESTAO NA TABELA DAS ALTERACOES DE CLASSIFICACAO SANITARIA?
-- qual a classificacao sanitaria destas 2972 exploracoes?

SELECT * FROM exploracoes WHERE marca NOT IN (SELECT exploracao_id FROM alteracoes_classificacao_sanitaria);



-- EXPLORACOES SEM CONTROLO SEROLOGICO ATUALMENTE 
-- (A1 - estatuto sanitario desconhecido, SC - sem classificacao, Em branco - classificacao nao disponivel)

SELECT *
FROM 
	(SELECT 
		exploracao_id, 
		MAX(data) AS data, 
		substring_index(group_concat(motivos_alteracao_classificacao_sanitaria.nome ORDER BY alteracoes_classificacao_sanitaria.data DESC), ',', 1) AS motivo,
		-- ordenar, para cada exploracao, as novas classificacoes por data decrescente e selecciona a mais recente
        substring_index(group_concat(nova_classificacao ORDER BY data DESC), ',', 1) AS classificacao_mais_recente 
        
	FROM alteracoes_classificacao_sanitaria
    LEFT JOIN motivos_alteracao_classificacao_sanitaria ON motivo_id = motivos_alteracao_classificacao_sanitaria.id
	GROUP BY exploracao_id) tabela_1
    
-- seleccionar A1(1), SC(10) e em branco(12)
WHERE tabela_1.classificacao_mais_recente IN (1, 10, 12);



-- EXPLORACOES QUE PASSARAM DIRETAMENTE DE A2 (positivo), A2A (positivo ativo) OU A2NA (positivo nao ativo) 
-- PARA A4 (indemne)

SELECT * 
FROM
-- selecionar colunas que interessam da tabela alteracoes_classificacao_sanitaria
-- criar nova coluna chamada antiga_classificacao com a classificacao que a exploracao tinha imeadiatamente antes
-- adicionar o motivo da alteracao com base na tabela motivos_alteracao_classificacao_sanitaria
	(SELECT  
        exploracao_id,
		data,
		LAG(nova_classificacao, 1) OVER (
			PARTITION BY exploracao_id
			ORDER BY data) antiga_classificacao,
		nova_classificacao,
        motivos_alteracao_classificacao_sanitaria.nome AS motivo
	FROM 
		alteracoes_classificacao_sanitaria
	JOIN 
		motivos_alteracao_classificacao_sanitaria
    ON 
		motivo_id = motivos_alteracao_classificacao_sanitaria.id) AS classificacoes

-- selecionar exploracoes que tem antiga classificacao A2(2), A2A(3) ou A2NA(4) e que tem como nova classificacao A4(6)
WHERE antiga_classificacao IN (2, 3, 4) AND nova_classificacao = 6;

-- assim obtivemos uma tabela com todas as exploracoes que passaram de A2, A2A ou A2NA diretamente para A4
-- as exploracoes que aparecem mais do que uma vez significa que passaram diretamente para A4 mais do que uma vez
-- a data é a data em que a exploracao passou a ser classificada como A4
-- na ultima coluna podemos ver o motivo da alteracao da classificacao sanitaria



-- CARACTERIZACAO DAS EXPLORACOES POSITIVAS (A2, A2A E A2NA)

SELECT * 
FROM
	(SELECT 
		exploracao_id, 
        -- seleccionar a data mais recente
        MAX(alteracoes_classificacao_sanitaria.data) AS data,
		-- ordenar, para cada exploracao, as novas classificacoes por data decrescente e selecciona a mais recente
		substring_index(group_concat(nova_classificacao ORDER BY alteracoes_classificacao_sanitaria.data DESC), ',', 1) AS classificacao_mais_recente,
        -- ordenar, para cada exploracao, os motivos por data decrescente e selecciona o mais recente
        substring_index(group_concat(motivos_alteracao_classificacao_sanitaria.nome ORDER BY alteracoes_classificacao_sanitaria.data DESC), ',', 1) AS motivo,
		-- coluna com duracao desta classificacao (diferenca entre dia em que obteve a classificacao A2, A2A ou A2NA e dia de hoje) 
		DATEDIFF(NOW(), MAX(alteracoes_classificacao_sanitaria.data)) AS dias_nesta_classificacao,
		svl.nome AS regiao,
        exploracoes.licenciada,
        exploracoes.estado AS estado_exploracao,
		sistemas_exploracao.descricao AS sistema_exploracao,
		tipos_instalacoes.descricao AS tipo_instalacoes,
		tipos_exploracao.descricao AS tipos_exploracao,
		tipos_producao.descricao AS tipo_producao,
        declaracoes_existencias.data AS data_ultima_declaracao_existencia,
        -- ordenar, para cada exploracao, o nome da serie por data decrescente e selecciona a mais recente
		substring_index(group_concat(series.nome ORDER BY vacinacoes.data DESC), ',', 1) AS serie_nome,
		-- ordenar, para cada exploracao, o estado das vacinacoes (concluido ou nao) por data decrescente e selecciona a mais recente
        substring_index(group_concat(vacinacoes.estado ORDER BY vacinacoes.data DESC), ',', 1) AS estado_vacinacao,
		-- ordenar, para cada exploracao, o numero de animais vacinados por data decrescente e selecciona a mais recente
		substring_index(group_concat(vacinacoes.animais_vacinados ORDER BY vacinacoes.data DESC), ',', 1) AS numero_animais_vacinados_na_ultima_vacinacao
	
    FROM alteracoes_classificacao_sanitaria
	
    LEFT JOIN motivos_alteracao_classificacao_sanitaria ON alteracoes_classificacao_sanitaria.motivo_id = motivos_alteracao_classificacao_sanitaria.id
	LEFT JOIN exploracoes ON alteracoes_classificacao_sanitaria.exploracao_id =  exploracoes.marca
	LEFT JOIN svl ON exploracoes.svl = svl.id
    LEFT JOIN sistemas_exploracao ON exploracoes.sistema_exploracao = sistemas_exploracao.codigo
    LEFT JOIN tipos_instalacoes ON exploracoes.tipo_instalacao = tipos_instalacoes.codigo
    LEFT JOIN tipos_exploracao ON exploracoes.tipo_exploracao = tipos_exploracao.codigo
    LEFT JOIN tipos_producao ON exploracoes.tipo_producao = tipos_producao.codigo
	LEFT JOIN declaracoes_existencias ON exploracoes.ultima_declaracao_existencias = declaracoes_existencias.id
	LEFT JOIN vacinacoes ON alteracoes_classificacao_sanitaria.exploracao_id = vacinacoes.exploracoes_marca
	LEFT JOIN series ON vacinacoes.serie = series.id
    
	GROUP BY exploracao_id) caracterizacao_exploracoes

-- selecionar exploracoes que tem antiga classificacao A2(2), A2A(3) ou A2NA(4)
WHERE caracterizacao_exploracoes.classificacao_mais_recente IN (2, 3, 4);


-- aqui podemos ver as exploracoes que atualmete sao A2, A2A ou A2NA e o motivo para se encontrarem nesta classificacao
-- encontram-se nesta classificacao há já pelo menos 91 e há menos de 819 dias (hoje é dia 2/12/2020)
-- estas explroacoes encontram-se em: 3 em aveiro, 1 em caldas da rainha, 2 em santigo do cacém, 2 em serpa, 2 em baixo alentejo, 2 em alentejo central e 1 em castelo branco
-- apenas 1 das exploracoes nao se encontra licenciada (0)?
-- todas as exploracoes estao ativas
-- 6 em 13 sao extensivas, 3 em 13 sao intensivas e 2 em 13 sao intensivas ao ar livre e 2 em 13 não tem informacao
-- todas sao exploracoes exceto uma que e explorcao de detencao caseira
-- 2 sao caseiras de cria, 1 é familiar ciclo completo, 4 sao familiares de cria, 3 sao industriais ciclo completo, 1 é industrial de cria e 2 sem movimentos
-- 5 das 13 sao de producao, 6 das 13 sao de producao de leitoes e 2 das 13 nao tem informacao
-- 11 das 13 tem a ultima declaracao de existencia de agosto 2020, 1 tem de abril 2020 e outra tem de dezembro 2019
-- em todas as exploracoes o nome da serie e vacinacao o que significa que apenas estao a vacinar
-- todas as exploracoes tem o estado de vacinacao concluido
-- o numero de animais vacinados na ultima vacinacao varia entre 3 e 256, no entanto nao sabemos o numero total de animais na exploracao

-- NOTAS:
	-- A exploracao PTWF85B tem duas classificacoes diferentes em simultaneo (2 e 10), nesta ultima analise nao esta a ser contabilizada (assumiu-se = 10)
    -- Nao sabemos o que sao as variaveis: licenciada, capacidade, classe, nucleo_de_producao
    -- Nao percebemos o que significa uma exploracao sem movimento dentro da variavel tipos_exploracao


-- caracteristicas da vacinacao
SELECT 
	exploracao_id,
    doencas.nome AS vacinacao_contra,
    vacinacoes.data AS data_vacinacao,
    vacinacoes.animais_vacinados AS numero_animais_vacinados
FROM
	(SELECT
		exploracao_id,
        MAX(data) AS data,
		substring_index(group_concat(nova_classificacao ORDER BY data DESC), ',', 1) AS classificacao_mais_recente
	FROM alteracoes_classificacao_sanitaria
	GROUP BY exploracao_id) classificacao_atual

LEFT JOIN vacinacoes ON exploracao_id = vacinacoes.exploracoes_marca
LEFT JOIN doencas ON doenca = doencas.id

-- selecionar exploracoes que tem antiga classificacao A2(2), A2A(3) ou A2NA(4)
WHERE classificacao_atual.classificacao_mais_recente IN (2, 3, 4)

-- ordenar pela exploracao_id, quando esta e igual ordenar por vacinacao_contra e quando esta e igual ordenar por data_vacinacao
ORDER BY exploracao_id, vacinacao_contra, data_vacinacao DESC;

