# A2 para A4
classificacoes_a2 <- classificacoes %>%
  filter((classificacao_sanitaria == 'A4' & motivo == 'Classificação repovoamento') | classificacao_sanitaria == 'A2') %>%
  group_by(exploracao_id)

a2 <- classificacoes_a2 %>% 
  group_by(exploracao_id, data) %>% 
  filter(n()>1) %>%
  arrange(desc(data))

a21 <- a2 %>%
  group_by(exploracao_id, data) %>%
  filter(!(duplicated(classificacao_sanitaria, fromLast = TRUE)| duplicated(classificacao_sanitaria))) 



# A2A para A4
classificacoes_a2a <- classificacoes %>%
  filter((classificacao_sanitaria == 'A4' & motivo == 'Classificação repovoamento') | classificacao_sanitaria == 'A2A') %>%
  group_by(exploracao_id)

a2a <- classificacoes_a2a %>% 
  group_by(data,exploracao_id) %>% 
  filter(n()>1) %>%
  arrange(desc(data))

a2a1 <- a2a %>%
  group_by(data, classificacao_sanitaria) %>%
  filter(!(duplicated(classificacao_sanitaria, fromLast = TRUE)| duplicated(classificacao_sanitaria))) 

# A2NA para A4
classificacoes_a2na <- classificacoes %>%
  filter((classificacao_sanitaria == 'A4' & motivo == 'Classificação repovoamento') | classificacao_sanitaria == 'A2NA') %>%
  group_by(exploracao_id)

a2na <- classificacoes_a2na %>% 
  group_by(data,exploracao_id) %>% 
  filter(n()>1) %>%
  arrange(desc(data))

a2na1 <- a2na %>%
  group_by(data, classificacao_sanitaria) %>%
  filter(!(duplicated(classificacao_sanitaria, fromLast = TRUE)| duplicated(classificacao_sanitaria))) 


# A2S para A4
classificacoes_a2s <- classificacoes %>%
  filter((classificacao_sanitaria == 'A4' & motivo == 'Classificação repovoamento') | classificacao_sanitaria == 'A2S') %>%
  group_by(exploracao_id)

a2s <- classificacoes_a2s %>% 
  group_by(data,exploracao_id) %>% 
  filter(n()>1) %>%
  arrange(desc(data))

a2s1 <- a2s %>%
  group_by(data, classificacao_sanitaria) %>%
  filter(!(duplicated(classificacao_sanitaria, fromLast = TRUE)| duplicated(classificacao_sanitaria))) 



