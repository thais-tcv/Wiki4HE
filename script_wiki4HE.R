#### Teste DTi

# Candidato: Thais Claudino Viana   CPF: 099.925.086-80
# Vaga: Cientista de Dados


data = read.csv('C:/Users/Thaís Claudino/Desktop/DTi/wiki4HE.csv', sep = ";", header = T)
str(data) # Temos 53 variáveis com 913 respostas


# Parte 1: Descritivas Básicas

summary(data) # Verificando em quais questões existem dados faltantes
# É possível perceber que a idade média dos professores, em geral, é de 42 anos e que a maioria deles
# possui dez anos de experiência.

require(descr) # Pacote utilizado para auxiliar nas descrições dos dados

freq(data$GENDER)
pie(table(data$GENDER), main = "Sexo")
# 57% dos professores univesitários entrevistados é do sexo masculino

boxplot(data$AGE~data$GENDER, main = "Idade por Sexo")
summary(data$AGE[data$GENDER == 0])
summary(data$AGE[data$GENDER == 1])
t.test(data$AGE~data$GENDER, paired = F)
# A idade dos professores tem uma média geral de 42 anos, porém o grupo do sexo masculino é
# ligeiramente mais velho que o do sexo feminino, fato comprovado pelo teste T de diferenças (p = 0.0001).
# Além disso, a pessoa mais idosa do grupo de respondentes é do sexo masculino.

freq(data$DOMAIN)
# 39% dos respondentes dominam a área de conhecimento 6 - ou seja, outras áreas diferentes das listadas
# nas opções. Dois professores não responderam e 20% são cientistas. 

freq(data$DOMAIN[data$GENDER == 0], plot = T) #Distribuição da área de conhecimento no sexo masculino
freq(data$DOMAIN[data$GENDER == 1], plot = T) #Distribuição da área de conhecimento no sexo feminino
# 22% dos homens são da área de Engenharia e Arquitetura, enquanto apenas 5% das mulheres são dessa área 
# do conhecimento. Dentre as mulheres, 27% são da área de Artes e Humanidades enquanto 14% dos homens
# estão nessa mesma área do conhecimento. A proporção de professores da área de Direito é a mesma
# entre os dois gêneros. Além disso, é possível perceber que os dois entrevistados que não responderam em 
# que área do conhecimento atuam são do sexo masculino.


freq(data$PhD, plot = T)
freq(data$GENDER[data$PhD == 1], plot = T)
# 46% dos entrevistados possuem PhD, sendo 60.6% destes do sexo masculino e 39.4% do sexo feminino

freq(data$USERWIKI)
# Veja que 85% dos respondentes da pesquisa não saõ usuários da Wikipédia

# Parte 2: Analisar respostas de diferentes grupos de usuários para os itens ENJ1 e ENJ2

freq(data$ENJ1)
freq(data$ENJ2)
# A maioria dos entrevistados (66.59%) concordam ou concordam fortemmente que o uso da Wikipédia estimula 
# a curiosidade e 65.93% deles também accreditam que seu uso é divertido/agradável.

freq(data$ENJ1[data$GENDER == 0])
freq(data$ENJ1[data$GENDER == 1])

# Gráficos Likert

require(likert)

lik = likert(as.data.frame(data[, 17:18]))

plot(lik, type = "heat",wrap = 60, text.size = 3) + theme(axis.text.y = element_text(size = "5"))
# Veja que a resposta média (RM) dos respondentes é de 4.77 para a pergunta ENJ1 e de 4.75 para ENJ2, ou seja,
# há uma tendência de concordância com as afirmações propostas.

# Comparando a opinião considerando o sexo
data$GENDER[data$GENDER == 0] = "Masculino"
data$GENDER[data$GENDER == 1] = "Feminino"

lik2 = likert(as.data.frame(data[, 17:18]), grouping = data$GENDER)
plot(lik2, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))
# Veja que independente do gênero, a concordância com as duas afirmações é alta.


# Comparando a opinião considerando a área de atuação

lik3 = likert(as.data.frame(data[, 17:18]), grouping = data$DOMAIN)
plot(lik3, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))
# O mesmo ocorre quando avaliamos pela área de atuação. Veja que o grupo que mais discorda (ou menos concorda)
# com a afirmação em EJN1 é o que atua na área 3 - Health Sciences e isso se repete na questão ENJ2.


## Parte 3: Outras Análises Interessantes

freq(data$USERWIKI) # É interessante pontuar que, apesar de a maioria concordar com as afirmações propostas
 # nas questões de interesse, 85% dos respondentes NÃO UTILIZA a ferramenta, ou pelo menor não é um usuário registrado
 # na mesma. 

lik4 = likert(as.data.frame(data[, 27:29]))
plot(lik4, type = "heat",wrap = 60, text.size = 3) + theme(axis.text.y = element_text(size = "5"))

# Veja que a resposta média para a questão IM1 é de 3.42, ou seja, não há uma concordância explícita (nem discordância)
# a respeito do tema, mas apenas 13,4% dos entrevistados concordam que o uso da ferramenta é bem considerado
# ou bem visto entre os colegas enquanto cerca de 52% discordam da afirmação, ou seja, há uma tendência de 
# que o uso do Wikipédia não é muito bem aceito entre os entrevistados e colegas.

# Na questão IM2 temos uma concodância elevada, ou seja, a maioria dos respondentes acredita que o compartilhamento
# de ferramentas gratuitas de pesquisa é válido.

# Sobre o uso da Wikipédia, os respondentes não utilizam e há um equilíbrio no que se refere ao uso da ferramenta
# pelos colegas: cerca de 24% dos respondentes não acreditam que seus colegas façam uso da mesma e o mesmo percentual
# acredita que seus colegas fazem uso. 37.5% foram neutros e 6.2% não responderam.