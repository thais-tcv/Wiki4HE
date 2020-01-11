#### Teste DTi

# Candidato: Thais Claudino Viana   CPF: 099.925.086-80
# Vaga: Cientista de Dados


data = read.csv('C:/Users/Tha�s Claudino/Desktop/DTi/wiki4HE.csv', sep = ";", header = T)
str(data) # Temos 53 vari�veis com 913 respostas


# Parte 1: Descritivas B�sicas

summary(data) # Verificando em quais quest�es existem dados faltantes
# � poss�vel perceber que a idade m�dia dos professores, em geral, � de 42 anos e que a maioria deles
# possui dez anos de experi�ncia.

require(descr) # Pacote utilizado para auxiliar nas descri��es dos dados

freq(data$GENDER)
pie(table(data$GENDER), main = "Sexo")
# 57% dos professores univesit�rios entrevistados � do sexo masculino

boxplot(data$AGE~data$GENDER, main = "Idade por Sexo")
summary(data$AGE[data$GENDER == 0])
summary(data$AGE[data$GENDER == 1])
t.test(data$AGE~data$GENDER, paired = F)
# A idade dos professores tem uma m�dia geral de 42 anos, por�m o grupo do sexo masculino �
# ligeiramente mais velho que o do sexo feminino, fato comprovado pelo teste T de diferen�as (p = 0.0001).
# Al�m disso, a pessoa mais idosa do grupo de respondentes � do sexo masculino.

freq(data$DOMAIN)
# 39% dos respondentes dominam a �rea de conhecimento 6 - ou seja, outras �reas diferentes das listadas
# nas op��es. Dois professores n�o responderam e 20% s�o cientistas. 

freq(data$DOMAIN[data$GENDER == 0], plot = T) #Distribui��o da �rea de conhecimento no sexo masculino
freq(data$DOMAIN[data$GENDER == 1], plot = T) #Distribui��o da �rea de conhecimento no sexo feminino
# 22% dos homens s�o da �rea de Engenharia e Arquitetura, enquanto apenas 5% das mulheres s�o dessa �rea 
# do conhecimento. Dentre as mulheres, 27% s�o da �rea de Artes e Humanidades enquanto 14% dos homens
# est�o nessa mesma �rea do conhecimento. A propor��o de professores da �rea de Direito � a mesma
# entre os dois g�neros. Al�m disso, � poss�vel perceber que os dois entrevistados que n�o responderam em 
# que �rea do conhecimento atuam s�o do sexo masculino.


freq(data$PhD, plot = T)
freq(data$GENDER[data$PhD == 1], plot = T)
# 46% dos entrevistados possuem PhD, sendo 60.6% destes do sexo masculino e 39.4% do sexo feminino

freq(data$USERWIKI)
# Veja que 85% dos respondentes da pesquisa n�o sa� usu�rios da Wikip�dia

# Parte 2: Analisar respostas de diferentes grupos de usu�rios para os itens ENJ1 e ENJ2

freq(data$ENJ1)
freq(data$ENJ2)
# A maioria dos entrevistados (66.59%) concordam ou concordam fortemmente que o uso da Wikip�dia estimula 
# a curiosidade e 65.93% deles tamb�m accreditam que seu uso � divertido/agrad�vel.

freq(data$ENJ1[data$GENDER == 0])
freq(data$ENJ1[data$GENDER == 1])

# Gr�ficos Likert

require(likert)

lik = likert(as.data.frame(data[, 17:18]))

plot(lik, type = "heat",wrap = 60, text.size = 3) + theme(axis.text.y = element_text(size = "5"))
# Veja que a resposta m�dia (RM) dos respondentes � de 4.77 para a pergunta ENJ1 e de 4.75 para ENJ2, ou seja,
# h� uma tend�ncia de concord�ncia com as afirma��es propostas.

# Comparando a opini�o considerando o sexo
data$GENDER[data$GENDER == 0] = "Masculino"
data$GENDER[data$GENDER == 1] = "Feminino"

lik2 = likert(as.data.frame(data[, 17:18]), grouping = data$GENDER)
plot(lik2, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))
# Veja que independente do g�nero, a concord�ncia com as duas afirma��es � alta.


# Comparando a opini�o considerando a �rea de atua��o

lik3 = likert(as.data.frame(data[, 17:18]), grouping = data$DOMAIN)
plot(lik3, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))
# O mesmo ocorre quando avaliamos pela �rea de atua��o. Veja que o grupo que mais discorda (ou menos concorda)
# com a afirma��o em EJN1 � o que atua na �rea 3 - Health Sciences e isso se repete na quest�o ENJ2.


## Parte 3: Outras An�lises Interessantes

freq(data$USERWIKI) # � interessante pontuar que, apesar de a maioria concordar com as afirma��es propostas
 # nas quest�es de interesse, 85% dos respondentes N�O UTILIZA a ferramenta, ou pelo menor n�o � um usu�rio registrado
 # na mesma. 

lik4 = likert(as.data.frame(data[, 27:29]))
plot(lik4, type = "heat",wrap = 60, text.size = 3) + theme(axis.text.y = element_text(size = "5"))

# Veja que a resposta m�dia para a quest�o IM1 � de 3.42, ou seja, n�o h� uma concord�ncia expl�cita (nem discord�ncia)
# a respeito do tema, mas apenas 13,4% dos entrevistados concordam que o uso da ferramenta � bem considerado
# ou bem visto entre os colegas enquanto cerca de 52% discordam da afirma��o, ou seja, h� uma tend�ncia de 
# que o uso do Wikip�dia n�o � muito bem aceito entre os entrevistados e colegas.

# Na quest�o IM2 temos uma concod�ncia elevada, ou seja, a maioria dos respondentes acredita que o compartilhamento
# de ferramentas gratuitas de pesquisa � v�lido.

# Sobre o uso da Wikip�dia, os respondentes n�o utilizam e h� um equil�brio no que se refere ao uso da ferramenta
# pelos colegas: cerca de 24% dos respondentes n�o acreditam que seus colegas fa�am uso da mesma e o mesmo percentual
# acredita que seus colegas fazem uso. 37.5% foram neutros e 6.2% n�o responderam.