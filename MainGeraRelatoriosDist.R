#Este script tem como objetivo, gerar relatorios em pdf com os principais KPIs do programa de Marketing
#para todos os distribuidores participantes
#e gerado um pdf para cada distribuidor
#As fontes de dados sao:
#Qion: Contem informacoes de notas fiscais como preco, produto, quantidade de itens, data e distribuidor
#Codex: Base com informacoes de area de milho plantada no verao e na safrinha de diversos anos
#Produtos: Identifica qual a cultura de cada produto (milho ou sorgo) e se ele contem a proteina BT
#Nec: Base com informacao da estrutura comercial (Unidade, Regional, Distrito) de todas as cidades do Brasil.

library(XLConnect)
library(ggplot2)
library(rmarkdown)
library(dplyr)
library(reshape)
library(xtable)

#Caso apareca um erro ao passar pandoc para tex (especialmente nas linhas de imprimir tabelas),
#Feche e abra o Rstudio.
#Nao se sabe o que causa esse erro, exceto que "algo ruim entrou na cache e causou erro"
#Segundo o que eu achei no Stack overflow


#Limpa variaveis e coloca opcoes
rm(list=ls(all=TRUE))
options(scipen=3)

#Pega as bases citadas acima
qion = read.csv("qion.csv",sep = ";")
wb = XLConnect::loadWorkbook("CODEX.xlsx")
codex = XLConnect::readWorksheet(wb, sheet = "CODEX", header = TRUE)
produtos = read.csv("BASE_PRODUTO.csv",sep = ";")
wb = XLConnect::loadWorkbook("BASE NEC 2014.xlsx")
nec = XLConnect::readWorksheet(wb, sheet = "Sheet1", header = TRUE)

#Adiciona Cultura e dados BT para os produtos de notas fiscais do Qi.on
qion = left_join(qion,produtos)
test = subset(qion, is.na(TECNOLOGIA))
if (length(test$PRODUTO) !=0){print ("CHECAR PRODUTOS QUE NAO ESTAO NA TABELA")}

#S?o usados apenas alguns tipos de notas fiscais para analises. Desse modo tamb?m retira-se notas
#enviadas erradas pelos distribuidores ("NAO SE APLICA")
invoices = c("VENDA","DEVOLUCAO","VENDA COM ENTREGA FUTURA","BONIFICACAO")
qion = subset(qion, INVOICETYPE %in% invoices)

#Aqui basicamente inicia-se o loop
#A logica e a seguinte: Para cada matriz de distribuidor (atraves do codigo SAP), 
#A base e filtrada, aplica-se as funcoes e o PDF e gerado a partir dos resultados das fun??es. 
#Logo depois, retorna-se a base ao seu estado completo e escolhe-se uma nova matriz

#Guarda uma versao da base completa e um vetor com todas matrizes
qion_inicial = qion
unicos_qion = unique(qion_inicial$CODE_SAP_MATRIZ)



#length(unicos_qion)
for (i in 1:1){
  
  #Chama as funcoes que v?o ser usadas
  source("funcoesDist.R")
  
  #Retorna a base ao seu estado completo e escolhe a proxima matriz a ser usada
  qion = qion_inicial
  matriz = unique(qion$CODE_SAP_MATRIZ)[i]
  #deixo essa linha para testes
  matriz = 1832739 
  
  #Filtra a matriz/distribuidor sendo usado
  qion = subset(qion, CODE_SAP_MATRIZ == matriz)
  #Consideramos um cliente uma jun??o de CPF+Nome, devido a casos em que o CPF nao foi adicionado
  qion$Cliente = paste(qion$CPF,qion$AGRICULTOR)

  #Separa os tr?s anos presentes
  qion13 = subset(qion, CROP_YEAR=="13/14")
  qion14 = subset(qion, CROP_YEAR=="14/15")
  qion15 = subset(qion, CROP_YEAR=="15/16")
  
  #Prepara Tabela com dados de Retencao
  Retencao = Retencao(qion14,qion15)
  
  # Prepara Tabela com dados de Refugio dos dois ultimos CY
  refugio14 = Refugio(qion14)
  refugio15 = Refugio(qion15)
  
  #Calcula quais os principais produtos que o distribuidor comercializa 
  Pivot15 = Principais(qion15)
  
  #Clientes mais importantes considerando todos os produtos e depois apenas considerando milho
  Clientes15 = Clientes(qion15,FALSE)
  
  Clientes_milho15 = Clientes(qion15,TRUE)

  
  #Calculo de Mkt Share
  #Inicio com um pequeno ajuste da base Codex e depois somo as areas de safrinha+verao para um CY
  codex$Verao.13[codex$Verao.13=="N/A"] = 0
  codex$Verao.13[is.na(codex$Verao.13)] = 0
  codex$Safrinha.14[codex$Safrinha.14=="N/A"] = 0
  codex$Safrinha.14[is.na(codex$Safrinha.14)] = 0
  codex$CY14 = as.numeric(codex$Verao.13) + as.numeric(codex$Safrinha.14)
  
  #Calcula os ultimos 3 anos de Mkt share
  mktshare13 = Mktshare(codex, qion13)
  mktshare14 = Mktshare(codex, qion14)
  mktshare15 = Mktshare(codex, qion15)
  
  #Calcula as regionais que o distribuidor atua
  Regionais = Regionais(qion15, nec)
  
  
  #Faz a impress?o das tabelas finais para que se possa usar fora
  write.table(Retencao, "Rentencao.csv",sep = ";",row.names = FALSE)
  write.table(refugio15, "refugio15.csv",sep = ";",row.names = FALSE)
  write.table(Pivot15, "Pivot15.csv",sep = ";",row.names = FALSE)
  write.table(Clientes15, "Clientes15.csv",sep = ";",row.names = FALSE)
  write.table(mktshare15, "mktshare15.csv",sep = ";",row.names = FALSE)
  
  #Importante sen?o ao imprimir o nome pode causar erro.
  nome = make.names(qion$DEALER[1])
  
  #Caso apareca um erro ao passar pandoc para tex (especialmente nas linhas de imprimir tabelas),
  #Feche e abra o Rstudio.
  #Nao se sabe o que causa esse erro, exceto que "algo ruim entrou na cache e causou erro"
  #Segundo o que eu achei no Stack overflow
   render(input="GraphGeraRelatoriosDist.Rmd",
           output_file=paste0(nome,".pdf"
           ))
  
}

