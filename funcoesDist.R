Retencao = function(qion14, qion15) {
  #Funcao para calculo de rentencao entre dois anos
  #Entrada: Base do Qion filtrada para dois anos a serem comparados
  #Saida: Tabela com informacoes de Retencao
  
  #Totais vai guardar a tabela final
  Totais = c()
  
  #Agrega valores de Volume de venda por clientes para ambos os anos
  Ret14 = aggregate(list(qion14$QT_ITEMS), 
                  by=list(qion14$Cliente), FUN = sum)
  Ret15 = aggregate(list(qion15$QT_ITEMS), 
                    by=list(qion15$Cliente), FUN = sum)
  names(Ret14)[1] = "Cliente"
  names(Ret14)[2] = "Volume"
  names(Ret15)[1] = "Cliente"
  names(Ret15)[2] = "Volume"
  
  #Pessoal gosta de considerar rentencao apenas para acima de 5 sacos
  Ret14 = subset(Ret14, Volume>5)
  Ret15 = subset(Ret15, Volume>5)
  
  Totais[1] = length(unique(Ret14$Cliente)) #Total de clientes do ano anterior
  Totais[2] = length(unique(Ret15$Cliente)) #Total de clientes do ano seguinte
  Totais[3] = sum(unique(Ret15$Cliente) %in% unique(Ret14$Cliente)) #Clientes em ambos: Os retidos
  Totais[4] = Totais[2] - Totais[3] #Os clientes novos
  Retencao = as.data.frame(cbind(Row.Names=c("Total 13/14","Total 14/15","Retido","Novos Clientes"),
                                 Totais))
  return(Retencao)
}

Refugio = function(qion14){
  #Calcula o refugio para um dado ano e distribuidor
  #Entrada: Tabela do Qion, geralmente filtrado para um ano especifico
  #Saida: Tabela com todos os agricultores, seu volume com BT, sem BT, volume total e status de compliance de refugio

  qion14 = subset(qion14, CULTURA == "MILHO") #Calcula refugio apenas em milho
  
  #Agrega volume por ciente e tecnologia
  refugio.novo14 = cbind.data.frame(qion14$Cliente,qion14$QT_ITEMS, qion14$TECNOLOGIA)
  names(refugio.novo14)[1] = "Cliente"
  names(refugio.novo14)[2] = "QT_ITEMS"
  names(refugio.novo14)[3] = "BT"
  
  #Transposicao da tabela
  refugio14 = cast(refugio.novo14, Cliente ~ BT, sum, 
                   value = "QT_ITEMS")
  refugio14$Total = refugio14$BT + refugio14$"NAO BT"
  #Nao calculamos refugio para quem nao fez compra de BT
  refugio14 = subset(refugio14, BT>0) 
  refugio14$Compliance = "Nao"
  
  #Aplica regras de refugio do programa de marketing
  for (i in 1:length(refugio14$BT)){
    if(refugio14$Total[i]<= 13){
      if (refugio14$"NAO BT"[i]>=1){
        refugio14$Compliance[i] = "Sim"}}
    
    if(refugio14$Total[i]<= 26 & refugio14$Total[i]>= 14){
      if (refugio14$"NAO BT"[i]>=2){
        refugio14$Compliance[i] = "Sim"}}
    
    if(refugio14$Total[i]<= 39 & refugio14$Total[i]>= 27){
      if (refugio14$"NAO BT"[i]>=3){
        refugio14$Compliance[i] = "Sim"}}
    
    if(refugio14$Total[i]<= 89 & refugio14$Total[i]>= 40){
      if (refugio14$"NAO BT"[i]>=ceiling(0.08*refugio14$Total[i])){
        refugio14$Compliance[i] = "Sim"}}
    
    if(refugio14$Total[i]<= 200 & refugio14$Total[i]>= 90){
      if (refugio14$"NAO BT"[i]>=ceiling(0.09*refugio14$Total[i])){
        refugio14$Compliance[i] = "Sim"}}
    
    if(refugio14$Total[i]>= 200){
      if (refugio14$"NAO BT"[i]>=ceiling(0.10*refugio14$Total[i])){
        refugio14$Compliance[i] = "Sim"}}
  }
  
  #Ordena por volume
  refugio14 = refugio14[order(-refugio14$Total),]
  
  refugio14$BT = round(refugio14$BT)
  refugio14$"NAO BT" = round(refugio14$"NAO BT")
  refugio14$BT = round(refugio14$Total)
  
  return(refugio14)
  
}


Principais = function(qion14){
  
  #Calcula os principais produtos e o volume % acumulado que cada um representa
  #Entrada: Tabela do Qion, geralmente filtrado para um ano especifico
  #Saida: Tabela com todos os produtos, ordenada por volume e com % Acumulado
  
  #É bem direto. Agregado por volume (coloco o montante de vendas caso precise no futuro)
  #Retira casos abaixo de 0 (devolucoes), ordena e faz % acumulado
  Pivot14 = aggregate(list(qion14$MT_VENDAS, qion14$QT_ITEMS), 
                      by=list(qion14$PRODUTO), FUN = sum)
  names(Pivot14)[2] = "MontanteCanal"
  names(Pivot14)[3] = "VolumeCanal"
  Pivot14 = subset(Pivot14, VolumeCanal>0)
  Pivot14$MediaCanal = Pivot14$MontanteCanal/Pivot14$VolumeCanal
  
  Pivot14 = Pivot14[order(-Pivot14$VolumeCanal),]
  Pivot14$Acumulado = cumsum(Pivot14$VolumeCanal)
  Pivot14$PorcentagemAcumulada = 100*Pivot14$Acumulado/sum(Pivot14$VolumeCanal)
  
  return(Pivot14)
  
}

Clientes = function(qion14, milho){
  #Calcula os principais produtos e o volume % acumulado que cada um representa
  #Entrada: Tabela do Qion, geralmente filtrado para um ano especifico
  #Saida: Tabela com todos os produtos, ordenada por volume e com % Acumulado
  
 #Querem só clientes de milho
  if(milho == TRUE){
    qion14 = subset(qion14, CULTURA=="MILHO")
  }
  
  #É bem direto. Agregado por cliente 
  #Retira casos abaixo de 0 (devolucoes), ordena e faz % acumulado
  
  Clientes14 = aggregate(list(qion14$QT_ITEMS),
                         by=list(qion14$CPF, qion14$AGRICULTOR), FUN = sum)
  names(Clientes14)[1] = "CPF"
  names(Clientes14)[2] = "Agricultor"
  names(Clientes14)[3] = "Volume"
  
  Clientes14 = subset(Clientes14, Volume>0)
  
  Clientes14 = Clientes14[order(-Clientes14$Volume),]
  Clientes14$Acumulado = cumsum(Clientes14$Volume)
  Clientes14$PorcentagemAcumulada = 100*Clientes14$Acumulado/sum(Clientes14$Volume)
  
  
  return(Clientes14)
}

Mktshare = function(codex,qion14){
  #Calculados o Mkt share de milho, ou seja: Sacos Vendidos/Area de milho das cidades que o
  #distribuidor atuou. Considerou-se que cada saco é um ha
  #Entrada: Tabela do Qion, geralmente filtrado para um ano especifico. Tabela de codex
  #Saida: Tabela todas as cidades, o MKT na cidade. Somas e MKT share total.
  
  qion14 = subset(qion14, CULTURA=="MILHO")
  #Ajusto aqui o nome da cidade para a tabela a ser impressa no final
  qion14$Cidade = paste(toupper(qion14$CIDADE_CLIENTE)," - ",qion14$ESTADO_CLIENTE, sep="")
  AggCidade14 = aggregate(list(qion14$MT_VENDAS, qion14$QT_ITEMS), 
                          by=list(qion14$Cidade), FUN = sum)
  names(AggCidade14)[1] = "Municipios"
  names(AggCidade14)[2] = "MT_VENDAS"
  names(AggCidade14)[3] = "QT_ITEMS"
  
  #Usamos apenas cidades acima de 5 sacos de volume
  AggCidade14 = subset(AggCidade14, QT_ITEMS>5)
  
  
  resultado = left_join(AggCidade14,codex)
  
  #A tabela de codex tem muitas colunas que não uso
  resultado$Cod_IBGE = NULL
  resultado$Cod_IBGE.1 = NULL
  resultado$Verao.12 = NULL
  resultado$Verao.13 = NULL
  resultado$Safrinha.13 = NULL
  resultado$Safrinha.14 = NULL
  
  resultado$CY14[is.na(resultado$CY14)] = 0
  
  #Tira o MKT na cidade. Se a cidade "nao tem" area de milho. Considero 100%
  resultado$share = 100*resultado$QT_ITEMS/resultado$CY14
  resultado$share[(is.infinite(resultado$share))] = 100
  
  resultado = subset(resultado, QT_ITEMS != 0)
  resultado = resultado[order(-resultado$QT_ITEMS),]
  
  #Feito para ver principais cidade, mas nao necessario por enquanto
  resultado$QT_ACUMULADO = cumsum(resultado$QT_ITEMS)
  resultado$PctAcumulado = 100*resultado$QT_ACUMULADO/sum(resultado$QT_ITEMS)
  
  #Coloca a linha de somas e mkt share total
  soma = c("SOMA", sum(resultado$MT_VENDAS),sum(resultado$QT_ITEMS), 
           sum(resultado$CY14), 100*sum(resultado$QT_ITEMS)/sum(resultado$CY14),
           sum(resultado$MT_ITEMS),100)
  
  resultado = rbind.data.frame(resultado,soma)
  
  return(resultado)
  
}

Regionais = function(qion,nec){
  #Calcula as principais que o distribuidor atuou
  #Entrada: Tabela do Qion, geralmente filtrado para um ano especifico. Tabela de nec
  #Saida: Tabela todas as Regionais que o distribuidor atuou e a % Acumulada
  
  Aggmarca = aggregate(list(qion$QT_ITEMS), 
                   by=list(qion$MARCA_PRODUTO), FUN = sum)
  names(Aggmarca)[1] = "Marca"
  names(Aggmarca)[2] = "Volume"
  
  #Tabela de Nec e do Qi.on não usam a mesma denominação de marca, o que é importante para
  #a estrutura comercial
  marca = Aggmarca$Marca[which(Aggmarca$Volume==max(Aggmarca$Volume))]
  
  if (marca == "AGROCERES"){
    
    nec = subset(nec,MARCA == "AG")
  }
  if (marca == "AGROESTE"){
    
    nec = subset(nec,MARCA == "AS")
  }
  if (marca == "DEKALB"){
    
    nec = subset(nec,MARCA == "KB")
  }
  
  AggCidade14 = aggregate(list(qion$QT_ITEMS), 
                          by=list(qion$IBGECODE_CIDADE_CLIENTE), FUN = sum)
  names(AggCidade14)[1]="CODIGO_IBGE"
  names(AggCidade14)[2]="Volume"
  resultado = left_join(AggCidade14,nec,by=)
  
  resultado = aggregate(list(resultado$Volume), 
                        by=list(resultado$REGIONAL.SAP.PARA,resultado$UNIDADE.SAP.PARA), FUN = sum)
  names(resultado)[1] = "Regional"
  names(resultado)[2] = "Unidade"
  names(resultado)[3] = "Volume"
  
  resultado = resultado[order(-resultado$Volume),]
  
  resultado$Acumulado = cumsum(resultado$Volume)
  resultado$PorcentagemAcumulada = 100*resultado$Acumulado/sum(resultado$Volume)
  
  
  return (resultado)
    
  }
  

  
  