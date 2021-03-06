#Rotina para coletar e apresentar em gr�ficos algumas s�ries do banco central
#Feito por: Felipe Simpl�cio Ferreira
#�ltima atualiza��o: 10/10/2020


#Definindo diret�rios a serem utilizados
getwd()
setwd("C:/Users/User/Documents")

#PACOTES REQUERIDOS:
#INSTALAR QUANDO NECESS�RIO
#EXEMPLO:install.packages("pryr")
library(rio)

#Criando fun��o para coleta de s�ries
coleta_dados_sgs = function(series,datainicial="01/01/2013", datafinal = format(Sys.time(), "%d/%m/%Y")){
  #Argumentos: vetor de s�ries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Cria estrutura de repeti��o para percorrer vetor com c�digos de s�ries e depois juntar todas em um �nico dataframe
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    dados[,-1] = as.numeric(gsub(",",".",dados[,-1])) #As colunas do dataframe em objetos num�ricos exceto a da data
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o c�digo da s�rie
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os v�rios arquivos intermedi�rios que s�o criados com cada s�rie
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1 #Primeira repeti��o cria o dataframe
    else
      base = merge(base, dados, by = "data", all = T) #Demais repeti��es agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repeti��o
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y") #Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
  return(base)
}

#Saldos
series1 = c("28183", "28184", "28185", "28186", "28187", "28188", "28189", "28190", "28191", "28192", "28193","28194", "28195")

base1 <- coleta_dados_sgs(series1)

#Nomeando colunas do dataframe com todas as s�ries
names(base1) = c("Data", "Saldo de cr�dito ampliado - Total", "Saldo de empr�stimos total ao setor n�o financeiro", "Saldo de empr�stimos do SFN ao setor n�o financeiro", "Saldo de empr�stimos de OSF ao setor n�o financeiro",
                "Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro", "Saldo de t�tulos de d�vida - Total", "Saldo de t�tulos p�blicos", "Saldo de t�tulos privados", "Saldo de instrumentos de securitiza��o",
                "Saldo de d�vida externa - Total", "Saldo de d�vida externa - Empr�stimos", "Saldo de d�vida externa - T�tulos emitidos no mercado externo", "Saldo de d�vida externa - T�tulos emitidos no mercado dom�stico")

#Exportando resultados
write.csv2(base1,"1-Saldos.csv", row.names = F)
export(base1, "Saldo Ampliado.xlsx", sheetName = "1-Saldos")


#Saldos - Governo geral
series2 = c("28196", "28197", "28198", "28199", "28200", "28201", "28202")

base2 <- coleta_dados_sgs(series2)

#Nomeando colunas do dataframe com todas as s�ries
names(base2) = c("Data", "Saldo de cr�dito ampliado ao governo - Total", "Saldo de empr�stimos do SFN ao governo", "Saldo de t�tulos p�blicos - Governo geral", "Saldo de d�vida externa - Concedido ao governo - Total", "Saldo de d�vida externa - Empr�stimos ao governo",
                "Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo", "Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico")

#Exportando resultados
write.csv2(base2,"2-Saldos governo geral.csv", row.names = F)
export(base2, "Saldo Ampliado.xlsx", which = "2-Saldos governo geral")

#Saldos - Empresas e fam�lias
series3 = c("28203", "28204", "28205", "28206", "28207", "28208", "28209", "28210", "28211", "28212", "28213", "28214")

base3 <- coleta_dados_sgs(series3)

#Nomeando colunas do dataframe com todas as s�ries
names(base3) = c("Data", "Saldo de cr�dito ampliado a empresas e fam�lias - Total", "Saldo de empr�stimos a empresas e fam�lias - Total", "Saldo de empr�stimos do SFN a empresas e fam�lias",
                "Saldo de empr�stimos de OSF a empresas e fam�lias", "Saldo de empr�stimos de fundos governamentais a empresas e fam�lias", "Saldo de t�tulos de d�vida emitidos por empresas e fam�lias - Total",
                "Saldo de t�tulos privados emitidos por empresas e fam�lias", "Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias", "Saldo de d�vida externa - Concedido a empresas e fam�lias - Total",
                "Saldo de d�vida externa - Empr�stimos a empresas e fam�lias", "Saldo de d�vida externa - T�tulos privados emitidos no mercado externo", "Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico")

#Exportando resultados
write.csv2(base3,"3-Saldos empresas e fam�lias.csv", row.names = F)
export(base3, "Saldo Ampliado.xlsx", which = "3-Saldos empresas e fam�lias")


#PIB acumulado dos �ltimos 12 meses - Valores correntes (R$ milh�es)
pib <- coleta_dados_sgs("4382")

#Montagem da tabela do BCB
##Dados do �ltimo m�s
`Empr�stimos e financiamentos`=c(
  base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`)]/1000,
  base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`)]/1000,
  base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`)]/1000
  )
`Empr�stimos e financiamentos`[4] = sum(`Empr�stimos e financiamentos`)

`T�tulos de d�vida` = c(
  base2$`Saldo de t�tulos p�blicos - Governo geral`[length(base2$`Saldo de t�tulos p�blicos - Governo geral`)]/1000,
  base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`[length(base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`)]/1000,
  base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`[length(base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`)]/1000
)
`T�tulos de d�vida`[4] = sum(`T�tulos de d�vida`)

`D�vida externa p�blica` = c(
  base2$`Saldo de d�vida externa - Empr�stimos ao governo`[length(base2$`Saldo de d�vida externa - Empr�stimos ao governo`)],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`)],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`)]
)
`D�vida externa p�blica`[4] = sum(`D�vida externa p�blica`)

`D�vida externa privada` = c(
  base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`[length(base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`)],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`)],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`)]
)
`D�vida externa privada`[4] = sum(`D�vida externa privada`)

`D�vida externa` = (`D�vida externa p�blica` + `D�vida externa privada`)/1000

`Box1`=cbind(`Empr�stimos e financiamentos`, `T�tulos de d�vida`, `D�vida externa`)
`Box2`=c(`Box1`[1,1], `Box1`[2,1], `Box1`[3,1], `Box1`[4,1], `Box1`[1,2], `Box1`[2,2], `Box1`[3,2], `Box1`[4,2],
         `Box1`[1,3], `Box1`[2,3], `Box1`[3,3], `Box1`[4,3]
)

##Dados de 12 meses atr�s
`Empr�stimos e financiamentos`=c(
  base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`)-12]/1000,
  base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`)-12]/1000,
  base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`)-12]/1000
)
`Empr�stimos e financiamentos`[4] = sum(`Empr�stimos e financiamentos`)

`T�tulos de d�vida` = c(
  base2$`Saldo de t�tulos p�blicos - Governo geral`[length(base2$`Saldo de t�tulos p�blicos - Governo geral`)-12]/1000,
  base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`[length(base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`)-12]/1000,
  base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`[length(base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`)-12]/1000
)
`T�tulos de d�vida`[4] = sum(`T�tulos de d�vida`)

`D�vida externa p�blica` = c(
  base2$`Saldo de d�vida externa - Empr�stimos ao governo`[length(base2$`Saldo de d�vida externa - Empr�stimos ao governo`)-12],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`)-12],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`)-12]
)
`D�vida externa p�blica`[4] = sum(`D�vida externa p�blica`)

`D�vida externa privada` = c(
  base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`[length(base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`)-12],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`)-12],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`)-12]
)
`D�vida externa privada`[4] = sum(`D�vida externa privada`)
`D�vida externa` = (`D�vida externa p�blica` + `D�vida externa privada`)/1000

`Box3`=cbind(`Empr�stimos e financiamentos`, `T�tulos de d�vida`, `D�vida externa`)
`Box4`=c(`Box3`[1,1], `Box3`[2,1], `Box3`[3,1], `Box3`[4,1], `Box3`[1,2], `Box3`[2,2], `Box3`[3,2], `Box3`[4,2],
         `Box3`[1,3], `Box3`[2,3], `Box3`[3,3], `Box3`[4,3]
)

##Dados de 24 meses atr�s
`Empr�stimos e financiamentos`=c(
  base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`)-24]/1000,
  base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`)-24]/1000,
  base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`)-24]/1000
)
`Empr�stimos e financiamentos`[4] = sum(`Empr�stimos e financiamentos`)

`T�tulos de d�vida` = c(
  base2$`Saldo de t�tulos p�blicos - Governo geral`[length(base2$`Saldo de t�tulos p�blicos - Governo geral`)-24]/1000,
  base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`[length(base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`)-24]/1000,
  base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`[length(base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`)-24]/1000
)
`T�tulos de d�vida`[4] = sum(`T�tulos de d�vida`)

`D�vida externa p�blica` = c(
  base2$`Saldo de d�vida externa - Empr�stimos ao governo`[length(base2$`Saldo de d�vida externa - Empr�stimos ao governo`)-24],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`)-24],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`)-24]
)
`D�vida externa p�blica`[4] = sum(`D�vida externa p�blica`)

`D�vida externa privada` = c(
  base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`[length(base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`)-24],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`)-24],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`)-24]
)
`D�vida externa privada`[4] = sum(`D�vida externa privada`)
`D�vida externa` = (`D�vida externa p�blica` + `D�vida externa privada`)/1000

`Box5`=cbind(`Empr�stimos e financiamentos`, `T�tulos de d�vida`, `D�vida externa`)
`Box6`=c(`Box5`[1,1], `Box5`[2,1], `Box5`[3,1], `Box5`[4,1], `Box5`[1,2], `Box5`[2,2], `Box5`[3,2], `Box5`[4,2],
         `Box5`[1,3], `Box5`[2,3], `Box5`[3,3], `Box5`[4,3]
)

##Dados do �ltimo m�s em %PIB
`Empr�stimos e financiamentos`=c(
  base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos do SFN ao setor n�o financeiro`)]/pib$`4382`[length(pib$`4382`)],
  base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de OSF ao setor n�o financeiro`)]/pib$`4382`[length(pib$`4382`)],
  base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`[length(base1$`Saldo de empr�stimos de fundos governamentais ao setor n�o financeiro`)]/pib$`4382`[length(pib$`4382`)]
)
`Empr�stimos e financiamentos`[4] = sum(`Empr�stimos e financiamentos`)

`T�tulos de d�vida` = c(
  base2$`Saldo de t�tulos p�blicos - Governo geral`[length(base2$`Saldo de t�tulos p�blicos - Governo geral`)]/pib$`4382`[length(pib$`4382`)],
  base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`[length(base3$`Saldo de t�tulos privados emitidos por empresas e fam�lias`)]/pib$`4382`[length(pib$`4382`)],
  base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`[length(base3$`Saldo de instrumentos de securitiza��o - devedores empresas e fam�lias`)]/pib$`4382`[length(pib$`4382`)]
)
`T�tulos de d�vida`[4] = sum(`T�tulos de d�vida`)

`D�vida externa p�blica` = c(
  base2$`Saldo de d�vida externa - Empr�stimos ao governo`[length(base2$`Saldo de d�vida externa - Empr�stimos ao governo`)],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado externo`)],
  base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`[length(base2$`Saldo de d�vida externa - T�tulos p�blicos emitidos no mercado dom�stico`)]
)
`D�vida externa p�blica`[4] = sum(`D�vida externa p�blica`)

`D�vida externa privada` = c(
  base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`[length(base3$`Saldo de d�vida externa - Empr�stimos a empresas e fam�lias`)],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado externo`)],
  base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`[length(base3$`Saldo de d�vida externa - T�tulos privados emitidos no mercado dom�stico`)]
)
`D�vida externa privada`[4] = sum(`D�vida externa privada`)

`D�vida externa` = (`D�vida externa p�blica` + `D�vida externa privada`)/pib$`4382`[length(pib$`4382`)]

`Box7`=cbind(`Empr�stimos e financiamentos`, `T�tulos de d�vida`, `D�vida externa`)
`Box8`=c(`Box7`[1,1], `Box7`[2,1], `Box7`[3,1], `Box7`[4,1], `Box7`[1,2], `Box7`[2,2], `Box7`[3,2], `Box7`[4,2],
         `Box7`[1,3], `Box7`[2,3], `Box7`[3,3], `Box7`[4,3]
)

`Box9`=as.data.frame(cbind(Box6, Box4, Box2, Box8))
`Box9`[13,] = Box9[4,] + Box9[8,] + Box9[12,]

row.names(Box9) = c("Opera��es de cr�dito do SFN", "   Outras sociedades financeiras", "   Fundos governamentais"," Total1",
                    "   T�tulos p�blicos", "   T�tulos privados", "   Instrumentos de securitiza��o", " Total2",
                    "   Empr�stimos", "   T�tulos emitidos no mercado externo", "   T�tulos emitidos no mercado dom�stico", " Total3",
                    "Total"
                    )

names(Box9) = c("24 meses atr�s", "12 meses atr�s", "�ltimo M�s", "Ultimo M�s - %PIB")

write.csv2(Box9, "Box.csv")
Box10 <- data.frame(Dados = row.names(Box9), Box9)
names(Box10) = c("Dados", "24 meses atr�s", "12 meses atr�s", "�ltimo M�s", "Ultimo M�s - %PIB")
export(Box10, "Saldo Ampliado.xlsx", which = "Box")