#Rotina para coletar e apresentar em gráficos algumas séries do banco central
#Feito por: Felipe Simplício Ferreira
#última atualização: 0/01/2019


#Definindo diretórios a serem utilizados

getwd()
setwd("C:/Users/e270780232/Documents")

#Definindo data incial  e final (padrão usar a data de hoje) das séries que serão coletadas
datainicial = "01/01/2013"
datafinal = format(Sys.time(), "%d/%m/%Y")


#Saldos
serie = c("28183", "28184", "28185", "28186", "28187", "28188", "28189", "28190", "28191", "28192", "28193","28194", "28195")

#Repetição para coletar e juntar séries em um arquivo
for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base1 = serie1
  else
    base1 = merge(base1, dados, by = "data", all = T)
}

#Nomeando colunas do dataframe com todas as séries
names(base1) = c("Data", "Saldo de crédito ampliado - Total", "Saldo de empréstimos total ao setor não financeiro", "Saldo de empréstimos do SFN ao setor não financeiro", "Saldo de empréstimos de OSF ao setor não financeiro",
                "Saldo de empréstimos de fundos governamentais ao setor não financeiro", "Saldo de títulos de dívida - Total", "Saldo de títulos públicos", "Saldo de títulos privados", "Saldo de instrumentos de securitização",
                "Saldo de dívida externa - Total", "Saldo de dívida externa - Empréstimos", "Saldo de dívida externa - Títulos emitidos no mercado externo", "Saldo de dívida externa - Títulos emitidos no mercado doméstico")

rm(dados)
rm(list=objects(pattern="^serie"))

write.csv2(base1,"1-Saldos.csv", row.names = F)


#Saldos - Governo geral
serie = c("28196", "28197", "28198", "28199", "28200", "28201", "28202")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base2 = serie1
  else
    base2 = merge(base2, dados, by = "data", all = T)
}

names(base2) = c("Data", "Saldo de crédito ampliado ao governo - Total", "Saldo de empréstimos do SFN ao governo", "Saldo de títulos públicos - Governo geral", "Saldo de dívida externa - Concedido ao governo - Total", "Saldo de dívida externa - Empréstimos ao governo",
                "Saldo de dívida externa - Títulos públicos emitidos no mercado externo", "Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico")

rm(dados)
rm(list=objects(pattern="^serie"))

write.csv2(base2,"2-Saldos governo geral.csv", row.names = F)

#Saldos - Empresas e famílias
serie = c("28203", "28204", "28205", "28206", "28207", "28208", "28209", "28210", "28211", "28212", "28213", "28214")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base3 = serie1
  else
    base3 = merge(base3, dados, by = "data", all = T)
}

names(base3) = c("Data", "Saldo de crédito ampliado a empresas e famílias - Total", "Saldo de empréstimos a empresas e famílias - Total", "Saldo de empréstimos do SFN a empresas e famílias",
                "Saldo de empréstimos de OSF a empresas e famílias", "Saldo de empréstimos de fundos governamentais a empresas e famílias", "Saldo de títulos de dívida emitidos por empresas e famílias - Total",
                "Saldo de títulos privados emitidos por empresas e famílias", "Saldo de instrumentos de securitização - devedores empresas e famílias", "Saldo de dívida externa - Concedido a empresas e famílias - Total",
                "Saldo de dívida externa - Empréstimos a empresas e famílias", "Saldo de dívida externa - Títulos privados emitidos no mercado externo", "Saldo de dívida externa - Títulos privados emitidos no mercado doméstico")

rm(dados)
rm(list=objects(pattern="^serie"))

write.csv2(base3,"3-Saldos empresas e famílias.csv", row.names = F)

#PIB acumulado dos últimos 12 meses - Valores correntes (R$ milhões)
serie = "4382"
pib = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
pib[,2]=as.numeric(gsub(",","\\.",pib[,2]))

#Montagem da tabela do BCB
##Dados do último mês
`Empréstimos e financiamentos`=c(
  base1$`Saldo de empréstimos do SFN ao setor não financeiro`[length(base1$`Saldo de empréstimos do SFN ao setor não financeiro`)]/1000,
  base1$`Saldo de empréstimos de OSF ao setor não financeiro`[length(base1$`Saldo de empréstimos de OSF ao setor não financeiro`)]/1000,
  base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`[length(base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`)]/1000
  )
`Empréstimos e financiamentos`[4] = sum(`Empréstimos e financiamentos`)

`Títulos de dívida` = c(
  base2$`Saldo de títulos públicos - Governo geral`[length(base2$`Saldo de títulos públicos - Governo geral`)]/1000,
  base3$`Saldo de títulos privados emitidos por empresas e famílias`[length(base3$`Saldo de títulos privados emitidos por empresas e famílias`)]/1000,
  base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`[length(base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`)]/1000
)
`Títulos de dívida`[4] = sum(`Títulos de dívida`)

`Dívida externa pública` = c(
  base2$`Saldo de dívida externa - Empréstimos ao governo`[length(base2$`Saldo de dívida externa - Empréstimos ao governo`)],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`)],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`)]
)
`Dívida externa pública`[4] = sum(`Dívida externa pública`)

`Dívida externa privada` = c(
  base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`[length(base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`)],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`)],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`)]
)
`Dívida externa privada`[4] = sum(`Dívida externa privada`)

`Dívida externa` = (`Dívida externa pública` + `Dívida externa privada`)/1000

`Box1`=cbind(`Empréstimos e financiamentos`, `Títulos de dívida`, `Dívida externa`)
`Box2`=c(`Box1`[1,1], `Box1`[2,1], `Box1`[3,1], `Box1`[4,1], `Box1`[1,2], `Box1`[2,2], `Box1`[3,2], `Box1`[4,2],
         `Box1`[1,3], `Box1`[2,3], `Box1`[3,3], `Box1`[4,3]
)

##Dados de 12 meses atrás
`Empréstimos e financiamentos`=c(
  base1$`Saldo de empréstimos do SFN ao setor não financeiro`[length(base1$`Saldo de empréstimos do SFN ao setor não financeiro`)-12]/1000,
  base1$`Saldo de empréstimos de OSF ao setor não financeiro`[length(base1$`Saldo de empréstimos de OSF ao setor não financeiro`)-12]/1000,
  base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`[length(base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`)-12]/1000
)
`Empréstimos e financiamentos`[4] = sum(`Empréstimos e financiamentos`)

`Títulos de dívida` = c(
  base2$`Saldo de títulos públicos - Governo geral`[length(base2$`Saldo de títulos públicos - Governo geral`)-12]/1000,
  base3$`Saldo de títulos privados emitidos por empresas e famílias`[length(base3$`Saldo de títulos privados emitidos por empresas e famílias`)-12]/1000,
  base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`[length(base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`)-12]/1000
)
`Títulos de dívida`[4] = sum(`Títulos de dívida`)

`Dívida externa pública` = c(
  base2$`Saldo de dívida externa - Empréstimos ao governo`[length(base2$`Saldo de dívida externa - Empréstimos ao governo`)-12],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`)-12],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`)-12]
)
`Dívida externa pública`[4] = sum(`Dívida externa pública`)

`Dívida externa privada` = c(
  base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`[length(base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`)-12],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`)-12],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`)-12]
)
`Dívida externa privada`[4] = sum(`Dívida externa privada`)
`Dívida externa` = (`Dívida externa pública` + `Dívida externa privada`)/1000

`Box3`=cbind(`Empréstimos e financiamentos`, `Títulos de dívida`, `Dívida externa`)
`Box4`=c(`Box3`[1,1], `Box3`[2,1], `Box3`[3,1], `Box3`[4,1], `Box3`[1,2], `Box3`[2,2], `Box3`[3,2], `Box3`[4,2],
         `Box3`[1,3], `Box3`[2,3], `Box3`[3,3], `Box3`[4,3]
)

##Dados de 24 meses atrás
`Empréstimos e financiamentos`=c(
  base1$`Saldo de empréstimos do SFN ao setor não financeiro`[length(base1$`Saldo de empréstimos do SFN ao setor não financeiro`)-24]/1000,
  base1$`Saldo de empréstimos de OSF ao setor não financeiro`[length(base1$`Saldo de empréstimos de OSF ao setor não financeiro`)-24]/1000,
  base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`[length(base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`)-24]/1000
)
`Empréstimos e financiamentos`[4] = sum(`Empréstimos e financiamentos`)

`Títulos de dívida` = c(
  base2$`Saldo de títulos públicos - Governo geral`[length(base2$`Saldo de títulos públicos - Governo geral`)-24]/1000,
  base3$`Saldo de títulos privados emitidos por empresas e famílias`[length(base3$`Saldo de títulos privados emitidos por empresas e famílias`)-24]/1000,
  base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`[length(base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`)-24]/1000
)
`Títulos de dívida`[4] = sum(`Títulos de dívida`)

`Dívida externa pública` = c(
  base2$`Saldo de dívida externa - Empréstimos ao governo`[length(base2$`Saldo de dívida externa - Empréstimos ao governo`)-24],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`)-24],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`)-24]
)
`Dívida externa pública`[4] = sum(`Dívida externa pública`)

`Dívida externa privada` = c(
  base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`[length(base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`)-24],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`)-24],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`)-24]
)
`Dívida externa privada`[4] = sum(`Dívida externa privada`)
`Dívida externa` = (`Dívida externa pública` + `Dívida externa privada`)/1000

`Box5`=cbind(`Empréstimos e financiamentos`, `Títulos de dívida`, `Dívida externa`)
`Box6`=c(`Box5`[1,1], `Box5`[2,1], `Box5`[3,1], `Box5`[4,1], `Box5`[1,2], `Box5`[2,2], `Box5`[3,2], `Box5`[4,2],
         `Box5`[1,3], `Box5`[2,3], `Box5`[3,3], `Box5`[4,3]
)

##Dados do último mês em %PIB
`Empréstimos e financiamentos`=c(
  base1$`Saldo de empréstimos do SFN ao setor não financeiro`[length(base1$`Saldo de empréstimos do SFN ao setor não financeiro`)]/pib$valor[length(pib$valor)],
  base1$`Saldo de empréstimos de OSF ao setor não financeiro`[length(base1$`Saldo de empréstimos de OSF ao setor não financeiro`)]/pib$valor[length(pib$valor)],
  base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`[length(base1$`Saldo de empréstimos de fundos governamentais ao setor não financeiro`)]/pib$valor[length(pib$valor)]
)
`Empréstimos e financiamentos`[4] = sum(`Empréstimos e financiamentos`)

`Títulos de dívida` = c(
  base2$`Saldo de títulos públicos - Governo geral`[length(base2$`Saldo de títulos públicos - Governo geral`)]/pib$valor[length(pib$valor)],
  base3$`Saldo de títulos privados emitidos por empresas e famílias`[length(base3$`Saldo de títulos privados emitidos por empresas e famílias`)]/pib$valor[length(pib$valor)],
  base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`[length(base3$`Saldo de instrumentos de securitização - devedores empresas e famílias`)]/pib$valor[length(pib$valor)]
)
`Títulos de dívida`[4] = sum(`Títulos de dívida`)

`Dívida externa pública` = c(
  base2$`Saldo de dívida externa - Empréstimos ao governo`[length(base2$`Saldo de dívida externa - Empréstimos ao governo`)],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado externo`)],
  base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`[length(base2$`Saldo de dívida externa - Títulos públicos emitidos no mercado doméstico`)]
)
`Dívida externa pública`[4] = sum(`Dívida externa pública`)

`Dívida externa privada` = c(
  base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`[length(base3$`Saldo de dívida externa - Empréstimos a empresas e famílias`)],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado externo`)],
  base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`[length(base3$`Saldo de dívida externa - Títulos privados emitidos no mercado doméstico`)]
)
`Dívida externa privada`[4] = sum(`Dívida externa privada`)

`Dívida externa` = (`Dívida externa pública` + `Dívida externa privada`)/pib$valor[length(pib$valor)]

`Box7`=cbind(`Empréstimos e financiamentos`, `Títulos de dívida`, `Dívida externa`)
`Box8`=c(`Box7`[1,1], `Box7`[2,1], `Box7`[3,1], `Box7`[4,1], `Box7`[1,2], `Box7`[2,2], `Box7`[3,2], `Box7`[4,2],
         `Box7`[1,3], `Box7`[2,3], `Box7`[3,3], `Box7`[4,3]
)

`Box9`=as.data.frame(cbind(Box6, Box4, Box2, Box8))
`Box9`[13,] = Box9[4,] + Box9[8,] + Box9[12,]

row.names(Box9) = c("Operações de crédito do SFN", "   Outras sociedades financeiras", "   Fundos governamentais"," Total1",
                    "   Títulos públicos", "   Títulos privados", "   Instrumentos de securitização", " Total2",
                    "   Empréstimos", "   Títulos emitidos no mercado externo", "   Títulos emitidos no mercado doméstico", " Total3",
                    "Total"
                    )

names(Box9) = c("24 meses atrás", "12 meses atrás", "Último Mês", "Ultimo Mês - %PIB")

write.csv2(Box9, "Box.csv")

