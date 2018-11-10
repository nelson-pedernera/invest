#INFO: Una libreria con las herramientas y datasets para analizar datos economicos (de Argentina)
#USO:  source("libinvest.R")
#XXX: hacer "on demand" con funciones en vez de cargar todo de una
#XXX: estandarizar nombres de columnas, hacer compatible ej con quantstrat, etc.

library(data.table)
library(htmlwidgets)
library(dplyr)
library(ggplot2)
library(readr)
library(XLConnect) #U: el BCRA nos manda excel :(
#library(rjson)
library(stringr) #U: str_match_all
library(anytime)
library(plotly)

DIRDATOS <- "../x_datos_invest/"
dir.create(DIRDATOS)

fp <- function (fname) { #U: devuelve path estandar para datos
	paste0(DIRDATOS,fname);
}


bajarHoy <- function (url,fname) { #U: bajar un archivo si no lo tengo o no lo baje hoy
	dt <-Sys.Date() - as.Date(file.info(fname)$mtime)
	if (is.na(dt) || dt>0) { download.file(url,fname) }
	else { print(paste0("AL DIA ",fname))}
}

bajarUnaVez <- function (url,fname) { #U: bajar un archivo si no lo tengo
	dt <-Sys.Date() - as.Date(file.info(fname)$mtime)
	if (is.na(dt)) { download.file(url,fname) }
	else { print(paste0("AL DIA ",fname))}
}


preciosHistoricos <- function (k) { #U leer precios historicos de Rava (bajarlos si no los tenia)
	fname <- fp(paste0("x_",k,".csv"))
	bajarHoy(
		paste0("http://www.ravaonline.com/v2/empresas/precioshistoricos.php?e=",k,"&csv=1"),
		fname)
	df <-read_csv(fname)
	df$fecha <- as.Date(df$fecha)
	df$precio <- df$cierre
	df
}

############################################################
#S: url de datos que uso
DEPH_hist_url="https://www.indec.gob.ar/ftp/cuadros/sociedad/eph_ing_total_cuadro5.xls"

ISalarios_url="https://www.indec.gob.ar/ftp/cuadros/economia/serie_is_2012.xls"
ISalariosLast_url="https://www.indec.gob.ar/bajarCuadroEstadistico.asp?idc=478A1F345E953A2E6A41C9E2B963AAA1522A20390C3CDA07C3C3FE3F37338DF55317174114238A7C"

TcUSDHistBcra_url= "http://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/com3500.xls"
TcUSDHist_url= "http://infra.datos.gob.ar/catalog/sspm/dataset/175/distribution/175.1/download/tipos-de-cambio-historicos.csv"
TcUSDHistYFu_url= "http://infra.datos.gob.ar/catalog/sspm/dataset/168/distribution/168.1/download/datos-tipo-cambio-usd-futuro-dolar-frecuencia-diaria.csv"
# http://infra.datos.gob.ar/catalog/sspm/dataset/90/distribution/90.1/download/agregados-monetarios.csv

TcCER_url= "http://infra.datos.gob.ar/catalog/sspm/dataset/94/distribution/94.2/download/cer-uva-uvi-diarios.csv"

VolUSDBcra_url= "http://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/MC_Sectorial.xls"

############################################################
#S: bajar y normalizar datos 

bajarHoy(TcUSDHistYFu_url,fp("x_USD.csv"))
pUSD= read_csv(fp("x_USD.csv"))
pUSD$precio <- pUSD$tipo_cambio_a3500
colnames(pUSD)[colnames(pUSD)=="indice_tiempo"] <- "fecha"
tail(pUSD)

# 
# 
bajarHoy(TcUSDHistBcra_url,fp("x_USD_BCRA.xls"))
pUSDBcra= readWorksheetFromFile(fp("x_USD_BCRA.xls"),1,4,2,endCol=4)
colnames(pUSDBcra) <- c("fecha","precio")
pUSDBcra$fecha <- as.Date(pUSDBcra$fecha)


#DOLAR, volumen mercado cambiario
bajarHoy(VolUSDBcra_url,fp("x_USD_VOL_BCRA.xls"));
vUSDBcra= readWorksheetFromFile(fp("x_USD_VOL_BCRA.xls"),1,28,1,endCol=85)
vUSDBcra$Col2 <- NULL
vUSDBcra$Col3 <- NULL
colnames(vUSDBcra) <- strsplit("fecha v vcc vBienes vBienesI vBienesICobros vBienesIAnticipos vBienesIFin vBienesIFinPre vBienesIOtros vBienesO vBienesOAnticipos vBienesOVista vBienesODif vServicios vServiciosI vServiciosIFletes vServiciosITarjeta vServiciosIViajeros vServiciosIProf vServiciosIIP vServiciosISeguros vServiciosIComunic vServiciosIIT vServiciosIOtros vServiciosO vServiciosOFletes vServiciosOTarjeta vServiciosOViajeros vServiciosOProf vServiciosOIP vServiciosOSeguros vServiciosOComunic vServiciosOIT vServiciosOOtros vPrimario vPrimarioI vPrimarioIInteres vPrimarioIUtilidades vPrimarioIOtros vPrimarioO vPrimarioInteres vPrimarioOUtilidades vPrimarioOOtros vSecundario vSecundarioI vSecundarioO vCCapital vCCapitalI vCCapitalO vCFinanciera vCFinancieraID vCFinancieraIDI vCFinancieraIDIDirecta vCFinancieraIDIInmbuebles vCFinancieraIDIPortafolio vCFinancieraIDO vCFinancieraIDODirecta vCFinancieraIDOInmuebles vCFinancieraIDOPortafolio vCFinancieraDeuda vCFinancieraDeudaI vCFinancieraDeudaIOrg vCFinancieraDeudaIOtros vCFinancieraDeudaILineas vCFinancieraDeudaILocales vCFinancieraDeudaO vCFinancieraDeudaOOrg vCFinancieraDeudaOOtros vCFinancieraDeudaOLineas vCFinancieraDeudaOLocales vExternos vExternosI vExternosIBill vExternosIDivisas vExternosO vExternosOBill vExternosODivisas vCanje vTitulos vOtros vNoInf vTarjetasO"," ")[[1]]
vUSDBcra$fecha <- as.Date(paste0(vUSDBcra$fecha,"01"),"%Y%m%d")

#TASA
TasaBancos1D_url=paste0("https://www.cronista.com/MercadosOnline/json/getDinamicos.html?tipo=tasas&id=ARSCMPR1D&fechaDesde=7/7/2001&fechaHasta=",format(Sys.time(),"%d/%m/%Y"))
bajarHoy(TasaBancos1D_url,fp("x_tasa1.json"))
iBancos1D= local({
	x <- str_match_all(
		paste(readLines(fp("x_tasa1.json")),collapse=""),
		"Compra\":([0-9.]+).*?Date\\((\\d+)"
	)
	
	df <- data.frame(fecha= as.Date(anytime(as.numeric(x[[1]][,3])/1000)), tasa= 1+as.double(x[[1]][,2])/100)
	df[order(df$fecha),]
})

iBancos1D$tasa_dia= iBancos1D$tasa^(1/365)
iBancos1D$precio= cumprod(iBancos1D$tasa_dia)

#CER
bajarHoy(TcCER_url,fp("x_CER.csv"));
pCER= read_csv(fp("x_CER.csv"));
pCER$precio <- pCER$cer_diario
colnames(pCER)[colnames(pCER)=="indice_tiempo"] <- "fecha"

#Salarios
bajarHoy(ISalarios_url,fp("x_salarios.xls"))
pSalarios= readWorksheetFromFile(fp("x_salarios.xls"),2,8,1,endCol=4)
colnames(pSalarios) <- c("fecha","pregistrado","pnoregistrado","publico") #,"idx","varmensual")
pSalarios$fecha <- as.Date(pSalarios$fecha)
pSalarios <- filter(pSalarios,!is.na(pSalarios$fecha))
#A: hasta 2015-10-01

bajarHoy(ISalariosLast_url,fp("x_salarios_last.xls"))
pSalariosTmp= readWorksheetFromFile(fp("x_salarios_last.xls"),1,8,1,endCol=16)
pSalariosTmp= filter(pSalariosTmp,!is.na(pSalariosTmp$Col2))
pSalariosTmp$fecha <- seq(as.Date("2015-10-01"),by="month", length.out = nrow(pSalariosTmp))

fReg= pSalarios[nrow(pSalarios),"pregistrado"]/pSalariosTmp[1,"Col4"]
fPub= pSalarios[nrow(pSalarios),"publico"]/pSalariosTmp[1,"Col7"]

pSalarios2 <- data.frame(
	fecha=pSalariosTmp$fecha, 
	pregistrado=pSalariosTmp$Col4 * fReg,
	pnoregistrado=as.numeric(pSalariosTmp$Col13) * fReg,
	publico=pSalariosTmp$Col7 * fPub
)

pSalarios <- rbind(pSalarios, pSalarios2[2:nrow(pSalarios2),])
pSalarios2 <- NULL
pSalariosTmp <- NULL
#A: tengo los salarios en una sola serie

bajarUnaVez(DEPH_hist_url,fp("x_eph_2003-2015_perCapita.xls"))
dEPH15Caba= readWorksheetFromFile(fp("x_eph_2003-2015_perCapita.xls"),27,6,1,endCol=10)
dEPH15Caba$decil <- as.numeric(dEPH15Caba$Grupo.decílico)
dEPH15Caba= filter(dEPH15Caba,!is.na(dEPH15Caba$decil))
dEPH15Caba$decil <- as.factor(dEPH15Caba$decil)
dEPH15Caba$Grupo.decílico <- NULL
dEPH15Caba$X..de.hogares <- NULL
dEPH15Caba$Media <- as.numeric(gsub(",","",dEPH15Caba$Media))

dEPH15Caba$fecha <- rep(seq(as.Date('2015-04-01'),as.Date('2003-10-01'),by="-3 month"), each=10)
dEPH15Caba <- dEPH15Caba[order(dEPH15Caba$fecha,dEPH15Caba$decil),]
#for (d in 1:10) {
#	idx <- dEPH15Caba$decil==d
#	dEPH15Caba$tasa[idx] <- append(diff(dEPH15Caba$Media[idx]),0,after=0)/dEPH15Caba$Media[idx]
#}
#A: no calculo aqui la tasa porque probablemente la quiera ver en USD o CER

#Propiedades, CABA
#SEE: http://www.estadisticaciudad.gob.ar/eyc/?cat=129
# Precio promedio del m2 (dólares) de departamentos en venta de 2 ambientes usados por barrio. Ciudad de Buenos Aires. 4to. trimestre 2006/3er. trimestre 2018
DPropM2amb2u <- "http://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2018/05/MI_DVP_AX03.xls"
bajarUnaVez(DPropM2amb2u,fp("x_prop_m2_2amb_usado.xls"))
x_m2 <- readWorksheetFromFile(fp("x_prop_m2_2amb_usado.xls"),1,3,1,endRow=52)
pPropM2amb2u <- as.data.frame(t(x_m2[,2:ncol(x_m2)])) %>% mutate_all(function (x) {as.numeric(gsub(",","",x))})
colnames(pPropM2amb2u) <- x_m2[,1]
rownames(pPropM2amb2u) <- pPropM2amb2u$fecha <- seq(as.Date('2007-01-01'),length.out= nrow(pPropM2amb2u),by="3 month")

#ACCIONES
pMERVAL <- preciosHistoricos("MERVAL")
pALUA <- preciosHistoricos("ALUA")
pYPFD <- preciosHistoricos("YPFD")
pPBR <- preciosHistoricos("PBR")
pTS <- preciosHistoricos("TS")
pGGAL <- preciosHistoricos("GGAL")

############################################################
#S: graficos estandar

#FECHAS
fechaMacriPre= as.Date("2015-11-02") #A: un poco antes de Macri
fechaMacriIni= as.Date("2016-01-21") #A: despues que asumio, fin del cepo
fechaMacriEstable= as.Date("2016-03-21") #A: despues que asumio, fin del cepo

# Conseguir un factor para igualar dos series en una fecha dada, para comparar cambios/crecimiento
multiplicadorFecha <- function (pX, fecha) {
	x1 <- pUSDBcra[pUSDBcra$fecha==as.Date(fecha),]
	x2 <- pX[pX$fecha==as.Date(fecha),]
	x2$precio/x1$precio
}

# Un grafico donde dolar, CER y MERVAL las especies coinciden a la fecha f0
graficoSimpleAFecha <- function (f0) {
	mTasa= multiplicadorFecha(iBancos1D,f0)
	mCER= multiplicadorFecha(pCER,f0)
	mMERVAL= multiplicadorFecha(pMERVAL,f0)
	#A: escale todos los valores a una fecha, donde coinciden todos...
	
	dfx <- pUSDBcra %>% mutate(especie="USD") %>%
		bind_rows(pCER %>% mutate(especie="CER",precio=precio/mCER)) %>%
		bind_rows(pMERVAL %>% mutate(especie="MERVAL", precio=precio/mMERVAL))
	
	p <- ggplot(dfx,aes(y = precio,x=fecha,color=especie)) + 
		geom_line() +
		scale_color_manual(values=c("red","orange","green")) +
		theme(legend.position=c(.1,.85)) +
		labs(title=paste("Si tenia",pUSDBcra[pUSDBcra$fecha == f0,"precio"],"pesos el dia ",f0," cuanto valdrían invertidos en ...")) +
		ylab("pesos")
	
	p
}

# Un grafico donde todas las especies coinciden a la fecha f0
graficoUnificadoAFecha <- function (f0) {
	mTasa= multiplicadorFecha(iBancos1D,f0)
	mCER= multiplicadorFecha(pCER,f0)
	mMERVAL= multiplicadorFecha(pMERVAL,f0)
	
	mYPFD= multiplicadorFecha(pYPFD,f0)
	mPBR= multiplicadorFecha(pPBR,f0)
	
	mTS= multiplicadorFecha(pTS,f0)
	
	mALUA= multiplicadorFecha(pALUA,f0)
	
	mGGAL= multiplicadorFecha(pGGAL,f0)
	
	
	#A: escale todos los valores a una fecha, donde coinciden todos...
	
	p <- ggplot()+
		geom_line(data=iBancos1D, aes(x=fecha,y=(tasa-1)*100, text=fecha),col="pink",size=.5) + scale_y_continuous(limits= c(0,100))+
		geom_line(data=pUSDBcra,aes(x=fecha,y=precio/1, text=fecha), col="green") +
		geom_line(data=pCER,aes(x=fecha,y=precio/mCER, text=fecha), col="red") +
		geom_line(data=pMERVAL,aes(x=fecha,y=precio/mMERVAL, text=fecha), col="orange") +
		geom_line(data=pALUA,aes(x=fecha,y=precio/mALUA, text=fecha), stat = "identity", col = "purple")+
		geom_line(data=pYPFD,aes(x=fecha,y=precio/mYPFD, text=fecha), stat = "identity", col = "blue")+
		geom_line(data=pPBR,aes(x=fecha,y=precio/mPBR, text=fecha), stat = "identity", col = "lightblue")+
		geom_line(data=pTS,aes(x=fecha,y=precio/mTS, text=fecha), stat = "identity", col = "yellow")+
		geom_line(data=pGGAL,aes(x=fecha,y=precio/mGGAL, text=fecha), stat = "identity", col = "brown")
	p
}


#############################################################
#S: convertir a dolares algunos valores
#XXX: generalizar para cualquier serie de las que tengo

pUSDMes <- pUSDBcra %>% 
	mutate(fecha=as.Date(format(fecha,"%Y-%m-01"))) %>% 
	group_by(fecha) %>% 
	summarize(usd=max(precio))

pCERMes <- pCER %>% 
	mutate(fecha=as.Date(format(fecha,"%Y-%m-01"))) %>% 
	group_by(fecha) %>% 
	summarize(cer=max(precio))

diaIgual <- as.Date("2018-01-01")
fCER <- as.numeric(pUSDMes[pUSDMes$fecha==diaIgual,"usd"]/pCERMes[pCERMes$fecha==diaIgual,"cer"])
fSalario <- as.numeric(pUSDMes[pUSDMes$fecha==diaIgual,"usd"]/pSalarios[pSalarios$fecha==diaIgual,"pregistrado"])

