library("quantmod")

setwd("C:/Users/prosario/Documents/champions league")
""" Spanish Shares"""

getSymbols('SAN.MC',src='yahoo', from = "2001-01-01")
getSymbols('BBVA.MC',src='yahoo', from = "2001-01-01")
getSymbols('CABK.MC',src='yahoo', from = "2001-01-01")
getSymbols('SAB.MC',src='yahoo', from = "2001-01-01")
getSymbols('BKIA.MC',src='yahoo', from = "2001-01-01")
getSymbols('POP.MC',src='yahoo', from = "2001-01-01")
getSymbols('BKT.MC',src='yahoo', from = "2001-01-01")


write.csv(as.data.frame(SAN.MC), "SANMC.csv")
write.csv(as.data.frame(BBVA.MC), "BBVAMC.csv")
write.csv(as.data.frame(CABK.MC), "CABKMC.csv")
write.csv(as.data.frame(SAB.MC), "SABMC.csv")
write.csv(as.data.frame(BKIA.MC), "BKIAMC.csv")
write.csv(as.data.frame(POP.MC), "POPMC.csv")
write.csv(as.data.frame(BKT.MC), "BKTMC.csv")

""" Italian Shares"""

getSymbols('ISP.MI',src='yahoo', from = "2001-01-01")
getSymbols('UCG.MI',src='yahoo', from = "2001-01-01")
getSymbols('MB.MI',src='yahoo', from = "2001-01-01")
getSymbols('FBK.MI',src='yahoo', from = "2001-01-01")
getSymbols('UBI.MI',src='yahoo', from = "2001-01-01")
getSymbols('PMI.MI',src='yahoo', from = "2001-01-01")
getSymbols('BP.MI',src='yahoo', from = "2001-01-01")
getSymbols('BPE.MI',src='yahoo', from = "2001-01-01")
getSymbols('BMPS.MI',src='yahoo', from = "2001-01-01")



write.csv(as.data.frame(ISP.MI), "ISPMI.csv")
write.csv(as.data.frame(UCG.MI), "UCGMI.csv")
write.csv(as.data.frame(MB.MI), "MBMI.csv")
write.csv(as.data.frame(FBK.MI), "FBKMI.csv")
write.csv(as.data.frame(UBI.MI), "UBIMI.csv")
write.csv(as.data.frame(PMI.MI), "PMIMI.csv")
write.csv(as.data.frame(BP.MI), "BPMI.csv")
write.csv(as.data.frame(BPE.MI), "BPEMI.csv")
write.csv(as.data.frame(BMPS.MI), "BMPSMI.csv")

