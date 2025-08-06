#Library yang digunakan
library(openxlsx)
library(readxl)
library(metafor)
library(lme4)
library(extraDistr)
library(ggplot2)
library(meta)
library(glmmTMB)
library(GLMMadaptive)



#Memasukkan data dari tiap penelitian
Data_Skripsi = read_excel("C:/Users/ASUS/Downloads/Data Skripsi.xlsx", sheet = "Sheet 1")
View(Data_Skripsi)



#Mendefinisikan variabel dari penelitian
#Treatment = Penggunaan Kontrasepsi, Event = Kanker Payudara
event_treatment = Data_Skripsi$a 
noevent_treatment = Data_Skripsi$b 
event_control = Data_Skripsi$c 
noevent_control = Data_Skripsi$d 
total_treatment = Data_Skripsi$n1 
total_control = Data_Skripsi$n2 
total = Data_Skripsi$N 
study_id = Data_Skripsi$No 
jenis_kontrasepsi = Data_Skripsi$`Jenis Kontrasepsi`


#Visualisasi data
data = data.frame(
  value = c(Data_Skripsi$a, Data_Skripsi$b, Data_Skripsi$c, Data_Skripsi$d),
  category = factor(rep(c("a", "b", "c", "d"), each = length(Data_Skripsi$a)))
)
Total = aggregate(value ~ category, data = data, FUN = sum)

##Buat plot menggunakan ggplot
ggplot(data, aes(x = category, y = value, fill = category, label = value)) +
  geom_bar(stat = "identity") +
  geom_text(data = Total, aes(label = value, y = value), vjust = -0.5, size = 3) +
  labs(title = "Diagram Batang Banyak Sampel Pada Penelitian",
       x = "Kategori",
       y = "Nilai") +
  scale_fill_manual(values = c("a" = "red", "b" = "blue", "c" = "green", "d" = "orange"))



#Membuat dataframe baru yang sesuai untuk packages "Metafor"
metafor_df = data.frame(study_id = factor(1:37), 
                        jenis_kontrasepsi = jenis_kontrasepsi,
                        tpos = event_treatment,
                        tneg = noevent_treatment,
                        cpos = event_control,
                        cneg = noevent_control,
                        or = (event_treatment*noevent_control)/(noevent_treatment*event_control),
                        logor = log(event_treatment*noevent_control)/(noevent_treatment*event_control))

metafor_df

#Mantel-Haeszel Method
####Model Mantel-Haeszel 
glmm_mh = rma.mh(measure = 'OR', ai = tpos, bi = tneg, ci = cpos, di = cneg,
                 data = metafor_df,  method = "ML", level = 95, verbose = TRUE)

predict(glmm_mh) 
coef(glmm_mh)
vcov(glmm_mh)
cumul.rma.mh(glmm_mh)
summary(glmm_mh)
weights(glmm_mh)



#Visualisasi Mantel-Haeszel Method
####Forest Plot
forest(glmm_mh, main = 'Forest Plot of Mantel-Haenszel Model')
forest(glmm_mh, addpred = TRUE, xlim=c(-16,7), 
       ilab = cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5), at=seq(-5,10,by=1), 
       main = 'Forest Plot of Approx Mantel-Haeszel Model', header = "Penelitian")
text(c(-9.5,-8,-6,-4.5), glmm_mh$k+2, c("KP+","KP-","KP+","KP-"), cex = 0.5)
text(c(-8.75,-5.25), glmm_mh$k+4, c("Treatment","Control"), cex = 0.5)



#Kriteria Pemilihan model
###Fail-Safe N
fsn(glmm_mh$yi, glmm_mh$vi, glmm_mh, type="General")


###Identifikasi Bias
#Funnel Plot
plot(glmm_mh, qqplot = TRUE)
funnel(glmm_mh, atransf = exp)




#Hypergeometric-Normal Model
####Hypergeometric-Normal Model
glmm_hn_1 = rma.glmm(measure = 'OR', ai = tpos, bi = tneg, ci = cpos, di = cneg,
                     data = metafor_df, model = 'CM.EL', method = "ML", level = 95, nAGQ = 7, control = list(packages='GLMMadaptive'), verbose = TRUE)

predict(glmm_hn_1) 
coef(glmm_hn_1)
vcov(glmm_hn_1)
summary(glmm_hn_1)



#Visualisasi Hypergeometric-Normal
####Forest Plot
forest(glmm_hn_1, main = 'Forest Plot of Hypergeo-Normal Model')
forest(glmm_hn_1, addpred = TRUE, xlim=c(-16,7), 
       ilab = cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5), at=seq(-5,10,by=1), 
       main = 'Forest Plot of Approx Hypergeometric-Normal Model', header = "Penelitian")
text(c(-9.5,-8,-6,-4.5), glmm_hn_1$k+2, c("KP+","KP-","KP+","KP-"), cex = 0.5)
text(c(-8.75,-5.25), glmm_hn_1$k+4, c("Treatment","Control"), cex = 0.5)



#Kriteria Pemilihan model
###Fail-Safe N
fsn(glmm_hn_1$yi, glmm_hn_1$vi, glmm_hn_1, type="General")

###Identifikasi Bias
# Funnel Plot
funnel(glmm_hn_1, atransf = exp)



#Binomial-Normal Model
####GLMM Approximated Binomial-Normal Model
glmm_abn_1 = rma.glmm(measure = 'OR', ai = tpos, bi = tneg, ci = cpos, di = cneg, random = ~ 1 | study_id,
                      data = metafor_df, model = 'CM.AL',  method = "ML", level = 95, nAGQ = 7, control = list(packages='GLMMadaptive'), verbose = TRUE)

predict(glmm_abn_1)
coef(glmm_abn_1)
vcov(glmm_abn_1)
summary(glmm_abn_1)


#Visualisasi Binomial-Normal Model
####Forest Plot
forest(glmm_abn_1, main = 'Forest Plot of Approx Binomial-Normal Model')
forest(glmm_abn_1, addpred = TRUE, xlim=c(-16,7), 
       ilab = cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5), at=seq(-5,10,by=1), 
       main = 'Forest Plot of Approx Binomial-Normal Model', header = "Penelitian")
text(c(-9.5,-8,-6,-4.5), glmm_abn_1$k+2, c("KP+","KP-","KP+","KP-"), cex = 0.5)
text(c(-8.75,-5.25), glmm_abn_1$k+4, c("Treatment","Control"), cex = 0.5)



#Kriteria Pemilihan model
###Fail-Safe N
fsn(glmm_abn_1$yi, glmm_abn_1$vi, glmm_abn_1, type="General")


###Identifikasi Bias
# Funnel Plot
funnel(glmm_abn_1, atransf = exp)


#Analisis Subgrup
##Membuat dataframe baru menyesuaikan analisis subgrup
###Dataframe untuk Jenis Kontrasepsi
Jenis_Kontrasepsi = Data_Skripsi$`Jenis Kontrasepsi`
dummy = model.matrix(~ Jenis_Kontrasepsi, data = Data_Skripsi) #Untuk Hormonal vs Tidak Menggunakan
metafor_df2 = data.frame(study_id = factor(1:37),
                         tpos = event_treatment,
                         tneg = noevent_treatment,
                         cpos = event_control,
                         cneg = noevent_control,
                         dummy,
                         or = (event_treatment*noevent_control)/(noevent_treatment*event_control),
                         logor = log(event_treatment*noevent_control)/(noevent_treatment*event_control))

metafor_df2

###Dataframe untuk Ukuran Sampel
dummy2 = ifelse(Data_Skripsi$N < 100, "a", ifelse(Data_Skripsi$N >= 100 & Data_Skripsi$N < 1000, "b", "c"))
dummy3 = model.matrix(~ dummy2 - 1, data = Data_Skripsi) #Hasil a dan b
metafor_df3 = data.frame(study_id = factor(1:37),
                         tpos = event_treatment,
                         tneg = noevent_treatment,
                         cpos = event_control,
                         cneg = noevent_control,
                         dummy3,
                         or = (event_treatment*noevent_control)/(noevent_treatment*event_control),
                         logor = log(event_treatment*noevent_control)/(noevent_treatment*event_control))

metafor_df3


#Binomial-Normal Model
##1. Variabel Moderator: Jenis Kontrasepsi
#####Binomial-Normal Model (CM.AL)
glmm_abn2 = rma.glmm(measure = 'OR', ai = tpos, bi = tneg, ci = cpos, di = cneg,
                     data = metafor_df2, model = 'CM.AL', mods = ~ dummy, method = "ML", level = 95, nAGQ = 7, control = list(packages='GLMMadaptive'), verbose = TRUE)

predict(glmm_abn2)
coef(glmm_abn2)
vcov(glmm_abn2)
summary(glmm_abn2)




#Visualisasi Binomial-Normal Model dengan Variabel Moderator Jenis Kontrasepsi
####Forest Plot
forest(glmm_abn2, main = 'Forest Plot Jenis Kontrasepsi', order=Jenis_Kontrasepsi)
forest.rma(glmm_abn2, addpred = TRUE, xlim=c(-16,7), 
           ilab = cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5), at=seq(-5,10,by=1), 
           main = 'Forest Plot Jenis Kontrasepsi', header = "Penelitian", addfit = FALSE, order=Jenis_Kontrasepsi, top=2)
text(c(-9.5,-8,-6,-4.5), glmm_abn2$k+2, c("KP+","KP-","KP+","KP-"), cex = 0.5)
text(c(-8.75,-5.25), glmm_abn2$k+3, c("Treatment","Control"), cex = 0.5)



#Kriteria Pemilihan model
###Fail-Safe N
fsn(glmm_abn2$yi, glmm_abn2$vi, glmm_abn2, type="General")



##2. Variabel Moderator: Ukuran Sampel
#####Binomial-Normal Model (CM.AL)
glmm_abn3 = rma.glmm(measure = 'OR', ai = tpos, bi = tneg, ci = cpos, di = cneg,
                     data = metafor_df3, model = 'CM.AL', mods = ~ dummy3, method = "ML", level = 95, nAGQ = 7, control = list(packages='GLMMadaptive'), verbose = TRUE)

predict(glmm_abn3)
coef(glmm_abn3)
vcov(glmm_abn3)
summary(glmm_abn3)



#Visualisasi Binomial-Normal Model dengan variabel moderator Ukuran sampel
####Forest Plot
forest(glmm_abn3, main = 'Forest Plot Ukuran Sampel', order=dummy2)
forest.rma(glmm_abn3, addpred = TRUE, xlim=c(-16,7), 
           ilab = cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5), at=seq(-5,10,by=1), 
           main = 'Forest Plot of Ukuran Sampel', header = "Penelitian", addfit = FALSE, order=dummy2, top=2)
text(c(-9.5,-8,-6,-4.5), glmm_abn3$k+2, c("KP+","KP-","KP+","KP-"), cex = 0.5)
text(c(-8.75,-5.25), glmm_abn3$k+3, c("Treatment","Control"), cex = 0.5)



#Kriteria Pemilihan model
###Fail-Safe N
fsn(glmm_abn3$yi, glmm_abn3$vi, glmm_abn3, type="General")


