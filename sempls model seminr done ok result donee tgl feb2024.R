library(seminr)

#membentuk measurement model
model1_mm <- constructs(
  composite("E_CT", multi_items("ECT", c(".01",".02",".03"))),
  composite("E_CX", multi_items("ECX", c(".01",".02",".03"))),
  composite("E_SQ", multi_items("ESQ", c(".01",".02",".03"))),
  composite("E_T", multi_items("ET", c(".01",".02"))),
  composite("E_S", multi_items("ES", c(".01",".02")))
)

model_mm <- as.reflective(model1_mm) 

#membentuk struktural model yang menunjukkan urutan konstruksi
#dan hubungan di antara mereka 
model_sm <- relationships(
  paths(from = c("E_CT"), to = c("E_CX", "E_T", "E_S")),
  paths(from = c("E_SQ"), to = c("E_CX", "E_T", "E_S")),
  paths(from = c("E_CX"), to = c("E_T")),
  paths(from = c("E_T"), to = c("E_S")))

#membentuk PLS path model
model_pls <- estimate_pls(data = datasempls, 
                          measurement_model = model_mm, structural_model = model_sm)

#memunculkan summary dari model
model_summary <- summary(model_pls)
model_summary

#mengestimasi bootstrap model untuk memperkirakan standar error dan
#menghitung confidence interval
boot_model <- bootstrap_model(
  seminr_model = model_pls, nboot = 1000, cores = 2, NULL = 123)

#memunculkan summary dari bootstrap model
sum_boot <- summary(boot_model)
sum_boot

#Evaluasi model measurement
#1. Reliabilitas indikator
#Untuk model pengukuran reflektif, kita perlu memperkirakan hubungan antara 
#konstruk yang diukur secara reflektif dan indikatornya.
#Indikator yang rendah dapat mengakibatkan hasil konstruk menjadi bias.
#Indikator direkomendasikan mempunyai angka lebih 0,708
#Indikator dgn nilai antara 0,40 dan 0,70 harus dipertimbangkan untuk dihilangkan
#Indikator dgn nilai yg sangat rendah (dibawah 0,40) harus dihilangkan.
model_summary$loadings

#2. Reliabilitas Konsistensi Internal
#Reliabilitas konsistensi internal adalah sejauh mana indikator-indikator 
#yang mengukur konstruk yang sama dikaitkan satu sama lain.
#Suatu item dapat diterima untuk dimasukkan dalam model jika reliabilitas 
#konsistensi internalnya mempunyai nilai tertentu:
#nilai yang direkomendasikan 0,80 sampai 0,90
#nilai minimum 0,70 (atau 0,60 dalam penelitian eksplorasi)
#Nilai maksimum 0,95 untuk menghindari redundansi indikator yang dapat 
#membahayakan validitas konten
model_summary$reliability

plot(model_summary$reliability)

#3. Validitas konvergen
#Validitas konvergen adalah sejauh mana konstruk menyatu untuk menjelaskan
#varians indikatornya.
#Rata-rata varians yang diekstraksi (AVE) adalah rata-rata dari kuadrat
#pembebanan suatu indikator konstruk.
#AVE minimum yang dapat diterima adalah 0,50 atau lebih tinggi.
model_summary$reliability

#4. Validitas diskriminan
#HTMT adalah nilai rata-rata korelasi indikator antar konstruk relatif 
#terhadap rata-rata korelasi rata-rata untuk indikator yang mengukur 
#konstruk yang sama (yaitu korelasi monotrait-heterometode).
#Masalah validitas diskriminan muncul ketika nilai HTMT
#melebihi 0,90 untuk konstruksi yang secara konseptual sangat mirip.
#melebihi 0,85 untuk konstruksi yang secara konseptual lebih berbeda.
model_summary$validity$htmt

#5. Kolinieritas indikator
#Nilai VIF 5 atau lebih menunjukkan masalah kolinearitas.
model_summary$validity$vif_items

#6. Signifikansi dan relevansi bobot indikator
#Indikator dengan weight yang signifikan dapat dipertahankan.
#Jika weight suatu indikator tidak signifikan, maka indikator tersebut
#sebaiknya dipertimbangkan untuk dikeluarkan dari measurement model.
#Namun, indikator tersebut juga dapat dipertahankan jika, minimal, 
#loading suatu indikator signifikan.
sum_boot1 <- summary(boot_model, alpha = 0.05)
sum_boot1$bootstrapped_weights

sum_boot1$bootstrapped_loadings

#Evaluasi model struktural
#1. Masalah kolinearitas
#Nilai VIF di atas 5 dapat mengindikasikan masalah kolinearitas.
model_summary$vif_antecedents


#2. Signifikansi dan relevansi hubungan model struktural
sum_boot1$bootstrapped_paths

#3. Kekuatan dari eksplanatori
#Untuk mempertimbangkan kekuatan penjelas model, perlu menganalisis 
#R2 konstruk endogen dan ukuran efek f2 dari konstruk prediktor.
#Koefisien determinasi R2, nilai R2 0.75, 0.50, atau 0.25 berturut-turut 
#artinya proporsi indikator sudah substantial(bagus), moderate(sedang),
#atau weak(kurang) persentasenya dalam mengukur variabel endogennya.
model_summary$paths

#f2, mengindikasikan besarnya efek variabel independent pada variabel 
#dependent dengan kategori kisaran nilai 0.02, 0.15, dan 0.35
#menunjukkan efek small, medium, atau large.
model_summary$fSquare

thm <- seminr_theme_create(plot.rounding = 2, plot.adj = FALSE, 
                           sm.node.fill = "skyblue",
                           mm.node.fill = "yellow")
seminr_theme_set(thm)

plot(model_pls, title = "Model SEM PLS")

plot(boot_model, title = "Bootstrapped Model")
