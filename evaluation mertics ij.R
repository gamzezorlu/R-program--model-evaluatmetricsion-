confusion_matrix<- cm_dt[["table"]]
sinif_sayisi = nrow(confusion_matrix)

TP <- numeric(sinif_sayisi)
TN <- numeric(sinif_sayisi)
FP <- numeric(sinif_sayisi)
FN <- numeric(sinif_sayisi)

precision<-numeric(sinif_sayisi)
specificity<-numeric(sinif_sayisi)    
recall<-numeric(sinif_sayisi             )   ##sensitivity
F1_score<-numeric(sinif_sayisi)

for (sinif in 1:sinif_sayisi) {
# TP (True Positives): Do??ru tahmin edilen s??n??f say??s??
TP[sinif] <- confusion_matrix[sinif, sinif]

# TN (True Negatives): Di??er s??n??flar?? do??ru tahmin edilen toplam say??
TN[sinif] <- sum(confusion_matrix) - (sum(confusion_matrix[sinif, ]) + sum(confusion_matrix[, sinif]) - sum(confusion_matrix[sinif, sinif]))
                                    
# FP (False Positives): Bu s??n??f?? yanl???? tahmin edilen toplam say??
FN[sinif] <- sum(confusion_matrix[, sinif]) - sum(confusion_matrix[sinif, sinif])
# FN (False Negatives): Bu s??n??f??n ger??ek toplam?? ve TP ????kart??larak hesaplan??r
FP[sinif] <- sum(confusion_matrix[sinif, ]) - sum(confusion_matrix[sinif, sinif])
 
precision[sinif]<- TP[sinif]/(TP[sinif]+FP[sinif])
specificity[sinif]<- TN[sinif]/(TN[sinif]+FP[sinif])
recall[sinif]<- TP[sinif]/(TP[sinif]+FN[sinif])
F1_score[sinif]<- 2*((precision[sinif]*recall[sinif])/(precision[sinif]+recall[sinif]))
}

precision[1]   ##Pos Pred Value
c     specificity[1]
recall[1]   ##sensitivity
F1_score[1]
cm_dt[["byClass"]]

    