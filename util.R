
# Funkcija za izracunavanje evaluacionih metrika

compute_eval_metrics <- function(cm){
  TP <- cm[2,2]
  TN <- cm[1,1]
  FP <- cm[1,2]
  FN <- cm[2,1]
  a <- sum(diag(cm))/sum(cm)
  p <- TP / (TP + FP)
  r <- TP / (TP + FN)
  f1 <- 2*p*r/(p+r)
  
  c(accuracy = a, precision = p, recall = r, F1 = f1)
}
