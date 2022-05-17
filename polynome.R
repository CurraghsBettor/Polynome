rm(list = ls())

polynome <- function (a, b, c) {
  cat("l'equation est: y =", a,"x^2 +", b, "x +",c, "\n")
  delta <- (b^2) - (4*a*c)
  coor1 <- (-b)/(2*a)
  coor2 <- (a)*coor1^2+(b)*coor1+(c)
  if (delta < 0) {
    cat("Le polynome n'a pas de racine et par consequent ne change pas de signe", "\n", "Delta =", delta, "\n")
    if ( a < 0) {
      cat("Le signe du polynome est negatif")
      cat("La parabole est croissante sur ]", bquote("- \U221E"), ";", coor1, "] et decroissante sur [", coor1, ";", bquote("\U221E"), "[", "\n")
      cat("Le polynome admet un maximum M", "\n", "Cordonnees du point M: (", coor1, ";", coor2, ")", "\n")
    } else {
      cat("Le signe du polynome est positif")
      cat("La parabole est decroissante sur ]", bquote("- \U221E"), ";", coor1, "] et croissante sur [", coor1, ";", bquote("\U221E"), "[", "\n")
      cat("Le polynome admet un minimum m", "\n", "Cordonnees du point m: (", coor1, ";", coor2, ")", "\n")
    }
  } else if ( delta > 0) {
    racine1 <- (-b - sqrt(delta))/(2*a)
    racine2 <- (-b + sqrt(delta))/(2*a)
    cat("Le polynome a deux racines", "\n", "Delta =", delta, "\n", "x1 =", racine1, "\n", "x2 =", racine2, "\n")
    if (a < 0) {
      cat("Le signe du polynome est negatif sur ]", bquote("- \U221E"), ";", racine2, "]", "\n", "puis postif sur [", racine2, ";", racine1, "]", "\n", "puis negatif sur [", racine1, ";", bquote("\U221E"), "[", "\n")
      cat("La parabole est croissante sur ]", bquote("- \U221E"), ";", coor1, "] et decroissante sur [", coor1, ";", bquote("\U221E"), "[", "\n")
      cat("Le polynome admet un maximum M", "\n", "Cordonnees du point M: (", coor1, ";", coor2, ")","\n")
    } else {
      cat("Le signe du polynome est positif sur ]", bquote("- \U221E"), ";", racine1, "]", "\n", "puis negatif sur [", racine1, ";", racine2, "]", "\n", "puis positif sur [", racine2, ";", bquote("\U221E"), "[", "\n")
      cat("La parabole est decroissante sur ]", bquote("- \U221E"), ";", coor1, "] et croissante sur [", coor1, ";", bquote("\U221E"), "[", "\n")
      cat("Le polynome admet un minimum m", "\n", "Cordonnees du point m: (", coor1, ";", coor2, ")", "\n")
    } 
  } else {
    racine0 <- (-b)/(2*a)
    cat("Le polynome a une racine", "\n","Delta =", delta, "\n", "x0 =", racine0, "\n")
    if (a < 0) {
      cat("Le signe du polynome est negatif sur ]", bquote("- \U221E"), ";", racine2, "]", "\n", "puis postif sur [", racine2, ";", racine1, "]", "\n", "puis negatif sur [", racine1, ";", bquote("\U221E"), "[", "\n")
      cat("La parabole est croissante sur ]", bquote("- \U221E"), ";", coor1, "] et decroissante sur [", coor1, ";", bquote("\U221E"), "[", "\n")
      cat("Le polynome admet un maximum M", "\n", "Cordonnees du point M: (", coor1, ";", coor2, ")", "\n")
    } else {
      cat("Le signe du polynome est positif sur ]", bquote("- \U221E"), ";", racine1, "]", "\n", "puis negatif sur [", racine1, ";", racine2, "]", "\n", "puis positif sur [", racine2, ";", bquote("\U221E"), "[", "\n")
      cat("La parabole est decroissante sur ]", bquote("- \U221E"), ";", coor1, "] et croissante sur [", coor1, ";", bquote("\U221E"), "[", "\n")
      cat("Le polynome admet un minimum m", "\n", "Cordonnees du point m: (", coor1, ";", coor2, ")", "\n")
    }
  }
}

## exemple
polynome(-1, (-3), 2)
