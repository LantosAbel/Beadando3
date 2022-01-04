#3. Hozz létre egy fgv-t, amelyik egy ".sav" fájlt vár bemenetként, kimenetként 
#pedig készít minden változóról egy leíró statisztikát, amit egy listába tesz bele. 
#Ez a lista legyen a kimenet. Ha folytonos a változó, akkor szórás, átlag, range, 
#median az eredmény, ha kategóriális, akkor pedig a gyakoriság legyen az eredmény.


fgv <- function (a) {
  if (grepl(".sav",a)){
    ess <- data.frame(read.spss(
                                a,
                                rownames= F,
                                Stringsasfactors= T,
                                tolower= F,
                                reencode= T)
    )
  }
  else print(("nem spss fájl"))

  leir= list(NULL)
  atlag= NULL
  sor= NULL
  medi= NULL
  rang= list(NULL)
  
  
  for (i in colnames(ess)){
    if (is.numeric(ess[,i])){
      atlag[i] <- mean(ess[,i],na.rm = T)
    } 
  }
  
  
  for (i in colnames(ess)){
    if (is.numeric(ess[,i])){
      sor[i] <- sd(ess[,i],na.rm = T)
    }
  }
  
  
  for (i in colnames(ess)){
    if (is.numeric(ess[,i])){
      medi[i] <- median(ess[,i],na.rm = T)
    }
  } 
  
 
  for (i in colnames(ess)){
    if (is.numeric(ess[,i])){
      rang[[i]] <- range(ess[,i],na.rm = T)
    }
  }
   
   
  for (i in colnames(ess)){
    if (!is.numeric(ess[,i])){
      leir[[i]] <- summary(ess[,i])
    }
  }

  lista <<- list(gyakoriság=leir,átlag=atlag,medián=medi,range=range,szórás=sor)
  
}

fgv("Szakdoga ISSP.sav")


rm(lista)


