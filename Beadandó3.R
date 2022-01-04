#3. Hozz létre egy fgv-t, amelyik egy ".sav" fájlt vár bemenetként, kimenetként 
#pedig készít minden változóról egy leíró statisztikát, amit egy listába tesz bele. 
#Ez a lista legyen a kimenet. Ha folytonos a változó, akkor szórás, átlag, range, 
#median az eredmény, ha kategóriális, akkor pedig a gyakoriság legyen az eredmény.


#Előfordultak rejtélyes hibák. Mondjuk az egyik próba adatbázison következetesen félreszámolta az egyes oszlopok átlagát meg mindenét 
#(el nem tudom képzelni hogyan történhetett ez, hiszen ehhez valamit a sorokkal kellett volna félreszámolnia, 
#de a függvény nem is nyúl a sorokhoz). Ennél már csak az volt a rejtélyesebb, hogy egy másik adatbázis tesztelésekor ez a hiba elmúlt, 
#és utána már az eredeti adatbázist is jól elemezte. Szóval ha ez megint előfordulna, csak újra kell indítani a programot, és törölni 
#mindent a global environmentből.
#Ennél aggasztóbb, hogy a range meg egyáltalán nem akar működni. A listának szánt objektuma teljesen üres, valuenak meg azt írja, hogy
function (..., na.rm = FALSE)  .Primitive("range") typera meg szintén azt, hogy function(primitive)
#Feltételezem az is.numeric-el nem tetszik neki valami, mert anélkül meg működött; ugyanakkor az is.numeric meg kell, mert az méri, hogy
#folyamatos-e a változó. Ebben segíthetne a tanár úr, hogy ez mi, a google nem volt túl hasznos.

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


