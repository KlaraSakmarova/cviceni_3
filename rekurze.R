# cokoladova cesta 
Cokolada <- function(M,r,s){
 if (r == nrow(M)){
 return (M(r,s))
 }
 else {
   C <- M(r,s)
  }
 Cdolu <- Cokolada(M,r+1,s)
 Csikmo <- Cokolada(M,r+1,s+1)
 return (max(Cdolu, Csikmo) + C)
}
