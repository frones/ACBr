Function Main ()
   local nfe
   
   nfe := ACBrNFe():New()
   
   ?nfe:DistribuicaoDFePorUltNSU(33, "18760540000139", "0")
   
   ?nfe:Nome
   ?nfe:Versao

   nfe:Destroy()
    
return NIL