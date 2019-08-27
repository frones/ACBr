Function Main ()
   local sat
   local vendaRetorno, strIniVenda
   local hIni, hIniVenda
   LOCAL cSection
   
   sat := ACBrSat():New("", "")
   //                      Sessão       Chave    Valor
   // Configurações de log da Lib
   sat:ConfigGravarValor("Principal", "LogNivel", "4") // logParanoico     
   sat:ConfigGravarValor("Principal", "LogPath", hb_dirBase())

   //Configurações do Sat
   sat:ConfigGravarValor("SAT", "Modelo", "1") // satDinamico_cdecl
   sat:ConfigGravarValor("SAT", "NomeDLL", "C:\SAT\SAT.dll")
   sat:ConfigGravarValor("SAT", "CodigoDeAtivacao", "sefaz1234")
   sat:ConfigGravarValor("SAT", "SignAC", "111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111")
   
   // Softhouse   
   sat:ConfigGravarValor("SATConfig", "infCFe_versaoDadosEnt", "0.07")
   sat:ConfigGravarValor("SATConfig", "ide_CNPJ", "16716114000172")
   sat:ConfigGravarValor("SATConfig", "ide_numeroCaixa", "0")

   // Emitente
   sat:ConfigGravarValor("SATConfig", "emit_CNPJ", "14200166000166")
   sat:ConfigGravarValor("SATConfig", "emit_IE", "111111111111")
   sat:ConfigGravarValor("SATConfig", "emit_IM", "")
   sat:ConfigGravarValor("SATConfig", "emit_cRegTrib", "0") // RTSimplesNacional
   sat:ConfigGravarValor("SATConfig", "emit_cRegTribISSQN", "0") // RTISSMicroempresaMunicipal
   sat:ConfigGravarValor("SATConfig", "emit_indRatISSQN", "0") // irSim

   // Extrato
   sat:ConfigGravarValor("Extrato", "Tipo", "0") // teFortes
   sat:ConfigGravarValor("Extrato", "MostraPreview", "1") // True

   // Salvando configurações
   sat:ConfigGravar("")
  
   ?sat:Nome
   ?sat:Versao
   
   sat:Inicializar()

   ?sat:ConsultarStatusOperacional()

   // Gerando ini venda
   hIniVenda := Hash()
   hIniVenda["infCFe"] := Hash()
   hIniVenda["infCFe"]["versao"] := "0.07"

   hIniVenda["Destinatario"] := Hash()
   hIniVenda["Destinatario"]["CNPJCPF"] := "05481336000137"
   hIniVenda["Destinatario"]["xNome"] := "D.J. SYSTEM"

   hIniVenda["Entrega"] := Hash()
   hIniVenda["Entrega"]["xLgr"] := "Rua Cel. Aureliano de Camargo"
   hIniVenda["Entrega"]["nro"] := "973"
   hIniVenda["Entrega"]["xCpl"] := ""
   hIniVenda["Entrega"]["xBairro"] := "Centro"
   hIniVenda["Entrega"]["xMun"] := "Tatui"
   hIniVenda["Entrega"]["UF"] := "SP"

   hIniVenda["Produto001"] := Hash()
   hIniVenda["Produto001"]["cProd"] := "1189"
   hIniVenda["Produto001"]["infAdProd"] := "Teste de Produto"
   hIniVenda["Produto001"]["cEAN"] := ""
   hIniVenda["Produto001"]["xProd"] := "OVO VERMELHO"
   hIniVenda["Produto001"]["NCM"] := "04072100"
   hIniVenda["Produto001"]["CFOP"] := "5102"
   hIniVenda["Produto001"]["uCom"] := "DZ"
   hIniVenda["Produto001"]["Combustivel"] := "0"
   hIniVenda["Produto001"]["qCom"] := "510"
   hIniVenda["Produto001"]["vUnCom"] := "2,70"
   hIniVenda["Produto001"]["indRegra"] := "A"
   hIniVenda["Produto001"]["vDesc"] := "0"
   hIniVenda["Produto001"]["vOutro"] := "0"
   hIniVenda["Produto001"]["vItem12741"] := "137,00"

   hIniVenda["ObsFiscoDet001001"] := Hash()
   hIniVenda["ObsFiscoDet001001"]["xCampoDet"] := "Teste"
   hIniVenda["ObsFiscoDet001001"]["xTextoDet"] := "Texto Teste"

   hIniVenda["ICMS001"] := Hash()
   hIniVenda["ICMS001"]["Origem"] := "0"
   hIniVenda["ICMS001"]["CSOSN"] := "500"

   hIniVenda["PIS001"] := Hash()
   hIniVenda["PIS001"]["CST"] := "01"

   hIniVenda["COFINS001"] := Hash()
   hIniVenda["COFINS001"]["CST"] := "01"

   hIniVenda["Total"] := Hash()
   hIniVenda["Total"]["vCFeLei12741"] := "137,00"

   hIniVenda["DescAcrEntr"] := Hash()
   hIniVenda["DescAcrEntr"]["vDescSubtot"] := "7,00"

   hIniVenda["Pagto001"] := Hash()
   hIniVenda["Pagto001"]["cMP"] := "01"
   hIniVenda["Pagto001"]["vMP"] := "1400"

   hIniVenda["DadosAdicionais"] := Hash()
   hIniVenda["DadosAdicionais"]["infCpl"] := "Teste emissao CFe/SAT"

   hIniVenda["ObsFisco001"] := Hash()
   hIniVenda["ObsFisco001"]["xCampo"] := "ObsFisco 1"
   hIniVenda["ObsFisco001"]["xTexto"] := "Teste ObsFisco 1"

   strIniVenda := hb_iniWriteStr(hIniVenda)

   // Venda 
   ?"Ini Venda"
   ?strIniVenda
   ?""
   ?"-------------------------------------------------------------------------------------------------------------"
   ?"" 
   vendaRetorno := sat:CriarEnviarCFe(strIniVenda)
   ?vendaRetorno
   ?""
   ?"-------------------------------------------------------------------------------------------------------------"
   ?"" 
   hIni := hb_iniReadStr(vendaRetorno)
   cSection := hIni["ENVIO"]

   if cSection["CodigoDeRetorno"] != "6000"
      ?cSection["Resultado"]
   else
   ?"Impressão Preview"
   sat:ImprimirExtratoVenda(cSection["XML"], "")
   ?"Impressão PDF"
   ?sat:GerarPDFExtratoVenda(cSection["XML"], "")
   end if

   sat:DesInicializar()
   sat:Destroy()
   sat := nil
    
return NIL