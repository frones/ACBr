#!/bin/sh

# ------------------------------- Verificando a Existência de "lazres" --------------------------
echo "*** Verificando a Existência de 'lazres'"
LAZRES=$(which lazres)
if [ $? -gt 0 ] ; then
  echo "*******************************************************"
  echo "***  Utilitário 'lazres' não pode ser encontrado    ***"
  echo "***  Favor instalar corretamente o Lazarus          ***"
  echo "*******************************************************"
  exit 1
fi
echo "    OK"

:Comum
$LAZRES ACBrComum.lrs TACBrAAC.png
mv ACBrComum.lrs ..\ACBrComum
echo
echo "****************************************"
echo "* Arquivo   ACBrComum.lrs   gerado  *"
echo "****************************************"
echo

:OpenSSL
$LAZRES ACBrOpenSSL.lrs TACBrEAD.png
mv ACBrOpenSSL.lrs ..\ACBrOpenSSL
echo
echo "****************************************"
echo "* Arquivo   ACBrOpenSSL.lrs   gerado   *"
echo "****************************************"
echo

:Diversos
$LAZRES ACBrDiversos.lrs TACBrCalculadora.png TACBrExtenso.png TACBrValidador.png TACBrTroco.png TACBrCMC7.png TACBrFala.png TACBrGIF.png TACBrBarCode.png TACBrEnterTab.png TACBrCargaBal.png TACBrCotacao.png TACBrInStore.png
mv ACBrDiversos.lrs ..\ACBrDiversos
echo
echo "****************************************"
echo "* Arquivo   ACBrDiversos.lrs   gerado  *"
echo "****************************************"
echo

:Serial
$LAZRES ACBrSerial.lrs TACBrECF.png TACBrGAV.png TACBrCHQ.png TACBrLCB.png TACBrDIS.png TACBrBAL.png TACBrTER.png TACBrETQ.png TACBrRFD.png TACBrSMS.png TACBrPosPrinter.png TACBrECFVirtualNaoFiscal.png
mv ACBrSerial.lrs ..\ACBrSerial
echo
echo "****************************************"
echo "* Arquivo   ACBrSerial.lrs   gerado    *"
echo "****************************************"
echo

:TEFD
$LAZRES ACBrTEFD.lrs TACBrTEFD.png
mv ACBrTEFD.lrs ..\ACBrTEFD
echo
echo "****************************************"
echo "* Arquivo   ACBrTEFD.lrs   gerado      *"
echo "****************************************"
echo

:TCP
$LAZRES ACBrTCP.lrs TACBrTCPServer.png TACBrCEP.png TACBrIBGE.png TACBrNFPws.png TACBrCNIEE.png TACBrSuframa.png TACBrDownload.png TACBrIBPTax.png TACBrConsultaCNPJ.png TACBrMail.png TACBrConsultaCPF.png TACBrSpedTabelas.png TACBrSedex.png TACBrNCMs.png
mv ACBrTCP.lrs ..\ACBrTCP
echo
echo "****************************************"
echo "* Arquivo   ACBrTCP.lrs   gerado       *"
echo "****************************************"
echo

:MTER
$LAZRES ACBrMTER.lrs TACBrMTer.png
mv ACBrMTER.lrs ..\ACBrTCP
echo
echo "****************************************"
echo "* Arquivo   ACBrMTER.lrs   gerado       *"
echo "****************************************"
echo

:SPED
$LAZRES ACBrSpedFiscal.lrs TACBrSpedFiscal.png
$LAZRES ACBrSpedContabil.lrs TACBrSpedContabil.png
$LAZRES ACBrSpedPisCofins.lrs TACBrSpedPisCofins.png
$LAZRES ACBrSPEDPisCofinsImportar.lrs TACBrSpedPCImportar.png  
$LAZRES ACBrSpedECF.lrs TACBrSpedECF.png  
$LAZRES ACBrSpedFiscalImportar.lrs TACBrSpedFiscalImportar.png  

mv ACBrSpedFiscal.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDFiscal
mv ACBrSpedContabil.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDContabil
mv ACBrSpedPisCofins.lrs ..\ACBrTXT\ACBrSPED\ACBrSpedPisCofins
mv ACBrSPEDPisCofinsImportar.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDPisCofinsImportar
mv ACBrSpedECF.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDECF
mv ACBrSpedFiscalImportar.lrs ..\ACBrTXT\ACBrSPED\ACBrSpedFiscalImportar

echo
echo "************************************************"
echo "* Arquivo  ACBrSpedFiscal.lrs   gerado         *"
echo "* Arquivo  ACBrSpedContabil.lrs gerado         *"
echo "* Arquivo  ACBrSpedPisCofins.lrs gerado        *"
echo "* Arquivo  ACBrSPEDPisCofinsImportar.lrs gerado*"
echo "* Arquivo  ACBrSpedECF.lrs gerado              *"
echo "************************************************"
echo

:PAF
$LAZRES ACBrPAF.lrs TACBrPAF.png
mv ACBrPAF.lrs ..\ACBrTXT\ACBrPAF
echo
echo "****************************************"
echo "* Arquivo   ACBrPAF.lrs   gerado       *"
echo "****************************************"
echo

:Convenio115
$LAZRES ACBrConvenio115.lrs TACBrConvenio115.png
mv ACBrConvenio115.lrs ..\ACBrTXT\ACBrConvenio115
echo
echo "****************************************"
echo "* Arquivo   ACBrConvenio115.lrs   gerado       *"
echo "****************************************"
echo

:Boleto
$LAZRES ACBrBoleto.lrs TACBrBoleto.png
$LAZRES ACBrBoletoFCFortes.lrs TACBrBoletoFCFortes.png
$LAZRES ACBrBoletoFCLazReport.lrs TACBrBoletoFCLazReport.png ..\ACBrBoleto\FC\Laz\FCLazReport_Padrao.lrf ..\ACBrBoleto\FC\Laz\FCLazReport_CompEntrega.lrf ..\ACBrBoleto\FC\Laz\FCLazReport_Carne.lrf

mv ACBrBoleto.lrs ..\ACBrBoleto
mv ACBrBoletoFCFortes.lrs ..\ACBrBoleto\FC\Fortes
mv ACBrBoletoFCLazReport.lrs ..\ACBrBoleto\FC\Laz
echo
echo "****************************************"
echo "* Arquivo   ACBrBoleto.lrs   gerado    *"
echo "*    ACBrBoletoFCFortes.lrs  gerado    *"
echo "****************************************"
echo

:NFe
$LAZRES ACBrNFe.lrs TACBrNFe.png 
mv ACBrNFe.lrs ..\ACBrDFe\ACBrNFe

$LAZRES ACBrNFeDANFeRL.lrs TACBrNFeDANFeRL.png
mv ACBrNFeDANFeRL.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFe\Fortes

$LAZRES ACBrNFeDANFCeFortes.lrs TACBrNFeDANFCeFortes.png TACBrNFeDANFCeFortesA4.png
mv ACBrNFeDANFCeFortes.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFCe\Fortes

$LAZRES ACBrNFeDANFeESCPOS.lrs TACBrNFeDANFeESCPOS.png 
mv ACBrNFeDANFeESCPOS.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFCe\EscPos

$LAZRES ACBrECFVirtualNFCe.lrs TACBrECFVirtualNFCe.png
mv ACBrECFVirtualNFCe.lrs ..\ACBrDFe\ACBrNFe\ACBrECFVirtualNFCe


echo
echo "**************************************************************"
echo "* Arquivos    ACBrNFe.lrs, ACBrNFeDANFeRL.lrs     gerados    *"
echo "**************************************************************"
echo

:BlocoX
$LAZRES ACBrBlocoX.lrs TACBrBlocoX.png
mv ACBrBlocoX.lrs ..\ACBrDFe\ACBrBlocoX
echo
echo "****************************************"
echo "* Arquivo   ACBrBlocoX.lrs   gerado    *"
echo "****************************************"
echo

:GNRE
$LAZRES ACBrGNRE.lrs TACBrGNRE.png 
mv ACBrGNRE.lrs ..\ACBrDFe\ACBrGNRE

$LAZRES ACBrGNREGuiaFR.lrs TACBrGNREGuiaFR.png 
mv ACBrGNREGuiaFR.lrs ..\ACBrDFe\ACBrGNRE\GNRE\Fortes

echo
echo "***********************************************"
echo "* Arquivo    ACBrGNRE.lrs     gerado          *"
echo "* Arquivo    ACBrGNREGuiaFR.lrs     gerado    *"
echo "***********************************************"
echo

:NFSe
$LAZRES ACBrNFSe.lrs TACBrNFSe.png 
mv ACBrNFSe.lrs ..\ACBrDFe\ACBrNFSe

echo
echo "****************************************"
echo "* Arquivo    ACBrNFSe.lrs     gerado    *"
echo "****************************************"
echo

:CTe
$LAZRES ACBrCTe.lrs TACBrCTe.png 
mv ACBrCTe.lrs ..\ACBrDFe\ACBrCTe

$LAZRES ACBrCTeDACTeRL.lrs TACBrCTeDACTeRL.png 
mv ACBrCTeDACTeRL.lrs ..\ACBrDFe\ACBrCTe\DACTE\Fortes


echo
echo "***********************************************"
echo "* Arquivo    ACBrCTe.lrs     gerado           *"
echo "* Arquivo    ACBrCTeDACTeRL.lrs     gerado    *"
echo "***********************************************"
echo

:MDFe
$LAZRES ACBrMDFe.lrs TACBrMDFe.png
mv ACBrMDFe.lrs ..\ACBrDFe\ACBrMDFe

$LAZRES ACBrMDFeDAMDFeRL.lrs TACBrMDFeDAMDFeRL.png
mv ACBrMDFeDAMDFeRL.lrs ..\ACBrDFe\ACBrMDFe\DAMDFE\Fortes

echo
echo "****************************************"
echo "* Arquivo    ACBrMDFe.lrs     gerado    *"
echo "****************************************"
echo

:LFD
$LAZRES ACBrLFD.lrs TACBrLFD.png 
mv ACBrLFD.lrs ..\ACBrTXT\ACBrLFD

echo
echo "****************************************"
echo "* Arquivo    ACBrLFD.lrs     gerado    *"
echo "****************************************"
echo

:Sintegra
$LAZRES ACBrSintegra.lrs TACBrSintegra.png
mv ACBrSintegra.lrs ..\ACBrTXT\ACBrSintegra
echo
echo "****************************************"
echo "* Arquivo   ACBrSintegra.lrs   gerado  *"
echo "****************************************"
echo

:Ponto
$LAZRES ACBrPonto.lrs TACBrPonto.png
mv ACBrPonto.lrs ..\ACBrTXT\ACBrPonto
echo
echo "*************************************"
echo "* Arquivo   ACBrPonto.lrs   gerado  *"
echo "*************************************"
echo

:DeSTDA
$LAZRES ACBrDeSTDA.lrs TACBrDeSTDA.png
mv ACBrDeSTDA.lrs ..\ACBrTXT\ACBrDeSTDA
echo
echo "****************************************"
echo "* Arquivo   ACBrDeSTDA.lrs   gerado  *"
echo "****************************************"
echo

:SAT
$LAZRES ACBrSAT.lrs TACBrSAT.png
mv ACBrSAT.lrs ..\ACBrSAT
                  
$LAZRES ACBrSATExtratoESCPOS.lrs TACBrSATExtratoESCPOS.png
mv ACBrSATExtratoESCPOS.lrs ..\ACBrSAT\Extrato\EscPos

$LAZRES ACBrSATExtratoFortes.lrs TACBrSATExtratoFortes.png
mv ACBrSATExtratoFortes.lrs ..\ACBrSAT\Extrato\Fortes

$LAZRES ACBrECFVirtualSAT.lrs TACBrECFVirtualSAT.png
mv ACBrECFVirtualSAT.lrs ..\ACBrSAT\ACBrECFVirtualSAT

echo
echo "********************************************************************"
echo "*   Arquivos   ACBrSAT.lrs, ACBrSATExtratoFortes.lrs   gerados     *"
echo "********************************************************************"
echo

:Integrador
$LAZRES ACBrIntegrador.lrs TACBrIntegrador.png
mv ACBrIntegrador.lrs ..\ACBrIntegrador

echo
echo "********************************************************************"
echo "*   Arquivos   ACBrIntegrador.lrs  gerado     			 *"
echo "********************************************************************"
echo

:SEF2
$LAZRES ACBrSEF2.lrs TACBrSEF2.png
mv ACBrSEF2.lrs ..\ACBrTXT\ACBrSEF2
echo
echo "****************************************"
echo "*   Arquivo   ACBrSEF2.lrs   gerado     *"
echo "****************************************"
echo
