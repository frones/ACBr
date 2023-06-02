@ECHO OFF

if "%1" == "" goto param_default

if NOT EXIST %1\lazarus.exe goto erro_param

if NOT EXIST %1\tools\lazres.exe GOTO erro_lazres

:Comum
%1\tools\lazres.exe ACBrComum.lrs TACBrAAC.png
move ACBrComum.lrs ..\ACBrComum
echo.
echo "****************************************"
echo "* Arquivo   ACBrComum.lrs   gerado  *"
echo "****************************************"
echo.

:OpenSSL
%1\tools\lazres.exe ACBrOpenSSL.lrs TACBrEAD.png TACBrOpenSSLUtils.png
move ACBrOpenSSL.lrs ..\ACBrOpenSSL
echo.
echo "****************************************"
echo "* Arquivo   ACBrOpenSSL.lrs   gerado   *"
echo "****************************************"
echo.

:Diversos
%1\tools\lazres.exe ACBrDiversos.lrs TACBrCalculadora.png TACBrExtenso.png TACBrValidador.png TACBrTroco.png TACBrCMC7.png TACBrFala.png TACBrGIF.png TACBrBarCode.png TACBrEnterTab.png TACBrCargaBal.png TACBrCotacao.png TACBrInStore.png
move ACBrDiversos.lrs ..\ACBrDiversos
echo.
echo "****************************************"
echo "* Arquivo   ACBrDiversos.lrs   gerado  *"
echo "****************************************"
echo.

:Serial
%1\tools\lazres.exe ACBrSerial.lrs TACBrECF.png TACBrGAV.png TACBrCHQ.png TACBrLCB.png TACBrDIS.png TACBrBAL.png TACBrTER.png TACBrETQ.png TACBrRFD.png TACBrSMS.png TACBrPosPrinter.png TACBrECFVirtualNaoFiscal.png
move ACBrSerial.lrs ..\ACBrSerial
echo.
echo "****************************************"
echo "* Arquivo   ACBrSerial.lrs   gerado    *"
echo "****************************************"
echo.

:TEFD
%1\tools\lazres.exe ACBrTEFD.lrs TACBrTEFD.png TACBrPicPay.png TACBrPOS.png TACBrTEFAPI.png
move ACBrTEFD.lrs ..\ACBrTEFD
echo.
echo "****************************************"
echo "* Arquivo   ACBrTEFD.lrs   gerado      *"
echo "****************************************"
echo.

:PIXCD
%1\tools\lazres.exe ACBrPIXCD.lrs TACBrPixCD.png TACBrPSPItau.png TACBrPSPBancoDoBrasil.png TACBrPSPSantander.png TACBrPSPShipay.png TACBrPSPSicredi.png TACBrPSPSicoob.png TACBrPSPPagSeguro.png TACBrPSPGerenciaNet.png TACBrPSPBradesco.png TACBrPSPPixPDV.png TACBrPSPInter.png TACBrPSPAilos.png
move ACBrPIXCD.lrs ..\ACBrPIXCD
echo.
echo "****************************************"
echo "* Arquivo   ACBrPIXCD.lrs   gerado     *"
echo "****************************************"
echo.

:OpenDelivery
%1\tools\lazres.exe ACBrOpenDelivery.lrs TACBrOpenDelivery.png
move ACBrOpenDelivery.lrs ..\ACBrOpenDelivery
echo.
echo "***********************************************"
echo "* Arquivo   ACBrOpenDelivery.lrs   gerado     *"
echo "***********************************************"
echo.

:TCP
%1\tools\lazres.exe ACBrTCP.lrs TACBrTCPServer.png TACBrCEP.png TACBrIBGE.png TACBrNFPws.png TACBrCNIEE.png TACBrSuframa.png TACBrDownload.png TACBrIBPTax.png TACBrConsultaCNPJ.png TACBrMail.png TACBrConsultaCPF.png TACBrSpedTabelas.png TACBrSedex.png TACBrNCMs.png
move ACBrTCP.lrs ..\ACBrTCP
echo.
echo "****************************************"
echo "* Arquivo   ACBrTCP.lrs   gerado       *"
echo "****************************************"
echo.

:MTER
%1\tools\lazres.exe ACBrMTER.lrs TACBrMTer.png
move ACBrMTER.lrs ..\ACBrTCP
echo.
echo "****************************************"
echo "* Arquivo   ACBrMTER.lrs   gerado       *"
echo "****************************************"
echo.

:SPED
%1\tools\lazres.exe ACBrSpedFiscal.lrs TACBrSpedFiscal.png
%1\tools\lazres.exe ACBrSpedContabil.lrs TACBrSpedContabil.png
%1\tools\lazres.exe ACBrSpedPisCofins.lrs TACBrSpedPisCofins.png
%1\tools\lazres.exe ACBrSPEDPisCofinsImportar.lrs TACBrSpedPCImportar.png  
%1\tools\lazres.exe ACBrSpedECF.lrs TACBrSpedECF.png  
%1\tools\lazres.exe ACBrSpedFiscalImportar.lrs TACBrSpedFiscalImportar.png  

move ACBrSpedFiscal.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDFiscal
move ACBrSpedContabil.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDContabil
move ACBrSpedPisCofins.lrs ..\ACBrTXT\ACBrSPED\ACBrSpedPisCofins
move ACBrSPEDPisCofinsImportar.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDPisCofinsImportar
move ACBrSpedECF.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDECF
move ACBrSpedFiscalImportar.lrs ..\ACBrTXT\ACBrSPED\ACBrSpedFiscalImportar

echo.
echo "************************************************"
echo "* Arquivo  ACBrSpedFiscal.lrs   gerado         *"
echo "* Arquivo  ACBrSpedContabil.lrs gerado         *"
echo "* Arquivo  ACBrSpedPisCofins.lrs gerado        *"
echo "* Arquivo  ACBrSPEDPisCofinsImportar.lrs gerado*"
echo "* Arquivo  ACBrSpedECF.lrs gerado              *"
echo "************************************************"
echo.

:PAF
%1\tools\lazres.exe ACBrPAF.lrs TACBrPAF.png
move ACBrPAF.lrs ..\ACBrTXT\ACBrPAF
echo.
echo "****************************************"
echo "* Arquivo   ACBrPAF.lrs   gerado       *"
echo "****************************************"
echo.

:Convenio115
%1\tools\lazres.exe ACBrConvenio115.lrs TACBrConvenio115.png
move ACBrConvenio115.lrs ..\ACBrTXT\ACBrConvenio115
echo.
echo "****************************************"
echo "* Arquivo   ACBrConvenio115.lrs   gerado       *"
echo "****************************************"
echo.

:Boleto
%1\tools\lazres.exe ACBrBoleto.lrs TACBrBoleto.png
%1\tools\lazres.exe ACBrBoletoFCFortes.lrs TACBrBoletoFCFortes.png
%1\tools\lazres.exe ACBrBoletoFCLazReport.lrs TACBrBoletoFCLazReport.png ..\ACBrBoleto\FC\Laz\FCLazReport_Padrao.lrf ..\ACBrBoleto\FC\Laz\FCLazReport_CompEntrega.lrf ..\ACBrBoleto\FC\Laz\FCLazReport_Carne.lrf

move ACBrBoleto.lrs ..\ACBrBoleto
move ACBrBoletoFCFortes.lrs ..\ACBrBoleto\FC\Fortes
move ACBrBoletoFCLazReport.lrs ..\ACBrBoleto\FC\Laz
echo.
echo "****************************************"
echo "* Arquivo   ACBrBoleto.lrs   gerado    *"
echo "*    ACBrBoletoFCFortes.lrs  gerado    *"
echo "****************************************"
echo.

:NFe
%1\tools\lazres.exe ACBrNFe.lrs TACBrNFe.png 
move ACBrNFe.lrs ..\ACBrDFe\ACBrNFe

%1\tools\lazres.exe ACBrNFeDANFeRLRes.lrs TACBrNFeDANFeRL.png
move ACBrNFeDANFeRLRes.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFe\Fortes

%1\tools\lazres.exe ACBrNFeDANFeLazReport.lrs TACBrNFeDANFeLazReport.png
move ACBrNFeDANFeLazReport.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFe\LazReport

%1\tools\lazres.exe ACBrNFeDANFCeFortes.lrs TACBrNFeDANFCeFortes.png TACBrNFeDANFCeFortesA4.png
move ACBrNFeDANFCeFortes.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFCe\Fortes

%1\tools\lazres.exe ACBrNFeDANFeESCPOS.lrs TACBrNFeDANFeESCPOS.png 
move ACBrNFeDANFeESCPOS.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFCe\EscPos

%1\tools\lazres.exe ACBrECFVirtualNFCe.lrs TACBrECFVirtualNFCe.png
move ACBrECFVirtualNFCe.lrs ..\ACBrDFe\ACBrNFe\ACBrECFVirtualNFCe


echo.
echo "**************************************************************"
echo "* Arquivos    ACBrNFe.lrs, ACBrNFeDANFeRL.lrs     gerados    *"
echo "**************************************************************"
echo.

:BlocoX
%1\tools\lazres.exe ACBrBlocoX.lrs TACBrBlocoX.png
move ACBrBlocoX.lrs ..\ACBrDFe\ACBrBlocoX
echo.
echo "****************************************"
echo "* Arquivo   ACBrBlocoX.lrs   gerado    *"
echo "****************************************"
echo.

:GNRE
%1\tools\lazres.exe ACBrGNRE.lrs TACBrGNRE.png 
move ACBrGNRE.lrs ..\ACBrDFe\ACBrGNRE

%1\tools\lazres.exe ACBrGNREGuiaFR.lrs TACBrGNREGuiaFR.png 
move ACBrGNREGuiaFR.lrs ..\ACBrDFe\ACBrGNRE\GNRE\Fortes

echo.
echo "***********************************************"
echo "* Arquivo    ACBrGNRE.lrs     gerado          *"
echo "* Arquivo    ACBrGNREGuiaFR.lrs     gerado    *"
echo "***********************************************"
echo.

:NFSe
%1\tools\lazres.exe ACBrNFSe.lrs TACBrNFSe.png 
move ACBrNFSe.lrs ..\ACBrDFe\ACBrNFSe

echo.
echo "****************************************"
echo "* Arquivo    ACBrNFSe.lrs     gerado    *"
echo "****************************************"
echo.

%1\tools\lazres.exe ACBrNFSeX.lrs TACBrNFSeX.png 
move ACBrNFSeX.lrs ../ACBrDFe/ACBrNFSeX

%1\tools\lazres.exe ACBrNFSeXDANFSeFR.lrs TACBrNFSeXDANFSeFR.png
move ACBrNFSeXDANFSeFR.lrs ../ACBrDFe/ACBrNFSeX/DANFSE/Fast

%1\tools\lazres.exe ACBrNFSeXDANFSeRL.lrs TACBrNFSeXDANFSeRL.png
move ACBrNFSeXDANFSeRL.lrs ../ACBrDFe/ACBrNFSeX/DANFSE/Fortes

echo.
echo "****************************************"
echo "* Arquivo    ACBrNFSeX.lrs     gerado    *"
echo "* Arquivo    ACBrNFSeXDANFSeFR.lrs     gerado    *"
echo "* Arquivo    ACBrNFSeXDANFSeRL.lrs     gerado    *"
echo "****************************************"
echo.

:PagFor
%1\tools\lazres.exe ACBrPagFor.lrs TACBrPagFor.png 
move ACBrPagFor.lrs ..\ACBrPagFor\

echo.
echo "****************************************"
echo "* Arquivo    ACBrPagFor.lrs     gerado    *"
echo "****************************************"
echo.

:CTe
%1\tools\lazres.exe ACBrCTe.lrs TACBrCTe.png 
move ACBrCTe.lrs ..\ACBrDFe\ACBrCTe

%1\tools\lazres.exe ACBrCTeDACTeRL.lrs TACBrCTeDACTeRL.png 
move ACBrCTeDACTeRL.lrs ..\ACBrDFe\ACBrCTe\DACTE\Fortes

echo.
echo "***********************************************"
echo "* Arquivo    ACBrCTe.lrs     gerado           *"
echo "* Arquivo    ACBrCTeDACTeRL.lrs     gerado    *"
echo "***********************************************"
echo.

:GTIN
%1\tools\lazres.exe ACBrGTIN.lrs TACBrGTIN.png 
move ACBrGTIN.lrs ..\ACBrDFe\ACBrGTIN

echo.
echo "***********************************************"
echo "* Arquivo    ACBrGTIN.lrs     gerado          *"
echo "***********************************************"
echo.

:MDFe
%1\tools\lazres.exe ACBrMDFe.lrs TACBrMDFe.png
move ACBrMDFe.lrs ..\ACBrDFe\ACBrMDFe

%1\tools\lazres.exe ACBrMDFeDAMDFeRL.lrs TACBrMDFeDAMDFeRL.png
move ACBrMDFeDAMDFeRL.lrs ..\ACBrDFe\ACBrMDFe\DAMDFE\Fortes

echo.
echo "****************************************"
echo "* Arquivo    ACBrMDFe.lrs     gerado    *"
echo "****************************************"
echo.

:eSocial
%1\tools\lazres.exe ACBreSocial.lrs TACBreSocial.png 
move ACBreSocial.lrs ..\ACBrDFe\ACBreSocial

echo.
echo "****************************************"
echo "* Arquivo    ACBreSocial.lrs     gerado    *"
echo "****************************************"
echo.

:LFD
%1\tools\lazres.exe ACBrLFD.lrs TACBrLFD.png 
move ACBrLFD.lrs ..\ACBrTXT\ACBrLFD

echo.
echo "****************************************"
echo "* Arquivo    ACBrLFD.lrs     gerado    *"
echo "****************************************"
echo.

:Sintegra
%1\tools\lazres.exe ACBrSintegra.lrs TACBrSintegra.png
move ACBrSintegra.lrs ..\ACBrTXT\ACBrSintegra
echo.
echo "****************************************"
echo "* Arquivo   ACBrSintegra.lrs   gerado  *"
echo "****************************************"
echo.

:Ponto
%1\tools\lazres.exe ACBrPonto.lrs TACBrPonto.png
move ACBrPonto.lrs ..\ACBrTXT\ACBrPonto
echo.
echo "*************************************"
echo "* Arquivo   ACBrPonto.lrs   gerado  *"
echo "*************************************"
echo.

:DeSTDA
%1\tools\lazres.exe ACBrDeSTDA.lrs TACBrDeSTDA.png
move ACBrDeSTDA.lrs ..\ACBrTXT\ACBrDeSTDA
echo.
echo "****************************************"
echo "* Arquivo   ACBrDeSTDA.lrs   gerado  *"
echo "****************************************"
echo.

:SAT
%1\tools\lazres.exe ACBrSAT.lrs TACBrSAT.png
move ACBrSAT.lrs ..\ACBrSAT
                  
%1\tools\lazres.exe ACBrSATExtratoESCPOS.lrs TACBrSATExtratoESCPOS.png
move ACBrSATExtratoESCPOS.lrs ..\ACBrSAT\Extrato\EscPos

%1\tools\lazres.exe ACBrSATExtratoFortes.lrs TACBrSATExtratoFortes.png
move ACBrSATExtratoFortes.lrs ..\ACBrSAT\Extrato\Fortes

%1\tools\lazres.exe ACBrECFVirtualSAT.lrs TACBrECFVirtualSAT.png
move ACBrECFVirtualSAT.lrs ..\ACBrSAT\ACBrECFVirtualSAT

echo.
echo "********************************************************************"
echo "*   Arquivos   ACBrSAT.lrs, ACBrSATExtratoFortes.lrs   gerados     *"
echo "********************************************************************"
echo.

:Integrador
%1\tools\lazres.exe ACBrIntegrador.lrs TACBrIntegrador.png
move ACBrIntegrador.lrs ..\ACBrIntegrador

echo.
echo "********************************************************************"
echo "*   Arquivos   ACBrIntegrador.lrs  gerado     			 *"
echo "********************************************************************"
echo.

:SEF2
%1\tools\lazres.exe ACBrSEF2.lrs TACBrSEF2.png
move ACBrSEF2.lrs ..\ACBrTXT\ACBrSEF2
echo.
echo "****************************************"
echo "*   Arquivo   ACBrSEF2.lrs   gerado     *"
echo "****************************************"
echo.

GOTO fim

:param_default
Compila_LRS.bat c:\lazarus
goto :fim

:erro_param
ECHO "Informe o diretorio do Lazarus. Exemplo:"
echo.
ECHO "Compila_LRS.bat c:\lazarus"
goto :fim

:erro_lazres
ECHO "Arquivo:"
echo %1\tools\lazres.exe
ECHO "nao encontrado... Favor compilar o projeto:  lazres.lpi"
goto fim

:fim
echo "Tecle ENTER para sair" .
pause
