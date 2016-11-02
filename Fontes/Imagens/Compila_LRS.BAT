@ECHO OFF
@ECHO OFF

if "%1" == "" goto param_default

if NOT EXIST %1\lazarus.exe goto erro_param

if NOT EXIST %1\tools\lazres.exe GOTO erro_lazres

:Comum
%1\tools\lazres.exe ACBrComum.lrs TACBrAAC.bmp
move ACBrComum.lrs ..\ACBrComum
echo.
echo "****************************************"
echo "* Arquivo   ACBrComum.lrs   gerado  *"
echo "****************************************"
echo.

:OpenSSL
%1\tools\lazres.exe ACBrOpenSSL.lrs TACBrEAD.bmp
move ACBrOpenSSL.lrs ..\ACBrOpenSSL
echo.
echo "****************************************"
echo "* Arquivo   ACBrOpenSSL.lrs   gerado   *"
echo "****************************************"
echo.

:Diversos
%1\tools\lazres.exe ACBrDiversos.lrs TACBrCalculadora.bmp TACBrExtenso.bmp TACBrValidador.bmp TACBrTroco.bmp TACBrCMC7.bmp TACBrFala.bmp TACBrGIF.bmp TACBrBarCode.bmp TACBrEnterTab.bmp TACBrCargaBal.png TACBrCotacao.bmp TACBrInStore.png
move ACBrDiversos.lrs ..\ACBrDiversos
echo.
echo "****************************************"
echo "* Arquivo   ACBrDiversos.lrs   gerado  *"
echo "****************************************"
echo.

:Serial
%1\tools\lazres.exe ACBrSerial.lrs TACBrECF.bmp TACBrGAV.bmp TACBrCHQ.bmp TACBrLCB.bmp TACBrDIS.bmp TACBrBAL.bmp TACBrTER.bmp TACBrETQ.bmp TACBrRFD.bmp TACBrSMS.bmp TACBrPosPrinter.bmp
move ACBrSerial.lrs ..\ACBrSerial
echo.
echo "****************************************"
echo "* Arquivo   ACBrSerial.lrs   gerado    *"
echo "****************************************"
echo.

:TEFD
%1\tools\lazres.exe ACBrTEFD.lrs TACBrTEFD.bmp
move ACBrTEFD.lrs ..\ACBrTEFD
echo.
echo "****************************************"
echo "* Arquivo   ACBrTEFD.lrs   gerado      *"
echo "****************************************"
echo.

:TCP
%1\tools\lazres.exe ACBrTCP.lrs TACBrTCPServer.bmp TACBrCEP.bmp TACBrIBGE.bmp TACBrNFPws.bmp TACBrCNIEE.bmp TACBrSuframa.bmp TACBrDownload.bmp TACBrIBPTax.bmp TACBrConsultaCNPJ.bmp TACBrMail.bmp TACBrConsultaCPF.png TACBrSpedTabelas.bmp TACBrSedex.bmp TACBrNCMs.bmp
move ACBrTCP.lrs ..\ACBrTCP
echo.
echo "****************************************"
echo "* Arquivo   ACBrTCP.lrs   gerado       *"
echo "****************************************"
echo.

:MTER
%1\tools\lazres.exe ACBrMTER.lrs TACBrMTer.bmp
move ACBrMTER.lrs ..\ACBrTCP
echo.
echo "****************************************"
echo "* Arquivo   ACBrMTER.lrs   gerado       *"
echo "****************************************"
echo.

:SPED
%1\tools\lazres.exe ACBrSpedFiscal.lrs TACBrSpedFiscal.bmp
%1\tools\lazres.exe ACBrSpedContabil.lrs TACBrSpedContabil.bmp
%1\tools\lazres.exe ACBrSpedPisCofins.lrs TACBrSpedPisCofins.bmp
move ACBrSpedFiscal.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDFiscal
move ACBrSpedContabil.lrs ..\ACBrTXT\ACBrSPED\ACBrSPEDContabil
move ACBrSpedPisCofins.lrs ..\ACBrTXT\ACBrSPED\ACBrSpedPisCofins
move ACBrSpedECF.lrs ..\ACBrTXT\ACBrSPED\ACBrSpedECF
echo.
echo "****************************************"
echo "* Arquivo  ACBrSpedFiscal.lrs   gerado *"
echo "* Arquivo  ACBrSpedContabil.lrs gerado *"
echo "* Arquivo  ACBrSpedPisCofins.lrs gerado*"
echo "* Arquivo  ACBrSpedECF.lrs gerado      *"
echo "****************************************"
echo.

:PAF
%1\tools\lazres.exe ACBrPAF.lrs TACBrPAF.bmp
move ACBrPAF.lrs ..\ACBrTXT\ACBrPAF
echo.
echo "****************************************"
echo "* Arquivo   ACBrPAF.lrs   gerado       *"
echo "****************************************"
echo.

:Convenio115
%1\tools\lazres.exe ACBrConvenio115.lrs TACBrConvenio115.bmp
move ACBrConvenio115.lrs ..\ACBrTXT\ACBrConvenio115
echo.
echo "****************************************"
echo "* Arquivo   ACBrConvenio115.lrs   gerado       *"
echo "****************************************"
echo.

:Boleto
%1\tools\lazres.exe ACBrBoleto.lrs TACBrBoleto.bmp
%1\tools\lazres.exe ACBrBoletoFCFortes.lrs TACBrBoletoFCFortes.bmp
%1\tools\lazres.exe ACBrBoletoFCLazReport.lrs TACBrBoletoFCLazReport.bmp ..\ACBrBoleto\FC\Laz\FCLazReport_Padrao.lrf ..\ACBrBoleto\FC\Laz\FCLazReport_CompEntrega.lrf ..\ACBrBoleto\FC\Laz\FCLazReport_Carne.lrf

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
%1\tools\lazres.exe ACBrNFe.lrs TACBrNFe.bmp 
move ACBrNFe.lrs ..\ACBrDFe\ACBrNFe

%1\tools\lazres.exe ACBrNFeDANFeRL.lrs TACBrNFeDANFeRL.bmp
move ACBrNFeDANFeRL.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFe\Fortes

%1\tools\lazres.exe ACBrNFeDANFCeFortes.lrs TACBrNFeDANFCeFortes.bmp
move ACBrNFeDANFCeFortes.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFCe\Fortes

%1\tools\lazres.exe ACBrNFeDANFeESCPOS.lrs TACBrNFeDANFeESCPOS.bmp 
move ACBrNFeDANFeESCPOS.lrs ..\ACBrDFe\ACBrNFe\DANFE\NFCe\EscPos

%1\tools\lazres.exe ACBrECFVirtualNFCe.lrs TACBrECFVirtualNFCe.png
move ACBrECFVirtualNFCe.lrs ..\ACBrDFe\ACBrNFe\ACBrECFVirtualNFCe


echo.
echo "**************************************************************"
echo "* Arquivos    ACBrNFe.lrs, ACBrNFeDANFeRL.lrs     gerados    *"
echo "**************************************************************"
echo.

:GNRE
%1\tools\lazres.exe ACBrGNRE.lrs TACBrGNRE.bmp 
move ACBrGNRE.lrs ..\ACBrDFe\ACBrGNRE

%1\tools\lazres.exe ACBrGNREGuiaFR.lrs TACBrGNREGuiaFR.bmp 
move ACBrGNREGuiaFR.lrs ..\ACBrDFe\ACBrGNRE\GNRE\Fortes

echo.
echo "****************************************"
echo "* Arquivo    ACBrGNRE.lrs     gerado    *"
echo "****************************************"
echo.

:NFSe
%1\tools\lazres.exe ACBrNFSe.lrs TACBrNFSe.bmp 
move ACBrNFSe.lrs ..\ACBrDFe\ACBrNFSe

echo.
echo "****************************************"
echo "* Arquivo    ACBrNFSe.lrs     gerado    *"
echo "****************************************"
echo.

:CTe
%1\tools\lazres.exe ACBrCTe.lrs TACBrCTe.bmp 
move ACBrCTe.lrs ..\ACBrDFe\ACBrCTe

echo.
echo "****************************************"
echo "* Arquivo    ACBrCTe.lrs     gerado    *"
echo "****************************************"
echo.

:MDFe
%1\tools\lazres.exe ACBrMDFe.lrs TACBrMDFe.bmp
move ACBrMDFe.lrs ..\ACBrDFe\ACBrMDFe

%1\tools\lazres.exe ACBrMDFeDAMDFeRL.lrs TACBrMDFeDAMDFeRL.png
move ACBrMDFeDAMDFeRL.lrs ..\ACBrDFe\ACBrMDFe\DAMDFE\Fortes

echo.
echo "****************************************"
echo "* Arquivo    ACBrMDFe.lrs     gerado    *"
echo "****************************************"
echo.

:LFD
%1\tools\lazres.exe ACBrLFD.lrs TACBrLFD.bmp 
move ACBrLFD.lrs ..\ACBrTXT\ACBrLFD

echo.
echo "****************************************"
echo "* Arquivo    ACBrLFD.lrs     gerado    *"
echo "****************************************"
echo.

:Sintegra
%1\tools\lazres.exe ACBrSintegra.lrs TACBrSintegra.bmp
move ACBrSintegra.lrs ..\ACBrTXT\ACBrSintegra
echo.
echo "****************************************"
echo "* Arquivo   ACBrSintegra.lrs   gerado  *"
echo "****************************************"
echo.

:SAT
%1\tools\lazres.exe ACBrSAT.lrs TACBrSAT.bmp
move ACBrSAT.lrs ..\ACBrSAT
                  
%1\tools\lazres.exe ACBrSATExtratoESCPOS.lrs TACBrSATExtratoESCPOS.bmp
move ACBrSATExtratoESCPOS.lrs ..\ACBrSAT\Extrato\EscPos

%1\tools\lazres.exe ACBrSATExtratoFortes.lrs TACBrSATExtratoFortes.bmp
move ACBrSATExtratoFortes.lrs ..\ACBrSAT\Extrato\Fortes

%1\tools\lazres.exe ACBrECFVirtualSAT.lrs TACBrECFVirtualSAT.png
move ACBrECFVirtualSAT.lrs ..\ACBrSAT\ACBrECFVirtualSAT

echo.
echo "********************************************************************"
echo "*   Arquivos   ACBrSAT.lrs, ACBrSATExtratoFortes.lrs   gerados     *"
echo "********************************************************************"
echo.


:SEF2
%1\tools\lazres.exe ACBrSEF2.lrs TACBrSEF2.bmp
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
