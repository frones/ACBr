{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrPrinter1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, Forms, IniFiles, pcnConversao,
  ACBrPosPrinter, ACBrSAT, ACBrNFe, ACBrNFeDANFeRLClass, ACBrNFeDANFeESCPOS,
  ACBrDANFCeFortesFr, ACBrSATExtratoFortesFr, ACBrSATExtratoESCPOS;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    ACBrNFe1: TACBrNFe;
    ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrNFeDANFeRL1: TACBrNFeDANFeRL;
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrSAT1: TACBrSAT;
    ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS;
    ACBrSATExtratoFortes1: TACBrSATExtratoFortes;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
    procedure ShowHowToUseAndFinalize;
    procedure ReadConfigNFeNFCe(FileConfig : String);
    procedure ReadConfigSAT(FileConfig : String);
  public
    { public declarations }
    procedure ReadConfigPosPrinter(FileConfig : String);
  end;

var
  DataModule1: TDataModule1;

implementation

uses
  configuracoes, ACBrUtil.Base, ACBrUtil.FilesIO;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
  frConfiguracoes: TfrConfiguracoes;
  TipoRel, ArquivoImpressao, ArquivoConfiguracao : String;
  ArqTexto : TStringList;
begin
  if Paramcount >= 1 then
  begin
     if UpperCase(ParamStr(1)) = '/C' then
     begin
       frConfiguracoes := TfrConfiguracoes.Create(Self) ;
       frConfiguracoes.ShowModal;
       frConfiguracoes.Free;
       Application.Terminate;
     end
     else
     begin
       if Paramcount >= 1 then
         TipoRel := UpperCase(ParamStr(1))
       else
         TipoRel := '';

       if Paramcount >= 2 then
         ArquivoImpressao := UpperCase(ParamStr(2))
       else
         ArquivoImpressao := '';

       if Paramcount >= 3 then
         ArquivoConfiguracao := UpperCase(ParamStr(3))
       else
         ArquivoConfiguracao := ''; //Pegar valor default

       if EstaVazio(TipoRel) or EstaVazio(ArquivoImpressao) then
         ShowHowToUseAndFinalize;

       ReadConfigPosPrinter(ArquivoConfiguracao);

       if not FileExists(ArquivoImpressao) then
       begin
          MessageDlg('Arquivo '+ArquivoImpressao+' não encontrado.', mtError, [mbOK], 0);
          Application.Terminate;
       end;

       if TipoRel = 'SAT' then
       begin
         ACBrSAT1.CFe.Clear;
         ACBrSAT1.CFe.LoadFromFile(ArquivoImpressao);

         ReadConfigSAT(ArquivoConfiguracao);

         ACBrSAT1.ImprimirExtrato;
       end
       else if (TipoRel = 'NFE') or (TipoRel = 'NFCE') then
       begin
         ACBrNFe1.NotasFiscais.Clear;
         ACBrNFe1.NotasFiscais.LoadFromFile(ArquivoImpressao);

         ReadConfigNFeNFCe(ArquivoConfiguracao);

         ACBrNFe1.NotasFiscais.Imprimir;
       end
       else if (TipoRel = 'TEXTO') then
       begin
         ArqTexto := TStringList.Create;
         try
           ArqTexto.LoadFromFile(ArquivoImpressao);
           ACBrPosPrinter1.Imprimir(ArqTexto.Text);

         finally
           ArqTexto.Free;
         end;
       end;

       Application.Terminate;
     end;
  end
  else
  begin
    ShowHowToUseAndFinalize;
  end;
end;

procedure TDataModule1.ShowHowToUseAndFinalize;
begin
  MessageDlg('Para utilizar este aplicativo use a seguinte sintaxe:'+sLineBreak+
             //'ACBrPrinter.exe Impressora TipoRel ArquivoImpressao'+sLineBreak+
             'ACBrPrinter.exe TipoRel ArquivoImpressao ArquivoConfiguracao'+sLineBreak+
             'Onde: '+sLineBreak+
             //'Impressora - Nome da impressora cadastrada no ACBrPrinter.'+sLineBreak+
             'TipoRel - NFe, NFCe, SAT ou Texto.'+sLineBreak+
             'ArquivoImpressao - Arquivo a ser impresso.'+sLineBreak+
             'ArquivoConfiguracao - Nome do arquivo de configuração criado com o comando ACBrPrinter.exe /c.'+sLineBreak+
             'Para configurar, use ACBrPrinter.exe /c', mtInformation, [mbOK], 0);
  Application.Terminate;
end;

procedure TDataModule1.ReadConfigPosPrinter(FileConfig: String);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileConfig);
  try
    ACBrPosPrinter1.Desativar;
    ACBrPosPrinter1.Modelo             := TACBrPosPrinterModelo(INI.ReadInteger('PosPrinter', 'Modelo', Integer(ACBrPosPrinter1.Modelo)));
    ACBrPosPrinter1.Porta              := INI.ReadString('PosPrinter', 'Porta', ACBrPosPrinter1.Porta);
    ACBrPosPrinter1.LinhasBuffer       := INI.ReadInteger('PosPrinter', 'LinhasBuffer', ACBrPosPrinter1.LinhasBuffer);
    ACBrPosPrinter1.LinhasEntreCupons  := INI.ReadInteger('PosPrinter', 'LinhasPular', 7);
    ACBrPosPrinter1.EspacoEntreLinhas  := INI.ReadInteger('PosPrinter', 'EspacoEntreLinhas', ACBrPosPrinter1.EspacoEntreLinhas);
    ACBrPosPrinter1.ColunasFonteNormal := INI.ReadInteger('PosPrinter', 'Colunas', ACBrPosPrinter1.ColunasFonteNormal);
    ACBrPosPrinter1.ControlePorta      := True;
    ACBrPosPrinter1.PaginaDeCodigo     := TACBrPosPaginaCodigo(INI.ReadInteger('PosPrinter', 'PaginaDeCodigo', Integer(ACBrPosPrinter1.PaginaDeCodigo)));
    ACBrPosPrinter1.IgnorarTags        := INI.ReadBool('PosPrinter', 'IgnorarTags', ACBrPosPrinter1.IgnorarTags);
    ACBrPosPrinter1.TraduzirTags       := INI.ReadBool('PosPrinter', 'TraduzirTags', ACBrPosPrinter1.TraduzirTags);
    ACBrPosPrinter1.ArqLOG             := INI.ReadString('PosPrinter', 'ArqLog', ACBrPosPrinter1.ArqLOG);

    ACBrPosPrinter1.ConfigBarras.LarguraLinha  := INI.ReadInteger('Barras', 'Largura', ACBrPosPrinter1.ConfigBarras.LarguraLinha);
    ACBrPosPrinter1.ConfigBarras.Altura        := INI.ReadInteger('Barras', 'Altura', ACBrPosPrinter1.ConfigBarras.Altura);
    ACBrPosPrinter1.ConfigBarras.MostrarCodigo := INI.ReadBool('Barras', 'HRI', ACBrPosPrinter1.ConfigBarras.MostrarCodigo);

    ACBrPosPrinter1.ConfigQRCode.ErrorLevel    := INI.ReadInteger('QRCode', 'Tipo', ACBrPosPrinter1.ConfigQRCode.Tipo);
    ACBrPosPrinter1.ConfigQRCode.LarguraModulo := INI.ReadInteger('QRCode', 'LarguraModulo', ACBrPosPrinter1.ConfigQRCode.LarguraModulo);
    ACBrPosPrinter1.ConfigQRCode.Tipo          := INI.ReadInteger('QRCode', 'Tipo', ACBrPosPrinter1.ConfigQRCode.Tipo);

    ACBrPosPrinter1.ConfigLogo.FatorX   := INI.ReadInteger('Logo', 'FatorX', ACBrPosPrinter1.ConfigLogo.FatorX);
    ACBrPosPrinter1.ConfigLogo.FatorY   := INI.ReadInteger('Logo', 'FatorY', ACBrPosPrinter1.ConfigLogo.FatorY);
    ACBrPosPrinter1.ConfigLogo.KeyCode1 := INI.ReadInteger('Logo', 'KC1', ACBrPosPrinter1.ConfigLogo.KeyCode1);
    ACBrPosPrinter1.ConfigLogo.KeyCode2 := INI.ReadInteger('Logo', 'KC2', ACBrPosPrinter1.ConfigLogo.KeyCode2);

    ACBrNFe1.Configuracoes.Geral.Salvar := False;
    ACBrNFe1.Configuracoes.Arquivos.Salvar := False;
  finally
    Ini.Free;
  end;

end;

procedure TDataModule1.ReadConfigNFeNFCe(FileConfig: String);
var
  Ini: TIniFile;
  OK: Boolean;
begin
  Ini := TIniFile.Create(FileConfig);
  try
    if ACBrNFe1.NotasFiscais.Count > 0 then
    begin
      if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
      begin
        if Ini.ReadInteger('NFCe', 'Modelo', 1) = 0 then
          ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1
        else
          ACBrNFe1.DANFE := ACBrNFeDANFeESCPOS1;
        ACBrNFe1.DANFE.Impressora := Ini.ReadString('NFCe', 'ImpressoraPadrao', '0');
      end
      else
      begin
        ACBrNFe1.DANFE := ACBrNFeDANFeRL1;
        ACBrNFe1.DANFE.Impressora := Ini.ReadString('NFe', 'Impressora', '0');
      end;
    end;

    if ACBrNFe1.DANFE <> nil then
    begin
      ACBrNFe1.DANFE.TipoDANFE := StrToTpImp(OK, IntToStr(Ini.ReadInteger('NFe', 'DANFE', 0) + 1));
      ACBrNFe1.DANFE.Logo := Ini.ReadString('DANFE', 'LogoMarca', '');
      ACBrNFe1.DANFE.Sistema := Ini.ReadString('DANFE', 'SoftwareHouse', '');
      ACBrNFe1.DANFE.Site := Ini.ReadString('DANFE', 'Site', '');
      ACBrNFe1.DANFE.Email := Ini.ReadString('DANFE', 'Email', '');
      ACBrNFe1.DANFE.MostraPreview := Ini.ReadBool('DANFE', 'MostrarPreview', False);
      ACBrNFe1.DANFE.NumCopias := Ini.ReadInteger('DANFE', 'Copias', 1);
      ACBrNFe1.DANFE.MargemInferior := Ini.ReadFloat('DANFE', 'Margem', 0.8);
      ACBrNFe1.DANFE.MargemSuperior := Ini.ReadFloat('DANFE', 'MargemSup', 0.8);
      ACBrNFe1.DANFE.MargemDireita  := Ini.ReadFloat('DANFE', 'MargemDir', 0.51);
      ACBrNFe1.DANFE.MargemEsquerda := Ini.ReadFloat('DANFE', 'MargemEsq', 0.6);
      ACBrNFe1.DANFE.PathPDF := Ini.ReadString('DANFE', 'PathPDF', PathWithDelim(ExtractFilePath(Application.ExeName))+'PDF');
      ACBrNFe1.DANFE.CasasDecimais.qCom := Ini.ReadInteger('DANFE', 'DecimaisQTD', 2);
      ACBrNFe1.DANFE.CasasDecimais.vUnCom := Ini.ReadInteger('DANFE', 'DecimaisValor', 2);
      ACBrNFe1.DANFE.ImprimeTotalLiquido := Ini.ReadBool('DANFE', 'ImprimirValLiq', False);
      ACBrNFe1.DANFE.MostraStatus := Ini.ReadBool('DANFE', 'MostrarStatus', False);
      ACBrNFe1.DANFE.ExpandeLogoMarca := Ini.ReadBool('DANFE', 'ExpandirLogo', False);

      if ACBrNFe1.DANFE = ACBrNFeDANFeRL1 then
      begin
        ACBrNFeDANFeRL1.Fonte.Nome := TNomeFonte(Ini.ReadInteger('DANFE', 'Fonte', 0));
        ACBrNFeDANFeRL1.LarguraCodProd := Ini.ReadInteger('DANFE', 'LarguraCodigoProduto', 54);
        ACBrNFeDANFeRL1.ExibeEAN := Ini.ReadBool('DANFE', 'ExibirEAN', False);
        ACBrNFeDANFeRL1.QuebraLinhaEmDetalhamentos:= Ini.ReadBool('DANFE', 'QuebraLinhaEmDetalhe', False);
      end
      else if ACBrNFe1.DANFE = ACBrNFeDANFeESCPOS1 then
      begin
        ACBrNFeDANFeESCPOS1.ImprimeEmUmaLinha := Ini.ReadBool('NFCe', 'ImprimirItem1Linha', True);
        ACBrNFeDANFeESCPOS1.ImprimeDescAcrescItem := Ini.ReadBool('NFCe', 'ImprimirDescAcresItem', True);
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDataModule1.ReadConfigSAT(FileConfig: String);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileConfig);
  try
    if INI.ReadBool('SATFortes','UsarFortes', False) then
      ACBrSAT1.Extrato := ACBrSATExtratoFortes1
    else
      ACBrSAT1.Extrato := ACBrSATExtratoESCPOS1;

    ACBrSATExtratoFortes1.LarguraBobina := INI.ReadInteger('SATFortes','Largura',ACBrSATExtratoFortes1.LarguraBobina);
    ACBrSATExtratoFortes1.MargemSuperior  := INI.ReadFloat('SATFortes','MargemTopo',ACBrSATExtratoFortes1.MargemSuperior);
    ACBrSATExtratoFortes1.MargemInferior := INI.ReadFloat('SATFortes','MargemFundo',ACBrSATExtratoFortes1.MargemInferior);
    ACBrSATExtratoFortes1.MargemEsquerda := INI.ReadFloat('SATFortes','MargemEsquerda',ACBrSATExtratoFortes1.MargemEsquerda);
    ACBrSATExtratoFortes1.MargemDireita := INI.ReadFloat('Fortes','MargemDireita',ACBrSATExtratoFortes1.MargemDireita);
    ACBrSATExtratoFortes1.MostraPreview := INI.ReadBool('SATFortes','Preview',False);
  finally
    Ini.Free;
  end;
end;

end.

