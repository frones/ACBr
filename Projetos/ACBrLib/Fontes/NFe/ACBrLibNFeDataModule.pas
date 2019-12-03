{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

unit ACBrLibNFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, ACBrNFe, ACBrNFeDANFeRLClass, ACBrMail,
  ACBrPosPrinter, ACBrIntegrador, ACBrNFeDANFeESCPOS, ACBrDANFCeFortesFr,
  ACBrDANFCeFortesFrA4, ACBrLibConfig, ACBrLibMailImport,
  ACBrLibPosPrinterImport;

type

  { TLibNFeDM }

  TLibNFeDM = class(TDataModule)
    ACBrIntegrador1: TACBrIntegrador;
    ACBrNFe1: TACBrNFe;
    ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes;
    ACBrNFeDANFCeFortesA4: TACBrNFeDANFCeFortesA4;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrNFeDANFeRL1: TACBrNFeDANFeRL;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;
    FACBrMail: TACBrMail;
    FACBrPosPrinter: TACBrPosPrinter;

    FLibMail: TACBrLibMail;
    FLibPosPrinter: TACBrLibPosPrinter;
  public
    procedure CriarACBrMail;
    procedure CriarACBrPosPrinter;

    procedure AplicarConfiguracoes;
    procedure AplicarConfigMail;
    procedure AplicarConfigPosPrinter;
    procedure ValidarIntegradorNFCe();
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  Protocolo: String = ''; MostrarPreview: String = ''; MarcaDagua: String = '';
                                  ViaConsumidor: String = ''; Simplificado: String = '');
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  pcnConversao, pcnConversaoNFe,
  ACBrUtil, FileUtil, ACBrNFeDANFEClass, ACBrLibConsts,
  ACBrDeviceConfig, ACBrLibNFeConfig, ACBrLibComum;

{$R *.lfm}

{ TLibNFeDM }

procedure TLibNFeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;

  FACBrMail := Nil;
  FLibMail := Nil;
  FACBrPosPrinter := Nil;
  FLibPosPrinter := Nil;
end;

procedure TLibNFeDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;

  if Assigned(FLibMail) then
    FreeAndNil(FLibMail)
  else if Assigned(FACBrMail) then
    FreeAndNil(FACBrMail);

  if Assigned(FLibPosPrinter) then
    FreeAndNil(FLibPosPrinter)
  else if Assigned(FACBrPosPrinter) then
    FreeAndNil(FACBrPosPrinter);
end;

procedure TLibNFeDM.CriarACBrMail;
var
  NomeLib: String;
begin
  if Assigned(FLibMail) or Assigned(FACBrMail) then
    Exit;

  GravarLog('  CriarACBrMail', logCompleto);

  NomeLib := ApplicationPath + CACBrMailLIBName;
  if FileExists(NomeLib) then
  begin
    GravarLog('      Carregando MAIL de: ' + NomeLib, logCompleto);
    // Criando Classe para Leitura da Lib //
    FLibMail  := TACBrLibMail.Create(NomeLib, pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FACBrMail := FLibMail.ACBrMail;
  end
  else
  begin
    GravarLog('     Criando MAIL Interno', logCompleto);
    FACBrMail := TACBrMail.Create(Nil);
  end;

  ACBrNFe1.MAIL := FACBrMail;
end;

procedure TLibNFeDM.CriarACBrPosPrinter;
var
  NomeLib: String;
begin
  if Assigned(FLibPosPrinter) or Assigned(FACBrPosPrinter) then
    Exit;

  GravarLog('  CriarACBrPosPrinter', logCompleto);

  NomeLib := ApplicationPath + CACBrPosPrinterLIBName;
  if FileExists(NomeLib) then
  begin
    GravarLog('      Carregando PosPrinter de: ' + NomeLib, logCompleto);
    // Criando Classe para Leitura da Lib //
    FLibPosPrinter  := TACBrLibPosPrinter.Create(NomeLib, pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FACBrPosPrinter := FLibPosPrinter.ACBrPosPrinter;
  end
  else
  begin
    GravarLog('     Criando PosPrinter Interno', logCompleto);
    FACBrPosPrinter := TACBrPosPrinter.Create(Nil);
    TLibNFeConfig(pLib.Config).PosDevice := TDeviceConfig.Create(CSessaoPosPrinterDevice);
  end;

  ACBrNFeDANFeESCPOS1.PosPrinter := FACBrPosPrinter;
end;

procedure TLibNFeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibNFeConfig;
begin
  ACBrNFe1.SSL.DescarregarCertificado;
  pLibConfig := TLibNFeConfig(pLib.Config);
  ACBrNFe1.Configuracoes.Assign(pLibConfig.NFe);

  with ACBrIntegrador1 do
  begin
    ArqLOG := pLibConfig.Integrador.ArqLOG;
    PastaInput := pLibConfig.Integrador.PastaInput;
    PastaOutput := pLibConfig.Integrador.PastaOutput;
    Timeout := pLibConfig.Integrador.Timeout;
  end;

  AplicarConfigMail;
  AplicarConfigPosPrinter;
end;

procedure TLibNFeDM.AplicarConfigMail;
begin
  if Assigned(FLibMail) then
  begin
    FLibMail.ConfigLer(pLib.Config.NomeArquivo);
    Exit;
  end;

  with FACBrMail do
  begin
    Attempts := pLib.Config.Email.Tentativas;
    SetTLS := pLib.Config.Email.TLS;
    DefaultCharset := pLib.Config.Email.Codificacao;
    From := pLib.Config.Email.Conta;
    FromName := pLib.Config.Email.Nome;
    SetSSL := pLib.Config.Email.SSL;
    Host := pLib.Config.Email.Servidor;
    IDECharset := pLib.Config.Email.Codificacao;
    IsHTML := pLib.Config.Email.IsHTML;
    Password := pLib.Config.Email.Senha;
    Port := IntToStr(pLib.Config.Email.Porta);
    Priority := pLib.Config.Email.Priority;
    ReadingConfirmation := pLib.Config.Email.Confirmacao;
    DeliveryConfirmation := pLib.Config.Email.ConfirmacaoEntrega;
    TimeOut := pLib.Config.Email.TimeOut;
    Username := pLib.Config.Email.Usuario;
    UseThread := pLib.Config.Email.SegundoPlano;
  end;
end;

procedure TLibNFeDM.AplicarConfigPosPrinter;
var
  pLibConfig: TLibNFeConfig;
begin
  if Assigned(FLibPosPrinter) then
  begin
    FLibPosPrinter.ConfigLer(pLib.Config.NomeArquivo);
    Exit;
  end;

  pLibConfig := TLibNFeConfig(pLib.Config);

  with FACBrPosPrinter do
  begin
    ArqLog := pLibConfig.PosPrinter.ArqLog;
    Modelo := TACBrPosPrinterModelo(pLibConfig.PosPrinter.Modelo);
    Porta := pLibConfig.PosPrinter.Porta;
    PaginaDeCodigo := TACBrPosPaginaCodigo(pLibConfig.PosPrinter.PaginaDeCodigo);
    ColunasFonteNormal := pLibConfig.PosPrinter.ColunasFonteNormal;
    EspacoEntreLinhas := pLibConfig.PosPrinter.EspacoEntreLinhas;
    LinhasEntreCupons := pLibConfig.PosPrinter.LinhasEntreCupons;
    CortaPapel := pLibConfig.PosPrinter.CortaPapel;
    TraduzirTags := pLibConfig.PosPrinter.TraduzirTags;
    IgnorarTags := pLibConfig.PosPrinter.IgnorarTags;
    LinhasBuffer := pLibConfig.PosPrinter.LinhasBuffer;
    ControlePorta := pLibConfig.PosPrinter.ControlePorta;
    VerificarImpressora := pLibConfig.PosPrinter.VerificarImpressora;

    ConfigBarras.MostrarCodigo := pLibConfig.PosPrinter.BcMostrarCodigo;
    ConfigBarras.LarguraLinha := pLibConfig.PosPrinter.BcLarguraLinha;
    ConfigBarras.Altura := pLibConfig.PosPrinter.BcAltura;
    ConfigBarras.Margem := pLibConfig.PosPrinter.BcMargem;

    ConfigQRCode.Tipo := pLibConfig.PosPrinter.QrTipo;
    ConfigQRCode.LarguraModulo := pLibConfig.PosPrinter.QrLarguraModulo;
    ConfigQRCode.ErrorLevel := pLibConfig.PosPrinter.QrErrorLevel;

    ConfigLogo.IgnorarLogo := pLibConfig.PosPrinter.LgIgnorarLogo;
    ConfigLogo.KeyCode1 := pLibConfig.PosPrinter.LgKeyCode1;
    ConfigLogo.KeyCode2 := pLibConfig.PosPrinter.LgKeyCode2;
    ConfigLogo.FatorX := pLibConfig.PosPrinter.LgFatorX;
    ConfigLogo.FatorY := pLibConfig.PosPrinter.LgFatorY;

    ConfigGaveta.SinalInvertido := pLibConfig.PosPrinter.GvSinalInvertido;
    ConfigGaveta.TempoON := pLibConfig.PosPrinter.GvTempoON;
    ConfigGaveta.TempoOFF := pLibConfig.PosPrinter.GvTempoOFF;

    ConfigModoPagina.Largura := pLibConfig.PosPrinter.MpLargura;
    ConfigModoPagina.Altura := pLibConfig.PosPrinter.MpAltura;
    ConfigModoPagina.Esquerda := pLibConfig.PosPrinter.MpEsquerda;
    ConfigModoPagina.Topo := pLibConfig.PosPrinter.MpTopo;
    ConfigModoPagina.Direcao := TACBrPosDirecao(pLibConfig.PosPrinter.MpDirecao);
    ConfigModoPagina.EspacoEntreLinhas := pLibConfig.PosPrinter.MpEspacoEntreLinhas;

    pLibConfig.PosDevice.Apply(Device);
  end;
end;

procedure TLibNFeDM.ConfigurarImpressao(NomeImpressora: String; GerarPDF: Boolean = False;
  Protocolo: String = ''; MostrarPreview: String = ''; MarcaDagua: String = '';
  ViaConsumidor: String = ''; Simplificado: String = '');
var
  pLibConfig: TLibNFeConfig;
begin
  pLibConfig := TLibNFeConfig(pLib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  ACBrNFe1.DANFE := ACBrNFeDANFeRL1;

  if ACBrNFe1.NotasFiscais.Count > 0 then
  begin
    if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
    begin
      if (pLibConfig.DANFe.NFCe.TipoRelatorioBobina = tpFortes) or GerarPDF then
        ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1
      else if (pLibConfig.DANFe.NFCe.TipoRelatorioBobina = tpFortesA4) then
        ACBrNFe1.DANFE := ACBrNFeDANFCeFortesA4
      else
        ACBrNFe1.DANFE := ACBrNFeDANFeESCPOS1;
    end;

    if (ACBrNFe1.NotasFiscais.Items[0].NFe.procNFe.cStat in [101, 151, 155]) then
      ACBrNFe1.DANFE.Cancelada := True
    else
      ACBrNFe1.DANFE.Cancelada := False;
  end;

  if GerarPDF and not DirectoryExists(PathWithDelim(pLibConfig.DANFe.PathPDF))then
    ForceDirectories(PathWithDelim(pLibConfig.DANFe.PathPDF));

  pLibConfig.DANFe.Apply(ACBrNFe1.DANFE);

  if NaoEstaVazio(NomeImpressora) then
    ACBrNFe1.DANFE.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    ACBrNFe1.DANFE.MostraPreview := StrToBoolDef(MostrarPreview, False);

  if NaoEstaVazio(Protocolo) then
    ACBrNFe1.DANFE.Protocolo := Protocolo
  else
    ACBrNFe1.DANFE.Protocolo := '';

  if ACBrNFe1.DANFE is TACBrNFeDANFCEClass then
  begin
     if NaoEstaVazio(ViaConsumidor) then
       TACBrNFeDANFCEClass(ACBrNFe1.DANFE).ViaConsumidor := StrToBoolDef(ViaConsumidor, False);

     if ACBrNFe1.DANFE = ACBrNFeDANFeESCPOS1 then
     begin
        if not ACBrNFeDANFeESCPOS1.PosPrinter.ControlePorta then
        begin
          ACBrNFeDANFeESCPOS1.PosPrinter.Ativar;
          if not ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativo then
            ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativar;
        end;
     end;
  end
  else
  begin
    if NaoEstaVazio(Simplificado) then
    begin
      if StrToBoolDef(Simplificado, False) then
        ACBrNFeDANFeRL1.TipoDANFE := tiSimplificado;
    end;

     if NaoEstaVazio(MarcaDagua) then
       ACBrNFeDANFeRL1.MarcaDagua := MarcaDagua
     else
       ACBrNFeDANFeRL1.MarcaDagua := '';
  end;

  GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibNFeDM.ValidarIntegradorNFCe;

begin
  if (ACBrNFe1.Configuracoes.Geral.ModeloDF = moNFCe) and
     (ACBrNFe1.Configuracoes.WebServices.UF = 'CE') then
    ACBrNFe1.Integrador := ACBrIntegrador1
  else
    ACBrNFe1.Integrador := nil;
end;

procedure TLibNFeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibNFeDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibNFeDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.
