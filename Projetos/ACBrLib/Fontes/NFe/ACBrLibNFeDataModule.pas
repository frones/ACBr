{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
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

unit ACBrLibNFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, ACBrNFe, ACBrNFeDANFeRLClass, ACBrMail,
  ACBrPosPrinter, ACBrIntegrador, ACBrNFeDANFeESCPOS, ACBrDANFCeFortesFr,
  ACBrDANFCeFortesFrA4, ACBrLibConfig, ACBrLibMailImport,
  ACBrLibPosPrinterImport, ACBrLibComum;

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
    fpLib: TACBrLib;

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

    property Lib: TACBrLib read fpLib write fpLib;
  end;

implementation

uses
  pcnConversao, pcnConversaoNFe,
  ACBrUtil, FileUtil, ACBrDeviceSerial, ACBrNFeDANFEClass,
  ACBrDeviceConfig, ACBrLibNFeConfig;

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
    FLibMail  := TACBrLibMail.Create(NomeLib, Lib.Config.NomeArquivo, Lib.Config.ChaveCrypt);
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
    FLibPosPrinter  := TACBrLibPosPrinter.Create(NomeLib, Lib.Config.NomeArquivo, Lib.Config.ChaveCrypt);
    FACBrPosPrinter := FLibPosPrinter.ACBrPosPrinter;
  end
  else
  begin
    GravarLog('     Criando PosPrinter Interno', logCompleto);
    FACBrPosPrinter := TACBrPosPrinter.Create(Nil);
  end;

  ACBrNFeDANFeESCPOS1.PosPrinter := FACBrPosPrinter;
end;

procedure TLibNFeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibNFeConfig;
begin
  ACBrNFe1.SSL.DescarregarCertificado;
  pLibConfig := TLibNFeConfig(Lib.Config);
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
    FLibMail.ConfigLer(Lib.Config.NomeArquivo);
    Exit;
  end;

  with FACBrMail do
  begin
    Attempts := Lib.Config.Email.Tentativas;
    SetTLS := Lib.Config.Email.TLS;
    DefaultCharset := Lib.Config.Email.Codificacao;
    From := Lib.Config.Email.Conta;
    FromName := Lib.Config.Email.Nome;
    SetSSL := Lib.Config.Email.SSL;
    Host := Lib.Config.Email.Servidor;
    IDECharset := Lib.Config.Email.Codificacao;
    IsHTML := Lib.Config.Email.IsHTML;
    Password := Lib.Config.Email.Senha;
    Port := IntToStr(Lib.Config.Email.Porta);
    Priority := Lib.Config.Email.Priority;
    ReadingConfirmation := Lib.Config.Email.Confirmacao;
    DeliveryConfirmation := Lib.Config.Email.ConfirmacaoEntrega;
    TimeOut := Lib.Config.Email.TimeOut;
    Username := Lib.Config.Email.Usuario;
    UseThread := Lib.Config.Email.SegundoPlano;
  end;
end;

procedure TLibNFeDM.AplicarConfigPosPrinter;
var
  LibConfig: TLibNFeConfig;
begin
  if Assigned(FLibPosPrinter) then
  begin
    FLibPosPrinter.ConfigLer(Lib.Config.NomeArquivo);
    Exit;
  end;

  LibConfig := TLibNFeConfig(Lib.Config);

  with FACBrPosPrinter do
  begin
    ArqLog := LibConfig.PosPrinter.ArqLog;
    Modelo := TACBrPosPrinterModelo(LibConfig.PosPrinter.Modelo);
    Porta := LibConfig.PosPrinter.Porta;
    PaginaDeCodigo := TACBrPosPaginaCodigo(LibConfig.PosPrinter.PaginaDeCodigo);
    ColunasFonteNormal := LibConfig.PosPrinter.ColunasFonteNormal;
    EspacoEntreLinhas := LibConfig.PosPrinter.EspacoEntreLinhas;
    LinhasEntreCupons := LibConfig.PosPrinter.LinhasEntreCupons;
    CortaPapel := LibConfig.PosPrinter.CortaPapel;
    TraduzirTags := LibConfig.PosPrinter.TraduzirTags;
    IgnorarTags := LibConfig.PosPrinter.IgnorarTags;
    LinhasBuffer := LibConfig.PosPrinter.LinhasBuffer;
    ControlePorta := LibConfig.PosPrinter.ControlePorta;
    VerificarImpressora := LibConfig.PosPrinter.VerificarImpressora;

    ConfigBarras.MostrarCodigo := LibConfig.PosPrinter.BcMostrarCodigo;
    ConfigBarras.LarguraLinha := LibConfig.PosPrinter.BcLarguraLinha;
    ConfigBarras.Altura := LibConfig.PosPrinter.BcAltura;
    ConfigBarras.Margem := LibConfig.PosPrinter.BcMargem;

    ConfigQRCode.Tipo := LibConfig.PosPrinter.QrTipo;
    ConfigQRCode.LarguraModulo := LibConfig.PosPrinter.QrLarguraModulo;
    ConfigQRCode.ErrorLevel := LibConfig.PosPrinter.QrErrorLevel;

    ConfigLogo.IgnorarLogo := LibConfig.PosPrinter.LgIgnorarLogo;
    ConfigLogo.KeyCode1 := LibConfig.PosPrinter.LgKeyCode1;
    ConfigLogo.KeyCode2 := LibConfig.PosPrinter.LgKeyCode2;
    ConfigLogo.FatorX := LibConfig.PosPrinter.LgFatorX;
    ConfigLogo.FatorY := LibConfig.PosPrinter.LgFatorY;

    ConfigGaveta.SinalInvertido := LibConfig.PosPrinter.GvSinalInvertido;
    ConfigGaveta.TempoON := LibConfig.PosPrinter.GvTempoON;
    ConfigGaveta.TempoOFF := LibConfig.PosPrinter.GvTempoOFF;

    ConfigModoPagina.Largura := LibConfig.PosPrinter.MpLargura;
    ConfigModoPagina.Altura := LibConfig.PosPrinter.MpAltura;
    ConfigModoPagina.Esquerda := LibConfig.PosPrinter.MpEsquerda;
    ConfigModoPagina.Topo := LibConfig.PosPrinter.MpTopo;
    ConfigModoPagina.Direcao := TACBrPosDirecao(LibConfig.PosPrinter.MpDirecao);
    ConfigModoPagina.EspacoEntreLinhas := LibConfig.PosPrinter.MpEspacoEntreLinhas;

    Device.Baud := LibConfig.PosDeviceConfig.Baud;
    Device.Data := LibConfig.PosDeviceConfig.Data;
    Device.TimeOut := LibConfig.PosDeviceConfig.TimeOut;
    Device.Parity := TACBrSerialParity(LibConfig.PosDeviceConfig.Parity);
    Device.Stop := TACBrSerialStop(LibConfig.PosDeviceConfig.Stop);
    Device.MaxBandwidth := LibConfig.PosDeviceConfig.MaxBandwidth;
    Device.SendBytesCount := LibConfig.PosDeviceConfig.SendBytesCount;
    Device.SendBytesInterval := LibConfig.PosDeviceConfig.SendBytesInterval;
    Device.HandShake := TACBrHandShake(LibConfig.PosDeviceConfig.HandShake);
    Device.HardFlow := LibConfig.PosDeviceConfig.HardFlow;
    Device.SoftFlow := LibConfig.PosDeviceConfig.SoftFlow;
  end;
end;

procedure TLibNFeDM.ConfigurarImpressao(NomeImpressora: String; GerarPDF: Boolean = False;
  Protocolo: String = ''; MostrarPreview: String = ''; MarcaDagua: String = '';
  ViaConsumidor: String = ''; Simplificado: String = '');
var
  LibConfig: TLibNFeConfig;
begin
  LibConfig := TLibNFeConfig(Lib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  ACBrNFe1.DANFE := ACBrNFeDANFeRL1;

  if ACBrNFe1.NotasFiscais.Count > 0 then
  begin
    if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
    begin
      if (LibConfig.DANFe.NFCe.TipoRelatorioBobina = tpFortes) then
        ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1
      else if (LibConfig.DANFe.NFCe.TipoRelatorioBobina = tpFortesA4) then
        ACBrNFe1.DANFE := ACBrNFeDANFCeFortesA4
      else
        ACBrNFe1.DANFE := ACBrNFeDANFeESCPOS1;

      if GerarPDF and not (LibConfig.DANFe.NFCe.TipoRelatorioBobina in [tpFortes, tpFortesA4]) then
        ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1;
    end;

    if (ACBrNFe1.NotasFiscais.Items[0].NFe.procNFe.cStat in [101, 151, 155]) then
      ACBrNFe1.DANFE.Cancelada := True
    else
      ACBrNFe1.DANFE.Cancelada := False;
  end;

  if GerarPDF then
  begin
    if (LibConfig.DANFe.PathPDF <> '') then
      if not DirectoryExists(PathWithDelim(LibConfig.DANFe.PathPDF))then
        ForceDirectories(PathWithDelim(LibConfig.DANFe.PathPDF));
  end;

  LibConfig.DANFe.Apply(ACBrNFe1.DANFE, Lib);

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
  if Assigned(Lib) then
    Lib.GravarLog(AMsg, NivelLog, Traduzir);
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
