{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }
{******************************************************************************}

unit ACBrLibBPeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrBPe, ACBrMail, ACBrPosPrinter, ACBrBPeDABPeESCPOS,
  ACBrLibConfig, ACBrLibMailImport, ACBrLibPosPrinterImport;

type

  { TLibBPeDM }

  TLibBPeDM = class(TDataModule)
    ACBrBPe1: TACBrBPe;
    ACBrBPeDABPeESCPOS1: TACBrBPeDABPeESCPOS;

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
    procedure ConfigurarImpressao;
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;
  end;

implementation

uses
  ACBrUtil, FileUtil,
  ACBrLibBPeConfig, ACBrDeviceConfig, ACBrLibComum,
  ACBrLibConsts, ACBrLibBPeClass;

{$R *.lfm}

{ TLibBPeDM }

procedure TLibBPeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  FACBrMail := Nil;
  FLibMail := Nil;
  FACBrPosPrinter := Nil;
  FLibPosPrinter := Nil;
end;

procedure TLibBPeDM.DataModuleDestroy(Sender: TObject);
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

procedure TLibBPeDM.CriarACBrMail;
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

  ACBrBPe1.MAIL := FACBrMail;
end;

procedure TLibBPeDM.CriarACBrPosPrinter;
var
  NomeLib: String;
begin
  if Assigned(FLibPosPrinter) or Assigned(FACBrPosPrinter) then
    Exit;

  GravarLog('  CriarACBrPosPrinter', logCompleto);

  NomeLib := ApplicationPath + CACBrPosPrinterLIBName;
  if FileExists(NomeLib) then
  begin
    GravarLog('      Carregando PosPrinter de: '+NomeLib, logCompleto);
    // Criando Classe para Leitura da Lib //
    FLibPosPrinter  := TACBrLibPosPrinter.Create(NomeLib, pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FACBrPosPrinter := FLibPosPrinter.ACBrPosPrinter;
  end
  else
  begin
    GravarLog('     Criando PosPrinter Interno', logCompleto);
    FACBrPosPrinter := TACBrPosPrinter.Create(Nil);
    TLibBPeConfig(pLib.Config).PosDeviceConfig := TDeviceConfig.Create(CSessaoPosPrinterDevice);
  end;

  ACBrBPeDABPeESCPOS1.PosPrinter := FACBrPosPrinter;
end;

procedure TLibBPeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibBPeConfig;
begin
  ACBrBPe1.SSL.DescarregarCertificado;
  pLibConfig := TLibBPeConfig(pLib.Config);
  ACBrBPe1.Configuracoes.Assign(pLibConfig.BPeConfig);

  AplicarConfigMail;
  AplicarConfigPosPrinter;
end;

procedure TLibBPeDM.AplicarConfigMail;
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

procedure TLibBPeDM.AplicarConfigPosPrinter;
Var
  pLibConfig: TLibBPeConfig;
begin
  if Assigned(FLibPosPrinter) then
  begin
    FLibPosPrinter.ConfigLer(pLib.Config.NomeArquivo);
    Exit;
  end;

  pLibConfig := TLibBPeConfig(pLib.Config);

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

    pLibConfig.PosDeviceConfig.Apply(Device);
  end;
end;

procedure TLibBPeDM.ConfigurarImpressao;
var
  pLibConfig: TLibBPeConfig;
begin
  pLibConfig := TLibBPeConfig(pLib.Config);

  if ACBrBPe1.Bilhetes.Count > 0 then
  begin
    ACBrBPe1.DABPE := ACBrBPeDABPeESCPOS1;

    pLibConfig.DABPeConfig.Assign(ACBrBPe1.DABPE);
  end;

  if ACBrBPe1.DABPE = ACBrBPeDABPeESCPOS1 then
  begin
    if not ACBrBPeDABPeESCPOS1.PosPrinter.ControlePorta then
    begin
      ACBrBPeDABPeESCPOS1.PosPrinter.Ativar;
      if not ACBrBPeDABPeESCPOS1.PosPrinter.Device.Ativo then
        ACBrBPeDABPeESCPOS1.PosPrinter.Device.Ativar;
    end;
  end;
end;

procedure TLibBPeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(pLib) then
    pLib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibBPeDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibBPeDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.
