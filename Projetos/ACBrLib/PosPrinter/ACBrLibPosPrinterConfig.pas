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

{$I ACBr.inc}

unit ACBrLibPosPrinterConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrLibConfig, ACBrPosPrinter, ACBrDevice;

type

  { TPosPrinterConfig }
  TPosPrinterConfig = class
  private
    FArqLog: String;
    FDeviceParams: String;
    FModelo: TACBrPosPrinterModelo;
    FPorta: String;
    FTimeOut: Integer;
    FPaginaDeCodigo: TACBrPosPaginaCodigo;
    FColunasFonteNormal: Integer;
    FEspacoEntreLinhas: byte;
    FConfigBarras: TACBrECFConfigBarras;
    FConfigQRCode: TACBrConfigQRCode;
    FConfigLogo: TACBrConfigLogo;
    FConfigGaveta: TACBrConfigGaveta;
    FLinhasEntreCupons: Integer;
    FCortaPapel: Boolean;
    FTraduzirTags: Boolean;
    FIgnorarTags: Boolean;
    FLinhasBuffer: Integer;
    FControlePorta: Boolean;
    FVerificarImpressora: Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ArqLog: string read FArqLog write FArqLog;
    property DeviceParams: String read FDeviceParams write FDeviceParams;
    property Modelo: TACBrPosPrinterModelo read FModelo write FModelo;
    property Porta: String read FPorta write FPorta;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property PaginaDeCodigo: TACBrPosPaginaCodigo read FPaginaDeCodigo write FPaginaDeCodigo;
    property ColunasFonteNormal: Integer read FColunasFonteNormal write FColunasFonteNormal;
    property EspacoEntreLinhas: byte read FEspacoEntreLinhas write FEspacoEntreLinhas;
    property ConfigBarras: TACBrECFConfigBarras read FConfigBarras;
    property ConfigQRCode: TACBrConfigQRCode read FConfigQRCode;
    property ConfigLogo: TACBrConfigLogo read FConfigLogo;
    property ConfigGaveta: TACBrConfigGaveta read FConfigGaveta;
    property LinhasEntreCupons: Integer read FLinhasEntreCupons write FLinhasEntreCupons;
    property CortaPapel: Boolean read FCortaPapel write FCortaPapel;
    property TraduzirTags: Boolean read FTraduzirTags write FTraduzirTags;
    property IgnorarTags: Boolean read FIgnorarTags write FIgnorarTags;
    property LinhasBuffer: Integer read FLinhasBuffer write FLinhasBuffer;
    property ControlePorta: Boolean read FControlePorta write FControlePorta;
    property VerificarImpressora: Boolean read FVerificarImpressora write FVerificarImpressora;

  end;

  { TLibPosPrinterConfig }
  TLibPosPrinterConfig = class(TLibConfig)
  private
    FPosPrinterConfig: TPosPrinterConfig;

  protected
    function AtualizarArquivoConfiguracao: Boolean; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property PosPrinterConfig: TPosPrinterConfig read FPosPrinterConfig;
  end;

implementation

uses
  ACBrLibPosPrinterClass, ACBrLibPosPrinterConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TPosPrinterConfig }
constructor TPosPrinterConfig.Create;
begin
  FModelo := ppTexto;
  FDeviceParams := '';
  FPorta := '';
  FTimeOut := 3;
  FPaginaDeCodigo := pc850;
  FColunasFonteNormal := 48;
  FEspacoEntreLinhas := 0;
  FConfigBarras := TACBrECFConfigBarras.Create;
  FConfigQRCode := TACBrConfigQRCode.Create;
  FConfigLogo := TACBrConfigLogo.Create;
  FConfigGaveta := TACBrConfigGaveta.Create;
  FLinhasEntreCupons := 21;
  FCortaPapel := True;
  FTraduzirTags := True;
  FIgnorarTags := False;
  FLinhasBuffer := 0;
  FControlePorta := False;
  FVerificarImpressora := False;

end;

destructor TPosPrinterConfig.Destroy;
begin
  FConfigBarras.Free;
  FConfigQRCode.Free;
  FConfigLogo.Free;
  FConfigGaveta.Free;

  inherited Destroy;
end;

procedure TPosPrinterConfig.LerIni(const AIni: TCustomIniFile);
begin
  FArqLog := AIni.ReadString(CSessaoPosPrinter, CChaveLog, FArqLog);
  FModelo := TACBrPosPrinterModelo(AIni.ReadInteger(CSessaoPosPrinter, CChaveModelo, Integer(FModelo)));
  FDeviceParams := AIni.ReadString(CSessaoPosPrinter, CChaveDevice, FDeviceParams);
  FPorta := AIni.ReadString(CSessaoPosPrinter, CChavePorta, FPorta);
  FTimeOut := AIni.ReadInteger(CSessaoPosPrinter, CChaveTimeOut, FTimeOut);
  FPaginaDeCodigo := TACBrPosPaginaCodigo(AIni.ReadInteger(CSessaoPosPrinter, CChavePaginaDeCodigo, Integer(FPaginaDeCodigo)));
  FColunasFonteNormal := AIni.ReadInteger(CSessaoPosPrinter, CChaveColunasFonteNormal, FColunasFonteNormal);
  FEspacoEntreLinhas :=  AIni.ReadInteger(CSessaoPosPrinter, CChaveEspacoEntreLinhas, FEspacoEntreLinhas);
  FLinhasEntreCupons :=  AIni.ReadInteger(CSessaoPosPrinter, CChaveFLinhasEntreCupons, FLinhasEntreCupons);
  FCortaPapel :=  AIni.ReadBool(CSessaoPosPrinter, CChaveCortaPapel, FCortaPapel);
  FTraduzirTags :=  AIni.ReadBool(CSessaoPosPrinter, CChaveTraduzirTags, FTraduzirTags);
  FIgnorarTags :=  AIni.ReadBool(CSessaoPosPrinter, CChaveIgnorarTags, FIgnorarTags);
  FLinhasBuffer :=  AIni.ReadInteger(CSessaoPosPrinter, CChaveLinhasBuffer, FLinhasBuffer);
  FControlePorta :=  AIni.ReadBool(CSessaoPosPrinter, CChaveControlePorta, FControlePorta);
  FVerificarImpressora :=  AIni.ReadBool(CSessaoPosPrinter, CChaveVerificarImpressora, FVerificarImpressora);

  FConfigBarras.MostrarCodigo :=  AIni.ReadBool(CSessaoPosPrinterBarras, CChaveCBMostrarCodigo, FConfigBarras.MostrarCodigo);
  FConfigBarras.LarguraLinha :=  AIni.ReadInteger(CSessaoPosPrinterBarras, CChaveCBLarguraLinha, FConfigBarras.LarguraLinha);
  FConfigBarras.Altura :=  AIni.ReadInteger(CSessaoPosPrinterBarras, CChaveCBAltura, FConfigBarras.Altura);
  FConfigBarras.Margem :=  AIni.ReadInteger(CSessaoPosPrinterBarras, CChaveCBMargem, FConfigBarras.Margem);

  FConfigQRCode.Tipo :=  AIni.ReadInteger(CSessaoPosPrinterQRCode, CChaveQRTipo, FConfigQRCode.Tipo);
  FConfigQRCode.LarguraModulo :=  AIni.ReadInteger(CSessaoPosPrinterQRCode, CChaveQRLarguraModulo, FConfigQRCode.LarguraModulo);
  FConfigQRCode.ErrorLevel :=  AIni.ReadInteger(CSessaoPosPrinterQRCode, CChaveQRErrorLevel, FConfigQRCode.ErrorLevel);

  FConfigLogo.IgnorarLogo := AIni.ReadBool(CSessaoPosPrinterLogo, CChaveLGIgnorarLogo, FConfigLogo.IgnorarLogo);
  FConfigLogo.KeyCode1 := AIni.ReadInteger(CSessaoPosPrinterLogo, CChaveLGKeyCode1, FConfigLogo.KeyCode1);
  FConfigLogo.KeyCode2 := AIni.ReadInteger(CSessaoPosPrinterLogo, CChaveLGKeyCode2, FConfigLogo.KeyCode2);
  FConfigLogo.FatorX := AIni.ReadInteger(CSessaoPosPrinterLogo, CChaveLGFatorX, FConfigLogo.FatorX);
  FConfigLogo.FatorY := AIni.ReadInteger(CSessaoPosPrinterLogo, CChaveLGFatorY, FConfigLogo.FatorY);

  FConfigGaveta.SinalInvertido :=  AIni.ReadBool(CSessaoPosPrinterGaveta, CChaveGVSinalInvertido, FConfigGaveta.SinalInvertido);
  FConfigGaveta.TempoON :=  AIni.ReadInteger(CSessaoPosPrinterGaveta, CChaveGVTempoON, FConfigGaveta.TempoON);
  FConfigGaveta.TempoOFF :=  AIni.ReadInteger(CSessaoPosPrinterGaveta, CChaveGVTempoOFF, FConfigGaveta.TempoOFF);
end;

procedure TPosPrinterConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPosPrinter, CChaveLog, FArqLog);
  AIni.WriteInteger(CSessaoPosPrinter, CChaveModelo, Integer(FModelo));
  AIni.WriteString(CSessaoPosPrinter, CChaveDevice, FDeviceParams);
  AIni.WriteString(CSessaoPosPrinter, CChavePorta, FPorta);
  AIni.WriteInteger(CSessaoPosPrinter, CChaveTimeOut, FTimeOut);
  AIni.WriteInteger(CSessaoPosPrinter, CChavePaginaDeCodigo, Integer(FPaginaDeCodigo));
  AIni.WriteInteger(CSessaoPosPrinter, CChaveColunasFonteNormal, FColunasFonteNormal);
  AIni.WriteInteger(CSessaoPosPrinter, CChaveEspacoEntreLinhas, FEspacoEntreLinhas);
  AIni.WriteInteger(CSessaoPosPrinter, CChaveFLinhasEntreCupons, FLinhasEntreCupons);
  AIni.WriteBool(CSessaoPosPrinter, CChaveCortaPapel, FCortaPapel);
  AIni.WriteBool(CSessaoPosPrinter, CChaveTraduzirTags, FTraduzirTags);
  AIni.WriteBool(CSessaoPosPrinter, CChaveIgnorarTags, FIgnorarTags);
  AIni.WriteInteger(CSessaoPosPrinter, CChaveLinhasBuffer, FLinhasBuffer);
  AIni.WriteBool(CSessaoPosPrinter, CChaveControlePorta, FControlePorta);
  AIni.WriteBool(CSessaoPosPrinter, CChaveVerificarImpressora, FVerificarImpressora);

  AIni.WriteBool(CSessaoPosPrinterBarras, CChaveCBMostrarCodigo, FConfigBarras.MostrarCodigo);
  AIni.WriteInteger(CSessaoPosPrinterBarras, CChaveCBLarguraLinha, FConfigBarras.LarguraLinha);
  AIni.WriteInteger(CSessaoPosPrinterBarras, CChaveCBAltura, FConfigBarras.Altura);
  AIni.WriteInteger(CSessaoPosPrinterBarras, CChaveCBMargem, FConfigBarras.Margem);

  AIni.WriteInteger(CSessaoPosPrinterQRCode, CChaveQRTipo, FConfigQRCode.Tipo);
  AIni.WriteInteger(CSessaoPosPrinterQRCode, CChaveQRLarguraModulo, FConfigQRCode.LarguraModulo);
  AIni.WriteInteger(CSessaoPosPrinterQRCode, CChaveQRErrorLevel, FConfigQRCode.ErrorLevel);

  AIni.WriteBool(CSessaoPosPrinterLogo, CChaveLGIgnorarLogo, FConfigLogo.IgnorarLogo);
  AIni.WriteInteger(CSessaoPosPrinterLogo, CChaveLGKeyCode1, FConfigLogo.KeyCode1);
  AIni.WriteInteger(CSessaoPosPrinterLogo, CChaveLGKeyCode2, FConfigLogo.KeyCode2);
  AIni.WriteInteger(CSessaoPosPrinterLogo, CChaveLGFatorX, FConfigLogo.FatorX);
  AIni.WriteInteger(CSessaoPosPrinterLogo, CChaveLGFatorY, FConfigLogo.FatorY);

  AIni.WriteBool(CSessaoPosPrinterGaveta, CChaveGVSinalInvertido, FConfigGaveta.SinalInvertido);
  AIni.WriteInteger(CSessaoPosPrinterGaveta, CChaveGVTempoON, FConfigGaveta.TempoON);
  AIni.WriteInteger(CSessaoPosPrinterGaveta, CChaveGVTempoOFF, FConfigGaveta.TempoOFF);
end;

{ TLibPosPrinterConfig }

constructor TLibPosPrinterConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FPosPrinterConfig := TPosPrinterConfig.Create;
end;

destructor TLibPosPrinterConfig.Destroy;
begin
  FPosPrinterConfig.Free;

  inherited Destroy;
end;

function TLibPosPrinterConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibPosPrinterNome, '0');
  Result := (CompareVersions(CLibPosPrinterVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibPosPrinterConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FPosPrinterConfig.LerIni(Ini);
end;

procedure TLibPosPrinterConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibPosPrinterNome, CLibPosPrinterVersao);

  FPosPrinterConfig.GravarIni(Ini);
end;

procedure TLibPosPrinterConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibPosPrinter(Owner).PosDM.AplicarConfiguracoes;
end;

procedure TLibPosPrinterConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibPosPrinter(Owner) do
      PosDM.Travar;
  end;
end;

procedure TLibPosPrinterConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibPosPrinter(Owner) do
      PosDM.Destravar;
  end;
end;

end.

