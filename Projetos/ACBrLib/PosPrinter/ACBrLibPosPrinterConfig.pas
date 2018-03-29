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

  { TDeviceConfig }
  TDeviceConfig = class
  private
    FBaud: Integer;
    FData: Integer;
    FParity: TACBrSerialParity;
    FStop: TACBrSerialStop;
    FMaxBandwidth: Integer;
    FSendBytesCount: Integer;
    FSendBytesInterval: Integer;
    FHandShake: TACBrHandShake;
    FSoftFlow: Boolean;
    FHardFlow: Boolean;

  public
    constructor Create;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Baud: Integer read FBaud write FBaud;
    property Data: Integer read FData write FData;
    property Parity: TACBrSerialParity read FParity write FParity;
    property Stop: TACBrSerialStop read FStop write FStop;
    property MaxBandwidth: Integer read  FMaxBandwidth write FMaxBandwidth;
    property SendBytesCount: Integer read  FSendBytesCount write FSendBytesCount;
    property SendBytesInterval: Integer read  FSendBytesInterval write FSendBytesInterval;
    property HandShake: TACBrHandShake read FHandShake write FHandShake;
    property SoftFlow: Boolean read FSoftFlow write FSoftFlow;
    property HardFlow: Boolean read FHardFlow write FHardFlow;

  end;

  { TPosPrinterConfig }
  TPosPrinterConfig = class
  private
    FModelo: TACBrPosPrinterModelo;
    FPorta: String;
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

    property Modelo: TACBrPosPrinterModelo read FModelo write FModelo;
    property Porta: String read FPorta write FPorta;
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
    FDevice: TDeviceConfig;

  protected
    function AtualizarArquivoConfiguracao: Boolean; override;
    procedure AplicarConfiguracoes; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    procedure Ler; override;
    procedure Gravar; override;

    property PosPrinterConfig: TPosPrinterConfig read FPosPrinterConfig;
    property DeviceConfig: TDeviceConfig read FDevice;
  end;

implementation

uses
  ACBrLibPosPrinterClass, ACBrLibPosPrinterConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TDeviceConfig }
constructor TDeviceConfig.Create;
begin
  FBaud := 9600;
  FData := 8;
  FParity := pNone;
  FStop := s1;
  FMaxBandwidth := 0;
  FSendBytesCount := 0;
  FSendBytesInterval := 0;
  FHandShake := hsNenhum;
  FSoftFlow := False;
  FHardFlow := False;

end;

procedure TDeviceConfig.LerIni(const AIni: TCustomIniFile);
begin
  FBaud := AIni.ReadInteger(CSessaoDevice, CChaveDVBaud, FBaud);
  FData := AIni.ReadInteger(CSessaoDevice, CChaveDVData, FData);
  FParity := TACBrSerialParity(AIni.ReadInteger(CSessaoDevice, CChaveDVParity, Integer(FParity)));
  FStop := TACBrSerialStop(AIni.ReadInteger(CSessaoDevice, CChaveDVStop, Integer(FStop)));
  FMaxBandwidth := AIni.ReadInteger(CSessaoDevice, CChaveDVMaxBandwidth, FMaxBandwidth);
  FSendBytesCount := AIni.ReadInteger(CSessaoDevice, CChaveDVSendBytesCount, FSendBytesCount);
  FSendBytesInterval := AIni.ReadInteger(CSessaoDevice, CChaveDVSendBytesInterval, FSendBytesInterval);
  FHandShake := TACBrHandShake(AIni.ReadInteger(CSessaoDevice, CChaveDVHandShake, Integer(FHandShake)));
  FSoftFlow := AIni.ReadBool(CSessaoDevice, CChaveDVSoftFlow, FSoftFlow);
  FHardFlow := AIni.ReadBool(CSessaoDevice, CChaveDVHardFlow, FHardFlow);

end;

procedure TDeviceConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoDevice, CChaveDVBaud, FBaud);
  AIni.WriteInteger(CSessaoDevice, CChaveDVData, FData);
  AIni.WriteInteger(CSessaoDevice, CChaveDVParity, Integer(FParity));
  AIni.WriteInteger(CSessaoDevice, CChaveDVStop, Integer(FStop));
  AIni.WriteInteger(CSessaoDevice, CChaveDVMaxBandwidth, FMaxBandwidth);
  AIni.WriteInteger(CSessaoDevice, CChaveDVSendBytesCount, FSendBytesCount);
  AIni.WriteInteger(CSessaoDevice, CChaveDVSendBytesInterval, FSendBytesInterval);
  AIni.WriteInteger(CSessaoDevice, CChaveDVHandShake, Integer(FHandShake));
  AIni.WriteBool(CSessaoDevice, CChaveDVSoftFlow, FSoftFlow);
  AIni.WriteBool(CSessaoDevice, CChaveDVHardFlow, FHardFlow);
end;

{ TPosPrinterConfig }
constructor TPosPrinterConfig.Create;
begin
  FModelo := ppTexto;
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
  FModelo := TACBrPosPrinterModelo(AIni.ReadInteger(CSessaoPosPrinter, CChaveModelo, Integer(FModelo)));
  FPorta := AIni.ReadString(CSessaoPosPrinter, CChavePorta, FPorta);
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

  FConfigBarras.MostrarCodigo :=  AIni.ReadBool(CSessaoConfigBarras, CChaveCBMostrarCodigo, FConfigBarras.MostrarCodigo);
  FConfigBarras.LarguraLinha :=  AIni.ReadInteger(CSessaoConfigBarras, CChaveCBLarguraLinha, FConfigBarras.LarguraLinha);
  FConfigBarras.Altura :=  AIni.ReadInteger(CSessaoConfigBarras, CChaveCBAltura, FConfigBarras.Altura);
  FConfigBarras.Margem :=  AIni.ReadInteger(CSessaoConfigBarras, CChaveCBMargem, FConfigBarras.Margem);

  FConfigQRCode.Tipo :=  AIni.ReadInteger(CSessaoConfigQRCode, CChaveQRTipo, FConfigQRCode.Tipo);
  FConfigQRCode.LarguraModulo :=  AIni.ReadInteger(CSessaoConfigQRCode, CChaveQRLarguraModulo, FConfigQRCode.LarguraModulo);
  FConfigQRCode.ErrorLevel :=  AIni.ReadInteger(CSessaoConfigQRCode, CChaveQRErrorLevel, FConfigQRCode.ErrorLevel);

  FConfigGaveta.SinalInvertido :=  AIni.ReadBool(CSessaoConfigGaveta, CChaveGVSinalInvertido, FConfigGaveta.SinalInvertido);
  FConfigGaveta.TempoON :=  AIni.ReadInteger(CSessaoConfigGaveta, CChaveGVTempoON, FConfigGaveta.TempoON);
  FConfigGaveta.TempoOFF :=  AIni.ReadInteger(CSessaoConfigGaveta, CChaveGVTempoOFF, FConfigGaveta.TempoOFF);
end;

procedure TPosPrinterConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoPosPrinter, CChaveModelo, Integer(FModelo));
  AIni.WriteString(CSessaoPosPrinter, CChavePorta, FPorta);
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

  AIni.WriteBool(CSessaoConfigBarras, CChaveCBMostrarCodigo, FConfigBarras.MostrarCodigo);
  AIni.WriteInteger(CSessaoConfigBarras, CChaveCBLarguraLinha, FConfigBarras.LarguraLinha);
  AIni.WriteInteger(CSessaoConfigBarras, CChaveCBAltura, FConfigBarras.Altura);
  AIni.WriteInteger(CSessaoConfigBarras, CChaveCBMargem, FConfigBarras.Margem);

  AIni.WriteInteger(CSessaoConfigQRCode, CChaveQRTipo, FConfigQRCode.Tipo);
  AIni.WriteInteger(CSessaoConfigQRCode, CChaveQRLarguraModulo, FConfigQRCode.LarguraModulo);
  AIni.WriteInteger(CSessaoConfigQRCode, CChaveQRErrorLevel, FConfigQRCode.ErrorLevel);

  AIni.WriteBool(CSessaoConfigGaveta, CChaveGVSinalInvertido, FConfigGaveta.SinalInvertido);
  AIni.WriteInteger(CSessaoConfigGaveta, CChaveGVTempoON, FConfigGaveta.TempoON);
  AIni.WriteInteger(CSessaoConfigGaveta, CChaveGVTempoOFF, FConfigGaveta.TempoOFF);
end;

{ TLibPosPrinterConfig }

constructor TLibPosPrinterConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FPosPrinterConfig := TPosPrinterConfig.Create;
  FDevice := TDeviceConfig.Create;
end;

destructor TLibPosPrinterConfig.Destroy;
begin
  FPosPrinterConfig.Free;
  FDevice.Free;

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

procedure TLibPosPrinterConfig.AplicarConfiguracoes;
begin
  inherited AplicarConfiguracoes;

  if Assigned(Owner) then
    TACBrLibPosPrinter(Owner).PosDM.Travar;

  try
    inherited AplicarConfiguracoes;

    TACBrLibPosPrinter(Owner).PosDM.AplicarConfiguracoes;

    TACBrLibPosPrinter(Owner).GravarLog(ClassName + '.AplicarConfiguracoes - Feito', logParanoico);
  finally
    if Assigned(Owner) then
      TACBrLibPosPrinter(Owner).PosDM.Destravar;
  end;
end;

procedure TLibPosPrinterConfig.Ler;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibPosPrinter(Owner) do
    begin
      GravarLog(ClassName + '.Ler: ' + NomeArquivo, logCompleto);
      PosDM.Travar;
    end;
  end;

  try
    inherited Ler;

    FPosPrinterConfig.LerIni(Ini);
    FDevice.LerIni(Ini);

    AplicarConfiguracoes;
  finally
    // Ajustes pos leitura das configurações //
    if Assigned(Owner) then
    begin
      with TACBrLibPosPrinter(Owner) do
      begin
        PosDM.Destravar;
        GravarLog(ClassName + '.Ler - Feito', logParanoico);
      end;
    end;
  end;
end;

procedure TLibPosPrinterConfig.Gravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibPosPrinter(Owner) do
    begin
      GravarLog(ClassName + '.Gravar: ' + NomeArquivo, logCompleto);
      PosDM.Travar;
    end;
  end;

  try
    inherited Gravar;

    Ini.WriteString(CSessaoVersao, CLibPosPrinterNome, CLibPosPrinterVersao);

    FPosPrinterConfig.GravarIni(Ini);
    FDevice.GravarIni(Ini);

    Ini.UpdateFile;
  finally
    if Assigned(Owner) then
    begin
      with TACBrLibPosPrinter(Owner) do
      begin
        PosDM.Destravar;
        GravarLog(ClassName + '.Gravar - Feito', logParanoico);
      end;
    end;
  end;
end;

end.

