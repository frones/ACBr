{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

{$I ACBr.inc}

unit ACBrLibPIXCDConfig;

interface

uses
  Classes, SysUtils, IniFiles, synachar,
  ACBrBase, ACBrLibConfig, ACBrPIXCD, ACBrPIXPSPBancoDoBrasil, ACBrPIXBase, ACBrLibPIXCDDataModule;

type

  { TPIXCDMercadoPagoConfig }
  TPIXCDMercadoPagoConfig = class
    FChavePIX: String;
    FAccessToken: String;

    public
    Constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property AccessToken: String read FAccessToken write FAccessToken;
  end;

  { TPIXCDCieloConfig }
  TPIXCDCieloConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;

    public
    Constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
  end;

  { TPIXCDMateraConfig }
  TPIXCDMateraConfig = class
    FChavePIX: String;
    FClientID: String;
    FSecretKey: String;
    FClientSecret: String;
    FArqCertificado: String;
    FArqChavePrivada: String;
    FAccountID: String;
    FMediatorFee: Currency;

    public
    Constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property SecretKey: String read FSecretKey write FSecretKey;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property AccountID: String read FAccountID write FAccountID;
    property MediatorFee: Currency read FMediatorFee write FMediatorFee;
  end;


  { TPIXCDAilosConfig }
  TPIXCDAilosConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FArqChavePrivada: String;
    FArqCertificado: String;
    FArqCertificadoRoot: String;

    public
    Constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
    property ArqCertificadoRoot: String read FArqCertificadoRoot write FArqCertificadoRoot;
  end;

  { TPIXCDBancoDoBrasilConfig }
  TPIXCDBancoDoBrasilConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FDeveloperApplicationKey: String;
    FArqChavePrivada: String;
    FArqCertificado: String;
    FArqPFX: String;
    FSenhaPFX: AnsiString;
    FBBAPIVersao: TACBrBBAPIVersao;
    FAPIVersion: TACBrPIXAPIVersion;

    public
    Constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property DeveloperApplicationKey: String read FDeveloperApplicationKey write FDeveloperApplicationKey;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
    property ArqPFX: String read FArqPFX write FArqPFX;
    property SenhaPFX: AnsiString read FSenhaPFX write FSenhaPFX;
    property BBAPIVersao: TACBrBBAPIVersao read FBBAPIVersao write FBBAPIVersao;
    property APIVersion: TACBrPIXAPIVersion read FAPIVersion write FAPIVersion;
  end;

  { TPIXCDGerenciaNetConfig }
  TPIXCDGerenciaNetConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FArqPFX: String;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqPFX: String read FArqPFX write FArqPFX;
  end;

  { TPIXCDInterConfig }
  TPIXCDInterConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FArqChavePrivada: String;
    FArqCertificado: String;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
  end;

  { TPIXCDItauConfig }
  TPIXCDItauConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FArqChavePrivada: String;
    FArqCertificado: String;
    FAPIVersion: TACBrPIXAPIVersion;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
    property APIVersion: TACBrPIXAPIVersion read FAPIVersion write FAPIVersion;
  end;

  { TPIXCDPagSeguroConfig }
  TPIXCDPagSeguroConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FArqChavePrivada: String;
    FArqCertificado: String;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
  end;

  { TPIXCDPixPDVConfig }
  TPIXCDPixPDVConfig = class
    FCNPJ: String;
    FToken: String;
    FSecretKey: String;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property CNPJ: String read FCNPJ write FCNPJ;
    property Token: String read FToken write FToken;
    property SecretKey: String read FSecretKey write FSecretKey;
  end;

  { TPIXCDSantanderConfig }
  TPIXCDSantanderConfig = class
    FChavePIX: String;
    FConsumerKey: String;
    FConsumerSecret: String;
    FArqCertificadoPFX: String;
    FSenhaCertificadoPFX: AnsiString;
    FAPIVersion: TACBrPIXAPIVersion;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ConsumerKey: String read FConsumerKey write FConsumerKey;
    property ConsumerSecret: String read FConsumerSecret write FConsumerSecret;
    property ArqCertificadoPFX: String read FArqCertificadoPFX write FArqCertificadoPFX;
    property SenhaCertificadoPFX: AnsiString read FSenhaCertificadoPFX write FSenhaCertificadoPFX;
    property APIVersion: TACBrPIXAPIVersion read FAPIVersion write FAPIVersion;
  end;

  { TPIXCDShipayConfig }
  TPIXCDShipayConfig = class
    FClientID: String;
    FSecretKey: String;
    FAccessKey: String;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ClientID: String read FClientID write FClientID;
    property SecretKey: String read FSecretKey write FSecretKey;
    property AccessKey: String read FAccessKey write FAccessKey;
  end;

  { TPIXCDSiccobConfig }
  TPIXCDSiccobConfig = class
    FChavePIX: String;
    FClientID: String;
    FTokenSandbox: String;
    FArqChavePrivada: String;
    FArqCertificado: String;
    FAPIVersion: TACBrPIXAPIVersion;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property TokenSandbox: String read FTokenSandbox write FTokenSandbox;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
    property APIVersion: TACBrPIXAPIVersion read FAPIVersion write FAPIVersion;
  end;

  { TPIXCDSicrediConfig }
  TPIXCDSicrediConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FArqChavePrivada: String;
    FArqCertificado: String;
    FAPIVersion: TACBrPIXAPIVersion;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqChavePrivada: String read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: String read FArqCertificado write FArqCertificado;
    property APIVersion: TACBrPIXAPIVersion read FAPIVersion write FAPIVersion;
  end;

  {TPIXCDBradescoConfig}
  TPIXCDBradescoConfig = class
    FChavePIX: String;
    FClientID: String;
    FClientSecret: String;
    FArqPFX: String;
    FSenhaPFX: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: String read FChavePIX write FChavePIX;
    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property ArqPFX: String read FArqPFX write FArqPFX;
    property SenhaPFX: AnsiString read FSenhaPFX write FSenhaPFX;
  end;

  { TPIXCDConfig }
  TPIXCDConfig = class
    FAmbiente: TACBrPixCDAmbiente;
    FArqLOG: String;
    FDadosAutomacao: TACBrPixDadosAutomacao;
    FNivelLog: Byte;
    FProxy: TACBrHttpProxy;
    FTipoChave: TACBrPIXTipoChave;
    FPSP: TACBrPIXPSP;
    FQuandoGravarLog: TACBrGravarLog;
    FRecebedor: TACBrPixRecebedor;
    FTimeOut: Integer;

    public
    constructor Create;
    destructor Destroy; override;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Ambiente: TACBrPixCDAmbiente read FAmbiente write FAmbiente;
    property ArqLog: String read FArqLOG write FArqLOG;
    property DadosAutomacao: TACBrPixDadosAutomacao read FDadosAutomacao write FDadosAutomacao;
    property NivelLog: Byte read FNivelLog write FNivelLog;
    property Proxy: TACBrHttpProxy read FProxy write FProxy;
    property TipoChave: TACBrPIXTipoChave read FTipoChave write FTipoChave;
    property PSP: TACBrPIXPSP read FPSP write FPSP;
    property QuandoGravarLog: TACBrGravarLog read FQuandoGravarLog write FQuandoGravarLog;
    property Recebedor: TACBrPixRecebedor read FRecebedor write FRecebedor;
    property TimeOut: Integer read FTimeOut write FTimeOut;
  end;

  { TLibPIXCDConfig }
  TLibPIXCDConfig = class(TLibConfig)
    private
      FPIXCDConfig: TPIXCDConfig;
      FPIXCDBradescoConfig: TPIXCDBradescoConfig;
      FPIXCDSicrediConfig: TPIXCDSicrediConfig;
      FPIXCDSiccobConfig: TPIXCDSiccobConfig;
      FPIXCDShipayConfig: TPIXCDShipayConfig;
      FPIXCDSantanderConfig: TPIXCDSantanderConfig;
      FPIXCDPixPDVConfig: TPIXCDPixPDVConfig;
      FPIXCDPagSeguroConfig: TPIXCDPagSeguroConfig;
      FPIXCDItauConfig: TPIXCDItauConfig;
      FPIXCDInterConfig: TPIXCDInterConfig;
      FPIXCDGerenciaNetConfig: TPIXCDGerenciaNetConfig;
      FPIXCDBancoDoBrasilConfig: TPIXCDBancoDoBrasilConfig;
      FPIXCDAilosConfig: TPIXCDAilosConfig;
      FPIXCDMateraConfig: TPIXCDMateraConfig;
      FPIXCDCieloConfig: TPIXCDCieloConfig;
      FPIXCDMercadoPagoConfig: TPIXCDMercadoPagoConfig;

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

      property PIXCDConfig:        TPIXCDConfig read FPIXCDConfig;
      property PIXCDBradesco:      TPIXCDBradescoConfig read FPIXCDBradescoConfig;
      property PIXCDSicredi:       TPIXCDSicrediConfig read FPIXCDSicrediConfig;
      property PIXCDSiccob:        TPIXCDSiccobConfig read FPIXCDSiccobConfig;
      property PIXCDShipay:        TPIXCDShipayConfig read FPIXCDShipayConfig;
      property PIXCDSantander:     TPIXCDSantanderConfig read FPIXCDSantanderConfig;
      property PIXCDPixPDV:        TPIXCDPixPDVConfig read FPIXCDPixPDVConfig;
      property PIXCDPagSeguro:     TPIXCDPagSeguroConfig read FPIXCDPagSeguroConfig;
      property PIXCDItau:          TPIXCDItauConfig read FPIXCDItauConfig;
      property PIXCDInter:         TPIXCDInterConfig read FPIXCDInterConfig;
      property PIXCDGerenciaNet:   TPIXCDGerenciaNetConfig read FPIXCDGerenciaNetConfig;
      property PIXCDBancoDoBrasil: TPIXCDBancoDoBrasilConfig read FPIXCDBancoDoBrasilConfig;
      property PIXCDAilos:         TPIXCDAilosConfig read FPIXCDAilosConfig;
      property PIXCDMatera:        TPIXCDMateraConfig read FPIXCDMateraConfig;
      property PIXCDCielo:         TPIXCDCieloConfig read FPIXCDCieloConfig;
      property PIXCDMercadoPago:   TPIXCDMercadoPagoConfig read FPIXCDMercadoPagoConfig;
  end;

implementation

Uses
  ACBrLibPIXCDBase, ACBrLibPIXCDConsts, ACBrLibConsts, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TLibPIXCDConfig }
constructor TLibPIXCDConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FPIXCDConfig := TPIXCDConfig.Create;
  FPIXCDBradescoConfig := TPIXCDBradescoConfig.Create;
  FPIXCDSicrediConfig := TPIXCDSicrediConfig.Create;
  FPIXCDSiccobConfig := TPIXCDSiccobConfig.Create;
  FPIXCDShipayConfig := TPIXCDShipayConfig.Create;
  FPIXCDSantanderConfig := TPIXCDSantanderConfig.Create;
  FPIXCDPixPDVConfig := TPIXCDPixPDVConfig.Create;
  FPIXCDPagSeguroConfig := TPIXCDPagSeguroConfig.Create;
  FPIXCDItauConfig := TPIXCDItauConfig.Create;
  FPIXCDInterConfig := TPIXCDInterConfig.Create;
  FPIXCDGerenciaNetConfig := TPIXCDGerenciaNetConfig.Create;
  FPIXCDBancoDoBrasilConfig := TPIXCDBancoDoBrasilConfig.Create;
  FPIXCDAilosConfig := TPIXCDAilosConfig.Create;
  FPIXCDMateraConfig := TPIXCDMateraConfig.Create;
  FPIXCDCieloConfig := TPIXCDCieloConfig.Create;
  FPIXCDMercadoPagoConfig := TPIXCDMercadoPagoConfig.Create;
end;

destructor TLibPIXCDConfig.Destroy;
begin
  FPIXCDConfig.Free;
  FPIXCDBradescoConfig.Free;
  FPIXCDSicrediConfig.Free;
  FPIXCDSiccobConfig.Free;
  FPIXCDShipayConfig.Free;
  FPIXCDSantanderConfig.Free;
  FPIXCDPixPDVConfig.Free;
  FPIXCDPagSeguroConfig.Free;
  FPIXCDItauConfig.Free;
  FPIXCDInterConfig.Free;
  FPIXCDGerenciaNetConfig.Free;
  FPIXCDBancoDoBrasilConfig.Free;
  FPIXCDAilosConfig.Free;
  FPIXCDMateraConfig.Free;
  FPIXCDCieloConfig.Free;
  FPIXCDMercadoPagoConfig.Free;

  inherited Destroy;
end;

function TLibPIXCDConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibPIXCDNome, '0');
  Result := (CompareVersions(CLibPIXCDVersao, Versao) > 0 ) or
             (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibPIXCDConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FPIXCDConfig.LerIni(Ini);
  FPIXCDBradescoConfig.LerIni(Ini);
  FPIXCDSicrediConfig.LerIni(Ini);
  FPIXCDSiccobConfig.LerIni(Ini);
  FPIXCDShipayConfig.LerIni(Ini);
  FPIXCDSantanderConfig.LerIni(Ini);
  FPIXCDPixPDVConfig.LerIni(Ini);
  FPIXCDPagSeguroConfig.LerIni(Ini);
  FPIXCDItauConfig.LerIni(Ini);
  FPIXCDInterConfig.LerIni(Ini);
  FPIXCDGerenciaNetConfig.LerIni(Ini);
  FPIXCDBancoDoBrasilConfig.LerIni(Ini);
  FPIXCDAilosConfig.LerIni(Ini);
  FPIXCDMateraConfig.LerIni(Ini);
  FPIXCDCieloConfig.LerIni(Ini);
  FPIXCDMercadoPagoConfig.LerIni(Ini);
end;

procedure TLibPIXCDConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibPIXCDNome, CLibPIXCDVersao);

  FPIXCDConfig.GravarIni(Ini);
  FPIXCDBradescoConfig.GravarIni(Ini);
  FPIXCDSicrediConfig.GravarIni(Ini);
  FPIXCDSiccobConfig.GravarIni(Ini);
  FPIXCDShipayConfig.GravarIni(Ini);
  FPIXCDSantanderConfig.GravarIni(Ini);
  FPIXCDPixPDVConfig.GravarIni(Ini);
  FPIXCDPagSeguroConfig.GravarIni(Ini);
  FPIXCDItauConfig.GravarIni(Ini);
  FPIXCDInterConfig.GravarIni(Ini);
  FPIXCDGerenciaNetConfig.GravarIni(Ini);
  FPIXCDBancoDoBrasilConfig.GravarIni(Ini);
  FPIXCDAilosConfig.GravarIni(Ini);
  FPIXCDMateraConfig.GravarIni(Ini);
  FPIXCDCieloConfig.GravarIni(Ini);
  FPIXCDMercadoPagoConfig.GravarIni(Ini);
end;

procedure TLibPIXCDConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
  TACBrLibPIXCD(Owner).PIXCDDM.AplicarConfiguracoes;
end;

procedure TLibPIXCDConfig.Travar;
begin
  if Assigned(Owner) then
    begin
      with TACBrLibPIXCD(Owner) do
      PIXCDDM.Travar;
    end;
end;

procedure TLibPIXCDConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibPIXCD(Owner) do
    PIXCDDM.Destravar;
  end;
end;

{ TPIXCDConfig }
constructor TPIXCDConfig.Create;
begin
  inherited;
  FPSP := PSP;
  FAmbiente := ambTeste;
  FArqLOG := '';
  FDadosAutomacao := TACBrPixDadosAutomacao.Create;
  FNivelLog := 1;
  FProxy := TACBrHttpProxy.Create;
  FQuandoGravarLog := Nil;
  FRecebedor := TACBrPixRecebedor.Create;
  FTimeOut := ChttpTimeOutDef;
end;

destructor TPIXCDConfig.Destroy;
begin
  FRecebedor.Free;
  FDadosAutomacao.Free;
  FProxy.Free;

  inherited Destroy
end;

procedure TPIXCDConfig.LerIni(const AIni: TCustomIniFile);
begin
  Ambiente := TACBrPixCDAmbiente(AIni.ReadInteger(CSessaoPixCDConfig, CChaveAmbiente, integer(Ambiente)));
  ArqLog   := AIni.ReadString(CSessaoPixCDConfig, CChaveArqLogPixCD, ArqLog);
  NivelLog := AIni.ReadInteger(CSessaoPixCDConfig, CChaveNivelLog, NivelLog);
  TipoChave:= TACBrPIXTipoChave(AIni.ReadInteger(CSessaoPixCDConfig, CChaveTipoChave, Integer(TipoChave)));
  PSP := TACBrPIXPSP(AIni.ReadInteger(CSessaoPixCDConfig, CChavePSP, Integer(PSP)));
  TimeOut:= AIni.ReadInteger(CSessaoPixCDConfig, CChaveTimeOut, TimeOut);

  with DadosAutomacao do
  begin
    CNPJSoftwareHouse := AIni.ReadString(CSessaoPixCDConfig, CChaveCNPJSoftwareHouse, CNPJSoftwareHouse);
    NomeAplicacao := AIni.ReadString(CSessaoPixCDConfig, CChaveNomeAplicacao, NomeAplicacao);
    NomeSoftwareHouse := AIni.ReadString(CSessaoPixCDConfig, CChaveNomeSoftwareHouse, NomeSoftwareHouse);
    VersaoAplicacao := AIni.ReadString(CSessaoPixCDConfig, CChaveVersaoAplicacao, VersaoAplicacao);
  end;

  with Proxy do
  begin
    Host := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyHost, Host);
    Pass := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyPass, Pass);
    Port := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyPort, Port);
    User := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyUser, User);
  end;

  with Recebedor do
  begin
    CodCategoriaComerciante := AIni.ReadInteger(CSessaoPixCDConfig, CChaveCodCategoriaComerciante, CodCategoriaComerciante);
    CEP := AIni.ReadString(CSessaoPixCDConfig, CChaveCEPRecebedor, CEP);
    Cidade := AIni.ReadString(CSessaoPixCDConfig, CChaveCidadeRecebedor, Cidade);
    Nome := AIni.ReadString(CSessaoPixCDConfig, CChaveNomeRecebedor, Nome);
    UF := AIni.ReadString(CSessaoPixCDConfig, CChaveUFRecebedor, UF);
  end;
end;

procedure TPIXCDConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoPixCDConfig, CChaveAmbiente, integer(Ambiente));
  AIni.WriteString(CSessaoPixCDConfig, CChaveArqLogPixCD, ArqLog);
  AIni.WriteInteger(CSessaoPixCDConfig, CChaveNivelLog, NivelLog);
  AIni.WriteInteger(CSessaoPixCDConfig, CChaveTipoChave, Integer(TipoChave));
  AIni.WriteInteger(CSessaoPixCDConfig, CChavePSP, Integer(PSP));
  AIni.WriteInteger(CSessaoPixCDConfig, CChaveTimeOut, TimeOut);

  with DadosAutomacao do
  begin
    AIni.WriteString(CSessaoPixCDConfig, CChaveCNPJSoftwareHouse, CNPJSoftwareHouse);
    AIni.WriteString(CSessaoPixCDConfig, CChaveNomeAplicacao, NomeAplicacao);
    AIni.WriteString(CSessaoPixCDConfig, CChaveNomeSoftwareHouse, NomeSoftwareHouse);
    AIni.WriteString(CSessaoPixCDConfig, CChaveVersaoAplicacao, VersaoAplicacao);
  end;

  with Proxy do
  begin
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyHost, Host);
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyPass, Pass);
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyPort, Port);
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyUser, User);
  end;

  with Recebedor do
  begin
    AIni.WriteInteger(CSessaoPixCDConfig, CChaveCodCategoriaComerciante, CodCategoriaComerciante);
    AIni.WriteString(CSessaoPixCDConfig, CChaveCEPRecebedor, CEP);
    AIni.WriteString(CSessaoPixCDConfig, CChaveCidadeRecebedor, Cidade);
    AIni.WriteString(CSessaoPixCDConfig, CChaveNomeRecebedor, Nome);
    AIni.WriteString(CSessaoPixCDConfig, CChaveUFRecebedor, UF);
  end;
end;

{ TPIXCDBradescoConfig }
constructor TPIXCDBradescoConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqPFX := '';
  FSenhaPFX := '';
end;

procedure TPIXCDBradescoConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := Aini.ReadString(CSessaoPIXCDBradescoConfig, CChavePIXBradesco, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDBradescoConfig, CChaveClientIDBradesco, ClientID);
  ClientSecret:= AIni.ReadString(CSessaoPIXCDBradescoConfig, CChaveClientSecretBradesco, ClientSecret);
  ArqPFX := AIni.ReadString(CSessaoPIXCDBradescoConfig, CChaveArqPFXBradesco, ArqPFX);
  SenhaPFX := AIni.ReadString(CSessaoPIXCDBradescoConfig, CChaveSenhaPFXBradesco, SenhaPFX);
end;

procedure TPIXCDBradescoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDBradescoConfig, CChavePIXBradesco, ChavePIX);
  AIni.WriteString(CSessaoPIXCDBradescoConfig, CChaveClientIDBradesco, ClientID);
  AIni.WriteString(CSessaoPIXCDBradescoConfig, CChaveClientSecretBradesco, ClientSecret);
  AIni.WriteString(CSessaoPIXCDBradescoConfig, CChaveArqPFXBradesco, CChaveArqPFXBradesco);
  AIni.WriteString(CSessaoPIXCDBradescoConfig, CChaveSenhaPFXBradesco, SenhaPFX);
end;

{ TPIXCDSicrediConfig }
constructor TPIXCDSicrediConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
  FAPIVersion:= ver262;
end;

procedure TPIXCDSicrediConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDSicrediConfig, CChavePIXSicredi, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDSicrediConfig, CChaveClientIDSicredi, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDSicrediConfig, CChaveClientSecretSicredi, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDSicrediConfig, CChaveArqChavePrivadaSicredi, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDSicrediConfig, CChaveArqCertificadoSicredi, ArqCertificado);
  APIVersion := TACBrPIXAPIVersion(AIni.ReadInteger(CSessaoPIXCDSicrediConfig, CChaveAPIVersionSicredi, Integer(APIVersion)));
end;

procedure TPIXCDSicrediConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDSicrediConfig, CChavePIXSicredi, ChavePIX);
  AIni.WriteString(CSessaoPIXCDSicrediConfig, CChaveClientIDSicredi, ClientID);
  AIni.WriteString(CSessaoPIXCDSicrediConfig, CChaveClientSecretSicredi, ClientSecret);
  AIni.WriteString(CSessaoPIXCDSicrediConfig, CChaveArqChavePrivadaSicredi, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDSicrediConfig, CChaveArqCertificadoSicredi, ArqCertificado);
  AIni.WriteInteger(CSessaoPIXCDSicrediConfig, CChaveAPIVersionSicredi, Integer(APIVersion));
end;

{ TPIXCDSiccobConfig }
constructor TPIXCDSiccobConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FTokenSandbox := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
  FAPIVersion := ver262;
end;

procedure TPIXCDSiccobConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDSicoobConfig, CChavePIXSicoob, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDSicoobConfig, CChaveClientIDSicoob, ClientID);
  TokenSandbox := AIni.ReadString(CSessaoPIXCDSicoobConfig, CChaveTokenSandboxSicoob, TokenSandbox);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDSicoobConfig, CChaveArqChavePrivadaSicoob, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDSicoobConfig, CChaveArqCertificadoSicoob, ArqCertificado);
  APIVersion := TACBrPIXAPIVersion(AIni.ReadInteger(CSessaoPIXCDSicoobConfig, CChaveAPIVersionSicoob, Integer(APIVersion)));
end;

procedure TPIXCDSiccobConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDSicoobConfig, CChavePIXSicoob, ChavePIX);
  AIni.WriteString(CSessaoPIXCDSicoobConfig, CChaveClientIDSicoob, ClientID);
  AIni.WriteString(CSessaoPIXCDSicoobConfig, CChaveTokenSandboxSicoob, TokenSandbox);
  Aini.WriteString(CSessaoPIXCDSicoobConfig, CChaveArqChavePrivadaSicoob, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDSicoobConfig, CChaveArqCertificadoSicoob, ArqCertificado);
  AIni.WriteInteger(CSessaoPIXCDSicoobConfig, CChaveAPIVersionSicoob, Integer(APIVersion));
end;

{ TPIXCDShipayConfig }
constructor TPIXCDShipayConfig.Create;
begin
  FClientID := '';
  FSecretKey := '';
  FAccessKey := '';
end;

procedure TPIXCDShipayConfig.LerIni(const AIni: TCustomIniFile);
begin
  ClientID := AIni.ReadString(CSessaoPIXCDShipayConfig, CChaveClientIDShipay, ClientID);
  SecretKey := AIni.ReadString(CSessaoPIXCDShipayConfig, CChaveSecretKeyShipay, SecretKey);
  AccessKey := AIni.ReadString(CSessaoPIXCDShipayConfig, CChaveAccessKeyShipay, AccessKey);
end;

procedure TPIXCDShipayConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDShipayConfig, CChaveClientIDShipay, ClientID);
  AIni.WriteString(CSessaoPIXCDShipayConfig, CChaveSecretKeyShipay, SecretKey);
  AIni.WriteString(CSessaoPIXCDShipayConfig, CChaveAccessKeyShipay, AccessKey);
end;

{ TPIXCDSantanderConfig }
constructor TPIXCDSantanderConfig.Create;
begin
  FChavePIX := '';
  FConsumerKey := '';
  FConsumerSecret := '';
  FArqCertificadoPFX := '';
  FSenhaCertificadoPFX := '';
  FAPIVersion:= ver262;
end;

procedure TPIXCDSantanderConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDSantanderConfig, CChavePIXSantander, ChavePIX);
  ConsumerKey := AIni.ReadString(CSessaoPIXCDSantanderConfig, CChaveConsumerKeySantander, ConsumerKey);
  ConsumerSecret := AIni.ReadString(CSessaoPIXCDSantanderConfig, CChaveConsumerSecretSantander, ConsumerSecret);
  ArqCertificadoPFX := AIni.ReadString(CSessaoPIXCDSantanderConfig, CChaveArqCertificadoPFXSantander, ArqCertificadoPFX);
  SenhaCertificadoPFX := AIni.ReadString(CSessaoPIXCDSantanderConfig, CChaveSenhaCertificadoPFXSantander, SenhaCertificadoPFX);
  APIVersion := TACBrPIXAPIVersion(AIni.ReadInteger(CSessaoPIXCDSantanderConfig, CChaveAPIVersionSantander, Integer(APIVersion)));
end;

procedure TPIXCDSantanderConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDSantanderConfig, CChavePIXSantander, ChavePIX);
  AIni.WriteString(CSessaoPIXCDSantanderConfig, CChaveConsumerKeySantander, ConsumerKey);
  AIni.WriteString(CSessaoPIXCDSantanderConfig, CChaveConsumerSecretSantander, ConsumerSecret);
  AIni.WriteString(CSessaoPIXCDSantanderConfig, CChaveArqCertificadoPFXSantander, ArqCertificadoPFX);
  AIni.WriteString(CSessaoPIXCDSantanderConfig, CChaveSenhaCertificadoPFXSantander, SenhaCertificadoPFX);
  AIni.WriteInteger(CSessaoPIXCDSantanderConfig, CChaveAPIVersionSantander, Integer(APIVersion));
end;

{ TPIXCDPixPDVConfig }
constructor TPIXCDPixPDVConfig.Create;
begin
  FCNPJ := '';
  FToken := '';
  FSecretKey := '';
end;

procedure TPIXCDPixPDVConfig.LerIni(const AIni: TCustomIniFile);
begin
  CNPJ := AIni.ReadString(CSessaoPIXCDPixPDVConfig, CChaveCNPJPixPDV, CNPJ);
  Token := AIni.ReadString(CSessaoPIXCDPixPDVConfig, CChaveToken, Token);
  SecretKey := AIni.ReadString(CSessaoPIXCDPixPDVConfig, CChaveSecretKeyPixPDV, SecretKey);
end;

procedure TPIXCDPixPDVConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDPixPDVConfig, CChaveCNPJPixPDV, CNPJ);
  AIni.WriteString(CSessaoPIXCDPixPDVConfig, CChaveToken, Token);
  AIni.WriteString(CSessaoPIXCDPixPDVConfig, CChaveSecretKeyPixPDV, SecretKey);
end;

{ TPIXCDPagSeguroConfig }
constructor TPIXCDPagSeguroConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
end;

procedure TPIXCDPagSeguroConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDPagSeguroConfig, CChavePIXPagSeguro, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDPagSeguroConfig, CChaveClientIDPagSeguro, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDPagSeguroConfig, CChaveClientSecretPagSeguro, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDPagSeguroConfig, CChaveArqChavePrivadaPagSeguro, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDPagSeguroConfig, CChaveArqCertificadoPagSeguro, ArqCertificado);
end;

procedure TPIXCDPagSeguroConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDPagSeguroConfig, CChavePIXPagSeguro, ChavePIX);
  AIni.WriteString(CSessaoPIXCDPagSeguroConfig, CChaveClientIDPagSeguro, ClientID);
  AIni.WriteString(CSessaoPIXCDPagSeguroConfig, CChaveClientSecretPagSeguro, ClientSecret);
  AIni.WriteString(CSessaoPIXCDPagSeguroConfig, CChaveArqChavePrivadaPagSeguro, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDPagSeguroConfig, CChaveArqCertificadoPagSeguro, ArqCertificado);
end;

{ TPIXCDItauConfig }
constructor TPIXCDItauConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
  FAPIVersion:= ver262;
end;

procedure TPIXCDItauConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDItauConfig, CChavePIXItau, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDItauConfig, CChaveClientIDItau, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDItauConfig, CChaveClientSecretItau, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDItauConfig, CChaveArqChavePrivadaItau, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDItauConfig, CChaveArqCertificadoItau, ArqCertificado);
  APIVersion := TACBrPIXAPIVersion(AIni.ReadInteger(CSessaoPIXCDItauConfig, CChaveAPIVersionItau, Integer(APIVersion)));
end;

procedure TPIXCDItauConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDItauConfig, CChavePIXItau, ChavePIX);
  AIni.WriteString(CSessaoPIXCDItauConfig, CChaveClientIDItau, ClientID);
  AIni.WriteString(CSessaoPIXCDItauConfig, CChaveClientSecretItau, ClientSecret);
  AIni.WriteString(CSessaoPIXCDItauConfig, CChaveArqChavePrivadaItau, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDItauConfig, CChaveArqCertificadoItau, ArqCertificado);
  AIni.WriteInteger(CSessaoPIXCDItauConfig, CChaveAPIVersionItau, Integer(APIVersion));
end;

{ TPIXCDInterConfig }
constructor TPIXCDInterConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
end;

procedure TPIXCDInterConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDInterConfig, CChavePIXInter, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDInterConfig, CChaveClientIDInter, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDInterConfig, CChaveClientSecretInter, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDInterConfig, CChaveArqChavePrivadaInter, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDInterConfig, CChaveArqCertificadoInter, ArqCertificado);
end;

procedure TPIXCDInterConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDInterConfig, CChavePIXInter, ChavePIX);
  AIni.WriteString(CSessaoPIXCDInterConfig, CChaveClientIDInter, ClientID);
  AIni.WriteString(CSessaoPIXCDInterConfig, CChaveClientSecretInter, ClientSecret);
  AIni.WriteString(CSessaoPIXCDInterConfig, CChaveArqChavePrivadaInter, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDInterConfig, CChaveArqCertificadoInter, ArqCertificado);
end;

{ TPIXCDGerenciaNetConfig }
constructor TPIXCDGerenciaNetConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqPFX := '';
end;

procedure TPIXCDGerenciaNetConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDGerenciaNetConfig, CChavePIXGerenciaNet, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDGerenciaNetConfig, CChaveClientIDGerenciaNet, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDGerenciaNetConfig, CChaveClientSecretGerenciaNet, ClientSecret);
  ArqPFX := AIni.ReadString(CSessaoPIXCDGerenciaNetConfig, CChaveArqPFXGerenciaNet, ArqPFX);
end;

procedure TPIXCDGerenciaNetConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDGerenciaNetConfig, CChavePIXGerenciaNet, ChavePIX);
  AIni.WriteString(CSessaoPIXCDGerenciaNetConfig, CChaveClientIDGerenciaNet, ClientID);
  AIni.WriteString(CSessaoPIXCDGerenciaNetConfig, CChaveClientSecretGerenciaNet, ClientSecret);
  AIni.WriteString(CSessaoPIXCDGerenciaNetConfig, CChaveArqPFXGerenciaNet, ArqPFX);
end;

{ TPIXCDBancoDoBrasilConfig }
constructor TPIXCDBancoDoBrasilConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FDeveloperApplicationKey := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
  FArqPFX := '';
  FSenhaPFX := '';
  FBBAPIVersao := apiVersao1;
  FAPIVersion := ver262;
end;

procedure TPIXCDBancoDoBrasilConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChavePIXBancoBrasil, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChaveClientIDBancoBrasil, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChaveClientSecretBancoBrasil, ClientSecret);
  DeveloperApplicationKey := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChaveDeveloperApplicationKeyBancoBrasil, DeveloperApplicationKey);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChaveArqChavePrivadaBancoBrasil, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChaveArqCertificadoBancoBrasil, ArqCertificado);
  ArqPFX := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChaveArqPFXBancoBrasil, ArqPFX);
  SenhaPFX := AIni.ReadString(CSessaoPIXCDBancoBrasilConfig, CChaveSenhaPFXBancoBrasil, SenhaPFX);
  BBAPIVersao := TACBrBBAPIVersao(AIni.ReadInteger(CSessaoPIXCDBancoBrasilConfig, CChaveBBAPIVersaoBancoBrasil, Integer(BBAPIVersao)));
  APIVersion := TACBrPIXAPIVersion(AIni.ReadInteger(CSessaoPIXCDBancoBrasilConfig, CChaveAPIVersionBancoBrasil, Integer(APIVersion)));
end;

procedure TPIXCDBancoDoBrasilConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChavePIXBancoBrasil, ChavePIX);
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChaveClientIDBancoBrasil, ClientID);
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChaveClientSecretBancoBrasil, ClientSecret);
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChaveDeveloperApplicationKeyBancoBrasil, DeveloperApplicationKey);
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChaveArqChavePrivadaBancoBrasil, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChaveArqCertificadoBancoBrasil, ArqCertificado);
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChaveArqPFXBancoBrasil, ArqPFX);
  AIni.WriteString(CSessaoPIXCDBancoBrasilConfig, CChaveSenhaPFXBancoBrasil, SenhaPFX);
  AIni.WriteInteger(CSessaoPIXCDBancoBrasilConfig, CChaveBBAPIVersaoBancoBrasil, Integer(BBAPIVersao));
  AIni.WriteInteger(CSessaoPIXCDBancoBrasilConfig, CChaveAPIVersionBancoBrasil, Integer(APIVersion));
end;

{ TPIXCDAilosConfig }
constructor TPIXCDAilosConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
  FArqCertificadoRoot := '';
end;

procedure TPIXCDAilosConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDAilosConfig, CChavePIXAilos, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDAilosConfig, CChaveClientIDAilos, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDAilosConfig, CChaveClientSecretAilos, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDAilosConfig, CChaveArqChavePrivadaAilos, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDAilosConfig, CChaveArqCertificadoAilos, ArqCertificado);
  ArqCertificadoRoot := AIni.ReadString(CSessaoPIXCDAilosConfig, CChaveArqCertificadoRootAilos, ArqCertificadoRoot);
end;

procedure TPIXCDAilosConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDAilosConfig, CChavePIXAilos, ChavePIX);
  AIni.WriteString(CSessaoPIXCDAilosConfig, CChaveClientIDAilos, ClientID);
  AIni.WriteString(CSessaoPIXCDAilosConfig, CChaveClientSecretAilos, ClientSecret);
  AIni.WriteString(CSessaoPIXCDAilosConfig, CChaveArqChavePrivadaAilos, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDAilosConfig, CChaveArqCertificadoAilos, ArqCertificado);
  AIni.WriteString(CSessaoPIXCDAilosConfig, CChaveArqCertificadoRootAilos, ArqCertificadoRoot);
end;

{ TPIXCDMateraConfig }
constructor TPIXCDMateraConfig.Create;
begin
  FClientID := '';
  FSecretKey := '';
  FClientSecret := '';
  FArqCertificado := '';
  FArqChavePrivada := '';
  FAccountID := '';
  FChavePIX := '';
  FMediatorFee  := 0;
end;

procedure TPIXCDMateraConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDMateraConfig, CChavePIXMatera, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDMateraConfig, CChaveClientIDMatera, ClientID);
  SecretKey := AIni.ReadString(CSessaoPIXCDMateraConfig, CChaveSecretKeyMatera, SecretKey);
  ClientSecret := AIni.ReadString(CSessaoPIXCDMateraConfig, CChaveClientSecretMatera, ClientSecret);
  ArqCertificado := AIni.ReadString(CSessaoPIXCDMateraConfig, CChaveArqCertificadoMatera, ArqCertificado);
  ArqChavePrivada := AIni.ReadString(CSessaoPIXCDMateraConfig, CChaveArqChavePrivadaMatera, ArqChavePrivada);
  AccountID := AIni.ReadString(CSessaoPIXCDMateraConfig, CChaveAccountIDMatera, AccountID);
  MediatorFee := AIni.ReadFloat(CSessaoPIXCDMateraConfig, CChaveMediatorFeeMatera, MediatorFee);
end;

procedure TPIXCDMateraConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDMateraConfig, CChavePIXMatera, ChavePIX);
  AIni.WriteString(CSessaoPIXCDMateraConfig, CChaveClientIDMatera, ClientID);
  AIni.WriteString(CSessaoPIXCDMateraConfig, CChaveSecretKeyMatera, SecretKey);
  AIni.WriteString(CSessaoPIXCDMateraConfig, CChaveClientSecretMatera, ClientSecret);
  AIni.WriteString(CSessaoPIXCDMateraConfig, CChaveArqCertificadoMatera, ArqCertificado);
  AIni.WriteString(CSessaoPIXCDMateraConfig, CChaveArqChavePrivadaMatera, ArqChavePrivada);
  AIni.WriteString(CSessaoPIXCDMateraConfig, CChaveAccountIDMatera, AccountID);
  AIni.WriteFloat(CSessaoPIXCDMateraConfig, CChaveMediatorFeeMatera, MediatorFee);
end;

{ TPIXCDCieloConfig }
constructor TPIXCDCieloConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
end;

procedure TPIXCDCieloConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDCieloConfig, CChavePIXCielo, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPIXCDCieloConfig, CChaveClientIDCielo, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPIXCDCieloConfig, CChaveClientSecretCielo, ClientSecret);
end;

procedure TPIXCDCieloConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDCieloConfig, CChavePIXCielo, ChavePIX);
  AIni.WriteString(CSessaoPIXCDCieloConfig, CChaveClientIDCielo, ClientID);
  AIni.WriteString(CSessaoPIXCDCieloConfig, CChaveClientSecretCielo, ClientSecret);
end;

{ TPIXCDMercadoPagoConfig }
constructor TPIXCDMercadoPagoConfig.Create;
begin
  FChavePIX:= '';
  FAccessToken:= '';
end;

procedure TPIXCDMercadoPagoConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPIXCDMercadoPagoConfig, CChavePIXMercadoPago, ChavePIX);
  AccessToken := AIni.ReadString(CSessaoPIXCDMercadoPagoConfig, CChaveAccesTokenMercadoPago, AccessToken);
end;

procedure TPIXCDMercadoPagoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPIXCDMercadoPagoConfig, CChavePIXMercadoPago, ChavePIX);
  AIni.WriteString(CSessaoPIXCDMercadoPagoConfig, CChaveAccesTokenMercadoPago, AccessToken);
end;

end.

