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

  { TPIXCDMateraConfig }
  TPIXCDMateraConfig = class

  end;


  { TPIXCDAilosConfig }
  TPIXCDAilosConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FArqChavePrivada: AnsiString;
    FArqCertificado: AnsiString;
    FArqCertificadoRoot: AnsiString;

    public
    Constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property ArqChavePrivada: AnsiString read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: AnsiString read FArqCertificado write FArqCertificado;
    property ArqCertificadoRoot: AnsiString read FArqCertificadoRoot write FArqCertificadoRoot;
  end;

  { TPIXCDBancoDoBrasilConfig }
  TPIXCDBancoDoBrasilConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FDeveloperApplicationKey: AnsiString;
    FArqChavePrivada: AnsiString;
    FArqCertificado: AnsiString;
    FArqPFX: AnsiString;
    FSenhaPFX: AnsiString;
    FVersaoAPI: TACBrBBAPIVersao;
    FTipoCertificado: Integer;

    public
    Constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property DeveloperApplicationKey: AnsiString read FDeveloperApplicationKey write FDeveloperApplicationKey;
    property ArqChavePrivada: AnsiString read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: AnsiString read FArqCertificado write FArqCertificado;
    property ArqPFX: AnsiString read FArqPFX write FArqPFX;
    property SenhaPFX: AnsiString read FSenhaPFX write FSenhaPFX;
    property VersaoAPI: TACBrBBAPIVersao read FVersaoAPI write FVersaoAPI;
    property TipoCertificado: Integer read FTipoCertificado write FTipoCertificado;
  end;

  { TPIXCDGerenciaNetConfig }
  TPIXCDGerenciaNetConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FArqPFX: Ansistring;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property ArqPFX: AnsiString read FArqPFX write FArqPFX;
  end;

  { TPIXCDInterConfig }
  TPIXCDInterConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FArqChavePrivada: AnsiString;
    FArqCertificado: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property ArqChavePrivada: AnsiString read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: AnsiString read FArqCertificado write FArqCertificado;
  end;

  { TPIXCDItauConfig }
  TPIXCDItauConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FArqChavePrivada: AnsiString;
    FArqCertificado: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property ArqChavePrivada: AnsiString read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: AnsiString read FArqCertificado write FArqCertificado;
  end;

  { TPIXCDPagSeguroConfig }
  TPIXCDPagSeguroConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FArqChavePrivada: AnsiString;
    FArqCertificado: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property ArqChavePrivada: AnsiString read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: AnsiString read FArqCertificado write FArqCertificado;
  end;

  { TPIXCDPixPDVConfig }
  TPIXCDPixPDVConfig = class
    FCNPJ: AnsiString;
    FToken: AnsiString;
    FSecretKey: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property CNPJ: AnsiString read FCNPJ write FCNPJ;
    property Token: AnsiString read FToken write FToken;
    property SecretKey: AnsiString read FSecretKey write FSecretKey;
  end;

  { TPIXCDSantanderConfig }
  TPIXCDSantanderConfig = class
    FChavePIX: AnsiString;
    FConsumerKey: AnsiString;
    FConsumerSecret: AnsiString;
    FArqCertificadoPFX: AnsiString;
    FSenhaCertificadoPFX: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ConsumerKey: AnsiString read FConsumerKey write FConsumerKey;
    property ConsumerSecret: AnsiString read FConsumerSecret write FConsumerSecret;
    property ArqCertificadoPFX: AnsiString read FArqCertificadoPFX write FArqCertificadoPFX;
    property SenhaCertificadoPFX: AnsiString read FSenhaCertificadoPFX write FSenhaCertificadoPFX;
  end;

  { TPIXCDShipayConfig }
  TPIXCDShipayConfig = class
    FClientID: AnsiString;
    FSecretKey: AnsiString;
    FAccessKey: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ClientID: AnsiString read FClientID write FClientID;
    property SecretKey: AnsiString read FSecretKey write FSecretKey;
    property AccessKey: AnsiString read FAccessKey write FAccessKey;
  end;

  { TPIXCDSiccobConfig }
  TPIXCDSiccobConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FArqChavePrivada: AnsiString;
    FArqCertificado: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FChavePIX;
    property ArqChavePrivada: AnsiString read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: AnsiString read FArqCertificado write FArqCertificado;
  end;

  { TPIXCDSicrediConfig }
  TPIXCDSicrediConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FArqChavePrivada: AnsiString;
    FArqCertificado: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property ArqChavePrivada: AnsiString read FArqChavePrivada write FArqChavePrivada;
    property ArqCertificado: AnsiString read FArqCertificado write FArqCertificado;
  end;

  {TPIXCDBradescoConfig}
  TPIXCDBradescoConfig = class
    FChavePIX: AnsiString;
    FClientID: AnsiString;
    FClientSecret: AnsiString;
    FArqPFX: AnsiString;
    FSenhaPFX: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ChavePIX: AnsiString read FChavePIX write FChavePIX;
    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;
    property ArqPFX: AnsiString read FArqPFX write FArqPFX;
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
  end;

implementation

Uses
  ACBrLibPIXCDBase, ACBrLibPIXCDConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil.FilesIO, ACBrUtil.Strings;

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
  //FPIXCDMateraConfig := TPIXCDMatera.Create;
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
  //FPIXCDMateraConfig.Free;

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
  //FPIXCDMateraConfig.LerIni(Ini);
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
  //FPIXCDMateraConfig.GravarIni(Ini);
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

  with DadosAutomacao do
  begin
    CNPJSoftwareHouse := AIni.ReadString(CSessaoPixCDConfig, CChaveCNPJSoftwareHouse, CNPJSoftwareHouse);
    NomeAplicacao := AIni.ReadString(CSessaoPixCDConfig, CChaveNomeAplicacao, NomeAplicacao);
    NomeSoftwareHouse := AIni.ReadString(CSessaoPixCDConfig, CChaveNomeSoftwareHouse, NomeSoftwareHouse);
    VersaoAplicacao := AIni.ReadString(CSessaoPixCDConfig, CChaveVersaoAplicacao, VersaoAplicacao);
  end;

  NivelLog := AIni.ReadInteger(CSessaoPixCDConfig, CChaveNivelLog, NivelLog);

  with Proxy do
  begin
    Host := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyHost, Host);
    Pass := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyPass, Pass);
    Port := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyPort, Port);
    User := AIni.ReadString(CSessaoPixCDConfig, CChaveProxyUser, User);
  end;

  TipoChave:= TACBrPIXTipoChave(AIni.ReadInteger(CSessaoPixCDConfig, CChaveTipoChave, Integer(TipoChave)));
  PSP := TACBrPIXPSP(AIni.ReadInteger(CSessaoPixCDConfig, CChavePSP, Integer(PSP)));

  with Recebedor do
  begin
    CEP := AIni.ReadString(CSessaoPixCDConfig, CChaveCEPRecebedor, CEP);
    Cidade := AIni.ReadString(CSessaoPixCDConfig, CChaveCidadeRecebedor, Cidade);
    CodCategoriaComerciante := AIni.ReadInteger(CSessaoPixCDConfig, CChaveCodCategoriaComerciante, CodCategoriaComerciante);
    Nome := AIni.ReadString(CSessaoPixCDConfig, CChaveNomeRecebedor, Nome);
    UF := AIni.ReadString(CSessaoPixCDConfig, CChaveUFRecebedor, UF);
  end;

  TimeOut:= AIni.ReadInteger(CSessaoPixCDConfig, CChaveTimeOut, TimeOut);
end;

procedure TPIXCDConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoPixCDConfig, CChaveAmbiente, integer(Ambiente));
  AIni.WriteString(CSessaoPixCDConfig, CChaveArqLogPixCD, ArqLog);

  with DadosAutomacao do
  begin
    AIni.WriteString(CSessaoPixCDConfig, CChaveCNPJSoftwareHouse, CNPJSoftwareHouse);
    AIni.WriteString(CSessaoPixCDConfig, CChaveNomeAplicacao, NomeAplicacao);
    AIni.WriteString(CSessaoPixCDConfig, CChaveNomeSoftwareHouse, NomeSoftwareHouse);
    AIni.WriteString(CSessaoPixCDConfig, CChaveVersaoAplicacao, VersaoAplicacao);
  end;

  AIni.WriteInteger(CSessaoPixCDConfig, CChaveNivelLog, NivelLog);

  with Proxy do
  begin
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyHost, Host);
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyPass, Pass);
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyPort, Port);
    AIni.WriteString(CSessaoPixCDConfig, CChaveProxyUser, User);
  end;

  AIni.WriteInteger(CSessaoPixCDConfig, CChaveTipoChave, Integer(TipoChave));
  AIni.WriteInteger(CSessaoPixCDConfig, CChavePSP, Integer(PSP));

  with Recebedor do
  begin
    AIni.WriteString(CSessaoPixCDConfig, CChaveCEPRecebedor, CEP);
    AIni.WriteString(CSessaoPixCDConfig, CChaveCidadeRecebedor, Cidade);
    AIni.WriteInteger(CSessaoPixCDConfig, CChaveCodCategoriaComerciante, CodCategoriaComerciante);
    AIni.WriteString(CSessaoPixCDConfig, CChaveNomeRecebedor, Nome);
    AIni.WriteString(CSessaoPixCDConfig, CChaveUFRecebedor, UF);
  end;

  AIni.WriteInteger(CSessaoPixCDConfig, CChaveTimeOut, TimeOut);
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
  ChavePIX := Aini.ReadString(CSessaoPixCDBradescoConfig, CChavePIXBradesco, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDBradescoConfig, CChaveClientIDBradesco, ClientID);
  ClientSecret:= AIni.ReadString(CSessaoPixCDBradescoConfig, CChaveClientSecretBradesco, ClientSecret);
  ArqPFX := AIni.ReadString(CSessaoPixCDBradescoConfig, CChaveArqPFXBradesco, ArqPFX);
  SenhaPFX := AIni.ReadString(CSessaoPixCDBradescoConfig, CChaveSenhaPFXBradesco, SenhaPFX);
end;

procedure TPIXCDBradescoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDBradescoConfig, CChavePIXBradesco, ChavePIX);
  AIni.WriteString(CSessaoPixCDBradescoConfig, CChaveClientIDBradesco, ClientID);
  AIni.WriteString(CSessaoPixCDBradescoConfig, CChaveClientSecretBradesco, ClientSecret);
  AIni.WriteString(CSessaoPixCDBradescoConfig, CChaveArqPFXBradesco, CChaveArqPFXBradesco);
  AIni.WriteString(CSessaoPixCDBradescoConfig, CChaveSenhaPFXBradesco, SenhaPFX);
end;

{ TPIXCDSicrediConfig }
constructor TPIXCDSicrediConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
end;

procedure TPIXCDSicrediConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPixCDSicrediConfig, CChavePIXSicredi, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDSicrediConfig, CChaveClientIDSicredi, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPixCDSicrediConfig, CChaveClientSecretSicredi, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPixCDSicrediConfig, CChaveArqChavePrivadaSicredi, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPixCDSicrediConfig, CChaveArqCertificadoSicredi, ArqCertificado);
end;

procedure TPIXCDSicrediConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDSicrediConfig, CChavePIXSicredi, ChavePIX);
  AIni.WriteString(CSessaoPixCDSicrediConfig, CChaveClientIDSicredi, ClientID);
  AIni.WriteString(CSessaoPixCDSicrediConfig, CChaveClientSecretSicredi, ClientSecret);
  AIni.WriteString(CSessaoPixCDSicrediConfig, CChaveArqChavePrivadaSicredi, ArqChavePrivada);
  AIni.WriteString(CSessaoPixCDSicrediConfig, CChaveArqCertificadoSicredi, ArqCertificado);
end;

{ TPIXCDSiccobConfig }
constructor TPIXCDSiccobConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
end;

procedure TPIXCDSiccobConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPixCDSicoobConfig, CChavePIXSicoob, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDSicoobConfig, CChaveClientIDSicoob, ClientID);
  ArqChavePrivada := AIni.ReadString(CSessaoPixCDSicoobConfig, CChaveArqChavePrivadaSicoob, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPixCDSicoobConfig, CChaveArqCertificadoSicoob, ArqCertificado);
end;

procedure TPIXCDSiccobConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDSicoobConfig, CChavePIXSicoob, ChavePIX);
  AIni.WriteString(CSessaoPixCDSicoobConfig, CChaveClientIDSicoob, ClientID);
  Aini.WriteString(CSessaoPixCDSicoobConfig, CChaveArqChavePrivadaSicoob, ArqChavePrivada);
  AIni.WriteString(CSessaoPixCDSicoobConfig, CChaveArqCertificadoSicoob, ArqCertificado);
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
  ClientID := AIni.ReadString(CSessaoPixCDShipayConfig, CChaveClientIDShipay, ClientID);
  SecretKey := AIni.ReadString(CSessaoPixCDShipayConfig, CChaveSecretKeyShipay, SecretKey);
  AccessKey := AIni.ReadString(CSessaoPixCDShipayConfig, CChaveAccessKey, AccessKey);
end;

procedure TPIXCDShipayConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDShipayConfig, CChaveClientIDShipay, ClientID);
  AIni.WriteString(CSessaoPixCDShipayConfig, CChaveSecretKeyShipay, SecretKey);
  AIni.WriteString(CSessaoPixCDShipayConfig, CChaveAccessKey, AccessKey);
end;

{ TPIXCDSantanderConfig }
constructor TPIXCDSantanderConfig.Create;
begin
  FChavePIX := '';
  FConsumerKey := '';
  FConsumerSecret := '';
  FArqCertificadoPFX := '';
  FSenhaCertificadoPFX := '';
end;

procedure TPIXCDSantanderConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPixCDSantanderConfig, CChavePIXSantander, ChavePIX);
  ConsumerKey := AIni.ReadString(CSessaoPixCDSantanderConfig, CChaveConsumerKey, ConsumerKey);
  ConsumerSecret := AIni.ReadString(CSessaoPixCDSantanderConfig, CChaveConsumerSecret, ConsumerSecret);
  ArqCertificadoPFX := AIni.ReadString(CSessaoPixCDSantanderConfig, CChaveArqCertificadoPFX, ArqCertificadoPFX);
  SenhaCertificadoPFX := AIni.ReadString(CSessaoPixCDSantanderConfig, CChaveSenhaCertificadoPFX, SenhaCertificadoPFX);
end;

procedure TPIXCDSantanderConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDSantanderConfig, CChavePIXSantander, ChavePIX);
  AIni.WriteString(CSessaoPixCDSantanderConfig, CChaveConsumerKey, ConsumerKey);
  AIni.WriteString(CSessaoPixCDSantanderConfig, CChaveConsumerSecret, ConsumerSecret);
  AIni.WriteString(CSessaoPixCDSantanderConfig, CChaveArqCertificadoPFX, ArqCertificadoPFX);
  AIni.WriteString(CSessaoPixCDSantanderConfig, CChaveSenhaCertificadoPFX, SenhaCertificadoPFX);
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
  CNPJ := AIni.ReadString(CSessaoPixCDPixPDVConfig, CChaveCNPJPixPDV, CNPJ);
  Token := AIni.ReadString(CSessaoPixCDPixPDVConfig, CChaveToken, Token);
  SecretKey := AIni.ReadString(CSessaoPixCDPixPDVConfig, CChaveSecretKeyPixPDV, SecretKey);
end;

procedure TPIXCDPixPDVConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDPixPDVConfig, CChaveCNPJPixPDV, CNPJ);
  AIni.WriteString(CSessaoPixCDPixPDVConfig, CChaveToken, Token);
  AIni.WriteString(CSessaoPixCDPixPDVConfig, CChaveSecretKeyPixPDV, SecretKey);
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
  ChavePIX := AIni.ReadString(CSessaoPixCDPagSeguroConfig, CChavePIXPagSeguro, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDPagSeguroConfig, CChaveClientIDPagSeguro, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPixCDPagSeguroConfig, CChaveClientSecretPagSeguro, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPixCDPagSeguroConfig, CChaveArqChavePrivadaPagSeguro, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPixCDPagSeguroConfig, CChaveArqCertificadoPagSeguro, ArqCertificado);
end;

procedure TPIXCDPagSeguroConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDPagSeguroConfig, CChavePIXPagSeguro, ChavePIX);
  AIni.WriteString(CSessaoPixCDPagSeguroConfig, CChaveClientIDPagSeguro, ClientID);
  AIni.WriteString(CSessaoPixCDPagSeguroConfig, CChaveClientSecretPagSeguro, ClientSecret);
  AIni.WriteString(CSessaoPixCDPagSeguroConfig, CChaveArqChavePrivadaPagSeguro, ArqChavePrivada);
  AIni.WriteString(CSessaoPixCDPagSeguroConfig, CChaveArqCertificadoPagSeguro, ArqCertificado);
end;

{ TPIXCDItauConfig }
constructor TPIXCDItauConfig.Create;
begin
  FChavePIX := '';
  FClientID := '';
  FClientSecret := '';
  FArqChavePrivada := '';
  FArqCertificado := '';
end;

procedure TPIXCDItauConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPixCDItauConfig, CChavePIXItau, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDItauConfig, CChaveClientIDItau, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPixCDItauConfig, CChaveClientSecretItau, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPixCDItauConfig, CChaveArqChavePrivadaItau, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPixCDItauConfig, CChaveArqCertificadoItau, ArqCertificado);
end;

procedure TPIXCDItauConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDItauConfig, CChavePIXItau, ChavePIX);
  AIni.WriteString(CSessaoPixCDItauConfig, CChaveClientIDItau, ClientID);
  AIni.WriteString(CSessaoPixCDItauConfig, CChaveClientSecretItau, ClientSecret);
  AIni.WriteString(CSessaoPixCDItauConfig, CChaveArqChavePrivadaItau, ArqChavePrivada);
  AIni.WriteString(CSessaoPixCDItauConfig, CChaveArqCertificadoItau, ArqCertificado);
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
  ChavePIX := AIni.ReadString(CSessaoPixCDInterConfig, CChavePIXInter, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDInterConfig, CChaveClientIDInter, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPixCDInterConfig, CChaveClientSecretInter, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPixCDInterConfig, CChaveArqChavePrivadaInter, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPixCDInterConfig, CChaveArqCertificadoInter, ArqCertificado);
end;

procedure TPIXCDInterConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDInterConfig, CChavePIXInter, ChavePIX);
  AIni.WriteString(CSessaoPixCDInterConfig, CChaveClientIDInter, ClientID);
  AIni.WriteString(CSessaoPixCDInterConfig, CChaveClientSecretInter, ClientSecret);
  AIni.WriteString(CSessaoPixCDInterConfig, CChaveArqChavePrivadaInter, ArqChavePrivada);
  AIni.WriteString(CSessaoPixCDInterConfig, CChaveArqCertificadoInter, ArqCertificado);
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
  ChavePIX := AIni.ReadString(CSessaoPixCDGerenciaNetConfig, CChavePIXGerenciaNet, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDGerenciaNetConfig, CChaveClientIDGerenciaNet, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPixCDGerenciaNetConfig, CChaveClientSecretGerenciaNet, ClientSecret);
  ArqPFX := AIni.ReadString(CSessaoPixCDGerenciaNetConfig, CChaveArqPFXGerenciaNet, ArqPFX);
end;

procedure TPIXCDGerenciaNetConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDGerenciaNetConfig, CChavePIXGerenciaNet, ChavePIX);
  AIni.WriteString(CSessaoPixCDGerenciaNetConfig, CChaveClientIDGerenciaNet, ClientID);
  AIni.WriteString(CSessaoPixCDGerenciaNetConfig, CChaveClientSecretGerenciaNet, ClientSecret);
  AIni.WriteString(CSessaoPixCDGerenciaNetConfig, CChaveArqPFXGerenciaNet, ArqPFX);
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
  FVersaoAPI := apiVersao1;
  FTipoCertificado := 0;
end;

procedure TPIXCDBancoDoBrasilConfig.LerIni(const AIni: TCustomIniFile);
begin
  ChavePIX := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChavePIXBancoBrasil, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChaveClientIDBancoBrasil, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChaveClientSecretBancoBrasil, ClientSecret);
  DeveloperApplicationKey := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChaveDeveloperApplicationKeyBancoBrasil, DeveloperApplicationKey);
  ArqChavePrivada := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChaveArqChavePrivadaBancoBrasil, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChaveArqCertificadoBancoBrasil, ArqCertificado);
  ArqPFX := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChaveArqPFXBancoBrasil, ArqPFX);
  SenhaPFX := AIni.ReadString(CSessaoPixCDBancoBrasilConfig, CChaveSenhaPFXBancoBrasil, SenhaPFX);
  VersaoAPI := TACBrBBAPIVersao(AIni.ReadInteger(CSessaoPixCDBancoBrasilConfig, CChaveVersaoAPIBancoBrasil, Integer(VersaoAPI)));
  TipoCertificado := AIni.ReadInteger(CSessaoPixCDBancoBrasilConfig, CChaveTipoCertificadoBancoBrasil, TipoCertificado);
end;

procedure TPIXCDBancoDoBrasilConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChavePIXBancoBrasil, ChavePIX);
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChaveClientIDBancoBrasil, ClientID);
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChaveClientSecretBancoBrasil, ClientSecret);
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChaveDeveloperApplicationKeyBancoBrasil, DeveloperApplicationKey);
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChaveArqChavePrivadaBancoBrasil, ArqChavePrivada);
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChaveArqCertificadoBancoBrasil, ArqCertificado);
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChaveArqPFXBancoBrasil, ArqPFX);
  AIni.WriteString(CSessaoPixCDBancoBrasilConfig, CChaveSenhaPFXBancoBrasil, SenhaPFX);
  AIni.WriteInteger(CSessaoPixCDBancoBrasilConfig, CChaveVersaoAPIBancoBrasil, Integer(VersaoAPI));
  AIni.WriteInteger(CSessaoPixCDBancoBrasilConfig, CChaveTipoCertificadoBancoBrasil, Integer(TipoCertificado));
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
  ChavePIX := AIni.ReadString(CSessaoPixCDAilosConfig, CChavePIXAilos, ChavePIX);
  ClientID := AIni.ReadString(CSessaoPixCDAilosConfig, CChaveClientIDAilos, ClientID);
  ClientSecret := AIni.ReadString(CSessaoPixCDAilosConfig, CChaveClientSecretAilos, ClientSecret);
  ArqChavePrivada := AIni.ReadString(CSessaoPixCDAilosConfig, CChaveArqChavePrivadaAilos, ArqChavePrivada);
  ArqCertificado := AIni.ReadString(CSessaoPixCDAilosConfig, CChaveArqCertificadoAilos, ArqCertificado);
  ArqCertificadoRoot := AIni.ReadString(CSessaoPixCDAilosConfig, CChaveArqCertificadoRootAilos, ArqCertificadoRoot);
end;

procedure TPIXCDAilosConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDAilosConfig, CChavePIXAilos, ChavePIX);
  AIni.WriteString(CSessaoPixCDAilosConfig, CChaveClientIDAilos, ClientID);
  AIni.WriteString(CSessaoPixCDAilosConfig, CChaveClientSecretAilos, ClientSecret);
  AIni.WriteString(CSessaoPixCDAilosConfig, CChaveArqChavePrivadaAilos, ArqChavePrivada);
  AIni.WriteString(CSessaoPixCDAilosConfig, CChaveArqCertificadoAilos, ArqCertificado);
  AIni.WriteString(CSessaoPixCDAilosConfig, CChaveArqCertificadoRootAilos, ArqCertificadoRoot);
end;

end.

