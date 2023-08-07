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
  ACBrBase, ACBrLibConfig, ACBrPIXCD;

type

  { TPIXCDAilosConfig }
  TPIXCDAilosConfig = class

  end;

  { TPIXCDBancoDoBrasilConfig }
  TPIXCDBancoDoBrasilConfig = class

  end;

  { TPIXCDGerenciaNetConfig }
  TPIXCDGerenciaNetConfig = class

  end;

  { TPIXCDInterConfig }
  TPIXCDInterConfig = class

  end;

  { TPIXCDItauConfig }
  TPIXCDItauConfig = class

  end;

  { TPIXCDPagSeguroConfig }
  TPIXCDPagSeguroConfig = class

  end;

  { TPIXCDPixPDVConfig }
  TPIXCDPixPDVConfig = class

  end;

  { TPIXCDSantanderConfig }
  TPIXCDSantanderConfig = class

  end;

  { TPIXCDShipayConfig }
  TPIXCDShipayConfig = class

  end;

  { TPIXCDSiccobConfig }
  TPIXCDSiccobConfig = class

  end;

  { TPIXCDSicrediConfig }
  TPIXCDSicrediConfig = class

  end;

  {TPIXCDBradescoConfig}
  TPIXCDBradescoConfig = class
    FClientID: AnsiString;
    FClientSecret: AnsiString;

    public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ClientID: AnsiString read FClientID write FClientID;
    property ClientSecret: AnsiString read FClientSecret write FClientSecret;

  end;

  { TPIXCDConfig }
  TPIXCDConfig = class
    FAmbiente: TACBrPixCDAmbiente;
    FArqLOG: String;
    FDadosAutomacao: TACBrPixDadosAutomacao;
    FNivelLog: Byte;
    FProxy: TACBrHttpProxy;
    FPSP: TACBrPSP;
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
    property PSP: TACBrPSP read FPSP write FPSP;
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

    protected

      procedure Travar; override;
      procedure Destravar; override;

      procedure INIParaClasse; override;
      procedure ClasseParaINI; override;
      procedure ClasseParaComponentes; override;

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
  inherited Destroy;
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
end;

procedure TLibPIXCDConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;
  ini.WriteString(CSessaoVersao, CLibPIXCDNome, CLibPIXCDVersao);
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
end;

procedure TLibPIXCDConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
  TACBrLibPIXCD(Owner).PIXCDDM.AplicarConfiguracoes;
end;

procedure TLibPIXCDConfig.Travar;
begin
  if Assigned(Owner) then
    TACBrLibPIXCD(Owner).PIXCDDM.Travar;
end;

procedure TLibPIXCDConfig.Destravar;
begin
  if Assigned(Owner) then
    TACBrLibPIXCD(Owner).PIXCDDM.Destravar;
end;

{ TPIXCDConfig }
constructor TPIXCDConfig.Create;
begin
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

  with Recebedor do
  begin
    CEP := AIni.ReadString(CSessaoPixCDConfig, CChaveCEPRecebedor, CEP);
    Cidade := Aini.ReadString(CSessaoPixCDConfig, CChaveCidadeRecebedor, Cidade);
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
  FClientID := '';
  FClientSecret := '';
end;

procedure TPIXCDBradescoConfig.LerIni(const AIni: TCustomIniFile);
begin
  ClientID := AIni.ReadString(CSessaoPixCDBradescoConfig, CChaveClientID, ClientID);
  ClientSecret:= AIni.ReadString(CSessaoPixCDBradescoConfig, CChaveClientSecret, ClientSecret);
end;

procedure TPIXCDBradescoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoPixCDBradescoConfig, CChaveClientID, ClientID);
  AIni.WriteString(CSessaoPixCDBradescoConfig, CChaveClientSecret, ClientSecret);
end;

end.

