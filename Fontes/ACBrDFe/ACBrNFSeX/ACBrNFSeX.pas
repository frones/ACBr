{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrNFSeX;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrUtil, ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrNFSeXDANFSEClass, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrXmlBase, ACBrNFSeXInterface, ACBrNFSeXWebserviceBase,
  ACBrNFSeXWebservicesResponse, ACBrNFSeXProviderManager, ACBrNFSeXConversao;

resourcestring
  ERR_SEM_PROVEDOR = 'Nenhum provedor selecionado';

type
  EACBrNFSeException = class(EACBrDFeException);

  { TACBrNFSe }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFSeX = class(TACBrDFe)
  private
    FProvider: IACBrNFSeXProvider;
    FDANFSE: TACBrNFSeXDANFSEClass;
    FNotasFiscais: TNotasFiscais;
    FStatus: TStatusACBrNFSe;
    fpCidadesJaCarregadas: Boolean;

    function GetNumID(ANFSe : TNFSe): String;
    function GetConfiguracoes: TConfiguracoesNFSe;
    procedure SetConfiguracoes(AValue: TConfiguracoesNFSe);
    procedure SetDANFSE(const Value: TACBrNFSeXDANFSEClass);

  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetProvedor;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamNFSe: TStream = nil; const NomeArq: String = ''; sReplyTo: TStrings = nil); override;

    function GerarLote(aLote: Integer; aqMaxRps: Integer = 50;
      aModoEnvio: TmodoEnvio = meAutomatico): TNFSeEmiteResponse; overload;

    function GerarLote(const aLote: String; aqMaxRps: Integer = 50;
      aModoEnvio: TmodoEnvio = meAutomatico): TNFSeEmiteResponse; overload;

    function Emitir(aLote: Integer; aModoEnvio: TmodoEnvio = meAutomatico;
      aImprimir: Boolean = True): TNFSeEmiteResponse; overload;

    function Emitir(const aLote: String; aModoEnvio: TmodoEnvio = meAutomatico;
      aImprimir: Boolean = True): TNFSeEmiteResponse; overload;

    // Usado pelos provedores que seguem a versão 1 do layout da ABRASF.
    function ConsultarSituacao(const AProtocolo: String;
      const ANumLote: String = ''): TNFSeConsultaSituacaoResponse;

    function ConsultarLoteRps(const AProtocolo: String;
      const ANumLote: String = ''): TNFSeConsultaLoteRpsResponse;

    function ConsultarNFSePorRps(const ANumRPS, ASerie, ATipo: String;
      const ACodVerificacao: string = ''): TNFSeConsultaNFSeporRpsResponse;

    // Usado pelos provedores que seguem a versão 1 do layout da ABRASF.
    function ConsultarNFSePorNumero(const aNumero: string;
      aRetorno: TtpRetorno = trXML; aPagina: Integer = 1): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: string;
      aPagina: Integer = 1): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime;
      aPagina: Integer = 1; aNumeroLote: string = '';
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoPrestadoPorNumero(const aNumero: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime;
      aPagina: Integer = 1; aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoTomadoPorNumero(const aNumero: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime;
      aPagina: Integer = 1; aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    function ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao): TNFSeConsultaNFSeResponse;

    function ConsultarNFSe(aInfConsultaNFSe: TInfConsultaNFSe): TNFSeConsultaNFSeResponse;

    function CancelarNFSe(aInfCancelamento: TInfCancelamento): TNFSeCancelaNFSeResponse;

    function SubstituirNFSe(const ANumNFSe: String;
      const ACodCancelamento: string; const AMotCancelamento: String = '';
      const ANumLote: String = ''; const ACodVerificacao: String = ''): TNFSeSubstituiNFSeResponse;

    function LinkNFSe(ANumNFSe: String; const ACodVerificacao: String;
      const AChaveAcesso: String = ''): String;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function GerarIntegridade(const AXML: string): string;
    procedure SetStatus(const stNewStatus: TStatusACBrNFSe);
    procedure LerCidades;

    property NotasFiscais: TNotasFiscais  read FNotasFiscais write FNotasFiscais;
    property Status: TStatusACBrNFSe      read FStatus;
    property Provider: IACBrNFSeXProvider read FProvider;
    property NumID[ANFSe: TNFSe]: string  read GetNumID;

  published
    property Configuracoes: TConfiguracoesNFSe read GetConfiguracoes write SetConfiguracoes;
    property DANFSE: TACBrNFSeXDANFSEClass     read FDANFSE          write SetDANFSE;

  end;

implementation

uses
  StrUtils, DateUtils,
  ACBrDFeSSL, ACBrNFSeXProviderBase;

{$IFDEF FPC}
 {$R ACBrNFSeXServicos.rc}
{$ELSE}
 {$R ACBrNFSeXServicos.res}
{$ENDIF}

{ TACBrNFSeX }

constructor TACBrNFSeX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNotasFiscais := TNotasFiscais.Create(Self);

  fpCidadesJaCarregadas := False;
end;

destructor TACBrNFSeX.Destroy;
begin
  FNotasFiscais.Free;

  if Assigned(FProvider) then
    FProvider := nil;

  inherited Destroy;
end;

procedure TACBrNFSeX.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamNFSe: TStream; const NomeArq: String;
  sReplyTo: TStrings);
begin
  SetStatus( stNFSeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamNFSe,
                          NomeArq, sReplyTo);
  finally
    SetStatus( stNFSeIdle );
  end;
end;

procedure TACBrNFSeX.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDANFSE <> nil) and
     (AComponent is TACBrNFSeXDANFSEClass) then
    FDANFSE := nil;
end;

function TACBrNFSeX.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesNFSe.Create(Self);
end;

procedure TACBrNFSeX.SetDANFSE(const Value: TACBrNFSeXDANFSEClass);
var
  OldValue: TACBrNFSeXDANFSEClass;
begin
  if Value <> FDANFSE then
  begin
    if Assigned(FDANFSE) then
      FDANFSE.RemoveFreeNotification(Self);

    OldValue := FDANFSE; // Usa outra variavel para evitar Loop Infinito
    FDANFSE := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrNFSe) then
        OldValue.ACBrNFSe := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrNFSe := self;
    end;
  end;
end;

procedure TACBrNFSeX.SetProvedor;
begin
  if Assigned(FProvider) then
    FProvider := nil;

  FProvider := TACBrNFSeXProviderManager.GetProvider(Self);

  if not Assigned(FProvider) then Exit;

  with FProvider.ConfigGeral do
  begin
    TabServicosExt := Configuracoes.Arquivos.TabServicosExt;
  end;
end;

function TACBrNFSeX.GetNomeModeloDFe: String;
begin
  Result := 'NFSe';
end;

function TACBrNFSeX.GetNameSpaceURI: String;
begin
  Result := '';
end;

function TACBrNFSeX.GetNumID(ANFSe: TNFSe): String;
var
  NumDoc, xCNPJ: String;
begin
  if ANFSe = nil then
    raise EACBrNFSeException.Create('Não foi informado o objeto TNFSe para gerar a chave!');

  if ANFSe.Numero = '' then
    NumDoc := ANFSe.IdentificacaoRps.Numero
  else
    NumDoc := ANFSe.Numero;

  xCNPJ := ANFSe.Prestador.IdentificacaoPrestador.Cnpj;

  if Configuracoes.Arquivos.NomeLongoNFSe then
    Result := GerarNomeNFSe(Configuracoes.WebServices.UFCodigo,
                            ANFSe.DataEmissao,
                            OnlyNumber(xCNPJ),
                            StrToInt64Def(NumDoc, 0))
  else
    Result := NumDoc + ANFSe.IdentificacaoRps.Serie;
end;

function TACBrNFSeX.GetConfiguracoes: TConfiguracoesNFSe;
begin
  Result := TConfiguracoesNFSe(FPConfiguracoes);
end;

procedure TACBrNFSeX.SetConfiguracoes(AValue: TConfiguracoesNFSe);
begin
  FPConfiguracoes := AValue;
end;

procedure TACBrNFSeX.LerCidades;
begin
  if not fpCidadesJaCarregadas then
  begin
    LerParamsIni(True);
    fpCidadesJaCarregadas := True;
  end;
end;

procedure TACBrNFSeX.SetStatus(const stNewStatus: TStatusACBrNFSe);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrNFSeX.GerarLote(aLote: Integer; aqMaxRps: Integer;
  aModoEnvio: TmodoEnvio): TNFSeEmiteResponse;
begin
  Result := GerarLote(IntToStr(aLote), aqMaxRps, aModoEnvio);
end;

function TACBrNFSeX.GerarLote(const aLote: String; aqMaxRps: Integer;
  aModoEnvio: TmodoEnvio): TNFSeEmiteResponse;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.GeraLote(aLote, aqMaxRps, aModoEnvio);
end;

function TACBrNFSeX.Emitir(aLote: Integer; aModoEnvio: TmodoEnvio;
  aImprimir: Boolean): TNFSeEmiteResponse;
begin
  Result := Emitir(IntToStr(aLote), aModoEnvio, aImprimir);
end;

function TACBrNFSeX.Emitir(const aLote: String; aModoEnvio: TmodoEnvio;
  aImprimir: Boolean): TNFSeEmiteResponse;
var
  i: Integer;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.Emite(aLote, aModoEnvio);

  if DANFSE <> nil then
  begin
    SetStatus(stNFSeImprimir);

    for i:= 0 to NotasFiscais.Count-1 do
    begin
      if NotasFiscais.Items[i].Confirmada and aImprimir then
        NotasFiscais.Items[i].Imprimir;
    end;

    SetStatus(stNFSeIdle);
  end;
end;

function TACBrNFSeX.GerarIntegridade(const AXML: string): string;
var
  XML: string;
  i, j: Integer;
  xAssinatura: TStringList;
begin
  j := Length(AXML);
  XML := '';

  for i := 1 to J do
  begin
    if {$IFNDEF HAS_CHARINSET}ACBrUtil.{$ENDIF}CharInSet(AXML[i], ['!'..'~']) then
      XML := XML + AXML[i];
  end;

//  SSL.CarregarCertificadoSeNecessario;

  xAssinatura := TStringList.Create;
  try
    xAssinatura.Add(XML + Configuracoes.Geral.Emitente.WSChaveAcesso);

    Result := string(SSL.CalcHash(xAssinatura, dgstSHA512, outHexa, False));
    Result := lowerCase(Result);
  finally
    xAssinatura.Free;
  end;
end;

function TACBrNFSeX.ConsultarLoteRps(const AProtocolo, ANumLote: String): TNFSeConsultaLoteRpsResponse;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.ConsultaLoteRps(AProtocolo, ANumLote);
end;

function TACBrNFSeX.ConsultarNFSe(aInfConsultaNFSe: TInfConsultaNFSe): TNFSeConsultaNFSeResponse;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.ConsultaNFSe(aInfConsultaNFSe);
end;

function TACBrNFSeX.ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: string;
  aPagina: Integer): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcPorFaixa;

      NumeroIniNFSe := aNumeroInicial;
      NumeroFinNFSe := aNumeroFinal;
      Pagina        := aPagina;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSePorNumero(const aNumero: string;
  aRetorno: TtpRetorno; aPagina: Integer): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      if aRetorno = trXML then
        tpConsulta := tcPorNumero
      else
        tpConsulta := tcPorNumeroURLRetornado;

      NumeroIniNFSe := aNumero;
      NumeroFinNFSe := aNumero;
      Pagina        := aPagina;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime;
  aPagina: Integer; aNumeroLote: string; aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcPorPeriodo;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
      Pagina      := aPagina;

      NumeroLote  := aNumeroLote;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSePorRps(const ANumRPS, ASerie, ATipo,
  ACodVerificacao: String): TNFSeConsultaNFSeporRpsResponse;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.ConsultaNFSeporRps(ANumRPS, ASerie, ATipo,
    ACodVerificacao);
end;

function TACBrNFSeX.ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoPrestado;

      CNPJInter := aCNPJ;
      IMInter   := aInscMun;
      Pagina    := aPagina;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoPrestadoPorNumero(const aNumero: string;
  aPagina: Integer; aDataInicial: TDateTime; aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoPrestado;

      NumeroIniNFSe := aNumero;
      NumeroFinNFSe := aNumero;
      Pagina        := aPagina;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial,
  aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoPrestado;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
      Pagina      := aPagina;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoPrestado;

      CNPJTomador := aCNPJ;
      IMTomador   := aInscMun;
      Pagina      := aPagina;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoTomado;

      CNPJInter := aCNPJ;
      IMInter   := aInscMun;
      Pagina    := aPagina;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoTomadoPorNumero(const aNumero: string;
  aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoTomado;

      NumeroIniNFSe := aNumero;
      NumeroFinNFSe := aNumero;
      Pagina        := aPagina;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial,
  aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoTomado;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
      Pagina      := aPagina;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoTomado;

      CNPJPrestador := aCNPJ;
      IMPrestador   := aInscMun;
      Pagina        := aPagina;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarNFSeServicoTomadoPorTomador(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo): TNFSeConsultaNFSeResponse;
var
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      tpConsulta := tcServicoTomado;

      CNPJTomador := aCNPJ;
      IMTomador   := aInscMun;
      Pagina      := aPagina;

      DataInicial := aDataInicial;
      DataFinal   := aDataFinal;
      tpPeriodo   := aTipoPeriodo;
    end;

    Result := ConsultarNFSe(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;
end;

function TACBrNFSeX.ConsultarSituacao(const AProtocolo,
  ANumLote: String): TNFSeConsultaSituacaoResponse;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.ConsultaSituacao(AProtocolo, ANumLote);
end;

function TACBrNFSeX.CancelarNFSe(aInfCancelamento: TInfCancelamento): TNFSeCancelaNFSeResponse;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.CancelaNFSe(aInfCancelamento);
end;

function TACBrNFSeX.SubstituirNFSe(const ANumNFSe, ACodCancelamento: String;
  const AMotCancelamento, ANumLote, ACodVerificacao: String): TNFSeSubstituiNFSeResponse;
begin
  if ACodCancelamento = '' then
    GerarException(ACBrStr('ERRO: Código de Cancelamento não informado'));

  if ANumNFSe = '' then
    GerarException(ACBrStr('ERRO: Numero da NFS-e não informada'));

  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  Result := FProvider.SubstituiNFSe(ANumNFSe, ACodCancelamento,
                                    AMotCancelamento, ANumLote, ACodVerificacao);
end;

function TACBrNFSeX.LinkNFSe(ANumNFSe: String; const ACodVerificacao,
  AChaveAcesso: String): String;
var
  Texto, xNumeroNFSe, xNomeMunic: String;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  if Configuracoes.WebServices.AmbienteCodigo = 1 then
    Texto := Provider.ConfigWebServices.Producao.LinkURL
  else
    Texto := Provider.ConfigWebServices.Homologacao.LinkURL;

  // %CodVerif%      : Representa o Código de Verificação da NFS-e
  // %NumeroNFSe%    : Representa o Numero da NFS-e
  // %NomeMunicipio% : Representa o Nome do Municipio
  // %InscMunic%     : Representa a Inscrição Municipal do Emitente
  // %Cnpj%          : Representa o CNPJ do Emitente

  xNumeroNFSe := ANumNFSe;

  Texto := StringReplace(Texto, '%CodVerif%', ACodVerificacao, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%NumeroNFSe%', xNumeroNFSe, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%NomeMunicipio%', xNomeMunic, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%InscMunic%', Configuracoes.Geral.Emitente.InscMun, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%ChaveAcesso%', AChaveAcesso, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%Cnpj%', Configuracoes.Geral.Emitente.CNPJ, [rfReplaceAll]);

  Result := Texto;
end;

end.

