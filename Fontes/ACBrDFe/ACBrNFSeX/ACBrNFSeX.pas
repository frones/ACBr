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
  ACBrBase,
  ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXWebservices,
  ACBrNFSeXClass, ACBrNFSeXInterface, ACBrNFSeXConversao,
  ACBrNFSeXWebserviceBase, ACBrNFSeXDANFSeClass;

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
    FDANFSE: TACBrNFSeXDANFSeClass;
    FNotasFiscais: TNotasFiscais;
    FStatus: TStatusACBrNFSe;
    fpCidadesJaCarregadas: Boolean;
    FWebService: TWebServices;

    function GetConfiguracoes: TConfiguracoesNFSe;
    procedure SetConfiguracoes(AValue: TConfiguracoesNFSe);
    procedure SetDANFSE(const Value: TACBrNFSeXDANFSeClass);

  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetProvedor(aProvedor: TnfseProvedor = proNenhum;
      aVersao: TVersaoNFSe = ve100);
    procedure SetProvider;

    function GetNumID(ANFSe: TNFSe): String;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamNFSe: TStream = nil; const NomeArq: String = ''; sReplyTo: TStrings = nil); override;

    procedure GerarLote(const aLote: String; aqMaxRps: Integer = 50;
      aModoEnvio: TmodoEnvio = meAutomatico); overload;

    procedure Emitir(const aLote: String; aModoEnvio: TmodoEnvio = meAutomatico;
      aImprimir: Boolean = True);

    // Usado pelos provedores que seguem a versão 1 do layout da ABRASF.
    procedure ConsultarSituacao(const AProtocolo: String;
      const ANumLote: String = '');

    procedure ConsultarLoteRps(const AProtocolo: String;
      const ANumLote: String = '');

    procedure ConsultarNFSePorRps(const ANumRPS, ASerie, ATipo: String;
      const ACodVerificacao: string = '');

    // Usado pelos provedores que seguem a versão 1 do layout da ABRASF.
    procedure ConsultarNFSePorNumero(const aNumero: string; aPagina: Integer = 1);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: string;
      aPagina: Integer = 1);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime;
      aPagina: Integer = 1; aNumeroLote: string = '';
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoPrestadoPorNumero(const aNumero: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime;
      aPagina: Integer = 1; aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoTomadoPorNumero(const aNumero: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime;
      aPagina: Integer = 1; aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado pelos provedores que seguem a versão 2 do layout da ABRASF.
    procedure ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: string;
      aPagina: Integer = 1; aDataInicial: TDateTime = 0; aDataFinal: TDateTime = 0;
      aTipoPeriodo: TtpPeriodo = tpEmissao);

    // Usado Para realizar consultas genericas
    procedure ConsultarNFSeGenerico(aInfConsultaNFSe: TInfConsultaNFSe);

    procedure ConsultarNFSe;

    procedure CancelarNFSe(aInfCancelamento: TInfCancelamento);

    procedure SubstituirNFSe(const ANumNFSe: String; const ASerieNFSe: String;
      const ACodCancelamento: string; const AMotCancelamento: String = '';
      const ANumLote: String = ''; const ACodVerificacao: String = '');

    function LinkNFSe(ANumNFSe: String; const ACodVerificacao: String;
      const AChaveAcesso: String = ''; const AValorServico: String = ''): String;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function GerarIntegridade(const AXML: string): string;
    procedure SetStatus(const stNewStatus: TStatusACBrNFSe);
    procedure LerCidades;

    property NotasFiscais: TNotasFiscais  read FNotasFiscais write FNotasFiscais;
    property Status: TStatusACBrNFSe      read FStatus;
    property Provider: IACBrNFSeXProvider read FProvider;
    property NumID[ANFSe: TNFSe]: string  read GetNumID;
    property WebService: TWebServices     read FWebService;

  published
    property Configuracoes: TConfiguracoesNFSe read GetConfiguracoes write SetConfiguracoes;
    property DANFSE: TACBrNFSeXDANFSeClass     read FDANFSE          write SetDANFSE;

  end;

implementation

uses
  Math,
  ACBrUtil,
  ACBrDFeSSL,
  ACBrNFSeXProviderManager;

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
  FWebService := TWebservices.Create;


  fpCidadesJaCarregadas := False;
end;

destructor TACBrNFSeX.Destroy;
begin
  FNotasFiscais.Free;
  FWebService.Free;
  if Assigned(FProvider) then FProvider := nil;

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
     (AComponent is TACBrNFSeXDANFSeClass) then
    FDANFSE := nil;
end;

function TACBrNFSeX.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesNFSe.Create(Self);
end;

procedure TACBrNFSeX.SetDANFSE(const Value: TACBrNFSeXDANFSeClass);
var
  OldValue: TACBrNFSeXDANFSeClass;
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

procedure TACBrNFSeX.SetProvedor(aProvedor: TnfseProvedor; aVersao: TVersaoNFSe);
begin
  Configuracoes.Geral.Provedor := aProvedor;
  Configuracoes.Geral.Versao := aVersao;

  if aProvedor <> proNenhum then
    SetProvider;
end;

procedure TACBrNFSeX.SetProvider;
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

  xCNPJ := ANFSe.Prestador.IdentificacaoPrestador.CpfCnpj;

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

procedure TACBrNFSeX.GerarLote(const aLote: String; aqMaxRps: Integer; aModoEnvio: TmodoEnvio);
begin
  if not Assigned(FProvider) then raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FWebService.Gerar.Clear;
  FWebService.Gerar.Lote := aLote;
  FWebService.Gerar.MaxRps := aqMaxRps;
  FWebService.Gerar.ModoEnvio := aModoEnvio;

  FProvider.GeraLote;
end;

procedure TACBrNFSeX.Emitir(const aLote: String; aModoEnvio: TmodoEnvio; aImprimir: Boolean);
var
  i, qTentativas, Intervalo, Situacao: Integer;
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);


  FWebService.Emite.Clear;
  FWebService.Emite.Lote := aLote;
  FWebService.Emite.ModoEnvio := aModoEnvio;

  FProvider.Emite;

  if Configuracoes.Geral.ConsultaLoteAposEnvio and
     (FWebService.Emite.ModoEnvio = meLoteAssincrono) then
  begin
    if (FWebService.Emite.Protocolo <> '') or (FWebService.Emite.Lote <> '') then
    begin
      if FProvider.ConfigGeral.ConsultaSitLote then
      begin
        with Configuracoes.WebServices do
        begin
          FWebService.ConsultaSituacao.Clear;
          FWebService.ConsultaSituacao.Protocolo := FWebService.Emite.Protocolo;
          FWebService.ConsultaSituacao.Lote := FWebService.Emite.Lote;

          Sleep(AguardarConsultaRet);

          qTentativas := 0;
          Situacao := 0;
          Intervalo := max(IntervaloTentativas, 1000);

          while (Situacao < 3) and (qTentativas < Tentativas) do
          begin
            FProvider.ConsultaSituacao;

            Situacao := StrToIntDef(FWebService.ConsultaSituacao.Situacao, 0);
            Inc(qTentativas);
            sleep(Intervalo);
          end;
        end;
      end;

      if FProvider.ConfigGeral.ConsultaLote then
      begin
        FWebService.ConsultaLoteRps.Clear;

        if FProvider.ConfigMsgDados.UsarNumLoteConsLote then
        begin
          FWebService.ConsultaLoteRps.Protocolo := FWebService.Emite.Protocolo;
          FWebService.ConsultaLoteRps.Lote := FWebService.Emite.Lote;
        end
        else
        begin
          FWebService.ConsultaLoteRps.Protocolo := FWebService.Emite.Protocolo;
          FWebService.ConsultaLoteRps.Lote := '';
        end;

        FProvider.ConsultaLoteRps;
      end;
    end;
  end;

  if DANFSE <> nil then
  begin
    SetStatus(stNFSeImprimir);

    for i := 0 to NotasFiscais.Count-1 do
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

procedure TACBrNFSeX.ConsultarLoteRps(const AProtocolo, ANumLote: String);
begin
  if not Assigned(FProvider) then raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FWebService.ConsultaLoteRps.Clear;
  FWebService.ConsultaLoteRps.Protocolo := AProtocolo;
  FWebService.ConsultaLoteRps.Lote := ANumLote;

  FProvider.ConsultaLoteRps;
end;

procedure TACBrNFSeX.ConsultarNFSe;
begin
  if not Assigned(FProvider) then raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FProvider.ConsultaNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeGenerico(aInfConsultaNFSe: TInfConsultaNFSe);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := aInfConsultaNFSe.tpConsulta;
    NumeroIniNFSe := aInfConsultaNFSe.NumeroIniNFSe;
    NumeroFinNFSe := aInfConsultaNFSe.NumeroFinNFSe;
    SerieNFSe := aInfConsultaNFSe.SerieNFSe;
    tpPeriodo := aInfConsultaNFSe.tpPeriodo;
    DataInicial := aInfConsultaNFSe.DataInicial;
    DataFinal := aInfConsultaNFSe.DataFinal;
    CNPJPrestador := aInfConsultaNFSe.CNPJPrestador;
    IMPrestador := aInfConsultaNFSe.IMPrestador;
    CNPJTomador := aInfConsultaNFSe.CNPJTomador;
    IMTomador := aInfConsultaNFSe.IMTomador;
    CNPJInter := aInfConsultaNFSe.CNPJInter;
    IMInter   := aInfConsultaNFSe.IMInter;
    NumeroLote := aInfConsultaNFSe.NumeroLote;
    Pagina := aInfConsultaNFSe.Pagina;
    CadEconomico := aInfConsultaNFSe.CadEconomico;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: string; aPagina: Integer);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcPorFaixa;

    NumeroIniNFSe := aNumeroInicial;
    NumeroFinNFSe := aNumeroFinal;
    Pagina        := aPagina;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSePorNumero(const aNumero: string; aPagina: Integer);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcPorNumero;

    NumeroIniNFSe := aNumero;
    NumeroFinNFSe := aNumero;
    Pagina        := aPagina;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime;
  aPagina: Integer; aNumeroLote: string; aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcPorPeriodo;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
    Pagina      := aPagina;

    NumeroLote  := aNumeroLote;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSePorRps(const ANumRPS, ASerie, ATipo,
  ACodVerificacao: String);
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FWebService.ConsultaNFSeporRps.Clear;
  FWebService.ConsultaNFSeporRps.NumRPS := ANumRPS;
  FWebService.ConsultaNFSeporRps.Serie := ASerie;
  FWebService.ConsultaNFSeporRps.Tipo := ATipo;
  FWebService.ConsultaNFSeporRps.CodVerificacao := ACodVerificacao;

  FProvider.ConsultaNFSeporRps;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoPrestado;

    CNPJInter := aCNPJ;
    IMInter   := aInscMun;
    Pagina    := aPagina;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoPrestadoPorNumero(const aNumero: string;
  aPagina: Integer; aDataInicial: TDateTime; aDataFinal: TDateTime; aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoPrestado;

    NumeroIniNFSe := aNumero;
    NumeroFinNFSe := aNumero;
    Pagina        := aPagina;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial,
  aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoPrestado;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
    Pagina      := aPagina;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoPrestado;

    CNPJTomador := aCNPJ;
    IMTomador   := aInscMun;
    Pagina      := aPagina;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoTomado;

    CNPJInter := aCNPJ;
    IMInter   := aInscMun;
    Pagina    := aPagina;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoTomadoPorNumero(const aNumero: string;
  aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoTomado;

    NumeroIniNFSe := aNumero;
    NumeroFinNFSe := aNumero;
    Pagina        := aPagina;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial,
  aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoTomado;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
    Pagina      := aPagina;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoTomado;

    CNPJPrestador := aCNPJ;
    IMPrestador   := aInscMun;
    Pagina        := aPagina;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarNFSeServicoTomadoPorTomador(const aCNPJ,
  aInscMun: string; aPagina: Integer; aDataInicial, aDataFinal: TDateTime;
  aTipoPeriodo: TtpPeriodo);
begin
  FWebService.ConsultaNFSe.Clear;

  with FWebService.ConsultaNFSe.InfConsultaNFSe do
  begin
    tpConsulta := tcServicoTomado;

    CNPJTomador := aCNPJ;
    IMTomador   := aInscMun;
    Pagina      := aPagina;

    DataInicial := aDataInicial;
    DataFinal   := aDataFinal;
    tpPeriodo   := aTipoPeriodo;
  end;

  ConsultarNFSe;
end;

procedure TACBrNFSeX.ConsultarSituacao(const AProtocolo, ANumLote: String);
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FWebService.ConsultaSituacao.Clear;
  FWebService.ConsultaSituacao.Protocolo := AProtocolo;
  FWebService.ConsultaSituacao.Lote := ANumLote;

  FProvider.ConsultaSituacao;
end;

procedure TACBrNFSeX.CancelarNFSe(aInfCancelamento: TInfCancelamento);
begin
  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FWebService.CancelaNFSe.Clear;

  with FWebService.CancelaNFSe.InfCancelamento do
  begin
    NumeroNFSe := aInfCancelamento.NumeroNFSe;
    SerieNFSe := aInfCancelamento.SerieNFSe;
    ChaveNFSe := aInfCancelamento.ChaveNFSe;
    CodCancelamento := aInfCancelamento.CodCancelamento;
    MotCancelamento := aInfCancelamento.MotCancelamento;
    NumeroLote := aInfCancelamento.NumeroLote;
    NumeroRps := aInfCancelamento.NumeroRps;
    SerieRps := aInfCancelamento.SerieRps;
    ValorNFSe := aInfCancelamento.ValorNFSe;
    CodVerificacao := aInfCancelamento.CodVerificacao;
    email := aInfCancelamento.email;
    NumeroNFSeSubst := aInfCancelamento.NumeroNFSeSubst;
    SerieNFSeSubst := aInfCancelamento.SerieNFSeSubst;
  end;

  FProvider.CancelaNFSe;

  if Configuracoes.Geral.ConsultaAposCancelar and
     FProvider.ConfigGeral.ConsultaNFSe then
  begin
//    try
      FWebService.ConsultaNFSe.Clear;

      with FWebService.ConsultaNFSe.InfConsultaNFSe do
      begin
        if FProvider.ConfigGeral.ConsultaPorFaixa then
          tpConsulta := tcPorFaixa
        else
          tpConsulta := tcPorNumero;

        NumeroIniNFSe := FWebService.CancelaNFSe.InfCancelamento.NumeroNFSe;
        NumeroFinNFSe := FWebService.CancelaNFSe.InfCancelamento.NumeroNFSe;
        Pagina        := 1;
      end;

      FProvider.ConsultaNFSe;
//    finally
//      FWebService.CancelaNFSe.Situacao := FWebService.ConsultaNFSe.Situacao;
//    end;
  end;
end;

procedure TACBrNFSeX.SubstituirNFSe(const ANumNFSe, ASerieNFSe, ACodCancelamento: String;
  const AMotCancelamento, ANumLote, ACodVerificacao: String);
begin
  if ANumNFSe = '' then
    GerarException(ACBrStr('ERRO: Numero da NFS-e não informada'));

  if ACodCancelamento = '' then
    GerarException(ACBrStr('ERRO: Código de Cancelamento não informado'));

  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if not Assigned(FProvider) then
    raise EACBrNFSeException.Create(ERR_SEM_PROVEDOR);

  FWebService.SubstituiNFSe.Clear;
  with FWebService.SubstituiNFSe.InfCancelamento do
  begin
    NumeroNFSe := aNumNFSe;
    SerieNFSe := aSerieNFSe;
    CodCancelamento := aCodCancelamento;
    MotCancelamento := aMotCancelamento;
    NumeroLote := aNumLote;
    CodVerificacao := aCodVerificacao;
  end;

  FProvider.SubstituiNFSe;
end;

function TACBrNFSeX.LinkNFSe(ANumNFSe: String; const ACodVerificacao,
  AChaveAcesso, AValorServico: String): String;
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
  Texto := StringReplace(Texto, '%ValorServico%', AValorServico, [rfReplaceAll]);

  Result := Texto;
end;

end.

