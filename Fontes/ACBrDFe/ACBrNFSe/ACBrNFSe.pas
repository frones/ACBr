{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrNFSe;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrDFe,
  ACBrDFeException,
  ACBrDFeConfiguracoes,
  ACBrNFSeDANFSeClass,
  ACBrNFSeConfiguracoes,
  ACBrNFSeNotasFiscais,
  ACBrNFSeWebServices,
  pnfsNFSe,
  pcnConversao,
  pnfsConversao,
  ACBrUtil.Base,
  ACBrUtil.Compatibilidade,
  ACBrUtil.Strings;

const
  ACBRNFSE_NAMESPACE = 'NameSpace.varia.conforme.provedor';

type
  EACBrNFSeException = class(EACBrDFeException);

  { TACBrNFSe }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFSe = class(TACBrDFe)
  private
    FDANFSE: TACBrNFSeDANFSEClass;
    FNotasFiscais: TNotasFiscais;
    FStatus: TStatusACBrNFSe;
    FWebServices: TWebServices;

    function GetNumID(ANFSe : TNFSe): String;
    function GetConfiguracoes: TConfiguracoesNFSe;
    procedure SetConfiguracoes(AValue: TConfiguracoesNFSe);
    procedure SetDANFSE(const Value: TACBrNFSeDANFSEClass);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamNFSe: TStream = nil; const NomeArq: String = ''; sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function GerarLote(ALote: Integer; AqMaxRps: Integer = 50;
      ASincrono: Boolean = False): Boolean; overload;
    function GerarLote(const ALote: String; AqMaxRps: Integer = 50;
      ASincrono: Boolean = False): Boolean; overload;

    function Enviar(ALote: integer; Imprimir: Boolean = True): Boolean; overload;
    function Enviar(const ALote: String; Imprimir: Boolean = True): Boolean; overload;

    function TesteEnviar(ALote: Integer): Boolean; overload;
    function TesteEnviar(const ALote: String): Boolean; overload;

    function EnviarSincrono(ALote: Integer; Imprimir: Boolean = True): Boolean; overload;
    function EnviarSincrono(const ALote: String; Imprimir: Boolean = True): Boolean; overload;

    function Gerar(ARps: Integer; ALote: Integer = 1; Imprimir: Boolean = True): Boolean;

    function ConsultarSituacao(const AProtocolo: String;
                               const ANumLote: String = ''): Boolean;
    function ConsultarLoteRps(const ANumLote, AProtocolo: string): Boolean;
    function ConsultarNFSeporRps(const ANumero, ASerie, ATipo: String;
                                 const ANumLote: String = '';
                                 const ACodMunicipioTOM: Integer = 0): Boolean;
    function ConsultarNFSe(ADataInicial, ADataFinal: TDateTime;
      const ANumeroNFSe: String = ''; APagina: Integer = 1;
      const ACNPJTomador: String = ''; const AIMTomador: String = '';
      const ANomeInter: String = ''; const ACNPJInter: String = ''; const AIMInter: String = '';
      const ASerie: String = ''; const ANumeroLote: String = ''): Boolean;

    function ConsultarURL(const ACNPJPrestador, AIMPrestador : string;
                          const ANumeroNFSe, ACodigoTribMun : string) : Boolean;

    function CancelarNFSe(const ACodigoCancelamento: String;
                          const ANumeroNFSe: String = '';
                          const AMotivoCancelamento: String = '';
                          const ANumLote: String = '';
                          const ACodigoVerificacao: string = '';
                          const ASerieNFSe: string = '';
                          const ANumeroRps: string = '';
                          const ASerieRps: string = '';
                          const AValorNFSe: Double = 0): Boolean;

    function SubstituirNFSe(const ACodigoCancelamento, ANumeroNFSe: String;
                            const AMotivoCancelamento: String = ''): Boolean;

    function LinkNFSe(ANumeroNFSe: Integer; const ACodVerificacao: String; const AChaveAcesso: String = ''): String; overload;
    function LinkNFSe(ANumeroNFSe: Double; const ACodVerificacao: String; const AChaveAcesso: String = ''): String; overload;
    function LinkNFSe(ANumeroNFSe: String; const ACodVerificacao: String; const AChaveAcesso: String = ''): String; overload;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmada(AValue: integer): Boolean;
    function cStatProcessado(AValue: integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutNFSe; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutNFSe): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaNFSe;
    function GerarNomeArqSchema(const ALayOut: TLayOutNFSe;
      VersaoServico: Double): String;

    function GerarIntegridade(const AXML: string): string;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property NotasFiscais: TNotasFiscais read FNotasFiscais write FNotasFiscais;
    property Status: TStatusACBrNFSe read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrNFSe);

    property NumID[ANFSe : TNFSe] :  string read GetNumID;
  published
    property Configuracoes: TConfiguracoesNFSe
      read GetConfiguracoes write SetConfiguracoes;
    property DANFSE: TACBrNFSeDANFSEClass read FDANFSE write SetDANFSE;
  end;

implementation

uses
  strutils, dateutils, ACBrDFeSSL;

{$IFDEF FPC}
 {$R ACBrNFSeServicos.rc}
{$ELSE}
 {$R ACBrNFSeServicos.res}
{$ENDIF}

{ TACBrNFSe }

constructor TACBrNFSe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNotasFiscais := TNotasFiscais.Create(Self, NotaFiscal);
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrNFSe.Destroy;
begin
  FNotasFiscais.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrNFSe.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamNFSe: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stNFSeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamNFSe, NomeArq,
      sReplyTo, sBCC);
  finally
    SetStatus( stNFSeIdle );
  end;
end;

procedure TACBrNFSe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDANFSE <> nil) and
    (AComponent is TACBrNFSeDANFSEClass) then
    FDANFSE := nil;
end;

function TACBrNFSe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesNFSe.Create(Self);
end;

procedure TACBrNFSe.SetDANFSE(const Value: TACBrNFSeDANFSEClass);
var
  OldValue: TACBrNFSeDANFSEClass;
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

function TACBrNFSe.GetNomeModeloDFe: String;
begin
  Result := 'NFSe';
end;

function TACBrNFSe.GetNameSpaceURI: String;
begin
  Result := ''; // ACBRNFSE_NAMESPACE;
end;

function TACBrNFSe.cStatConfirmada(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrNFSe.cStatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302: Result := True;
    else
      Result := False;
  end;
end;

function TACBrNFSe.IdentificaSchema(const AXML: String): TSchemaNFSe;
var
  I: integer;
begin

  Result := schNFSe;
  I := pos('<infNFSe', AXML);
  if I = 0 then
  begin
    I := pos('<infCanc', AXML);
    if I > 0 then
      Result := schCancNFSe
  end;
end;

function TACBrNFSe.GerarNomeArqSchema(const ALayOut: TLayOutNFSe;
  VersaoServico: Double): String;
var
  NomeServico, NomeSchema, ArqSchema: String;
  Versao: Double;
begin
  // Procura por Versão na pasta de Schemas //
  NomeServico := LayOutToServico(ALayOut);
  NomeSchema := NomeServicoToNomeSchema(NomeServico);
  ArqSchema := '';
  if NaoEstaVazio(NomeSchema) then
  begin
    Versao := VersaoServico;
    AchaArquivoSchema( NomeSchema, Versao, ArqSchema );
  end;

  Result := ArqSchema;
end;

function TACBrNFSe.GetNumID(ANFSe: TNFSe): String;
var
  NumDoc, xCNPJ: String;
begin
  if ANFSe = nil then
    raise EACBrNFSeException.Create('Não foi informado o objeto TNFSe para gerar a chave!');

  if ANFSe.Numero = '' then
    NumDoc := ANFSe.IdentificacaoRps.Numero
  else
    NumDoc := ANFSe.Numero;

  if ANFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
    xCNPJ := ANFSe.Prestador.Cnpj
  else
    xCNPJ := ANFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;

  if Configuracoes.Arquivos.NomeLongoNFSe then
    Result := GerarNomeNFSe(Configuracoes.WebServices.UFCodigo,
                            ANFSe.DataEmissao,
                            OnlyNumber(xCNPJ),
                            StrToInt64Def(NumDoc, 0))
  else
    Result := NumDoc + ANFSe.IdentificacaoRps.Serie;
end;

function TACBrNFSe.GetConfiguracoes: TConfiguracoesNFSe;
begin
  Result := TConfiguracoesNFSe(FPConfiguracoes);
end;

procedure TACBrNFSe.SetConfiguracoes(AValue: TConfiguracoesNFSe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrNFSe.LerVersaoDeParams(LayOutServico: TLayOutNFSe): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoNFSeToDbl( ve100 {Configuracoes.Geral.VersaoDF}));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrNFSe.NomeServicoToNomeSchema(const NomeServico: String): String;
Var
  ok: Boolean;
  ALayout: TLayOutNFSe;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaNFSeToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

procedure TACBrNFSe.LerServicoDeParams(LayOutServico: TLayOutNFSe;
  var Versao: Double; var URL: String);
begin
  if Configuracoes.WebServices.Ambiente = taHomologacao then
  begin
    case LayOutServico of
      LayNfseRecepcaoLote:         URL := Configuracoes.Geral.ConfigURL.HomRecepcaoLoteRPS;
      LayNfseConsultaSitLoteRps:   URL := Configuracoes.Geral.ConfigURL.HomConsultaSitLoteRPS;
      LayNfseConsultaLote:         URL := Configuracoes.Geral.ConfigURL.HomConsultaLoteRPS;
      LayNfseConsultaNfseRps:      URL := Configuracoes.Geral.ConfigURL.HomConsultaNFSeRPS;
      LayNfseConsultaNfse:         URL := Configuracoes.Geral.ConfigURL.HomConsultaNFSe;
      LayNfseCancelaNfse:          URL := Configuracoes.Geral.ConfigURL.HomCancelaNFSe;
      LayNfseGerar:                URL := Configuracoes.Geral.ConfigURL.HomGerarNFSe;
      LayNfseRecepcaoLoteSincrono: URL := Configuracoes.Geral.ConfigURL.HomRecepcaoSincrono;
      LayNfseSubstituiNfse:        URL := Configuracoes.Geral.ConfigURL.HomSubstituiNFSe;
      LayNfseAbrirSessao:          URL := Configuracoes.Geral.ConfigURL.HomAbrirSessao;
      LayNfseFecharSessao:         URL := Configuracoes.Geral.ConfigURL.HomFecharSessao;
      LayNfseConsultaURL:          URL := Configuracoes.Geral.ConfigURL.HomConsultaURL;
    end;
  end
  else begin
    case LayOutServico of
      LayNfseRecepcaoLote:         URL := Configuracoes.Geral.ConfigURL.ProRecepcaoLoteRPS;
      LayNfseConsultaSitLoteRps:   URL := Configuracoes.Geral.ConfigURL.ProConsultaSitLoteRPS;
      LayNfseConsultaLote:         URL := Configuracoes.Geral.ConfigURL.ProConsultaLoteRPS;
      LayNfseConsultaNfseRps:      URL := Configuracoes.Geral.ConfigURL.ProConsultaNFSeRPS;
      LayNfseConsultaNfse:         URL := Configuracoes.Geral.ConfigURL.ProConsultaNFSe;
      LayNfseCancelaNfse:          URL := Configuracoes.Geral.ConfigURL.ProCancelaNFSe;
      LayNfseGerar:                URL := Configuracoes.Geral.ConfigURL.ProGerarNFSe;
      LayNfseRecepcaoLoteSincrono: URL := Configuracoes.Geral.ConfigURL.ProRecepcaoSincrono;
      LayNfseSubstituiNfse:        URL := Configuracoes.Geral.ConfigURL.ProSubstituiNFSe;
      LayNfseAbrirSessao:          URL := Configuracoes.Geral.ConfigURL.ProAbrirSessao;
      LayNfseFecharSessao:         URL := Configuracoes.Geral.ConfigURL.ProFecharSessao;
      LayNfseConsultaURL:          URL := Configuracoes.Geral.ConfigURL.ProConsultaURL;
    end;
  end;
end;

procedure TACBrNFSe.SetStatus(const stNewStatus: TStatusACBrNFSe);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrNFSe.GerarLote(ALote: Integer; AqMaxRps: Integer;
  ASincrono: Boolean): Boolean;
begin
  Result := GerarLote(IntToStr(ALote), AqMaxRps, ASincrono);
end;

function TACBrNFSe.GerarLote(const ALote: String; AqMaxRps: Integer;
  ASincrono: Boolean): Boolean;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > AqMaxRps then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de ' +
      IntToStr(AqMaxRps) + ' RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  NotasFiscais.Assinar(Configuracoes.Geral.ConfigAssinar.RPS);

  Result := WebServices.GeraLote(ALote, AqMaxRps, ASincrono);
end;

function TACBrNFSe.Enviar(ALote: integer; Imprimir: Boolean): Boolean;
begin
  Result := Enviar(IntToStr(ALote),Imprimir);
end;

function TACBrNFSe.Enviar(const ALote: String; Imprimir: Boolean): Boolean;
var
  i: Integer;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  NotasFiscais.Assinar(Configuracoes.Geral.ConfigAssinar.RPS);

  Result := WebServices.Envia(ALote);

  if DANFSE <> nil then
  begin
    for i:= 0 to NotasFiscais.Count-1 do
    begin
      if NotasFiscais.Items[i].Confirmada and Imprimir then
        NotasFiscais.Items[i].Imprimir;
    end;
    SetStatus( stNFSeIdle );
  end;
end;

function TACBrNFSe.EnviarSincrono(ALote: Integer;
  Imprimir: Boolean): Boolean;
begin
  Result := EnviarSincrono(IntToStr(ALote), Imprimir);
end;

function TACBrNFSe.EnviarSincrono(const ALote: String;
  Imprimir: Boolean): Boolean;
var
  i: Integer;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  NotasFiscais.Assinar(Configuracoes.Geral.ConfigAssinar.RPS);

  Result := WebServices.EnviaSincrono(ALote);

  if DANFSE <> nil then
  begin
    for i:= 0 to NotasFiscais.Count-1 do
    begin
      if NotasFiscais.Items[i].Confirmada and Imprimir then
        NotasFiscais.Items[i].Imprimir;
    end;
    SetStatus( stNFSeIdle );
  end;
end;

function TACBrNFSe.Gerar(ARps: Integer; ALote: Integer; Imprimir: Boolean): Boolean;
var
  i: Integer;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao componente'));

  if Configuracoes.Geral.Provedor in [proBHISS, proWebISS, proWebISSv2] then
  begin
    if NotasFiscais.Count > 3 then
      GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 3 RPS)' +
        ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));
  end
  else begin
    if NotasFiscais.Count > 1 then
      GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 1 RPS)' +
        ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));
  end;

  NotasFiscais.Assinar(Configuracoes.Geral.ConfigAssinar.RpsGerar);

  Result := WebServices.Gera(ARps, ALote);

  if DANFSE <> nil then
  begin
    for i:= 0 to NotasFiscais.Count-1 do
    begin
      if NotasFiscais.Items[i].Confirmada and Imprimir then
        NotasFiscais.Items[i].Imprimir;
    end;
    SetStatus( stNFSeIdle );
  end;
end;

function TACBrNFSe.GerarIntegridade(const AXML: string): string;
var
  XML: string;
  i, j: Integer;
begin
  j := Length(AXML);
  XML := '';
  for i := 1 to J do
  begin
    if {$IFNDEF HAS_CHARINSET}ACBrUtil.Compatibilidade.{$ENDIF}CharInSet(AXML[i], ['!'..'~']) then
       XML := XML + AXML[i];
  end;

//  SSL.CarregarCertificadoSeNecessario;
  Result := SSL.CalcHash(XML + Configuracoes.Geral.Emitente.WebChaveAcesso,
                         dgstSHA512, outHexa, False);
  Result := lowerCase(Result);
end;

function TACBrNFSe.ConsultarSituacao(const AProtocolo: String; const ANumLote: String): Boolean;
begin
  Result := WebServices.ConsultaSituacao(AProtocolo, ANumLote);
end;

function TACBrNFSe.ConsultarURL(const ACNPJPrestador, AIMPrestador, ANumeroNFSe,
  ACodigoTribMun: string): Boolean;
begin
  Result := WebServices.ConsultaURL(ACNPJPrestador, AIMPrestador, ANumeroNFSe, ACodigoTribMun);
end;

function TACBrNFSe.ConsultarLoteRps(const ANumLote, AProtocolo: string): Boolean;
begin
  Result := WebServices.ConsultaLoteRps(ANumLote, AProtocolo);
end;

function TACBrNFSe.ConsultarNFSeporRps(const ANumero, ASerie, ATipo: String;
                                       const ANumLote: String = '';
                                       const ACodMunicipioTOM: Integer = 0): Boolean;
begin
//Removido por não ter necessidade de preencher o componente.
//  if NotasFiscais.Count <= 0 then
//    GerarException(ACBrStr('ERRO: Nenhum RPS carregado ao componente'));

  Result := WebServices.ConsultaNFSeporRps(ANumero, ASerie, ATipo, ANumLote, ACodMunicipioTOM);
end;

function TACBrNFSe.ConsultarNFSe(ADataInicial, ADataFinal: TDateTime;
  const ANumeroNFSe: String; APagina: Integer; const ACNPJTomador, AIMTomador,
  ANomeInter, ACNPJInter, AIMInter, ASerie, ANumeroLote: String): Boolean;
begin
  Result := WebServices.ConsultaNFSe(ADataInicial, ADataFinal, ANumeroNFSe,
            APagina, ACNPJTomador, AIMTomador, ANomeInter, ACNPJInter, AIMInter,
            ASerie, ANumeroLote);
end;

function TACBrNFSe.CancelarNFSe(const ACodigoCancelamento: String;
  const ANumeroNFSe: String = ''; const AMotivoCancelamento: String = '';
  const ANumLote: String = ''; const ACodigoVerificacao: String = '';
  const ASerieNFSe: string = ''; const ANumeroRps: string = '';
  const ASerieRps: string = ''; const AValorNFSe: Double = 0): Boolean;
begin
  Result := WebServices.CancelaNFSe(ACodigoCancelamento, ANumeroNFSe,
                             AMotivoCancelamento, ANumLote, ACodigoVerificacao,
                             ASerieNFSe, ANumeroRps, ASerieRps, AValorNFSe);
end;

function TACBrNFSe.SubstituirNFSe(const ACodigoCancelamento,
  ANumeroNFSe: String; const AMotivoCancelamento: String): Boolean;
var
  Assina: Boolean;
begin
  if ACodigoCancelamento = '' then
    GerarException(ACBrStr('ERRO: Código de Cancelamento não informado'));

  if ANumeroNFSe = '' then
    GerarException(ACBrStr('ERRO: Numero da NFS-e não informada'));

  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  Assina := Configuracoes.Geral.ConfigAssinar.RPS or
            Configuracoes.Geral.ConfigAssinar.SubstituirRps;

  NotasFiscais.Assinar(Assina);

  Result := WebServices.SubstituiNFSe(ACodigoCancelamento, ANumeroNFSe,
                                      AMotivoCancelamento);
end;

function TACBrNFSe.TesteEnviar(ALote: Integer): Boolean;
begin
  Result := TesteEnviar(IntToStr(ALote));
end;

function TACBrNFSe.TesteEnviar(const ALote: String): Boolean;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  NotasFiscais.Assinar(Configuracoes.Geral.ConfigAssinar.RPS);

  Result := WebServices.TestaEnvio(ALote);
end;

function TACBrNFSe.LinkNFSe(ANumeroNFSe: Integer; const ACodVerificacao: String; const AChaveAcesso: String = ''): String;
begin
  Result := LinkNFSe(IntToStr(ANumeroNFSe), ACodVerificacao, AChaveAcesso);
end;

function TACBrNFSe.LinkNFSe(ANumeroNFSe: Double; const ACodVerificacao,
  AChaveAcesso: String): String;
begin
  Result := LinkNFSe(FloatToStr(ANumeroNFSe), ACodVerificacao, AChaveAcesso);
end;

function TACBrNFSe.LinkNFSe(ANumeroNFSe: String; const ACodVerificacao,
  AChaveAcesso: String): String;
var
  Texto, xNumeroNFSe, xNomeMunic, xLink: String;
begin
  if Configuracoes.WebServices.Ambiente = taProducao then
  begin
    Texto := Configuracoes.Geral.ConfigGeral.ProLinkNFSe;
    xNomeMunic := Configuracoes.Geral.xNomeURL_P;
    xLink := Configuracoes.Geral.xLinkURL_P;
  end
  else
  begin
    Texto := Configuracoes.Geral.ConfigGeral.HomLinkNFSe;
    xNomeMunic := Configuracoes.Geral.xNomeURL_H;
    xLink := Configuracoes.Geral.xLinkURL_H;
  end;
  // %CodVerif%      : Representa o Código de Verificação da NFS-e
  // %NumeroNFSe%    : Representa o Numero da NFS-e
  // %NomeMunicipio% : Representa o Nome do Municipio
  // %InscMunic%     : Representa a Inscrição Municipal do Emitente
  // %Cnpj%          : Representa o CNPJ do Emitente

  xNumeroNFSe := ANumeroNFSe;

  Texto := StringReplace(Texto, '%CodVerif%', ACodVerificacao, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%NumeroNFSe%', xNumeroNFSe, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%NomeMunicipio%', xNomeMunic, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%InscMunic%', Configuracoes.Geral.Emitente.InscMun, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%ChaveAcesso%', AChaveAcesso, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%Cnpj%', Configuracoes.Geral.Emitente.CNPJ, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%LinkURL%', xLink, [rfReplaceAll]);

  Result := Texto;
end;

end.
