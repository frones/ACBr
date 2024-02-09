{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3e;

interface

uses
  Classes, SysUtils, synautil,
  ACBrBase, ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrXmlBase,
  ACBrNF3eConfiguracoes, ACBrNF3eWebServices, ACBrNF3eNotasFiscais,
  ACBrNF3eDANF3eClass,
  pcnConversao,
  ACBrNF3eClass, ACBrNF3eConversao, ACBrNF3eEnvEvento;

const
  ACBRNF3e_NAMESPACE = 'http://www.portalfiscal.inf.br/nf3e';
  ACBRNF3e_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
     'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrNF3eException = class(EACBrDFeException);

  { TACBrNF3e }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNF3e = class(TACBrDFe)
  private
    FDANF3e: TACBrNF3eDANF3eClass;
    FNotasFiscais: TNotasFiscais;
    FEventoNF3e: TEventoNF3e;
    FStatus: TStatusNF3e;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesNF3e;
    function Distribuicao(AcUFAutor: integer; const ACNPJCPF, AultNSU, ANSU,
      chNF3e: String): Boolean;

    procedure SetConfiguracoes(AValue: TConfiguracoesNF3e);
    procedure SetDANF3e(const Value: TACBrNF3eDANF3eClass);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamNF3e: TStream = nil; const NomeArq: String = ''; 
	  sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function CstatConfirmada(AValue: integer): Boolean;
    function CstatProcessado(AValue: integer): Boolean;
    function CstatCancelada(AValue: integer): Boolean;

    function Enviar(ALote: Int64; Imprimir: Boolean = True;
      Sincrono: Boolean = False): Boolean; overload;
    function Enviar(const ALote: String; Imprimir: Boolean = True;
      Sincrono: Boolean = False): Boolean; overload;
    function Cancelamento(const AJustificativa: String; ALote: Int64 = 0): Boolean;
    function Consultar(const AChave: String = ''; AExtrairEventos: Boolean = False): Boolean;
    function EnviarEvento(idLote: Int64): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOut; var Versao: Double;
      var URL: String; var Servico: String; var SoapAction: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOut): String; reintroduce; overload;

    function AjustarVersaoQRCode( AVersaoQRCode: TVersaoQrCode;
      AVersaoXML: TVersaoNF3e): TVersaoQrCode;
    function GetURLConsultaNF3e(const CUF: integer;
      const TipoAmbiente: TACBrTipoAmbiente;
      const Versao: Double): String;
    function GetURLQRCode(const CUF: integer;
       const TipoAmbiente: TACBrTipoAmbiente; const TipoEmissao: TACBrTipoEmissao;
       const AChaveNF3e: String; const Versao: Double): String;

    function IdentificaSchema(const AXML: String): TSchemaNF3e;
    function GerarNomeArqSchema(const ALayOut: TLayOut; VersaoServico: Double): String;
    function GerarNomeArqSchemaEvento(ASchemaEventoNF3e: TSchemaNF3e; VersaoServico: Double): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property NotasFiscais: TNotasFiscais read FNotasFiscais write FNotasFiscais;
    property EventoNF3e: TEventoNF3e read FEventoNF3e write FEventoNF3e;
    property Status: TStatusNF3e read FStatus;

    procedure SetStatus(const stNewStatus: TStatusNF3e);
    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;

    function DistribuicaoDFePorUltNSU(AcUFAutor: integer; const ACNPJCPF,
      AultNSU: String): Boolean;
    function DistribuicaoDFePorNSU(AcUFAutor: integer; const ACNPJCPF,
      ANSU: String): Boolean;
    function DistribuicaoDFePorChaveNF3e(AcUFAutor: integer; const ACNPJCPF,
      AchNF3e: String): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmailEvento(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

  published
    property Configuracoes: TConfiguracoesNF3e
      read GetConfiguracoes write SetConfiguracoes;
    property DANF3e: TACBrNF3eDANF3eClass read FDANF3e write SetDANF3e;
  end;

implementation

uses
  dateutils, math,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrDFeSSL;

{$IFDEF FPC}
 {$R ACBrNF3eServicos.rc}
{$ELSE}
 {$R ACBrNF3eServicos.res}
{$ENDIF}

{ TACBrNF3e }

constructor TACBrNF3e.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNotasFiscais := TNotasFiscais.Create(Self, NotaFiscal);
  FEventoNF3e := TEventoNF3e.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrNF3e.Destroy;
begin
  FNotasFiscais.Free;
  FEventoNF3e.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrNF3e.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamNF3e: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stNF3eEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamNF3e, NomeArq,
      sReplyTo, sBCC);
  finally
    SetStatus( stIdle );
  end;
end;

procedure TACBrNF3e.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDANF3e <> nil) and
     (AComponent is TACBrNF3eDANF3eClass) then
    FDANF3e := nil;
end;

function TACBrNF3e.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesNF3e.Create(Self);
end;

procedure TACBrNF3e.SetDANF3e(const Value: TACBrNF3eDANF3eClass);
var
  OldValue: TACBrNF3eDANF3eClass;
begin
  if Value <> FDANF3e then
  begin
    if Assigned(FDANF3e) then
      FDANF3e.RemoveFreeNotification(Self);

    OldValue := FDANF3e;   // Usa outra variavel para evitar Loop Infinito
    FDANF3e := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrNF3e) then
        OldValue.ACBrNF3e := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrNF3e := self;
    end;
  end;
end;

function TACBrNF3e.GetNomeModeloDFe: String;
begin
  Result := 'NF3e';
end;

function TACBrNF3e.GetNameSpaceURI: String;
begin
  Result := ACBRNF3e_NAMESPACE;
end;

function TACBrNF3e.CstatConfirmada(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
  else
    Result := False;
  end;
end;

function TACBrNF3e.CstatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
  else
    Result := False;
  end;
end;

function TACBrNF3e.CstatCancelada(AValue: integer): Boolean;
begin
  case AValue of
    101, 135, 151, 155: Result := True;
  else
    Result := False;
  end;
end;

function TACBrNF3e.IdentificaSchema(const AXML: String): TSchemaNF3e;
var
  lTipoEvento: String;
  I: integer;
begin
  Result := schNF3e;
  I := pos('<infNF3e', AXML);

  if I = 0 then
  begin
    I := pos('<infInut', AXML);

    if I > 0 then
      Result := schInutNF3e
    else
    begin
      I := Pos('<infEvento', AXML);

      if I > 0 then
      begin
        lTipoEvento := Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>'));

        if lTipoEvento = '110111' then
          Result := schCancNF3e; // Cancelamento
      end;
    end;
  end;
end;

function TACBrNF3e.GerarNomeArqSchema(const ALayOut: TLayOut;
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

function TACBrNF3e.GerarNomeArqSchemaEvento(ASchemaEventoNF3e: TSchemaNF3e;
  VersaoServico: Double): String;
var
  xComplemento: string;
begin
  if VersaoServico = 0.0 then
    Result := ''
  else
  begin
    xComplemento := '';

    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaEventoToStr(ASchemaEventoNF3e) + xComplemento + '_v' +
              FloatToString(VersaoServico, '.', '0.00') + '.xsd';
  end;
end;

function TACBrNF3e.GetConfiguracoes: TConfiguracoesNF3e;
begin
  Result := TConfiguracoesNF3e(FPConfiguracoes);
end;

procedure TACBrNF3e.SetConfiguracoes(AValue: TConfiguracoesNF3e);
begin
  FPConfiguracoes := AValue;
end;

function TACBrNF3e.LerVersaoDeParams(LayOutServico: TLayOut): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoNF3eToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrNF3e.AjustarVersaoQRCode(AVersaoQRCode: TVersaoQrCode;
  AVersaoXML: TVersaoNF3e): TVersaoQrCode;
begin
  if (AVersaoXML <= ve100) then
    Result := veqr100
  else     // ve100 ou superior
    Result := TVersaoQrCode(max(Integer(AVersaoQRCode), Integer(veqr100)));
end;

procedure TACBrNF3e.LerServicoDeParams(LayOutServico: TLayOut;
  var Versao: Double; var URL: String; var Servico: String;
  var SoapAction: String);
var
  AUF: String;
begin
  case Configuracoes.Geral.FormaEmissao of
    teNormal: AUF := Configuracoes.WebServices.UF;
    teSVCAN: AUF := 'SVC-AN';
    teSVCRS: AUF := 'SVC-RS';
  else
    AUF := Configuracoes.WebServices.UF;
  end;

  Versao := VersaoNF3eToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  Servico := '';
  SoapAction := '';

  LerServicoDeParams(GetNomeModeloDFe, AUF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL, Servico, SoapAction);
end;

function TACBrNF3e.GetURLConsultaNF3e(const CUF: integer;
  const TipoAmbiente: TACBrTipoAmbiente; const Versao: Double): String;
var
  VersaoDFe: TVersaoNF3e;
  VersaoQrCode: TVersaoQrCode;
  ok: Boolean;
begin
  VersaoDFe := DblToVersaoNF3e(ok, Versao);
  VersaoQrCode := AjustarVersaoQRCode(Configuracoes.Geral.VersaoQRCode, VersaoDFe);

  Result := LerURLDeParams('NF3e', CUFtoUF(CUF), TpcnTipoAmbiente(TipoAmbiente),
    'URL-ConsultaNF3e', VersaoQrCodeToDbl(VersaoQrCode));
end;

function TACBrNF3e.GetURLQRCode(const CUF: integer;
  const TipoAmbiente: TACBrTipoAmbiente; const TipoEmissao: TACBrTipoEmissao;
  const AChaveNF3e: String; const Versao: Double): String;
var
  idNF3e, sEntrada, urlUF, Passo2, sign: String;
  VersaoDFe: TVersaoNF3e;
  VersaoQrCode: TVersaoQrCode;
  Ok: Boolean;
begin
  VersaoDFe := DblToVersaoNF3e(Ok, Versao);
  VersaoQrCode := AjustarVersaoQRCode(Configuracoes.Geral.VersaoQRCode, VersaoDFe);

  urlUF := LerURLDeParams('NF3e', CUFtoUF(CUF), TpcnTipoAmbiente(TipoAmbiente),
    'URL-QRCode', VersaoQrCodeToDbl(VersaoQrCode));

  if Pos('?', urlUF) <= 0 then
    urlUF := urlUF + '?';

  idNF3e := AChaveNF3e;

  // Passo 1
  sEntrada := 'chNF3e=' + idNF3e + '&tpAmb=' + TipoAmbienteToStr(TipoAmbiente);

  // Passo 2 calcular o SHA-1 da string idCTe se o Tipo de Emissão for EPEC ou FSDA
  if TpcnTipoEmissao(TipoEmissao) = teOffLine then
  begin
    // Tipo de Emissão em Contingência
    SSL.CarregarCertificadoSeNecessario;
    sign := SSL.CalcHash(idNF3e, dgstSHA1, outBase64, True);
    Passo2 := '&sign=' + sign;

    sEntrada := sEntrada + Passo2;
  end;

  Result := urlUF + sEntrada;
end;

function TACBrNF3e.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FEventoNF3e.Xml) then
    FEventoNF3e.GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FEventoNF3e.Xml));
  Result := True;
end;

procedure TACBrNF3e.SetStatus(const stNewStatus: TStatusNF3e);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;

    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrNF3e.Cancelamento(const AJustificativa: String; ALote: Int64 = 0): Boolean;
var
  i: integer;
begin
  if NotasFiscais.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NF3-e Informada!'));

  for i := 0 to NotasFiscais.Count - 1 do
  begin
    WebServices.Consulta.NF3eChave := NotasFiscais.Items[i].NumID;

    if not WebServices.Consulta.Executar then
      GerarException(WebServices.Consulta.Msg);

    EventoNF3e.Evento.Clear;
    with EventoNF3e.Evento.New do
    begin
      infEvento.CNPJ     := NotasFiscais.Items[i].NF3e.Emit.CNPJ;
      infEvento.cOrgao   := StrToIntDef(copy(OnlyNumber(WebServices.Consulta.NF3eChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chNF3e   := WebServices.Consulta.NF3eChave;

      infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;

    try
      EnviarEvento(ALote);
    except
      GerarException(WebServices.EnvEvento.EventoRetorno.xMotivo);
    end;
  end;

  Result := True;
end;

function TACBrNF3e.Consultar(const AChave: String; AExtrairEventos: Boolean): Boolean;
var
  i: integer;
begin
  if (NotasFiscais.Count = 0) and EstaVazio(AChave) then
    GerarException(ACBrStr('ERRO: Nenhuma NF3-e ou Chave Informada!'));

  if NaoEstaVazio(AChave) then
  begin
    NotasFiscais.Clear;
    WebServices.Consulta.NF3eChave      := AChave;
    WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    WebServices.Consulta.Executar;
  end
  else
  begin
    for i := 0 to NotasFiscais.Count - 1 do
    begin
      WebServices.Consulta.NF3eChave      := NotasFiscais.Items[i].NumID;
      WebServices.Consulta.ExtrairEventos := AExtrairEventos;
      WebServices.Consulta.Executar;
    end;
  end;

  Result := True;
end;

function TACBrNF3e.Enviar(ALote: Int64; Imprimir: Boolean = True;
  Sincrono: Boolean = False): Boolean;
begin
  Result := Enviar(IntToStr(ALote), Imprimir, Sincrono);
end;

function TACBrNF3e.Enviar(const ALote: String; Imprimir: Boolean;
  Sincrono: Boolean): Boolean;
var
  i: integer;
begin
  WebServices.Enviar.Clear;
  WebServices.Retorno.Clear;

  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NF3-e adicionada ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de NF3-e transmitidas (máximo de 50 NF3-e)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  NotasFiscais.Assinar;
  NotasFiscais.Validar;

  Result := WebServices.Envia(ALote, Sincrono);

  if DANF3e <> nil then
  begin
    for i := 0 to NotasFiscais.Count - 1 do
    begin
      if NotasFiscais.Items[i].Confirmada and Imprimir then
        NotasFiscais.Items[i].Imprimir;
    end;
  end;

end;

function TACBrNF3e.EnviarEvento(idLote: Int64): Boolean;
var
  i, j: integer;
  chNF3e: String;
begin
  if EventoNF3e.Evento.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum Evento adicionado ao Lote'));

  if EventoNF3e.Evento.Count > 20 then
    GerarException(ACBrStr('ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
      'excedido. Quantidade atual: ' + IntToStr(EventoNF3e.Evento.Count)));

  WebServices.EnvEvento.idLote := idLote;

  {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
  for i := 0 to EventoNF3e.Evento.Count - 1 do
  begin
    if EventoNF3e.Evento.Items[i].infEvento.nSeqEvento = 0 then
      EventoNF3e.Evento.Items[i].infEvento.nSeqEvento := 1;

    FEventoNF3e.Evento.Items[i].InfEvento.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);

    if NotasFiscais.Count > 0 then
    begin
      chNF3e := OnlyNumber(EventoNF3e.Evento.Items[i].InfEvento.chNF3e);

      // Se tem a chave da NF3e no Evento, procure por ela nas notas carregadas //
      if NaoEstaVazio(chNF3e) then
      begin
        for j := 0 to NotasFiscais.Count - 1 do
        begin
          if chNF3e = NotasFiscais.Items[j].NumID then
            Break;
        end ;

        if j = NotasFiscais.Count then
          GerarException( ACBrStr('Não existe NF3e com a chave ['+chNF3e+'] carregada') );
      end
      else
        j := 0;

      if trim(EventoNF3e.Evento.Items[i].InfEvento.CNPJ) = '' then
        EventoNF3e.Evento.Items[i].InfEvento.CNPJ := NotasFiscais.Items[j].NF3e.Emit.CNPJ;

      if chNF3e = '' then
        EventoNF3e.Evento.Items[i].InfEvento.chNF3e := NotasFiscais.Items[j].NumID;

      if trim(EventoNF3e.Evento.Items[i].infEvento.detEvento.nProt) = '' then
      begin
        if EventoNF3e.Evento.Items[i].infEvento.tpEvento = teCancelamento then
        begin
          EventoNF3e.Evento.Items[i].infEvento.detEvento.nProt := NotasFiscais.Items[j].NF3e.procNF3e.nProt;

          if trim(EventoNF3e.Evento.Items[i].infEvento.detEvento.nProt) = '' then
          begin
            WebServices.Consulta.NF3eChave := EventoNF3e.Evento.Items[i].InfEvento.chNF3e;

            if not WebServices.Consulta.Executar then
              GerarException(WebServices.Consulta.Msg);

            EventoNF3e.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
          end;
        end;
      end;
    end;
  end;

  Result := WebServices.EnvEvento.Executar;

  if not Result then
    GerarException( WebServices.EnvEvento.Msg );
end;

function TACBrNF3e.NomeServicoToNomeSchema(const NomeServico: String): String;
var
  ok: Boolean;
  ALayout: TLayOut;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);

  if ok then
    Result := SchemaNF3eToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

procedure TACBrNF3e.ImprimirEvento;
begin
  if not Assigned(DANF3e) then
    GerarException('Componente DANF3e não associado.')
  else
    DANF3e.ImprimirEVENTO(nil);
end;

procedure TACBrNF3e.ImprimirEventoPDF;
begin
  if not Assigned(DANF3e) then
    GerarException('Componente DANF3e não associado.')
  else
    DANF3e.ImprimirEVENTOPDF(nil);
end;

function TACBrNF3e.Distribuicao(AcUFAutor: integer; const ACNPJCPF, AultNSU, ANSU,
  chNF3e: String): Boolean;
begin
  {
  WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
  WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
  WebServices.DistribuicaoDFe.ultNSU   := AultNSU;
  WebServices.DistribuicaoDFe.NSU      := ANSU;
  WebServices.DistribuicaoDFe.chNF3e   := chNF3e;

  Result := WebServices.DistribuicaoDFe.Executar;

  if not Result then
    GerarException( WebServices.DistribuicaoDFe.Msg );
  }
end;

function TACBrNF3e.DistribuicaoDFePorUltNSU(AcUFAutor: integer; const ACNPJCPF,
  AultNSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, AultNSU, '', '');
end;

function TACBrNF3e.DistribuicaoDFePorNSU(AcUFAutor: integer; const ACNPJCPF,
  ANSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', ANSU, '');
end;

function TACBrNF3e.DistribuicaoDFePorChaveNF3e(AcUFAutor: integer; const ACNPJCPF,
  AchNF3e: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', '', AchNF3e);
end;

procedure TACBrNF3e.EnviarEmailEvento(const sPara, sAssunto: String;
  sMensagem: TStrings; sCC: TStrings; Anexos: TStrings;
  sReplyTo: TStrings);
var
  NomeArq: String;
  AnexosEmail: TStrings;
  StreamNF3e : TMemoryStream;
begin
  AnexosEmail := TStringList.Create;
  StreamNF3e := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;

    GravarStream(StreamNF3e);

    ImprimirEventoPDF;
    AnexosEmail.Add(DANF3E.ArquivoPDF);

    NomeArq := OnlyNumber(EventoNF3e.Evento[0].InfEvento.Id);
    EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNF3e,
  	  NomeArq + '-procEventoNF3e.xml', sReplyTo);
  finally
    AnexosEmail.Free;
    StreamNF3e.Free;
  end;
end;

end.

