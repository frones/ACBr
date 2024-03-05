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

unit ACBrBPe;

interface

uses
  Classes, SysUtils, synautil,
  ACBrXmlBase,
  ACBrBase,
  ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrBPeConfiguracoes, ACBrBPeWebServices, ACBrBPeBilhetes, ACBrBPeDABPEClass,
  ACBrBPeClass,
  pcnConversao,
  ACBrBPeConversao, ACBrBPeEnvEvento;

const
  ACBRBPE_NAMESPACE = 'http://www.portalfiscal.inf.br/bpe';
  ACBRBPE_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do ' +
               'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrBPeException = class(EACBrDFeException);

  { TACBrBPe }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBPe = class(TACBrDFe)
  private
    FDABPE: TACBrBPeDABPEClass;
    FBilhetes: TBilhetes;
    FEventoBPe: TEventoBPe;
    FStatus: TStatusACBrBPe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesBPe;
    function Distribuicao(AcUFAutor: Integer; const ACNPJCPF, AultNSU, ANSU,
      chBPe: String): Boolean;

    procedure SetConfiguracoes(AValue: TConfiguracoesBPe);
    procedure SetDABPE(const Value: TACBrBPeDABPEClass);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamBPe: TStream = nil; const NomeArq: String = '';
      sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function Enviar(ALote: Int64; Imprimir: Boolean = True): Boolean; overload;
    function Enviar(const ALote: String; Imprimir: Boolean = True): Boolean; overload;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;
    function EhAutorizacao(AVersao: TVersaoBPe; AUFCodigo: Integer): Boolean;

    function CstatConfirmada(AValue: Integer): Boolean;
    function CstatProcessado(AValue: Integer): Boolean;
    function CstatCancelada(AValue: Integer): Boolean;

    function Cancelamento(const AJustificativa: String; ALote: Int64 = 0): Boolean;
    function Consultar(const AChave: String = ''; AExtrairEventos: Boolean = False): Boolean;
    function EnviarEvento(idLote: Int64): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutBPe; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutBPe): String; reintroduce; overload;

    function GetURLConsultaBPe(const CUF: Integer;
      const TipoAmbiente: TACBrTipoAmbiente): String;
    function GetURLQRCode(const CUF: Integer; const TipoAmbiente: TACBrTipoAmbiente;
      const AChaveBPe: String): String;

    function IdentificaSchema(const AXML: String): TSchemaBPe;
    function GerarNomeArqSchema(const ALayOut: TLayOutBPe; VersaoServico: Double): String;
    function GerarNomeArqSchemaEvento(ASchemaEventoBPe: TSchemaBPe; VersaoServico: Double): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Bilhetes: TBilhetes read FBilhetes write FBilhetes;
    property EventoBPe: TEventoBPe read FEventoBPe write FEventoBPe;
    property Status: TStatusACBrBPe read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrBPe);
    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;

    function DistribuicaoDFe(AcUFAutor: Integer; const ACNPJCPF, AultNSU,
      ANSU: String; const AchBPe: String = ''): Boolean;
    function DistribuicaoDFePorUltNSU(AcUFAutor: Integer; const ACNPJCPF,
      AultNSU: String): Boolean;
    function DistribuicaoDFePorNSU(AcUFAutor: Integer; const ACNPJCPF,
      ANSU: String): Boolean;
    function DistribuicaoDFePorChaveBPe(AcUFAutor: Integer; const ACNPJCPF,
      AchBPe: String): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmailEvento(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

  published
    property Configuracoes: TConfiguracoesBPe
      read GetConfiguracoes write SetConfiguracoes;
    property DABPE: TACBrBPeDABPEClass read FDABPE write SetDABPE;
  end;

implementation

uses
  dateutils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeSSL;

{$IFDEF FPC}
 {$R ACBrBPeServicos.rc}
{$ELSE}
 {$R ACBrBPeServicos.res}
{$ENDIF}

{ TACBrBPe }

constructor TACBrBPe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBilhetes := TBilhetes.Create(Self, Bilhete);
  FEventoBPe := TEventoBPe.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrBPe.Destroy;
begin
  FBilhetes.Free;
  FEventoBPe.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrBPe.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamBPe: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stBPeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamBPe, NomeArq,
      sReplyTo, sBCC);
  finally
    SetStatus( stBPeIdle );
  end;
end;

procedure TACBrBPe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDABPE <> nil) and
    (AComponent is TACBrBPeDABPEClass) then
    FDABPE := nil;
end;

function TACBrBPe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesBPe.Create(Self);
end;

procedure TACBrBPe.SetDABPE(const Value: TACBrBPeDABPEClass);
var
  OldValue: TACBrBPeDABPEClass;
begin
  if Value <> FDABPE then
  begin
    if Assigned(FDABPE) then
      FDABPE.RemoveFreeNotification(Self);

    OldValue := FDABPE;   // Usa outra variavel para evitar Loop Infinito
    FDABPE := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrBPe) then
        OldValue.ACBrBPe := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrBPe := self;
    end;
  end;
end;

function TACBrBPe.GetNomeModeloDFe: String;
begin
//  Result := 'BPe';
  Result := ModeloBPeToPrefixo( Configuracoes.Geral.ModeloDF );
end;

function TACBrBPe.GetNameSpaceURI: String;
begin
  Result := ACBRBPe_NAMESPACE;
end;

function TACBrBPe.CstatConfirmada(AValue: Integer): Boolean;
begin
  case AValue of
    100, 102, 110, 150, 301, 302, 303: Result := True;
  else
      Result := False;
  end;
end;

function TACBrBPe.CstatProcessado(AValue: Integer): Boolean;
begin
  case AValue of
    100, 102, 110, 150, 301, 302, 303: Result := True;
  else
      Result := False;
  end;
end;

function TACBrBPe.CstatCancelada(AValue: Integer): Boolean;
begin
  case AValue of
    101, 151, 155: Result := True;
    else
      Result := False;
  end;
end;

function TACBrBPe.EhAutorizacao( AVersao: TVersaoBPe; AUFCodigo: Integer ): Boolean;
begin
  Result := True;
end;

function TACBrBPe.IdentificaSchema(const AXML: String): TSchemaBPe;
var
  lTipoEvento: String;
  I: Integer;
begin
  if Configuracoes.Geral.ModeloDF = moBPe then
    Result := schBPe
  else
    Result := schBPeTM;

  I := pos('<infBPe', AXML);

  if I = 0 then
  begin
    I := Pos('<infEvento', AXML);

    if I > 0 then
    begin
      lTipoEvento := Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>'));

      if lTipoEvento = '110111' then
        Result := schevCancBPe // Cancelamento
      else if lTipoEvento = '110115' then
        Result := schevNaoEmbBPe // Não Embarque
      else if lTipoEvento = '110116' then
        Result := schevAlteracaoPoltrona // Alteração de Poltrona
      else if lTipoEvento = '110117' then
        Result := schevExcessoBagagem; // Excesso de Bagagem
    end;
  end;
end;

function TACBrBPe.GerarNomeArqSchema(const ALayOut: TLayOutBPe;
  VersaoServico: Double): String;
var
  NomeServico, NomeSchema, ArqSchema: String;
  Versao: Double;
begin
  // Procura por Versão na pasta de Schemas //
  NomeServico := LayOutBPeToServico(ALayOut);
  NomeSchema := NomeServicoToNomeSchema(NomeServico);
  ArqSchema := '';

  if NaoEstaVazio(NomeSchema) then
  begin
    Versao := VersaoServico;
    AchaArquivoSchema( NomeSchema, Versao, ArqSchema );
  end;

  Result := ArqSchema;
end;

function TACBrBPe.GerarNomeArqSchemaEvento(ASchemaEventoBPe: TSchemaBPe;
  VersaoServico: Double): String;
begin
  if VersaoServico = 0.0 then
    Result := ''
  else
    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaBPeToStr(ASchemaEventoBPe) + '_v' +
              FloatToString(VersaoServico, '.', '0.00') + '.xsd';
end;

function TACBrBPe.GetConfiguracoes: TConfiguracoesBPe;
begin
  Result := TConfiguracoesBPe(FPConfiguracoes);
end;

procedure TACBrBPe.SetConfiguracoes(AValue: TConfiguracoesBPe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrBPe.LerVersaoDeParams(LayOutServico: TLayOutBPe): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutBPeToServico(LayOutServico),
    VersaoBPeToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrBPe.LerServicoDeParams(LayOutServico: TLayOutBPe;
  var Versao: Double; var URL: String);
var
  AUF: String;
begin
  case Configuracoes.Geral.FormaEmissao of
    pcnConversao.teNormal: AUF := Configuracoes.WebServices.UF;
    teSVCAN: AUF := 'SVC-AN';
    teSVCRS: AUF := 'SVC-RS';
  else
    AUF := Configuracoes.WebServices.UF;
  end;

  Versao := VersaoBPeToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  LerServicoDeParams(GetNomeModeloDFe, AUF,
    Configuracoes.WebServices.Ambiente, LayOutBPeToServico(LayOutServico),
    Versao, URL);
end;

function TACBrBPe.GetURLConsultaBPe(const CUF: Integer;
  const TipoAmbiente: TACBrTipoAmbiente): String;
begin
  Result := LerURLDeParams('BPe', CUFtoUF(CUF), TpcnTipoAmbiente(TipoAmbiente),
    'URL-ConsultaBPe', 0);
end;

function TACBrBPe.GetURLQRCode(const CUF: Integer;
  const TipoAmbiente: TACBrTipoAmbiente; const AChaveBPe: String): String;
var
  Passo1, Passo2, urlUF, idBPe, tpEmis, sign: String;
begin
  urlUF := LerURLDeParams('BPe', CUFtoUF(CUF), TpcnTipoAmbiente(TipoAmbiente), 'URL-QRCode', 0);
  idBPe := OnlyNumber(AChaveBPe);
  tpEmis := Copy(idBPe, 35, 1);

  Passo1 := urlUF;
  if Pos('?', urlUF) = 0 then
    Passo1 := Passo1 + '?';

  Passo1 := Passo1 + 'chBPe=' + idBPe + '&tpAmb=' + TipoAmbienteToStr(TipoAmbiente);
  Result := Passo1;

  if tpEmis <> '1' then
  begin
    // Tipo de Emissão em Contingência
    SSL.CarregarCertificadoSeNecessario;
    sign := SSL.CalcHash(idBPe, dgstSHA1, outBase64, True);
    Passo2 := '&sign='+sign;

    Result := Result + Passo2;
  end;
end;

function TACBrBPe.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FEventoBPe.Xml) then
    FEventoBPe.GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FEventoBPe.Xml));
  Result := True;
end;

procedure TACBrBPe.SetStatus(const stNewStatus: TStatusACBrBPe);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrBPe.Cancelamento(const AJustificativa: String; ALote: Int64 = 0): Boolean;
var
  i: Integer;
begin
  if Bilhetes.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum Bilhete Eletrônico Informado!'));

  for i := 0 to Bilhetes.Count - 1 do
  begin
    WebServices.Consulta.BPeChave := Bilhetes.Items[i].NumID;

    if not WebServices.Consulta.Executar then
      GerarException(WebServices.Consulta.Msg);

    EventoBPe.Evento.Clear;
    with EventoBPe.Evento.New do
    begin
      infEvento.CNPJ := Bilhetes.Items[i].BPe.Emit.CNPJ;
      infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(WebServices.Consulta.BPeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chBPe := WebServices.Consulta.BPeChave;
      infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;

    try
      EnviarEvento(ALote);
    except
      GerarException(WebServices.EnvEvento.EventoRetorno.retInfEvento.xMotivo);
    end;
  end;
  Result := True;
end;

function TACBrBPe.Consultar(const AChave: String; AExtrairEventos: Boolean): Boolean;
var
  i: Integer;
begin
  if (Bilhetes.Count = 0) and EstaVazio(AChave) then
    GerarException(ACBrStr('ERRO: Nenhum Bilhete Eletrônico ou Chave Informada!'));

  if NaoEstaVazio(AChave) then
  begin
    Bilhetes.Clear;
    WebServices.Consulta.BPeChave       := AChave;
    WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    WebServices.Consulta.Executar;
  end
  else
  begin
    for i := 0 to Bilhetes.Count - 1 do
    begin
      WebServices.Consulta.BPeChave       := Bilhetes.Items[i].NumID;
      WebServices.Consulta.ExtrairEventos := AExtrairEventos;
      WebServices.Consulta.Executar;
    end;
  end;

  Result := True;
end;

function TACBrBPe.Enviar(ALote: Int64; Imprimir: Boolean = True): Boolean;
begin
  Result := Enviar(IntToStr(ALote), Imprimir);
end;

function TACBrBPe.Enviar(const ALote: String; Imprimir: Boolean = True): Boolean;
var
  i: Integer;
begin
  WebServices.Enviar.Clear;

  if Bilhetes.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NF-e adicionada ao Lote'));

  if Bilhetes.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de NF-e transmitidas (máximo de 50 NF-e)' +
      ' excedido. Quantidade atual: ' + IntToStr(Bilhetes.Count)));

  Bilhetes.Assinar;
  Bilhetes.Validar;

  Result := WebServices.Envia(ALote);

  if DABPE <> nil then
  begin
    for i := 0 to Bilhetes.Count - 1 do
    begin
      if Bilhetes.Items[i].Confirmada and Imprimir then
        Bilhetes.Items[i].Imprimir;
    end;
  end;
end;

function TACBrBPe.EnviarEvento(idLote: Int64): Boolean;
var
  i, j: Integer;
  chBPe: String;
begin
  if EventoBPe.Evento.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum Evento adicionado ao Lote'));

  if EventoBPe.Evento.Count > 20 then
    GerarException(ACBrStr('ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
      'excedido. Quantidade atual: ' + IntToStr(EventoBPe.Evento.Count)));

  WebServices.EnvEvento.idLote := idLote;

  {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
  for i := 0 to EventoBPe.Evento.Count - 1 do
  begin
    if EventoBPe.Evento.Items[i].infEvento.nSeqEvento = 0 then
      EventoBPe.Evento.Items[i].infEvento.nSeqEvento := 1;

    FEventoBPe.Evento.Items[i].infEvento.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);

    if Bilhetes.Count > 0 then
    begin
      chBPe := OnlyNumber(EventoBPe.Evento.Items[i].infEvento.chBPe);

      // Se tem a chave da BPe no Evento, procure por ela nos bilhetes carregados //
      if NaoEstaVazio(chBPe) then
      begin
        For j := 0 to Bilhetes.Count - 1 do
        begin
          if chBPe = Bilhetes.Items[j].NumID then
            Break;
        end ;

        if j = Bilhetes.Count then
          GerarException( ACBrStr('Não existe BPe com a chave [' + chBPe + '] carregada') );
      end
      else
        j := 0;

      if trim(EventoBPe.Evento.Items[i].infEvento.CNPJ) = '' then
        EventoBPe.Evento.Items[i].infEvento.CNPJ := Bilhetes.Items[j].BPe.Emit.CNPJ;

      if chBPe = '' then
        EventoBPe.Evento.Items[i].infEvento.chBPe := Bilhetes.Items[j].NumID;

      if trim(EventoBPe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
      begin
        if EventoBPe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
        begin
          EventoBPe.Evento.Items[i].infEvento.detEvento.nProt := Bilhetes.Items[j].BPe.procBPe.nProt;

          if trim(EventoBPe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
          begin
            WebServices.Consulta.BPeChave := EventoBPe.Evento.Items[i].infEvento.chBPe;

            if not WebServices.Consulta.Executar then
              GerarException(WebServices.Consulta.Msg);

            EventoBPe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
          end;
        end;
      end;
    end;
  end;

  Result := WebServices.EnvEvento.Executar;

  if not Result then
    GerarException( WebServices.EnvEvento.Msg );
end;

function TACBrBPe.NomeServicoToNomeSchema(const NomeServico: String): String;
var
  ALayout: TLayOutBPe;
  Ok: Boolean;
begin
  ALayout := ServicoToLayOutBPe(Ok, NomeServico);
  if Ok then
    Result := SchemaBPeToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

procedure TACBrBPe.ImprimirEvento;
begin
  if not Assigned(DABPE) then
    GerarException('Componente DABPE não associado.')
  else
    DABPE.ImprimirEVENTO(nil);
end;

procedure TACBrBPe.ImprimirEventoPDF;
begin
  if not Assigned(DABPE) then
    GerarException('Componente DABPE não associado.')
  else
    DABPE.ImprimirEVENTOPDF(nil);
end;

function TACBrBPe.Distribuicao(AcUFAutor: Integer; const ACNPJCPF, AultNSU, ANSU,
  chBPe: String): Boolean;
begin
{
  WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
  WebServices.DistribuicaoDFe.CNPJCPF := ACNPJCPF;
  WebServices.DistribuicaoDFe.ultNSU := AultNSU;
  WebServices.DistribuicaoDFe.NSU := ANSU;
  WebServices.DistribuicaoDFe.chBPe := chBPe;

  Result := WebServices.DistribuicaoDFe.Executar;

  if not Result then
    GerarException( WebServices.DistribuicaoDFe.Msg );
  }
end;

function TACBrBPe.DistribuicaoDFe(AcUFAutor: Integer;
  const ACNPJCPF, AultNSU, ANSU: String; const AchBPe: String = ''): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, AultNSU, ANSU, AchBPe);
end;

function TACBrBPe.DistribuicaoDFePorUltNSU(AcUFAutor: Integer; const ACNPJCPF,
  AultNSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, AultNSU, '', '');
end;

function TACBrBPe.DistribuicaoDFePorNSU(AcUFAutor: Integer; const ACNPJCPF,
  ANSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', ANSU, '');
end;

function TACBrBPe.DistribuicaoDFePorChaveBPe(AcUFAutor: Integer; const ACNPJCPF,
  AchBPe: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', '', AchBPe);
end;

procedure TACBrBPe.EnviarEmailEvento(const sPara, sAssunto: String;
  sMensagem: TStrings; sCC: TStrings; Anexos: TStrings;
  sReplyTo: TStrings);
var
  NomeArq: String;
  AnexosEmail: TStrings;
  StreamBPe : TMemoryStream;
begin
  AnexosEmail := TStringList.Create;
  StreamBPe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;

    GravarStream(StreamBPe);

    ImprimirEventoPDF;
    AnexosEmail.Add(DABPE.ArquivoPDF);

    NomeArq := OnlyNumber(EventoBPe.Evento[0].InfEvento.Id);
    EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamBPe,
	    NomeArq + '-procEventoBPe.xml', sReplyTo);
  finally
    AnexosEmail.Free;
    StreamBPe.Free;
  end;
end;

end.

