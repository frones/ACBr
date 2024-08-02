{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrDCe;

interface

uses
  Classes, SysUtils, synautil,
  ACBrDFe, ACBrDFeConfiguracoes, ACBrDFeException, ACBrBase,
  ACBrXmlBase, ACBrDCeConfiguracoes, ACBrDCeWebServices, ACBrDCeDeclaracoes,
  ACBrDCe.DACEClass, ACBrDCe.Classes, pcnConversao, ACBrDCe.Conversao,
  ACBrDCe.EventoClass,
  ACBrDCe.EnvEvento;

const
  ACBRDCE_NAMESPACE = 'http://www.portalfiscal.inf.br/dce';
  ACBRDCE_CErroAmbDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
     'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrDCeException = class(EACBrDFeException);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrDCe = class(TACBrDFe)
  private
    FDACE: TACBrDCeDACEClass;
    FDeclaracoes: TDeclaracoes;
    FEventoDCe: TEventoDCe;
    FStatus: TStatusDCe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesDCe;

    procedure SetConfiguracoes(AValue: TConfiguracoesDCe);
    procedure SetDACE(const Value: TACBrDCeDACEClass);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamDCe: TStream = nil; const NomeArq: String = ''; 
	  sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    procedure EnviarEmailEvento(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    function Enviar(const ALote: String; Imprimir: Boolean = True;
      Zipado: Boolean = True): Boolean;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmado(AValue: integer): Boolean;
    function cStatProcessado(AValue: integer): Boolean;
    function cStatCancelado(AValue: integer): Boolean;

    function Consultar(const AChave: String = ''; AExtrairEventos: Boolean = False): Boolean;
    function Cancelamento(const AJustificativa: String; ALote: integer = 0): Boolean;
    function EnviarEvento(idLote: integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutDCe; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutDCe): String; reintroduce; overload;

    function GetURLConsulta(const CUF: integer; const TipoAmbiente: TpcnTipoAmbiente;
      const Versao: Double): String;

    function GetURLQRCode(const CUF: integer; const TipoAmbiente: TpcnTipoAmbiente;
      const TipoEmissao: TpcnTipoEmissao; const AChaveDCe: String;
      const DocEmitente: string; const TipoEmitente: string;
      const Versao: Double): String;

    function IdentificaSchema(const AXML: String): TSchemaDCe;
    function IdentificaSchemaEvento(const AXML: String): TSchemaDCe;

    function GerarNomeArqSchema(const ALayOut: TLayOutDCe; VersaoServico: Double): String;
    function GerarNomeArqSchemaEvento(ASchemaEventoDCe: TSchemaDCe; VersaoServico: Double): String;

    function GerarChaveContingencia(FDCe: TDCe): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Declaracoes: TDeclaracoes read FDeclaracoes write FDeclaracoes;
    property EventoDCe: TEventoDCe read FEventoDCe write FEventoDCe;
    property Status: TStatusDCe read FStatus;

    procedure SetStatus(const stNewStatus: TStatusDCe);
    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;

  published
    property Configuracoes: TConfiguracoesDCe
      read GetConfiguracoes write SetConfiguracoes;
    property DACE: TACBrDCeDACEClass read FDACE write SetDACE;
  end;

implementation

uses
  dateutils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeSSL;

{$IFDEF FPC}
 {$R ACBrDCeServicos.rc}
{$ELSE}
 {$R ACBrDCeServicos.res}
{$ENDIF}

{ TACBrDCe }

constructor TACBrDCe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDeclaracoes := TDeclaracoes.Create(Self, TDeclaracao);
  FEventoDCe := TEventoDCe.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrDCe.Destroy;
begin
  FDeclaracoes.Free;
  FEventoDCe.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrDCe.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamDCe: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stDCeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamDCe, NomeArq,
      sReplyTo, sBCC);
  finally
    SetStatus( stDCeIdle );
  end;
end;

procedure TACBrDCe.EnviarEmailEvento(const sPara, sAssunto: String; sMensagem, sCC, Anexos, sReplyTo: TStrings);
var
  NomeArq: String;
  AnexosEmail: TStrings;
  StreamDCe : TMemoryStream;
begin
  AnexosEmail := TStringList.Create;
  StreamDCe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;

    GravarStream(StreamDCe);

    ImprimirEventoPDF;
    AnexosEmail.Add(DACE.ArquivoPDF);

    NomeArq := OnlyNumber(EventoDCe.Evento[0].InfEvento.Id);
    EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamDCe,
	    NomeArq + '-procEventoDCe.xml', sReplyTo);
  finally
    AnexosEmail.Free;
    StreamDCe.Free;
  end;
end;

procedure TACBrDCe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDACE <> nil) and
     (AComponent is TACBrDCeDACEClass) then
    FDACE := nil;
end;

function TACBrDCe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesDCe.Create(Self);
end;

procedure TACBrDCe.SetDACE(const Value: TACBrDCeDACEClass);
var
 OldValue: TACBrDCeDACEClass;
begin
  if Value <> FDACE then
  begin
    if Assigned(FDACE) then
      FDACE.RemoveFreeNotification(Self);

    OldValue := FDACE;   // Usa outra variavel para evitar Loop Infinito
    FDACE  := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrDCe) then
        OldValue.ACBrDCe := nil;

    if Value <> nil then
     begin
       Value.FreeNotification(self);
       Value.ACBrDCe := self;
     end;
  end;
end;

function TACBrDCe.GetNomeModeloDFe: String;
begin
  Result := 'DCe';
end;

function TACBrDCe.GetURLConsulta(const CUF: integer;
  const TipoAmbiente: TpcnTipoAmbiente; const Versao: Double): String;
//var
//  VersaoDFe: TVersaoDCe;
//  ok: Boolean;
begin
  // Se futuramente viermos a ter versões diferentes de URL de consulta
  // devemos descomentar as linhas e trocar o zero da função abaixo pela variável
  // VersaoDFe
//  VersaoDFe := DblToVersaoDCe(ok, Versao);
  Result := LerURLDeParams('DCe', CUFtoUF(CUF), TipoAmbiente, 'URL-Consulta', 0);
end;

function TACBrDCe.GetURLQRCode(const CUF: integer;
  const TipoAmbiente: TpcnTipoAmbiente; const TipoEmissao: TpcnTipoEmissao;
  const AChaveDCe: String; const DocEmitente: string; const TipoEmitente: string;
  const Versao: Double): String;
var
  idDCe,
  sEntrada, urlUF, Passo3, Passo4, Sign: String;
//  VersaoDFe: TVersaoDCe;
//  ok: Boolean;
begin
//  VersaoDFe := DblToVersaoDCe(ok, Versao);

  urlUF := LerURLDeParams('DCe', CUFtoUF(CUF), TipoAmbiente, 'URL-QRCode', 0);

  if Pos('?', urlUF) <= 0 then
    urlUF := urlUF + '?';

  idDCe := OnlyNumber(AChaveDCe);

  // Passo 1
  sEntrada := 'chDCe=' + idDCe + '&tpAmb=' + TpAmbToStr(TipoAmbiente);

  // Passo 2 calcular o SHA-1 da string idDCe se emissão em contingência
  if TipoEmissao = teContingencia then
  begin
    if TipoEmitente = 'J' then
      Passo3 := '&CNPJ=' + DocEmitente
    else
      if TipoEmitente = 'F' then
        Passo3 := '&CPF=' + DocEmitente
      else
        Passo3 := '&idOutros=' + DocEmitente;

    // Tipo de Emissão em Contingência
    SSL.CarregarCertificadoSeNecessario;
    Sign := SSL.CalcHash(idDCe, dgstSHA1, outBase64, True);
    Passo4 := '&sign='+Sign;

    sEntrada := sEntrada + Passo3 + Passo4;
  end;

  Result := urlUF + sEntrada;
end;

function TACBrDCe.GravarStream(AStream: TStream): Boolean;
begin
//  if EstaVazio(FEventoDCe.Gerador.ArquivoFormatoXML) then
//    FEventoDCe.GerarXML;

  AStream.Size := 0;
//  WriteStrToStream(AStream, AnsiString(FEventoDCe.Gerador.ArquivoFormatoXML));
  Result := True;
end;

function TACBrDCe.GetNameSpaceURI: String;
begin
  Result := ACBRDCE_NAMESPACE;
end;

function TACBrDCe.cStatConfirmado(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrDCe.cStatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302: Result := True;
    else
      Result := False;
  end;
end;

function TACBrDCe.cStatCancelado(AValue: integer): Boolean;
begin
  case AValue of
    101, 151, 155: Result := True;
    else
      Result := False;
  end;
end;

function TACBrDCe.IdentificaSchema(const AXML: String): TSchemaDCe;
var
 lTipoEvento: TpcnTpEvento;
 I: Integer;
 Ok: Boolean;
begin
  Result := schDCe;

  I := pos('<infDCe', AXML);
  if I = 0 then
  begin
    I := pos('<infEvento', AXML);
    if I > 0 then
    begin
      lTipoEvento := StrToTpEventoDCe(Ok, Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>')));

      case lTipoEvento of
        teCancelamento: Result := schevCancDCe;
      else 
        Result := schErroDCe;
      end;
    end;
  end;
end;

function TACBrDCe.IdentificaSchemaEvento(const AXML: String): TSchemaDCe;
begin
  // Implementar
  Result := schErroDCe;
end;

function TACBrDCe.GerarNomeArqSchema(const ALayOut: TLayOutDCe;
  VersaoServico: Double): String;
var
  NomeServico, NomeSchema, ArqSchema: String;
  Versao: Double;
begin
  // Procura por Versão na pasta de Schemas //
  NomeServico := LayOutDCeToServico(ALayOut);
  NomeSchema := NomeServicoToNomeSchema(NomeServico);
  ArqSchema := '';
  if NaoEstaVazio(NomeSchema) then
  begin
    Versao := VersaoServico;
    AchaArquivoSchema( NomeSchema, Versao, ArqSchema );
  end;

  Result := ArqSchema;
end;

function TACBrDCe.GerarNomeArqSchemaEvento(ASchemaEventoDCe: TSchemaDCe;
  VersaoServico: Double): String;
begin
  if VersaoServico = 0 then
    Result := ''
  else
    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaDCeToStr(ASchemaEventoDCe) + '_v' +
              FloatToString(VersaoServico, '.', '0.00') + '.xsd';
end;

function TACBrDCe.GerarChaveContingencia(FDCe: TDCe): String;

  function GerarDigito_Contigencia(out Digito: integer; chave: String): Boolean;
  var
    i, j: integer;
  const
    PESO = '43298765432987654329876543298765432';
  begin
    chave := OnlyNumber(chave);
    j := 0;
    Digito := 0;
    Result := True;
    try
      for i := 1 to 35 do
        j := j + StrToInt(copy(chave, i, 1)) * StrToInt(copy(PESO, i, 1));
      Digito := 11 - (j mod 11);
      if (j mod 11) < 2 then
        Digito := 0;
    except
      Result := False;
    end;
    if length(chave) <> 35 then
      Result := False;
  end;

var
  wchave: String;
//  Digito: integer;
begin
  wchave := '';

  Result := wchave;
end;

function TACBrDCe.GetConfiguracoes: TConfiguracoesDCe;
begin
  Result := TConfiguracoesDCe(FPConfiguracoes);
end;

procedure TACBrDCe.SetConfiguracoes(AValue: TConfiguracoesDCe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrDCe.LerVersaoDeParams(LayOutServico: TLayOutDCe): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutDCeToServico(LayOutServico),
    VersaoDCeToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrDCe.NomeServicoToNomeSchema(const NomeServico: String): String;
var
  ALayout: TLayOutDCe;
begin
  ALayout := ServicoToLayOutDCe(NomeServico);
  Result := SchemaDCeToStr(LayOutDCeToSchema(ALayout));
end;

procedure TACBrDCe.LerServicoDeParams(LayOutServico: TLayOutDCe;
  var Versao: Double; var URL: String);
begin
  Versao := VersaoDCeToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  LerServicoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutDCeToServico(LayOutServico),
    Versao, URL);
end;

procedure TACBrDCe.SetStatus(const stNewStatus: TStatusDCe);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrDCe.Cancelamento(const AJustificativa: String; ALote: integer = 0): Boolean;
var
  i: integer;
begin
  if Declaracoes.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum DC-e Informado!'));

  for i := 0 to Declaracoes.Count - 1 do
  begin
    WebServices.Consulta.DCeChave := Declaracoes.Items[i].NumID;

    if not WebServices.Consulta.Executar then
      raise Exception.Create(WebServices.Consulta.Msg);

    EventoDCe.Evento.Clear;
    with EventoDCe.Evento.New do
    begin
      infEvento.CNPJCPF := Declaracoes.Items[i].DCe.Emit.CNPJCPF;
      infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(WebServices.Consulta.DCeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chDCe := WebServices.Consulta.DCeChave;

      infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;

    try
      EnviarEvento(ALote);
    except
      raise Exception.Create(WebServices.EnvEvento.EventoRetorno.retInfEvento.xMotivo);
    end;
  end;

  Result := True;
end;

function TACBrDCe.Consultar(const AChave: String; AExtrairEventos: Boolean): Boolean;
var
 i: Integer;
begin
  if (Declaracoes.Count = 0) and EstaVazio(AChave) then
    GerarException(ACBrStr('ERRO: Nenhum DC-e ou Chave Informada!'));

  if NaoEstaVazio(AChave) then
  begin
    Declaracoes.Clear;
    WebServices.Consulta.DCeChave := AChave;
    WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    WebServices.Consulta.Executar;
  end
  else
  begin
    for i := 0 to Declaracoes.Count - 1 do
    begin
      WebServices.Consulta.DCeChave := Declaracoes.Items[i].NumID;
      WebServices.Consulta.ExtrairEventos := AExtrairEventos;
      WebServices.Consulta.Executar;
    end;
  end;

  Result := True;
end;

function TACBrDCe.Enviar(const ALote: String; Imprimir:Boolean = True;
  Zipado: Boolean = True): Boolean;
var
 i: Integer;
begin
  WebServices.Enviar.Clear;

  if Declaracoes.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum DC-e adicionado ao Lote'));

  if Declaracoes.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de DC-e transmitidos (máximo de 1 DC-e)' +
      ' excedido. Quantidade atual: ' + IntToStr(Declaracoes.Count)));

  Declaracoes.Assinar;
  Declaracoes.Validar;

  Result := WebServices.Envia(ALote, Zipado);

  if DACE <> nil then
  begin
    for i := 0 to Declaracoes.Count - 1 do
    begin
      if Declaracoes.Items[i].Confirmado and Imprimir then
        Declaracoes.Items[i].Imprimir;
    end;
  end;
end;

function TACBrDCe.EnviarEvento(idLote: Integer): Boolean;
var
  i, j: integer;
  chDCe: String;
begin
  if EventoDCe.Evento.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum Evento adicionado ao Lote'));

  if EventoDCe.Evento.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de Eventos transmitidos (máximo de 1) ' +
      'excedido. Quantidade atual: ' + IntToStr(EventoDCe.Evento.Count)));

  WebServices.EnvEvento.idLote := idLote;

  {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
  for i := 0 to EventoDCe.Evento.Count - 1 do
  begin
    if EventoDCe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
      EventoDCe.Evento.Items[i].infEvento.nSeqEvento := 1;

    FEventoDCe.Evento.Items[i].InfEvento.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);

    if Declaracoes.Count > 0 then
    begin
      chDCe := OnlyNumber(EventoDCe.Evento.Items[i].InfEvento.chDCe);

      // Se tem a chave do DCe no Evento, procure por ela nos Declaracoes carregados //
      if NaoEstaVazio(chDCe) then
      begin
        for j := 0 to Declaracoes.Count - 1 do
        begin
          if chDCe = Declaracoes.Items[j].NumID then
            Break;
        end;

        if j = Declaracoes.Count then
          GerarException( ACBrStr('Não existe DCe com a chave ['+chDCe+'] carregado') );
      end
      else
        j := 0;

      if trim(EventoDCe.Evento.Items[i].InfEvento.CNPJCPF) = '' then
        EventoDCe.Evento.Items[i].InfEvento.CNPJCPF := Declaracoes.Items[j].DCe.Emit.CNPJCPF;

      if chDCe = '' then
        EventoDCe.Evento.Items[i].InfEvento.chDCe := Declaracoes.Items[j].NumID;

      if trim(EventoDCe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
      begin
        if EventoDCe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
        begin
          EventoDCe.Evento.Items[i].infEvento.detEvento.nProt := Declaracoes.Items[j].DCe.procDCe.nProt;

          if trim(EventoDCe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
          begin
            WebServices.Consulta.DCeChave := EventoDCe.Evento.Items[i].InfEvento.chDCe;

            if not WebServices.Consulta.Executar then
              GerarException(WebServices.Consulta.Msg);

            EventoDCe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
          end;
        end;
      end;
    end;
  end;

  Result := WebServices.EnvEvento.Executar;

  if not Result then
    GerarException( WebServices.EnvEvento.Msg );
end;

procedure TACBrDCe.ImprimirEvento;
begin
  if not Assigned(DACE) then
     raise EACBrDCeException.Create('Componente DACE não associado.')
  else
     DACE.ImprimirEVENTO(nil);
end;

procedure TACBrDCe.ImprimirEventoPDF;
begin
  if not Assigned(DACE) then
     raise EACBrDCeException.Create('Componente DACE não associado.')
  else
     DACE.ImprimirEVENTOPDF(nil);
end;

end.
