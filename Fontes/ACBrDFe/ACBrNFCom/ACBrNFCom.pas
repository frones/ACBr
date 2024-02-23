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

unit ACBrNFCom;

interface

uses
  Classes, SysUtils, synautil,
  ACBrBase, ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrXmlBase,
  ACBrNFComConfiguracoes, ACBrNFComWebServices, ACBrNFComNotasFiscais,
  ACBrNFComDANFComClass,
//  ACBrDFeConversao,
  pcnConversao,
  ACBrNFComClass, ACBrNFComConversao, ACBrNFComEnvEvento;

const
  ACBRNFCom_NAMESPACE = 'http://www.portalfiscal.inf.br/nfcom';
  ACBRNFCom_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
     'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrNFComException = class(EACBrDFeException);

  { TACBrNFCom }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFCom = class(TACBrDFe)
  private
    FDANFCom: TACBrNFComDANFComClass;
    FNotasFiscais: TNotasFiscais;
    FEventoNFCom: TEventoNFCom;
    FStatus: TStatusNFCom;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesNFCom;

    procedure SetConfiguracoes(AValue: TConfiguracoesNFCom);
    procedure SetDANFCom(const Value: TACBrNFComDANFComClass);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: string): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: string;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamNFCom: TStream = nil; const NomeArq: string = '';
	    sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function GetNomeModeloDFe: string; override;
    function GetNameSpaceURI: string; override;

    function CstatConfirmada(AValue: integer): Boolean;
    function CstatProcessado(AValue: integer): Boolean;
    function CstatCancelada(AValue: integer): Boolean;

    function Enviar(Imprimir: Boolean = True): Boolean;
    function Cancelamento(const AJustificativa: string; ALote: Int64 = 0): Boolean;
    function Consultar(const AChave: string = ''; AExtrairEventos: Boolean = False): Boolean;
    function EnviarEvento(idLote: Int64): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutNFCom; var Versao: Double;
      var URL: string; var Servico: string; var SoapAction: string); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutNFCom): string; reintroduce; overload;

    function AjustarVersaoQRCode(AVersaoQRCode: TVersaoQrCode;
      AVersaoXML: TVersaoNFCom): TVersaoQrCode;

    function GetURLConsultaNFCom(const CUF: integer;
      const TipoAmbiente: TACBrTipoAmbiente;
      const Versao: Double): string;

    function GetURLQRCode(const CUF: integer;
      const TipoAmbiente: TACBrTipoAmbiente; const TipoEmissao: TACBrTipoEmissao;
      const AChaveNFCom: string; const Versao: Double): string;

    function IdentificaSchema(const AXML: string): TSchemaNFCom;
    function GerarNomeArqSchema(const ALayOut: TLayOutNFCom; VersaoServico: Double): string;
    function GerarNomeArqSchemaEvento(ASchemaEventoNFCom: TSchemaNFCom; VersaoServico: Double): string;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property NotasFiscais: TNotasFiscais read FNotasFiscais write FNotasFiscais;
    property EventoNFCom: TEventoNFCom read FEventoNFCom write FEventoNFCom;
    property Status: TStatusNFCom read FStatus;

    procedure SetStatus(const stNewStatus: TStatusNFCom);
    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmailEvento(const sPara, sAssunto: string;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

  published
    property Configuracoes: TConfiguracoesNFCom
      read GetConfiguracoes write SetConfiguracoes;
    property DANFCom: TACBrNFComDANFComClass read FDANFCom write SetDANFCom;
  end;

implementation

uses
  dateutils, math,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrDFeSSL;

{$IFDEF FPC}
 {$R ACBrNFComServicos.rc}
{$ELSE}
 {$R ACBrNFComServicos.res}
{$ENDIF}

{ TACBrNFCom }

constructor TACBrNFCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNotasFiscais := TNotasFiscais.Create(Self, NotaFiscal);
  FEventoNFCom := TEventoNFCom.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrNFCom.Destroy;
begin
  FNotasFiscais.Free;
  FEventoNFCom.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrNFCom.EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamNFCom: TStream; const NomeArq: string;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stNFComEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamNFCom,
      NomeArq, sReplyTo, sBCC);
  finally
    SetStatus( stNFComIdle );
  end;
end;

procedure TACBrNFCom.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDANFCom <> nil) and
     (AComponent is TACBrNFComDANFComClass) then
    FDANFCom := nil;
end;

function TACBrNFCom.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesNFCom.Create(Self);
end;

procedure TACBrNFCom.SetDANFCom(const Value: TACBrNFComDANFComClass);
var
  OldValue: TACBrNFComDANFComClass;
begin
  if Value <> FDANFCom then
  begin
    if Assigned(FDANFCom) then
      FDANFCom.RemoveFreeNotification(Self);

    OldValue := FDANFCom;   // Usa outra variavel para evitar Loop Infinito
    FDANFCom := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrNFCom) then
        OldValue.ACBrNFCom := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrNFCom := self;
    end;
  end;
end;

function TACBrNFCom.GetNomeModeloDFe: string;
begin
  Result := 'NFCom';
end;

function TACBrNFCom.GetNameSpaceURI: string;
begin
  Result := ACBRNFCom_NAMESPACE;
end;

function TACBrNFCom.CstatConfirmada(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
  else
    Result := False;
  end;
end;

function TACBrNFCom.CstatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
  else
    Result := False;
  end;
end;

function TACBrNFCom.CstatCancelada(AValue: integer): Boolean;
begin
  case AValue of
    101, 135, 151, 155: Result := True;
  else
    Result := False;
  end;
end;

function TACBrNFCom.IdentificaSchema(const AXML: string): TSchemaNFCom;
var
  lTipoEvento: string;
  I: integer;
begin
  Result := schNFCom;
  I := pos('<infNFCom', AXML);

  if I = 0 then
  begin
    I := Pos('<infEvento', AXML);

    if I > 0 then
    begin
      lTipoEvento := Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>'));

      if lTipoEvento = '110111' then
        Result := schevCancNFCom; // Cancelamento
    end;
  end;
end;

function TACBrNFCom.GerarNomeArqSchema(const ALayOut: TLayOutNFCom;
  VersaoServico: Double): string;
var
  NomeServico, NomeSchema, ArqSchema: string;
  Versao: Double;
begin
  // Procura por Versão na pasta de Schemas //
  NomeServico := LayOutNFComToServico(ALayOut);
  NomeSchema := NomeServicoToNomeSchema(NomeServico);
  ArqSchema := '';

  if NaoEstaVazio(NomeSchema) then
  begin
    Versao := VersaoServico;
    AchaArquivoSchema( NomeSchema, Versao, ArqSchema );
  end;

  Result := ArqSchema;
end;

function TACBrNFCom.GerarNomeArqSchemaEvento(ASchemaEventoNFCom: TSchemaNFCom;
  VersaoServico: Double): string;
var
  xComplemento: string;
begin
  if VersaoServico = 0.0 then
    Result := ''
  else
  begin
    xComplemento := '';

    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaEventoToStr(ASchemaEventoNFCom) + xComplemento + '_v' +
              FloatToString(VersaoServico, '.', '0.00') + '.xsd';
  end;
end;

function TACBrNFCom.GetConfiguracoes: TConfiguracoesNFCom;
begin
  Result := TConfiguracoesNFCom(FPConfiguracoes);
end;

procedure TACBrNFCom.SetConfiguracoes(AValue: TConfiguracoesNFCom);
begin
  FPConfiguracoes := AValue;
end;

function TACBrNFCom.LerVersaoDeParams(LayOutServico: TLayOutNFCom): string;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutNFComToServico(LayOutServico),
    VersaoNFComToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrNFCom.AjustarVersaoQRCode(AVersaoQRCode: TVersaoQrCode;
  AVersaoXML: TVersaoNFCom): TVersaoQrCode;
begin
  if (AVersaoXML <= ve100) then
    Result := veqr100
  else     // ve100 ou superior
    Result := TVersaoQrCode(max(Integer(AVersaoQRCode), Integer(veqr100)));
end;

procedure TACBrNFCom.LerServicoDeParams(LayOutServico: TLayOutNFCom;
  var Versao: Double; var URL: string; var Servico: string;
  var SoapAction: string);
var
  AUF: string;
begin
  case Configuracoes.Geral.FormaEmissao of
    teNormal: AUF := Configuracoes.WebServices.UF;
    teSVCAN: AUF := 'SVC-AN';
    teSVCRS: AUF := 'SVC-RS';
  else
    AUF := Configuracoes.WebServices.UF;
  end;

  Versao := VersaoNFComToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  Servico := '';
  SoapAction := '';

  LerServicoDeParams(GetNomeModeloDFe, AUF,
    Configuracoes.WebServices.Ambiente, LayOutNFComToServico(LayOutServico),
    Versao, URL, Servico, SoapAction);
end;

function TACBrNFCom.GetURLConsultaNFCom(const CUF: integer;
  const TipoAmbiente: TACBrTipoAmbiente; const Versao: Double): string;
var
  VersaoDFe: TVersaoNFCom;
  VersaoQrCode: TVersaoQrCode;
begin
  VersaoDFe := DblToVersaoNFCom(Versao);
  VersaoQrCode := AjustarVersaoQRCode(Configuracoes.Geral.VersaoQRCode, VersaoDFe);

  Result := LerURLDeParams('NFCom', CUFtoUF(CUF), TpcnTipoAmbiente(TipoAmbiente),
    'URL-ConsultaNFCom', VersaoQrCodeToDbl(VersaoQrCode));
end;

function TACBrNFCom.GetURLQRCode(const CUF: integer;
  const TipoAmbiente: TACBrTipoAmbiente; const TipoEmissao: TACBrTipoEmissao;
  const AChaveNFCom: string; const Versao: Double): string;
var
  idNFCom, sEntrada, urlUF, Passo2, sign: string;
  VersaoDFe: TVersaoNFCom;
  VersaoQrCode: TVersaoQrCode;
begin
  VersaoDFe := DblToVersaoNFCom(Versao);
  VersaoQrCode := AjustarVersaoQRCode(Configuracoes.Geral.VersaoQRCode, VersaoDFe);

  urlUF := LerURLDeParams('NFCom', CUFtoUF(CUF), TpcnTipoAmbiente(TipoAmbiente),
    'URL-QRCode', VersaoQrCodeToDbl(VersaoQrCode));

  if Pos('?', urlUF) <= 0 then
    urlUF := urlUF + '?';

  idNFCom := AChaveNFCom;

  // Passo 1
  sEntrada := 'chNFCom=' + idNFCom + '&tpAmb=' + TipoAmbienteToStr(TipoAmbiente);

  // Passo 2 calcular o SHA-1 da string idCTe se o Tipo de Emissão for EPEC ou FSDA
  if TipoEmissao = TACBrTipoEmissao(teOffLine) then
  begin
    // Tipo de Emissão em Contingência
    SSL.CarregarCertificadoSeNecessario;
    sign := SSL.CalcHash(idNFCom, dgstSHA1, outBase64, True);
    Passo2 := '&sign=' + sign;

    sEntrada := sEntrada + Passo2;
  end;

  Result := urlUF + sEntrada;
end;

function TACBrNFCom.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FEventoNFCom.Xml) then
    FEventoNFCom.GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FEventoNFCom.Xml));
  Result := True;
end;

procedure TACBrNFCom.SetStatus(const stNewStatus: TStatusNFCom);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;

    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrNFCom.Cancelamento(const AJustificativa: string; ALote: Int64 = 0): Boolean;
var
  i: integer;
begin
  if NotasFiscais.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NFCom-e Informada!'));

  for i := 0 to NotasFiscais.Count - 1 do
  begin
    WebServices.Consulta.NFComChave := NotasFiscais.Items[i].NumID;

    if not WebServices.Consulta.Executar then
      GerarException(WebServices.Consulta.Msg);

    EventoNFCom.Evento.Clear;
    with EventoNFCom.Evento.New do
    begin
      infEvento.CNPJ     := NotasFiscais.Items[i].NFCom.Emit.CNPJ;
      infEvento.cOrgao   := StrToIntDef(copy(OnlyNumber(WebServices.Consulta.NFComChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chNFCom  := WebServices.Consulta.NFComChave;

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

function TACBrNFCom.Consultar(const AChave: string; AExtrairEventos: Boolean): Boolean;
var
  i: integer;
begin
  if (NotasFiscais.Count = 0) and EstaVazio(AChave) then
    GerarException(ACBrStr('ERRO: Nenhuma NFCom-e ou Chave Informada!'));

  if NaoEstaVazio(AChave) then
  begin
    NotasFiscais.Clear;
    WebServices.Consulta.NFComChave := AChave;
    WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    WebServices.Consulta.Executar;
  end
  else
  begin
    for i := 0 to NotasFiscais.Count - 1 do
    begin
      WebServices.Consulta.NFComChave := NotasFiscais.Items[i].NumID;
      WebServices.Consulta.ExtrairEventos := AExtrairEventos;
      WebServices.Consulta.Executar;
    end;
  end;

  Result := True;
end;

function TACBrNFCom.Enviar(Imprimir: Boolean): Boolean;
var
  i: integer;
begin
  WebServices.Enviar.Clear;
//  WebServices.Retorno.Clear;

  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NFCom-e adicionada ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de NFCom-e transmitidas (máximo de 50 NFCom-e)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  NotasFiscais.Assinar;
  NotasFiscais.Validar;

  Result := WebServices.Envia;

  if DANFCom <> nil then
  begin
    for i := 0 to NotasFiscais.Count - 1 do
    begin
      if NotasFiscais.Items[i].Confirmada and Imprimir then
        NotasFiscais.Items[i].Imprimir;
    end;
  end;

end;

function TACBrNFCom.EnviarEvento(idLote: Int64): Boolean;
var
  i, j: integer;
  chNFCom: string;
begin
  if EventoNFCom.Evento.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum Evento adicionado ao Lote'));

  if EventoNFCom.Evento.Count > 20 then
    GerarException(ACBrStr('ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
      'excedido. Quantidade atual: ' + IntToStr(EventoNFCom.Evento.Count)));

  WebServices.EnvEvento.idLote := idLote;

  {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
  for i := 0 to EventoNFCom.Evento.Count - 1 do
  begin
    if EventoNFCom.Evento.Items[i].infEvento.nSeqEvento = 0 then
      EventoNFCom.Evento.Items[i].infEvento.nSeqEvento := 1;

    FEventoNFCom.Evento.Items[i].InfEvento.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);

    if NotasFiscais.Count > 0 then
    begin
      chNFCom := OnlyNumber(EventoNFCom.Evento.Items[i].InfEvento.chNFCom);

      // Se tem a chave da NFCom no Evento, procure por ela nas notas carregadas //
      if NaoEstaVazio(chNFCom) then
      begin
        for j := 0 to NotasFiscais.Count - 1 do
        begin
          if chNFCom = NotasFiscais.Items[j].NumID then
            Break;
        end ;

        if j = NotasFiscais.Count then
          GerarException( ACBrStr('Não existe NFCom com a chave ['+chNFCom+'] carregada') );
      end
      else
        j := 0;

      if trim(EventoNFCom.Evento.Items[i].InfEvento.CNPJ) = '' then
        EventoNFCom.Evento.Items[i].InfEvento.CNPJ := NotasFiscais.Items[j].NFCom.Emit.CNPJ;

      if chNFCom = '' then
        EventoNFCom.Evento.Items[i].InfEvento.chNFCom := NotasFiscais.Items[j].NumID;

      if trim(EventoNFCom.Evento.Items[i].infEvento.detEvento.nProt) = '' then
      begin
        if EventoNFCom.Evento.Items[i].infEvento.tpEvento = teCancelamento then
        begin
          EventoNFCom.Evento.Items[i].infEvento.detEvento.nProt := NotasFiscais.Items[j].NFCom.procNFCom.nProt;

          if trim(EventoNFCom.Evento.Items[i].infEvento.detEvento.nProt) = '' then
          begin
            WebServices.Consulta.NFComChave := EventoNFCom.Evento.Items[i].InfEvento.chNFCom;

            if not WebServices.Consulta.Executar then
              GerarException(WebServices.Consulta.Msg);

            EventoNFCom.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
          end;
        end;
      end;
    end;
  end;

  Result := WebServices.EnvEvento.Executar;

  if not Result then
    GerarException( WebServices.EnvEvento.Msg );
end;

function TACBrNFCom.NomeServicoToNomeSchema(const NomeServico: string): string;
var
  ALayout: TLayOutNFCom;
begin
  ALayout := ServicoToLayOutNFCom(NomeServico);

  Result := SchemaNFComToStr(LayOutNFComToSchema(ALayout))
end;

procedure TACBrNFCom.ImprimirEvento;
begin
  if not Assigned(DANFCom) then
    GerarException('Componente DANFCom não associado.')
  else
    DANFCom.ImprimirEVENTO(nil);
end;

procedure TACBrNFCom.ImprimirEventoPDF;
begin
  if not Assigned(DANFCom) then
    GerarException('Componente DANFCom não associado.')
  else
    DANFCom.ImprimirEVENTOPDF(nil);
end;

procedure TACBrNFCom.EnviarEmailEvento(const sPara, sAssunto: string;
  sMensagem: TStrings; sCC: TStrings; Anexos: TStrings;
  sReplyTo: TStrings);
var
  NomeArq: string;
  AnexosEmail: TStrings;
  StreamNFCom : TMemoryStream;
begin
  AnexosEmail := TStringList.Create;
  StreamNFCom := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;

    GravarStream(StreamNFCom);

    ImprimirEventoPDF;
    AnexosEmail.Add(DANFCom.ArquivoPDF);

    NomeArq := OnlyNumber(EventoNFCom.Evento[0].InfEvento.Id);

    EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFCom,
            	  NomeArq + '-procEventoNFCom.xml', sReplyTo);
  finally
    AnexosEmail.Free;
    StreamNFCom.Free;
  end;
end;

end.

