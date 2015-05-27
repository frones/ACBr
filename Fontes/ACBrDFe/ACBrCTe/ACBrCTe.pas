{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Wiliam Zacarias da Silva Rosa          }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Desenvolvimento                                                              }
{         de Cte: Wiliam Zacarias da Silva Rosa                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrCTe;

interface

uses
  Classes, Sysutils,
  ACBrDFe, ACBrDFeConfiguracoes,
  ACBrCTeConfiguracoes, ACBrCTeWebServices, ACBrCTeConhecimentos,
  ACBrCTeDACTeClass,ACBrDFeException,
  pcteCTe, pcnConversao, pcteConversaoCTe,
  pcteEnvEventoCTe, pcteInutCTe,
  ACBrDFeUtil, ACBrUtil;

//  Forms,
//  smtpsend, ssl_openssl, mimemess, mimepart, // units para enviar email
//   pcteRetEnvEventoCTe,
//  ACBrCTeUtil,
//  pcteRetInutCTe;

const
  ACBRCTE_VERSAO = '2.0.0a';
  ACBRCTE_NAMESPACE = 'http://www.portalfiscal.inf.br/cte';

type
  EACBrCTeException = class(EACBrDFeException);

  { TACBrCTe }

  TACBrCTe = class(TACBrDFe)
  private
    FDACTe: TACBrCTeDACTEClass;
    FConhecimentos: TConhecimentos;
    FEventoCTe: TEventoCTe;
    FInutCTe: TInutCTe;
    FStatus: TStatusACBrCTe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesCTe;
    procedure SetConfiguracoes(AValue: TConfiguracoesCTe);
  	procedure SetDACTE(const Value: TACBrCTeDACTEClass);

  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetAbout: String; override;
    function GetNomeArquivoServicos: String; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamCTe: TStream = nil; NomeArq: String = ''); override;

    function Enviar(ALote: Integer; Imprimir: Boolean = True): Boolean;  overload;
    function Enviar(ALote: String; Imprimir: Boolean = True): Boolean;  overload;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmado(AValue: Integer): Boolean;
    function cStatProcessado(AValue: Integer): Boolean;

    function Cancelamento(AJustificativa: WideString; ALote: Integer = 0): Boolean;
    function Consultar: Boolean;
    function EnviarEvento(idLote: Integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutCTe; var Versao: Double; var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutCTe): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaCTe;
    function IdentificaSchemaModal(const AXML: String): TSchemaCTe;
    function IdentificaSchemaEvento(const AXML: String): TSchemaCTe;
    function IdentificaSchemaLayOut(const ALayOut: TLayOutCTe): TSchemaCTe;

    function GerarNomeArqSchema(const ALayOut: TLayOutCTe; VersaoServico: String): String;
    function GerarNomeArqSchemaModal(const AXML: String; VersaoServico: String): String;
    function GerarNomeArqSchemaEvento(const AXML: String; VersaoServico: String): String;

    function GerarChaveContingencia(FCTe: TCTe): String;

    property WebServices: TWebServices     read FWebServices   write FWebServices;
    property Conhecimentos: TConhecimentos read FConhecimentos write FConhecimentos;
    property EventoCTe: TEventoCTe         read FEventoCTe     write FEventoCTe;
    property InutCTe: TInutCTe             read FInutCTe       write FInutCTe;
    property Status: TStatusACBrCTe        read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrCTe);

    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;
    procedure ImprimirInutilizacao;
    procedure ImprimirInutilizacaoPDF;

  published
    property Configuracoes: TConfiguracoesCTe read GetConfiguracoes write SetConfiguracoes;
  	property DACTE: TACBrCTeDACTEClass        read FDACTE           write SetDACTE;
  end;

implementation

uses
  strutils, dateutils,
  pcnAuxiliar, synacode;

{$IFDEF FPC}
 {$R ACBrCTeServicos.rc}
{$ELSE}
 {$R ACBrCTeServicos.res ACBrCTeServicos.rc}
{$ENDIF}

{ TACBrCTe }

constructor TACBrCTe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FConhecimentos := TConhecimentos.Create(Self, Conhecimento);
  FEventoCTe := TEventoCTe.Create;
  FInutCTe := TInutCTe.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrCTe.Destroy;
begin
  FConhecimentos.Free;
  FEventoCTe.Free;
  FInutCTe.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrCTe.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamCTe: TStream; NomeArq: String);
begin
  SetStatus( stCTeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamCTe, NomeArq);
  finally
    SetStatus( stCTeIdle );
  end;
end;

procedure TACBrCTe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDACTE <> nil) and
     (AComponent is TACBrCTeDACTEClass) then
    FDACTE := nil;
end;

function TACBrCTe.GetAbout: String;
begin
  Result := 'ACBrCTe Ver: ' + ACBRCTE_VERSAO;
end;

function TACBrCTe.GetNomeArquivoServicos: String;
begin
  Result := 'ACBrCTeServicos.ini';
end;

function TACBrCTe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesCTe.Create(Self);
end;

procedure TACBrCTe.SetDACTE(const Value: TACBrCTeDACTEClass);
var
  OldValue: TACBrCTeDACTEClass;
begin
  if Value <> FDACTE then
  begin
     if Assigned(FDACTE) then
        FDACTE.RemoveFreeNotification(Self);

     OldValue := FDACTE;   // Usa outra variavel para evitar Loop Infinito
     FDACTE   := Value;    // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.ACBrCTe) then
           OldValue.ACBrCTe := nil;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        Value.ACBrCTe := self;
     end;
  end;
end;

function TACBrCTe.GetNomeModeloDFe: String;
begin
  Result := 'CTe';
end;

function TACBrCTe.GetNameSpaceURI: String;
begin
  Result := ACBRCTE_NAMESPACE;
end;

function TACBrCTe.cStatConfirmado(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrCTe.cStatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302: Result := True;
    else
      Result := False;
  end;
end;

function TACBrCTe.IdentificaSchema(const AXML: String): TSchemaCTe;
var
 lTipoEvento: TpcnTpEvento;
 I: Integer;
 Ok: Boolean;
begin
  Result := schCTe;

  I := pos('<infCte', AXML);
  if I = 0  then
  begin
    I := pos('<infInut', AXML);
    if I > 0 then
      Result := schInutCTe
    else begin
      I := pos('<infEvento', AXML);
      if I > 0 then
      begin
        lTipoEvento := StrToTpEvento(Ok, Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>')));

        case lTipoEvento of
          teCCe:          Result := schEventoCTe;
          teCancelamento: Result := schEventoCTe;
          teEPEC:         Result := schEventoCTe;
          teMultiModal:   Result := schEventoCTe;
          else            Result := schErro;
        end;
      end
      else
        Result := schErro;
    end;
  end;
end;

function TACBrCTe.IdentificaSchemaModal(const AXML: String): TSchemaCTe;
var
  XML: String;
  I: Integer;
begin
  XML := Trim(RetornarConteudoEntre(AXML, '<infModal', '</infModal>'));

  Result := schcteModalRodoviario;

  I := pos( '<rodo>', XML);
  if I = 0 then
  begin
    I := pos( '<aereo>', XML);
    if I> 0 then
      Result := schcteModalAereo
    else begin
      I := pos( '<aquav>', XML);
      if I> 0 then
        Result := schcteModalAquaviario
      else begin
        I := pos( '<duto>', XML);
        if I> 0 then
          Result := schcteModalDutoviario
        else begin
          I := pos( '<ferrov>', XML);
          if I> 0 then
            Result := schcteModalFerroviario
          else begin
            I := pos( '<multimodal>', XML);
            if I> 0 then
              Result := schcteMultiModal
            else
              Result := schErro;
          end;
        end;
      end;
    end;
  end;
end;

function TACBrCTe.IdentificaSchemaEvento(const AXML: String): TSchemaCTe;
begin
  // Implementar
end;

function TACBrCTe.IdentificaSchemaLayout(const ALayOut: TLayOutCTe): TSchemaCTe;
begin
  case ALayOut of
    LayCTeRecepcao:     Result := schCTe;
    LayCTeInutilizacao: Result := schInutCTe;
    LayCTeEvento:       Result := schEventoCTe;
    LayCTeEventoEPEC:   Result := schEventoCTe;
    else                Result := schErro;
  end;
end;

function TACBrCTe.GerarNomeArqSchema(const ALayOut: TLayOutCTe;
  VersaoServico: String): String;
begin
  if EstaVazio(VersaoServico) then
    VersaoServico := LerVersaoDeParams(ALayOut);

  Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
            SchemaCTeToStr(IdentificaSchemaLayout(ALayOut)) + '_v' +
            VersaoServico + '.xsd';
end;

function TACBrCTe.GerarNomeArqSchemaModal(const AXML: String;
  VersaoServico: String): String;
begin
  if EstaVazio(VersaoServico) then
    Result := ''
  else
    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaCTeToStr(IdentificaSchemaModal(AXML)) + '_v' +
              VersaoServico + '.xsd';
end;

function TACBrCTe.GerarNomeArqSchemaEvento(const AXML: String;
  VersaoServico: String): String;
begin
 // Implementar
end;

function TACBrCTe.GerarChaveContingencia(FCTe:TCTe): String;

  function GerarDigito_Contingencia(out Digito: Integer; chave: String): Boolean;
  var
    i, j: Integer;
  const
    PESO = '43298765432987654329876543298765432';
  begin
    chave  := OnlyNumber(chave);
    j      := 0;
    Digito := 0;
    result := True;
    try
      for i := 1 to 35 do
        j := j + StrToInt(copy(chave, i, 1)) * StrToInt(copy(PESO, i, 1));
      Digito := 11 - (j mod 11);
      if (j mod 11) < 2 then
        Digito := 0;
    except
      result := False;
    end;
    if length(chave) <> 35 then
      result := False;
  end;

var
  wchave: String;
  wicms_s, wicms_p: String;
  wd,wm,wa: word;
  Digito: Integer;
begin
  // Alterado Conforme NT 2012/007
  // UF
  // TpcteTomador = ( tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmOutros);
  if FCTe.Ide.toma4.CNPJCPF <> '' then
  begin
    if FCTe.Ide.toma4.enderToma.UF = 'EX' then
      wchave := '99' //exterior
    else
      wchave := copy(inttostr(FCTe.Ide.toma4.enderToma.cMun),1,2);
  end
  else begin
    case FCTe.Ide.toma03.Toma of
     tmRemetente: if FCTe.Rem.enderReme.UF = 'EX' then
                    wchave := '99' //exterior
                  else
                    wchave := copy(inttostr(FCTe.Rem.enderReme.cMun), 1, 2);
     tmExpedidor: if FCTe.Exped.enderExped.UF = 'EX' then
                    wchave := '99' //exterior
                  else
                    wchave := copy(inttostr(FCTe.Exped.enderExped.cMun), 1, 2);
     tmRecebedor: if FCTe.Receb.enderReceb.UF = 'EX' then
                    wchave := '99' //exterior
                  else
                    wchave := copy(inttostr(FCTe.Receb.enderReceb.cMun), 1, 2);
     tmDestinatario: if FCTe.Dest.EnderDest.UF = 'EX' then
                       wchave := '99' //exterior
                     else
                       wchave := copy(inttostr(FCTe.Dest.EnderDest.cMun), 1, 2);
    end;
  end;

  //TIPO DE EMISSAO
  case FCTe.Ide.tpEmis of
   teDPEC,
   teContingencia: wchave := wchave + '2';
   teFSDA:         wchave := wchave + '5';
   else            wchave := wchave + '0'; //esta valor caracteriza ERRO, valor tem q ser  2 ou 5
  end;

  //CNPJ OU CPF
  if FCTe.Ide.toma4.CNPJCPF <> '' then
  begin
    if FCTe.Ide.toma4.enderToma.UF = 'EX' then
      wchave := wchave + Poem_Zeros('0', 14)
    else
      wchave := wchave + Poem_Zeros(FCTe.Ide.toma4.CNPJCPF, 14);
  end
  else begin
    case FCTe.Ide.toma03.Toma of
     tmRemetente: if (FCTe.Rem.enderReme.UF='EX') then
                    wchave := wchave + Poem_Zeros('0', 14)
                  else
                    wchave := wchave + Poem_Zeros(FCTe.Rem.CNPJCPF, 14);
     tmExpedidor: if (FCTe.Exped.enderExped.UF='EX') then
                    wchave := wchave + Poem_Zeros('0', 14)
                  else
                    wchave := wchave + Poem_Zeros(FCTe.Exped.CNPJCPF, 14);
     tmRecebedor: if (FCTe.Receb.enderReceb.UF='EX') then
                    wchave := wchave + Poem_Zeros('0', 14)
                  else
                    wchave := wchave + Poem_Zeros(FCTe.Receb.CNPJCPF, 14);
     tmDestinatario: if (FCTe.Dest.EnderDest.UF='EX') then
                       wchave := wchave + Poem_Zeros('0', 14)
                     else
                       wchave := wchave + Poem_Zeros(FCTe.Dest.CNPJCPF, 14);
    end;
  end;

  //VALOR DA CT-e
  wchave := wchave + Poem_Zeros(OnlyNumber(FloatToStrf(FCTe.vPrest.vTPrest, ffFixed, 18, 2)), 14);

  //DESTAQUE ICMS PROPRIO E ST
  wicms_p := '2';
  wicms_s := '2';

  // Checar esse trecho

{$IFDEF PL_103}
  if (NaoEstaZerado(FCTe.Imp.ICMS.CST00.vICMS)) then
    wicms_p := '1';
  if (NaoEstaZerado(FCTe.Imp.ICMS.CST80.vICMS)) then
    wicms_s := '1';
{$ELSE}
  if (NaoEstaZerado(FCTe.Imp.ICMS.ICMS00.vICMS)) then
    wicms_p := '1';
  if (NaoEstaZerado(FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF)) then
    wicms_s := '1';
{$ENDIF}

  wchave := wchave + wicms_p + wicms_s;

  //DIA DA EMISSAO
  decodedate(FCTe.Ide.dhEmi, wa, wm, wd);
  wchave := wchave + Poem_Zeros(inttostr(wd), 2);

  //DIGITO VERIFICADOR
  GerarDigito_Contingencia(Digito, wchave);
  wchave := wchave + inttostr(digito);

  //RETORNA A CHAVE DE CONTINGENCIA
  result := wchave;
end;

function TACBrCTe.GetConfiguracoes: TConfiguracoesCTe;
begin
  Result := TConfiguracoesCTe(FPConfiguracoes);
end;

procedure TACBrCTe.SetConfiguracoes(AValue: TConfiguracoesCTe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrCTe.LerVersaoDeParams(LayOutServico: TLayOutCTe): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoCTeToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrCTe.LerServicoDeParams(LayOutServico: TLayOutCTe;
  var Versao: Double; var URL: String);
begin
  Versao := VersaoCTeToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  LerServicoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL);
end;

procedure TACBrCTe.SetStatus(const stNewStatus: TStatusACBrCTe);
begin
  if (stNewStatus <> FStatus) then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrCTe.Cancelamento(AJustificativa: WideString; ALote: Integer): Boolean;
var
  i: Integer;
begin
  if Self.Conhecimentos.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum Conhecimento Eletrônico Informado!'));

  for i := 0 to self.Conhecimentos.Count - 1 do
  begin
    Self.WebServices.Consulta.CTeChave :=
      OnlyNumber(self.Conhecimentos.Items[i].CTe.infCTe.ID);

    if not Self.WebServices.Consulta.Executar then
      raise Exception.Create(Self.WebServices.Consulta.Msg);

    Self.EventoCTe.Evento.Clear;
    with Self.EventoCTe.Evento.Add do
    begin
      infEvento.CNPJ := copy(OnlyNumber(Self.WebServices.Consulta.CTeChave), 7, 14);
      infEvento.cOrgao := StrToIntDef(
        copy(OnlyNumber(Self.WebServices.Consulta.CTeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chCTe := Self.WebServices.Consulta.CTeChave;
      infEvento.detEvento.nProt := Self.WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;

    try
      Self.EnviarEvento(ALote);
    except
      raise Exception.Create(Self.WebServices.EnvEvento.EventoRetorno.xMotivo);
    end;
  end;

  Result := True;
end;

function TACBrCTe.Consultar: Boolean;
var
  i: Integer;
begin
  if Self.Conhecimentos.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum Conhecimento Eletrônico Informado!'));

  for i := 0 to Self.Conhecimentos.Count - 1 do
  begin
    WebServices.Consulta.CTeChave :=
      OnlyNumber(self.Conhecimentos.Items[i].CTe.infCTe.ID);
    WebServices.Consulta.Executar;
  end;

  Result := True;
end;

function TACBrCTe.Enviar(ALote: Integer; Imprimir: Boolean = True): Boolean;
begin
  Result := Enviar(IntToStr(ALote), Imprimir);
end;

function TACBrCTe.Enviar(ALote: String; Imprimir: Boolean): Boolean;
var
  i: Integer;
begin
  if Conhecimentos.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum CT-e adicionad ao Lote'));

  if Conhecimentos.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de CT-e transmitidos (máximo de 50 CT-e)' +
      ' excedido. Quantidade atual: ' + IntToStr(Conhecimentos.Count)));

  Conhecimentos.Assinar;
  Conhecimentos.Validar;

  Result := WebServices.Envia(ALote);

  if DACTE <> nil then
  begin
     for i := 0 to Conhecimentos.Count-1 do
     begin
       if Conhecimentos.Items[i].Confirmado and Imprimir then
       begin
         Conhecimentos.Items[i].Imprimir;
       end;
     end;
  end;
end;

function TACBrCTe.EnviarEvento(idLote: Integer): Boolean;
var
  i: Integer;
begin
  if EventoCTe.Evento.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum Evento adicionado ao Lote'));

  if EventoCTe.Evento.Count > 20 then
    GerarException(ACBrStr('ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
      'excedido. Quantidade atual: ' + IntToStr(EventoCTe.Evento.Count)));

  WebServices.EnvEvento.idLote := idLote;

  {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
  for i := 0 to EventoCTe.Evento.Count -1 do
  begin
    try
      if EventoCTe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
        EventoCTe.Evento.Items[i].infEvento.nSeqEvento := 1;
      if self.Conhecimentos.Count > 0 then
       begin
         if trim(EventoCTe.Evento.Items[i].InfEvento.CNPJ) = '' then
           EventoCTe.Evento.Items[i].InfEvento.CNPJ := self.Conhecimentos.Items[i].CTe.Emit.CNPJ;
         if trim(EventoCTe.Evento.Items[i].InfEvento.chCTe) = '' then
           EventoCTe.Evento.Items[i].InfEvento.chCTe := copy(self.Conhecimentos.Items[i].CTe.infCTe.ID, (length(self.Conhecimentos.Items[i].CTe.infCTe.ID)-44)+1, 44);
         if trim(EventoCTe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
         begin
           if EventoCTe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
            begin
              EventoCTe.Evento.Items[i].infEvento.detEvento.nProt := self.Conhecimentos.Items[i].CTe.procCTe.nProt;
              if trim(EventoCTe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
               begin
                  WebServices.Consulta.CTeChave := EventoCTe.Evento.Items[i].InfEvento.chCTe;
                  if not WebServices.Consulta.Executar then
                    raise Exception.Create(WebServices.Consulta.Msg);
                  EventoCTe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
               end;
            end;
         end;
       end;
    except
    end;
  end;

  Result := WebServices.EnvEvento.Executar;

  if not Result then
    GerarException( WebServices.EnvEvento.Msg );
end;

procedure TACBrCTe.ImprimirEvento;
begin
  if not Assigned(DACTE) then
    raise EACBrCTeException.Create('Componente DACTE não associado.')
  else
    DACTE.ImprimirEVENTO(nil);
end;

procedure TACBrCTe.ImprimirEventoPDF;
begin
  if not Assigned(DACTE) then
    raise EACBrCTeException.Create('Componente DACTE não associado.')
  else
    DACTE.ImprimirEVENTOPDF(nil);
end;

procedure TACBrCTe.ImprimirInutilizacao;
begin
  if not Assigned(DACTE) then
    raise EACBrCTeException.Create('Componente DACTE não associado.')
  else
    DACTE.ImprimirINUTILIZACAO(nil);
end;

procedure TACBrCTe.ImprimirInutilizacaoPDF;
begin
  if not Assigned(DACTE) then
    raise EACBrCTeException.Create('Componente DACTE não associado.')
  else
    DACTE.ImprimirINUTILIZACAOPDF(nil);
end;

end.
