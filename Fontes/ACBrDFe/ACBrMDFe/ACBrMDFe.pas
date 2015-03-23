{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFe;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeConfiguracoes,
  ACBrMDFeConfiguracoes, ACBrMDFeWebServices, ACBrMDFeManifestos,
  ACBrMDFeDAMDFEClass,
  pcnMDFe, pcnConversao, pcnConversaoMDFe,
  pcnEnvEventoMDFe,
  ACBrUtil;

const
  ACBRMDFe_VERSAO = '2.0.0a';
  ACBRMDFE_NAMESPACE = 'http://www.portalfiscal.inf.br/mdfe';

type
  EACBrMDFeException = class(EACBrDFeException)

  TACBrMDFe = class(TACBrDFe)
  private
    FDAMDFE: TACBrMDFeDAMDFEClass;
    FManifestos: TManifestos;
    FEventoMDFe: TEventoMDFe;
    FStatus: TStatusACBrMDFe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesMDFe;
    procedure SetConfiguracoes(AValue: TConfiguracoesMDFe);
    procedure SetDAMDFE(const Value: TACBrMDFeDAMDFEClass);
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
      StreamMDFe: TStream = nil; NomeArq: String = ''); overload;

    function Enviar(ALote: integer; Imprimir: Boolean = True): Boolean; overload;
    function Enviar(ALote: String; Imprimir: Boolean = True): Boolean; overload;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmado(AValue: integer): Boolean;
    function cStatProcessado(AValue: integer): Boolean;

    function Cancelamento(AJustificativa: WideString; ALote: integer = 0): Boolean;
    function Consultar: Boolean;
    function ConsultarMDFeNaoEnc(ACNPJ: String): Boolean;
    function EnviarEvento(idLote: integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOut; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOut): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaMDFe;
    function IdentificaSchemaLayout(const ALayOut: TLayOut): TSchemaMDFe;
    function GerarNomeArqSchema(const ALayOut: TLayOut; VersaoServico: String): String;
    function GerarChaveContingencia(FMDFe: TMDFe): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Manifestos: TManifestos read FManifestos write FManifestos;
    property EventoMDFe: TEventoMDFe read FEventoMDFe write FEventoMDFe;
    property Status: TStatusACBrMDFe read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrMDFe);
    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;

  published
    property Configuracoes: TConfiguracoesMDFe
      read GetConfiguracoes write SetConfiguracoes;
    property DAMDFE: TACBrMDFeDAMDFEClass read FDAMDFE write SetDAMDFE;
  end;

procedure ACBrAboutDialog;

implementation

uses
  strutils, dateutils,
  pcnAuxiliar, synacode;

{$IFDEF FPC}
 {$R ACBrMDFeServicos.rc}
{$ELSE}
 {$R ACBrMDFeServicos.res ACBrMDFeServicos.rc}
{$ENDIF}

{ TACBrMDFe }

constructor TACBrMDFe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FManifestos := TManifestos.Create(Self, Manifesto);
  FEventoMDFe := TEventoMDFe.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrMDFe.Destroy;
begin
  FManifestos.Free;
  FEventoMDFe.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrMDFe.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamMDFe: TStream; NomeArq: String);
begin
  SetStatus( stMDFeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamMDFe, NomeArq);
  finally
    SetStatus( stMDFeIdle );
  end;
end;

procedure TACBrMDFe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDAMDFe <> nil) and
     (AComponent is TACBrMDFeDAMDFeClass) then
    FDAMDFe := nil;
end;

function TACBrMDFe.GetAbout: String;
begin
  Result := 'ACBrMDFe Ver: ' + ACBRMDFE_VERSAO;
end;

function TACBrMDFe.GetNomeArquivoServicos: String;
begin
  Result := 'ACBrServicosMDFe.ini';
end;

function TACBrMDFe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesMDFe.Create(Self);
end;

procedure TACBrMDFe.SetDAMDFe(const Value: TACBrMDFeDAMDFeClass);
var
 OldValue: TACBrMDFeDAMDFeClass;
begin
  if Value <> FDAMDFe then
  begin
    if Assigned(FDAMDFe) then
      FDAMDFe.RemoveFreeNotification(Self);

    OldValue := FDAMDFe;   // Usa outra variavel para evitar Loop Infinito
    FDAMDFe  := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrMDFe) then
        OldValue.ACBrMDFe := nil;

    if Value <> nil then
     begin
       Value.FreeNotification(self);
       Value.ACBrMDFe := self;
     end;
  end;
end;

function TACBrMDFe.GetNomeModeloDFe: String;
begin
  Result := 'MDFe';
end;

function TACBrMDFe.GetNameSpaceURI: String;
begin
  Result := ACBRMDFE_NAMESPACE;
end;

function TACBrMDFe.cStatConfirmado(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrMDFe.cStatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302: Result := True;
    else
      Result := False;
  end;
end;

function TACBrMDFe.IdentificaSchema(const AXML: String): TSchemaMDFe;
var
 lTipoEvento: TpcnTpEvento;
 I: Integer;
 Ok: Boolean;
begin
  Result := schMDFe;

  I := pos('<infMDFe', AXML);
  if I = 0 then
  begin
    I := pos('<infEvento', AXML);
    if I > 0 then
    begin
      lTipoEvento := StrToTpEvento(Ok, Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>')));

      case lTipoEvento of
        teCancelamento: Result := schEnvEventoCancMDFe;
        teEncerramento: Result := schEncerramentoMDFe;
        else Result := schIncusaoCondMDFe;
      end;
    end;
  end;
end;

function TACBrMDFe.IdentificaSchemaLayout(const ALayOut: TLayOut): TSchemaMDFe;
begin
  case ALayOut of
    LayMDFeRecepcao:
      Result := schMDFe;
    LayMDFeEvento:
      Result := schEncerramentoMDFe;
    else
      Result := schMDFe;
  end;
end;

function TACBrMDFe.GerarNomeArqSchema(const ALayOut: TLayOut;
  VersaoServico: String): String;
begin
  if EstaVazio(VersaoServico) then
    VersaoServico := LerVersaoDeParams(ALayOut);

  Result := SchemaMDFeToStr(IdentificaSchemaLayout(ALayOut)) + '_v' +
            VersaoServico + '.xsd';
end;

function TACBrMDFe.GerarChaveContingencia(FMDFe: TMDFe): String;

  function GerarDigito_Contigencia(out Digito: integer; chave: String): Boolean;
  var
    i, j: integer;
  const
    PESO = '43298765432987654329876543298765432';
  begin
    // Manual Integracao Contribuinte v2.02a - Página: 70 //
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

function TACBrMDFe.GetConfiguracoes: TConfiguracoesMDFe;
begin
  Result := TConfiguracoesMDFe(FPConfiguracoes);
end;

procedure TACBrMDFe.SetConfiguracoes(AValue: TConfiguracoesMDFe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrMDFe.LerVersaoDeParams(LayOutServico: TLayOut): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoMDFeToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrMDFe.LerServicoDeParams(LayOutServico: TLayOut;
  var Versao: Double; var URL: String);
begin
  Versao := VersaoMDFeToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  LerServicoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL);
end;

procedure TACBrMDFe.SetStatus(const stNewStatus: TStatusACBrMDFe);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrMDFe.Cancelamento(AJustificativa: WideString; ALote: integer = 0): Boolean;
var
  i: integer;
begin
  if Self.Manifestos.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum Manifesto Eletrônico Informado!'));

  for i := 0 to self.Manifestos.Count - 1 do
  begin
    Self.WebServices.Consulta.MDFeChave :=
      OnlyNumber(self.Manifestos.Items[i].MDFe.infMDFe.ID);

    if not Self.WebServices.Consulta.Executar then
      raise Exception.Create(Self.WebServices.Consulta.Msg);

    Self.EventoMDFe.Evento.Clear;
    with Self.EventoMDFe.Evento.Add do
    begin
      infEvento.CNPJ := copy(OnlyNumber(Self.WebServices.Consulta.MDFeChave), 7, 14);
      infEvento.cOrgao := StrToIntDef(
        copy(OnlyNumber(Self.WebServices.Consulta.MDFeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chMDFe := Self.WebServices.Consulta.MDFeChave;
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

function TACBrMDFe.Consultar: Boolean;
var
 i: Integer;
begin
  if Self.Manifestos.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum Manifesto Eletrônico Informado!'));

  for i := 0 to Self.Manifestos.Count - 1 do
  begin
    WebServices.Consulta.MDFeChave :=
      OnlyNumber(self.Manifestos.Items[i].MDFe.infMDFe.ID);
    WebServices.Consulta.Executar;
  end;

  Result := True;
end;

function TACBrMDFe.ConsultarMDFeNaoEnc(ACNPJ: String): Boolean;
begin
  Result := WebServices.ConsultaMDFeNaoEnc(ACNPJ);
end;

function TACBrMDFe.Enviar(ALote: Integer; Imprimir:Boolean = True): Boolean;
begin
  Result := Enviar(IntToStr(ALote), Imprimir);
end;

function TACBrMDFe.Enviar(ALote: String; Imprimir:Boolean = True): Boolean;
var
 i: Integer;
begin
  if Manifestos.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum MDF-e adicionado ao Lote'));

  if Manifestos.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de MDF-e transmitidos (máximo de 1 MDF-e)' +
      ' excedido. Quantidade atual: ' + IntToStr(Manifestos.Count)));

  Manifestos.Assinar;
  Manifestos.Validar;

  Result := WebServices.Envia(ALote);

  if DAMDFE <> nil then
  begin
    for i := 0 to Manifestos.Count - 1 do
    begin
      if Manifestos.Items[i].Confirmado and Imprimir then
      begin
        Manifestos.Items[i].Imprimir;
      end;
    end;
  end;
end;

function TACBrMDFe.EnviarEvento(idLote: Integer): Boolean;
var
  i: integer;
begin
  if EventoMDFe.Evento.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum Evento adicionado ao Lote'));

  if EventoMDFe.Evento.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de Eventos transmitidos (máximo de 1) ' +
      'excedido. Quantidade atual: ' + IntToStr(EventoMDFe.Evento.Count)));

  WebServices.EnvEvento.idLote := idLote;

  {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
  for i := 0 to EventoMDFe.Evento.Count - 1 do
  begin
    try
      if EventoMDFe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
        EventoMDFe.Evento.Items[i].infEvento.nSeqEvento := 1;
      if self.Manifestos.Count > 0 then
      begin
        if trim(EventoMDFe.Evento.Items[i].InfEvento.CNPJ) = '' then
          EventoMDFe.Evento.Items[i].InfEvento.CNPJ :=
            self.Manifestos.Items[i].MDFe.Emit.CNPJ;

        if trim(EventoMDFe.Evento.Items[i].InfEvento.chMDFe) = '' then
          EventoMDFe.Evento.Items[i].InfEvento.chMDFe :=
            copy(self.Manifestos.Items[i].MDFe.infMDFe.ID,
            (length(self.Manifestos.Items[i].MDFe.infMDFe.ID) - 44) + 1, 44);

        if trim(EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
        begin
          if EventoMDFe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
          begin
            EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt :=
              self.Manifestos.Items[i].MDFe.procMDFe.nProt;

            if trim(EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
            begin
              WebServices.Consulta.MDFeChave := EventoMDFe.Evento.Items[i].InfEvento.chMDFe;

              if not WebServices.Consulta.Executar then
                raise Exception.Create(WebServices.Consulta.Msg);

              EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt :=
                WebServices.Consulta.Protocolo;
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

procedure TACBrMDFe.ImprimirEvento;
begin
  if not Assigned(DAMDFE) then
     raise EACBrMDFeException.Create('Componente DAMDFE não associado.')
  else
     DAMDFE.ImprimirEVENTO(nil);
end;

procedure TACBrMDFe.ImprimirEventoPDF;
begin
  if not Assigned(DAMDFE) then
     raise EACBrMDFeException.Create('Componente DAMDFE não associado.')
  else
     DAMDFE.ImprimirEVENTOPDF(nil);
end;

end.
