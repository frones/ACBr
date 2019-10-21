
{$I ACBr.inc}

unit ACBrCIOT;

interface

uses
  Classes, SysUtils,
  ACBrUtil, ACBrDFe, ACBrDFeConfiguracoes, ACBrDFeException,
  ACBrCIOTConfiguracoes, ACBrCIOTWebServices, ACBrCIOTContratos,
  pcnConversao, pcnCIOT, pcnConversaoCIOT;

const
  ACBRCIOT_VERSAO = '1.0.0a';
  ACBRCIOT_NAMESPACE = 'ATMWenSvr';
  ACBRCIOT_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
               'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrCIOTException = class(EACBrDFeException);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}

  TACBrCIOT = class(TACBrDFe)
  private
    FContratos: TContratos;
    FStatus: TStatusACBrCIOT;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesCIOT;
    procedure SetConfiguracoes(AValue: TConfiguracoesCIOT);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamCIOT: TStream = nil; const NomeArq: String = ''; sReplyTo: TStrings = nil); override;

    function Enviar: Boolean;

    function IntegrarMotorista: Boolean;
    function AverbarNFe: Boolean;
    function DeclararMDFe: Boolean;
    function AddBackMail: Boolean;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmado(AValue: integer): Boolean;
    function cStatProcessado(AValue: integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutCIOT; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutCIOT): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaCIOT;

    function GerarNomeArqSchema(const ALayOut: TLayOutCIOT; VersaoServico: Double): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property Contratos: TContratos read FContratos write FContratos;
    property Status: TStatusACBrCIOT read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrCIOT);

  published
    property Configuracoes: TConfiguracoesCIOT read GetConfiguracoes write SetConfiguracoes;
  end;

implementation

uses
  strutils, dateutils,
  pcnAuxiliar, synacode;

{$IFDEF FPC}
 {$R ACBrCIOTServicos.rc}
{$ELSE}
 {$R ACBrCIOTServicos.res}
{$ENDIF}

{ TACBrCIOT }

constructor TACBrCIOT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FContratos := TContratos.Create(Self, Contrato);
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrCIOT.Destroy;
begin
  FContratos.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrCIOT.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamCIOT: TStream; const NomeArq: String;
  sReplyTo: TStrings);
begin
  SetStatus( stCIOTEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamCIOT, NomeArq,
      sReplyTo);
  finally
    SetStatus( stCIOTIdle );
  end;
end;

procedure TACBrCIOT.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  {
  if (Operation = opRemove) and (FDACIOT <> nil) and
     (AComponent is TACBrCIOTDACIOTClass) then
    FDACIOT := nil;
 }
end;

function TACBrCIOT.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesCIOT.Create(Self);
end;

function TACBrCIOT.GetNomeModeloDFe: String;
begin
  Result := 'CIOT';
end;

function TACBrCIOT.GetNameSpaceURI: String;
begin
  Result := ACBRCIOT_NAMESPACE;
end;

function TACBrCIOT.cStatConfirmado(AValue: integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrCIOT.cStatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302: Result := True;
    else
      Result := False;
  end;
end;

function TACBrCIOT.IdentificaSchema(const AXML: String): TSchemaCIOT;
//var
// lTipoEvento: TpcnTpEvento;
// I: Integer;
// Ok: Boolean;
begin
  Result := schEnviar;
(*
  Result := schCIOT;

  I := pos('<infCIOT', AXML);
  if I = 0 then
  begin
    I := pos('<infEvento', AXML);
    if I > 0 then
    begin
      lTipoEvento := StrToTpEvento(Ok, Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>')));

      case lTipoEvento of
        teCancelamento: Result := schevCancCIOT;
        teEncerramento: Result := schevEncCIOT;
        else Result := schevIncCondutorCIOT;
      end;
    end;
  end;
*)
end;

function TACBrCIOT.GerarNomeArqSchema(const ALayOut: TLayOutCIOT;
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

function TACBrCIOT.GetConfiguracoes: TConfiguracoesCIOT;
begin
  Result := TConfiguracoesCIOT(FPConfiguracoes);
end;

procedure TACBrCIOT.SetConfiguracoes(AValue: TConfiguracoesCIOT);
begin
  FPConfiguracoes := AValue;
end;

function TACBrCIOT.LerVersaoDeParams(LayOutServico: TLayOutCIOT): String;
var
  Versao: Double;
  UF: String;
begin
  // Para qual quer UF a URL é sempre a mesma.
  UF := 'XX';
  Versao := LerVersaoDeParams(GetNomeModeloDFe, UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoCIOTToint(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrCIOT.NomeServicoToNomeSchema(const NomeServico: String): String;
//var
//  ok: Boolean;
//  ALayout: TLayOutCIOT;
begin
  Result := '';
  (*
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaCIOTToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
  *)
end;

procedure TACBrCIOT.LerServicoDeParams(LayOutServico: TLayOutCIOT;
  var Versao: Double; var URL: String);
var
  UF: String;
begin
  Versao := VersaoCIOTToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  // Para qual quer UF a URL é sempre a mesma.
  UF := 'XX';
  LerServicoDeParams(GetNomeModeloDFe, UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL);
end;

procedure TACBrCIOT.SetStatus(const stNewStatus: TStatusACBrCIOT);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrCIOT.Enviar: Boolean;
begin
  if Contratos.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum CIOT adicionado'));

  if Contratos.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de CIOT transmitidos (máximo de 1 CIOT)' +
      ' excedido. Quantidade atual: ' + IntToStr(Contratos.Count)));

  Contratos.Assinar;

  Result := WebServices.Envia;
end;

function TACBrCIOT.IntegrarMotorista: Boolean;
begin
  Result := True;

//  if Contratos.Count <= 0 then
//    GerarException(ACBrStr('ERRO: Nenhum CIOT adicionado'));
//
//  if Contratos.Count > 1 then
//    GerarException(ACBrStr('ERRO: Conjunto de CIOT transmitidos (máximo de 1 CIOT)' +
//      ' excedido. Quantidade atual: ' + IntToStr(Contratos.Count)));

  Contratos.Assinar;
end;

function TACBrCIOT.AverbarNFe: Boolean;
begin
  Result := True;

  if Contratos.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum CIOT adicionado'));

  if Contratos.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de CIOT transmitidos (máximo de 1 CIOT)' +
      ' excedido. Quantidade atual: ' + IntToStr(Contratos.Count)));

  Contratos.Assinar;
end;

function TACBrCIOT.DeclararMDFe: Boolean;
begin
  Result := True;

  if Contratos.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum CIOT adicionado'));

  if Contratos.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de CIOT transmitidos (máximo de 1 CIOT)' +
      ' excedido. Quantidade atual: ' + IntToStr(Contratos.Count)));

  Contratos.Assinar;
end;

function TACBrCIOT.AddBackMail: Boolean;
begin
  {a}
  Result := True;
end;

end.
