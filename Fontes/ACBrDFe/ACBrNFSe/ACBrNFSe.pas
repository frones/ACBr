{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSe;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrNFSeDANFSEClass,
  ACBrNFSeConfiguracoes,
  ACBrNFSeNotasFiscais,
  ACBrNFSeWebServices,
  pnfsNFSe, pcnConversao, pnfsConversao,
  ACBrUtil;

const
  ACBRNFSE_VERSAO = '2.0.0a';
  ACBRNFSE_NAMESPACE = 'http://www.portalfiscal.inf.br/nfe';

type
  EACBrNFSeException = class(EACBrDFeException);

  { TACBrNFSe }

  TACBrNFSe = class(TACBrDFe)
  private
    FDANFSE: TACBrNFSeDANFSEClass;
    FNotasFiscais: TNotasFiscais;
    FStatus: TStatusACBrNFSe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesNFSe;
    procedure SetConfiguracoes(AValue: TConfiguracoesNFSe);
    procedure SetDANFSE(const Value: TACBrNFSeDANFSEClass);
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
      StreamNFSe: TStream = nil; NomeArq: String = ''); override;

    function Enviar(ALote: integer; Imprimir: Boolean = True): Boolean; overload;
    function Enviar(ALote: String; Imprimir: Boolean = True): Boolean; overload;

    function EnviarSincrono(ALote: Integer; Imprimir: Boolean = True): Boolean; overload;
    function EnviarSincrono(ALote: String; Imprimir: Boolean = True): Boolean; overload;

    function Gerar(ARps: Integer): Boolean;

    function ConsultarSituacao(ACnpj, AInscricaoMunicipal, AProtocolo: String;
                               const ANumLote: String = ''): Boolean;
    function ConsultarLoteRps(ANumLote, AProtocolo: string; ACNPJ: String = '';
      AInscricaoMunicipal: String = ''; ASenha: string = '';
      AFraseSecreta: string =''; Mes: Integer = 0; Ano: Integer = 0;
      ARazaoSocial: string = ''): Boolean;
    function ConsultarNFSeporRps(ANumero, ASerie, ATipo, ACnpj,
      AInscricaoMunicipal: String; ASenha: String = '';
      AFraseSecreta: String = ''; ARazaoSocial: String = ''): Boolean;
    function ConsultarNFSe(ACnpj, AInscricaoMunicipal: String; ADataInicial,
      ADataFinal: TDateTime; ANumeroNFSe: String = ''; APagina: Integer = 1;
      ASenha : String = ''; AFraseSecreta : String = '';
      ACNPJTomador: String = ''; AIMTomador: String = '';
      ANomeInter: String = ''; ACNPJInter: String = ''; AIMInter: String = '';
      ASerie: String = ''): Boolean;

    function CancelarNFSe(ACodigoCancelamento: String): Boolean;
    function SubstituirNFSe(ACodigoCancelamento, ANumeroNFSe: String): Boolean;

    function LinkNFSe(ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String): String;
    function GerarLote(ALote: Integer): Boolean; overload;
    function GerarLote(ALote: String): Boolean; overload;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmada(AValue: integer): Boolean;
    function cStatProcessado(AValue: integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOut; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOut): String; reintroduce; overload;

    function IdentificaSchema(const AXML: String): TSchemaNFSe;
    function IdentificaSchemaLayout(const ALayOut: TLayOut): TSchemaNFSe;
    function GerarNomeArqSchema(const ALayOut: TLayOut;
      VersaoServico: String): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property NotasFiscais: TNotasFiscais read FNotasFiscais write FNotasFiscais;
    property Status: TStatusACBrNFSe read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrNFSe);

  published
    property Configuracoes: TConfiguracoesNFSe
      read GetConfiguracoes write SetConfiguracoes;
    property DANFSE: TACBrNFSeDANFSEClass read FDANFSE write SetDANFSE;
  end;


implementation

uses
  strutils, dateutils,
  pcnAuxiliar, synacode;

{$IFDEF FPC}
 {$R ACBrNFSeServicos.rc}
{$ELSE}
 {$R ACBrNFSeServicos.res ACBrNFSeServicos.rc}
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

procedure TACBrNFSe.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamNFSe: TStream; NomeArq: String);
begin
  SetStatus( stNFSeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamNFSe, NomeArq);
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

function TACBrNFSe.GetAbout: String;
begin
  Result := 'ACBrNFSe Ver: ' + ACBRNFSE_VERSAO;
end;

function TACBrNFSe.GetNomeArquivoServicos: String;
begin
  Result := 'ACBrNFSeServicos.ini';
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
  Result := ACBRNFSE_NAMESPACE;
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
  lTipoEvento: String;
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

function TACBrNFSe.IdentificaSchemaLayout(const ALayOut: TLayOut): TSchemaNFSe;
begin
  case ALayOut of
    LayNfseRecepcaoLote:
      Result := schNFSe;
    //LayNFSeRetRecepcao,
    //LayNFSeCancelamento,
    //LayNFSeInutilizacao,
    //LayNFSeConsulta,
    //LayNFSeStatusServico,
    //LayNFSeCadastro,
    //LayNFSeCCe,
    //LayNFSeConsNFSeDest,
    //LayNFSeDownloadNFSe,
      //LayNFSeRetAutorizacao,
      //LayAdministrarCSCNFCe,
      //LayDistDFeIn
    else
      Result := schErro;
  end;

end;

function TACBrNFSe.GerarNomeArqSchema(const ALayOut: TLayOut;
  VersaoServico: String): String;
begin
  if EstaVazio(VersaoServico) then
    VersaoServico := LerVersaoDeParams(ALayOut);

  Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
            SchemaNFSeToStr(IdentificaSchemaLayout(ALayOut)) + '_v' +
            VersaoServico + '.xsd';
end;

function TACBrNFSe.GetConfiguracoes: TConfiguracoesNFSe;
begin
  Result := TConfiguracoesNFSe(FPConfiguracoes);
end;

procedure TACBrNFSe.SetConfiguracoes(AValue: TConfiguracoesNFSe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrNFSe.LerVersaoDeParams(LayOutServico: TLayOut): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoNFSeToDbl( ve100 {Configuracoes.Geral.VersaoDF}));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBrNFSe.LerServicoDeParams(LayOutServico: TLayOut;
  var Versao: Double; var URL: String);
begin
  Versao := VersaoNFSeToDbl( ve100 {Configuracoes.Geral.VersaoDF});
  URL := '';
  LerServicoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL);
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

function TACBrNFSe.Enviar(ALote: integer; Imprimir: Boolean): Boolean;
begin
  Result := Enviar(IntToStr(ALote),Imprimir);
end;

function TACBrNFSe.Enviar(ALote: String; Imprimir: Boolean): Boolean;
var
  i: Integer;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  if Configuracoes.Geral.AssinaRPS then
    NotasFiscais.Assinar;

  Result := WebServices.Envia(ALote);

  if DANFSE <> nil then
  begin
    for i:= 0 to NotasFiscais.Count-1 do
    begin
      if NotasFiscais.Items[i].Confirmada and Imprimir then
        NotasFiscais.Items[i].Imprimir;
    end;
  end;
end;

function TACBrNFSe.EnviarSincrono(ALote: Integer;
  Imprimir: Boolean): Boolean;
begin
  Result := EnviarSincrono(IntToStr(ALote));
end;

function TACBrNFSe.EnviarSincrono(ALote: String;
  Imprimir: Boolean): Boolean;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  if Configuracoes.Geral.AssinaRPS then
    NotasFiscais.Assinar;

//  Result := WebServices.EnviarSincrono(ALote);
end;

function TACBrNFSe.Gerar(ARps: Integer): Boolean;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 1 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  if Configuracoes.Geral.AssinaGerar then
    NotasFiscais.Assinar;

//  Result := WebServices.Gera(ARps);
end;

function TACBrNFSe.ConsultarSituacao(ACnpj, AInscricaoMunicipal,
  AProtocolo: String; const ANumLote: String): Boolean;
begin
// Result := WebServices.ConsultaSituacao(ACnpj, AInscricaoMunicipal,
//                                        AProtocolo, ANumLote);
end;

function TACBrNFSe.ConsultarLoteRps(ANumLote, AProtocolo, ACNPJ,
  AInscricaoMunicipal, ASenha, AFraseSecreta: string; Mes, Ano: Integer;
  ARazaoSocial: string): Boolean;
var
 aPath: String;
 wAno, wMes, wDia: Word;
begin
 aPath := Configuracoes.Arquivos.PathGer;

 if (ACNPJ='') and (AInscricaoMunicipal='')
  then begin
   if Configuracoes.Arquivos.SepararPorMes
    then begin
     DecodeDate(Now, wAno, wMes, wDia);
     if Mes > 0 then
       wMes:= Mes;
     if Ano > 0 then
       wAno:= Ano;
     if Pos(IntToStr(wAno)+IntToStrZero(wMes,2),aPath) <= 0
      then aPath := PathWithDelim(aPath)+IntToStr(wAno)+IntToStrZero(wMes,2) + '\';
    end;

    //sincrono o arquivo tem outro nome
    if FilesExists(aPath+'Ger\'+ANumLote+'-env-lotS.xml') then
    begin
     if Configuracoes.Arquivos.AdicionarLiteral then
        NotasFiscais.LoadFromFile(aPath+'Ger\'+ANumLote+'-env-lotS.xml')
      else
       if Configuracoes.Arquivos.Salvar then
         NotasFiscais.LoadFromFile(aPath+ANumLote+'-env-lotS.xml');
    end
    else
    begin
      if Configuracoes.Arquivos.AdicionarLiteral then
        NotasFiscais.LoadFromFile(aPath+'Ger\'+ANumLote+'-env-lot.xml')
      else
       if Configuracoes.Arquivos.Salvar then
         NotasFiscais.LoadFromFile(aPath+ANumLote+'-env-lot.xml');
    end;

   if NotasFiscais.Count <= 0 then
     GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));
  end;

//  if (Trim(Self.WebServices.ConsLote.NumeroLote) = '') then
//    Self.WebServices.ConsLote.NumeroLote:= ANumLote;

  //obrigatorio passar a razao social para o provedor Tecnos
  if (Configuracoes.Geral.Provedor in [proTecnos]) and (ARazaoSocial = '') then
    ARazaoSocial := NotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial;

//  Result := WebServices.ConsultaLoteRps(AProtocolo, ACNPJ, AInscricaoMunicipal,
//                                        ASenha, AFraseSecreta, ARazaoSocial);
end;

function TACBrNFSe.ConsultarNFSeporRps(ANumero, ASerie, ATipo, ACnpj,
  AInscricaoMunicipal, ASenha, AFraseSecreta,
  ARazaoSocial: String): Boolean;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

//  Result := WebServices.ConsultaNFSeporRps(ANumero, ASerie, ATipo, ACnpj,
//                AInscricaoMunicipal, ASenha, AFraseSecreta, ARazaoSocial);
end;

function TACBrNFSe.ConsultarNFSe(ACnpj, AInscricaoMunicipal: String;
  ADataInicial, ADataFinal: TDateTime; ANumeroNFSe: String;
  APagina: Integer; ASenha, AFraseSecreta, ACNPJTomador, AIMTomador,
  ANomeInter, ACNPJInter, AIMInter, ASerie: String): Boolean;
begin
//  Result := WebServices.ConsultaNFSe(ACnpj, AInscricaoMunicipal, ADataInicial,
//            ADataFinal, ANumeroNFSe, APagina, ASenha, AFraseSecreta,
//            ACNPJTomador, AIMTomador, ANomeInter, ACNPJInter, AIMInter, ASerie);
end;

function TACBrNFSe.CancelarNFSe(ACodigoCancelamento: String): Boolean;
begin
  if Self.NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NFS-e adicionada ao Lote'));

// Result := WebServices.CancelaNFSe(ACodigoCancelamento, True);
end;

function TACBrNFSe.SubstituirNFSe(ACodigoCancelamento,
  ANumeroNFSe: String): Boolean;
begin
  if Self.NotasFiscais.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

 NotasFiscais.Assinar;

// Result := WebServices.SubstitiNFSe(ACodigoCancelamento, ANumeroNFSe);
end;

function TACBrNFSe.LinkNFSe(ANumeroNFSe: Integer; ACodVerificacao,
  AInscricaoM: String): String;
begin
// Result := WebServices.LinkNFSeGerada(ANumeroNFSe, ACodVerificacao, AInscricaoM);
end;

function TACBrNFSe.GerarLote(ALote: Integer): Boolean;
begin
  Result := GerarLote(IntToStr(ALote));
end;

function TACBrNFSe.GerarLote(ALote: String): Boolean;
begin
  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  if Configuracoes.Geral.AssinaRPS then
    NotasFiscais.Assinar;

//  Result := WebServices.GeraLote(ALote);
end;

end.

