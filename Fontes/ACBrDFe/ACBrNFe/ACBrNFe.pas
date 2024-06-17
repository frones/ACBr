{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Wemerson Souto                                  }
{                              André Ferreira de Moraes                        }
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

unit ACBrNFe;

interface

uses
  Classes, SysUtils, ACBrBase, synautil,
  ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrNFeConfiguracoes, ACBrNFeWebServices, ACBrNFeNotasFiscais,
  ACBrDFeDANFeReport,
  pcnNFe, pcnConversao, pcnConversaoNFe,
  ACBrNFe.EnvEvento,
  ACBrNFe.Inut,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.Math, ACBrUtil.FilesIO;

const
  ACBRNFE_NAMESPACE = 'http://www.portalfiscal.inf.br/nfe';
  CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
     'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrNFeException = class(EACBrDFeException);

  { TCartaCorrecao }

  TCartaCorrecao = class
  private
    FCCe: TEventoNFe;
  public
    constructor Create;
    destructor Destroy; override;

    property CCe: TEventoNFe read FCCe write FCCe;
  end;

  { TACBrNFe }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFe = class(TACBrDFe)
  private
    FDANFE: TACBrDFeDANFeReport;
    FNotasFiscais: TNotasFiscais;
    FCartaCorrecao: TCartaCorrecao;
    FEventoNFe: TEventoNFe;
    FInutNFe: TInutNFe;
    FStatus: TStatusACBrNFe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesNFe;
    function Distribuicao(AcUFAutor: integer; const ACNPJCPF, AultNSU, ANSU,
      chNFe: String): Boolean;

    procedure SetConfiguracoes(AValue: TConfiguracoesNFe);
    procedure SetDANFE(const Value: TACBrDFeDANFeReport);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamNFe: TStream = nil; const NomeArq: String = ''; sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    function Enviar(ALote: Int64; Imprimir: Boolean = True;
      Sincrono: Boolean = False; Zipado: Boolean = False): Boolean; overload;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;
    function EhAutorizacao(AVersao: TpcnVersaoDF; AModelo: TpcnModeloDF;
      AUFCodigo: Integer): Boolean;

    function CstatConfirmada(AValue: integer): Boolean;
    function CstatProcessado(AValue: integer): Boolean;
    function CstatCancelada(AValue: integer): Boolean;

    function Enviar(const ALote: String; Imprimir: Boolean = True;
      Sincrono: Boolean = False; Zipado: Boolean = False): Boolean; overload;
    function Cancelamento(const AJustificativa: String; ALote: Int64 = 0): Boolean;
    function Consultar(const AChave: String = ''; AExtrairEventos: Boolean = False): Boolean;
    function EnviarCartaCorrecao(idLote: Int64): Boolean;
    function EnviarEvento(idLote: Int64): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOut; var Versao: Double;
      var URL: String; var Servico: String; var SoapAction: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOut): String; reintroduce; overload;

    function AjustarVersaoQRCode( AVersaoQRCode: TpcnVersaoQrCode;
      AVersaoXML: TpcnVersaoDF): TpcnVersaoQrCode;
    function GetURLConsultaNFCe(const CUF: integer;
      const TipoAmbiente: TpcnTipoAmbiente;
      const Versao: Double): String;
    function GetURLQRCode(const CUF: integer; const TipoAmbiente: TpcnTipoAmbiente;
      const AChaveNFe, Destinatario: String; const DataHoraEmissao: TDateTime;
      const ValorTotalNF, ValorTotalICMS: currency; const DigestValue: String;
      const Versao: Double): String;

    function IdentificaSchema(const AXML: String): TSchemaNFe;
    function GerarNomeArqSchema(const ALayOut: TLayOut; VersaoServico: Double
      ): String;
    function GerarNomeArqSchemaEvento(ASchemaEventoNFe: TSchemaNFe; VersaoServico: Double): String;
    function GerarChaveContingencia(FNFe: TNFe): String;

    property WebServices: TWebServices read FWebServices write FWebServices;
    property NotasFiscais: TNotasFiscais read FNotasFiscais write FNotasFiscais;
    property CartaCorrecao: TCartaCorrecao read FCartaCorrecao write FCartaCorrecao;
    property EventoNFe: TEventoNFe read FEventoNFe write FEventoNFe;
    property InutNFe: TInutNFe read FInutNFe write FInutNFe;
    property Status: TStatusACBrNFe read FStatus;

    procedure SetStatus(const stNewStatus: TStatusACBrNFe);
    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;
    procedure ImprimirInutilizacao;
    procedure ImprimirInutilizacaoPDF;

    function AdministrarCSC(const ARaizCNPJ: String; AIndOP: TpcnIndOperacao;
      AIdCSC: integer; const ACodigoCSC: String): Boolean;
    function DistribuicaoDFe(AcUFAutor: integer; const ACNPJCPF, AultNSU,
      ANSU: String; const AchNFe: String = ''): Boolean;
    function DistribuicaoDFePorUltNSU(AcUFAutor: integer; const ACNPJCPF,
      AultNSU: String): Boolean;
    function DistribuicaoDFePorNSU(AcUFAutor: integer; const ACNPJCPF,
      ANSU: String): Boolean;
    function DistribuicaoDFePorChaveNFe(AcUFAutor: integer; const ACNPJCPF,
      AchNFe: String): Boolean;
    function Inutilizar(const ACNPJ, AJustificativa: String;
      AAno, ASerie, ANumInicial, ANumFinal: Integer): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmailEvento(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

  published
    property Configuracoes: TConfiguracoesNFe
      read GetConfiguracoes write SetConfiguracoes;
    property DANFE: TACBrDFeDANFeReport read FDANFE write SetDANFE;
  end;

implementation

uses
  strutils, dateutils, math,
  ACBrDFeUtil,
  ACBrUtil.DateTime,
  synacode;

{$IFDEF FPC}
 {$R ACBrNFeServicos.rc}
{$ELSE}
 {$R ACBrNFeServicos.res}
{$ENDIF}

{ TACBrNFe }

constructor TACBrNFe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNotasFiscais := TNotasFiscais.Create(Self, NotaFiscal);
  FCartaCorrecao := TCartaCorrecao.Create; //(Self);
  FEventoNFe := TEventoNFe.Create;
  FInutNFe := TInutNFe.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrNFe.Destroy;
begin
  FNotasFiscais.Free;
  FCartaCorrecao.Free;
  FEventoNFe.Free;
  FInutNFe.Free;
  FWebServices.Free;

  inherited;
end;

procedure TACBrNFe.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamNFe: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stNFeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamNFe, NomeArq,
      sReplyTo, sBCC);
  finally
    SetStatus( stIdle );
  end;
end;

procedure TACBrNFe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDANFE <> nil) and
    (AComponent is TACBrDFeDANFeReport) then
    FDANFE := nil;
end;

function TACBrNFe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesNFe.Create(Self);
end;

procedure TACBrNFe.SetDANFE(const Value: TACBrDFeDANFeReport);
var
  OldValue: TACBrDFeDANFeReport;
begin
  if Value <> FDANFE then
  begin
    if Assigned(FDANFE) then
      FDANFE.RemoveFreeNotification(Self);

    OldValue := FDANFE;   // Usa outra variavel para evitar Loop Infinito
    FDANFE := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrNFe) then
        OldValue.ACBrNFe := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrNFe := self;
    end;
  end;
end;

function TACBrNFe.GetNomeModeloDFe: String;
begin
  Result := ModeloDFToPrefixo( Configuracoes.Geral.ModeloDF );
end;

function TACBrNFe.GetNameSpaceURI: String;
begin
  Result := ACBRNFE_NAMESPACE;
end;

function TACBrNFe.CstatConfirmada(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
    else
      Result := False;
  end;
end;

function TACBrNFe.CstatProcessado(AValue: integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302, 303: Result := True;
    else
      Result := False;
  end;
end;

function TACBrNFe.CstatCancelada(AValue: integer): Boolean;
begin
  case AValue of
    101, 135, 151, 155: Result := True;
    else
      Result := False;
  end;
end;

function TACBrNFe.EhAutorizacao( AVersao: TpcnVersaoDF; AModelo: TpcnModeloDF;
  AUFCodigo: Integer ): Boolean;
begin
  Result := (AVersao >= ve310);

  if (AModelo = moNFCe) and (AVersao = ve310) then
    Result := not (AUFCodigo in [13]); // AM
end;

function TACBrNFe.IdentificaSchema(const AXML: String): TSchemaNFe;
var
  lTipoEvento: String;
  I: integer;
begin
  Result := schNfe;
  I := pos('<infNFe', AXML);

  if I = 0 then
  begin
    I := pos('<infCanc', AXML);
    if I > 0 then
      Result := schCancNFe
    else
    begin
      I := pos('<infInut', AXML);
      if I > 0 then
        Result := schInutNFe
      else
      begin
        I := Pos('<infEvento', AXML);
        if I > 0 then
        begin
          lTipoEvento := Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>'));
          if lTipoEvento = '110111' then
            Result := schEnvEventoCancNFe // Cancelamento
          else if lTipoEvento = '210200' then
            Result := schEnvConfRecebto //Manif. Destinatario: Confirmação da Operação
          else if lTipoEvento = '210210' then
            Result := schEnvConfRecebto
          //Manif. Destinatario: Ciência da Operação Realizada
          else if lTipoEvento = '210220' then
            Result := schEnvConfRecebto
          //Manif. Destinatario: Desconhecimento da Operação
          else if lTipoEvento = '210240' then
            Result := schEnvConfRecebto // Manif. Destinatario: Operação não Realizada
          else if lTipoEvento = '110140' then
            Result := schEnvEPEC // EPEC
          else
            Result := schEnvCCe; //Carta de Correção Eletrônica
        end ;
      end;
    end;
  end;
end;

function TACBrNFe.GerarNomeArqSchema(const ALayOut: TLayOut;
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

function TACBrNFe.GerarNomeArqSchemaEvento(ASchemaEventoNFe: TSchemaNFe;
  VersaoServico: Double): String;
var
  xComplemento: string;
begin
  if VersaoServico = 0 then
    Result := ''
  else
  begin
    if ASchemaEventoNFe = schEnvEPEC then
      xComplemento := GetNomeModeloDFe
    else
      xComplemento := '';

    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaEventoToStr(ASchemaEventoNFe) + xComplemento + '_v' +
              FloatToString(VersaoServico, '.', '0.00') + '.xsd';
  end;
end;

function TACBrNFe.GerarChaveContingencia(FNFe: TNFe): String;

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
  wicms_s, wicms_p: String;
  Digito: integer;
begin
  //ajustado de acordo com nota tecnica 2009.003

  //UF
  if FNFe.Dest.EnderDest.UF = 'EX' then
    wchave := '99' //exterior
  else
  begin
    if FNFe.Ide.tpNF = tnSaida then
      wchave := copy(IntToStr(FNFe.Dest.EnderDest.cMun), 1, 2) //saida
    else
      wchave := copy(IntToStr(FNFe.Emit.EnderEmit.cMun), 1, 2); //entrada
  end;

  if FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSVCAN, teSVCRS] then
    wchave := wchave + TpEmisToStr(FNFe.Ide.tpEmis)
  else
    wchave := wchave + '0'; //este valor caracteriza ERRO, valor tem q ser  2, 5, 6 ou 7

  //CNPJ OU CPF
  if (FNFe.Dest.EnderDest.UF = 'EX') then
    wchave := wchave + ACBrUtil.Strings.Poem_Zeros('0', 14)
  else
    wchave := wchave + ACBrUtil.Strings.Poem_Zeros(FNFe.Dest.CNPJCPF, 14);

  //VALOR DA NF
  wchave := wchave + IntToStrZero(Round(FNFe.Total.ICMSTot.vNF * 100), 14);

  //DESTAQUE ICMS PROPRIO E ST
  wicms_p := IfThen(NaoEstaZerado(FNFe.Total.ICMSTot.vICMS), '1', '2');
  wicms_s := IfThen(NaoEstaZerado(FNFe.Total.ICMSTot.vST), '1', '2');
  wchave := wchave + wicms_p + wicms_s;

  //DIA DA EMISSAO
  wchave := wchave + ACBrUtil.Strings.Poem_Zeros(DayOf(FNFe.Ide.dEmi), 2);

  //DIGITO VERIFICADOR
  GerarDigito_Contigencia(Digito, wchave);
  wchave := wchave + IntToStr(digito);

  //RETORNA A CHAVE DE CONTINGENCIA
  Result := wchave;
end;

function TACBrNFe.GetConfiguracoes: TConfiguracoesNFe;
begin
  Result := TConfiguracoesNFe(FPConfiguracoes);
end;

procedure TACBrNFe.SetConfiguracoes(AValue: TConfiguracoesNFe);
begin
  FPConfiguracoes := AValue;
end;

function TACBrNFe.LerVersaoDeParams(LayOutServico: TLayOut): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    VersaoDFToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

function TACBrNFe.AjustarVersaoQRCode(AVersaoQRCode: TpcnVersaoQrCode;
  AVersaoXML: TpcnVersaoDF): TpcnVersaoQrCode;
begin
  if (AVersaoXML <= ve310) then
    Result := veqr000
  else     // ve400 ou superior
    Result := TpcnVersaoQrCode(max(Integer(AVersaoQRCode), Integer(veqr100)));
end;

procedure TACBrNFe.LerServicoDeParams(LayOutServico: TLayOut;
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

  Versao := VersaoDFToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  Servico := '';
  SoapAction := '';

  LerServicoDeParams(GetNomeModeloDFe, AUF,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL, Servico, SoapAction);
end;

function TACBrNFe.GetURLConsultaNFCe(const CUF: integer;
  const TipoAmbiente: TpcnTipoAmbiente; const Versao: Double): String;
var
  VersaoDFe: TpcnVersaoDF;
  VersaoQrCode: TpcnVersaoQrCode;
  ok: Boolean;
begin
  VersaoDFe := DblToVersaoDF(ok, Versao);
  VersaoQrCode := AjustarVersaoQRCode(Configuracoes.Geral.VersaoQRCode, VersaoDFe);

  Result := LerURLDeParams('NFCe', CUFtoUF(CUF), TipoAmbiente, 'URL-ConsultaNFCe', VersaoQrCodeToDbl(VersaoQrCode));
end;

function TACBrNFe.GetURLQRCode(const CUF: integer;
  const TipoAmbiente: TpcnTipoAmbiente; const AChaveNFe, Destinatario: String;
  const DataHoraEmissao: TDateTime; const ValorTotalNF,
  ValorTotalICMS: currency; const DigestValue: String; const Versao: Double
  ): String;
var
  idNFe, sdhEmi_HEX, sdigVal_HEX, sNF, sICMS, cIdCSC, cCSC, sCSC,
  sEntrada, cHashQRCode, urlUF, cDest: String;
  VersaoDFe: TpcnVersaoDF;
  VersaoQrCode: TpcnVersaoQrCode;
  ok: Boolean;
begin
  VersaoDFe := DblToVersaoDF(ok, Versao);
  VersaoQrCode := AjustarVersaoQRCode(Configuracoes.Geral.VersaoQRCode, VersaoDFe);

  urlUF := LerURLDeParams('NFCe', CUFtoUF(CUF), TipoAmbiente, 'URL-QRCode', VersaoQrCodeToDbl(VersaoQrCode));
  idNFe := OnlyNumber(AChaveNFe);
  cDest := Trim(Destinatario);

  // Passo 1
  sdhEmi_HEX := AsciiToHex(DateTimeTodh(DataHoraEmissao) +
    GetUTC(CodigoUFparaUF(CUF), DataHoraEmissao));
  sdigVal_HEX := AsciiToHex(DigestValue);

  if (CUF in [35, 41, 50]) then
  begin
    sdhEmi_HEX := LowerCase(sdhEmi_HEX);
    sdigVal_HEX := LowerCase(sdigVal_HEX);
  end;

  // Passo 3 e 4
  cCSC := Configuracoes.Geral.CSC;

  if VersaoQrCode >= veqr200 then
    cIdCSC := IntToStr(StrToIntDef(Configuracoes.Geral.IdCSC,0))
  else
    cIdCSC := IntToStrZero(StrToIntDef(Configuracoes.Geral.IdCSC,0),6);

  if EstaVazio(cCSC) then
    cCSC := Copy(idNFe, 7, 8) + '20' + Copy(idNFe, 3, 2) + Copy(cIdCSC, 3, 4);

  sCSC := cIdCSC + cCSC;
  sNF := FloatToString( ValorTotalNF, '.', FloatMask(2, False));
  sICMS := FloatToString( ValorTotalICMS, '.', FloatMask(2, False));

  if VersaoQrCode >= veqr200 then
  begin
    sEntrada := idNFe + '|' + VersaoQrCodeToStr(VersaoQrCode)+  '|'  +
      TpAmbToStr(TipoAmbiente) + '|';

    if ExtrairTipoEmissaoChaveAcesso(idNFe) = 9 then
      sEntrada := sEntrada + Format('%.2d',[DayOf(DataHoraEmissao)]) + '|' +
                           sNF + '|' + sdigVal_HEX + '|';
  end
  else
    sEntrada := 'chNFe=' + idNFe + '&nVersao=100&tpAmb=' +
      TpAmbToStr(TipoAmbiente) + IfThen(cDest = '', '', '&cDest=' +
      cDest) + '&dhEmi=' + sdhEmi_HEX + '&vNF=' + sNF + '&vICMS=' +
      sICMS + '&digVal=' + sdigVal_HEX + '&cIdToken=';

  // Passo 5 calcular o SHA-1 da string sEntrada
  cHashQRCode := AsciiToHex(SHA1(sEntrada + sCSC));

  // Passo 6
  if VersaoQrCode >= veqr200 then
  begin
    Result := urlUF;
    if Pos('?p=', urlUF) <= 0 then
      Result := Result + '?p=';

    Result := Result + sEntrada + cIdCSC + '|' + cHashQRCode;
  end
  else
  begin
    if Pos('?', urlUF) > 0 then
      Result := urlUF + '&'
    else
      Result := urlUF + '?';

    Result := Result + sEntrada + cIdCSC + '&cHashQRCode=' + cHashQRCode;
  end;
end;

function TACBrNFe.GravarStream(AStream: TStream): Boolean;
begin
//  if EstaVazio(FEventoNFe.Gerador.ArquivoFormatoXML) then
    FEventoNFe.GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FEventoNFe.XmlEnvio));
  Result := True;
end;

procedure TACBrNFe.SetStatus(const stNewStatus: TStatusACBrNFe);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrNFe.Cancelamento(const AJustificativa: String; ALote: Int64 = 0): Boolean;
var
  i: integer;
begin
  if NotasFiscais.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhuma Nota Fiscal Eletrônica Informada!'));

  for i := 0 to NotasFiscais.Count - 1 do
  begin
    WebServices.Consulta.NFeChave := NotasFiscais.Items[i].NumID;

    if not WebServices.Consulta.Executar then
      GerarException(WebServices.Consulta.Msg);

    EventoNFe.Evento.Clear;
    with EventoNFe.Evento.New do
    begin
      infEvento.CNPJ:= NotasFiscais.Items[i].NFe.Emit.CNPJCPF;
      infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(WebServices.Consulta.NFeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chNFe := WebServices.Consulta.NFeChave;
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

function TACBrNFe.Consultar(const AChave: String; AExtrairEventos: Boolean): Boolean;
var
  i: integer;
begin
  if (NotasFiscais.Count = 0) and EstaVazio(AChave) then
    GerarException(ACBrStr('ERRO: Nenhuma Nota Fiscal Eletrônica ou Chave Informada!'));

  if NaoEstaVazio(AChave) then
  begin
    NotasFiscais.Clear;
    WebServices.Consulta.NFeChave       := AChave;
    WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    WebServices.Consulta.Executar;
  end
  else
  begin
    for i := 0 to NotasFiscais.Count - 1 do
    begin
      WebServices.Consulta.NFeChave       := NotasFiscais.Items[i].NumID;
      WebServices.Consulta.ExtrairEventos := AExtrairEventos;
      WebServices.Consulta.Executar;
    end;
  end;

  Result := True;
end;

function TACBrNFe.Enviar(ALote: Int64; Imprimir: Boolean = True;
  Sincrono: Boolean = False; Zipado: Boolean = False): Boolean;
begin
  Result := Enviar(IntToStr(ALote), Imprimir, Sincrono, Zipado);
end;

function TACBrNFe.Enviar(const ALote: String; Imprimir: Boolean; Sincrono: Boolean;
  Zipado: Boolean): Boolean;
var
  i: integer;
begin
  WebServices.Enviar.Clear;
  WebServices.Retorno.Clear;

  if NotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NF-e adicionada ao Lote'));

  if NotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de NF-e transmitidas (máximo de 50 NF-e)' +
      ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count)));

  NotasFiscais.Assinar;
  NotasFiscais.Validar;

  Result := WebServices.Envia(ALote, Sincrono, Zipado);

  if Imprimir and (DANFE <> nil) then
  begin
    for i := 0 to NotasFiscais.Count - 1 do
    begin
      if NotasFiscais.Items[i].Confirmada then
        NotasFiscais.Items[i].Imprimir;
    end;
  end;
end;

function TACBrNFe.EnviarCartaCorrecao(idLote: Int64): Boolean;
var
  i: integer;
begin
  EventoNFe.Evento.Clear;

  for i := 0 to CartaCorrecao.CCe.Evento.Count - 1 do
  begin
    with EventoNFe.Evento.New do
    begin
      infEvento.id := CartaCorrecao.CCe.Evento[i].InfEvento.id;
      infEvento.cOrgao := CartaCorrecao.CCe.Evento[i].InfEvento.cOrgao;
      infEvento.tpAmb := CartaCorrecao.CCe.Evento[i].InfEvento.tpAmb;
      infEvento.CNPJ := CartaCorrecao.CCe.Evento[i].InfEvento.CNPJ;
      infEvento.chNFe := CartaCorrecao.CCe.Evento[i].InfEvento.chNFe;
      infEvento.dhEvento := CartaCorrecao.CCe.Evento[i].InfEvento.dhEvento;
      infEvento.tpEvento := teCCe;
      infEvento.nSeqEvento := CartaCorrecao.CCe.Evento[i].InfEvento.nSeqEvento;
      infEvento.versaoEvento := CartaCorrecao.CCe.Evento[i].InfEvento.versaoEvento;
      infEvento.detEvento.versao :=
        CartaCorrecao.CCe.Evento[i].InfEvento.detEvento.versao;
      infEvento.detEvento.descEvento :=
        CartaCorrecao.CCe.Evento[i].InfEvento.detEvento.descEvento;
      infEvento.detEvento.xCondUso :=
        CartaCorrecao.CCe.Evento[i].InfEvento.detEvento.xCondUso;
      infEvento.detEvento.xCorrecao :=
        CartaCorrecao.CCe.Evento[i].InfEvento.detEvento.xCorrecao;
    end;
  end;

  Result := EnviarEvento(idLote);
end;

function TACBrNFe.EnviarEvento(idLote: Int64): Boolean;
var
  i, j: integer;
  chNfe: String;
begin
  if EventoNFe.Evento.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum Evento adicionado ao Lote'));

  if EventoNFe.Evento.Count > 20 then
    GerarException(ACBrStr('ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
      'excedido. Quantidade atual: ' + IntToStr(EventoNFe.Evento.Count)));

  WebServices.EnvEvento.idLote := idLote;

  {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
  for i := 0 to EventoNFe.Evento.Count - 1 do
  begin
    if EventoNFe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
      EventoNFe.Evento.Items[i].infEvento.nSeqEvento := 1;

    FEventoNFe.Evento.Items[i].InfEvento.tpAmb := Configuracoes.WebServices.Ambiente;

    if NotasFiscais.Count > 0 then
    begin
      chNfe := OnlyNumber(EventoNFe.Evento.Items[i].InfEvento.chNfe);

      // Se tem a chave da NFe no Evento, procure por ela nas notas carregadas //
      if NaoEstaVazio(chNfe) then
      begin
        For j := 0 to NotasFiscais.Count - 1 do
        begin
          if chNfe = NotasFiscais.Items[j].NumID then
            Break;
        end ;

        if j = NotasFiscais.Count then
          GerarException( ACBrStr('Não existe NFe com a chave ['+chNfe+'] carregada') );
      end
      else
        j := 0;

      if trim(EventoNFe.Evento.Items[i].InfEvento.CNPJ) = '' then
        EventoNFe.Evento.Items[i].InfEvento.CNPJ := NotasFiscais.Items[j].NFe.Emit.CNPJCPF;

      if chNfe = '' then
        EventoNFe.Evento.Items[i].InfEvento.chNfe := NotasFiscais.Items[j].NumID;

      if trim(EventoNFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
      begin
        if EventoNFe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
        begin
          EventoNFe.Evento.Items[i].infEvento.detEvento.nProt := NotasFiscais.Items[j].NFe.procNFe.nProt;

          if trim(EventoNFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
          begin
            WebServices.Consulta.NFeChave := EventoNFe.Evento.Items[i].InfEvento.chNfe;

            if not WebServices.Consulta.Executar then
              GerarException(WebServices.Consulta.Msg);

            EventoNFe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
          end;
        end;
      end;
    end;
  end;

  Result := WebServices.EnvEvento.Executar;

  if not Result then
    GerarException( WebServices.EnvEvento.Msg );
end;

function TACBrNFe.NomeServicoToNomeSchema(const NomeServico: String): String;
Var
  ok: Boolean;
  ALayout: TLayOut;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaNFeToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

procedure TACBrNFe.ImprimirEvento;
begin
  if not Assigned(DANFE) then
    GerarException('Componente DANFE não associado.')
  else
    DANFE.ImprimirEVENTO;
end;

procedure TACBrNFe.ImprimirEventoPDF;
begin
  if not Assigned(DANFE) then
    GerarException('Componente DANFE não associado.')
  else
    DANFE.ImprimirEVENTOPDF;
end;

procedure TACBrNFe.ImprimirInutilizacao;
begin
  if not Assigned(DANFE) then
    GerarException('Componente DANFE não associado.')
  else
    DANFE.ImprimirINUTILIZACAO;
end;

procedure TACBrNFe.ImprimirInutilizacaoPDF;
begin
  if not Assigned(DANFE) then
    GerarException('Componente DANFE não associado.')
  else
    DANFE.ImprimirINUTILIZACAOPDF;
end;

function TACBrNFe.AdministrarCSC(const ARaizCNPJ: String; AIndOP: TpcnIndOperacao;
  AIdCSC: integer; const ACodigoCSC: String): Boolean;
begin
  WebServices.AdministrarCSCNFCe.RaizCNPJ := ARaizCNPJ;
  WebServices.AdministrarCSCNFCe.indOP := AIndOP;
  WebServices.AdministrarCSCNFCe.idCsc := AIdCSC;
  WebServices.AdministrarCSCNFCe.codigoCsc := ACodigoCSC;

  Result := WebServices.AdministrarCSCNFCe.Executar;

  if not Result then
    GerarException( WebServices.AdministrarCSCNFCe.Msg );
end;

function TACBrNFe.Distribuicao(AcUFAutor: integer; const ACNPJCPF, AultNSU, ANSU,
  chNFe: String): Boolean;
begin
  WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
  WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
  WebServices.DistribuicaoDFe.ultNSU   := AultNSU;
  WebServices.DistribuicaoDFe.NSU      := ANSU;
  WebServices.DistribuicaoDFe.chNFe    := chNFe;

  Result := WebServices.DistribuicaoDFe.Executar;

  if not Result then
    GerarException( WebServices.DistribuicaoDFe.Msg );
end;

function TACBrNFe.DistribuicaoDFe(AcUFAutor: integer;
  const ACNPJCPF, AultNSU, ANSU: String; const AchNFe: String = ''): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, AultNSU, ANSU, AchNFe);
end;

function TACBrNFe.DistribuicaoDFePorUltNSU(AcUFAutor: integer; const ACNPJCPF,
  AultNSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, AultNSU, '', '');
end;

function TACBrNFe.DistribuicaoDFePorNSU(AcUFAutor: integer; const ACNPJCPF,
  ANSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', ANSU, '');
end;

function TACBrNFe.DistribuicaoDFePorChaveNFe(AcUFAutor: integer; const ACNPJCPF,
  AchNFe: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', '', AchNFe);
end;

function TACBrNFe.Inutilizar(const ACNPJ, AJustificativa: String; AAno, ASerie,
  ANumInicial, ANumFinal: Integer): Boolean;
begin
  Result := True;
  WebServices.Inutiliza(ACNPJ, AJustificativa, AAno,
                        Configuracoes.Geral.ModeloDFCodigo,
                        ASerie, ANumInicial, ANumFinal);
end;

procedure TACBrNFe.EnviarEmailEvento(const sPara, sAssunto: String;
  sMensagem: TStrings; sCC: TStrings; Anexos: TStrings;
  sReplyTo: TStrings);
var
  NomeArq: String;
  AnexosEmail: TStrings;
  StreamNFe : TMemoryStream;
begin
  AnexosEmail := TStringList.Create;
  StreamNFe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;

    GravarStream(StreamNFe);

    ImprimirEventoPDF;
    AnexosEmail.Add(DANFE.ArquivoPDF);

    NomeArq := OnlyNumber(EventoNFe.Evento[0].InfEvento.Id);
    EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFe,
	    NomeArq + '-procEventoNFe.xml', sReplyTo);
  finally
    AnexosEmail.Free;
    StreamNFe.Free;
  end;
end;

{ TCartaCorrecao }

constructor TCartaCorrecao.Create;
begin
  inherited Create;
  FCCe := TEventoNFe.Create;
end;

destructor TCartaCorrecao.Destroy;
begin
  FCCe.Free;
  inherited Destroy;
end;

end.

