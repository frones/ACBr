{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                              Wemerson Souto                                  }
{                              Wiliam Zacarias da Silva Rosa                   }
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

unit ACBrCTe;

interface

uses
  Classes, Sysutils, synautil,
  ACBrDFe, ACBrDFeConfiguracoes, ACBrBase,
  ACBrCTeConfiguracoes, ACBrCTeWebServices, ACBrCTeConhecimentos,
  ACBrCTeDACTEClass, ACBrDFeException,
  pcteCTe, pcnConversao, pcteConversaoCTe,
  pcteEnvEventoCTe, pcteInutCTe, 
  ACBrDFeUtil;

const
  ACBRCTE_NAMESPACE = 'http://www.portalfiscal.inf.br/cte';
  ACBRCTE_CErroAmbDiferente = 'Ambiente do XML (tpAmb) é diferente do '+
     'configurado no Componente (Configuracoes.WebServices.Ambiente)';

type
  EACBrCTeException = class(EACBrDFeException);

  { TACBrCTe }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCTe = class(TACBrDFe)
  private
    FDACTe: TACBrCTeDACTEClass;
    FConhecimentos: TConhecimentos;
    FEventoCTe: TEventoCTe;
    FInutCTe: TInutCTe;
    FStatus: TStatusACBrCTe;
    FWebServices: TWebServices;

    function GetConfiguracoes: TConfiguracoesCTe;
    function Distribuicao(AcUFAutor: integer; const ACNPJCPF, AultNSU, ANSU,
      AchCTe: String): Boolean;
    function GetUFFormaEmissao: string;

    procedure SetConfiguracoes(AValue: TConfiguracoesCTe);
  	procedure SetDACTE(const Value: TACBrCTeDACTEClass);

  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNomeModeloDFe: String; override;
    function GetNameSpaceURI: String; override;

    function cStatConfirmado(AValue: Integer): Boolean;
    function cStatProcessado(AValue: Integer): Boolean;
    function cStatCancelado(AValue: integer): Boolean;

    procedure LerServicoDeParams(LayOutServico: TLayOutCTe; var Versao: Double;
      var URL: String); reintroduce; overload;
    function LerVersaoDeParams(LayOutServico: TLayOutCTe): String; reintroduce; overload;

    function GetURLConsulta(const CUF: integer;
      const TipoAmbiente: TpcnTipoAmbiente;
      const Versao: Double): String;
    function GetURLQRCode(const CUF: integer; const TipoAmbiente: TpcnTipoAmbiente;
      const TipoEmissao: TpcnTipoEmissao; const AChaveCTe: String;
      const Versao: Double): String;

    function IdentificaSchema(const AXML: String): TSchemaCTe;
    function IdentificaSchemaModal(const AXML: String): TSchemaCTe;
    function IdentificaSchemaEvento(const AXML: String): TSchemaCTe;

    function GerarNomeArqSchema(const ALayOut: TLayOutCTe; VersaoServico: Double): String;
    function GerarNomeArqSchemaModal(const AXML: String; VersaoServico: Double): String;
    function GerarNomeArqSchemaEvento(ASchemaEventoCTe: TSchemaCTe; VersaoServico: Double): String;

    function GerarChaveContingencia(FCTe: TCTe): String;

    procedure SetStatus(const stNewStatus: TStatusACBrCTe);

    function Enviar(ALote: Int64; Imprimir: Boolean = True;
      ASincrono: Boolean = False): Boolean;  overload;
    function Enviar(const ALote: String; Imprimir: Boolean = True;
      ASincrono: Boolean = False): Boolean;  overload;

    function Consultar( const AChave: String = ''; AExtrairEventos: Boolean = False): Boolean;
    function Cancelamento(const AJustificativa: String; ALote: Int64 = 0): Boolean;
    function EnviarEvento(idLote: Int64): Boolean;
    function Inutilizar(const ACNPJ, AJustificativa: String;
      AAno, ASerie, ANumInicial, ANumFinal: Integer): Boolean;
    function DistribuicaoDFe(AcUFAutor: integer; const ACNPJCPF, AultNSU,
      ANSU: String; const AchNFe: String = ''): Boolean;
    function DistribuicaoDFePorUltNSU(AcUFAutor: integer;
      const ACNPJCPF, AultNSU: String): Boolean;
    function DistribuicaoDFePorNSU(AcUFAutor: integer;
      const ACNPJCPF, ANSU: String): Boolean;
    function DistribuicaoDFePorChaveCTe(AcUFAutor: integer;
      const ACNPJCPF, AchCTe: String): Boolean;
    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamCTe: TStream = nil; const NomeArq: String = ''; sReplyTo: TStrings = nil; sBCC: TStrings = nil); override;

    procedure EnviarEmailEvento(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    function GravarStream(AStream: TStream): Boolean;

    procedure ImprimirEvento;
    procedure ImprimirEventoPDF;

    procedure ImprimirInutilizacao;
    procedure ImprimirInutilizacaoPDF;

    property WebServices: TWebServices     read FWebServices   write FWebServices;
    property Conhecimentos: TConhecimentos read FConhecimentos write FConhecimentos;
    property EventoCTe: TEventoCTe         read FEventoCTe     write FEventoCTe;
    property InutCTe: TInutCTe             read FInutCTe       write FInutCTe;
    property Status: TStatusACBrCTe        read FStatus;

  published
    property Configuracoes: TConfiguracoesCTe read GetConfiguracoes write SetConfiguracoes;
  	property DACTE: TACBrCTeDACTEClass        read FDACTE           write SetDACTE;
  end;

implementation

uses
  dateutils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeSSL;

{$IFDEF FPC}
 {$R ACBrCTeServicos.rc}
{$ELSE}
 {$R ACBrCTeServicos.res}
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

function TACBrCTe.GetConfiguracoes: TConfiguracoesCTe;
begin
  Result := TConfiguracoesCTe(FPConfiguracoes);
end;

procedure TACBrCTe.SetConfiguracoes(AValue: TConfiguracoesCTe);
begin
  FPConfiguracoes := AValue;
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

function TACBrCTe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesCTe.Create(Self);
end;

procedure TACBrCTe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDACTE <> nil) and
     (AComponent is TACBrCTeDACTEClass) then
    FDACTE := nil;
end;

function TACBrCTe.GetNomeModeloDFe: String;
begin
  Result := ModeloCTeToPrefixo( Configuracoes.Geral.ModeloDF );
end;

function TACBrCTe.GetUFFormaEmissao: string;
begin
  case Configuracoes.Geral.FormaEmissao of
    teNormal: Result := Configuracoes.WebServices.UF;
    teDPEC: begin
              case Configuracoes.WebServices.UFCodigo of
                11, // Rondônia
                12, // Acre
                13, // Amazonas
                14, // Roraima
                15, // Pará
                16, // Amapá
                17, // Tocantins
                21, // Maranhão
                22, // Piauí
                23, // Ceará
                24, // Rio Grande do Norte
                25, // Paraibá
                27, // Alagoas
                28, // Sergipe
                29, // Bahia
                31, // Minas Gerais
                32, // Espirito Santo
                33, // Rio de Janeiro
                41, // Paraná
                42, // Santa Catarina
                43, // Rio Grande do Sul
                52, // Goiás
                53: // Distrito Federal
                    Result := 'SVC-SP';
                26, // Pernanbuco
                35, // São Paulo
                50, // Mato Grosso do Sul
                51: // Mato Grosso
                    Result := 'SVC-RS';
              end;
            end;
    teSVCAN: Result := 'SVC-AN';
    teSVCRS: Result := 'SVC-RS';
    teSVCSP: Result := 'SVC-SP';
  else
    Result := Configuracoes.WebServices.UF;
  end;
end;

function TACBrCTe.GetURLConsulta(const CUF: integer;
  const TipoAmbiente: TpcnTipoAmbiente; const Versao: Double): String;
//var
//  VersaoDFe: TVersaoCTe;
//  ok: Boolean;
begin
  // Se futuramente viermos a ter versões diferentes de URL de consulta
  // devemos descomentar as linhas e trocar o zero da função abaixo pela variável
  // VersaoDFe
//  VersaoDFe := DblToVersaoCTe(ok, Versao);
  Result := LerURLDeParams('CTe', CUFtoUF(CUF), TipoAmbiente, 'URL-ConsultaCTe', 0);
end;

function TACBrCTe.GetURLQRCode(const CUF: integer;
  const TipoAmbiente: TpcnTipoAmbiente; const TipoEmissao: TpcnTipoEmissao;
  const AChaveCTe: String; const Versao: Double): String;
var
  idCTe, sEntrada, urlUF, Passo2, sign: String;
//  VersaoDFe: TVersaoCTe;
begin
//  VersaoDFe := DblToVersaoCTe(ok, Versao);  // Deixado para usu futuro

  if TipoAmbiente = taHomologacao then
  begin
    if ( (TipoEmissao in [teSVCSP]) and (CUF in [41, 50, 51]) ) then
      urlUF := LerURLDeParams('CTe', GetUFFormaEmissao, TipoAmbiente, 'URL-QRCode', 0)
    else
      urlUF := LerURLDeParams('CTe', CUFtoUF(CUF), TipoAmbiente, 'URL-QRCode', 0);
  end
  else
    urlUF := LerURLDeParams('CTe', CUFtoUF(CUF), TipoAmbiente, 'URL-QRCode', 0);

  if Pos('?', urlUF) <= 0 then
    urlUF := urlUF + '?';

  idCTe := OnlyNumber(AChaveCTe);

  // Passo 1
  sEntrada := 'chCTe=' + idCTe + '&tpAmb=' + TpAmbToStr(TipoAmbiente);

  // Passo 2 calcular o SHA-1 da string idCTe se o Tipo de Emissão for EPEC ou FSDA
  if TipoEmissao in [teDPEC, teFSDA] then
  begin
    // Tipo de Emissão em Contingência
    SSL.CarregarCertificadoSeNecessario;
    sign := SSL.CalcHash(idCTe, dgstSHA1, outBase64, True);
    Passo2 := '&sign=' + sign;

    sEntrada := sEntrada + Passo2;
  end;

  Result := urlUF + sEntrada;
end;

function TACBrCTe.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FEventoCTe.Gerador.ArquivoFormatoXML) then
    FEventoCTe.GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FEventoCTe.Gerador.ArquivoFormatoXML));
  Result := True;
end;

function TACBrCTe.GetNameSpaceURI: String;
begin
  Result := ACBRCTE_NAMESPACE;
end;

function TACBrCTe.cStatConfirmado(AValue: Integer): Boolean;
begin
  case AValue of
    100, 150: Result := True;
    else
      Result := False;
  end;
end;

function TACBrCTe.cStatProcessado(AValue: Integer): Boolean;
begin
  case AValue of
    100, 110, 150, 301, 302: Result := True;
    else
      Result := False;
  end;
end;

function TACBrCTe.cStatCancelado(AValue: integer): Boolean;
begin
  case AValue of
    101, 151, 155: Result := True;
    else
      Result := False;
  end;
end;

function TACBrCTe.NomeServicoToNomeSchema(const NomeServico: String): String;
Var
  ok: Boolean;
  ALayout: TLayOutCTe;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaCTeToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

procedure TACBrCTe.LerServicoDeParams(LayOutServico: TLayOutCTe;
  var Versao: Double; var URL: String);
begin
  Versao := VersaoCTeToDbl(Configuracoes.Geral.VersaoDF);
  URL := '';
  LerServicoDeParams(GetNomeModeloDFe, GetUFFormaEmissao,
    Configuracoes.WebServices.Ambiente, LayOutToServico(LayOutServico),
    Versao, URL);
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

function TACBrCTe.IdentificaSchema(const AXML: String): TSchemaCTe;
var
 lTipoEvento: TpcnTpEvento;
 I: Integer;
 Ok: Boolean;
begin
  case Configuracoes.Geral.ModeloDF of
    moCTeOS: Result := schCTeOS;
    moGTVe: Result := schGTVe;
  else
    Result := schCTe;
  end;

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
        lTipoEvento := StrToTpEventoCTe(Ok, Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>')));

        case lTipoEvento of
          teCCe,
          teCancelamento,
          teEPEC,
          teMultiModal,
          tePrestDesacordo,
          teCancPrestDesacordo,
          teGTV,
          teComprEntrega,
          teCancComprEntrega: Result := schEventoCTe;
        else
          Result := schErro;
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
    I := pos( '<rodoOS>', XML);
    if I > 0 then
      Result := schcteModalRodoviarioOS
    else begin
      I := pos( '<aereo>', XML);
      if I > 0 then
        Result := schcteModalAereo
      else begin
        I := pos( '<aquav>', XML);
        if I > 0 then
          Result := schcteModalAquaviario
        else begin
          I := pos( '<duto>', XML);
          if I > 0 then
            Result := schcteModalDutoviario
          else begin
            I := pos( '<ferrov>', XML);
            if I > 0 then
              Result := schcteModalFerroviario
            else begin
              I := pos( '<multimodal>', XML);
              if I > 0 then
                Result := schcteMultiModal
              else
                Result := schErro;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TACBrCTe.IdentificaSchemaEvento(const AXML: String): TSchemaCTe;
begin
  // Implementar
  Result := schErro;
end;

function TACBrCTe.GerarNomeArqSchema(const ALayOut: TLayOutCTe;
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

function TACBrCTe.GerarNomeArqSchemaModal(const AXML: String;
  VersaoServico: Double): String;
begin
  if VersaoServico = 0 then
    Result := ''
  else
    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaCTeToStr(IdentificaSchemaModal(AXML)) + '_v' +
              FloatToString(VersaoServico, '.', '0.00') + '.xsd';
end;

function TACBrCTe.GerarNomeArqSchemaEvento(ASchemaEventoCTe: TSchemaCTe;
  VersaoServico: Double): String;
begin
  if VersaoServico = 0 then
    Result := ''
  else
    Result := PathWithDelim( Configuracoes.Arquivos.PathSchemas ) +
              SchemaCTeToStr(ASchemaEventoCTe) + '_v' +
              FloatToString(VersaoServico, '.', '0.00') + '.xsd';
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
  if FCTe.Ide.modelo = 67 then
  begin
    if FCTe.toma.enderToma.UF = 'EX' then
      wchave := '99' //exterior
    else
      wchave := Copy(IntToStr(FCTe.toma.enderToma.cMun), 1, 2);
  end
  else
  begin
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
  end;

  case FCTe.Ide.tpEmis of
   teDPEC,
   teContingencia: wchave := wchave + '2';
   teFSDA:         wchave := wchave + '5';
   else            wchave := wchave + '0'; //este valor caracteriza ERRO, valor tem q ser  2 ou 5
  end;

  if FCTe.Ide.modelo = 67 then
  begin
    if FCTe.toma.enderToma.UF = 'EX' then
      wchave := wchave + Poem_Zeros('0', 14)
    else
      wchave := wchave + Poem_Zeros(FCTe.toma.CNPJCPF, 14);
  end
  else
  begin
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
  end;

  //VALOR DA CT-e
  wchave := wchave + Poem_Zeros(OnlyNumber(FloatToStrf(FCTe.vPrest.vTPrest, ffFixed, 18, 2)), 14);

  //DESTAQUE ICMS PROPRIO E ST
  wicms_p := '2';
  wicms_s := '2';

  // Checar esse trecho

  if (NaoEstaZerado(FCTe.Imp.ICMS.ICMS00.vICMS)) then
    wicms_p := '1';
  if (NaoEstaZerado(FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF)) then
    wicms_s := '1';

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

procedure TACBrCTe.SetStatus(const stNewStatus: TStatusACBrCTe);
begin
  if (stNewStatus <> FStatus) then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBrCTe.Enviar(ALote: Int64; Imprimir: Boolean = True;
      ASincrono: Boolean = False): Boolean;
begin
  Result := Enviar(IntToStr(ALote), Imprimir, ASincrono);
end;

function TACBrCTe.Enviar(const ALote: String; Imprimir: Boolean = True;
      ASincrono: Boolean = False): Boolean;
var
  i: Integer;
begin
  WebServices.Enviar.Clear;
  WebServices.Retorno.Clear;

  if Conhecimentos.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum CT-e adicionado ao Lote'));

  if ASincrono then
  begin
    if Conhecimentos.Count > 1 then
      GerarException(ACBrStr('ERRO: Conjunto de CT-e transmitidos (máximo de 1 CT-e)' +
        ' excedido. Quantidade atual: ' + IntToStr(Conhecimentos.Count)));
  end
  else
  begin
    if Conhecimentos.Count > 50 then
      GerarException(ACBrStr('ERRO: Conjunto de CT-e transmitidos (máximo de 50 CT-e)' +
        ' excedido. Quantidade atual: ' + IntToStr(Conhecimentos.Count)));
  end;

  Conhecimentos.Assinar;
  Conhecimentos.Validar;

  if Configuracoes.Geral.ModeloDF = moCTeOS then
    Result := WebServices.EnviaOS(ALote)
  else
    Result := WebServices.Envia(ALote, ASincrono);

  if DACTE <> nil then
  begin
     for i := 0 to Conhecimentos.Count-1 do
     begin
       if Conhecimentos.Items[i].Confirmado and Imprimir then
         Conhecimentos.Items[i].Imprimir;
     end;
  end;
end;

function TACBrCTe.Consultar(const AChave: String; AExtrairEventos: Boolean): Boolean;
var
  i: Integer;
begin
  if (Conhecimentos.Count = 0) and EstaVazio(AChave) then
    GerarException(ACBrStr('ERRO: Nenhum CT-e ou Chave Informada!'));

  if NaoEstaVazio(AChave) then
  begin
    Conhecimentos.Clear;
    WebServices.Consulta.CTeChave       := AChave;
    WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    WebServices.Consulta.Executar;
  end
  else
  begin
    for i := 0 to Conhecimentos.Count - 1 do
    begin
      WebServices.Consulta.CTeChave       := Conhecimentos.Items[i].NumID;
      WebServices.Consulta.ExtrairEventos := AExtrairEventos;
      WebServices.Consulta.Executar;
    end;
  end;

  Result := True;
end;

function TACBrCTe.Cancelamento(const AJustificativa: String; ALote: Int64): Boolean;
var
  i: Integer;
begin
  if Conhecimentos.Count = 0 then
    GerarException(ACBrStr('ERRO: Nenhum CT-e Informado!'));

  for i := 0 to Conhecimentos.Count - 1 do
  begin
    WebServices.Consulta.CTeChave := Conhecimentos.Items[i].NumID;

    if not WebServices.Consulta.Executar then
      raise Exception.Create(WebServices.Consulta.Msg);

    EventoCTe.Evento.Clear;
    with EventoCTe.Evento.New do
    begin
      infEvento.CNPJ := Conhecimentos.Items[i].CTe.Emit.CNPJ;
      infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(WebServices.Consulta.CTeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chCTe := WebServices.Consulta.CTeChave;
      infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;

    try
      EnviarEvento(ALote);
    except
      raise Exception.Create(WebServices.EnvEvento.EventoRetorno.xMotivo);
    end;
  end;

  Result := True;
end;

function TACBrCTe.EnviarEvento(idLote: Int64): Boolean;
var
  i, j: Integer;
  chCTe: String;
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
    if EventoCTe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
      EventoCTe.Evento.Items[i].infEvento.nSeqEvento := 1;

    FEventoCTe.Evento.Items[i].InfEvento.tpAmb := Configuracoes.WebServices.Ambiente;

    if Conhecimentos.Count > 0 then
    begin
      chCTe := OnlyNumber(EventoCTe.Evento.Items[i].InfEvento.chCTe);

      // Se tem a chave do CTe no Evento, procure por ela nos conhecimentos carregados //
      if NaoEstaVazio(chCTe) then
      begin
        for j := 0 to Conhecimentos.Count - 1 do
        begin
          if chCTe = Conhecimentos.Items[j].NumID then
            Break;
        end;

        if j = Conhecimentos.Count then
          GerarException( ACBrStr('Não existe CTe com a chave ['+chCTe+'] carregado') );
      end
      else
        j := 0;

      if trim(EventoCTe.Evento.Items[i].InfEvento.CNPJ) = '' then
        EventoCTe.Evento.Items[i].InfEvento.CNPJ := Conhecimentos.Items[j].CTe.Emit.CNPJ;

      if chCTe = '' then
        EventoCTe.Evento.Items[i].InfEvento.chCTe := Conhecimentos.Items[j].NumID;

      if trim(EventoCTe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
      begin
        if EventoCTe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
        begin
          EventoCTe.Evento.Items[i].infEvento.detEvento.nProt := Conhecimentos.Items[j].CTe.procCTe.nProt;

          if trim(EventoCTe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
          begin
            WebServices.Consulta.CTeChave := EventoCTe.Evento.Items[i].InfEvento.chCTe;

            if not WebServices.Consulta.Executar then
              GerarException(WebServices.Consulta.Msg);

            EventoCTe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
          end;
        end;
      end;
    end;
  end;

  Result := WebServices.EnvEvento.Executar;

  if not Result then
    GerarException( WebServices.EnvEvento.Msg );
end;

function TACBrCTe.Inutilizar(const ACNPJ, AJustificativa: String; AAno, ASerie,
  ANumInicial, ANumFinal: Integer): Boolean;
begin
  Result := True;
  WebServices.Inutiliza(ACNPJ, AJustificativa, AAno,
                        Configuracoes.Geral.ModeloDFCodigo,
                        ASerie, ANumInicial, ANumFinal);
end;

function TACBrCTe.Distribuicao(AcUFAutor: integer; const ACNPJCPF, AultNSU, ANSU,
  AchCTe: String): Boolean;
begin
  WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
  WebServices.DistribuicaoDFe.CNPJCPF := ACNPJCPF;
  WebServices.DistribuicaoDFe.ultNSU := AultNSU;
  WebServices.DistribuicaoDFe.NSU := ANSU;
  WebServices.DistribuicaoDFe.chCTe := AchCTe;

  Result := WebServices.DistribuicaoDFe.Executar;

  if not Result then
    GerarException( WebServices.DistribuicaoDFe.Msg );
end;

function TACBrCTe.DistribuicaoDFe(AcUFAutor: integer; const ACNPJCPF, AultNSU,
  ANSU: String; const AchNFe: String): Boolean;
begin
  // Aguardando a SEFAZ implementar esse recurso já existente para a NF-e.
  if AchNFe <> '' then
  begin
    Result := False;
    GerarException('Aguardando a SEFAZ implementar consulta pela chave, já existente para a NF-e.');
  end
  else
    Result := Distribuicao(AcUFAutor, ACNPJCPF, AultNSU, ANSU, AchNFe);
end;

function TACBrCTe.DistribuicaoDFePorUltNSU(AcUFAutor: integer; const ACNPJCPF,
  AultNSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, AultNSU, '', '');
end;

function TACBrCTe.DistribuicaoDFePorNSU(AcUFAutor: integer; const ACNPJCPF,
  ANSU: String): Boolean;
begin
  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', ANSU, '');
end;

function TACBrCTe.DistribuicaoDFePorChaveCTe(AcUFAutor: integer; const ACNPJCPF,
  AchCTe: String): Boolean;
begin
  // Aguardando a SEFAZ implementar esse recurso já existente para a NF-e.
  Result := False;
  GerarException('Aguardando a SEFAZ implementar esse recurso já existente para a NF-e.');

//  Result := Distribuicao(AcUFAutor, ACNPJCPF, '', '', AchCTe);
end;

procedure TACBrCTe.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamCTe: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
begin
  SetStatus( stCTeEmail );

  try
    inherited EnviarEmail(sPara, sAssunto, sMensagem, sCC, Anexos, StreamCTe, NomeArq,
     sReplyTo, sBCC);
  finally
    SetStatus( stCTeIdle );
  end;
end;

procedure TACBrCTe.EnviarEmailEvento(const sPara, sAssunto: String;
  sMensagem: TStrings; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq: String;
  AnexosEmail: TStrings;
  StreamCTe : TMemoryStream;
begin
  AnexosEmail := TStringList.Create;
  StreamCTe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Anexos <> nil then
      AnexosEmail.Text := Anexos.Text;

    GravarStream(StreamCTe);

    ImprimirEventoPDF;
    AnexosEmail.Add(DACTE.ArquivoPDF);

    NomeArq := OnlyNumber(EventoCTe.Evento[0].InfEvento.Id);
    EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamCTe,
  	  NomeArq + '-procEventoCTe.xml', sReplyTo);
  finally
    AnexosEmail.Free;
    StreamCTe.Free;
  end;
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
    DACTE.ImprimirEVENTOPDF;
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
