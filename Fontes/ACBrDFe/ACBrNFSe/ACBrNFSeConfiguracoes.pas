{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
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

{$I ACBr.inc}

unit ACBrNFSeConfiguracoes;

interface

uses
{$IFNDEF ACBrNFSeOpenSSL}
  Windows, ACBrCAPICOM_TLB, ACBrMSXML2_TLB,
  JwaWinCrypt, JwaWinType,
{$ENDIF}
  Classes, Sysutils, pnfsConversao, pcnConversao, ActiveX;

{$IFNDEF ACBrNFSeOpenSSL}
  const CAPICOM_STORE_NAME = 'My'; //My CA Root AddressBook
{$ENDIF}

type

 TProvedorClass = Class;

 TConfigCidade = record
    VersaoSoap: String;
    Prefixo2: String;
    Prefixo3: String;
    Prefixo4: String;
    Identificador: String;
    NameSpaceEnvelope: String;
    AssinaRPS: Boolean;
    AssinaLote: Boolean;
    AssinaGerar: Boolean;
    QuebradeLinha: String;
 end;

 TConfigSchema = record
    VersaoCabecalho: String;
    VersaoDados: String;
    VersaoXML: String;
    NameSpaceXML: String;
    Cabecalho: String;
    ServicoEnviar: String;
    ServicoConSit: String;
    ServicoConLot: String;
    ServicoConRps: String;
    ServicoConSeqRps: String;
    ServicoConNfse: String;
    ServicoCancelar: String;
    ServicoGerar: String;
    ServicoEnviarSincrono: String;
    ServicoSubstituir: String;
    DefTipos: String;
  end;

 TConfigURL = record
    HomNomeCidade:String;
    HomRecepcaoLoteRPS: String;
    HomConsultaLoteRPS: String;
    HomConsultaNFSeRPS: String;
    HomConsultaSitLoteRPS: String;
    HomConsultaSeqRPS: String;
    HomConsultaNFSe: String;
    HomCancelaNFSe: String;
    HomGerarNFSe: String;
    HomRecepcaoSincrono: String;
    HomSubstituiNFSe: String;

    ProNomeCidade:String;
    ProRecepcaoLoteRPS: String;
    ProConsultaLoteRPS: String;
    ProConsultaSeqRPS: String;
    ProConsultaNFSeRPS: String;
    ProConsultaSitLoteRPS: String;
    ProConsultaNFSe: String;
    ProCancelaNFSe: String;
    ProGerarNFSe: String;
    ProRecepcaoSincrono: String;
    ProSubstituiNFSe: String;
  end;

 TCertificadosConf = class(TComponent)
  private
    FAssinaLote: Boolean;
    FAssinaRPS: Boolean;
    FAssinaGerar: Boolean;
    FSenhaCert: AnsiString;
    {$IFNDEF ACBrNFSeOpenSSL}
       FNumeroSerie: AnsiString;
       FDataVenc: TDateTime;
       FInformacao: AnsiString;
       FCNPJ : String;

       procedure SetNumeroSerie(const Value: AnsiString);
       function GetNumeroSerie: AnsiString;
       function GetDataVenc: TDateTime;
       function GetInformacao: AnsiString;
       function GetCNPJ: String;
    {$ELSE}
       FCertificado: AnsiString;
    {$ENDIF}
  public
    {$IFNDEF ACBrNFSeOpenSSL}
       function SelecionarCertificado:AnsiString;
       function GetCertificado: ICertificate2;
    {$ENDIF}
  published
    {$IFNDEF ACBrNFSeOpenSSL}
       property NumeroSerie: AnsiString read GetNumeroSerie write SetNumeroSerie;
       property DataVenc: TDateTime     read GetDataVenc;
       property Informacao: AnsiString  read GetInformacao;
       property CNPJ: String            read GetCNPJ;
    {$ELSE}
       property Certificado: AnsiString read FCertificado write FCertificado;
    {$ENDIF}
    property AssinaRPS: Boolean  read FAssinaRPS;
    property AssinaLote: Boolean read FAssinaLote;
    property AssinaGerar: Boolean read FAssinaGerar;
    property Senha: AnsiString   read FSenhaCert write FSenhaCert;
  end;

 TWebServicesConf = Class(TComponent)
  private
    FProvedorClass: TProvedorClass;

    FSalvar: Boolean;
    FVisualizar : Boolean;
    FAmbiente: TpcnTipoAmbiente;
    FAmbienteCodigo: Integer;
    FProxyHost: String;
    FProxyPort: String;
    FProxyUser: String;
    FProxyPass: String;
    FAguardarConsultaRet : Cardinal;
    FTentativas : Integer;
    FIntervaloTentativas : Cardinal;
    FAjustaAguardaConsultaRet : Boolean;
    FCodigoMunicipio: Integer;
    FPrefixo2: String;
    FPrefixo3: String;
    FPrefixo4: String;
    FProvedor: TnfseProvedor;
    FxProvedor: String;
    FVersaoSoap: String;
    FIdentificador: String;
    FNameSpace: String;
    FSenhaWeb: AnsiString;
    FUserWeb: String;
    FQuebradeLinha: String;

    // Schemas
    FVersaoCabecalho: String;
    FVersaoDados: String;
    FVersaoXML: String;
    FURL: String;
    FCabecalho: String;
    FServicoEnviar: String;
    FServicoConSit: String;
    FServicoConLot: String;
    FServicoConRps: String;
    FServicoConSeqRps: String;
    FServicoConNfse: String;
    FServicoCancelar: String;
    FServicoGerar: String;
    FServicoEnviarSincrono: String;
    FServicoSubstituir: String;
    FDefTipos: String;

    // URLs
    FHomNomeCidade:String;
    FHomRecepcaoLoteRPS: String;
    FHomConsultaLoteRPS: String;
    FHomConsultaSeqRPS: String;
    FHomConsultaNFSeRPS: String;
    FHomConsultaSitLoteRPS: String;
    FHomConsultaNFSe: String;
    FHomCancelaNFSe: String;
    FHomGerarNFSe: String;
    FHomRecepcaoSincrono: String;
    FHomSubstituiNFSe: String;

    FProNomeCidade:String;
    FProRecepcaoLoteRPS: String;
    FProConsultaLoteRPS: String;
    FProConsultaSeqRPS: String;
    FProConsultaNFSeRPS: String;
    FProConsultaSitLoteRPS: String;
    FProConsultaNFSe: String;
    FProCancelaNFSe: String;
    FProGerarNFSe: String;
    FProRecepcaoSincrono: String;
    FProSubstituiNFSe: String;

    FConsultaLoteAposEnvio: Boolean;

    procedure SetAmbiente(AValue: TpcnTipoAmbiente);
    procedure SetTentativas(const Value: Integer);
    procedure SetIntervaloTentativas(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetConfigMunicipio(aPath: String = '');
  published
    property Salvar: Boolean               read FSalvar     write FSalvar     default False;
    property Visualizar: Boolean           read FVisualizar write FVisualizar default False;
    property Ambiente: TpcnTipoAmbiente    read FAmbiente   write SetAmbiente default taHomologacao;
    property AmbienteCodigo: Integer       read FAmbienteCodigo;
    property ProxyHost: String             read FProxyHost  write FProxyHost;
    property ProxyPort: String             read FProxyPort  write FProxyPort;
    property ProxyUser: String             read FProxyUser  write FProxyUser;
    property ProxyPass: String             read FProxyPass  write FProxyPass;
    property AguardarConsultaRet: Cardinal read FAguardarConsultaRet write FAguardarConsultaRet;
    property Tentativas : Integer read FTentativas write SetTentativas default 18;
    property IntervaloTentativas : Cardinal read FIntervaloTentativas write SetIntervaloTentativas;
    property AjustaAguardaConsultaRet : Boolean read FAjustaAguardaConsultaRet write FAjustaAguardaConsultaRet;
    property CodigoMunicipio: Integer read FCodigoMunicipio write FCodigoMunicipio;
    property Prefixo2: String read FPrefixo2;
    property Prefixo3: String read FPrefixo3;
    property Prefixo4: String read FPrefixo4;
    property Provedor: TnfseProvedor read FProvedor;
    property xProvedor: String read FxProvedor;
    property VersaoSoap: String read FVersaoSoap;
    property Identificador: String read FIdentificador;
    property NameSpace: String read FNameSpace;
    property SenhaWeb: AnsiString read FSenhaWeb write FSenhaWeb;
    property UserWeb: String read FUserWeb write FUserWeb;
    property ConsultaLoteAposEnvio: Boolean read FConsultaLoteAposEnvio write FConsultaLoteAposEnvio;
    property QuebradeLinha: String read FQuebradeLinha;

    // Schemas
    property VersaoCabecalho: String read FVersaoCabecalho;
    property VersaoDados: String read FVersaoDados;
    property VersaoXML: String read FVersaoXML;
    property URL: String read FURL;
    property Cabecalho: String read FCabecalho;
    property ServicoEnviar: String read FServicoEnviar;
    property ServicoConSit: String read FServicoConSit;
    property ServicoConLot: String read FServicoConLot;
    property ServicoConRps: String read FServicoConRps;
    property ServicoConSeqRps: String read FServicoConSeqRps;
    property ServicoConNfse: String read FServicoConNfse;
    property ServicoCancelar: String read FServicoCancelar;
    property ServicoGerar: String read FServicoGerar;
    property ServicoEnviarSincrono: String read FServicoEnviarSincrono;
    property ServicoSubstituir: String read FServicoSubstituir;
    property DefTipos: String read FDefTipos;

    // URLs
    property HomNomeCidade: String read FHomNomeCidade;
    property HomRecepcaoLoteRPS: String read FHomRecepcaoLoteRPS;
    property HomConsultaLoteRPS: String read FHomConsultaLoteRPS;
    property HomConsultaSeqRPS: String read FHomConsultaSeqRPS;
    property HomConsultaNFSeRPS: String read FHomConsultaNFSeRPS;
    property HomConsultaSitLoteRPS: String read FHomConsultaSitLoteRPS;
    property HomConsultaNFSe: String read FHomConsultaNFSe;
    property HomCancelaNFSe: String read FHomCancelaNFSe;
    property HomGerarNFSe: String read FHomGerarNFSe;
    property HomRecepcaoSincrono: String read FHomRecepcaoSincrono;
    property HomSubstituiNFSe: String read FHomSubstituiNFSe;

    property ProNomeCidade: String read FProNomeCidade;
    property ProRecepcaoLoteRPS: String read FProRecepcaoLoteRPS;
    property ProConsultaLoteRPS: String read FProConsultaLoteRPS;
    property ProConsultaSeqRPS: String read FProConsultaSeqRPS;
    property ProConsultaNFSeRPS: String read FProConsultaNFSeRPS;
    property ProConsultaSitLoteRPS: String read FProConsultaSitLoteRPS;
    property ProConsultaNFSe: String read FProConsultaNFSe;
    property ProCancelaNFSe: String read FProCancelaNFSe;
    property ProGerarNFSe: String read FProGerarNFSe;
    property ProRecepcaoSincrono: String read FProRecepcaoSincrono;
    property ProSubstituiNFSe: String read FProSubstituiNFSe;
  end;

 TGeralConf = class(TComponent)
  private
    FSalvar: Boolean;
    FPathSalvar: String;
    FPathSchemas: String;
    FExibirErroSchema: Boolean;
    FFormatoAlerta: string;
    FRetirarAcentos: Boolean;

    function GetPathSalvar: String;
    function GetFormatoAlerta: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Save(AXMLName: String; AXMLFile: WideString; aPath: String = ''): Boolean;
  published
    property Salvar: Boolean           read FSalvar           write FSalvar default False;
    property PathSalvar: String        read GetPathSalvar     write FPathSalvar;
    property PathSchemas: String       read FPathSchemas      write FPathSchemas;
    property ExibirErroSchema: Boolean read FExibirErroSchema write FExibirErroSchema;
    property FormatoAlerta: string     read GetFormatoAlerta  write FFormatoAlerta;
    property RetirarAcentos: Boolean   read FRetirarAcentos   write FRetirarAcentos;
  end;

 TArquivosConf = class(TComponent)
  private
    FSalvar   : Boolean;
    FMensal   : Boolean;
    FLiteral  : Boolean;
    FEmissaoPathNFSe  : Boolean;
    FPathNFSe : String;
    FPathCan  : String;
    FPathRPS: String;
    FPathGer: String;
    FNomeLongoNFSe: Boolean;
    FTabServicosExt: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPathCan(Data: TDateTime = 0): String;
    function GetPathGer(Data: TDateTime = 0): String;
    function GetPathRPS(Data: TDateTime = 0): String;
    function GetPathNFSe(Data : TDateTime = 0): String;
  published
    property Salvar: Boolean           read FSalvar          write FSalvar          default False;
    property PastaMensal: Boolean      read FMensal          write FMensal          default False;
    property AdicionarLiteral: Boolean read FLiteral         write FLiteral         default False;
    property EmissaoPathNFSe: Boolean  read FEmissaoPathNFSe write FEmissaoPathNFSe default False;
    property PathNFSe: String          read FPathNFSe        write FPathNFSe;
    property PathCan: String           read FPathCan         write FPathCan;
    property PathRPS: String           read FPathRPS         write FPathRPS;
    property PathGer: String           read FPathGer         write FPathGer;
    property NomeLongoNFSe: Boolean    read FNomeLongoNFSe   write FNomeLongoNFSe   default False;
    property TabServicosExt: Boolean   read FTabServicosExt  write FTabServicosExt  default False;
  end;

 TConfiguracoes = class(TComponent)
  private
    FGeral: TGeralConf;
    FWebServices: TWebServicesConf;
    FCertificados: TCertificadosConf;
    FArquivos: TArquivosConf;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Geral: TGeralConf read FGeral;
    property WebServices: TWebServicesConf read FWebServices;
    property Certificados: TCertificadosConf read FCertificados;
    property Arquivos: TArquivosConf read FArquivos;
  end;

 TProvedorClass = Class
  private

  public
   Constructor Create;

   function GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade; Virtual; Abstract;
   function GetConfigSchema(ACodCidade: Integer): TConfigSchema; Virtual; Abstract;
   function GetConfigURL(ACodCidade: Integer): TConfigURL; Virtual; Abstract;
   function GetURI(URI: String): String; Virtual; Abstract;
   function GetAssinarXML(Acao: TnfseAcao): Boolean; Virtual; Abstract;
   function GetValidarLote: Boolean; Virtual; Abstract;

   function Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4, NameSpaceDad, Identificador, URI: String): AnsiString; Virtual; Abstract;
   function Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados, NameSpaceCab: String; ACodCidade: Integer): AnsiString; Virtual; Abstract;
   function Gera_DadosSenha(CNPJ, Senha: String): AnsiString; Virtual; Abstract;
   function Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString; Virtual; Abstract;

   function GeraEnvelopeRecepcionarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeConsultarSituacaoLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeConsultarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeConsultarNFSeporRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeGerarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeRecepcionarSincrono(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;
   function GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;

   function GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Virtual; Abstract;

   function GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String; Virtual; Abstract;
   function GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString; Virtual; Abstract;

   function GeraRetornoNFSe(Prefixo: String; RetNFSe: AnsiString; NomeCidade: String): AnsiString; Virtual; Abstract;
   function GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String; Virtual; Abstract;

  end;

implementation

uses
 IniFiles, DateUtils, Math, StrUtils, ACBrUtil, ACBrNFSe, ACBrNFSeUtil, ACBrDFeUtil,
 ACBrProvedorGinfesV3, ACBrProvedorPublica, ACBrProvedorRJ,
 ACBrProvedorTiplan, ACBrProvedorISSNet, ACBrProvedorWebISS,
 ACBrProvedorProdemge, ACBrProvedorISSIntel, ACBrProvedorGovBR,
 ACBrProvedorRecife, ACBrProvedorSimplISS, ACBrProvedorThema,
 ACBrProvedorEquiplano, ACBrProvedorfintelISS, ACBrProvedorDigifred,
 ACBrProvedorBetha, ACBrProvedorBetim, ACBrProvedorSaatri,
 ACBrProvedorAbaco, ACBrProvedorGoiania, ACBrProvedorIssCuritiba,
 ACBrProvedorBHISS, ACBrProvedorNatal, ACBrProvedorTinus, ACBrProvedorISSDigital,
 ACBrProvedorISSe, ACBrProvedor4R, ACBrProvedorGovDigital,
 ACBrProvedorFiorilli, ACBrProvedorISSDSF, ACBrProvedorInfisc, ACBrProvedorCoplan,
 ACBrProvedorProdata, ACBrProvedorAgili, ACBrProvedorFISSLex,
 ACBrProvedorVirtual, ACBrProvedorPVH, ACBrProvedorFreire,
 ACBrProvedorLink3, ACBrProvedorSpeedGov, ACBrProvedorVitoria,
 ACBrProvedorMitra, ACBrProvedorTecnos, ACBrProvedorPronim,
 ACBrProvedorActcon, ACBrProvedorEL, ACBrProvedorEgoverneISS,
 ACBrProvedorSisPMJP, ACBrProvedorSystemPro, ACBrProvedorSalvador,
 ACBrProvedorDBSeller, ACBrProvedorLexsom, ACBrProvedorABRASFv1,
 ACBrProvedorABRASFv2, ACBrProvedorNFSEBrasil, ACBrProvedorSJP,
  ACBrProvedorCONAM;

{ TConfiguracoes }

constructor TConfiguracoes.Create(AOwner: TComponent);
begin
 inherited Create( AOwner );

 FGeral      := TGeralConf.Create(Self);
 FGeral.Name := 'GeralConf';

{$IFDEF COMPILER6_UP}
  FGeral.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}

 FCertificados      := TCertificadosConf.Create(self);
 FCertificados.Name := 'CertificadosConf';

{$IFDEF COMPILER6_UP}
  FCertificados.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}

 FWebServices      := TWebServicesConf.Create(self);
 FWebServices.Name := 'WebServicesConf';

{$IFDEF COMPILER6_UP}
  FWebServices.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}

 FArquivos      := TArquivosConf.Create(self);
 FArquivos.Name := 'ArquivosConf';

{$IFDEF COMPILER6_UP}
  FArquivos.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}
end;

destructor TConfiguracoes.Destroy;
begin
 FGeral.Free;
 FWebServices.Free;
 FCertificados.Free;
 FArquivos.Free;

 inherited;
end;

{ TGeralConf }

constructor TGeralConf.Create(AOwner: TComponent);
begin
 Inherited Create( AOwner );

 FSalvar           := False;
 FPathSalvar       := '';
 FPathSchemas      := '';
 FExibirErroSchema := True;

 FFormatoAlerta    := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';
 // O Formato da mensagem de erro pode ser alterado pelo usuario alterando-se a property FFormatoAlerta: onde;
 // %TAGNIVEL%  : Representa o Nivel da TAG; ex: <transp><vol><lacres>
 // %TAG%       : Representa a TAG; ex: <nLacre>
 // %ID%        : Representa a ID da TAG; ex X34
 // %MSG%       : Representa a mensagem de alerta
 // %DESCRICAO% : Representa a Descrição da TAG

 FRetirarAcentos := True;
end;

function TGeralConf.GetFormatoAlerta: string;
begin
  if (FFormatoAlerta = '') or (
     (pos('%TAGNIVEL%',FFormatoAlerta) <= 0) and
     (pos('%TAG%',FFormatoAlerta) <= 0) and
     (pos('%ID%',FFormatoAlerta) <= 0) and
     (pos('%MSG%',FFormatoAlerta) <= 0) and
     (pos('%DESCRICAO%',FFormatoAlerta) <= 0) )then
     Result := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
  else
     Result := FFormatoAlerta;
end;

function TGeralConf.GetPathSalvar: String;
begin
 if EstaVazio(FPathSalvar)
  then Result := ApplicationPath
  else Result := FPathSalvar;

 Result := NotaUtil.PathWithDelim( Trim(Result) );
end;

function TGeralConf.Save(AXMLName: String; AXMLFile: WideString; aPath: String = ''): Boolean;
var
 vSalvar: TStrings;
begin
 Result := False;
 vSalvar := TStringList.Create;
 try
  try
   if NaoEstaVazio(ExtractFilePath(AXMLName))
    then begin
     aPath := ExtractFilePath(AXMLName);
     AXMLName := StringReplace(AXMLName,aPath,'',[rfIgnoreCase]);
    end
    else begin
     if EstaVazio(aPath)
      then aPath := PathSalvar
      else aPath := PathWithDelim(aPath);
    end;

   vSalvar.Text := AXMLFile;
   if not DirectoryExists( aPath )
    then ForceDirectories( aPath );

   vSalvar.Text := StringReplace(vSalvar.Text, '<-><->', '', [rfReplaceAll]);
   
   vSalvar.SaveToFile( aPath + AXMLName);
   Result := True;
  except on E: Exception do
   raise Exception.Create('Erro ao salvar. '+E.Message);
  end;
 finally
  vSalvar.Free;
 end;
end;

{ TWebServicesConf }

constructor TWebServicesConf.Create(AOwner: TComponent);
begin
 Inherited Create( AOwner );

 FSalvar     := False;
 FVisualizar := False;
 FAmbiente   := taHomologacao;
 if FAmbiente=taProducao
  then FAmbienteCodigo := 1
  else FAmbienteCodigo := 2;

 FPrefixo2      := 'ns2:';
 FPrefixo3      := 'ns3:';
 FPrefixo4      := 'ns4:';
 FProvedor      := proNenhum;
 FxProvedor     := '';
 FVersaoSoap    := '';
 FIdentificador := 'Id';
 FNameSpace     := '';
 FQuebradeLinha := ';';

 FConsultaLoteAposEnvio := True;
end;

destructor TWebServicesConf.Destroy;
begin
 if Assigned(FProvedorClass) then
  FProvedorClass.Free;

 inherited;
end;

procedure TWebServicesConf.SetAmbiente(AValue: TpcnTipoAmbiente);
begin
 FAmbiente := AValue;
 if AValue=taProducao
  then FAmbienteCodigo := 1
  else FAmbienteCodigo := 2;
end;

{ TCertificadosConf }

{$IFNDEF ACBrNFSeOpenSSL}
function TCertificadosConf.GetCertificado: ICertificate2;
var
 Store          : IStore3;
 Certs          : ICertificates2;
 Cert           : ICertificate2;
 Extension      : IExtension;

 i,j,k          : Integer;

 xmldoc         : IXMLDOMDocument3;
 xmldsig        : IXMLDigitalSignature;
 dsigKey        : IXMLDSigKey;
 SigKey         : IXMLDSigKeyEx;
 PrivateKey     : IPrivateKey;
 hCryptProvider : HCRYPTPROV;
 XML,
 Propriedades   : String;
 Lista          : TStringList;
begin
 CoInitialize(nil); // PERMITE O USO DE THREAD
 try  
   if EstaVazio( FNumeroSerie )
    then raise Exception.Create('Número de Série do Certificado Digital não especificado !');

   Result := nil;
   Store := CoStore.Create;
   Store.Open(CAPICOM_CURRENT_USER_STORE, CAPICOM_STORE_NAME, CAPICOM_STORE_OPEN_READ_ONLY);

   Certs := Store.Certificates as ICertificates2;
   for i := 1 to Certs.Count do
    begin
     Cert := IInterface(Certs.Item[i]) as ICertificate2;
     if Cert.SerialNumber = FNumeroSerie
      then begin
       if EstaVazio(NumCertCarregado)
        then NumCertCarregado := Cert.SerialNumber;

        PrivateKey := Cert.PrivateKey;

        if  CertStoreMem = nil then
         begin
           CertStoreMem := CoStore.Create;
           CertStoreMem.Open(CAPICOM_MEMORY_STORE, 'MemoriaACBrNFSe', CAPICOM_STORE_OPEN_READ_ONLY);
           CertStoreMem.Add(Cert);

           if (FSenhaCert <> '') and PrivateKey.IsHardwareDevice then
            begin
              XML := XML + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"><SignedInfo><CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/><SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />';
              XML := XML + '<Reference URI="#">';
              XML := XML + '<Transforms><Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" /><Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" /></Transforms><DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />';
              XML := XML + '<DigestValue></DigestValue></Reference></SignedInfo><SignatureValue></SignatureValue><KeyInfo></KeyInfo></Signature>';

              xmldoc := CoDOMDocument50.Create;
              xmldoc.async              := False;
              xmldoc.validateOnParse    := False;
              xmldoc.preserveWhiteSpace := True;
              xmldoc.loadXML(XML);
              xmldoc.setProperty('SelectionNamespaces', DSIGNS);

              xmldsig := CoMXDigitalSignature50.Create;
              xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');
              xmldsig.store := CertStoreMem;

              dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType, PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
              if (dsigKey = nil) then
                 raise Exception.Create('Erro ao criar a chave do CSP.');

              SigKey := dsigKey as IXMLDSigKeyEx;
              SigKey.getCSPHandle( hCryptProvider );

              try
                 CryptSetProvParam( hCryptProvider , PP_SIGNATURE_PIN, windows.PBYTE(FSenhaCert), 0 );
              finally
                CryptReleaseContext(hCryptProvider, 0);
              end;

              SigKey    := nil;
              dsigKey   := nil;
              xmldsig   := nil;
              xmldoc    := nil;
           end;
         end;

        Result := Cert;
        FDataVenc := Cert.ValidToDate;
        FInformacao := Cert.SubjectName;

        for J :=1 to Cert.Extensions.Count do
         begin
           Extension := IInterface(Cert.Extensions.Item[J]) as IExtension;
           Propriedades := Extension.EncodedData.Format(True);
           if (Pos('2.16.76.1.3.3', Propriedades) > 0) then
            begin
              Lista := TStringList.Create;
        			try
                Lista.Text := Propriedades;
                for K :=0 to Lista.Count-1 do
                 begin
                  if (Pos('2.16.76.1.3.3',Lista.Strings[K]) > 0) then
                   begin
                     FCNPJ := StringReplace(Lista.Strings[K],'2.16.76.1.3.3=','',[rfIgnoreCase]);
                     FCNPJ := OnlyNumber(HexToAscii(RemoveString(' ',FCNPJ)));
                     break;
                   end;
                 end;
       			  finally
                Lista.free;
         			end;
              break;
            end;
           Extension := nil;
         end;
        break;
      end;
    end;

    if not(Assigned(Result)) then
      raise Exception.Create('Certificado Digital não encontrado!');
  finally
    CoUninitialize;
  end;
end;

function TCertificadosConf.GetNumeroSerie: AnsiString;
begin
 Result := Trim(UpperCase(StringReplace(FNumeroSerie,' ','',[rfReplaceAll] )));
end;

procedure TCertificadosConf.SetNumeroSerie(const Value: AnsiString);
begin
 FNumeroSerie := Trim(UpperCase(StringReplace(Value,' ','',[rfReplaceAll] )));
end;

function TCertificadosConf.SelecionarCertificado: AnsiString;
var
 Store  : IStore3;
 Certs  : ICertificates2;
 Certs2 : ICertificates2;
 Cert   : ICertificate2;
begin
 Store := CoStore.Create;
 Store.Open(CAPICOM_CURRENT_USER_STORE, CAPICOM_STORE_NAME, CAPICOM_STORE_OPEN_READ_ONLY);

 Certs  := Store.Certificates as ICertificates2;
 Certs2 := Certs.Select('Certificado(s) Digital(is) disponível(is)', 'Selecione o Certificado Digital para uso no aplicativo', false);

 if not(Certs2.Count = 0)
  then begin
   Cert         := IInterface(Certs2.Item[1]) as ICertificate2;
   FNumeroSerie := Cert.SerialNumber;
   FDataVenc    := Cert.ValidToDate;
   FInformacao  := Cert.SubjectName
  end;

 Result := FNumeroSerie;
end;

function TCertificadosConf.GetDataVenc: TDateTime;
begin
 if NaoEstaVazio(FNumeroSerie)
  then begin
   if FDataVenc = 0
    then GetCertificado;
   Result := FDataVenc;
  end
 else Result := 0;
end;

function TCertificadosConf.GetInformacao: AnsiString;
begin
 if NaoEstaVazio(FNumeroSerie)
  then begin
   if FInformacao = ''
    then GetCertificado;
    Result := UpperCase(FInformacao);
  end
 else Result := '';
end;

function TCertificadosConf.GetCNPJ: String;
begin
 if NaoEstaVazio(FNumeroSerie) then
  begin
    if FCNPJ = '' then
       GetCertificado;
    Result := FCNPJ;
  end
 else
    Result := '';
end;
{$ENDIF}

Procedure TWebServicesConf.SetConfigMunicipio(aPath: String = '');
var
 Ok:           Boolean;
 ConfigCidade: TConfigCidade;
 ConfigSchema: TConfigSchema;
 ConfigURL:    TConfigURL;
begin
 FxProvedor := CodCidadeToProvedor(FCodigoMunicipio);
 FProvedor  := StrToProvedor(Ok, FxProvedor);

 if Provedor = proNenhum
  then raise Exception.Create('Código do Municipio ['+ IntToStr(FCodigoMunicipio) +'] não Encontrado.');

 FProvedorClass.Free;

 case FProvedor of
  proGINFES:      FProvedorClass := TProvedorGinfesV3.Create;
  proPublica:     FProvedorClass := TProvedorPublica.Create;
  proRJ:          FProvedorClass := TProvedorRJ.Create;
  proTiplan:      FProvedorClass := TProvedorTiplan.Create;
  proISSNet:      FProvedorClass := TProvedorISSNet.Create;
  proWebISS:      FProvedorClass := TProvedorWebISS.Create;
  proProdemge:    FProvedorClass := TProvedorProdemge.Create;
  proISSIntel:    FProvedorClass := TProvedorISSIntel.Create;
  proGovBR:       FProvedorClass := TProvedorGovBR.Create;
  proRecife:      FProvedorClass := TProvedorRecife.Create;
  proSimplISS:    FProvedorClass := TProvedorSimplISS.Create;
  proThema:       FProvedorClass := TProvedorThema.Create;
  proEquiplano:   FProvedorClass := TProvedorEquiplano.Create;
  profintelISS:   FProvedorClass := TProvedorfintelISS.Create;
  proDigifred:    FProvedorClass := TProvedorDigifred.Create;
  proBetha:       FProvedorClass := TProvedorBetha.Create;
  proBetim:       FProvedorClass := TProvedorBetim.Create;
  proSaatri:      FProvedorClass := TProvedorSaatri.Create;
  proAbaco:       FProvedorClass := TProvedorAbaco.Create;
  proGoiania:     FProvedorClass := TProvedorGoiania.Create;
  proIssCuritiba: FProvedorClass := TProvedorIssCuritiba.Create;
  proBHISS:       FProvedorClass := TProvedorBHISS.Create;
  proNatal:       FProvedorClass := TProvedorNatal.Create;
  proTinus:       FProvedorClass := TProvedorTinus.Create;
  proISSDigital:  FProvedorClass := TProvedorISSDigital.Create;
  proISSe:        FProvedorClass := TProvedorISSe.Create;
  pro4R:          FProvedorClass := TProvedor4R.Create;
  proGovDigital:  FProvedorClass := TProvedorGovDigital.Create;
  proFiorilli:    FProvedorClass := TProvedorFiorilli.Create;
  proIssDSF:      FProvedorClass := TProvedorIssDSF.Create;
  proInfisc:      FProvedorClass := TProvedorInfisc.Create;
  proCoplan:      FProvedorClass := TProvedorCoplan.Create;
  proProdata:     FProvedorClass := TProvedorProdata.Create;
  proAgili:       FProvedorClass := TProvedorAgili.Create;
  proFISSLex:     FProvedorClass := TProvedorFISSLex.Create;
  proVirtual:     FProvedorClass := TProvedorVirtual.Create;
  ProPVH:         FProvedorClass := TProvedorPVH.Create;
  proFreire:      FProvedorClass := TProvedorFreire.Create;
  proLink3:       FProvedorClass := TProvedorLink3.Create;
  proSpeedGov:    FProvedorClass := TProvedorSpeedGov.Create;
  proVitoria:     FProvedorClass := TProvedorVitoria.Create;
  proMitra:       FProvedorClass := TProvedorMitra.Create;
  proTecnos:      FProvedorClass := TProvedorTecnos.Create;
  proPronim:      FProvedorClass := TProvedorPronim.Create;
  proActcon:      FProvedorClass := TProvedorActcon.Create;
  proEL:          FProvedorClass := TProvedorEL.Create;
  proEgoverneISS: FProvedorClass := TProvedorEgoverneISS.Create;
  proSisPMJP:     FProvedorClass := TProvedorSisPMJP.Create;
  proSystemPro:   FProvedorClass := TProvedorSystemPro.Create;
  proSalvador:    FProvedorClass := TProvedorSalvador.Create;
  proDBSeller:    FProvedorClass := TProvedorDBSeller.Create;
  proLexsom:      FProvedorClass := TProvedorLexsom.Create;
  proABRASFv1:    FProvedorClass := TProvedorABRASFv1.Create;
  proABRASFv2:    FProvedorClass := TProvedorABRASFv2.Create;
  proNFSEBrasil:  FProvedorClass := TProvedorNFSEBrasil.Create;
  proSJP:         FProvedorClass := TProvedorSJP.Create;
  proCONAM:       FProvedorClass := TProvedorCONAM.Create;
 end;

 ConfigCidade   := FProvedorClass.GetConfigCidade(FCodigoMunicipio, FAmbienteCodigo);

 FVersaoSoap    := ConfigCidade.VersaoSoap;
 FPrefixo2      := ConfigCidade.Prefixo2;
 FPrefixo3      := ConfigCidade.Prefixo3;
 FPrefixo4      := ConfigCidade.Prefixo4;
 FIdentificador := ConfigCidade.Identificador;
 FNameSpace     := ConfigCidade.NameSpaceEnvelope;
 FQuebradeLinha := ConfigCidade.QuebradeLinha;

 TConfiguracoes( Self.Owner ).Certificados.FAssinaRPS   := ConfigCidade.AssinaRPS;
 TConfiguracoes( Self.Owner ).Certificados.FAssinaLote  := ConfigCidade.AssinaLote;
 TConfiguracoes( Self.Owner ).Certificados.FAssinaGerar := ConfigCidade.AssinaGerar;

 ConfigSchema := FProvedorClass.GetConfigSchema(FCodigoMunicipio);

 FVersaoCabecalho       := ConfigSchema.VersaoCabecalho;
 FVersaoDados           := ConfigSchema.VersaoDados;
 FVersaoXML             := ConfigSchema.VersaoXML;
 FURL                   := ConfigSchema.NameSpaceXML;
 FCabecalho             := ConfigSchema.Cabecalho;
 FServicoEnviar         := ConfigSchema.ServicoEnviar;
 FServicoConSit         := ConfigSchema.ServicoConSit;
 FServicoConLot         := ConfigSchema.ServicoConLot;
 FServicoConRps         := ConfigSchema.ServicoConRps;
 FServicoConSeqRps      := ConfigSchema.ServicoConSeqRps;
 FServicoConNfse        := ConfigSchema.ServicoConNfse;
 FServicoCancelar       := ConfigSchema.ServicoCancelar;
 FServicoGerar          := ConfigSchema.ServicoGerar;
 FServicoEnviarSincrono := ConfigSchema.ServicoEnviarSincrono;
 FServicoSubstituir     := ConfigSchema.ServicoSubstituir;
 FDefTipos              := ConfigSchema.DefTipos;

 ConfigURL := FProvedorClass.GetConfigURL(FCodigoMunicipio);

 FHomNomeCidade         := ConfigURL.HomNomeCidade;
 FHomRecepcaoLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
 FHomConsultaLoteRPS    := ConfigURL.HomConsultaLoteRPS;
 FHomConsultaSeqRPS     := ConfigURL.HomConsultaSeqRPS;
 FHomConsultaNFSeRPS    := ConfigURL.HomConsultaNFSeRPS;
 FHomConsultaSitLoteRPS := ConfigURL.HomConsultaSitLoteRPS;
 FHomConsultaNFSe       := ConfigURL.HomConsultaNFSe;
 FHomCancelaNFSe        := ConfigURL.HomCancelaNFSe;
 FHomGerarNFSe          := ConfigURL.HomGerarNFSe;
 FHomRecepcaoSincrono   := ConfigURL.HomRecepcaoSincrono;
 FHomSubstituiNFSe      := ConfigURL.HomSubstituiNFSe;

 FProNomeCidade         := ConfigURL.ProNomeCidade;
 FProRecepcaoLoteRPS    := ConfigURL.ProRecepcaoLoteRPS;
 FProConsultaLoteRPS    := ConfigURL.ProConsultaLoteRPS;
 FProConsultaSeqRPS     := ConfigURL.ProConsultaSeqRPS;
 FProConsultaNFSeRPS    := ConfigURL.ProConsultaNFSeRPS;
 FProConsultaSitLoteRPS := ConfigURL.ProConsultaSitLoteRPS;
 FProConsultaNFSe       := ConfigURL.ProConsultaNFSe;
 FProCancelaNFSe        := ConfigURL.ProCancelaNFSe;
 FProGerarNFSe          := ConfigURL.ProGerarNFSe;
 FProRecepcaoSincrono   := ConfigURL.ProRecepcaoSincrono;
 FProSubstituiNFSe      := ConfigURL.ProSubstituiNFSe;
end;

procedure TWebServicesConf.SetIntervaloTentativas(const Value: Cardinal);
begin
 if (Value > 0) and (Value < 1000)
  then FIntervaloTentativas := 1000
  else FIntervaloTentativas := Value;
end;

procedure TWebServicesConf.SetTentativas(const Value: Integer);
begin
 if Value <= 0
  then FTentativas := 18
  else FTentativas := Value;
end;

{ TArquivosConf }

constructor TArquivosConf.Create(AOwner: TComponent);
begin
 inherited;

end;

function TArquivosConf.GetPathCan(Data: TDateTime = 0): String;
var
 wDia, wMes, wAno : Word;
 Dir : String;
begin
 if EstaVazio(FPathCan)
  then Dir := TConfiguracoes( Self.Owner ).Geral.PathSalvar
  else Dir := FPathCan;

 if FMensal
  then begin
   if Data = 0
    then Data := Now;
   DecodeDate(Data, wAno, wMes, wDia);
   if Pos(IntToStr(wAno)+IntToStrZero(wMes,2),Dir) <= 0
    then Dir := PathWithDelim(Dir)+IntToStr(wAno)+IntToStrZero(wMes,2);
  end;

 if FLiteral
  then begin
   if copy(Dir,length(Dir)-2,3) <> 'Can'
    then Dir := PathWithDelim(Dir)+'Can';
  end;

 if not DirectoryExists(Dir)
  then ForceDirectories(Dir);

 Result := Dir;
end;

function TArquivosConf.GetPathNFSe(Data: TDateTime): String;
var
 wDia, wMes, wAno : Word;
 Dir : String;
begin
 if EstaVazio(FPathNFSe)
  then Dir := TConfiguracoes( Self.Owner ).Geral.PathSalvar
  else Dir := FPathNFSe;

 if FMensal
  then begin
   if Data = 0
    then Data := Now;
   DecodeDate(Data, wAno, wMes, wDia);
   if Pos(IntToStr(wAno)+IntToStrZero(wMes,2),Dir) <= 0
    then Dir := PathWithDelim(Dir)+IntToStr(wAno)+IntToStrZero(wMes,2);
  end;

 if FLiteral
  then begin
   if copy(Dir,length(Dir)-2,3) <> 'NFSe'
    then Dir := PathWithDelim(Dir)+'NFSe';
  end;

 if not DirectoryExists(Dir)
  then ForceDirectories(Dir);

 Result := Dir;
end;

function TArquivosConf.GetPathRPS(Data: TDateTime): String;
var
 wDia, wMes, wAno : Word;
 Dir : String;
begin
 if EstaVazio(FPathRPS)
  then Dir := TConfiguracoes( Self.Owner ).Geral.PathSalvar
  else Dir := FPathRPS;

 if FMensal
  then begin
   if Data = 0
    then Data := Now;
   DecodeDate(Data, wAno, wMes, wDia);
   if Pos(IntToStr(wAno)+IntToStrZero(wMes,2),Dir) <= 0
    then Dir := PathWithDelim(Dir)+IntToStr(wAno)+IntToStrZero(wMes,2);
  end;

 if FLiteral
  then begin
   if copy(Dir,length(Dir)-2,3) <> 'RPS'
    then Dir := PathWithDelim(Dir)+'RPS';
  end;

 if not DirectoryExists(Dir)
  then ForceDirectories(Dir);

 Result := Dir;
end;

function TArquivosConf.GetPathGer(Data: TDateTime = 0): String;
var
 wDia, wMes, wAno : Word;
 Dir : String;
begin
 if EstaVazio(FPathGer)
  then Dir := TConfiguracoes( Self.Owner ).Geral.PathSalvar
  else Dir := FPathGer;

 if FMensal
  then begin
   if Data = 0
    then Data := Now;
   DecodeDate(Data, wAno, wMes, wDia);
   if Pos(IntToStr(wAno)+IntToStrZero(wMes,2),Dir) <= 0
    then Dir := PathWithDelim(Dir)+IntToStr(wAno)+IntToStrZero(wMes,2);
  end;

 if FLiteral
  then begin
   if copy(Dir,length(Dir)-2,3) <> 'Ger'
    then Dir := PathWithDelim(Dir)+'Ger';
  end;

 if not DirectoryExists(Dir)
  then ForceDirectories(Dir);

 Result := Dir;
end;

{ TProvedorClass }

constructor TProvedorClass.Create;
begin
 {----}
end;

end.
