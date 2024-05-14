{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFSeXParametros;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrXmlBase, ACBrNFSeXConversao;

type

  { TACBrNFSeXConfigParams }

  TACBrNFSeXConfigParams = Class
  private
    fSL: TStringList;
    FAsString: String;

    procedure SetAsString(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    function TemParametro(const AParam: String): Boolean;
    function ValorParametro(const AParam: String): String;
    function ParamTemValor(const AParam, AValor: String): Boolean;

    property AsString: String read FAsString write SetAsString;
  end;

  { TACBrNFSeXAutenticacao }

  TACBrNFSeXAutenticacao = Class
  private
    FRequerCertificado: Boolean;
    FRequerLogin: Boolean;
    FRequerChaveAcesso: Boolean;
    FRequerChaveAutorizacao: Boolean;
    FRequerFraseSecreta: Boolean;
  public
    property RequerCertificado: Boolean read FRequerCertificado write FRequerCertificado;
    property RequerLogin: Boolean read FRequerLogin write FRequerLogin;
    property RequerChaveAcesso: Boolean read FRequerChaveAcesso write FRequerChaveAcesso;
    property RequerChaveAutorizacao: Boolean read FRequerChaveAutorizacao write FRequerChaveAutorizacao;
    property RequerFraseSecreta: Boolean read FRequerFraseSecreta write FRequerFraseSecreta;
  end;

  { TACBrNFSeXServicosDispobilizados }

  TACBrNFSeXServicosDispobilizados = Class
  private
    FEnviarLoteAssincrono: Boolean;
    FEnviarLoteSincrono: Boolean;
    FEnviarUnitario: Boolean;
    FConsultarSituacao: Boolean;
    FConsultarLote: Boolean;
    FConsultarRps: Boolean;
    FConsultarNfse: Boolean;
    FConsultarFaixaNfse: Boolean;
    FConsultarServicoPrestado: Boolean;
    FConsultarServicoTomado: Boolean;
    FCancelarNfse: Boolean;
    FSubstituirNfse: Boolean;
    FGerarToken: Boolean;
    FEnviarEvento: Boolean;
    FConsultarEvento: Boolean;
    FConsultarDFe: Boolean;
    FConsultarParam: Boolean;
    FConsultarSeqRps: Boolean;
    FConsultarLinkNfse: Boolean;
    FConsultarNfseChave: Boolean;
    FTestarEnvio: Boolean;
  public
    property EnviarLoteAssincrono: Boolean read FEnviarLoteAssincrono write FEnviarLoteAssincrono;
    property EnviarLoteSincrono: Boolean read FEnviarLoteSincrono write FEnviarLoteSincrono;
    property EnviarUnitario: Boolean read FEnviarUnitario write FEnviarUnitario;
    property ConsultarSituacao: Boolean read FConsultarSituacao write FConsultarSituacao;
    property ConsultarLote: Boolean read FConsultarLote write FConsultarLote;
    property ConsultarRps: Boolean read FConsultarRps write FConsultarRps;
    property ConsultarNfse: Boolean read FConsultarNfse write FConsultarNfse;
    property ConsultarFaixaNfse: Boolean read FConsultarFaixaNfse write FConsultarFaixaNfse;
    property ConsultarServicoPrestado: Boolean read FConsultarServicoPrestado write FConsultarServicoPrestado;
    property ConsultarServicoTomado: Boolean read FConsultarServicoTomado write FConsultarServicoTomado;
    property CancelarNfse: Boolean read FCancelarNfse write FCancelarNfse;
    property SubstituirNfse: Boolean read FSubstituirNfse write FSubstituirNfse;
    property GerarToken: Boolean read FGerarToken write FGerarToken;
    property EnviarEvento: Boolean read FEnviarEvento write FEnviarEvento;
    property ConsultarEvento: Boolean read FConsultarEvento write FConsultarEvento;
    property ConsultarDFe: Boolean read FConsultarDFe write FConsultarDFe;
    property ConsultarParam: Boolean read FConsultarParam write FConsultarParam;
    property ConsultarSeqRps: Boolean read FConsultarSeqRps write FConsultarSeqRps;
    property ConsultarLinkNfse: Boolean read FConsultarLinkNfse write FConsultarLinkNfse;
    property ConsultarNfseChave: Boolean read FConsultarNfseChave write FConsultarNfseChave;
    property TestarEnvio: Boolean read FTestarEnvio write FTestarEnvio;
  end;

  TACBrNFSeXParticularidades = class
  private
    FPermiteMaisDeUmServico: Boolean;
    FPermiteTagOutrasInformacoes: Boolean;
  public
    property PermiteMaisDeUmServico: Boolean read FPermiteMaisDeUmServico write FPermiteMaisDeUmServico;
    property PermiteTagOutrasInformacoes: Boolean read FPermiteTagOutrasInformacoes write FPermiteTagOutrasInformacoes;
  end;

  { TConfigGeral }

  TConfigGeral = class
  private
    // define como é o atributo ID: "Id" ou "id", se for fazio o atributo não é gerado
    FIdentificador: string;
    // define o caracter ou caracteres a serem usados como quebra de linha
    FQuebradeLinha: string;
    // define se vai usar certificado digital ou não
    FUseCertificateHTTP: boolean;
    // define se vai usar autorização no cabeçalho ou não
    FUseAuthorizationHeader: boolean;
    // define o numero maximo de Rps a serem incluidos no GerarNfse
    FNumMaxRpsGerar: integer;
    // define o numero maximo de Rps a serem incluidos no EnviarLoteRpsEnvio e EnviarLoteRpsSincronoEnvio
    FNumMaxRpsEnviar: integer;
    // define o numero minimo de Rps a serem incluidos no EnviarLoteRpsEnvio e EnviarLoteRpsSincronoEnvio
    FNumMinRpsEnviar: integer;
    // define se vai ser utilizado uma tabela externa de serviço ou não
    FTabServicosExt: Boolean;
    // define o modo de envio dos Rps para o webservice
    FModoEnvio: TmodoEnvio;
    // define se vai consultar a situação do lote ou não, após o envio
    FConsultaSitLote: Boolean;
    // define se vai consultar o lote ou não, após o envio
    FConsultaLote: Boolean;
    // define se vai consultar a NFS-e ou não, após o cancelamento
    FConsultaNFSe: Boolean;
    // define se vai consultar a NFS-e por faixa ou não, após o cancelamento
    FConsultaPorFaixa: Boolean;
    // define se precisa preencher o MotCancelamento ou não ao cancelar Nfse
    FCancPreencherMotivo: Boolean;
    // define se precisa preencher o SerieNfse ou não ao cancelar Nfse
    FCancPreencherSerieNfse: Boolean;
    // define se precisa preencher o CodVerificacao ou não ao cancelar Nfse
    FCancPreencherCodVerificacao: Boolean;
    // define se vai gerar ou não a tag <NumeroNfseFinal> na consulta por faixa
    FConsultaPorFaixaPreencherNumNfseFinal: Boolean;

    // uso diverso
    FParams: TACBrNFSeXConfigParams;

    // Provedor lido do arquivo ACBrNFSeXServicos
    FProvedor: TnfseProvedor;
    // Versão lido do arquivo ACBrNFSeXServicos
    FVersao: TVersaoNFSe;
    // Nome lido do arquivo ACBrNFSeXServicos
    FxMunicipio: string;
    // Ambiente setando na configuração do componente
    FAmbiente: TACBrTipoAmbiente;
    // Código IBGE da Cidade lido do arquivo ACBrNFSeXServicos
    FCodIBGE: string;
    // define se deve imprimir o conteudo do campo Discriminação ou a lista de
    // serviços
    FDetalharServico: Boolean;
    // Layout setando ao ler o provedor
    FLayout: TLayout;
    FIniTabServicos: string;
    // Formato do Arquivo de Envio utilizado pelo provedor
    FFormatoArqEnvio: TFormatoArq;
    // Formato do Arquivo de Retorno utilizado pelo provedor
    FFormatoArqRetorno: TFormatoArq;
    // Formato do Arquivo de Envio Soap utilizado pelo provedor
    FFormatoArqEnvioSoap: TFormatoArq;
    // Formato do Arquivo de Retorno Soap utilizado pelo provedor
    FFormatoArqRetornoSoap: TFormatoArq;
    // Formato do Arquivo do Recibo utilizado pelo provedor
    FFormatoArqRecibo: TFormatoArq;
    // Formato do Arquivo da Nota utilizado pelo provedor
    FFormatoArqNota: TFormatoArq;
    // Formato do Arquivo do Evento utilizado pelo provedor
    FFormatoArqEvento: TFormatoArq;
    // define se deve imprimir o Local da Prestação de Serviço ou não
    FImprimirLocalPrestServ: Boolean;
    FAutenticacao: TACBrNFSeXAutenticacao;
    FServicosDisponibilizados: TACBrNFSeXServicosDispobilizados;
    FParticularidades: TACBrNFSeXParticularidades;
    FImprimirOptanteSN: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadParams(AINI: TCustomIniFile; const ASession: string);

    property Identificador: string read FIdentificador write FIdentificador;
    property QuebradeLinha: string read FQuebradeLinha write FQuebradeLinha;
    property UseCertificateHTTP: boolean read FUseCertificateHTTP write FUseCertificateHTTP;
    property UseAuthorizationHeader: boolean read FUseAuthorizationHeader write FUseAuthorizationHeader;
    property NumMaxRpsGerar: integer read FNumMaxRpsGerar write FNumMaxRpsGerar;
    property NumMaxRpsEnviar: integer read FNumMaxRpsEnviar write FNumMaxRpsEnviar;
    property NumMinRpsEnviar: integer read FNumMinRpsEnviar write FNumMinRpsEnviar;
    property TabServicosExt: Boolean read FTabServicosExt write FTabServicosExt;
    property ModoEnvio: TmodoEnvio read FModoEnvio write FModoEnvio;
    property ConsultaSitLote: Boolean read FConsultaSitLote write FConsultaSitLote;
    property ConsultaLote: Boolean read FConsultaLote write FConsultaLote;
    property ConsultaNFSe: Boolean read FConsultaNFSe write FConsultaNFSe;
    property ConsultaPorFaixa: Boolean read FConsultaPorFaixa write FConsultaPorFaixa;
    property CancPreencherMotivo: Boolean read FCancPreencherMotivo write FCancPreencherMotivo;
    property CancPreencherSerieNfse: Boolean read FCancPreencherSerieNfse write FCancPreencherSerieNfse;
    property CancPreencherCodVerificacao: Boolean read FCancPreencherCodVerificacao write FCancPreencherCodVerificacao;
    property ConsultaPorFaixaPreencherNumNfseFinal: Boolean read FConsultaPorFaixaPreencherNumNfseFinal write FConsultaPorFaixaPreencherNumNfseFinal;

    // Parametros lidos no arquivo .Res ou .ini
    property Params: TACBrNFSeXConfigParams read FParams;

    property Provedor: TnfseProvedor read FProvedor write FProvedor;
    property Versao: TVersaoNFSe read FVersao write FVersao;
    property xMunicipio: string read FxMunicipio write FxMunicipio;
    property Ambiente: TACBrTipoAmbiente read FAmbiente write FAmbiente;
    property CodIBGE: string read FCodIBGE write FCodIBGE;
    property DetalharServico: Boolean read FDetalharServico write FDetalharServico;
    property Layout: TLayout read FLayout write FLayout;
    property IniTabServicos: string read FIniTabServicos write FIniTabServicos;
    property FormatoArqEnvio: TFormatoArq read FFormatoArqEnvio write FFormatoArqEnvio;
    property FormatoArqRetorno: TFormatoArq read FFormatoArqRetorno write FFormatoArqRetorno;
    property FormatoArqEnvioSoap: TFormatoArq read FFormatoArqEnvioSoap write FFormatoArqEnvioSoap;
    property FormatoArqRetornoSoap: TFormatoArq read FFormatoArqRetornoSoap write FFormatoArqRetornoSoap;
    property FormatoArqRecibo: TFormatoArq read FFormatoArqRecibo write FFormatoArqRecibo;
    property FormatoArqNota: TFormatoArq read FFormatoArqNota write FFormatoArqNota;
    property FormatoArqEvento: TFormatoArq read FFormatoArqEvento write FFormatoArqEvento;
    property ImprimirLocalPrestServ: Boolean read FImprimirLocalPrestServ write FImprimirLocalPrestServ;
    property Autenticacao: TACBrNFSeXAutenticacao read FAutenticacao;
    property ServicosDisponibilizados: TACBrNFSeXServicosDispobilizados read FServicosDisponibilizados;
    property Particularidades: TACBrNFSeXParticularidades read FParticularidades write FParticularidades;
    property ImprimirOptanteSN: Boolean read FImprimirOptanteSN write FImprimirOptanteSN;
  end;

  { TWebserviceInfo }
  TWebserviceInfo = class
  private
    // URL para verificação no site se a note realmente existe
    FLinkURL: string;
    // NameSpace utilizado no Envelope Soap
    FNameSpace: string;
    // NameSpace utilizado no XML
    FXMLNameSpace: string;
    // URL de homologação ou produção para o serviço Recepcionar
    FRecepcionar: string;
    // URL de homologação ou produção para o serviço ConsultarLote
    FConsultarLote: string;
    // URL de homologação ou produção para o serviço ConsultarNFSeRps
    FConsultarNFSeRps: string;
    // URL de homologação ou produção para o serviço ConsultarSituacao
    FConsultarSituacao: string;
    // URL de homologação ou produção para o serviço ConsultarNFSe
    FConsultarNFSe: string;
    // URL de homologação ou produção para o serviço ConsultarNFSePorChave
    FConsultarNFSePorChave: string;
    // URL de homologação ou produção para o serviço ConsultarNFSePorFaixa
    FConsultarNFSePorFaixa: string;
    // URL de homologação ou produção para o serviço ConsultarNFSeServicoPrestado
    FConsultarNFSeServicoPrestado: string;
    // URL de homologação ou produção para o serviço ConsultarNFSeServicoTomado
    FConsultarNFSeServicoTomado: string;
    // URL de homologação ou produção para o serviço ConsultarLinkNFSe
    FConsultarLinkNFSe: string;
    // URL de homologação ou produção para o serviço CancelarNFSe
    FCancelarNFSe: string;
    // URL de homologação ou produção para o serviço GerarNFSe
    FGerarNFSe: string;
    // URL de homologação ou produção para o serviço RecepcionarSincrono
    FRecepcionarSincrono: string;
    // URL de homologação ou produção para o serviço SubstituirNFSe
    FSubstituirNFSe: string;
    // URL de homologação ou produção para o serviço AbrirSessao
    FAbrirSessao: string;
    // URL de homologação ou produção para o serviço FecharSessao
    FFecharSessao: string;
    // URL de homologação ou produção para o serviço TesteEnvio
    FTesteEnvio: string;
    // URL de homologação ou produção do SoapAction
    FSoapAction: string;
    // URL de homologação ou produção para o serviço GerarToken
    FGerarToken: string;
    // URL de homologação ou produção para o serviço EnviarEvento
    FEnviarEvento: string;
    // URL de homologação ou produção para o serviço ConsultarEvento
    FConsultarEvento: string;
    // URL de homologação ou produção para o serviço ConsultarDFe
    FConsultarDFe: string;
    // URL de homologação ou produção para o serviço ConsultarParam
    FConsultarParam: string;
    // URL de homologação ou produção para o serviço ConsultarSeqRps
    FConsultarSeqRps: string;

  public
    property LinkURL: string read FLinkURL;
    property NameSpace: string read FNameSpace;
    property XMLNameSpace: string read FXMLNameSpace;
    property Recepcionar: string read FRecepcionar;
    property ConsultarLote: string read FConsultarLote;
    property ConsultarNFSeRps: string read FConsultarNFSeRps;
    property ConsultarSituacao: string read FConsultarSituacao;
    property ConsultarNFSe: string read FConsultarNFSe;
    property ConsultarNFSePorChave: string read FConsultarNFSePorChave;
    property ConsultarNFSePorFaixa: string read FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: string read FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: string read FConsultarNFSeServicoTomado;
    property ConsultarLinkNFSe: string read FConsultarLinkNFSe;
    property CancelarNFSe: string read FCancelarNFSe;
    property GerarNFSe: string read FGerarNFSe;
    property RecepcionarSincrono: string read FRecepcionarSincrono;
    property SubstituirNFSe: string read FSubstituirNFSe;
    property AbrirSessao: string read FAbrirSessao;
    property FecharSessao: string read FFecharSessao;
    property TesteEnvio: string read FTesteEnvio;
    property SoapAction: string read FSoapAction;
    property GerarToken: string read FGerarToken;
    property EnviarEvento: string read FEnviarEvento;
    property ConsultarEvento: string read FConsultarEvento;
    property ConsultarDFe: string read FConsultarDFe;
    property ConsultarParam: string read FConsultarParam;
    property ConsultarSeqRps: string read FConsultarSeqRps;

  end;

  { TConfigWebServices }
  TConfigWebServices = class
  private
    // Versão dos dados informado na tag <VersaoDados>
    FVersaoDados: string;
    // Versão informada no atributo versao=
    FVersaoAtrib: string;
    // Grafia do atributo usado como versão no Lote de Rps
    FAtribVerLote: string;
    // Grupo de URLs do Ambiente de Produção
    FProducao: TWebserviceInfo;
    // Grupo de URLs do Ambiente de Homologação
    FHomologacao: TWebserviceInfo;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadUrlProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadUrlHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadLinkUrlProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadLinkUrlHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadXMLNameSpaceProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadXMLNameSpaceHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadNameSpaceProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadNameSpaceHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadSoapActionProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadSoapActionHomologacao(AINI: TCustomIniFile; const ASession: string);

    property VersaoDados: string read FVersaoDados write FVersaoDados;
    property VersaoAtrib: string read FVersaoAtrib write FVersaoAtrib;
    property AtribVerLote: string read FAtribVerLote write FAtribVerLote;
    property Producao: TWebserviceInfo read FProducao;
    property Homologacao: TWebserviceInfo read FHomologacao;

  end;

  { TDocElement }
  TDocElement = class
  private
    // contem o namespace a ser incluido no XML
    Fxmlns: string;
    // nome do elemento a ser utilizado na assinatura digital que contem o atributo ID
    FInfElemento: string;
    // nome do elemento do documento a ser assinado
    FDocElemento: string;

  public
    property xmlns: string read Fxmlns write Fxmlns;
    property InfElemento: string read FInfElemento write FInfElemento;
    property DocElemento: string read FDocElemento write FDocElemento;
  end;

  { TConfigMsgDados }
  TConfigMsgDados = class
  private
    // Alguns provedores como o Ginfes existem a presença de prefixo nas tags
    FPrefixo: String;
    // Prefixo para Tipo Simples usado na montagem do XML de envio
    FPrefixoTS: String;
    // Contem o XML do cabeçalho exigido por alguns provedores
    FDadosCabecalho: String;

    // Contem a definição dos campos TDocElement para o XML do Rps
    FXmlRps: TDocElement;
    // Contem a definição dos campos TDocElement para o XML do Lote Rps
    FLoteRps: TDocElement;
    // Contem a definição dos campos TDocElement para o XML do Lote Rps Sincrono
    FLoteRpsSincrono: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta a Situação
    FConsultarSituacao: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta ao Lote
    FConsultarLote: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta a NFSe por Rps
    FConsultarNFSeRps: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta a NFS-e
    FConsultarNFSe: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta a NFS-e por Chave
    FConsultarNFSePorChave: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta a NFS-e por Faixa
    FConsultarNFSePorFaixa: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta a NFS-e Serviço Prestado
    FConsultarNFSeServicoPrestado: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta a NFS-e Serviço Tomado
    FConsultarNFSeServicoTomado: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Consulta do Link da NFS-e
    FConsultarLinkNFSe: TDocElement;
    // Contem a definição dos campos TDocElement para o XML do Cancelamento da NFS-e
    FCancelarNFSe: TDocElement;
    // Contem a definição dos campos TDocElement para o XML do Gerar NFS-e
    FGerarNFSe: TDocElement;
    // Contem a definição dos campos TDocElement para o XML da Substituição da NFS-e
    FSubstituirNFSe: TDocElement;
    // Contem a definição dos campos TDocElement para o XML do Abrir Sessão
    FAbrirSessao: TDocElement;
    // Contem a definição dos campos TDocElement para o XML do Fechar Sessão
    FFecharSessao: TDocElement;
    // Contem a definição dos campos TDocElement para o XML de Gerar o Token
    FGerarToken: TDocElement;
    // Contem a definição dos campos TDocElement para o XML de Enviar Evento
    FEnviarEvento: TDocElement;
    // Contem a definição dos campos TDocElement para o XML de Consultar Evento
    FConsultarEvento: TDocElement;
    // Contem a definição dos campos TDocElement para o XML de Consultar DFe
    FConsultarDFe: TDocElement;
    // Contem a definição dos campos TDocElement para o XML de Consultar Param
    FConsultarParam: TDocElement;
    // Contem a definição dos campos TDocElement para o XML de Consultar Sequencial RPS
    FConsultarSeqRps: TDocElement;

    // Se True gera o namespace no Lote de Rps
    FGerarNSLoteRps: Boolean;
    // Se True gera o Prestador no Lote de Rps
    FGerarPrestadorLoteRps: Boolean;
    // Se True gera o grupo <NumeroLote> ao Consultar a Situação e o Lote
    FUsarNumLoteConsLote: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    property Prefixo: String read FPrefixo write FPrefixo;
    property PrefixoTS: String read FPrefixoTS write FPrefixoTS;
    property DadosCabecalho: String read FDadosCabecalho write FDadosCabecalho;

    property XmlRps: TDocElement read FXmlRps;
    property LoteRps: TDocElement read FLoteRps;
    property LoteRpsSincrono: TDocElement read FLoteRpsSincrono;
    property ConsultarSituacao: TDocElement read FConsultarSituacao;
    property ConsultarLote: TDocElement read FConsultarLote;
    property ConsultarNFSeRps: TDocElement read FConsultarNFSeRps;
    property ConsultarNFSe: TDocElement read FConsultarNFSe;
    property ConsultarNFSePorChave: TDocElement read FConsultarNFSePorChave;
    property ConsultarNFSePorFaixa: TDocElement read FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: TDocElement read FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: TDocElement read FConsultarNFSeServicoTomado;
    property ConsultarLinkNFSe: TDocElement read FConsultarLinkNFSe;
    property CancelarNFSe: TDocElement read FCancelarNFSe;
    property GerarNFSe: TDocElement read FGerarNFSe;
    property SubstituirNFSe: TDocElement read FSubstituirNFSe;
    property AbrirSessao: TDocElement read FAbrirSessao;
    property FecharSessao: TDocElement read FFecharSessao;
    property GerarToken: TDocElement read FGerarToken;
    property EnviarEvento: TDocElement read FEnviarEvento;
    property ConsultarEvento: TDocElement read FConsultarEvento;
    property ConsultarDFe: TDocElement read FConsultarDFe;
    property ConsultarParam: TDocElement read FConsultarParam;
    property ConsultarSeqRps: TDocElement read FConsultarSeqRps;

    property GerarNSLoteRps: Boolean read FGerarNSLoteRps write FGerarNSLoteRps;
    property GerarPrestadorLoteRps: Boolean read FGerarPrestadorLoteRps write FGerarPrestadorLoteRps;
    property UsarNumLoteConsLote: Boolean read FUsarNumLoteConsLote write FUsarNumLoteConsLote;
  end;

  { TConfigAssinar }
  TConfigAssinar = class
  private
    // Se True assina o Rps
    FRps: boolean;
    // Se True assina o Lote de Rps
    FLoteRps: boolean;
    // Se True assina a Consulta a Situação
    FConsultarSituacao: boolean;
    // Se True assina a Consulta ao Lote
    FConsultarLote: boolean;
    // Se True assina a Consulta a NFS-e por Rps
    FConsultarNFSeRps: boolean;
    // Se True assina a Consulta a NFS-e
    FConsultarNFSe: boolean;
    // Se True assina a Consulta a NFS-e por Chave
    FConsultarNFSePorChave: boolean;
    // Se True assina a Consulta a NFS-e por Faixa
    FConsultarNFSePorFaixa: boolean;
    // Se True assina a Consulta a NFS-e Serviço Prestado
    FConsultarNFSeServicoPrestado: boolean;
    // Se True assina a Consulta a NFS-e Serviço Tomado
    FConsultarNFSeServicoTomado: boolean;
    // Se True assina a Consulta de Link da NFS-e
    FConsultarLinkNFSe: boolean;
    // Se True assina o Cancelamento da NFS-e
    FCancelarNFSe: boolean;
    // Se True assina o Rps do Gerar NFS-e
    FRpsGerarNFSe: boolean;
    // Se True assina o Lote do Gerar NFS-e
    FLoteGerarNFSe: boolean;
    // Se True assina o Rps do Substituir NFS-e
    FRpsSubstituirNFSe: boolean;
    // Se True assina o Substituir NFS-e
    FSubstituirNFSe: boolean;
    // Se True assina o Abrir Sessão
    FAbrirSessao: boolean;
    // Se True assina o Fechar Sessão
    FFecharSessao: boolean;
    // Se True assina a Geração do Token
    FGerarToken: boolean;
    // Se True assina o Enviar Evento
    FEnviarEvento: boolean;
    // Se True assina o Consultar Evento
    FConsultarEvento: boolean;
    // Se True assina o Consultar DFe
    FConsultarDFe: boolean;
    // Se True assina o Consultar Param
    FConsultarParam: boolean;
    // se True assina o Consultar Sequencial Rps
    FConsultarSeqRps: boolean;
    // Se True Incluir o valor de ID na URI da assinatura
    FIncluirURI: boolean;
    // Se True gera uma assinatura adicional
    FAssinaturaAdicional: boolean;
    // Tipo de Assinaturas
    FAssinaturas: TAssinaturas;
    // Contem o conteudo do atributo Id da tag SignatureValue
    FIdSignatureValue: string;

  public
    property Rps: boolean read FRps write FRps;
    property LoteRps: boolean read FLoteRps write FLoteRps;
    property ConsultarSituacao: boolean read FConsultarSituacao write FConsultarSituacao;
    property ConsultarLote: boolean read FConsultarLote write FConsultarLote;
    property ConsultarNFSeRps: boolean read FConsultarNFSeRps write FConsultarNFSeRps;
    property ConsultarNFSe: boolean read FConsultarNFSe write FConsultarNFSe;
    property ConsultarNFSePorChave: boolean read FConsultarNFSePorChave write FConsultarNFSePorChave;
    property ConsultarNFSePorFaixa: boolean read FConsultarNFSePorFaixa write FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: boolean read FConsultarNFSeServicoPrestado write FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: boolean read FConsultarNFSeServicoTomado write FConsultarNFSeServicoTomado;
    property ConsultarLinkNFSe: boolean read FConsultarLinkNFSe write FConsultarLinkNFSe;
    property CancelarNFSe: boolean read FCancelarNFSe write FCancelarNFSe;
    property RpsGerarNFSe: boolean read FRpsGerarNFSe write FRpsGerarNFSe;
    property LoteGerarNFSe: boolean read FLoteGerarNFSe write FLoteGerarNFSe;
    property RpsSubstituirNFSe: boolean read FRpsSubstituirNFSe write FRpsSubstituirNFSe;
    property SubstituirNFSe: boolean read FSubstituirNFSe write FSubstituirNFSe;
    property AbrirSessao: boolean read FAbrirSessao write FAbrirSessao;
    property FecharSessao: boolean read FFecharSessao write FFecharSessao;
    property GerarToken: boolean read FGerarToken write FGerarToken;
    property EnviarEvento: boolean read FEnviarEvento write FEnviarEvento;
    property ConsultarEvento: boolean read FConsultarEvento write FConsultarEvento;
    property ConsultarDFe: boolean read FConsultarDFe write FConsultarDFe;
    property ConsultarParam: boolean read FConsultarParam write FConsultarParam;
    property ConsultarSeqRps: boolean read FConsultarSeqRps write FConsultarSeqRps;

    property IncluirURI: boolean read FIncluirURI write FIncluirURI;
    property AssinaturaAdicional: boolean read FAssinaturaAdicional write FAssinaturaAdicional;
    property Assinaturas: TAssinaturas read FAssinaturas write FAssinaturas;
    property IdSignatureValue: string read FIdSignatureValue write FIdSignatureValue;
  end;

  { TConfigSchemas }
  TConfigSchemas = class
  private
    // Nome do arquivo XSD para validar o Recepcionar (Envio do Lote de Rps)
    FRecepcionar: string;
    // Nome do arquivo XSD para validar o Consultar Situação
    FConsultarSituacao: string;
    // Nome do arquivo XSD para validar o Consultar Lote
    FConsultarLote: string;
    // Nome do arquivo XSD para validar o Consultar NFSe por Rps
    FConsultarNFSeRps: string;
    // Nome do arquivo XSD para validar o Consultar NFSe
    FConsultarNFSe: string;
    // Nome do arquivo XSD para validar o Consultar NFSe por Chave
    FConsultarNFSePorChave: string;
    // Nome do arquivo XSD para validar o Consultar NFSe por Faixa
    FConsultarNFSePorFaixa: string;
    // Nome do arquivo XSD para validar o Consultar NFSe Serviço Prestado
    FConsultarNFSeServicoPrestado: string;
    // Nome do arquivo XSD para validar o Consultar NFSe Serviço Tomado
    FConsultarNFSeServicoTomado: string;
    // Nome do arquivo XSD para validar o Cancelar NFSe
    FCancelarNFSe: string;
    // Nome do arquivo XSD para validar o Gerar NFSe
    FGerarNFSe: string;
    // Nome do arquivo XSD para validar o Recepcionar Sincrono (Envio do Lote de Rps)
    FRecepcionarSincrono: string;
    // Nome do arquivo XSD para validar o Substituir NFSe
    FSubstituirNFSe: string;
    // Nome do arquivo XSD para validar o Abrir Sessão
    FAbrirSessao: string;
    // Nome do arquivo XSD para validar o Fechar Sessão
    FFecharSessao: string;
    // Nome do arquivo XSD para validar o Teste de Envio
    FTeste: string;
    // Nome do arquivo XSD para validar a Geração do Token
    FGerarToken: string;
    // Nome do arquivo XSD para validar o Enviar Evento
    FEnviarEvento: string;
    // Nome do arquivo XSD para validar o Consultar Evento
    FConsultarEvento: string;
    // Nome do arquivo XSD para validar o Consultar DFe
    FConsultarDFe: string;
    // Nome do arquivo XSD para validar o Consultar Param
    FConsultarParam: string;
    // Nome do arquivo XSD para validar o Consultar Sequencial Rps
    FConsultarSeqRps: string;
    // Nome do arquivo XSD para validar o Consultar Sequencial Rps
    FConsultarLinkNFSe: string;

    // Se True realiza a validação do XML com os Schemas
    FValidar: boolean;
  public
    property Recepcionar: string read FRecepcionar write FRecepcionar;
    property ConsultarSituacao: string read FConsultarSituacao write FConsultarSituacao;
    property ConsultarLote: string read FConsultarLote write FConsultarLote;
    property ConsultarNFSeRps: string read FConsultarNFSeRps write FConsultarNFSeRps;
    property ConsultarNFSe: string read FConsultarNFSe write FConsultarNFSe;
    property ConsultarNFSePorChave: string read FConsultarNFSePorChave write FConsultarNFSePorChave;
    property ConsultarNFSePorFaixa: string read FConsultarNFSePorFaixa write FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: string read FConsultarNFSeServicoPrestado write FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: string read FConsultarNFSeServicoTomado write FConsultarNFSeServicoTomado;
    property CancelarNFSe: string read FCancelarNFSe write FCancelarNFSe;
    property GerarNFSe: string read FGerarNFSe write FGerarNFSe;
    property RecepcionarSincrono: string read FRecepcionarSincrono write FRecepcionarSincrono;
    property SubstituirNFSe: string read FSubstituirNFSe write FSubstituirNFSe;
    property AbrirSessao: string read FAbrirSessao write FAbrirSessao;
    property FecharSessao: string read FFecharSessao write FFecharSessao;
    property Teste: string read FTeste write FTeste;
    property GerarToken: string read FGerarToken write FGerarToken;
    property EnviarEvento: string read FEnviarEvento write FEnviarEvento;
    property ConsultarEvento: string read FConsultarEvento write FConsultarEvento;
    property ConsultarDFe: string read FConsultarDFe write FConsultarDFe;
    property ConsultarParam: string read FConsultarParam write FConsultarParam;
    property ConsultarSeqRps: string read FConsultarSeqRps write FConsultarSeqRps;
    property ConsultarLinkNFSe: string read FConsultarLinkNFSe write FConsultarLinkNFSe;

    property Validar: boolean read FValidar write FValidar;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TACBrNFSeXConfigParams }

constructor TACBrNFSeXConfigParams.Create;
begin
  inherited Create;

  fSL := TStringList.Create;
end;

destructor TACBrNFSeXConfigParams.Destroy;
begin
  fSL.Free;

  inherited Destroy;
end;

function TACBrNFSeXConfigParams.ParamTemValor(const AParam,
  AValor: String): Boolean;
begin
  Result := (Pos(lowercase(AValor), lowercase(ValorParametro(AParam))) > 0);
end;

function TACBrNFSeXConfigParams.TemParametro(const AParam: String): Boolean;
var
  p: Integer;
begin
  p := fSL.IndexOfName(Trim(AParam));
  Result := (p >= 0);
end;

function TACBrNFSeXConfigParams.ValorParametro(const AParam: String): String;
begin
  Result := fSL.Values[AParam];
end;

procedure TACBrNFSeXConfigParams.SetAsString(const AValue: String);
var
  s: String;
begin
  if FAsString = AValue then Exit;
  FAsString := Trim(AValue);
  s := StringReplace(FAsString, ':', '=', [rfReplaceAll]);
  AddDelimitedTextToList(s, '|', fSL, #0);
end;

{ TConfigWebServices }

constructor TConfigWebServices.Create;
begin
  FProducao := TWebserviceInfo.Create;
  FHomologacao := TWebserviceInfo.Create;
end;

destructor TConfigWebServices.Destroy;
begin
  FProducao.Free;
  FHomologacao.Free;

  inherited Destroy;
end;

procedure TConfigWebServices.LoadLinkUrlHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Homologacao.FLinkURL := AINI.ReadString(ASession, 'HomLinkURL', '');
end;

procedure TConfigWebServices.LoadLinkUrlProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FLinkURL := AINI.ReadString(ASession, 'ProLinkURL', '');
end;

procedure TConfigWebServices.LoadNameSpaceHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Homologacao.FNameSpace := AINI.ReadString(ASession, 'HomNameSpace', '');
end;

procedure TConfigWebServices.LoadNameSpaceProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FNameSpace := AINI.ReadString(ASession, 'ProNameSpace', '');
end;

procedure TConfigWebServices.LoadXMLNameSpaceHomologacao(
  AINI: TCustomIniFile; const ASession: string);
begin
  Homologacao.FXMLNameSpace := AINI.ReadString(ASession, 'HomXMLNameSpace', '');
end;

procedure TConfigWebServices.LoadXMLNameSpaceProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FXMLNameSpace := AINI.ReadString(ASession, 'ProXMLNameSpace', '');
end;

procedure TConfigWebServices.LoadSoapActionHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Homologacao.FSoapAction := AINI.ReadString(ASession, 'HomSoapAction', '');
end;

procedure TConfigWebServices.LoadSoapActionProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FSoapAction := AINI.ReadString(ASession, 'ProSoapAction', '');
end;

procedure TConfigWebServices.LoadUrlHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  with Homologacao do
  begin
    FRecepcionar         := AINI.ReadString(ASession, 'HomRecepcionar'        , '');
    FConsultarSituacao   := AINI.ReadString(ASession, 'HomConsultarSituacao'  , FRecepcionar);
    FConsultarLote       := AINI.ReadString(ASession, 'HomConsultarLote'      , FRecepcionar);
    FConsultarNFSeRPS    := AINI.ReadString(ASession, 'HomConsultarNFSeRps'   , FRecepcionar);
    FConsultarNFSe       := AINI.ReadString(ASession, 'HomConsultarNFSe'      , FRecepcionar);
    FConsultarLinkNFSe   := AINI.ReadString(ASession, 'HomConsultarLinkNFSe'  , FRecepcionar);
    FCancelarNFSe        := AINI.ReadString(ASession, 'HomCancelarNFSe'       , FRecepcionar);
    FGerarNFSe           := AINI.ReadString(ASession, 'HomGerarNFSe'          , FRecepcionar);
    FRecepcionarSincrono := AINI.ReadString(ASession, 'HomRecepcionarSincrono', FRecepcionar);
    FSubstituirNFSe      := AINI.ReadString(ASession, 'HomSubstituirNFSe'     , FRecepcionar);
    FAbrirSessao         := AINI.ReadString(ASession, 'HomAbrirSessao'        , FRecepcionar);
    FFecharSessao        := AINI.ReadString(ASession, 'HomFecharSessao'       , FRecepcionar);
    FGerarToken          := AINI.ReadString(ASession, 'HomGerarToken'         , FRecepcionar);
    FEnviarEvento        := AINI.ReadString(ASession, 'HomEnviarEvento'       , FRecepcionar);
    FConsultarEvento     := AINI.ReadString(ASession, 'HomConsultarEvento'    , FRecepcionar);
    FConsultarDFe        := AINI.ReadString(ASession, 'HomConsultarDFe'       , FRecepcionar);
    FConsultarParam      := AINI.ReadString(ASession, 'HomConsultarParam'     , FRecepcionar);

    FConsultarNFSePorChave        := AINI.ReadString(ASession, 'HomConsultarNFSePorChave'       , FRecepcionar);
    FConsultarNFSePorFaixa        := AINI.ReadString(ASession, 'HomConsultarNFSePorFaixa'       , FRecepcionar);
    FConsultarNFSeServicoPrestado := AINI.ReadString(ASession, 'HomConsultarNFSeServicoPrestado', FRecepcionar);
    FConsultarNFSeServicoTomado   := AINI.ReadString(ASession, 'HomConsultarNFSeServicoTomado'  , FRecepcionar);

    FTesteEnvio      := AINI.ReadString(ASession, 'HomTesteEnvio'     , FRecepcionar);
    FConsultarSeqRps := AINI.ReadString(ASession, 'HomConsultarSeqRps', FRecepcionar);
  end;
end;

procedure TConfigWebServices.LoadUrlProducao(AINI: TCustomIniFile; const ASession: string);
begin
  with Producao do
  begin
    FRecepcionar         := AINI.ReadString(ASession, 'ProRecepcionar'        , '');
    FConsultarSituacao   := AINI.ReadString(ASession, 'ProConsultarSituacao'  , FRecepcionar);
    FConsultarLote       := AINI.ReadString(ASession, 'ProConsultarLote'      , FRecepcionar);
    FConsultarNFSeRPS    := AINI.ReadString(ASession, 'ProConsultarNFSeRps'   , FRecepcionar);
    FConsultarNFSe       := AINI.ReadString(ASession, 'ProConsultarNFSe'      , FRecepcionar);
    FConsultarLinkNFSe   := AINI.ReadString(ASession, 'ProConsultarLinkNFSe'  , FRecepcionar);
    FCancelarNFSe        := AINI.ReadString(ASession, 'ProCancelarNFSe'       , FRecepcionar);
    FGerarNFSe           := AINI.ReadString(ASession, 'ProGerarNFSe'          , FRecepcionar);
    FRecepcionarSincrono := AINI.ReadString(ASession, 'ProRecepcionarSincrono', FRecepcionar);
    FSubstituirNFSe      := AINI.ReadString(ASession, 'ProSubstituirNFSe'     , FRecepcionar);
    FAbrirSessao         := AINI.ReadString(ASession, 'ProAbrirSessao'        , FRecepcionar);
    FFecharSessao        := AINI.ReadString(ASession, 'ProFecharSessao'       , FRecepcionar);
    FGerarToken          := AINI.ReadString(ASession, 'ProGerarToken'         , FRecepcionar);
    FEnviarEvento        := AINI.ReadString(ASession, 'ProEnviarEvento'       , FRecepcionar);
    FConsultarEvento     := AINI.ReadString(ASession, 'ProConsultarEvento'    , FRecepcionar);
    FConsultarDFe        := AINI.ReadString(ASession, 'ProConsultarDFe'       , FRecepcionar);
    FConsultarParam      := AINI.ReadString(ASession, 'ProConsultarParam'     , FRecepcionar);

    FConsultarNFSePorChave        := AINI.ReadString(ASession, 'ProConsultarNFSePorChave'       , FRecepcionar);
    FConsultarNFSePorFaixa        := AINI.ReadString(ASession, 'ProConsultarNFSePorFaixa'       , FRecepcionar);
    FConsultarNFSeServicoPrestado := AINI.ReadString(ASession, 'ProConsultarNFSeServicoPrestado', FRecepcionar);
    FConsultarNFSeServicoTomado   := AINI.ReadString(ASession, 'ProConsultarNFSeServicoTomado'  , FRecepcionar);

    FTesteEnvio      := AINI.ReadString(ASession, 'ProTesteEnvio'     , FRecepcionar);
    FConsultarSeqRps := AINI.ReadString(ASession, 'ProConsultarSeqRps', FRecepcionar);
  end;
end;

{ TConfigMsgDados }

constructor TConfigMsgDados.Create;
begin
  FXmlRps := TDocElement.Create;
  FLoteRps := TDocElement.Create;
  FLoteRpsSincrono := TDocElement.Create;
  FConsultarSituacao := TDocElement.Create;
  FConsultarLote := TDocElement.Create;
  FConsultarNFSeRps := TDocElement.Create;
  FConsultarNFSe := TDocElement.Create;
  FConsultarNFSePorChave := TDocElement.Create;
  FConsultarNFSePorFaixa := TDocElement.Create;
  FConsultarNFSeServicoPrestado := TDocElement.Create;
  FConsultarNFSeServicoTomado := TDocElement.Create;
  FConsultarLinkNFSe := TDocElement.Create;
  FCancelarNFSe := TDocElement.Create;
  FGerarNFSe := TDocElement.Create;
  FSubstituirNFSe := TDocElement.Create;
  FAbrirSessao := TDocElement.Create;
  FFecharSessao := TDocElement.Create;
  FGerarToken := TDocElement.Create;
  FEnviarEvento := TDocElement.Create;
  FConsultarEvento := TDocElement.Create;
  FConsultarDFe := TDocElement.Create;
  FConsultarParam := TDocElement.Create;
  FConsultarSeqRps := TDocElement.Create;
end;

destructor TConfigMsgDados.Destroy;
begin
  FXmlRps.Free;
  FLoteRps.Free;
  FLoteRpsSincrono.Free;
  FConsultarSituacao.Free;
  FConsultarLote.Free;
  FConsultarNFSeRps.Free;
  FConsultarNFSe.Free;
  FConsultarNFSePorChave.Free;
  FConsultarNFSePorFaixa.Free;
  FConsultarNFSeServicoPrestado.Free;
  FConsultarNFSeServicoTomado.Free;
  FConsultarLinkNFSe.Free;
  FCancelarNFSe.Free;
  FGerarNFSe.Free;
  FSubstituirNFSe.Free;
  FAbrirSessao.Free;
  FFecharSessao.Free;
  FGerarToken.Free;
  FEnviarEvento.Free;
  FConsultarEvento.Free;
  FConsultarDFe.Free;
  FConsultarParam.Free;
  FConsultarSeqRps.Free;

  inherited Destroy;
end;

{ TConfigGeral }

constructor TConfigGeral.Create;
begin
  inherited Create;

  FParams := TACBrNFSeXConfigParams.Create;
  FAutenticacao := TACBrNFSeXAutenticacao.Create;
  FServicosDisponibilizados := TACBrNFSeXServicosDispobilizados.Create;
  FParticularidades := TACBrNFSeXParticularidades.Create;
end;

destructor TConfigGeral.Destroy;
begin
  FParams.Free;
  FAutenticacao.Free;
  FServicosDisponibilizados.Free;
  FParticularidades.Free;

  inherited Destroy;
end;

procedure TConfigGeral.LoadParams(AINI: TCustomIniFile; const ASession: string);
begin
  FParams.AsString := AINI.ReadString(ASession, 'Params', '');
end;

end.
