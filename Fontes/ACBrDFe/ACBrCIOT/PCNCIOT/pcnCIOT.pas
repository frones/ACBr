{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcnCIOT;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrCIOTConversao;

type

  TIntegradora = class(TObject)
  private
    FToken: string;
    FIntegrador: string;
    FSenha: string;
    FUsuario: string;

    FIntegradora: TCIOTIntegradora;
    FOperacao: TpOperacao;
  public
    property Token: string read FToken write FToken;
    property Integrador: string read FIntegrador write FIntegrador;
    property Senha: string read FSenha write FSenha;
    property Usuario: string read FUsuario write FUsuario;

    property Integradora: TCIOTIntegradora read FIntegradora write FIntegradora;
    property Operacao: TpOperacao read FOperacao write FOperacao;
  end;

  TTelefone = class(TObject)
  private
    FDDD: integer;
    FNumero: integer;
  public
    property DDD: integer read FDDD write FDDD;
    property Numero: integer read FNumero write FNumero;
  end;

  TTelefones = class(TObject)
  private
    FCelular: TTelefone;
    FFixo: TTelefone;
    FFax: TTelefone;
  public
    constructor Create;
    destructor Destroy; override;

    property Celular: TTelefone read FCelular write FCelular;
    property Fixo: TTelefone read FFixo write FFixo;
    property Fax: TTelefone read FFax write FFax;
  end;

  TEndereco = class(TObject)
  private
    FRua: String;
    FNumero: String;
    FComplemento: String;
    FBairro: String;
    FCodigoMunicipio: Integer;
    FxMunicipio: String;
    FCEP: string;

    procedure SetCEP(const Value: string);
  public
    property Rua: String read FRua    write FRua;
    property Numero: String read FNumero     write FNumero;
    property Complemento: String read FComplemento    write FComplemento;
    property Bairro: String read FBairro write FBairro;
    property CodigoMunicipio: Integer read FCodigoMunicipio    write FCodigoMunicipio;
    property xMunicipio: String read FxMunicipio    write FxMunicipio;
    property CEP: string read FCEP     write SetCEP;
  end;

  TContratante = class(TObject)
  private
    FNomeOuRazaoSocial: string;
    FCpfOuCnpj: string;
    FEndereco: TEndereco;
    FEMail: string;
    FTelefones: TTelefones;
    FResponsavelPeloPagamento: Boolean;
    FRNTRC: string;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property NomeOuRazaoSocial: string read FNomeOuRazaoSocial write FNomeOuRazaoSocial;
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property EMail: string read FEMail write FEMail;
    property Telefones: TTelefones read FTelefones write FTelefones;
    property ResponsavelPeloPagamento: Boolean read FResponsavelPeloPagamento write FResponsavelPeloPagamento;
    property RNTRC: string read FRNTRC write FRNTRC;
  end;

  TOptMotorista = class(TObject)
  private
    FCpfOuCnpj: string;
    FCNH: string;
    FCelular: TTelefone;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property CNH: string read FCNH write FCNH;
    property Celular: TTelefone read FCelular write FCelular;
  end;

  TDestinatario = class(TObject)
  private
    FNomeOuRazaoSocial: string;
    FCpfOuCnpj: string;
    FEndereco: TEndereco;
    FEMail: string;
    FTelefones: TTelefones;
    FResponsavelPeloPagamento: Boolean;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property NomeOuRazaoSocial: string read FNomeOuRazaoSocial write FNomeOuRazaoSocial;
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property EMail: string read FEMail write FEMail;
    property Telefones: TTelefones read FTelefones write FTelefones;
    property ResponsavelPeloPagamento: Boolean read FResponsavelPeloPagamento write FResponsavelPeloPagamento;
  end;

  TContratado = class(TObject)
  private
    FCpfOuCnpj: string;
    FRNTRC: string;

    procedure SetCpfOuCnpj(const Value: string);
  public
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property RNTRC: string read FRNTRC write FRNTRC;
  end;

  TConsignatario = class(TObject)
  private
    FNomeOuRazaoSocial: string;
    FCpfOuCnpj: string;
    FEndereco: TEndereco;
    FEMail: string;
    FTelefones: TTelefones;
    FResponsavelPeloPagamento: Boolean;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property NomeOuRazaoSocial: string read FNomeOuRazaoSocial write FNomeOuRazaoSocial;
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property EMail: string read FEMail write FEMail;
    property Telefones: TTelefones read FTelefones write FTelefones;
    property ResponsavelPeloPagamento: Boolean read FResponsavelPeloPagamento write FResponsavelPeloPagamento;
  end;

  TInformacoesBancarias = class(TObject)
  private
    FInstituicaoBancaria: string;
    FAgencia: string;
    FConta: string;
    FTipoConta: tpTipoConta;
  public
    property InstituicaoBancaria: string read FInstituicaoBancaria write FInstituicaoBancaria;
    property Agencia: string read FAgencia write FAgencia;
    property Conta: string read FConta write FConta;
    property TipoConta: tpTipoConta read FTipoConta write FTipoConta;
  end;

  TPagamentoCollectionItem = class(TObject)
  private
    FIdPagamentoCliente: string;
    FDataDeLiberacao: TDateTime;
    FValor: Double;
    FTipoPagamento: TpTipoPagamento;
    FCategoria: TpTipoCategoriaPagamento;
    FDocumento: string;
    FInformacoesBancarias: TInformacoesBancarias;
    FInformacaoAdicional: string;
    FCnpjFilialAbastecimento: string;
  public
    constructor Create;
    destructor Destroy; override;

    property IdPagamentoCliente: string read FIdPagamentoCliente write FIdPagamentoCliente;
    property DataDeLiberacao: TDateTime read FDataDeLiberacao write FDataDeLiberacao;
    property Valor: Double read FValor write FValor;
    property TipoPagamento: TpTipoPagamento read FTipoPagamento write FTipoPagamento;
    property Categoria: TpTipoCategoriaPagamento read FCategoria write FCategoria;
    property Documento: string read FDocumento write FDocumento;
    property InformacoesBancarias: TInformacoesBancarias read FInformacoesBancarias write FInformacoesBancarias;
    property InformacaoAdicional: string read FInformacaoAdicional write FInformacaoAdicional;
    property CnpjFilialAbastecimento: string read FCnpjFilialAbastecimento write FCnpjFilialAbastecimento;
  end;

  TPagamentoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TPagamentoCollectionItem;
    procedure SetItem(Index: Integer; Value: TPagamentoCollectionItem);
  public
    function Add: TPagamentoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TPagamentoCollectionItem;
    property Items[Index: Integer]: TPagamentoCollectionItem read GetItem write SetItem; default;
  end;

  TVeiculoCollectionItem = class(TObject)
  private
    FPlaca: String;

    procedure SetPlaca(const Value: String);
  public
    property Placa: String read FPlaca write SetPlaca;
  end;

  TVeiculoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TVeiculoCollectionItem;
    procedure SetItem(Index: Integer; Value: TVeiculoCollectionItem);
  public
    function Add: TVeiculoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TVeiculoCollectionItem;
    property Items[Index: Integer]: TVeiculoCollectionItem read GetItem write SetItem; default;
  end;

  TImpostos = class(TObject)
  private
    FIRRF: Double;
    FSestSenat: Double;
    FINSS: Double;
    FISSQN: Double;
    FOutrosImpostos: Double;
    FDescricaoOutrosImpostos: string;
  public
    property IRRF: Double read FIRRF write FIRRF;
    property SestSenat: Double read FSestSenat write FSestSenat;
    property INSS: Double read FINSS write FINSS;
    property ISSQN: Double read FISSQN write FISSQN;
    property OutrosImpostos: Double read FOutrosImpostos write FOutrosImpostos;
    property DescricaoOutrosImpostos: string read FDescricaoOutrosImpostos write FDescricaoOutrosImpostos;
  end;

  TValoresOT = class(TObject)
  private
    FTotalOperacao: Double;
    FTotalViagem: Double;
    FTotalDeAdiantamento: Double;
    FTotalDeQuitacao: Double;
    FCombustivel: Double;
    FPedagio: Double;
    FOutrosCreditos: Double;
    FJustificativaOutrosCreditos: string;
    FSeguro: Double;
    FOutrosDebitos: Double;
    FJustificativaOutrosDebitos: string;
  public
    property TotalOperacao: Double read FTotalOperacao write FTotalOperacao;
    property TotalViagem: Double read FTotalViagem write FTotalViagem;
    property TotalDeAdiantamento: Double read FTotalDeAdiantamento write FTotalDeAdiantamento;
    property TotalDeQuitacao: Double read FTotalDeQuitacao write FTotalDeQuitacao;
    property Combustivel: Double read FCombustivel write FCombustivel;
    property Pedagio: Double read FPedagio write FPedagio;
    property OutrosCreditos: Double read FOutrosCreditos write FOutrosCreditos;
    property JustificativaOutrosCreditos: string read FJustificativaOutrosCreditos write FJustificativaOutrosCreditos;
    property Seguro: Double read FSeguro write FSeguro;
    property OutrosDebitos: Double read FOutrosDebitos write FOutrosDebitos;
    property JustificativaOutrosDebitos: string read FJustificativaOutrosDebitos write FJustificativaOutrosDebitos;
  end;

  TToleranciaDePerdaDeMercadoria = class(TObject)
  private
    FTipo: TpTipoProporcao;
    FValor: Double;
  public
    property Tipo: TpTipoProporcao read FTipo write FTipo;
    property Valor: Double read FValor write FValor;
  end;

  TDiferencaFreteMargem = class(TObject)
  private
    FTipo: TpTipoProporcao;
    FValor: Double;
  public
    property Tipo: TpTipoProporcao read FTipo write FTipo;
    property Valor: Double read FValor write FValor;
  end;

   TDiferencaDeFrete = class(TObject)
  private
    FTipo: TpDiferencaFrete;
    FBase: TpDiferencaFreteBaseCalculo;
    FTolerancia: TDiferencaFreteMargem;
    FMargemGanho: TDiferencaFreteMargem;
    FMargemPerda: TDiferencaFreteMargem;
  public
    constructor Create;
    destructor Destroy; override;

    property Tipo: TpDiferencaFrete read FTipo write FTipo;
    property Base: TpDiferencaFreteBaseCalculo read FBase write FBase;
    property Tolerancia: TDiferencaFreteMargem read FTolerancia write FTolerancia;
    property MargemGanho: TDiferencaFreteMargem read FMargemGanho write FMargemGanho;
    property MargemPerda: TDiferencaFreteMargem read FMargemPerda write FMargemPerda;
  end;

 TNotaFiscalCollectionItem = class(TObject)
  private
    FNumero: string;
    FSerie: string;
    FData: TDateTime;
    FValorTotal: Double;
    FValorDaMercadoriaPorUnidade: Double;
    FCodigoNCMNaturezaCarga: integer;
    FDescricaoDaMercadoria: string;
    FUnidadeDeMedidaDaMercadoria: TpUnidadeDeMedidaDaMercadoria;
    FTipoDeCalculo: TpViagemTipoDeCalculo;
    FValorDoFretePorUnidadeDeMercadoria: Double;
    FQuantidadeDaMercadoriaNoEmbarque: double;
    FQuantidadeDaMercadoriaNoDesembarque: double;
    FToleranciaDePerdaDeMercadoria: TToleranciaDePerdaDeMercadoria;
    FDiferencaDeFrete: TDiferencaDeFrete;
  public
    constructor Create;
    destructor Destroy; override;

    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Data: TDateTime read FData write FData;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property ValorDaMercadoriaPorUnidade: Double read FValorDaMercadoriaPorUnidade write FValorDaMercadoriaPorUnidade;
    property CodigoNCMNaturezaCarga: integer read FCodigoNCMNaturezaCarga write FCodigoNCMNaturezaCarga;
    property DescricaoDaMercadoria: string read FDescricaoDaMercadoria write FDescricaoDaMercadoria;
    property UnidadeDeMedidaDaMercadoria: TpUnidadeDeMedidaDaMercadoria read FUnidadeDeMedidaDaMercadoria write FUnidadeDeMedidaDaMercadoria;
    property TipoDeCalculo: TpViagemTipoDeCalculo read FTipoDeCalculo write FTipoDeCalculo;
    property ValorDoFretePorUnidadeDeMercadoria: Double read FValorDoFretePorUnidadeDeMercadoria write FValorDoFretePorUnidadeDeMercadoria;
    property QuantidadeDaMercadoriaNoEmbarque: double read FQuantidadeDaMercadoriaNoEmbarque write FQuantidadeDaMercadoriaNoEmbarque;
    property QuantidadeDaMercadoriaNoDesembarque: double read FQuantidadeDaMercadoriaNoDesembarque write FQuantidadeDaMercadoriaNoDesembarque;
    property ToleranciaDePerdaDeMercadoria: TToleranciaDePerdaDeMercadoria read FToleranciaDePerdaDeMercadoria write FToleranciaDePerdaDeMercadoria;
    property DiferencaDeFrete: TDiferencaDeFrete read FDiferencaDeFrete write FDiferencaDeFrete;
  end;

  TNotaFiscalCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNotaFiscalCollectionItem;
    procedure SetItem(Index: Integer; Value: TNotaFiscalCollectionItem);
  public
    function Add: TNotaFiscalCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TNotaFiscalCollectionItem;
    property Items[Index: Integer]: TNotaFiscalCollectionItem read GetItem write SetItem; default;
  end;

  TViagemCollectionItem = class(TObject)
  private
    FDocumentoViagem: string;
    FCodigoMunicipioOrigem: integer;
    FCodigoMunicipioDestino: integer;
    FCepOrigem: string;
    FCepDestino: string;
    FDistanciaPercorrida: Integer;
    FValores: TValoresOT;
    FTipoPagamento: TpTipoPagamento;
    FInformacoesBancarias: TInformacoesBancarias;
    FNotasFiscais: TNotaFiscalCollection;

    procedure SetNotasFiscais(const Value: TNotaFiscalCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property DocumentoViagem: string read FDocumentoViagem write FDocumentoViagem;
    property CodigoMunicipioOrigem: integer read FCodigoMunicipioOrigem write FCodigoMunicipioOrigem;
    property CodigoMunicipioDestino: integer read FCodigoMunicipioDestino write FCodigoMunicipioDestino;
    property CepOrigem: string read FCepOrigem write FCepOrigem;
    property CepDestino: string read FCepDestino write FCepDestino;
    property DistanciaPercorrida: Integer read FDistanciaPercorrida write FDistanciaPercorrida;
    property Valores: TValoresOT read FValores write FValores;
    property TipoPagamento: TpTipoPagamento read FTipoPagamento write FTipoPagamento;
    property InformacoesBancarias: TInformacoesBancarias read FInformacoesBancarias write FInformacoesBancarias;
    property NotasFiscais: TNotaFiscalCollection read FNotasFiscais write SetNotasFiscais;
  end;

  TViagemCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TViagemCollectionItem;
    procedure SetItem(Index: Integer; Value: TViagemCollectionItem);
  public
    function Add: TViagemCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TViagemCollectionItem;
    property Items[Index: Integer]: TViagemCollectionItem read GetItem write SetItem; default;
  end;

  TSubcontratante = class(TObject)
  private
    FNomeOuRazaoSocial: string;
    FCpfOuCnpj: string;
    FEndereco: TEndereco;
    FEMail: string;
    FTelefones: TTelefones;
    FResponsavelPeloPagamento: Boolean;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property NomeOuRazaoSocial: string read FNomeOuRazaoSocial write FNomeOuRazaoSocial;
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property EMail: string read FEMail write FEMail;
    property Telefones: TTelefones read FTelefones write FTelefones;
    property ResponsavelPeloPagamento: Boolean read FResponsavelPeloPagamento write FResponsavelPeloPagamento;
  end;

  TTomadorServico = class(TObject)
  private
    FNomeOuRazaoSocial: string;
    FCpfOuCnpj: string;
    FEndereco: TEndereco;
    FEMail: string;
    FTelefones: TTelefones;
    FResponsavelPeloPagamento: Boolean;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property NomeOuRazaoSocial: string read FNomeOuRazaoSocial write FNomeOuRazaoSocial;
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property EMail: string read FEMail write FEMail;
    property Telefones: TTelefones read FTelefones write FTelefones;
    property ResponsavelPeloPagamento: Boolean read FResponsavelPeloPagamento write FResponsavelPeloPagamento;
  end;

  TRemetente = class(TObject)
  private
    FNomeOuRazaoSocial: string;
    FCpfOuCnpj: string;
    FEndereco: TEndereco;
    FEMail: string;
    FTelefones: TTelefones;
    FResponsavelPeloPagamento: Boolean;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property NomeOuRazaoSocial: string read FNomeOuRazaoSocial write FNomeOuRazaoSocial;
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property EMail: string read FEMail write FEMail;
    property Telefones: TTelefones read FTelefones write FTelefones;
    property ResponsavelPeloPagamento: Boolean read FResponsavelPeloPagamento write FResponsavelPeloPagamento;
  end;

  TProprietarioCarga = class(TObject)
  private
    FNomeOuRazaoSocial: string;
    FCpfOuCnpj: string;
    FEndereco: TEndereco;
    FEMail: string;
    FTelefones: TTelefones;
    FResponsavelPeloPagamento: Boolean;

    procedure SetCpfOuCnpj(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property NomeOuRazaoSocial: string read FNomeOuRazaoSocial write FNomeOuRazaoSocial;
    property CpfOuCnpj: string read FCpfOuCnpj write SetCpfOuCnpj;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property EMail: string read FEMail write FEMail;
    property Telefones: TTelefones read FTelefones write FTelefones;
    property ResponsavelPeloPagamento: Boolean read FResponsavelPeloPagamento write FResponsavelPeloPagamento;
  end;

  TCancelamento = class(TObject)
  private
    FMotivo: string;
    FProtocolo: string;
    FData: TDateTime;
    FIdPagamentoCliente: string;
  public
    property Motivo: string read FMotivo write FMotivo;
    property Data: TDateTime read FData write FData;
    property Protocolo: string read FProtocolo write FProtocolo;
    property IdPagamentoCliente: string read FIdPagamentoCliente write FIdPagamentoCliente;
  end;

  TMensagemCollectionItem = class(TObject)
  private
    FMensagem: string;
  public
    property Mensagem: string read FMensagem write FMensagem;
  end;

  TMensagemCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TMensagemCollectionItem;
    procedure SetItem(Index: Integer; Value: TMensagemCollectionItem);
  public
    function Add: TMensagemCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TMensagemCollectionItem;
    property Items[Index: Integer]: TMensagemCollectionItem read GetItem write SetItem; default;
  end;

  TAdicionarOperacao = class(TObject)
  private
    FTipoViagem: TpTipoViagem;
    FTipoPagamento: TpTipoPagamento;
    FBloquearNaoEquiparado: Boolean;
    FMatrizCNPJ: string;
    FFilialCNPJ: string;
    FIdOperacaoCliente: string;
    FDataInicioViagem: TDateTime;
    FDataFimViagem: TDateTime;
    FCodigoNCMNaturezaCarga: integer;
    FPesoCarga: double;
    FTipoEmbalagem: tpTipoEmbalagem;
    FViagens: TViagemCollection;
    FImpostos: TImpostos;
    FPagamentos: TPagamentoCollection;
    FContratado: TContratado;
    FMotorista: TOptMotorista;
    FDestinatario: TDestinatario;
    FContratante: TContratante;
    FSubcontratante: TSubcontratante;
    FConsignatario: TConsignatario;
    FTomadorServico: TTomadorServico;
    FRemetente: TRemetente;
    FProprietarioCarga: TProprietarioCarga;
    FVeiculos: TVeiculoCollection;
    FCodigoIdentificacaoOperacaoPrincipal: string;
    FObservacoesAoTransportador: TMensagemCollection;
    FObservacoesAoCredenciado: TMensagemCollection;
    FEntregaDocumentacao: TpEntregaDocumentacao;
    FQuantidadeSaques: Integer;
    FQuantidadeTransferencias: Integer;
    FValorSaques: Double;
    FValorTransferencias: Double;

    FAltoDesempenho: Boolean;
    FDestinacaoComercial: Boolean;
    FFreteRetorno: Boolean;
    FCepRetorno: String;
    FDistanciaRetorno: Integer;

    FIntegrador: string;
    FEmissaoGratuita: Boolean;
    FCodigoTipoCarga: tpTipoCarga;

    procedure SetFilialCNPJ(const Value: string);
    procedure SetMatrizCNPJ(const Value: string);
    procedure SetViagens(const Value: TViagemCollection);
    procedure SetPagamentos(const Value: TPagamentoCollection);
    procedure SetVeiculos(const Value: TVeiculoCollection);
    procedure SetObservacoesAoCredenciado(const Value: TMensagemCollection);
    procedure SetObservacoesAoTransportador(const Value: TMensagemCollection);
    procedure SetEmissaoGratuita(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property TipoViagem: TpTipoViagem read FTipoViagem write FTipoViagem;
    property TipoPagamento: TpTipoPagamento read FTipoPagamento write FTipoPagamento;
    property EmissaoGratuita: Boolean read FEmissaoGratuita write SetEmissaoGratuita;
    property BloquearNaoEquiparado: Boolean read FBloquearNaoEquiparado write FBloquearNaoEquiparado;
    property MatrizCNPJ: string read FMatrizCNPJ write SetMatrizCNPJ;
    property FilialCNPJ: string read FFilialCNPJ write SetFilialCNPJ;
    property IdOperacaoCliente: string read FIdOperacaoCliente write FIdOperacaoCliente;
    property DataInicioViagem: TDateTime read FDataInicioViagem write FDataInicioViagem;
    property DataFimViagem: TDateTime read FDataFimViagem write FDataFimViagem;
    property CodigoNCMNaturezaCarga: integer read FCodigoNCMNaturezaCarga write FCodigoNCMNaturezaCarga;
    property PesoCarga: double read FPesoCarga write FPesoCarga;
    property TipoEmbalagem: tpTipoEmbalagem read FTipoEmbalagem write FTipoEmbalagem;
    property Viagens: TViagemCollection read FViagens write SetViagens;
    property Impostos: TImpostos read FImpostos write FImpostos;
    property Pagamentos: TPagamentoCollection read FPagamentos write SetPagamentos;
    property Contratado: TContratado read FContratado write FContratado;
    property Motorista: TOptMotorista read FMotorista write FMotorista;
    property Destinatario: TDestinatario read FDestinatario write FDestinatario;
    property Contratante: TContratante read FContratante write FContratante;
    property Subcontratante: TSubcontratante read FSubcontratante write FSubcontratante;
    property Consignatario: TConsignatario read FConsignatario write FConsignatario;
    property TomadorServico: TTomadorServico read FTomadorServico write FTomadorServico;
    property Remetente: TRemetente read FRemetente write FRemetente;
    property ProprietarioCarga: TProprietarioCarga read FProprietarioCarga write FProprietarioCarga;
    property Veiculos: TVeiculoCollection read FVeiculos write SetVeiculos;
    property CodigoIdentificacaoOperacaoPrincipal: string read FCodigoIdentificacaoOperacaoPrincipal write FCodigoIdentificacaoOperacaoPrincipal;
    property ObservacoesAoTransportador: TMensagemCollection read FObservacoesAoTransportador write SetObservacoesAoTransportador;
    property ObservacoesAoCredenciado: TMensagemCollection read FObservacoesAoCredenciado write SetObservacoesAoCredenciado;
    property EntregaDocumentacao: TpEntregaDocumentacao read FEntregaDocumentacao write FEntregaDocumentacao;
    property QuantidadeSaques: Integer read FQuantidadeSaques write FQuantidadeSaques;
    property QuantidadeTransferencias: Integer read FQuantidadeTransferencias write FQuantidadeTransferencias;
    property ValorSaques: Double read FValorSaques write FValorSaques;
    property ValorTransferencias: Double read FValorTransferencias write FValorTransferencias;
    property CodigoTipoCarga: tpTipoCarga read FCodigoTipoCarga write FCodigoTipoCarga;
    property AltoDesempenho: Boolean read FAltoDesempenho write FAltoDesempenho;
    property DestinacaoComercial: Boolean read FDestinacaoComercial write FDestinacaoComercial;
    property FreteRetorno: Boolean read FFreteRetorno write FFreteRetorno;
    property CepRetorno: String read FCepRetorno write FCepRetorno;
    property DistanciaRetorno: Integer read FDistanciaRetorno write FDistanciaRetorno;

    property Integrador: string read FIntegrador write FIntegrador;
  end;

  TObterOperacaoTransportePDF = class(TObject)
  private
    FCodigoIdentificacaoOperacao: string;
    FDocumentoViagem: string;
  public
    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property DocumentoViagem: string read FDocumentoViagem write FDocumentoViagem;
  end;

  TRetificarOperacao = class(TObject)
  private
    FCodigoIdentificacaoOperacao: string;
    FDataInicioViagem: TDateTime;
    FDataFimViagem: TDateTime;
    FCodigoNCMNaturezaCarga: integer;
    FPesoCarga: double;
    FCodigoMunicipioOrigem: integer;
    FCodigoMunicipioDestino: integer;
    FVeiculos: TVeiculoCollection;
    FQuantidadeSaques: Integer;
    FQuantidadeTransferencias: Integer;
    FValorSaques: Double;
    FValorTransferencias: Double;
    FCodigoTipoCarga: tpTipoCarga;
    FCepOrigem: string;
    FCepDestino: string;
    FDistanciaPercorrida: Integer;

    procedure SetVeiculos(const Value: TVeiculoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property DataInicioViagem: TDateTime read FDataInicioViagem write FDataInicioViagem;
    property DataFimViagem: TDateTime read FDataFimViagem write FDataFimViagem;
    property CodigoNCMNaturezaCarga: integer read FCodigoNCMNaturezaCarga write FCodigoNCMNaturezaCarga;
    property PesoCarga: double read FPesoCarga write FPesoCarga;
    property CodigoMunicipioOrigem: integer read FCodigoMunicipioOrigem write FCodigoMunicipioOrigem;
    property CodigoMunicipioDestino: integer read FCodigoMunicipioDestino write FCodigoMunicipioDestino;
    property Veiculos: TVeiculoCollection read FVeiculos write SetVeiculos;
    property QuantidadeSaques: Integer read FQuantidadeSaques write FQuantidadeSaques;
    property QuantidadeTransferencias: Integer read FQuantidadeTransferencias write FQuantidadeTransferencias;
    property ValorSaques: Double read FValorSaques write FValorSaques;
    property ValorTransferencias: Double read FValorTransferencias write FValorTransferencias;
    property CodigoTipoCarga: tpTipoCarga read FCodigoTipoCarga write FCodigoTipoCarga;
    property CepOrigem: string read FCepOrigem write FCepOrigem;
    property CepDestino: string read FCepDestino write FCepDestino;
    property DistanciaPercorrida: Integer read FDistanciaPercorrida write FDistanciaPercorrida;
  end;

  TCancelarOperacao = class(TObject)
  private
    FCodigoIdentificacaoOperacao: string;
    FMotivo: string;
  public
    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property Motivo: string read FMotivo write FMotivo;
  end;

  TAdicionarViagem = class(TObject)
  private
    FCodigoIdentificacaoOperacao: string;
    FViagens: TViagemCollection;
    FPagamentos: TPagamentoCollection;
    FNaoAdicionarParcialmente: Boolean;

    procedure SetViagens(const Value: TViagemCollection);
    procedure SetPagamentos(const Value: TPagamentoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property Viagens: TViagemCollection read FViagens write SetViagens;
    property Pagamentos: TPagamentoCollection read FPagamentos write SetPagamentos;
    property NaoAdicionarParcialmente: Boolean read FNaoAdicionarParcialmente write FNaoAdicionarParcialmente;
  end;

  TAdicionarPagamento = class(TObject)
  private
    FCodigoIdentificacaoOperacao: string;
    FPagamentos: TPagamentoCollection;

    procedure SetPagamentos(const Value: TPagamentoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property Pagamentos: TPagamentoCollection read FPagamentos write SetPagamentos;
  end;

  TAlterarDataLiberacaoPagamento = class(TObject)
  private
    FIdPagamentoCliente: string;
    FMotivo: string;
    FCodigoIdentificacaoOperacao: string;
    FDataDeLiberacao: TDateTime;
  public
    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property IdPagamentoCliente: string read FIdPagamentoCliente write FIdPagamentoCliente;
    property DataDeLiberacao: TDateTime read FDataDeLiberacao write FDataDeLiberacao;
    property Motivo: string read FMotivo write FMotivo;
  end;

  TCancelarPagamento = class(TObject)
  private
    FCodigoIdentificacaoOperacao: string;
    FIdPagamentoCliente: string;
    FMotivo: string;
  public
    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property IdPagamentoCliente: string read FIdPagamentoCliente write FIdPagamentoCliente;
    property Motivo: string read FMotivo write FMotivo;
  end;

  TEncerrarOperacao = class(TObject)
  private
    FCodigoIdentificacaoOperacao: string;
    FPesoCarga: double;
    FViagens: TViagemCollection;
    FPagamentos: TPagamentoCollection;
    FImpostos: TImpostos;
    FQuantidadeSaques: Integer;
    FQuantidadeTransferencias: Integer;
    FValorSaques: Double;
    FValorTransferencias: Double;

    procedure SetViagens(const Value: TViagemCollection);
    procedure SetPagamentos(const Value: TPagamentoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property PesoCarga: double read FPesoCarga write FPesoCarga;
    property Viagens: TViagemCollection read FViagens write SetViagens;
    property Pagamentos: TPagamentoCollection read FPagamentos write SetPagamentos;
    property Impostos: TImpostos read FImpostos write FImpostos;
    property QuantidadeSaques: Integer read FQuantidadeSaques write FQuantidadeSaques;
    property QuantidadeTransferencias: Integer read FQuantidadeTransferencias write FQuantidadeTransferencias;
    property ValorSaques: Double read FValorSaques write FValorSaques;
    property ValorTransferencias: Double read FValorTransferencias write FValorTransferencias;
  end;

  TGravarVeiculo = class(TObject)
  private
    FPlaca: string;
    FRenavam: string;
    FChassi: string;
    FRNTRC: string;
    FNumeroDeEixos: Integer;
    FCodigoMunicipio: Integer;
    FMarca: string;
    FModelo: string;
    FAnoFabricacao: Integer;
    FAnoModelo: Integer;
    FCor: string;
    FTara: Integer;
    FCapacidadeKg: Integer;
    FCapacidadeM3: Integer;
    FTipoRodado: TpTipoRodado;
    FTipoCarroceria: TpTipoCarroceria;
  public
    property Placa: string read FPlaca write FPlaca;
    property Renavam: string read FRenavam write FRenavam;
    property Chassi: string read FChassi write FChassi;
    property RNTRC: string read FRNTRC write FRNTRC;
    property NumeroDeEixos: Integer read FNumeroDeEixos write FNumeroDeEixos;
    property CodigoMunicipio: Integer read FCodigoMunicipio write FCodigoMunicipio;
    property Marca: string read FMarca write FMarca;
    property Modelo: string read FModelo write FModelo;
    property AnoFabricacao: Integer read FAnoFabricacao write FAnoFabricacao;
    property AnoModelo: Integer read FAnoModelo write FAnoModelo;
    property Cor: string read FCor write FCor;
    property Tara: Integer read FTara write FTara;
    property CapacidadeKg: Integer read FCapacidadeKg write FCapacidadeKg;
    property CapacidadeM3: Integer read FCapacidadeM3 write FCapacidadeM3;
    property TipoRodado: TpTipoRodado read FTipoRodado write FTipoRodado;
    property TipoCarroceria: TpTipoCarroceria read FTipoCarroceria write FTipoCarroceria;
  end;

  TGravarMotorista = class(TObject)
  private
    FCPF: string;
    FNome: string;
    FCNH: string;
    FDataNascimento: TDateTime;
    FNomeDeSolteiraDaMae: string;
    FEndereco: TEndereco;
    FTelefones: TTelefones;

    procedure setCPF(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property CPF: string read FCPF write setCPF;
    property Nome: string read FNome write FNome;
    property CNH: string read FCNH write FCNH;
    property DataNascimento: TDateTime read FDataNascimento write FDataNascimento;
    property NomeDeSolteiraDaMae: string read FNomeDeSolteiraDaMae write FNomeDeSolteiraDaMae;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Telefones: TTelefones read FTelefones write FTelefones;
  end;

  TGravarProprietario = class(TObject)
  private
    FCNPJ: string;
    FTipoPessoa: tpTipoPessoa;
    FRazaoSocial: string;
    FRNTRC: string;
    FEndereco: TEndereco;
    FTelefones: TTelefones;
    // Os campos abaixos são obtidos pelo retorno do envio.
    FTipo: TpTipoProprietario;
    FTACouEquiparado: Boolean;
    FDataValidadeRNTRC: TDateTime;
    FRNTRCAtivo: Boolean;

    procedure setCNPJ(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property CNPJ: string read FCNPJ write setCNPJ;
    property TipoPessoa: tpTipoPessoa read FTipoPessoa write FTipoPessoa;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property RNTRC: string read FRNTRC write FRNTRC;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Telefones: TTelefones read FTelefones write FTelefones;
    // Os campos abaixos são obtidos pelo retorno do envio.
    property Tipo: TpTipoProprietario read FTipo write FTipo;
    property TACouEquiparado: Boolean read FTACouEquiparado write FTACouEquiparado;
    property DataValidadeRNTRC: TDateTime read FDataValidadeRNTRC write FDataValidadeRNTRC;
    property RNTRCAtivo: Boolean read FRNTRCAtivo write FRNTRCAtivo;
  end;

  TObterCodigoOperacaoTransporte = class(TObject)
  private
    FMatrizCNPJ: string;
    FIdOperacaoCliente: string;
  public
    property MatrizCNPJ: string read FMatrizCNPJ write FMatrizCNPJ;
    property IdOperacaoCliente: string read FIdOperacaoCliente write FIdOperacaoCliente;
  end;

  TCIOT = class(TObject)
  private
    FIntegradora: TIntegradora;

    FGravarProprietario: TGravarProprietario;
    FGravarVeiculo: TGravarVeiculo;
    FGravarMotorista: TGravarMotorista;

    FAdicionarOperacao: TAdicionarOperacao;
    FObterOperacaoTransportePDF: TObterOperacaoTransportePDF;
    FRetificarOperacao: TRetificarOperacao;
    FCancelarOperacao: TCancelarOperacao;
    FAdicionarViagem: TAdicionarViagem;
    FAdicionarPagamento: TAdicionarPagamento;
    FCancelarPagamento: TCancelarPagamento;
    FEncerrarOperacao: TEncerrarOperacao;
    FObterCodigoOperacaoTransporte: TObterCodigoOperacaoTransporte;
    FAlterarDataLiberacaoPagamento: TAlterarDataLiberacaoPagamento;
  public
    constructor Create;
    destructor Destroy; override;

    property Integradora: TIntegradora read FIntegradora write FIntegradora;

    property GravarProprietario: TGravarProprietario read FGravarProprietario write FGravarProprietario;
    property GravarVeiculo: TGravarVeiculo read FGravarVeiculo write FGravarVeiculo;
    property GravarMotorista: TGravarMotorista read FGravarMotorista write FGravarMotorista;

    property AdicionarOperacao: TAdicionarOperacao read FAdicionarOperacao write FAdicionarOperacao;
    property ObterOperacaoTransportePDF: TObterOperacaoTransportePDF read FObterOperacaoTransportePDF write FObterOperacaoTransportePDF;
    property RetificarOperacao: TRetificarOperacao read FRetificarOperacao write FRetificarOperacao;
    property CancelarOperacao: TCancelarOperacao read FCancelarOperacao write FCancelarOperacao;
    property AdicionarViagem: TAdicionarViagem read FAdicionarViagem write FAdicionarViagem;
    property AdicionarPagamento: TAdicionarPagamento read FAdicionarPagamento write FAdicionarPagamento;
    property CancelarPagamento: TCancelarPagamento read FCancelarPagamento write FCancelarPagamento;
    property EncerrarOperacao: TEncerrarOperacao read FEncerrarOperacao write FEncerrarOperacao;
    property ObterCodigoOperacaoTransporte: TObterCodigoOperacaoTransporte read FObterCodigoOperacaoTransporte write FObterCodigoOperacaoTransporte;
    property AlterarDataLiberacaoPagamento: TAlterarDataLiberacaoPagamento read FAlterarDataLiberacaoPagamento write FAlterarDataLiberacaoPagamento;
  end;

  TConsultaTipoCargaCollectionItem = class(TObject)
  private
    FCodigo: Integer;
    FDescricao: tpTipoCarga;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: tpTipoCarga read FDescricao write FDescricao;
  end;

  TConsultaTipoCargaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TConsultaTipoCargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TConsultaTipoCargaCollectionItem);
  public
    function Add: TConsultaTipoCargaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TConsultaTipoCargaCollectionItem;
    property Items[Index: Integer]: TConsultaTipoCargaCollectionItem read GetItem write SetItem; default;
  end;

// ************* Retorno

  TRetEnvio = class(TObject)
  private
    FMensagem: String;
    FCodigo: String;

    FVersao: Integer;
    FSucesso: String;
    FProtocoloServico: String;

    FToken: String;

    FProprietario: TGravarProprietario;
    FVeiculo: TGravarVeiculo;
    FMotorista: TGravarMotorista;

    FPDF: AnsiString;
    FPDFNomeArquivo :string;
    FCodigoIdentificacaoOperacao: string;
    FDataRetificacao: TDateTime;
    FData: TDateTime;
    FProtocolo: String;
    FQuantidadeViagens: Integer;
    FQuantidadePagamentos: Integer;
    FDocumentoViagem: TMensagemCollection;
    FDocumentoPagamento: TMensagemCollection;
    FIdPagamentoCliente: string;
    FEstadoCiot: tpEstadoCIOT;
    FTipoCarga: TConsultaTipoCargaCollection;
    FAlterarDataLiberacaoPagamento: TAlterarDataLiberacaoPagamento;

    procedure SetDocumentoViagem(const Value: TMensagemCollection);
    procedure SetDocumentoPagamento(const Value: TMensagemCollection);
    procedure SetTipoCarga(const Value: TConsultaTipoCargaCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property Mensagem: String read FMensagem write FMensagem;
    property Codigo: String read FCodigo write FCodigo;

    property Versao: Integer read FVersao write FVersao;
    property Sucesso: String read FSucesso write FSucesso;
    property ProtocoloServico: String read FProtocoloServico write FProtocoloServico;

    property Token: String read FToken write FToken;

    property Proprietario: TGravarProprietario read FProprietario write FProprietario;
    property Veiculo: TGravarVeiculo read FVeiculo write FVeiculo;
    property Motorista: TGravarMotorista read FMotorista write FMotorista;

    property PDF: AnsiString read FPDF write FPDF;
    property PDFNomeArquivo : String read FPDFNomeArquivo write FPDFNomeArquivo;
    property CodigoIdentificacaoOperacao: string read FCodigoIdentificacaoOperacao write FCodigoIdentificacaoOperacao;
    property DataRetificacao: TDateTime read FDataRetificacao write FDataRetificacao;
    property Data: TDateTime read FData write FData;
    property Protocolo: String read FProtocolo write FProtocolo;
    property QuantidadeViagens: Integer read FQuantidadeViagens write FQuantidadeViagens;
    property QuantidadePagamentos: Integer read FQuantidadePagamentos write FQuantidadePagamentos;
    property DocumentoViagem: TMensagemCollection read FDocumentoViagem write SetDocumentoViagem;
    property DocumentoPagamento: TMensagemCollection read FDocumentoPagamento write SetDocumentoPagamento;
    property IdPagamentoCliente: string read FIdPagamentoCliente write FIdPagamentoCliente;
    property EstadoCiot: tpEstadoCIOT read FEstadoCiot write FEstadoCiot;
    property TipoCarga: TConsultaTipoCargaCollection read FTipoCarga write SetTipoCarga;
    property AlterarDataLiberacaoPagamento: TAlterarDataLiberacaoPagamento read FAlterarDataLiberacaoPagamento write FAlterarDataLiberacaoPagamento;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TCIOT }

constructor TCIOT.Create;
begin
  inherited Create;

  FIntegradora := TIntegradora.Create;

  FGravarProprietario := TGravarProprietario.Create;
  FGravarVeiculo      := TGravarVeiculo.Create;
  FGravarMotorista    := TGravarMotorista.Create;

  FAdicionarOperacao             := TAdicionarOperacao.Create;
  FObterCodigoOperacaoTransporte := TObterCodigoOperacaoTransporte.Create;
  FObterOperacaoTransportePDF    := TObterOperacaoTransportePDF.Create;
  FRetificarOperacao             := TRetificarOperacao.Create;
  FCancelarOperacao              := TCancelarOperacao.Create;
  FAdicionarViagem               := TAdicionarViagem.Create;
  FAdicionarPagamento            := TAdicionarPagamento.Create;
  FCancelarPagamento             := TCancelarPagamento.Create;
  FEncerrarOperacao              := TEncerrarOperacao.Create;
  FAlterarDataLiberacaoPagamento := TAlterarDataLiberacaoPagamento.Create;
end;

destructor TCIOT.Destroy;
begin
  FIntegradora.Free;

  FGravarProprietario.Free;
  FGravarVeiculo.Free;
  FGravarMotorista.Free;

  FAdicionarOperacao.Free;
  FObterCodigoOperacaoTransporte.Free;
  FObterOperacaoTransportePDF.Free;
  FRetificarOperacao.Free;
  FCancelarOperacao.Free;
  FAdicionarViagem.Free;
  FAdicionarPagamento.Free;
  FCancelarPagamento.Free;
  FEncerrarOperacao.Free;
  FAlterarDataLiberacaoPagamento.Free;

  inherited Destroy;
end;

{ TAdicionarOperacao }

constructor TAdicionarOperacao.Create;
begin
  inherited Create;

  FViagens := TViagemCollection.Create;
  FImpostos := TImpostos.Create;
  FPagamentos := TPagamentoCollection.Create;
  FContratado := TContratado.Create;
  FMotorista := TOptMotorista.Create;
  FDestinatario := TDestinatario.Create;
  FContratante := TContratante.Create;
  FSubcontratante := TSubcontratante.Create;
  FConsignatario := TConsignatario.Create;
  FTomadorServico := TTomadorServico.Create;
  FRemetente := TRemetente.Create;
  FProprietarioCarga := TProprietarioCarga.Create;
  FVeiculos := TVeiculoCollection.Create;
  FObservacoesAoCredenciado := TMensagemCollection.Create;
  FObservacoesAoTransportador := TMensagemCollection.Create;
end;

destructor TAdicionarOperacao.Destroy;
begin
  FViagens.Free;
  FImpostos.Free;
  FPagamentos.Free;
  FContratado.Free;
  FMotorista.Free;
  FDestinatario.Free;
  FContratante.Free;
  FSubcontratante.Free;
  FConsignatario.Free;
  FTomadorServico.Free;
  FRemetente.Free;
  FProprietarioCarga.Free;
  FVeiculos.Free;
  FObservacoesAoCredenciado.Free;
  FObservacoesAoTransportador.Free;

  inherited Destroy;
end;

procedure TAdicionarOperacao.SetEmissaoGratuita(const Value: Boolean);
begin
  FEmissaoGratuita := Value;
end;

procedure TAdicionarOperacao.SetFilialCNPJ(const Value: string);
begin
  FFilialCNPJ := {SomenteNumeros(}Value{)};
end;

procedure TAdicionarOperacao.SetMatrizCNPJ(const Value: string);
begin
  FMatrizCNPJ := {SomenteNumeros(}Value{)};
end;

procedure TAdicionarOperacao.SetObservacoesAoCredenciado(
  const Value: TMensagemCollection);
begin
  FObservacoesAoCredenciado := Value;
end;

procedure TAdicionarOperacao.SetObservacoesAoTransportador(
  const Value: TMensagemCollection);
begin
  FObservacoesAoTransportador := Value;
end;

procedure TAdicionarOperacao.SetPagamentos(const Value: TPagamentoCollection);
begin
  FPagamentos := Value;
end;

procedure TAdicionarOperacao.SetVeiculos(const Value: TVeiculoCollection);
begin
  FVeiculos := Value;
end;

procedure TAdicionarOperacao.SetViagens(const Value: TViagemCollection);
begin
  FViagens := Value;
end;

{ TContratante }

constructor TContratante.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
  FTelefones := TTelefones.Create;
end;

destructor TContratante.Destroy;
begin
  FEndereco.Free;
  FTelefones.Free;

  inherited Destroy;
end;

procedure TContratante.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {omenteNumeros(}Value{)};
end;

{ TTelefones }

constructor TTelefones.Create;
begin
  inherited Create;

  FCelular := TTelefone.Create;
  FFixo := TTelefone.Create;
  FFax := TTelefone.Create;
end;

destructor TTelefones.Destroy;
begin
  FCelular.Free;
  FFixo.Free;
  FFax.Free;

  inherited Destroy;
end;

{ TEndereco }

procedure TEndereco.SetCEP(const Value: string);
begin
  FCEP := {SomenteNumeros(}Value{)};
end;

{ TOptMotorista }

constructor TOptMotorista.Create;
begin
  inherited Create;

  FCelular := TTelefone.Create;
end;

destructor TOptMotorista.Destroy;
begin
  FCelular.Free;

  inherited Destroy;
end;

procedure TOptMotorista.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {omenteNumeros(}Value{)};
end;

{ TDestinatario }

constructor TDestinatario.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
  FTelefones := TTelefones.Create;
end;

destructor TDestinatario.Destroy;
begin
  FEndereco.Free;
  FTelefones.Free;

  inherited Destroy;
end;

procedure TDestinatario.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {omenteNumeros(}Value{)};
end;

{ TContratado }

procedure TContratado.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {omenteNumeros(}Value{)};
end;

{ TConsignatario }

constructor TConsignatario.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
  FTelefones := TTelefones.Create;
end;

destructor TConsignatario.Destroy;
begin
  FEndereco.Free;
  FTelefones.Free;

  inherited Destroy;
end;

procedure TConsignatario.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {omenteNumeros(}Value{)};
end;

{ TPagamentoCollectionItem }

constructor TPagamentoCollectionItem.Create;
begin
  inherited Create;

  FInformacoesBancarias := TInformacoesBancarias.Create;
end;

destructor TPagamentoCollectionItem.Destroy;
begin
  FInformacoesBancarias.Free;

  inherited Destroy;
end;

{ TPagamentoCollection }

function TPagamentoCollection.Add: TPagamentoCollectionItem;
begin
  Result := Self.New;
end;

function TPagamentoCollection.GetItem(Index: Integer): TPagamentoCollectionItem;
begin
  Result := TPagamentoCollectionItem(inherited Items[Index]);
end;

function TPagamentoCollection.New: TPagamentoCollectionItem;
begin
  Result := TPagamentoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TPagamentoCollection.SetItem(Index: Integer;
  Value: TPagamentoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TVeiculoCollectionItem }

procedure TVeiculoCollectionItem.SetPlaca(const Value: String);
begin
  FPlaca := RemoveStrings(Value, [' ', '-', '/', '\', '*', '_']);
end;

{ TVeiculoCollection }

function TVeiculoCollection.Add: TVeiculoCollectionItem;
begin
  Result := Self.New;
end;

function TVeiculoCollection.GetItem(Index: Integer): TVeiculoCollectionItem;
begin
  Result := TVeiculoCollectionItem(inherited Items[Index]);
end;

function TVeiculoCollection.New: TVeiculoCollectionItem;
begin
  Result := TVeiculoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TVeiculoCollection.SetItem(Index: Integer;
  Value: TVeiculoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TViagemCollectionItem }

constructor TViagemCollectionItem.Create;
begin
  inherited Create;

  FTipoPagamento := TransferenciaBancaria;

  FValores              := TValoresOT.Create;
  FInformacoesBancarias := TInformacoesBancarias.Create;
  FNotasFiscais         := TNotaFiscalCollection.Create;
end;

destructor TViagemCollectionItem.Destroy;
begin
  FValores.Free;
  FInformacoesBancarias.Free;
  FNotasFiscais.Free;

  inherited Destroy;
end;

procedure TViagemCollectionItem.SetNotasFiscais(
  const Value: TNotaFiscalCollection);
begin
  FNotasFiscais := Value;
end;

{ TViagemCollection }

function TViagemCollection.Add: TViagemCollectionItem;
begin
  Result := Self.New;
end;

function TViagemCollection.GetItem(Index: Integer): TViagemCollectionItem;
begin
  Result := TViagemCollectionItem(inherited Items[Index]);
end;

function TViagemCollection.New: TViagemCollectionItem;
begin
  Result := TViagemCollectionItem.Create;
  Self.Add(Result);
end;

procedure TViagemCollection.SetItem(Index: Integer;
  Value: TViagemCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TNotaFiscalCollectionItem }

constructor TNotaFiscalCollectionItem.Create;
begin
  inherited Create;

  FToleranciaDePerdaDeMercadoria := TToleranciaDePerdaDeMercadoria.Create;
  FDiferencaDeFrete := TDiferencaDeFrete.Create;
end;

destructor TNotaFiscalCollectionItem.Destroy;
begin
  FToleranciaDePerdaDeMercadoria.Free;
  FDiferencaDeFrete.Free;

  inherited Destroy;
end;

{ TNotaFiscalCollection }

function TNotaFiscalCollection.Add: TNotaFiscalCollectionItem;
begin
  Result := Self.New;
end;

function TNotaFiscalCollection.GetItem(
  Index: Integer): TNotaFiscalCollectionItem;
begin
  Result := TNotaFiscalCollectionItem(inherited Items[Index]);
end;

function TNotaFiscalCollection.New: TNotaFiscalCollectionItem;
begin
  Result := TNotaFiscalCollectionItem.Create;
  Self.Add(Result);
end;

procedure TNotaFiscalCollection.SetItem(Index: Integer;
  Value: TNotaFiscalCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TDiferencaDeFrete }

constructor TDiferencaDeFrete.Create;
begin
  inherited Create;

  Tolerancia := TDiferencaFreteMargem.Create;
  MargemGanho := TDiferencaFreteMargem.Create;
  MargemPerda := TDiferencaFreteMargem.Create;
end;

destructor TDiferencaDeFrete.Destroy;
begin
  Tolerancia.Free;
  MargemGanho.Free;
  MargemPerda.Free;

  inherited Destroy;
end;

{ TSubcontratante }

constructor TSubcontratante.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
  FTelefones := TTelefones.Create;
end;

destructor TSubcontratante.Destroy;
begin
  FEndereco.Free;
  FTelefones.Free;

  inherited Destroy;
end;

procedure TSubcontratante.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {SomenteNumeros(}Value{)};
end;

{ TTomadorServico }

constructor TTomadorServico.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
  FTelefones := TTelefones.Create;
end;

destructor TTomadorServico.Destroy;
begin
  FEndereco.Free;
  FTelefones.Free;

  inherited Destroy;
end;

procedure TTomadorServico.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {SomenteNumeros(}Value{)};
end;

{ TRetificarOperacao }

constructor TRetificarOperacao.Create;
begin
  inherited Create;

  FVeiculos := TVeiculoCollection.Create;
end;

destructor TRetificarOperacao.Destroy;
begin
  FVeiculos.Free;

  inherited Destroy;
end;

procedure TRetificarOperacao.setVeiculos(
  const Value: TVeiculoCollection);
begin
  FVeiculos := Value;
end;

// ************* Retorno

{ TRemetente }

constructor TRemetente.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
  FTelefones := TTelefones.Create;
end;

destructor TRemetente.Destroy;
begin
  FEndereco.Free;
  FTelefones.Free;

  inherited Destroy;
end;

procedure TRemetente.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {omenteNumeros(}Value{)};
end;

{ TProprietarioCarga }

constructor TProprietarioCarga.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
  FTelefones := TTelefones.Create;
end;

destructor TProprietarioCarga.Destroy;
begin
  FEndereco.Free;
  FTelefones.Free;

  inherited Destroy;
end;

procedure TProprietarioCarga.SetCpfOuCnpj(const Value: string);
begin
  FCpfOuCnpj := {omenteNumeros(}Value{)};
end;

{ TEncerrarOperacao }

constructor TEncerrarOperacao.Create;
begin
  inherited Create;

  FViagens := TViagemCollection.Create;
  FPagamentos := TPagamentoCollection.Create;
  FImpostos := TImpostos.Create;
end;

destructor TEncerrarOperacao.Destroy;
begin
  FViagens.Free;
  FPagamentos.Free;
  FImpostos.Free;

  inherited Destroy;
end;

procedure TEncerrarOperacao.SetPagamentos(const Value: TPagamentoCollection);
begin
  FPagamentos := Value;
end;

procedure TEncerrarOperacao.SetViagens(const Value: TViagemCollection);
begin
  FViagens := Value;
end;

{ TAdicionarViagem }

constructor TAdicionarViagem.Create;
begin
  inherited Create;

  FViagens := TViagemCollection.Create;
  FPagamentos := TPagamentoCollection.Create;
end;

destructor TAdicionarViagem.Destroy;
begin
  FViagens.Free;
  FPagamentos.Free;

  inherited Destroy;
end;

procedure TAdicionarViagem.SetPagamentos(const Value: TPagamentoCollection);
begin
  FPagamentos := Value;
end;

procedure TAdicionarViagem.SetViagens(const Value: TViagemCollection);
begin
  FViagens := Value;
end;

{ TMensagemCollection }

function TMensagemCollection.Add: TMensagemCollectionItem;
begin
  Result := Self.New;
end;

function TMensagemCollection.GetItem(Index: Integer): TMensagemCollectionItem;
begin
  Result := TMensagemCollectionItem(inherited Items[Index]);
end;

function TMensagemCollection.New: TMensagemCollectionItem;
begin
  Result := TMensagemCollectionItem.Create;
  Self.Add(Result);
end;

procedure TMensagemCollection.SetItem(Index: Integer;
  Value: TMensagemCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetEnvio }

constructor TRetEnvio.Create;
begin
  inherited Create;

  FProprietario       := TGravarProprietario.Create;
  FVeiculo            := TGravarVeiculo.Create;
  FMotorista          := TGravarMotorista.Create;
  FDocumentoViagem    := TMensagemCollection.Create;
  FDocumentoPagamento := TMensagemCollection.Create;
  FTipoCarga          := TConsultaTipoCargaCollection.Create;
  FAlterarDataLiberacaoPagamento := TAlterarDataLiberacaoPagamento.Create;
end;

destructor TRetEnvio.Destroy;
begin
  FProprietario.Free;
  FVeiculo.Free;
  FMotorista.Free;
  FDocumentoViagem.Free;
  FDocumentoPagamento.Free;
  FTipoCarga.Free;
  FAlterarDataLiberacaoPagamento.Free;

  inherited Destroy;
end;

procedure TRetEnvio.SetDocumentoPagamento(const Value: TMensagemCollection);
begin
  FDocumentoPagamento := Value;
end;

procedure TRetEnvio.SetDocumentoViagem(const Value: TMensagemCollection);
begin
  FDocumentoViagem := Value;
end;

procedure TRetEnvio.SetTipoCarga(const Value: TConsultaTipoCargaCollection);
begin
  FTipoCarga := Value;
end;

{ TAdicionarPagamento }

constructor TAdicionarPagamento.Create;
begin
  inherited Create;

  FPagamentos := TPagamentoCollection.Create;
end;

destructor TAdicionarPagamento.Destroy;
begin
  FPagamentos.Free;

  inherited Destroy;
end;

procedure TAdicionarPagamento.SetPagamentos(const Value: TPagamentoCollection);
begin
  FPagamentos := Value;
end;

{ TGravarMotorista }

constructor TGravarMotorista.Create;
begin
  inherited Create;

  FTelefones := TTelefones.Create;
  FEndereco  := TEndereco.Create;
end;

destructor TGravarMotorista.Destroy;
begin
  FTelefones.Free;
  FEndereco.Free;

  inherited Destroy;
end;

procedure TGravarMotorista.setCPF(const Value: string);
begin
  FCPF := {SomenteNumeros(}Value{)};
end;

{ TGravarProprietario }

constructor TGravarProprietario.Create;
begin
  inherited Create;

  FTelefones := TTelefones.Create;
  FEndereco  := TEndereco.Create;
end;

destructor TGravarProprietario.Destroy;
begin
  FTelefones.Free;
  FEndereco.Free;

  inherited Destroy;
end;

procedure TGravarProprietario.setCNPJ(const Value: string);
begin
  FCNPJ := {SomenteNumeros(}Value{)};
end;

{ TConsultaTipoCarga }

function TConsultaTipoCargaCollection.Add: TConsultaTipoCargaCollectionItem;
begin
  Result := Self.New;
end;

function TConsultaTipoCargaCollection.GetItem(Index: Integer): TConsultaTipoCargaCollectionItem;
begin
  Result := TConsultaTipoCargaCollectionItem(inherited Items[Index]);
end;

function TConsultaTipoCargaCollection.New: TConsultaTipoCargaCollectionItem;
begin
  Result := TConsultaTipoCargaCollectionItem.Create;
  Self.Add(Result);               
end;

procedure TConsultaTipoCargaCollection.SetItem(Index: Integer;
  Value: TConsultaTipoCargaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.

