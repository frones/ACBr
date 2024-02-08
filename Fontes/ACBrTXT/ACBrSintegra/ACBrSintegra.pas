{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Ederson Selvati                                 }
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

unit ACBrSintegra;

interface

uses Classes, SysUtils, Contnrs, ACBrConsts, ACBrBase, StrUtils;

type
  TVersaoValidador = (vv523, vv524);
  TVersaoEan = (eanIndefinido, ean8, ean12, ean13, ean14);


  TRegistro10 = class
  private
    FCNPJ: string;
    FCodigoConvenio: string;
    FFinalidadeArquivo: string;
    FNaturezaInformacoes: string;
    FRazaoSocial: string;
    FInscricao: string;
    FCidade: string;
    FDataFinal: TDateTime;
    FDataInicial: TDateTime;
    FTelefone: string;
    FEstado: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property Cidade: string read FCidade write FCidade;
    property Estado: string read FEstado write FEstado;
    property Telefone: string read FTelefone write FTelefone;
    property DataInicial: TDateTime read FDataInicial write FDataInicial;
    property DataFinal: TDateTime read FDataFinal write FDataFinal;
    property CodigoConvenio: string read FCodigoConvenio write FCodigoConvenio;
    property NaturezaInformacoes: string read FNaturezaInformacoes
      write FNaturezaInformacoes;
    property FinalidadeArquivo: string read FFinalidadeArquivo write FFinalidadeArquivo;
  end;

  TRegistro11 = class
  private
    FResponsavel: string;
    FBairro: string;
    FCep: string;
    FNumero: string;
    FComplemento : String;
    FEndereco: string;
    FTelefone: string;
  public
    property Endereco: string read FEndereco write FEndereco;
    property Numero: string read FNumero write FNumero;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Cep: string read FCep write FCep;
    property Responsavel: string read FResponsavel write FResponsavel;
    property Telefone: string read FTelefone write FTelefone;
  end;

  TRegistro88EC = class
  private
    FAlteraContabilista: integer;
    FCRCContabilista: string;
    FCPFContabilista: string;
    FEmailContabilista: string;
    FNomeContabilista: string;
    FTelefoneContabilista: string;
  public
    property NomeContabilista: string read FNomeContabilista write FNomeContabilista;
    property CPFContabilista: string read FCPFContabilista write FCPFContabilista;
    property CRCContabilista: string read FCRCContabilista write FCRCContabilista;
    property TelefoneContabilista: string read FTelefoneContabilista write FTelefoneContabilista;
    property EmailContabilista: string read FEmailContabilista write FEmailContabilista;
    property AlteraContabilista: integer read FAlteraContabilista write FAlteraContabilista;
  end;

  TRegistro88SF = class
  private
    FAlteraEmpresa: integer;
    FCNPJEmpresa: string;
    FEmailEmpresa: string;
    FTelefoneEmpresa: string;
    FNomeEmpresa: string;
    FCPFTecnico: string;
  public
    property NomeEmpresa: string read FNomeEmpresa write FNomeEmpresa;
    property CNPJEmpresa: string read FCNPJEmpresa write FCNPJEmpresa;
    property CPFTecnico: string read FCPFTecnico write FCPFTecnico;
    property TelefoneEmpresa: string read FTelefoneEmpresa write FTelefoneEmpresa;
    property EmailEmpresa: string read FEmailEmpresa write FEmailEmpresa;
    property AlteraEmpresa: integer read FAlteraEmpresa write FAlteraEmpresa;
  end;

  TRegistro88STES = class
  private
    FCNPJ: string;
    FDataInventario: TDateTime;
    FCodigoProduto: string;
    FQuantidade: Double;
    FVlrICMSST: Double;
    FVlrICMSOP: Double;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property DataInventario: TDateTime read FDataInventario write FDataInventario;
    property CodigoProduto: string read FCodigoProduto write FCodigoProduto;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property VlrICMSST: Double read FVlrICMSST write FVlrICMSST;
    property VlrICMSOP: Double read FVlrICMSOP write FVlrICMSOP;
  end;
  {Lista de objetos do tipo Registro88STES}
  TRegistros88STES = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88STES);
    function GetObject (Index: Integer): TRegistro88STES;
    procedure Insert (Index: Integer; Obj: TRegistro88STES);
  public
    function Add (Obj: TRegistro88STES): Integer;
    property Objects [Index: Integer]: TRegistro88STES read GetObject write SetObject; default;
  end;

  TRegistro88STITNF = class
  private
    FCNPJ: string;
    FModelo: string;
    FSerie: string;
    FNumero: string;
    FCFOP: string;
    FCST: string;
    FNumeroItem: integer;
    FDataEntrada: TDateTime;
    FCodigoProduto: string;
    FQuantidade: Double;
    FVlrProduto: Double;
    FValorDesconto: Double;
    FBaseICMSOP: Double;
    FBaseICMSST: Double;
    FAliquotaICMSST: Double;
    FAliquotaICMSOP: Double;
    FVlrIPI: Double;
    FChaveNFE: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FNumero write FNumero;
    property CFOP: string read FCFOP write FCFOP;
    property CST: string read FCST write FCST;
    property NumeroItem: integer read FNumeroItem write FNumeroItem;
    property DataEntrada: TDateTime read FDataEntrada write FDataEntrada;
    property CodigoProduto: string read FCodigoProduto write FCodigoProduto;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property VlrProduto: Double read FVlrProduto write FVlrProduto;
    property ValorDesconto: Double read FValorDesconto write FValorDesconto;
    property BaseICMSOP: Double read FBaseICMSOP write FBaseICMSOP;
    property BaseICMSST: Double read FBaseICMSST write FBaseICMSST;
    property AliquotaICMSST: Double read FAliquotaICMSST write FAliquotaICMSST;
    property AliquotaICMSOP: Double read FAliquotaICMSOP write FAliquotaICMSOP;
    property VlrIPI: Double read FVlrIPI write FVlrIPI;
    property ChaveNFE: string read FChaveNFE write FChaveNFE;
  end;
  {Lista de objetos do tipo Registro88STITNF}
  TRegistros88STITNF = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88STITNF);
    function GetObject (Index: Integer): TRegistro88STITNF;
    procedure Insert (Index: Integer; Obj: TRegistro88STITNF);
  public
    function Add (Obj: TRegistro88STITNF): Integer;
    property Objects [Index: Integer]: TRegistro88STITNF read GetObject write SetObject; default;
  end;

  TRegistro88C = class
  private
    FCPFCNPJ: string;
    FModelo: string;
    FSerie: string;
    FNumero: string;
    FCFOP: string;
    FNumeroItem: Integer;
    FCodigo: string;
    FQuantidade: Double;
    FBaseST: Double;
    FValorIcmsSTRepassar: Double;
    FValorIcmsSTComplementar: Double;
    FBasedeCalculoRetencao: Double;
    FValorParcelaImpostoRetido: Double;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FNumero write FNumero;
    property CFOP: string read FCFOP write FCFOP;
    property NumeroItem: Integer read FNumeroItem write FNumeroItem;
    property Codigo: string read FCodigo write FCodigo;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property BaseST: Double read FBaseST write FBaseST;
    property ValorIcmsSTRepassar: Double read FValorIcmsSTRepassar write FValorIcmsSTRepassar;
    property ValorIcmsSTComplementar: Double read FValorIcmsSTComplementar write FValorIcmsSTComplementar;
    property BasedeCalculoRetencao: Double read FBasedeCalculoRetencao write FBasedeCalculoRetencao;
    property ValorParcelaImpostoRetido: Double read FValorParcelaImpostoRetido write FValorParcelaImpostoRetido;
  end;

  {Lista de objetos do tipo Registro88C}
  TRegistros88C = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88C);
    function GetObject (Index: Integer): TRegistro88C;
    procedure Insert (Index: Integer; Obj: TRegistro88C);
  public
    function Add (Obj: TRegistro88C): Integer;
    property Objects [Index: Integer]: TRegistro88C read GetObject write SetObject; default;
  end;

  TRegistro88D = class
  private
    FCPFCNPJ: string;
    FInscricao: string;
    FUF: string;
    FModelo: string;
    FSerie: string;
    FNumero: string;
    FEmissorDocumento: string;
    FDataDocumento: TDateTime;
    FDataSaidaEntrada: TDateTime;
    FCNPJLocalSaida: String;
    FUFLocalSaida: string;
    FIeLocalSaida: string;
    FCNPJLocalEntrega: String;
    FUFLocalEntrega: string;
    FIeLocalEntrega: string;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property UF: string read FUF write FUF;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FNumero write FNumero;
    property EmissorDocumento: string read FEmissorDocumento write FEmissorDocumento;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property DataSaidaEntrada: TDateTime read FDataSaidaEntrada write FDataSaidaEntrada;
    property CNPJLocalSaida: String read FCNPJLocalSaida write FCNPJLocalSaida;
    property UFLocalSaida: string read FUFLocalSaida write FUFLocalSaida;
    property IeLocalSaida: string read FIeLocalSaida write FIeLocalSaida;
    property CNPJLocalEntrega: String read FCNPJLocalEntrega write FCNPJLocalEntrega;
    property UFLocalEntrega: string read FUFLocalEntrega write FUFLocalEntrega;
    property IeLocalEntrega: string read FIeLocalEntrega write FIeLocalEntrega;
  end;

  {Lista de objetos do tipo Registro88D}
  TRegistros88D = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88D);
    function GetObject (Index: Integer): TRegistro88D;
    procedure Insert (Index: Integer; Obj: TRegistro88D);
  public
    function Add (Obj: TRegistro88D): Integer;
    property Objects [Index: Integer]: TRegistro88D read GetObject write SetObject; default;
  end;

  TRegistro88E = class
  private
    FCPFCNPJ: string;
    FInscricao: string;
    FCodigoInformante: string;
    FCodigoSefaz: string;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property CodigoInformate: string read FCodigoInformante write FCodigoInformante;
    property CodigoSefaz: string read FCodigoSefaz write FCodigoSefaz;
  end;

  {Lista de objetos do tipo Registro88E}
  TRegistros88E = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88E);
    function GetObject (Index: Integer): TRegistro88E;
    procedure Insert (Index: Integer; Obj: TRegistro88E);
  public
    function Add (Obj: TRegistro88E): Integer;
    property Objects [Index: Integer]: TRegistro88E read GetObject write SetObject; default;
  end;

  TRegistro88T = class
  private
    FCPFCNPJ: string;
    FDataDocumento: TDateTime;
    FUF: string;
    FModelo: string;
    FSerie: string;
    FNumero: string;
    FEmissorDocumento: string;
    FCifFob: String;
    FCPFCNPJFrete: String;
    FUFFrete: string;
    FIeFrete: string;
    FModal: Integer;
    FPlaca1: String;
    FUFPlaca1: String;
    FPlaca2: String;
    FUFPlaca2: String;
    FPlaca3: String;
    FUFPlaca3: String;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property UF: string read FUF write FUF;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FNumero write FNumero;
    property EmissorDocumento: string read FEmissorDocumento write FEmissorDocumento;
    property CifFob: String read FCifFob write FCifFob;
    property CPFCNPJFrete: String read FCPFCNPJFrete write FCPFCNPJFrete;
    property UFFrete: string read FUFFrete write FUFFrete;
    property IeFrete: string read FIeFrete write FIeFrete;
    property Modal: Integer read FModal write FModal;
    property Placa1: String read FPlaca1 write FPlaca1;
    property UFPlaca1: String read FUFPlaca1 write FUFPlaca1;
    property Placa2: String read FPlaca2 write FPlaca2;
    property UFPlaca2: String read FUFPlaca2 write FUFPlaca2;
    property Placa3: String read FPlaca3 write FPlaca3;
    property UFPlaca3: String read FUFPlaca3 write FUFPlaca3;
  end;

  {Lista de objetos do tipo Registro88D}
  TRegistros88T = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88T);
    function GetObject (Index: Integer): TRegistro88T;
    procedure Insert (Index: Integer; Obj: TRegistro88T);
  public
    function Add (Obj: TRegistro88T): Integer;
    property Objects [Index: Integer]: TRegistro88T read GetObject write SetObject; default;
  end;

  TRegistro54 = class
  private
    FAliquota: Double;
    FBaseST: Double;
    FBasedeCalculo: Double;
    FQuantidade: Double;
    FValorDescontoDespesa: Double;
    FValorIpi: Double;
    FValor: Double;
    FNumeroItem: Integer;
    FCST: string;
    FCodigo: string;
    FCFOP: string;
    FDescricao: string;
    FCPFCNPJ: string;
    FNumero: string;
    FModelo: string;
    FSerie: string;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FNumero write FNumero;
    property CFOP: string read FCFOP write FCFOP;
    property CST: string read FCST write FCST;
    property NumeroItem: Integer read FNumeroItem write FNumeroItem;
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property Valor: Double read FValor write FValor;
    property ValorDescontoDespesa: Double read FValorDescontoDespesa
      write FValorDescontoDespesa;
    property BasedeCalculo: Double read FBasedeCalculo write FBasedeCalculo;
    property BaseST: Double read FBaseST write FBaseST;
    property ValorIpi: Double read FValorIpi write FValorIpi;
    property Aliquota: Double read FAliquota write FAliquota;
  end;

  {Lista de objetos do tipo Registro54}
  TRegistros54 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro54);
    function GetObject (Index: Integer): TRegistro54;
    procedure Insert (Index: Integer; Obj: TRegistro54);
  public
    function Add (Obj: TRegistro54): Integer;
    property Objects [Index: Integer]: TRegistro54
      read GetObject write SetObject; default;
  end;

  TRegistro55 = class
  private
    FValor: double;
    FAgencia: Integer;
    FBanco: Integer;
    FNumeroConvenio: string;
    FInscricao: string;
    FMesAno: string;
    FCNPJ: string;
    FUF: string;
    FUFFavorecida: string;
    FNumero: string;
    FDataPagamento: TDateTime;
    FVencimento: TDateTime;

  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property UF: string read FUF write FUF;
    property UFFavorecida: string read FUFFavorecida write FUFFavorecida;
    property Banco: Integer read FBanco write FBanco;
    property Agencia: Integer read FAgencia write FAgencia;
    property Numero: string read FNumero write FNumero;
    property Valor: double read FValor write FValor;
    property Vencimento: TDateTime read FVencimento write FVencimento;
    property MesAno: string read FMesAno write FMesAno;
    property NumeroConvenio: string read FNumeroConvenio write FNumeroConvenio;
  end;

  TRegistros55 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro55);
    function GetObject (Index: Integer): TRegistro55;
    procedure Insert (Index: Integer; Obj: TRegistro55);
  public
    function Add (Obj: TRegistro55): Integer;
    property Objects [Index: Integer]: TRegistro55
      read GetObject write SetObject; default;
  end;

  TRegistro56 = class
  private
    FCnpj: string;
    FModelo: string;
    FSerie: string;
    FNumero: string;
    FCfop: string;
    FCst: string;
    FNumeroItem: Integer;
    FCodigo: string;
    FTipoOperacao: string;
    FCnpjConcessionaria: string;
    FIpi: Double;
    FChassi: string;
  public
    property Cnpj: string read FCnpj write FCnpj;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FNumero write FNumero;
    property Cfop: string read Fcfop write Fcfop;
    property Cst: string read FCst write FCst;
    property NumeroItem: Integer read FNumeroItem write FNumeroItem;
    property Codigo: string read FCodigo write FCodigo;
    property TipoOperacao: string read FTipoOperacao write FTipoOperacao;
    property CnpjConcessionaria: string read FCnpjConcessionaria write FCnpjConcessionaria;
    property Ipi: Double read FIpi write FIpi;
    property Chassi: string read FChassi write FChassi;
  end;

  {Lista de objetos do tipo Registro56}
  TRegistros56 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro56);
    function GetObject (Index: Integer): TRegistro56;
    procedure Insert (Index: Integer; Obj: TRegistro56);
  public
    function Add (Obj: TRegistro56): Integer;
    property Objects [Index: Integer]: TRegistro56
      read GetObject write SetObject; default;
  end;

  TRegistro74 = class
  private
    FValorProduto: Double;
    FCodigoPosse: string;
    FInscricaoPossuidor: string;
    FCodigo: string;
    FCNPJPossuidor: string;
    FUFPossuidor: string;
    FData: TDateTime;
    FQuantidade: Double;
  public
    property Data: TDateTime read FData write FData;
    property Codigo: string read FCodigo write FCodigo;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property ValorProduto: Double read FValorProduto write FValorProduto;
    property CodigoPosse: string read FCodigoPosse write FCodigoPosse;
    property CNPJPossuidor: string read FCNPJPossuidor write FCNPJPossuidor;
    property InscricaoPossuidor: string read FInscricaoPossuidor
      write FInscricaoPossuidor;
    property UFPossuidor: string read FUFPossuidor write FUFPossuidor;
  end;

  {Lista de objetos do tipo Registro74}

  { TRegistros74 }

  TRegistros74 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro74);
    function GetObject (Index: Integer): TRegistro74;
    procedure Insert (Index: Integer; Obj: TRegistro74);
    function GetRegistroExiste(const FCodigo: string): Integer;
  public
    function Add (Obj: TRegistro74): Integer;
    property Objects [Index: Integer]: TRegistro74
      read GetObject write SetObject; default;
  end;

  TRegistro75 = class
  private
    FCodigo: string;
    FDescricao: string;
    FReducao: Double;
    FBaseST: Double;
    FAliquotaIpi: Double;
    FNCM: string;
    FUnidade: string;
    FAliquotaICMS: Double;
    FDataFinal: TDateTime;
    FDataInicial: TDateTime;
  public
    property DataInicial: TDateTime read FDataInicial write FDataInicial;
    property DataFinal: TDateTime read FDataFinal write FDataFinal;
    property Codigo: string read FCodigo write FCodigo;
    property NCM: string read FNCM write FNCM;
    property Descricao: string read FDescricao write FDescricao;
    property Unidade: string read FUnidade write FUnidade;
    property AliquotaIpi: Double read FAliquotaIpi write FAliquotaIpi;
    property AliquotaICMS: Double read FAliquotaICMS write FAliquotaICMS;
    property Reducao: Double read FReducao write FReducao;
    property BaseST: Double read FBaseST write FBaseST;
  end;

  {Lista de objetos do tipo Registro54}
  TRegistros75 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro75);
    function GetObject (Index: Integer): TRegistro75;
    procedure Insert (Index: Integer; Obj: TRegistro75);
    function GetRegistroExiste(const FCodigo: string): Integer;
  public
    function Add (Obj: TRegistro75): Integer;
    property Objects [Index: Integer]: TRegistro75
      read GetObject write SetObject; default;
  end;

  TTipoReceita = (trPropria, trTerceiros, trRessarcimento);

  TRegistro76 = class
  private
    FIsentas: Double;
    FValorTotal: Double;
    FIcms: Double;
    FBasedeCalculo: Double;
    FOutras: Double;
    FModelo: Integer;
    FNumero: Integer;
    FSituacao: string;
    FInscricao: string;
    FSubSerie: string;
    FUf: string;
    FSerie: string;
    FCfop: string;
    FCPFCNPJ: string;
    FDataDocumento: TDateTime;
    FTipoReceita: TTipoReceita;
    FAliquota: Integer;

  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property Modelo: Integer read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property SubSerie: string read FSubSerie write FSubSerie;
    property Numero: Integer read FNumero write FNumero;
    property Cfop: string read FCfop write FCfop;
    property TipoReceita: TTipoReceita read FTipoReceita write FTipoReceita;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property Uf: string read FUf write FUf;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property BasedeCalculo: Double read FBasedeCalculo write FBasedeCalculo;
    property Icms: Double read FIcms write FIcms;
    property Isentas: Double read FIsentas write FIsentas;
    property Outras: Double read FOutras write FOutras;
    property Aliquota: Integer read FAliquota write FAliquota;
    property Situacao: string read FSituacao write FSituacao;
  end;

  {Lista de objetos do tipo Registro76}
  TRegistros76 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro76);
    function GetObject (Index: Integer): TRegistro76;
    procedure Insert (Index: Integer; Obj: TRegistro76);
  public
    function Add (Obj: TRegistro76): Integer;
    property Objects [Index: Integer]: TRegistro76
      read GetObject write SetObject; default;
  end;

  TRegistro77 = class
  private
    FQuantidade: Double;
    FValorServico: Double;
    FValorDesconto: Double;
    FBaseDeCalculo: Double;
    FModelo: Integer;
    FNumero: Integer;
    FNumeroTerminal: Integer;
    FNumeroItem: Integer;
    FAliquota: Integer;
    FCNPJMF: string;
    FCfop: string;
    FCodigo: string;
    FSubSerie: string;
    FCPFCNPJ: string;
    FSerie: string;
    FTipoReceita: TTipoReceita;

  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Modelo: Integer read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property SubSerie: string read FSubSerie write FSubSerie;
    property Numero: Integer read FNumero write FNumero;
    property Cfop: string read FCfop write FCfop;
    property TipoReceita: TTipoReceita read FTipoReceita write FTipoReceita;
    property NumeroItem: Integer read FNumeroItem write FNumeroItem;
    property Codigo: string read FCodigo write FCodigo;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property ValorServico: Double read FValorServico write FValorServico;
    property ValorDesconto: Double read FValorDesconto write FValorDesconto;
    property BaseDeCalculo: Double read FBaseDeCalculo write FBaseDeCalculo;
    property Aliquota: Integer read FAliquota write FAliquota;
    property CNPJMF: string read FCNPJMF write FCNPJMF;
    property NumeroTerminal: Integer read FNumeroTerminal write FNumeroTerminal;
  end;

  {Lista de objetos do tipo Registro77}
  TRegistros77 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro77);
    function GetObject (Index: Integer): TRegistro77;
    procedure Insert (Index: Integer; Obj: TRegistro77);
  public
    function Add (Obj: TRegistro77): Integer;
    property Objects [Index: Integer]: TRegistro77
      read GetObject write SetObject; default;
  end;


  TRegistro50 = class
  private
    FOutras: Double;
    FSituacao: string;
    FAliquota: Double;
    FIsentas: Double;
    FIcms: Double;
    FValorContabil: Double;
    FBasedeCalculo: Double;
    FEmissorDocumento: string;
    FCfop: string;
    FInscricao: string;
    FUF: string;
    FSerie: string;
    FCPFCNPJ: string;
    FModelo: string;
    FNumero: string;
    FDataDocumento: TDateTime;
    FRegistros54: TRegistros54;
    FRegistros56: TRegistros56;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property UF: string read FUF write FUF;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FNumero write FNumero;
    property Cfop: string read FCfop write FCfop;
    property EmissorDocumento: string read FEmissorDocumento
      write FEmissorDocumento;
    property ValorContabil: Double read FValorContabil write FValorContabil;
    property BasedeCalculo: Double read FBasedeCalculo write FBasedeCalculo;
    property Icms: Double read FIcms write FIcms;
    property Isentas: Double read FIsentas write FIsentas;
    property Outras: Double read FOutras write FOutras;
    property Aliquota: Double read FAliquota write FAliquota;
    property Situacao: string read FSituacao write FSituacao;
    property Registros54: TRegistros54 read FRegistros54 write FRegistros54;
    property Registros56: TRegistros56 read FRegistros56 write FRegistros56;
    constructor Create;
    destructor Destroy; override;
  end;

  {Lista de objetos do tipo Registro50}
  TRegistros50 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro50);
    function GetObject (Index: Integer): TRegistro50;
    procedure Insert (Index: Integer; Obj: TRegistro50);
  public
    function Add (Obj: TRegistro50): Integer;
    property Objects [Index: Integer]: TRegistro50
      read GetObject write SetObject; default;
  end;

  TRegistro51 = class
  private
    FValorIpi: Double;
    FValorContabil: Double;
    FSerie: string;
    FDataDocumento: TDateTime;
    FCPFCNPJ: string;
    FCfop: string;
    FString: string;
    FInscricao: string;
    FSituacao: string;
    FValorIsentas: Double;
    FValorOutras: Double;
    FEstado: string;

  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property Estado: string read FEstado write FEstado;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FString write FString;
    property CFOP: string read FCfop write FCfop;
    property ValorContabil: Double read FValorContabil write FValorContabil;
    property ValorIpi: Double read FValorIpi write FValorIpi;
    property ValorOutras: Double read FValorOutras write FValorOutras;
    property ValorIsentas: Double read FValorIsentas write FValorIsentas;
    property Situacao: string read FSituacao write FSituacao;
  end;

  {Lista de objetos do tipo Registro51}
  TRegistros51 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro51);
    function GetObject (Index: Integer): TRegistro51;
    procedure Insert (Index: Integer; Obj: TRegistro51);
  public
    function Add (Obj: TRegistro51): Integer;
    property Objects [Index: Integer]: TRegistro51
      read GetObject write SetObject; default;
  end;

  TRegistro53 = class
  private
    FSerie: string;
    FDataDocumento: TDateTime;
    FCPFCNPJ: string;
    FCfop: string;
    FString: string;
    FInscricao: string;
    FSituacao: string;
    FEstado: string;
    FCodigoAntecipacao: string;
    FBaseST: Double;
    FModelo: string;
    FEmitente: string;
    FDespesas: Double;
    FIcmsRetido: Double;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property Estado: string read FEstado write FEstado;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Numero: string read FString write FString;
    property CFOP: string read FCfop write FCfop;
    property Emitente: string read FEmitente write FEmitente;
    property BaseST: Double read FBaseST write FBaseST;
    property IcmsRetido: Double read FIcmsRetido write FIcmsRetido;
    property Despesas: Double read FDespesas write FDespesas;
    property Situacao: string read FSituacao write FSituacao;
    property CodigoAntecipacao: string read FCodigoAntecipacao write FCodigoAntecipacao;
  end;

  {Lista de objetos do tipo Registro53}
  TRegistros53 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro53);
    function GetObject (Index: Integer): TRegistro53;
    procedure Insert (Index: Integer; Obj: TRegistro53);
  public
    function Add (Obj: TRegistro53): Integer;
    property Objects [Index: Integer]: TRegistro53
      read GetObject write SetObject; default;
  end;

  TRegistro70 = class
  private
    FIcms: Double;
    FValorContabil: Double;
    FUF: string;
    FIsentas: Double;
    FSubSerie: string;
    FSerie: string;
    FDataDocumento: TDateTime;
    FModelo: string;
    FCPFCNPJ: string;
    FCfop: string;
    FNumero: string;
    FInscricao: string;
    FSituacao: string;
    FOutras: Double;
    FBasedeCalculo: Double;
    FCifFobOutros: string;
  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property UF: string read FUF write FUF;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property SubSerie: string read FSubSerie write FSubSerie;
    property Numero: string read FNumero write FNumero;
    property Cfop: string read FCfop write FCfop;
    property ValorContabil: Double read FValorContabil write FValorContabil;
    property BasedeCalculo: Double read FBasedeCalculo write FBasedeCalculo;
    property Icms: Double read FIcms write FIcms;
    property Isentas: Double read FIsentas write FIsentas;
    property Outras: Double read FOutras write FOutras;
    property CifFobOutros: string read FCifFobOutros write FCifFobOutros;
    property Situacao: string read FSituacao write FSituacao;
  end;

  {Lista de objetos do tipo Registro70}
  TRegistros70 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro70);
    function GetObject (Index: Integer): TRegistro70;
    procedure Insert (Index: Integer; Obj: TRegistro70);
  public
    function Add (Obj: TRegistro70): Integer;
    property Objects [Index: Integer]: TRegistro70
      read GetObject write SetObject; default;
  end;


  TRegistro71 = class
  private
    FCPFCNPJ: string;
    FInscricao: string;
    FDataDocumento: TDateTime;
    FModelo: string;
    FSerie: string;
    FSubSerie: string;
    FNumero: string;
    FUF: string;

    FUFNF: string;
    FCPFCNPJNF: string;
    FInscricaoNF: string;
    FDataNF: TDateTime;
    FModeloNF: string;
    FSerieNF: string;
    FNumeroNF: string;
    FValorNF: Double;

  public
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property SubSerie: string read FSubSerie write FSubSerie;
    property Numero: string read FNumero write FNumero;
    property UF: string read FUF write FUF;

    property CPFCNPJNF: string read FCPFCNPJNF write FCPFCNPJNF;
    property InscricaoNF: string read FInscricaoNF write FInscricaoNF;
    property DataNF: TDateTime read FDataNF write FDataNF;
    property ModeloNF: string read FModeloNF write FModeloNF;
    property SerieNF: string read FSerieNF write FSerieNF;
    property NumeroNF: string read FNumeroNF write FNumeroNF;
    property UFNF: string read FUFNF write FUFNF;
    property ValorNF: Double read FValorNF write FValorNF;
  end;

  {Lista de objetos do tipo Registro71}
  TRegistros71 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro71);
    function GetObject (Index: Integer): TRegistro71;
    procedure Insert (Index: Integer; Obj: TRegistro71);
  public
    function Add (Obj: TRegistro71): Integer;
    property Objects [Index: Integer]: TRegistro71
      read GetObject write SetObject; default;
  end;


  TRegistro60A = class
  private
    FNumSerie: string;
    FStAliquota: string;
    FEmissao: TDateTime;
    FValor: Double;
  public
    property Emissao: TDateTime read FEmissao write FEmissao;
    property NumSerie: string read FNumSerie write FNumSerie;
    property StAliquota: string read FStAliquota write FStAliquota;
    property Valor: Double read FValor write FValor;
  end;

  {Lista de objetos do tipo Registro60A}
  TRegistros60A = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro60A);
    function GetObject (Index: Integer): TRegistro60A;
    procedure Insert (Index: Integer; Obj: TRegistro60A);
  public
    function Add (Obj: TRegistro60A): Integer;
    property Objects [Index: Integer]: TRegistro60A
      read GetObject write SetObject; default;
  end;

  TRegistro60I = class
  private
    FNumSerie: string;
    FStAliquota: string;
    FEmissao: TDateTime;
    FValor: Double;
    FCodigo: string;
    FValorIcms: Double;
    FQuantidade: Double;
    FBaseDeCalculo: Double;
    FItem: Integer;
    FCupom: string;
    FModeloDoc: string;
  public
    property Emissao: TDateTime read FEmissao write FEmissao;
    property NumSerie: string read FNumSerie write FNumSerie;
    property ModeloDoc: string read FModeloDoc write FModeloDoc;
    property Cupom: string read FCupom write FCupom;
    property Item: Integer read FItem write FItem;
    property Codigo: string read FCodigo write FCodigo;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property Valor: Double read FValor write FValor;
    property BaseDeCalculo: Double read FBaseDeCalculo write FBaseDeCalculo;
    property StAliquota: string read FStAliquota write FStAliquota;
    property ValorIcms: Double read FValorIcms write FValorIcms;
  end;

  {Lista de objetos do tipo Registro60I}
  TRegistros60I = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro60I);
    function GetObject (Index: Integer): TRegistro60I;
    procedure Insert (Index: Integer; Obj: TRegistro60I);
  public
    function Add (Obj: TRegistro60I): Integer;
    property Objects [Index: Integer]: TRegistro60I
      read GetObject write SetObject; default;
  end;

  TRegistro60D = class
  private
    FNumSerie: string;
    FStAliquota: string;
    FEmissao: TDateTime;
    FValor: Double;
    FCodigo: string;
    FValorIcms: Double;
    FQuantidade: Double;
    FBaseDeCalculo: Double;
  public
    property Emissao: TDateTime read FEmissao write FEmissao;
    property NumSerie: string read FNumSerie write FNumSerie;
    property Codigo: string read FCodigo write FCodigo;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property Valor: Double read FValor write FValor;
    property BaseDeCalculo: Double read FBaseDeCalculo write FBaseDeCalculo;
    property StAliquota: string read FStAliquota write FStAliquota;
    property ValorIcms: Double read FValorIcms write FValorIcms;
  end;

  {Lista de objetos do tipo Registro60D}
  TRegistros60D = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro60D);
    function GetObject (Index: Integer): TRegistro60D;
    procedure Insert (Index: Integer; Obj: TRegistro60D);
  public
    function Add (Obj: TRegistro60D): Integer;
    property Objects [Index: Integer]: TRegistro60D
      read GetObject write SetObject; default;
  end;

  TRegistro60R = class
  private
    FBaseDeCalculo: double;
    FValor: Double;
    FQtd: Double;
    FMesAno: string;
    FCodigo: string;
    FAliquota: string;

  public
    property MesAno: string read FMesAno write FMesAno;
    property Codigo: string read FCodigo write FCodigo;
    property Qtd: Double read FQtd write FQtd;
    property Valor: Double read FValor write FValor;
    property BaseDeCalculo: double read FBaseDeCalculo write FBaseDeCalculo;
    property Aliquota: string read FAliquota write FAliquota;
  end;

  {Lista de objetos do tipo Registro60D}
  TRegistros60R = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro60R);
    function GetObject (Index: Integer): TRegistro60R;
    procedure Insert (Index: Integer; Obj: TRegistro60R);
  public
    function Add (Obj: TRegistro60R): Integer;
    property Objects [Index: Integer]: TRegistro60R
      read GetObject write SetObject; default;
  end;

  TRegistro60M = class
  private
    FCRO: Integer;
    FNumOrdem: Integer;
    FVendaBruta: Double;
    FModeloDoc: string;
    FValorGT: Double;
    FCRZ: Integer;
    FCooFinal: Integer;
    FCooInicial: Integer;
    FNumSerie: string;
    FEmissao: TDateTime;
    FRegs60A: TRegistros60A;
    FRegs60D: TRegistros60D;
    FRegs60I: TRegistros60I;
  public
    property Regs60A: TRegistros60A read FRegs60A write FRegs60A;
    property Regs60D: TRegistros60D read FRegs60D write FRegs60D;
    property Regs60I: TRegistros60I read FRegs60I write FRegs60I;

    property Emissao: TDateTime read FEmissao write FEmissao;
    property NumSerie: string read FNumSerie write FNumSerie;
    property NumOrdem: Integer read FNumOrdem write FNumOrdem;
    property ModeloDoc: string read FModeloDoc write FModeloDoc;
    property CooInicial: Integer read FCooInicial write FCooInicial;
    property CooFinal: Integer read FCooFinal write FCooFinal;
    property CRZ: Integer read FCRZ write FCRZ;
    property CRO: Integer read FCRO write FCRO;
    property VendaBruta: Double read FVendaBruta write FVendaBruta;
    property ValorGT: Double read FValorGT write FValorGT;
    constructor Create;
    destructor Destroy; override;
  end;

  {Lista de objetos do tipo Registro60M}
  TRegistros60M = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro60M);
    function GetObject (Index: Integer): TRegistro60M;
    procedure Insert (Index: Integer; Obj: TRegistro60M);
  public
    function Add (Obj: TRegistro60M): Integer;
    property Objects [Index: Integer]: TRegistro60M
      read GetObject write SetObject; default;
  end;

  TRegistro61 = class
  private
    FEmissao: TDateTime;
    FValor: Double;
    FValorIcms: Double;
    FOutras: Double;
    FBaseDeCalculo: Double;
    FIsentas: Double;
    FNumOrdemInicial: integer;
    FNumOrdemFinal: integer;
    FModelo: string;
    FSubSerie: string;
    FSerie: string;
    FAliquota: Double;
  public
    property Emissao: TDateTime read FEmissao write FEmissao;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property SubSerie: string read FSubSerie write FSubSerie;
    property NumOrdemInicial: integer read FNumOrdemInicial
      write FNumOrdemInicial;
    property NumOrdemFinal: integer read FNumOrdemFinal
      write FNumOrdemFinal;
    property Valor: Double read FValor write FValor;
    property BaseDeCalculo: Double read FBaseDeCalculo write FBaseDeCalculo;
    property ValorIcms: Double read FValorIcms write FValorIcms;
    property Isentas: Double read FIsentas write FIsentas;
    property Outras: Double read FOutras write FOutras;
    property Aliquota: Double read FAliquota write FAliquota;
  end;

  {Lista de objetos do tipo Registro61}
  TRegistros61 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro61);
    function GetObject (Index: Integer): TRegistro61;
    procedure Insert (Index: Integer; Obj: TRegistro61);
  public
    function Add (Obj: TRegistro61): Integer;
    property Objects [Index: Integer]: TRegistro61
      read GetObject write SetObject; default;
  end;

  TRegistro61R = class
  private
    FAliquota: Double;
    FValor: Double;
    FQtd: Double;
    FMesAno: string;
    FCodigo: string;
    FBaseDeCalculo: Double;
  public
    property MesAno: string read FMesAno write FMesAno;
    property Codigo: string read FCodigo write FCodigo;
    property Qtd: Double read FQtd write FQtd;
    property Valor: Double read FValor write FValor;
    property BaseDeCalculo: Double read FBaseDeCalculo write FBaseDeCalculo;
    property Aliquota: Double read FAliquota write FAliquota;
  end;

  {Lista de objetos do tipo Registro61}
  TRegistros61R = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro61R);
    function GetObject (Index: Integer): TRegistro61R;
    procedure Insert (Index: Integer; Obj: TRegistro61R);
  public
    function Add (Obj: TRegistro61R): Integer;
    property Objects [Index: Integer]: TRegistro61R
      read GetObject write SetObject; default;
  end;
  TRegistro85 = class
  private
    FDeclaracao: string;
    FDataDeclaracao: TDateTime;
    FNaturezaExportacao: string;
    FRegistroExportacao: String;
    FDataRegistro: TDateTime;
    FConhecimento:string;
    FDataConhecimento:TDateTime;
    FTipoConhecimento: String;
    FPais:string;
    FDataAverbacao:TDateTime;
    FNumeroNotaFiscal:String;
    FDataNotaFiscal:TDateTime;
    FModelo: string;
    FSerie: string;
  public
    property Declaracao: String read FDeclaracao write FDeclaracao;
    property DataDeclaracao: TDateTime read FDataDeclaracao write FDataDeclaracao;
    property NaturezaExportacao: string read FNaturezaExportacao write FNaturezaExportacao;
    property RegistroExportacao: String read FRegistroExportacao write FRegistroExportacao;
    property DataRegistro: TDateTime read FDataRegistro write FDataRegistro;
    property Conhecimento: string read FConhecimento write FConhecimento;
    property DataConhecimento: TDateTime read FDataConhecimento write FDataConhecimento;
    property TipoConhecimento: String read FTipoConhecimento write FTipoConhecimento;
    property Pais: string read FPais write FPais;
    property DataAverbacao: TDateTime read FDataAverbacao write FDataAverbacao;
    property NumeroNotaFiscal: string read FNumeroNotaFiscal write FNumeroNotaFiscal;
    property DataNotaFiscal: TDateTime read FDataNotaFiscal write FDataNotaFiscal;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
  end;

  {Lista de objetos do tipo Registro85}
  TRegistros85 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro85);
    function GetObject (Index: Integer): TRegistro85;
    procedure Insert (Index: Integer; Obj: TRegistro85);
  public
    function Add (Obj: TRegistro85): Integer;
    property Objects [Index: Integer]: TRegistro85
      read GetObject write SetObject; default;
  end;

  TRegistro86 = class
  private
    FRegistroExportacao:string;
    FDataRegistro:TDateTime;
    FCPFCNPJ: string;
    FInscricao: string;
    FUF: string;
    FNumeroNotaFiscal: string;
    FDataDocumento: TDateTime;
    FModelo: string;
    FSerie: string;
    FCodigo: string;
    FQuantidade: Double;
    FValorUnitario :Double;
    FValorTotalProduto: Double;
    FRelacionamento: string;
  public
    property RegistroExportacao: string read FRegistroExportacao write FRegistroExportacao;
    property DataRegistro: TDateTime read FDataRegistro write FDataRegistro;
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Inscricao: string read FInscricao write FInscricao;
    property UF: string read FUF write FUF;
    property NumeroNotaFiscal: string read FNumeroNotaFiscal write FNumeroNotaFiscal;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property Modelo: string read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Codigo: string read FCodigo write FCodigo;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property ValorUnitario: Double read FValorUnitario write FValorUnitario;
    property ValorTotalProduto: Double read FValorTotalProduto write FValorTotalProduto;
    property Relacionamento: string read FRelacionamento write FRelacionamento;
  end;

  {Lista de objetos do tipo Registro86}
  TRegistros86 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro86);
    function GetObject (Index: Integer): TRegistro86;
    procedure Insert (Index: Integer; Obj: TRegistro86);
  public
    function Add (Obj: TRegistro86): Integer;
    property Objects [Index: Integer]: TRegistro86
      read GetObject write SetObject; default;
  end;

  TRegistro88Ean = class
  private
    FCodigoBarras: string;
    FUnidade: string;
    FDescricao: string;
    FCodigo: Int64;
    FVersaoEan: TVersaoEan;

  public
    property VersaoEan: TVersaoEan read FVersaoEan write FVersaoEan;
    property Codigo: Int64 read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property Unidade: string read FUnidade write FUnidade;
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    constructor Create;
  end;

  TRegistros88Ean = class(TObjectList)
  private
    function GetRegistroExiste(FCodigo: Integer): Integer;
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88Ean);
    function GetObject (Index: Integer): TRegistro88Ean;
    procedure Insert (Index: Integer; Obj: TRegistro88Ean);
  public
    function Add (Obj: TRegistro88Ean): Integer;
    property Objects [Index: Integer]: TRegistro88Ean
      read GetObject write SetObject; default;
  end;

  //Registros referentes ao Sapi MG
  TRegistro88SP02 = class
  private
    FDataEstoque   : TDateTime;
    FCodProduto    : String;
    FDescricao     : String;
    FUnidadeMedida : String;
    FQuantidade    : Double;
    FValorUnitario : Double;
    FValorTotal    : Double;
    FCodigoPosse   : String;
    FCnpj          : String;
    FIe            : String;
    FUf            : String;
  public
    property DataEstoque : TDateTime read FDataEstoque write FDataEstoque;
    property CodProduto  : String read FCodProduto write FCodProduto;
    property Descricao   : String read FDescricao write FDescricao;
    property UnidadeMedida : String read FUnidadeMedida write FUnidadeMedida;
    property Quantidade : Double read FQuantidade write FQuantidade;
    property ValorUnitario : Double read FValorUnitario write FValorUnitario;
    property ValorTotal  : Double read FValorTotal write FValorTotal;
    property CodigoPosse : String read FCodigoPosse write FCodigoPosse;
    property Cnpj : String read FCnpj write FCnpj;
    property Ie : String read FIe write FIe;
    property Uf : String read FUf write FUf;
  end;

  {Lista de objetos do tipo Registro88SP02}
  TRegistros88SP02 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88SP02);
    function GetObject (Index: Integer): TRegistro88SP02;
    procedure Insert (Index: Integer; Obj: TRegistro88SP02);
  public
    function Add (Obj: TRegistro88SP02): Integer;
    property Objects [Index: Integer]: TRegistro88SP02
      read GetObject write SetObject; default;
  end;

  TRegistro88SP03 = class
  private
    FCnpj : String;
    FIe : String;
    FDataRecebimento : TDateTime;
    FUf : String;
    FModelo : String;
    FSerie : String;
    FNumero : String;
    FCfop : String;
    FEmitente : String;
    FValorTotal : Double;
    FBaseCalculo : Double;
    FValorIcms  : Double;
    FIsentas    : Double;
    FOutras     : Double;
    FAliquota   : Double;
    FSituacao : String;
    FBeneficioFiscal : String;
    FAliquotaInterna : Double;
    FDataEmissao : TDateTime;
    FMicroEmpresa : String;
  public
    property Cnpj : String read FCnpj write FCnpj;
    property Ie : String read FIe write FIe;
    property DataRecebimento : TDateTime read FDataRecebimento write FDataRecebimento;
    property Uf : String read FUf write FUf;
    property Modelo : String read FModelo write FModelo;
    property Serie : String read FSerie write FSerie;
    property Numero : String read FNumero write FNumero;
    property Cfop : String read FCfop write FCfop;
    property Emitente : String read FEmitente write FEmitente; //P - proprio ou  T - terceiro
    property ValorTotal : Double read FValorTotal write FValorTotal;
    property BaseCalculo : Double read FBaseCalculo write FBaseCalculo;
    property ValorIcms  : Double read FValorIcms write FValorIcms;
    property Isentas    : Double read FIsentas write FIsentas;
    property Outras     : Double read FOutras write FOutras;
    property Aliquota   : Double read FAliquota write FAliquota;
    property Situacao : String read FSituacao write FSituacao;
    property BeneficioFiscal : String read FBeneficioFiscal write FBeneficioFiscal;
    property AliquotaInterna : Double read FAliquotaInterna write FAliquotaInterna;
    property DataEmissao : TDateTime read FDataEmissao write FDataEmissao;
    property MicroEmpresa : String read FMicroEmpresa write FMicroEmpresa;
  end;

 {Lista de objetos do tipo Registro88SP03}
  TRegistros88SP03 = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRegistro88SP03);
    function GetObject (Index: Integer): TRegistro88SP03;
    procedure Insert (Index: Integer; Obj: TRegistro88SP03);
  public
    function Add (Obj: TRegistro88SP03): Integer;
    property Objects [Index: Integer]: TRegistro88SP03
      read GetObject write SetObject; default;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSintegra = class(TACBrComponent)
  private
    FFileName: string;
    FRegistro10: TRegistro10;
    FRegistro11: TRegistro11;
    Arquivo: TextFile;
    FRegistros50: TRegistros50;
    FRegistros54: TRegistros54;
    FRegistros56: TRegistros56;
    FRegistros75: TRegistros75;
    FRegistros70: TRegistros70;
    FRegistros71: TRegistros71;

    FRegistros51: TRegistros51;
    FRegistros53: TRegistros53;
    FAtivo: Boolean;
    FRegistros60M: TRegistros60M;
    FRegistros60A: TRegistros60A;
    FVersaoValidador: TVersaoValidador;
    FRegistros60D: TRegistros60D;
    FRegistros74: TRegistros74;
    FRegistros61: TRegistros61;
    FRegistros61R: TRegistros61R;
    FRegistros60I: TRegistros60I;
    FRegistros60R: TRegistros60R;
    FRegistro88EC: TRegistro88EC;
    FRegistro88SF: TRegistro88SF;
    FRegistros85: TRegistros85;
    FRegistros86: TRegistros86;
    FRegistros55: TRegistros55;
    FRegistros88Ean: TRegistros88Ean;
    FInforma88SMS: Boolean;
    FInforma88SME: Boolean;
    FRegistros76: TRegistros76;
    FRegistros77: TRegistros77;
    FInforma88EAN: Boolean;
    FRegistros88C: TRegistros88C;
    FRegistros88D: TRegistros88D;
    FRegistros88E: TRegistros88E;
    FRegistros88T: TRegistros88T;
    FRegistros88SP02 : TRegistros88SP02;
    FRegistros88SP03 : TRegistros88SP03;
    FRegistros88STES : TRegistros88STES;
    FRegistros88STITNF : TRegistros88STITNF;
    FInforma88C: Boolean;
    FInformaSapiMG : Boolean;

    procedure GeraRegistro10;
    procedure GeraRegistro11;
    procedure GerarRegistros50;
    procedure GerarRegistros51;
    procedure GerarRegistros53;
    procedure GerarRegistros54;
    procedure GerarRegistros55;

    procedure GerarRegistros60M(Registro60M: TRegistro60M);
    procedure GerarRegistros60A(vRegistros60A: TRegistros60A);
    procedure GerarRegistros60D(Registros60D: TRegistros60D);
    procedure GerarRegistros60I(Registros60I: TRegistros60I);
    procedure GerarRegistros60R;

    procedure GerarRegistros61;
    procedure GerarRegistros61R;

    procedure GerarRegistros70;
    procedure GerarRegistros71;
    procedure GerarRegistros74;
    procedure GerarRegistros75;
    procedure GerarRegistros76;
    procedure GerarRegistros77;

    procedure GerarRegistros85;
    procedure GerarRegistros86;
    procedure GerarRegistros88Ean;

    procedure GerarRegistro88SME;
    procedure GerarRegistro88SMS;
    procedure GerarRegistro88STES;
    procedure GerarRegistro88STITNF;

    procedure GerarRegistros90;
    procedure WriteRecord(const Rec: string; vSapiMG : Boolean = false);
    function GetRegistro60M(Emissao: TDateTime; const NumSerie: string): TRegistro60M;
    procedure GerarConjuntoRegistros60;
    function GetVersao: string;
    procedure GerarRegistro88EC;
    procedure GerarRegistro88SF;
    procedure GerarRegistros88C;
    procedure GerarRegistros88D;
    procedure GerarRegistros88E;
    procedure GerarRegistros88T;
    procedure GerarRegistro88SP02;
    procedure GerarRegistro88SP03;
  public
    property Registro10: TRegistro10 read FRegistro10 write FRegistro10;
    property Registro11: TRegistro11 read FRegistro11 write FRegistro11;
    property Registros50: TRegistros50 read FRegistros50 write FRegistros50;
    property Registros51: TRegistros51 read FRegistros51 write FRegistros51;
    property Registros53: TRegistros53 read FRegistros53 write FRegistros53;

    property Registros54: TRegistros54 read FRegistros54 write FRegistros54;
    property Registros55: TRegistros55 read FRegistros55 write FRegistros55;
    property Registros56: TRegistros56 read FRegistros56 write FRegistros56;

    property Registros60M: TRegistros60M read FRegistros60M write FRegistros60M;
    property Registros60A: TRegistros60A read FRegistros60A write FRegistros60A;
    property Registros60D: TRegistros60D read FRegistros60D write FRegistros60D;
    property Registros60I: TRegistros60I read FRegistros60I write FRegistros60I;
    property Registros60R: TRegistros60R read FRegistros60R write FRegistros60R;
    property Registros61: TRegistros61 read FRegistros61 write FRegistros61;
    property Registros61R: TRegistros61R read FRegistros61R write FRegistros61R;
    property Registros70: TRegistros70 read FRegistros70 write FRegistros70;
    property Registros71: TRegistros71 read FRegistros71 write FRegistros71;
    property Registros74: TRegistros74 read FRegistros74 write FRegistros74;
    property Registros75: TRegistros75 read FRegistros75 write FRegistros75;
    property Registros76: TRegistros76 read FRegistros76 write FRegistros76;
    property Registros77: TRegistros77 read FRegistros77 write FRegistros77;

    property Registros85: TRegistros85 read FRegistros85 write FRegistros85;
    property Registros86: TRegistros86 read FRegistros86 write FRegistros86;
    property Registros88Ean: TRegistros88Ean read FRegistros88Ean write FRegistros88Ean;
    property Registro88EC: TRegistro88EC read FRegistro88EC write FRegistro88EC;
    property Registro88SF: TRegistro88SF read FRegistro88SF write FRegistro88SF;
    property Registros88C: TRegistros88C read FRegistros88C write FRegistros88C;
    property Registros88D: TRegistros88D read FRegistros88D write FRegistros88D;
    property Registros88E: TRegistros88E read FRegistros88E write FRegistros88E;
    property Registros88T: TRegistros88T read FRegistros88T write FRegistros88T;
    property Registros88SP02: TRegistros88SP02 read FRegistros88SP02 write FRegistros88SP02;
    property Registros88SP03: TRegistros88SP03 read FRegistros88SP03 write FRegistros88SP03;
    property Registros88STES: TRegistros88STES read FRegistros88STES write FRegistros88STES;
    property Registros88STITNF: TRegistros88STITNF read FRegistros88STITNF write FRegistros88STITNF;
    property Ativo: Boolean read FAtivo write FAtivo;
    procedure LimparRegistros;
    procedure GeraArquivo;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName: string read FFileName write FFileName;
    property VersaoValidador: TVersaoValidador read FVersaoValidador write
      FVersaoValidador;
    property Versao: string read GetVersao;
    property Informa88SME: Boolean read FInforma88SME write FInforma88SME;
    property Informa88SMS: Boolean read FInforma88SMS write FInforma88SMS;
    property Informa88EAN: Boolean read FInforma88EAN write FInforma88EAN;
    property Informa88C: Boolean read FInforma88C write FInforma88C;
    property InformaSapiMG : Boolean read FInformaSapiMG write FInformaSapiMG;
  end;

  function Sort50(Item1: Pointer;Item2: Pointer): Integer;
  function Sort51(Item1: Pointer;Item2: Pointer): Integer;
  function Sort53(Item1: Pointer;Item2: Pointer): Integer;
  function Sort54(Item1: Pointer;Item2: Pointer): Integer;
  function Sort56(Item1: Pointer;Item2: Pointer): Integer;
  function Sort60M(Item1: Pointer;Item2: Pointer): Integer;
  function Sort60A(Item1: Pointer;Item2: Pointer): Integer;
  function Sort60D(Item1: Pointer;Item2: Pointer): Integer;
  function Sort60I(Item1: Pointer;Item2: Pointer): Integer;
  function Sort70(Item1: Pointer;Item2: Pointer): Integer;
  function Sort71(Item1: Pointer;Item2: Pointer): Integer;
  function Sort74(Item1: Pointer;Item2: Pointer): Integer;
  function Sort75(Item1: Pointer;Item2: Pointer): Integer;
  function Sort85(Item1: Pointer;Item2: Pointer): Integer;
  function Sort86(Item1: Pointer;Item2: Pointer): Integer;

implementation

uses ACBrUtil.Strings,
     ACBrUtil.Base;

{ TACBrSintegra }

constructor TACBrSintegra.Create(AOwner: TComponent);
begin
  inherited;
  FRegistro10:=TRegistro10.Create;
  FRegistro11:=TRegistro11.Create;
  FRegistros50:=TRegistros50.Create(True);
  FRegistros51:=TRegistros51.Create(True);
  FRegistros53:=TRegistros53.Create(True);
  FRegistros54:=TRegistros54.Create(True);
  FRegistros55:=TRegistros55.Create(True);
  FRegistros56:=TRegistros56.Create(True);
  FRegistros60M:=TRegistros60M.Create(True);

  //Os registros abaixo são criados com AOwnsObject = False.
  // Seus itens então precisarão ser destruídos manualmente.
  // Veja: http://stackoverflow.com/questions/10410019/use-of-tlist-and-tobjectlist
  // AOwnsObject é False para que, ao gerar um arquivo da FVersaoValidador = vv574,
  //  o TACBrSintegra possa jogar os itens das propriedade Registros60M 60D 60I
  //  dentro do Registro60M nas propriedades equivalentes,
  //  (a saber FRegs60A, FRegs60D e FRegs60I),  e não cause Access Violation
  //  ao tentar destruir os registros novamente;
  FRegistros60A:=TRegistros60A.Create(False);
  FRegistros60D:=TRegistros60D.Create(False);
  FRegistros60I:=TRegistros60I.Create(False);

  FRegistros60R:=TRegistros60R.Create(True);

  FRegistros61:=TRegistros61.Create(True);
  FRegistros61R:=TRegistros61R.Create(True);
  FRegistros70:=TRegistros70.Create(True);
  FRegistros71:=TRegistros71.Create(True);
  FRegistros74:=TRegistros74.Create(True);
  FRegistros75:=TRegistros75.Create(True);
  FRegistros76:=TRegistros76.Create(True);
  FRegistros77:=TRegistros77.Create(True);
  FRegistros85:=TRegistros85.Create(True);
  FRegistros86:=TRegistros86.Create(True);
  FRegistros88Ean:=TRegistros88Ean.Create(True);
  FRegistro88EC:=TRegistro88EC.Create;
  FRegistro88SF:=TRegistro88SF.Create;
  FRegistros88C:=TRegistros88C.Create;
  FRegistros88D:=TRegistros88D.Create;
  FRegistros88E:=TRegistros88E.Create;
  FRegistros88T:=TRegistros88T.Create;
  FRegistros88SP02 := TRegistros88SP02.Create(True);
  FRegistros88SP03 := TRegistros88SP03.Create(True);
  FRegistros88STES:=TRegistros88STES.Create;
  FRegistros88STITNF:=TRegistros88STITNF.Create;
  FVersaoValidador:=vv524;
  Ativo:=True;
end;

destructor TACBrSintegra.Destroy;
begin
  LimparRegistros;
  FRegistro10.Free;
  FRegistro11.Free;
  FRegistros50.Free;
  FRegistros51.Free;
  FRegistros53.Free;
  FRegistros54.Free;
  FRegistros55.Free;
  FRegistros56.Free;
  FRegistros60M.Free;
  FRegistros60A.Free;
  FRegistros60D.Free;
  FRegistros60I.Free;
  FRegistros60R.Free;
  FRegistros61.Free;
  FRegistros61R.Free;
  FRegistros70.Free;
  FRegistros71.Free;
  FRegistros74.Free;
  FRegistros75.Free;
  FRegistros76.Free;
  FRegistros77.Free;
  FRegistros85.Free;
  FRegistros86.Free;
  FRegistros88Ean.Free;
  FRegistro88EC.Free;
  FRegistro88SF.Free;
  FRegistros88C.Free;
  FRegistros88D.Free;
  FRegistros88STES.Free;
  FRegistros88STITNF.Free;
  FRegistros88E.Free;
  FRegistros88T.Free;
  FRegistros88SP02.Free;
  FRegistros88SP03.Free;
  Ativo:=False;
  inherited;
end;

procedure TACBrSintegra.WriteRecord(const Rec: string; vSapiMG : Boolean);
begin
  //adicionado variavel para checar a geração dos registro para o SAPI MG
  //para não ocorrer problemas com a geração dos outros arquivos somente informo True nesta
  //variavel para os registros SP02 e SP03
  if not vSapiMG then
  begin
    if Length(Rec)<>126 then
      raise Exception.Create(ACBrStr('Registro inválido!'+#13+
        'Deve conter 126 posições. '+#13+
        'Registro: '+Rec+#13+
        'possui '+IntToStr(Length(Rec))+' posições.'));
    // mudança compatibilidade linux
  end;
  write(Arquivo, Rec + #13+#10 );
end;

procedure TACBrSintegra.GeraArquivo;
begin
  if Trim(FileName)='' then
    raise Exception.Create(ACBrStr('Informe um nome de arquivo!'));
  AssignFile(Arquivo,FileName);
  Rewrite(Arquivo);
  try
    GeraRegistro10;
    GeraRegistro11;

    GerarRegistros50;
    GerarRegistros51;
    GerarRegistros53;
    GerarRegistros54;
    GerarRegistros55;
    GerarConjuntoRegistros60;
    GerarRegistros60R;
    GerarRegistros61;
    GerarRegistros61R;
    GerarRegistros70;
    GerarRegistros71;
    GerarRegistros74;
    GerarRegistros75;
    GerarRegistros76;
    GerarRegistros77;
    GerarRegistros85;
    GerarRegistros86;
    //registros 88EAN
    if FInforma88EAN then
      GerarRegistros88Ean;

    if Trim(Registro88EC.NomeContabilista) <> '' then
      GerarRegistro88EC;
    if Trim(Registro88SF.NomeEmpresa) <> '' then
      GerarRegistro88SF;

    //registros 88SM
    if FInforma88SME then
      GerarRegistro88SME;

    if FInforma88SMS then
      GerarRegistro88SMS;

    if FInforma88C then begin
      GerarRegistros88C;
      GerarRegistros88D;
      GerarRegistros88E;
      GerarRegistros88T;
    end;

    if FInformaSapiMG then
    begin
      GerarRegistro88SP02;
      GerarRegistro88SP03;
    end;
    GerarRegistro88STES;
    GerarRegistro88STITNF;
    GerarRegistros90;
  finally
    CloseFile(Arquivo);
    LimparRegistros;
  end;
end;

procedure TACBrSintegra.GerarConjuntoRegistros60;
var
  i: Integer;
  wregistro60M: TRegistro60M;
begin
//associo todos os registros 60A/D/I criados a um registro 60M existente
//com esta abordagem permite-se que o usuario adicione registros fora
//de ordem na aplicacao cliente, ex: adiciona analitico primeiramente ao master.
//Com isto não há obrigacao de amarrar analitico ao master..deixando por conta do componente
//nao sei dizer se este processo de ordenacao efetuado pode tornar-se lento

if FVersaoValidador=vv524 then
begin
  //60A
  for i:=0 to Registros60A.Count - 1 do
  begin
    wregistro60M:=GetRegistro60M(Registros60A[i].Emissao,Registros60A[i].NumSerie);
    if not Assigned(wregistro60M) then
      raise Exception.Create(ACBrStr('Registro 60A sem registro 60M correspondente!'+#13+
        DateToStr(Registros60A[i].Emissao)+' - '+Registros60A[i].NumSerie));
    wregistro60M.Regs60A.Add(Registros60A[i]);
  end;

  //60D
  for i:=0 to Registros60D.Count - 1 do
  begin
    wregistro60M:=GetRegistro60M(Registros60D[i].Emissao,Registros60D[i].NumSerie);
    if not Assigned(wregistro60M) then
      raise Exception.Create(ACBrStr('Registro 60D sem registro 60M correspondente!'+#13+
        DateToStr(Registros60D[i].Emissao)+' - '+Registros60D[i].NumSerie));
    wregistro60M.Regs60D.Add(Registros60D[i]);
  end;

  //60I
  for i:=0 to Registros60I.Count - 1 do
  begin
    wregistro60M:=GetRegistro60M(Registros60I[i].Emissao,Registros60I[i].NumSerie);
    if not Assigned(wregistro60M) then
      raise Exception.Create(ACBrStr('Registro 60I sem registro 60M correspondente!'+#13+
        DateToStr(Registros60I[i].Emissao)+' - '+Registros60I[i].NumSerie));
    wregistro60M.Regs60I.Add(Registros60I[i]);
  end;

  //ordenando e gerando de acordo com o manual...
  Registros60M.Sort(Sort60M);
  for i:=0 to Registros60M.Count-1 do
  begin
    //removido pois sintegra tem ordem propria e nao consegui sortear os
    //objetos na lista de objetos com mais de um campo

    //    Registros60M[i].Regs60A.Sort(Sort60A);
    //    Registros60M[i].Regs60D.Sort(Sort60D);
    //    Registros60M[i].Regs60I.Sort(Sort60I);

    GerarRegistros60M(Registros60M[i]);
    GerarRegistros60A(Registros60M[i].Regs60A);
    GerarRegistros60D(Registros60M[i].Regs60D);
    GerarRegistros60I(Registros60M[i].Regs60I);
  end;
end
else if FVersaoValidador=vv523 then
begin
  //ordenando e gerando de acordo com o manual...
  Registros60M.Sort(Sort60M);
  for i:=0 to Registros60M.Count-1 do
    GerarRegistros60M(Registros60M[i]);
//  Registros60A.Sort(Sort60A);
  GerarRegistros60A(Registros60A);
//  Registros60D.Sort(Sort60D);
  GerarRegistros60D(Registros60D);
//  Registros60I.Sort(Sort60I);
  GerarRegistros60I(Registros60I);
end;
end;

procedure TACBrSintegra.GeraRegistro10;
var
  wregistro: string;
begin
wregistro:='10'+TBStrZero(TiraPontos(Registro10.CNPJ),14);
wregistro:=wregistro+PadRight(Trim(TiraPontos(Registro10.Inscricao)),14);
wregistro:=wregistro+PadRight(Copy(Registro10.RazaoSocial,1,35),35);
wregistro:=wregistro+PadRight(Copy(Registro10.Cidade,1,30),30);
wregistro:=wregistro+PadRight(Registro10.Estado,2);
wregistro:=wregistro+PadRight(TBStrZero(TiraPontos(Registro10.Telefone),10),10);
wregistro:=wregistro+FormatDateTime('yyyymmdd',Registro10.DataInicial);
wregistro:=wregistro+FormatDateTime('yyyymmdd',Registro10.DataFinal);
wregistro:=wregistro+PadRight(Registro10.CodigoConvenio,1);
wregistro:=wregistro+PadRight(Registro10.NaturezaInformacoes,1);
wregistro:=wregistro+PadRight(Registro10.FinalidadeArquivo,1);
WriteRecord(wregistro);
end;

procedure TACBrSintegra.GeraRegistro11;
var
  wregistro: string;
begin
wregistro:='11';
wregistro:=wregistro+PadRight(Copy(Registro11.Endereco,1,34),34);
wregistro:=wregistro+TBStrZero(TiraPontos(Registro11.Numero),5);
wregistro:=wregistro+PadRight(Copy(Registro11.Complemento,1,22),22);
wregistro:=wregistro+PadRight(Copy(Registro11.Bairro,1,15),15);
wregistro:=wregistro+TBStrZero(TiraPontos(Registro11.Cep),8);
wregistro:=wregistro+PadRight(Copy(Registro11.Responsavel,1,28),28);
wregistro:=wregistro+TBStrZero(TiraPontos(Registro11.Telefone),12);
WriteRecord(wregistro);
end;

procedure TACBrSintegra.GerarRegistros50;
var
  wregistro: string;
  i: Integer;
begin
//Registro 50 requer ordenacao por data do documento
//Registros50.Sort(Sort50); //removido temporariamente por questoes de erro de soma no validador
for i:=0 to Registros50.Count-1 do
begin
  with Registros50[i] do
  begin
    wregistro:='50';
    wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14)+PadRight(TiraPontos(Inscricao),14);
    wregistro:=wregistro+FormatDateTime('yyyymmdd',DataDocumento);
    wregistro:=wregistro+PadRight(UF,2);
    wregistro:=wregistro+TBStrZero(Modelo,2);
    wregistro:=wregistro+PadRight(Serie,3);

    wregistro:=wregistro+TBStrZero(RightStr(Numero,6),6);

    wregistro:=wregistro+PadRight(TiraPontos(Cfop),4);
    wregistro:=wregistro+PadRight(EmissorDocumento,1);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorContabil)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',BasedeCalculo)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Icms)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Isentas)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Outras)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Aliquota)),4);
    wregistro:=wregistro+PadRight(Situacao,1);
    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros51;
var
  wregistro: string;
  i: Integer;
begin
Registros51.Sort(Sort51);
for i := 0 to Registros51.Count-1 do
begin
  with Registros51[i] do
  begin
    wregistro := '51';
    wregistro := wregistro+TBStrZero(TiraPontos(CPFCNPJ),14) + PadRight(TiraPontos(Inscricao),14);
    wregistro := wregistro+FormatDateTime('yyyymmdd',DataDocumento);
    wregistro := wregistro+PadRight(Estado,2);
    wregistro := wregistro+PadRight(Serie,3);
    wregistro := wregistro+TBStrZero(RightStr(Numero,6),6);
    wregistro := wregistro+PadRight(TiraPontos(Cfop),4);
    wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorContabil)),13);
    wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorIpi)),13);
    wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorIsentas)),13);
    wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorOutras)),13);
    wregistro := wregistro+Space(20);
    wregistro := wregistro+PadRight(Situacao,1);
    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros53;
var
  wregistro: string;
  i: Integer;
begin

//Registros53.Sort(Sort53);//removido temporariamente por questoes de erro de soma no validador. Provavelmente mesmo erro do Registro 50 (Veja GerarRegistros50)
for i:=0 to Registros53.Count-1 do
begin
  with Registros53[i] do
  begin
    wregistro:='53';
    wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14)+
      PadRight(TiraPontos(Inscricao),14);
    wregistro:=wregistro+FormatDateTime('yyyymmdd',DataDocumento);
    wregistro:=wregistro+PadRight(Estado,2);
    wregistro:=wregistro+TBStrZero(Modelo,2);
    wregistro:=wregistro+PadRight(Trim(Serie),3);
    wregistro:=wregistro+TBStrZero(RightStr(Numero,6),6);
    wregistro:=wregistro+PadRight(TiraPontos(CFOP),4);
    wregistro:=wregistro+PadRight(Emitente,1);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',BaseST)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',IcmsRetido)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Despesas)),13);
    wregistro:=wregistro+PadRight(Situacao,1);
    wregistro:=wregistro+PadRight(CodigoAntecipacao,1);
    wregistro:=wregistro+Space(29);
    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros54;
var
  LRegistro: string;
  i: Integer;
  a:integer;
begin
  Registros54.Sort(Sort54);
  for i := 0 to Registros54.Count - 1 do
  begin
    LRegistro := '54';
    LRegistro := LRegistro + TBStrZero(TiraPontos(Registros54[ i ].CPFCNPJ), 14);
    LRegistro := LRegistro + TBStrZero(Registros54[ i ].Modelo, 2);
    LRegistro := LRegistro + PadRight(Registros54[ i ].Serie, 3);
    LRegistro := LRegistro + TBStrZero(RightStr(Registros54[ i ].Numero, 6), 6);

    LRegistro := LRegistro + PadRight(TiraPontos(Registros54[ i ].CFOP), 4);
    LRegistro := LRegistro + PadRight(Registros54[ i ].CST, 3);
    LRegistro := LRegistro + IntToStrZero(Registros54[ i ].NumeroItem, 3);
    if Registros54[ i ].NumeroItem <= 990 then
      LRegistro := LRegistro + PadRight(Registros54[ i ].Codigo, 14) //codigo do produto
    else
      LRegistro := LRegistro + Space(14);

    LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,###0.000', Registros54[ i ].Quantidade)), 11); //quantidade do produto
    LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00', Registros54[ i ].Valor)), 12); //total do produto
    LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00', Registros54[ i ].ValorDescontoDespesa)), 12); //desconto/despesa
    LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00', Registros54[ i ].BasedeCalculo)), 12);
    LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00', Registros54[ i ].BaseST)), 12); //base de calculo substituicao tributária
    LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00', Registros54[ i ].ValorIpi)), 12); //valor do ipi.
    LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00', Registros54[ i ].Aliquota)), 4);
    WriteRecord(LRegistro);
    if Registros56.Count > 0 then
    begin
      Registros56.Sort(Sort56);
      for a := 0 to Registros56.Count - 1 do
      begin
        if (Registros56[ a ].CFOP = Registros54[ i ].CFOP) and (Registros56[ a ].Modelo = Registros54[ i ].Modelo) and (Registros56[ a ].Serie = Registros54[ i ].Serie) and
          (Registros56[ a ].Numero = Registros54[ i ].Numero) and (Registros56[ a ].NumeroItem = Registros54[ i ].NumeroItem) and (Registros56[ a ].CFOP = Registros54[ i ].CFOP)
        then
        begin
          LRegistro := '56';
          LRegistro := LRegistro + TBStrZero(TiraPontos(Registros56[ a ].CNPJ), 14);
          LRegistro := LRegistro + TBStrZero(Registros56[ a ].Modelo, 2);
          LRegistro := LRegistro + PadRight(Registros56[ a ].Serie, 3);
          LRegistro := LRegistro + TBStrZero(Registros56[ a ].Numero, 6);
          LRegistro := LRegistro + PadRight(TiraPontos(Registros56[ a ].CFOP), 4);
          LRegistro := LRegistro + PadRight(Registros56[ a ].CST, 3);
          LRegistro := LRegistro + IntToStrZero(Registros56[ a ].NumeroItem, 3);
          LRegistro := LRegistro + PadRight(Registros56[ a ].Codigo, 14); //codigo do produto
          LRegistro := LRegistro + PadRight(Registros56[ a ].TipoOperacao, 1); //Tipo de operacao
          LRegistro := LRegistro + TBStrZero(TiraPontos(Registros56[ a ].CnpjConcessionaria), 14);
          LRegistro := LRegistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00', Registros56[ a ].Ipi)), 4);
          LRegistro := LRegistro + TBStrZero(TiraPontos(Registros56[ a ].Chassi), 17);
          LRegistro := LRegistro + Space(39);
          WriteRecord(LRegistro);
        end;
      end;
    end;
  end;
end;

procedure TACBrSintegra.GerarRegistros60A(vRegistros60A: TRegistros60A);
var
  wregistro: string;
  i: Integer;
begin
for i:=0 to vRegistros60A.Count -1 do
begin
  wregistro:='60A';
  wregistro:=wregistro+FormatDateTime('yyyymmdd',vRegistros60A[i].Emissao);
  wregistro:=wregistro+PadRight(Trim(vRegistros60A[i].NumSerie),20);
  wregistro:=wregistro+PadRight(TiraPontos(vRegistros60A[i].StAliquota),4);
  wregistro:=wregistro+TbStrZero(TiraPontos(
    FormatFloat('#,##0.00',vRegistros60A[i].Valor)),12);
  wregistro:=wregistro+space(79);
  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistros60D(Registros60D: TRegistros60D);
var
  wregistro: string;
  i: Integer;
begin
for I := 0 to Registros60D.Count - 1 do
begin
  wregistro:='60D';
  wregistro:=wregistro+FormatDateTime('yyyymmdd',Registros60D[i].Emissao);
  wregistro:=wregistro+PadRight(Trim(Registros60D[i].NumSerie),20);
  wregistro:=wregistro+PadRight(Registros60D[i].Codigo,14);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,###0.000',Registros60D[i].Quantidade)),13);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,##0.00',Registros60D[i].Valor)),16);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,##0.00',Registros60D[i].BaseDeCalculo)),16);
  wregistro:=wregistro+PadRight(TiraPontos(Registros60D[i].StAliquota),4);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,##0.00',Registros60D[i].ValorIcms)),13);
  wregistro:=wregistro+Space(19);
  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistros60M(Registro60M: TRegistro60M);
var
  wregistro: string;
begin
with Registro60M do
begin
  wregistro:='60M';
  wregistro:=wregistro+FormatDateTime('yyyymmdd',Emissao);
  wregistro:=wregistro+PadRight(Trim(NumSerie),20);
  wregistro:=wregistro+TBStrZero(IntToStr(NumOrdem),3);
  wregistro:=wregistro+ModeloDoc;
  wregistro:=wregistro+TBStrZero(IntToStr(CooInicial),6);
  wregistro:=wregistro+TBStrZero(IntToStr(CooFinal),6);
  wregistro:=wregistro+TBStrZero(IntToStr(CRZ),6);
  wregistro:=wregistro+TBStrZero(IntToStr(CRO),3);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',VendaBruta)),16);
  wregistro:=wregistro+TBStrZero(tirapontos(FormatFloat('#,##0.00',ValorGT)),16);
  wregistro:=wregistro+space(37);
  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistros70;
var
  i: Integer;
  wregistro: string;
begin
Registros70.Sort(Sort70);
for i:=0 to Registros70.Count - 1 do
begin
  wregistro:='70';
  wregistro:=wregistro+TBStrZero(TiraPontos(Registros70[i].CPFCNPJ),14)+
    PadRight(TiraPontos(Registros70[i].Inscricao),14);
  wregistro:=wregistro+FormatDateTime('yyyymmdd',Registros70[i].DataDocumento);
  wregistro:=wregistro+PadRight(Registros70[i].UF,2);
  wregistro:=wregistro+TBStrZero(Registros70[i].Modelo,2);
  wregistro:=wregistro+PadRight(Copy(Registros70[i].Serie,1,1),1);
  wregistro:=wregistro+PadRight(Copy(Registros70[i].SubSerie,1,2),2);
  wregistro:=wregistro+TBStrZero(RightStr(Registros70[i].Numero,6),6);
  wregistro:=wregistro+PadRight(TiraPontos(Registros70[i].Cfop),4);

  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Registros70[i].ValorContabil)),13);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Registros70[i].BasedeCalculo)),14);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Registros70[i].Icms)),14);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Registros70[i].Isentas)),14);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Registros70[i].Outras)),14);
  wregistro:=wregistro+TBStrZero(Registros70[i].CifFobOutros,1);
  wregistro:=wregistro+PadRight(Registros70[i].Situacao,1);
  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistros71;
var
  i: Integer;
  wregistro: string;
begin
Registros71.Sort(Sort71);
for i:=0 to Registros71.Count - 1 do
begin
  wregistro:='71';
  wregistro:=wregistro+TBStrZero(TiraPontos(Registros71[i].CPFCNPJ),14)+
                            PadRight(TiraPontos(Registros71[i].Inscricao),14);

  wregistro:=wregistro+FormatDateTime('yyyymmdd',Registros71[i].DataDocumento);
  wregistro:=wregistro+PadRight(Registros71[i].UF,2);
  wregistro:=wregistro+TBStrZero(Registros71[i].Modelo,2);
  wregistro:=wregistro+PadRight(Copy(Registros71[i].Serie,1,1),1);
  wregistro:=wregistro+PadRight(Copy(Registros71[i].SubSerie,1,2),2);
  wregistro:=wregistro+TBStrZero(RightStr(Registros71[i].Numero,6),6);

  wregistro:=wregistro+PadRight(Registros71[i].UFNF,2);
  wregistro:=wregistro+TBStrZero(TiraPontos(Registros71[i].CPFCNPJNF),14)+
                            PadRight(TiraPontos(Registros71[i].InscricaoNF),14);
  wregistro:=wregistro+FormatDateTime('yyyymmdd',Registros71[i].DataNF);
  wregistro:=wregistro+TBStrZero(Registros71[i].ModeloNF,2);
  wregistro:=wregistro+PadRight(Copy(Registros71[i].SerieNF,1,3),3);
  wregistro:=wregistro+TBStrZero(Registros71[i].NumeroNF,6);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Registros71[i].ValorNF)),14);
  wregistro:=wregistro+PadRight(' ',12);

  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistros74;
var
  i: Integer;
  wregistro: string;
begin
//Registros74.Sort(Sort74);
for i:=0 to Registros74.Count - 1 do
begin
  with Registros74[i] do
  begin
    wregistro:='74';
    wregistro:=wregistro+FormatDateTime('yyyymmdd',Data);
    wregistro:=wregistro+PadRight(Codigo,14);
    wregistro:=wregistro+TBStrZero(TiraPontos(
      FormatFloat('#,###0.000',Quantidade)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(
      FormatFloat('#,##0.00',ValorProduto)),13);
    wregistro:=wregistro+CodigoPosse;
    wregistro:=wregistro+TBStrZero(CNPJPossuidor,14);
    wregistro:=wregistro+PadRight(InscricaoPossuidor,14);
    wregistro:=wregistro+PadRight(UFPossuidor,2);
    wregistro:=wregistro+Space(45); //brancos
    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros75;
var
  i: Integer;
  wregistro: string;
begin
Registros75.Sort(Sort75);
for i := 0 to Registros75.Count - 1 do
begin
  wregistro:='75'+FormatDateTime('yyyymmdd',Registros75[i].DataInicial)+
    FormatDateTime('yyyymmdd',Registros75[i].DataFinal);
  wregistro:=wregistro+PadRight(Registros75[i].Codigo,14);
  wregistro:=wregistro+PadRight(Registros75[i].NCM,8);
  wregistro:=wregistro+PadRight(Copy(Trim(Registros75[i].Descricao),1,53),53);
  wregistro:=wregistro+PadRight(Registros75[i].Unidade,6);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
    Registros75[i].AliquotaIpi)),5);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
    Registros75[i].AliquotaIcms)),4);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
    Registros75[i].Reducao)),5);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
    Registros75[i].BaseST)),13);
  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistros85;
var
  wregistro: string;
  i: Integer;
begin
    for i:=0 to Registros85.Count-1 do
    begin
        wregistro:='85';
        wregistro:=wregistro+TBStrZero(TiraPontos(Registros85[i].Declaracao),11);
        wregistro:= wregistro + FormatDateTime('yyyymmdd',Registros85[i].DataDeclaracao);
        wregistro:=wregistro+PadRight(Registros85[i].NaturezaExportacao,1);
        wregistro:=wregistro+PadRight(Registros85[i].RegistroExportacao,12);
        if Registros85[i].DataRegistro > 0 then
          wregistro:= wregistro + FormatDateTime('yyyymmdd',Registros85[i].DataRegistro)
        else
          wregistro:= wregistro + '00000000';

        wregistro:=wregistro+PadRight((Registros85[i].Conhecimento),16);
        if Registros85[i].DataConhecimento > 0 then
          wregistro:= wregistro + FormatDateTime('yyyymmdd',Registros85[i].DataConhecimento)
        else
          wregistro:= wregistro + '00000000';
        wregistro:=wregistro+TBStrZero(TiraPontos(Registros85[i].TipoConhecimento),2);
        wregistro:=wregistro+TBStrZero(TiraPontos(Registros85[i].Pais),4);
        wregistro:= wregistro + PadRight('',8, '0');
        wregistro:= wregistro + FormatDateTime('yyyymmdd',Registros85[i].DataAverbacao);
        wregistro:=wregistro+TBStrZero(Registros85[i].NumeroNotaFiscal,6);
        wregistro:= wregistro + FormatDateTime('yyyymmdd',Registros85[i].DataNotaFiscal);
        wregistro:=wregistro+TBStrZero(Registros85[i].Modelo,2);
        wregistro:=wregistro+PadRight(Copy(Registros85[i].Serie,1,3),3);
        wregistro:=wregistro+PadRight('',19);
        WriteRecord(wregistro);
    end;
end;

procedure TACBrSintegra.GerarRegistros86;
var
  wregistro: string;
  i: Integer;
begin
    //Registro 86 requer ordenacao por Declaracao
    Registros86.Sort(Sort86);
    for i:=0 to Registros86.Count-1 do
    begin
        with Registros86[i] do
        begin
            wregistro:='86';
            wregistro:=wregistro+PadRight(Registros86[i].RegistroExportacao,12);
            wregistro:=wregistro+FormatDateTime('yyyymmdd',DataRegistro);
            wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14)+PadRight(TiraPontos(Inscricao),14);
            wregistro:=wregistro+PadRight(UF,2);
            wregistro:=wregistro+TBStrZero(NumeroNotaFiscal,6);
            wregistro:=wregistro+FormatDateTime('yyyymmdd',DataDocumento);
            wregistro:=wregistro+TBStrZero(Modelo,2);
            wregistro:=wregistro+PadRight(Serie,3);
            wregistro:=wregistro+PadRight(Codigo,14);
            wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.000',Quantidade)),11);
            wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorUnitario)),12);
            wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorTotalProduto)),12);
            wregistro:=wregistro+PadRight(Relacionamento,1);
            wregistro:= wregistro + PadRight('',5);
            WriteRecord(wregistro);
        end;
    end;
end;

procedure TACBrSintegra.GerarRegistros90;
var
  wregistro: string;
  WTotal60A, WTotal60D, WTotal60I : Integer;
  wtotal88: Integer;
  WTotal90: integer;
  WSeque90: integer;
  I: Integer;
begin

wregistro:='90'+TBStrZero(TiraPontos(Registro10.CNPJ),14);
wregistro:=wregistro+PadRight(TiraPontos(Registro10.Inscricao),14);
if Registros50.Count>0 then
  wregistro:=wregistro+'50'+TBStrZero(IntToStr(Registros50.Count),8);
if Registros51.Count>0 then
  wregistro:=wregistro+'51'+TBStrZero(IntToStr(Registros51.Count),8);
if Registros53.Count>0 then
  wregistro:=wregistro+'53'+TBStrZero(IntToStr(Registros53.Count),8);
if Registros54.Count>0 then
  wregistro:=wregistro+'54'+TBStrZero(IntToStr(Registros54.Count),8);
if Registros55.Count>0 then
  wregistro:=wregistro+'55'+TBStrZero(IntToStr(Registros55.Count),8);
if Registros56.Count>0 then
  wregistro:=wregistro+'56'+TBStrZero(IntToStr(Registros56.Count),8);

  if FVersaoValidador = vv524 then
  begin
    WTotal60A := 0;
    WTotal60D := 0;
    WTotal60I := 0;
    for I := 0 to (Registros60M.Count -1) do
    begin
      WTotal60A := WTotal60A + Registros60M[I].Regs60A.Count;
      WTotal60D := WTotal60D + Registros60M[I].Regs60D.Count;
      WTotal60I := WTotal60I + Registros60M[I].Regs60I.Count;
    end;
  end
  else
  begin
    WTotal60A := Registros60A.Count;
    WTotal60D := Registros60D.Count;
    WTotal60I := Registros60I.Count;
  end;

if Registros60M.Count>0 then
  wregistro:=wregistro+'60'+TBStrZero(IntToStr(Registros60M.Count+WTotal60A+
    WTotal60D+WTotal60I+Registros60R.Count),8);

if Registros61.Count>0 then
  wregistro:=wregistro+'61'+TBStrZero(IntToStr(Registros61.Count+Registros61R.Count),8);
if Registros70.Count>0 then
  wregistro:=wregistro+'70'+TBStrZero(IntToStr(Registros70.Count),8);
if Registros71.Count>0 then
  wregistro:=wregistro+'71'+TBStrZero(IntToStr(Registros71.Count),8);
if Registros74.Count>0 then
  wregistro:=wregistro+'74'+TBStrZero(IntToStr(Registros74.Count),8);
if Registros75.Count>0 then
  wregistro:=wregistro+'75'+TBStrZero(IntToStr(Registros75.Count),8);
if Registros76.Count>0 then
  wregistro:=wregistro+'76'+TBStrZero(IntToStr(Registros76.Count),8);
if Registros77.Count>0 then
  wregistro:=wregistro+'77'+TBStrZero(IntToStr(Registros77.Count),8);

if Registros85.Count>0 then
  wregistro:=wregistro+'85'+TBStrZero(IntToStr(Registros85.Count),8);
if Registros86.Count>0 then
  wregistro:=wregistro+'86'+TBStrZero(IntToStr(Registros86.Count),8);

//totalizador para registros 88
wtotal88:=0;
if FInforma88EAN then
  wtotal88:=wtotal88+Registros88Ean.Count;

if Trim(Registro88EC.CRCContabilista) <> '' then
  Inc(wtotal88);
if Trim(Registro88SF.FCNPJEmpresa) <> '' then
  Inc(wtotal88);

if FInforma88SME then
  inc(wtotal88);

if FInforma88SMS then
  inc(wtotal88);

if FRegistros88STES.Count > 0 then
  wtotal88 := wtotal88 + FRegistros88STES.Count;

if FRegistros88STITNF.Count > 0 then
  wtotal88 := wtotal88 + FRegistros88STITNF.Count;

if FInforma88C then begin
  wtotal88:=wtotal88+Registros88C.Count;
  wtotal88:=wtotal88+Registros88D.Count;
  wtotal88:=wtotal88+Registros88E.Count;
  wtotal88:=wtotal88+Registros88T.Count;
end;

if FInformaSapiMG then
begin
  wtotal88:=wtotal88+Registros88SP02.Count;
  wtotal88:=wtotal88+Registros88SP03.Count;
end;

if wtotal88>0 then
  wregistro:=wregistro+'88'+TBStrZero(IntToStr(wtotal88),8);
wtotal90:=3;
WSeque90:=1;
if length(wregistro)>30 then begin
   wregistro:=wregistro+Space(125-length(wregistro))+inttostr(wseque90+1);
   WriteRecord(wregistro);
   inc(wtotal90);
   inc(WSeque90);
end;

wregistro:='90'+TBStrZero(TiraPontos(Registro10.CNPJ),14);
wregistro:=wregistro+PadRight(TiraPontos(Registro10.Inscricao),14);
wregistro:=wregistro+'99'+TBStrZero(IntToStr(Registros50.Count+Registros51.Count+
  Registros53.Count+Registros54.Count+Registros55.Count+registros56.count+
  (Registros60M.Count+WTotal60A+WTotal60D+WTotal60I+Registros60R.Count)+
  (Registros61.Count+Registros61R.Count)+Registros70.Count+Registros71.Count+
  Registros74.Count+Registros75.Count+Registros76.Count+Registros77.Count+
  Registros85.Count+Registros86.Count+wtotal88+wtotal90),8);

wregistro:=wregistro+Space(125-length(wregistro))+inttostr(wseque90);
WriteRecord(wregistro);
end;

function TACBrSintegra.GetRegistro60M(Emissao: TDateTime;
  const NumSerie: string): TRegistro60M;
var
  i: Integer;
begin
Result:=nil;
for i := 0 to Registros60M.Count - 1 do
begin
  if (Registros60M[i].Emissao=Emissao)
     and (Registros60M[i].NumSerie=NumSerie) then
  begin
    Result:=Registros60M[i];
    Break;
  end;
end;
end;

procedure TACBrSintegra.LimparRegistros;
begin
FRegistros50.Clear;
FRegistros51.Clear;
FRegistros53.Clear;
FRegistros54.Clear;
FRegistros55.Clear;
FRegistros56.Clear;
FRegistros60M.Clear;

if FVersaoValidador = vv523 then //Caso esteja usando vv523, destruir os componentes
  FRegistros60A.OwnsObjects := True;
FRegistros60A.Clear;
if FVersaoValidador = vv523 then
  FRegistros60D.OwnsObjects := True;
FRegistros60D.Clear;
if FVersaoValidador = vv523 then
  FRegistros60I.OwnsObjects := True;
FRegistros60I.Clear;
if FVersaoValidador = vv523 then
  FRegistros60R.OwnsObjects := True;
FRegistros60R.Clear;
FRegistros61.Clear;
FRegistros61R.Clear;
FRegistros70.Clear;
FRegistros71.Clear;
FRegistros74.Clear;
FRegistros75.Clear;
FRegistros76.Clear;
FRegistros77.Clear;
FRegistros85.Clear;
FRegistros86.Clear;
FRegistros88Ean.Clear;
FRegistros88C.Clear;
FRegistros88D.Clear;
FRegistros88E.Clear;
FRegistros88T.Clear;
FRegistros88SP02.Clear;
FRegistros88SP03.Clear;
FRegistros88STES.Clear;
FRegistros88STITNF.Clear;
end;

function TACBrSintegra.GetVersao: string;
begin
Result:=ACBR_VERSAO;
end;

procedure TACBrSintegra.GerarRegistros61;
var
  wregistro: string;
  i: Integer;
begin
for i:=0 to Registros61.Count-1 do
begin
  with Registros61[i] do
  begin
    wregistro := '61';
    wregistro := wregistro + Space(28);
    wregistro := wregistro + FormatDateTime('yyyymmdd',Emissao);
    wregistro := wregistro + TBStrZero(Modelo,2);
    wregistro := wregistro + PadRight(Serie,3);
    wregistro := wregistro + PadRight(SubSerie,2);
    wregistro := wregistro + RightStr(TBStrZero(IntToStr(NumOrdemInicial), 6), 6);
    wregistro := wregistro + RightStr(TBStrZero(IntToStr(NumOrdemFinal), 6), 6);
    wregistro := wregistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00',Valor)),13);
    wregistro := wregistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00',BaseDeCalculo)),13);
    wregistro := wregistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorIcms)),12);
    wregistro := wregistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00',Isentas)),13);
    wregistro := wregistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00',Outras)),13);
    wregistro := wregistro + TBStrZero(TiraPontos(FormatFloat('#,##0.00',Aliquota)),4);
    wregistro := wregistro + Space(1);
    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros61R;
var
  i: Integer;
  wregistro: string;
begin
for i:=0 to Registros61R.Count-1 do
begin
  with Registros61R[i] do
  begin
    wregistro:='61R';
    wregistro:=wregistro+PadLeft(MesAno,6,'0');
    wregistro:=wregistro+PadRight(Codigo,14);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.000',Qtd)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Valor)),16);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',BaseDeCalculo)),16);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Aliquota)),4);
    wregistro:=wregistro+space(54);
    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros60I(Registros60I: TRegistros60I);
var
  wregistro: string;
  i: Integer;
begin
for I := 0 to Registros60I.Count - 1 do
begin
  wregistro:='60I';
  wregistro:=wregistro+FormatDateTime('yyyymmdd',Registros60I[i].Emissao);
  wregistro:=wregistro+PadRight(Trim(Registros60I[i].NumSerie),20);
  wregistro:=wregistro+PadRight(Trim(Registros60I[i].ModeloDoc),2);
  wregistro:=wregistro+TBStrzero(Registros60I[i].Cupom,6);
  wregistro:=wregistro+TBStrzero(IntToStr(Registros60I[i].Item),3);
  wregistro:=wregistro+PadRight(Registros60I[i].Codigo,14);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,###0.000',Registros60I[i].Quantidade)),13);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,##0.00',Registros60I[i].Valor)),13);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,##0.00',Registros60I[i].BaseDeCalculo)),12);
  wregistro:=wregistro+PadRight(TiraPontos(Registros60I[i].StAliquota),4);
  wregistro:=wregistro+TBStrZero(TiraPontos(
    FormatFloat('#,##0.00',Registros60I[i].ValorIcms)),12);
  wregistro:=wregistro+Space(16);
  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistros60R;
var
  i: Integer;
  wregistro: string;
begin
  for i:=0 to Registros60R.Count-1 do
  begin
    with Registros60R[i] do
    begin
      wregistro:='60R';
      wregistro:=wregistro+PadLeft(MesAno,6,'0');
      wregistro:=wregistro+PadRight(Codigo,14);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.000',Qtd)),13);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Valor)),16);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',BaseDeCalculo)),16);
      wregistro:=wregistro+PadRight(TiraPontos(Aliquota),4);
      wregistro:=wregistro+space(54);
      WriteRecord(wregistro);
    end;
  end;
end;

procedure TACBrSintegra.GerarRegistros55;
var
  wregistro: string;
  i: Integer;
begin
for I := 0 to Registros55.Count - 1 do
begin
  wregistro:='55';
  wregistro:=wregistro+TBStrZero(TiraPontos(Registros55[i].CNPJ),14)+
    PadRight(TiraPontos(Registros55[i].Inscricao),14);
  wregistro:=wregistro+FormatDateTime('yyyymmdd',Registros55[i].DataPagamento);
  wregistro:=wregistro+PadRight(Registros55[i].UF,2);
  wregistro:=wregistro+PadRight(Registros55[i].UFFavorecida,2);
  wregistro:=wregistro+TBStrZero(IntToStr(Registros55[i].Banco),3);
  wregistro:=wregistro+TBStrZero(IntToStr(Registros55[i].Agencia),4);
  wregistro:=wregistro+PadRight(Registros55[i].Numero,20);
  wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Registros55[i].Valor)),13);
  wregistro:=wregistro+FormatDateTime('yyyymmdd',Registros55[i].Vencimento);
  wregistro:=wregistro+PadLeft(Registros55[i].MesAno,6,'0');
  wregistro:=wregistro+PadRight(Registros55[i].NumeroConvenio,30);
  WriteRecord(wregistro);
end;
end;


procedure TACBrSintegra.GerarRegistro88EC;
var
  wregistro: string;
begin
wregistro:='88EC'+PadRight(Copy(Registro88EC.NomeContabilista,1,39),39);
wregistro:=wregistro+PadRight(Trim(TiraPontos(Registro88EC.CPFContabilista)),11);
wregistro:=wregistro+PadRight(Copy(Registro88EC.CRCContabilista,1,10),10);
wregistro:=wregistro+PadRight(Copy(Registro88EC.TelefoneContabilista,1,11),11);
wregistro:=wregistro+PadRight(Copy(Registro88EC.EmailContabilista,1,50),50);
wregistro:=wregistro+IntToStr(Registro88EC.AlteraContabilista);
WriteRecord(wregistro);
end;

procedure TACBrSintegra.GerarRegistro88SF;
var
  wregistro: string;
begin
wregistro:='88SF'+PadRight(Copy(Registro88SF.NomeEmpresa,1,35),35);
wregistro:=wregistro+PadRight(Trim(TiraPontos(Registro88SF.CNPJEmpresa)),14);
wregistro:=wregistro+PadRight(Trim(TiraPontos(Registro88SF.CPFTecnico)),11);
wregistro:=wregistro+PadRight(Copy(Registro88SF.TelefoneEmpresa,1,11),11);
wregistro:=wregistro+PadRight(Copy(Registro88SF.EmailEmpresa,1,50),50);
wregistro:=wregistro+IntToStr(Registro88SF.AlteraEmpresa);
WriteRecord(wregistro);
end;

procedure TACBrSintegra.GerarRegistros88Ean;
var
  wregistro: string;
  i: Integer;
begin
for I := 0 to Registros88Ean.Count - 1 do
begin
  wregistro:='88EAN';
  if Registros88Ean[i].VersaoEan=eanIndefinido then
  begin
    case Length(Registros88Ean[i].CodigoBarras) of
      8: wregistro:=wregistro+'08';
      12: wregistro:=wregistro+'12';
      13: wregistro:=wregistro+'13';
      14: wregistro:=wregistro+'14';
    else
      wregistro:=wregistro+'13';
    end;
  end
  else
  begin
    case Registros88Ean[i].VersaoEan of
      ean8: wregistro:=wregistro+'08';
      ean12: wregistro:=wregistro+'12';
      ean13: wregistro:=wregistro+'13';
      ean14: wregistro:=wregistro+'14';
    end;
  end;
  wregistro:=wregistro+TBStrZero(IntToStr(Registros88Ean[i].Codigo),14);
  wregistro:=wregistro+PadRight(Registros88Ean[i].Descricao,53);
  wregistro:=wregistro+PadRight(Registros88Ean[i].Unidade,6);
  wregistro:=wregistro+PadRight(Registros88Ean[i].CodigoBarras,14);
  wregistro:=wregistro+Space(32);
  WriteRecord(wregistro);
end;
end;

procedure TACBrSintegra.GerarRegistro88SME;
begin
writerecord('88SME'+TBStrZero(TiraPontos(Registro10.CNPJ),14)+
  PadRight('Sem Movimento de Entradas',34)+Space(73));
end;

procedure TACBrSintegra.GerarRegistro88SMS;
begin
writerecord('88SMS'+TBStrZero(TiraPontos(Registro10.CNPJ),14)+
  PadRight('Sem Movimento de Saídas',34)+Space(73));
end;

procedure TACBrSintegra.GerarRegistros76;
var
  wregistro: string;
  i: Integer;
begin
for i:=0 to Registros76.Count-1 do
begin
  with Registros76[i] do
  begin
    wregistro:='76';
    wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14)+
      PadRight(TiraPontos(Inscricao),14);
    wregistro:=wregistro+IntToStrZero(Modelo,2);
    wregistro:=wregistro+PadRight(Serie,2);
    wregistro:=wregistro+PadRight(SubSerie,2);
    wregistro:=wregistro+IntToStrZero(Numero,10);
    wregistro:=wregistro+PadRight(TiraPontos(Cfop),4);
    wregistro:=wregistro+IntToStr(Ord(TTipoReceita(TipoReceita))+1);
    wregistro:=wregistro+FormatDateTime('yyyymmdd',DataDocumento);
    wregistro:=wregistro+PadRight(UF,2);

    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorTotal)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',BasedeCalculo)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Icms)),12);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Isentas)),12);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',Outras)),12);
    wregistro:=wregistro+IntToStrZero(Aliquota,2);
    wregistro:=wregistro+PadRight(Situacao,1);

    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros77;
var
  wregistro: string;
  i: Integer;
begin
for i:=0 to Registros77.Count-1 do
begin
  with Registros77[i] do
  begin
    wregistro:='77';
    wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14);
    wregistro:=wregistro+IntToStrZero(Modelo,2);
    wregistro:=wregistro+PadRight(Serie,2);
    wregistro:=wregistro+PadRight(SubSerie,2);
    wregistro:=wregistro+IntToStrZero(Numero,10);
    wregistro:=wregistro+PadRight(TiraPontos(Cfop),4);
    wregistro:=wregistro+IntToStr(Ord(TTipoReceita(TipoReceita))+1);
    wregistro:=wregistro+IntToStrZero(NumeroItem,3);
    wregistro:=wregistro+PadRight(Codigo,11);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.000',Quantidade)),13);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorServico)),12);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',ValorDesconto)),12);
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',BasedeCalculo)),12);
    wregistro:=wregistro+IntToStrZero(Aliquota,2);
    wregistro:=wregistro+TBStrZero(TiraPontos(CNPJMF),14);
    wregistro:=wregistro+IntToStrZero(NumeroTerminal,10);
    WriteRecord(wregistro);
  end;
end;
end;

procedure TACBrSintegra.GerarRegistros88C;
var
  wregistro: string;
  i: Integer;
begin
  for i:=0 to Registros88C.Count-1 do
  begin
    wregistro:='88C';
    wregistro:=wregistro+TBStrZero(TiraPontos(Registros88C[i].CPFCNPJ),14);
    wregistro:=wregistro+TBStrZero(Registros88C[i].Modelo,2);
    wregistro:=wregistro+PadRight(Registros88C[i].Serie,3);
    wregistro:=wregistro+TBStrZero(RightStr(Registros88C[i].Numero,6),6);
    wregistro:=wregistro+PadRight(TiraPontos(Registros88C[i].CFOP),4);
    wregistro:=wregistro+IntToStrZero(Registros88C[i].NumeroItem,3);
    wregistro:=wregistro+PadRight(Registros88C[i].Codigo,14); //codigo do produto
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.000',
      Registros88C[i].Quantidade)),11); //quantidade do produto
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
      Registros88C[i].BaseST)),12); //total do produto
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
      Registros88C[i].ValorIcmsSTRepassar)),12); //ICMS-ST a repassar/reduzir
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
      Registros88C[i].ValorIcmsSTComplementar)),12); //ICMS-ST a complementar
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
      Registros88C[i].BasedeCalculoRetencao)),12); //base de calculo da retenção
    wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00',
      Registros88C[i].ValorParcelaImpostoRetido)),12); //valor da parcela do imposto retido
    wregistro:=wregistro+Space(6);
    WriteRecord(wregistro);
  end;//for
end;

procedure TACBrSintegra.GerarRegistros88D;
var
  wregistro: string;
  i: Integer;
begin
  For i:=0 to Registros88D.Count-1 do
  begin
    With Registros88D[i] do
    begin
      wregistro:='88D';
      wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14)+PadRight(TiraPontos(Inscricao),14);
      wregistro:=wregistro+PadRight(UF,2);
      wregistro:=wregistro+TBStrZero(Modelo,2);
      wregistro:=wregistro+PadRight(Serie,3);
      wregistro:=wregistro+TBStrZero(RightStr(Numero,6),6);
      wregistro:=wregistro+PadRight(EmissorDocumento,1);
      wregistro:=wregistro+FormatDateTime('yyyymmdd',DataDocumento);
      wregistro:=wregistro+FormatDateTime('yyyymmdd',DataSaidaEntrada);
      wregistro:=wregistro+TBStrZero(TiraPontos(CNPJLocalSaida),14);
      wregistro:=wregistro+PadRight(UFLocalSaida,2);
      wregistro:=wregistro+PadRight(TiraPontos(IeLocalSaida),14);
      wregistro:=wregistro+TBStrZero(TiraPontos(CNPJLocalEntrega),14);
      wregistro:=wregistro+PadRight(UFLocalEntrega,2);
      wregistro:=wregistro+PadRight(TiraPontos(IeLocalEntrega),14);
      wregistro:=wregistro+Space(5);
      WriteRecord(wregistro);
    end;//With
  end;//For
end;

procedure TACBrSintegra.GerarRegistros88E;
var
  wregistro: string;
  i: Integer;
begin
  For i:=0 to Registros88E.Count-1 do
  begin
    With Registros88E[i] do
    begin
      wregistro:='88E';
      wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14)+PadRight(TiraPontos(Inscricao),14);
      wregistro:=wregistro+PadRight(Registros88E[i].CodigoInformate,14); //codigo do produto
      wregistro:=wregistro+PadRight(Registros88E[i].CodigoSefaz,14); //codigo do produto
      wregistro:=wregistro+Space(67);
      WriteRecord(wregistro);
    end;//With
  end;//For
end;

procedure TACBrSintegra.GerarRegistros88T;
var
  wregistro: string;
  i: Integer;
begin
  For i:=0 to Registros88T.Count-1 do
  begin
    With Registros88T[i] do
    begin
      wregistro:='88T';
      wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJ),14);
      wregistro:=wregistro+FormatDateTime('yyyymmdd',DataDocumento);
      wregistro:=wregistro+PadRight(UF,2);
      wregistro:=wregistro+TBStrZero(Modelo,2);
      wregistro:=wregistro+PadRight(Serie,3);
      wregistro:=wregistro+TBStrZero(RightStr(Numero,6),6);
      wregistro:=wregistro+PadRight(EmissorDocumento,1);
      wregistro:=wregistro+PadRight(CifFob,1);
      wregistro:=wregistro+TBStrZero(TiraPontos(CPFCNPJFrete),14);
      wregistro:=wregistro+PadRight(UFFrete,2);
      wregistro:=wregistro+PadRight(TiraPontos(IeFrete),14);
      wregistro:=wregistro+IntToStrZero(Modal,1);
      wregistro:=wregistro+PadRight(Placa1,7);
      wregistro:=wregistro+PadRight(UFPlaca1,2);
      wregistro:=wregistro+PadRight(Placa2,7);
      wregistro:=wregistro+PadRight(UFPlaca2,2);
      wregistro:=wregistro+PadRight(Placa3,7);
      wregistro:=wregistro+PadRight(UFPlaca3,2);
      wregistro:=wregistro+Space(28);
      WriteRecord(wregistro);
    end;//With
  end;//For
end;

procedure TACBrSintegra.GerarRegistro88SP02;
var
  wregistro: string;
  i: Integer;
begin
  For i:=0 to Registros88SP02.Count-1 do
  begin
    With Registros88SP02[i] do
    begin
      wregistro:='88SP02';
      wregistro:=wregistro+FormatDateTime('yyyymmdd',DataEstoque);
      wregistro:=wregistro+PadRight(CodProduto,14);
      wregistro:=wregistro+PadRight(Descricao,53);
      wregistro:=wregistro+PadRight(UnidadeMedida,6);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.000', Quantidade)),13); //quantidade do produto
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', ValorUnitario)),13); //valor unitario
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', ValorTotal)),13);
      wregistro:=wregistro+PadRight(CodigoPosse,1);
      wregistro:=wregistro+TBStrZero(TiraPontos(Cnpj),14);
      wregistro:=wregistro+PadRight(TiraPontos(Ie),14);
      wregistro:=wregistro+PadRight(Uf,2);
      WriteRecord(wregistro, True);
    end;//With
  end;//For
end;

procedure TACBrSintegra.GerarRegistro88SP03;
var
  wregistro: string;
  i: Integer;
begin
  For i:=0 to Registros88SP03.Count-1 do
  begin
    With Registros88SP03[i] do
    begin
      wregistro:='88SP03';
      wregistro:=wregistro+TBStrZero(TiraPontos(Cnpj),14);
      wregistro:=wregistro+PadRight(TiraPontos(Ie),14);
      wregistro:=wregistro+FormatDateTime('yyyymmdd',DataRecebimento);
      wregistro:=wregistro+PadRight(Uf,2);
      wregistro:=wregistro+PadRight(Modelo,2);
      wregistro:=wregistro+PadRight(Serie,3);
      wregistro:=wregistro+TBStrZero(Numero, 6);
      wregistro:=wregistro+PadRight(Cfop,4);
      wregistro:=wregistro+PadRight(Emitente,1);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', ValorTotal)),13); //quantidade do produto
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', BaseCalculo)),13); //valor unitario
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', ValorIcms)),13);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', Isentas)),13);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', Outras)),13);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', Aliquota)),4);
      wregistro:=wregistro+PadRight(Situacao,1);
      wregistro:=wregistro+PadRight(BeneficioFiscal,1);
      wregistro:=wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', AliquotaInterna)),4);
      wregistro:=wregistro+FormatDateTime('yyyymmdd',DataEmissao);
      wregistro:=wregistro+PadRight(MicroEmpresa,1);
      WriteRecord(wregistro, True);
    end;//With
  end;//For
end;

{ TRegistros88STES }

procedure TACBrSintegra.GerarRegistro88STES;
var
  wregistro: string;
  i: Integer;
begin
  For i:=0 to Registros88STES.Count-1 do
  begin
    With Registros88STES[i] do
    begin
      wregistro := '88STES';
      wregistro := wregistro+TBStrZero(TiraPontos(CNPJ),14);
      wregistro := wregistro+FormatDateTime('yyyymmdd',DataInventario);
      wregistro := wregistro+TBStrZero(CodigoProduto,14);
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', Quantidade)),13); //quantidade do produto
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00', VlrICMSST)),12); //Valor ICMS ST
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00', VlrICMSOP)),12); //Valor ICMS OP
      WriteRecord(wregistro, True);
    end;//With
  end;//For
end;

{ TRegistros88STITNF }

procedure TACBrSintegra.GerarRegistro88STITNF;
var
  wregistro: string;
  i: Integer;
begin
  For i:=0 to Registros88STITNF.Count-1 do
  begin
    With Registros88STITNF[i] do
    begin
      wregistro := '88STITNF';
      wregistro := wregistro+TBStrZero(TiraPontos(CNPJ),14);
      wregistro := wregistro+PadRight(Modelo,2);
      wregistro := wregistro+PadRight(Serie,3);
      wregistro := wregistro+TBStrZero(Numero, 9);
      wregistro := wregistro+PadRight(CFOP,4);
      wregistro := wregistro+PadRight(CST,3);
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('000', NumeroItem)),3); //Numero do Item
      wregistro := wregistro+FormatDateTime('yyyymmdd',DataEntrada);
      wregistro := wregistro+TBStrZero(CodigoProduto,60);
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', Quantidade)),11); //quantidade do produto
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00', VlrProduto)),12); //valor do produto
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00', ValorDesconto)),12); //valor desconto
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00', BaseICMSOP)),12);
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00', BaseICMSST)),12);
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', AliquotaICMSOP)),4);
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,###0.00', AliquotaICMSST)),4);
      wregistro := wregistro+TBStrZero(TiraPontos(FormatFloat('#,##0.00', VlrIPI)),12);
      wregistro := wregistro+TBStrZero(TiraPontos(ChaveNFE),44);
      WriteRecord(wregistro, True);
    end;//With
  end;//For
end;

{ TRegistros50 }

constructor TRegistro50.Create;
begin
FRegistros54:=TRegistros54.Create(True);
FRegistros56:=TRegistros56.Create(True);

end;

destructor TRegistro50.Destroy;
begin
FRegistros54.Free;
FRegistros56.Free;
  inherited;
end;

function TRegistros50.Add(Obj: TRegistro50): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros50.GetObject(Index: Integer): TRegistro50;
begin
  Result := inherited GetItem(Index) as TRegistro50 ;
end;

procedure TRegistros50.Insert(Index: Integer; Obj: TRegistro50);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros50.SetObject(Index: Integer; Item: TRegistro50);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort50(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro50;
begin
  witem1 := TRegistro50(Item1);
  witem2 := TRegistro50(Item2);
  if (witem1.DataDocumento>witem2.DataDocumento) and (witem1.Numero>witem2.Numero) then
    Result := 1
  else if (witem1.DataDocumento = witem2.DataDocumento) and (witem1.Numero=witem2.Numero) then
    Result := 0
  else
    Result := -1;
end;

{ TRegistros54 }

function TRegistros54.Add(Obj: TRegistro54): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros54.GetObject(Index: Integer): TRegistro54;
begin
  Result := inherited GetItem(Index) as TRegistro54 ;
end;

procedure TRegistros54.Insert(Index: Integer; Obj: TRegistro54);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros54.SetObject(Index: Integer; Item: TRegistro54);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort54(Item1: Pointer;Item2: Pointer): Integer;
var
  LItem1, LItem2 : TRegistro54;
begin
  LItem1 := TRegistro54(Item1);
  LItem2 := TRegistro54(Item2);
  if (StrToInt64Def(LItem1.CPFCNPJ,0) > StrToInt64Def(LItem2.CPFCNPJ,0))
   and (LItem1.Serie > LItem2.Serie)
   and (StrToInt64Def(LItem1.Numero,0) > StrToInt64Def(LItem2.Numero,0))
   and (LItem1.NumeroItem > LItem2.NumeroItem) then
    Result := 1
  else if (StrToInt64Def(LItem1.CPFCNPJ,0) = StrToInt64Def(LItem2.CPFCNPJ,0))
   and (LItem1.Serie = LItem2.Serie)
   and (StrToInt64Def(LItem1.Numero,0) = StrToInt64Def(LItem2.Numero,0))
   and (LItem1.NumeroItem = LItem2.NumeroItem) then
    Result := 0
  else
    Result := -1;
end;

{ TRegistros56 }

function TRegistros56.Add(Obj: TRegistro56): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros56.GetObject(Index: Integer): TRegistro56;
begin
  Result := inherited GetItem(Index) as TRegistro56 ;
end;

procedure TRegistros56.Insert(Index: Integer; Obj: TRegistro56);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros56.SetObject(Index: Integer; Item: TRegistro56);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort56(Item1: Pointer;Item2: Pointer): Integer;
var
  LItem1, LItem2 : TRegistro56;
begin
  LItem1 := TRegistro56(Item1);
  LItem2 := TRegistro56(Item2);
  if (StrToInt64Def(LItem1.Cnpj,0) > StrToInt64Def(LItem2.Cnpj,0))
   and (LItem1.Serie > LItem2.Serie)
   and (StrToInt64Def(LItem1.Numero,0) > StrToInt64Def(LItem2.Numero,0))
   and (LItem1.NumeroItem > LItem2.NumeroItem) then
    Result := 1
  else if (StrToInt64Def(LItem1.Cnpj,0) = StrToInt64Def(LItem2.Cnpj,0))
   and (LItem1.Serie = LItem2.Serie)
   and (StrToInt64Def(LItem1.Numero,0) = StrToInt64Def(LItem2.Numero,0))
   and (LItem1.NumeroItem = LItem2.NumeroItem) then
    Result := 0
  else
    Result := -1;
end;

{ TRegistros75 }

function TRegistros75.Add(Obj: TRegistro75): Integer;
begin
  Result := GetRegistroExiste(Obj.Codigo) ;
  if Result < 0 then
     Result := inherited Add(Obj)
  else
     Obj.Free;
end;

function TRegistros75.GetObject(Index: Integer): TRegistro75;
begin
  Result := inherited GetItem(Index) as TRegistro75 ;
end;

function TRegistros75.GetRegistroExiste(const FCodigo: string): Integer;
var
  i: Integer;
begin
  Result := -1 ;
  I      := 0 ;
  while (I < Self.Count) and (Self[I].Codigo <> FCodigo) do
     Inc( I ) ;

  if I < Self.Count then
     Result := I;
end;

procedure TRegistros75.Insert(Index: Integer; Obj: TRegistro75);
begin
  if GetRegistroExiste(Obj.Codigo) < 0 then
     inherited Insert(Index, Obj)
  else
     Obj.Free;
end;

procedure TRegistros75.SetObject(Index: Integer; Item: TRegistro75);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort75(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro75;
begin
  witem1 := TRegistro75(Item1);
  witem2 := TRegistro75(Item2);
  if witem1.Codigo>witem2.Codigo then
    Result := 1
  else if witem1.Codigo = witem2.Codigo then
    Result := 0
  else
    Result := -1;
end;

function Sort85(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro85;
begin
  witem1 := TRegistro85(Item1);
  witem2 := TRegistro85(Item2);
  if witem1.DataDeclaracao>witem2.DataDeclaracao then
    Result := 1
  else if witem1.DataDeclaracao = witem2.DataDeclaracao then
    Result := 0
  else
    Result := -1;
end;

function Sort86(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro86;
begin
  witem1 := TRegistro86(Item1);
  witem2 := TRegistro86(Item2);
  if witem1.DataRegistro > witem2.DataRegistro then
    Result := 1
  else if witem1.DataRegistro = witem2.DataRegistro then
    Result := 0
  else
    Result := -1;
end;

{ TRegistros70 }

function TRegistros70.Add(Obj: TRegistro70): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros70.GetObject(Index: Integer): TRegistro70;
begin
  Result := inherited GetItem(Index) as TRegistro70 ;
end;

procedure TRegistros70.Insert(Index: Integer; Obj: TRegistro70);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros70.SetObject(Index: Integer; Item: TRegistro70);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort70(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro70;
begin
  witem1 := TRegistro70(Item1);
  witem2 := TRegistro70(Item2);
  if witem1.DataDocumento>witem2.DataDocumento then
    Result := 1
  else if witem1.DataDocumento = witem2.DataDocumento then
    Result := 0
  else
    Result := -1;
end;

{ TRegistros71 }

function TRegistros71.Add(Obj: TRegistro71): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros71.GetObject(Index: Integer): TRegistro71;
begin
  Result := inherited GetItem(Index) as TRegistro71 ;
end;

procedure TRegistros71.Insert(Index: Integer; Obj: TRegistro71);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros71.SetObject(Index: Integer; Item: TRegistro71);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort71(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro71;
begin
  witem1 := TRegistro71(Item1);
  witem2 := TRegistro71(Item2);
  if witem1.DataDocumento>witem2.DataDocumento then
    Result := 1
  else if witem1.DataDocumento = witem2.DataDocumento then
    Result := 0
  else
    Result := -1;
end;


{ TRegistros51 }

function TRegistros51.Add(Obj: TRegistro51): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros51.GetObject(Index: Integer): TRegistro51;
begin
  Result := inherited GetItem(Index) as TRegistro51 ;
end;

procedure TRegistros51.Insert(Index: Integer; Obj: TRegistro51);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros51.SetObject(Index: Integer; Item: TRegistro51);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort51(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro51;
begin
  witem1 := TRegistro51(Item1);
  witem2 := TRegistro51(Item2);
  if witem1.DataDocumento>witem2.DataDocumento then
    Result := 1
  else if witem1.DataDocumento = witem2.DataDocumento then
    Result := 0
  else
    Result := -1;
end;

{ TRegistros53 }

function TRegistros53.Add(Obj: TRegistro53): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros53.GetObject(Index: Integer): TRegistro53;
begin
  Result := inherited GetItem(Index) as TRegistro53 ;
end;

procedure TRegistros53.Insert(Index: Integer; Obj: TRegistro53);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros53.SetObject(Index: Integer; Item: TRegistro53);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort53(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro53;
begin
  witem1 := TRegistro53(Item1);
  witem2 := TRegistro53(Item2);
  if witem1.DataDocumento>witem2.DataDocumento then
    Result := 1
  else if witem1.DataDocumento = witem2.DataDocumento then
    Result := 0
  else
    Result := -1;
end;

{ TRegistros60M }

function TRegistros60M.Add(Obj: TRegistro60M): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros60M.GetObject(Index: Integer): TRegistro60M;
begin
  Result := inherited GetItem(Index) as TRegistro60M;
end;

procedure TRegistros60M.Insert(Index: Integer; Obj: TRegistro60M);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros60M.SetObject(Index: Integer; Item: TRegistro60M);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort60M(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro60M;
begin
  witem1 := TRegistro60M(Item1);
  witem2 := TRegistro60M(Item2);

  if witem1.Emissao>witem2.Emissao then
  begin
    Result:=1
  end
  else if witem1.Emissao = witem2.Emissao then
  begin
    if witem1.CRZ >witem2.CRZ  then
    begin
      Result := 1;
    end
    else if witem1.CRZ =witem2.CRZ  then
    begin
      Result := 0;
    end
    else
    begin
      Result := -1;
    end;
  end
  else
  begin
    Result:=-1;
  end;
end;

{ TRegistros60A }

function TRegistros60A.Add(Obj: TRegistro60A): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros60A.GetObject(Index: Integer): TRegistro60A;
begin
  Result := inherited GetItem(Index) as TRegistro60A ;
end;

procedure TRegistros60A.Insert(Index: Integer; Obj: TRegistro60A);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros60A.SetObject(Index: Integer; Item: TRegistro60A);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort60A(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro60A;
begin
  witem1 := TRegistro60A(Item1);
  witem2 := TRegistro60A(Item2);

  if witem1.NumSerie>witem2.NumSerie then
    Result:=1
  else if witem1.NumSerie=witem2.NumSerie then
    Result:=0
  else
    Result:=-1;

end;

{ TRegistro60M }

constructor TRegistro60M.Create;
begin
  inherited Create;
  FRegs60A := TRegistros60A.Create(True);
  FRegs60D := TRegistros60D.Create(True);
  FRegs60I := TRegistros60I.Create(True);
end;

destructor TRegistro60M.Destroy;
begin
  FRegs60A.Free;
  FRegs60D.Free;
  FRegs60I.Free;
  inherited;
end;

{ TRegistros60D }

function TRegistros60D.Add(Obj: TRegistro60D): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros60D.GetObject(Index: Integer): TRegistro60D;
begin
  Result := inherited GetItem(Index) as TRegistro60D ;
end;

procedure TRegistros60D.Insert(Index: Integer; Obj: TRegistro60D);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros60D.SetObject(Index: Integer; Item: TRegistro60D);
begin
  inherited SetItem (Index, Item) ;
end;

function Sort60D(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro60D;
begin
  witem1 := TRegistro60D(Item1);
  witem2 := TRegistro60D(Item2);

  if witem1.NumSerie>witem2.NumSerie then
    Result:=1
  else if witem1.NumSerie=witem2.NumSerie then
    Result:=0
  else
    Result:=-1;
end;

{ TRegistros74 }

function Sort74(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro74;
begin
  witem1 := TRegistro74(Item1);
  witem2 := TRegistro74(Item2);

  if witem1.Codigo>witem2.Codigo then
    Result:=1
  else if witem1.Codigo=witem2.Codigo then
    Result:=0
  else
    Result:=-1;
end;

function TRegistros74.Add(Obj: TRegistro74): Integer;
begin
  Result := GetRegistroExiste(Obj.Codigo) ;
  if Result < 0 then
     Result := inherited Add(Obj)
  else
     Obj.Free;
end;

function TRegistros74.GetObject(Index: Integer): TRegistro74;
begin
  Result := inherited GetItem(Index) as TRegistro74 ;
end;

procedure TRegistros74.Insert(Index: Integer; Obj: TRegistro74);
begin
  if GetRegistroExiste(Obj.Codigo) < 0 then
     inherited Insert(Index, Obj)
  else
     Obj.Free;
end;

function TRegistros74.GetRegistroExiste(const FCodigo: string): Integer;
var
  i: Integer;
begin
  Result := -1 ;
  I      := 0 ;
  while (I < Self.Count) and (Self[I].Codigo <> FCodigo) do
     Inc( I ) ;

  if I < Self.Count then
     Result := I;
end;

procedure TRegistros74.SetObject(Index: Integer; Item: TRegistro74);
begin
  inherited SetItem (Index, Item) ;
end;

{ TRegistros61 }

function TRegistros61.Add(Obj: TRegistro61): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TRegistros61.GetObject(Index: Integer): TRegistro61;
begin
  Result := inherited GetItem(Index) as TRegistro61 ;
end;

procedure TRegistros61.Insert(Index: Integer; Obj: TRegistro61);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros61.SetObject(Index: Integer; Item: TRegistro61);
begin
  inherited SetItem (Index, Item);
end;

{ TRegistros61R }

function TRegistros61R.Add(Obj: TRegistro61R): Integer;
begin
  Result := inherited Add(Obj);
end;

function TRegistros61R.GetObject(Index: Integer): TRegistro61R;
begin
  Result := inherited GetItem(Index) as TRegistro61R;
end;

procedure TRegistros61R.Insert(Index: Integer; Obj: TRegistro61R);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros61R.SetObject(Index: Integer; Item: TRegistro61R);
begin
  inherited SetItem (Index, Item);
end;

{ TRegistros60I }

function TRegistros60I.Add(Obj: TRegistro60I): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros60I.GetObject(Index: Integer): TRegistro60I;
begin
  Result:=inherited GetItem(Index) as TRegistro60I;
end;

procedure TRegistros60I.Insert(Index: Integer; Obj: TRegistro60I);
begin
  inherited Insert(Index,Obj);
end;

procedure TRegistros60I.SetObject(Index: Integer; Item: TRegistro60I);
begin
  inherited SetItem(Index, Item);
end;

function Sort60I(Item1, Item2: Pointer): Integer;
var
  witem1, witem2 : TRegistro60I;
begin
  witem1 := TRegistro60I(Item1);
  witem2 := TRegistro60I(Item2);

  if witem1.NumSerie>witem2.NumSerie then
    Result:=1
  else if witem1.NumSerie=witem2.NumSerie then
    Result:=0
  else
    Result:=-1;
end;

{ TRegistros60R }

function TRegistros60R.Add(Obj: TRegistro60R): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros60R.GetObject(Index: Integer): TRegistro60R;
begin
Result:= inherited GetItem(Index) as TRegistro60R;
end;

procedure TRegistros60R.Insert(Index: Integer; Obj: TRegistro60R);
begin
inherited Insert(Index, Obj);
end;

procedure TRegistros60R.SetObject(Index: Integer; Item: TRegistro60R);
begin
inherited SetItem(Index, Item);
end;

procedure TRegistros85.SetObject(Index: Integer; Item: TRegistro85);
begin
  inherited SetItem (Index, Item) ;
end;

function TRegistros85.GetObject(Index: Integer): TRegistro85;
begin
  Result := inherited GetItem(Index) as TRegistro85 ;
end;

procedure TRegistros85.Insert(Index: Integer; Obj: TRegistro85);
begin
  inherited Insert(Index, Obj);
end;

function TRegistros85.Add(Obj: TRegistro85): Integer;
begin
  Result := inherited Add(Obj) ;
end;

procedure TRegistros86.SetObject(Index: Integer; Item: TRegistro86);
begin
  inherited SetItem (Index, Item) ;
end;

function TRegistros86.GetObject(Index: Integer): TRegistro86;
begin
  Result := inherited GetItem(Index) as TRegistro86 ;
end;

procedure TRegistros86.Insert(Index: Integer; Obj: TRegistro86);
begin
  inherited Insert(Index, Obj);
end;

function TRegistros86.Add(Obj: TRegistro86): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TRegistros55 }

function TRegistros55.Add(Obj: TRegistro55): Integer;
begin
  Result := inherited Add(Obj);
end;

function TRegistros55.GetObject(Index: Integer): TRegistro55;
begin
  Result := inherited GetItem(Index) as TRegistro55;
end;

procedure TRegistros55.Insert(Index: Integer; Obj: TRegistro55);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros55.SetObject(Index: Integer; Item: TRegistro55);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88Ean }

function TRegistros88Ean.Add(Obj: TRegistro88Ean): Integer;
begin
  Result := GetRegistroExiste(Obj.Codigo) ;
  if Result < 0 then
     Result := inherited Add(Obj)
  else
     Obj.Free ;
end;

function TRegistros88Ean.GetObject(Index: Integer): TRegistro88Ean;
begin
  Result := inherited GetItem(Index) as TRegistro88Ean;
end;

procedure TRegistros88Ean.Insert(Index: Integer; Obj: TRegistro88Ean);
begin
 if GetRegistroExiste(Obj.Codigo) < 0 then
    inherited Insert(Index, Obj)
 else
    Obj.Free;
end;

function TRegistros88Ean.GetRegistroExiste(FCodigo: Integer): Integer;
var
  i: Integer;
begin
  Result := -1 ;
  I      := 0 ;
  while (I < Self.Count) and (Self[I].Codigo <> FCodigo) do
    Inc( I ) ;

  if I < Self.Count then
     Result := I;
end;

procedure TRegistros88Ean.SetObject(Index: Integer; Item: TRegistro88Ean);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros76 }

function TRegistros76.Add(Obj: TRegistro76): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros76.GetObject(Index: Integer): TRegistro76;
begin
  Result:=inherited GetItem(Index) as TRegistro76;
end;

procedure TRegistros76.Insert(Index: Integer; Obj: TRegistro76);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros76.SetObject(Index: Integer; Item: TRegistro76);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros77 }

function TRegistros77.Add(Obj: TRegistro77): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros77.GetObject(Index: Integer): TRegistro77;
begin
  Result:=inherited GetItem(Index) as TRegistro77;
end;

procedure TRegistros77.Insert(Index: Integer; Obj: TRegistro77);
begin
  inherited Insert(Index, Obj);
end;

procedure TRegistros77.SetObject(Index: Integer; Item: TRegistro77);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistro88Ean }

constructor TRegistro88Ean.Create;
begin
  inherited;
  VersaoEan := eanIndefinido;
end;

{ TRegistros88C }

function TRegistros88C.Add(Obj: TRegistro88C): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros88C.GetObject(Index: Integer): TRegistro88C;
begin
  Result:=inherited GetItem(Index) as TRegistro88C;
end;

procedure TRegistros88C.Insert(Index: Integer; Obj: TRegistro88C);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88C.SetObject(Index: Integer; Item: TRegistro88C);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88D }

function TRegistros88D.Add(Obj: TRegistro88D): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros88D.GetObject(Index: Integer): TRegistro88D;
begin
  Result:=inherited GetItem(Index) as TRegistro88D;
end;

procedure TRegistros88D.Insert(Index: Integer; Obj: TRegistro88D);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88D.SetObject(Index: Integer; Item: TRegistro88D);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88STES }

function TRegistros88STES.Add(Obj: TRegistro88STES): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros88STES.GetObject(Index: Integer): TRegistro88STES;
begin
  Result:=inherited GetItem(Index) as TRegistro88STES;
end;

procedure TRegistros88STES.Insert(Index: Integer; Obj: TRegistro88STES);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88STES.SetObject(Index: Integer; Item: TRegistro88STES);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88E }

function TRegistros88E.Add(Obj: TRegistro88E): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros88E.GetObject(Index: Integer): TRegistro88E;
begin
  Result:=inherited GetItem(Index) as TRegistro88E;
end;

procedure TRegistros88E.Insert(Index: Integer; Obj: TRegistro88E);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88E.SetObject(Index: Integer; Item: TRegistro88E);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88T }

function TRegistros88T.Add(Obj: TRegistro88T): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros88T.GetObject(Index: Integer): TRegistro88T;
begin
  Result:=inherited GetItem(Index) as TRegistro88T;
end;

procedure TRegistros88T.Insert(Index: Integer; Obj: TRegistro88T);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88T.SetObject(Index: Integer; Item: TRegistro88T);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88SP02 }

function TRegistros88SP02.Add(Obj: TRegistro88SP02): Integer;
begin
  Result := inherited Add(Obj);
end;

function TRegistros88SP02.GetObject(Index: Integer): TRegistro88SP02;
begin
  Result := inherited GetItem(Index) as TRegistro88SP02;
end;

procedure TRegistros88SP02.Insert(Index: Integer; Obj: TRegistro88SP02);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88SP02.SetObject(Index: Integer;
  Item: TRegistro88SP02);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88SP03 }

function TRegistros88SP03.Add(Obj: TRegistro88SP03): Integer;
begin
  Result := inherited Add(Obj)
end;

function TRegistros88SP03.GetObject(Index: Integer): TRegistro88SP03;
begin
  Result := inherited GetItem(Index) as TRegistro88SP03;
end;

procedure TRegistros88SP03.Insert(Index: Integer; Obj: TRegistro88SP03);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88SP03.SetObject(Index: Integer;
  Item: TRegistro88SP03);
begin
  inherited SetItem(Index, Item);
end;

{ TRegistros88STITNF }

function TRegistros88STITNF.Add(Obj: TRegistro88STITNF): Integer;
begin
  Result:=inherited Add(Obj);
end;

function TRegistros88STITNF.GetObject(Index: Integer): TRegistro88STITNF;
begin
  Result:=inherited GetItem(Index) as TRegistro88STITNF;
end;

procedure TRegistros88STITNF.Insert(Index: Integer;
  Obj: TRegistro88STITNF);
begin
  inherited SetItem(Index, Obj);
end;

procedure TRegistros88STITNF.SetObject(Index: Integer;
  Item: TRegistro88STITNF);
begin
  inherited SetItem(Index, Item);
end;

end.
