{===============================================================================
Projeto Sintegra
Biblioteca de Componente para geração do arquivo Sintegra
Site: http://codigolivre.org.br/projects/sintegra/

Direitos Autorais Reservados (c) 2004 Régys Borges da Silveira

Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la sob
os termos da Licença Pública Geral Menor do GNU conforme publicada pela Free
Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) qualquer
versão posterior.

Esta biblioteca é distribuído na expectativa de que seja útil, porém, SEM
NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU
ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor do
GNU para mais detalhes.

Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto com
esta biblioteca; se não, escreva para a Free Software Foundation, Inc., no
endereço 59 TEmple Street, Suite 330, Boston, MA 02111-1307 USA.
================================================================================
Contato:
          Autor...: Régys Borges da Silveira
          Email...: regyssilveira@hotmail.com
================================================================================
Colaboração:
          Anderson Carli   <anderson@f10.com.br>
          Koplin           <alfredo@k-systems.eti.br>
          Marcelo Welter   <marcelo@welter.pro.br>
===============================================================================}

unit Sintegra;

interface

uses
  SysUtils, Classes, DateUtils, Dialogs, Sintegra_TLB;

  { Funcoes Utilitárias }
  function VerificaCEP(cCep, cEstado: string): Boolean;
  function VerificaCFOP(aCFOP: SmallInt): Boolean;
  function VerificaCPF_CNPJ(numero: string): Boolean;
  function VerificaInscEstadual(aInscricao, aTipo: string): Boolean;
  function VerificaUF(aUF: string): Boolean;

  { Factoring da Interface ISintegra }
  function CreateSintegra():ISintegra;

var
  Debug: Boolean = False;

implementation

const
  kReg50 = [01, 04, 06, 21, 22, 55];
  kReg70 = [07, 08, 09, 10, 11, 26];
  kReg71 = [08, 09, 10, 11, 26];


//############################################################################//

type
  TSintegraObject = class(TInterfacedObject)
  protected
    function RFIll(Str: string; Tamanho: Integer = 0; Caracter: Char = ' '): string; overload;
    function LFIll(Str: string; Tamanho: Integer = 0; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: Currency; Tamanho: Integer; Decimais: Integer = 2; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: Integer; Tamanho: Integer; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: TDateTime): string; overload;

    function RetornaFrete(aModFrete: TModalidadeFrete): string;
    function RetornaCodPosse(TipoPosse: TTipoPosse): string;
    function RetornaFinalidade(aFinalidade: TCodFinalidade): string;
    function RetornaModDocumento(aTipoDocumento: TModDocumento): string;
    function RetornaNatureza(aNaturezaOper: TCodIdentificaOper): string;
    function RetornaSituacao(aSituacao: TSituacao): string;
    function RetornaArquivoMagnetico(aTipo: TArquivoMagnetico): string;
    function RetornaTipoEmitente(aTipoEmitente: TTipoEmitente): string;
    function RetornaSoNumero(aNumero: string): string;
    function RetiraMascara(aString: String): string;
    function StrToNumero(aValor: string; aDecimais: Integer): Extended;

    function VerificaCST(aCST: string): Boolean;
    function VerificaSitTributaria(aSitTributaria: string): Boolean;
  end;

  TRegistro1X = class(TSintegraObject, IRegistro1X)
  private
    FRazaoSocial: string;
    FCNPJ: string;
    FInscEstadual: string;
    FMunicipio: string;
    FUF: string;
    FFax: string;
    FResponsavel: string;
    FEndereco: string;
    FComplemento: string;
    FNumero: Integer;
    FBairro: string;
    FCEP: string;
    FFone: string;
    FContribuinteIPI: Boolean;
    FSubstitutoTributario: Boolean;
    function GetRazaoSocial: string;
    procedure SetRazaoSocial(const Valor: string);
    function GetCNPJ: string;
    procedure SetCNPJ(const Valor: string);
    function GetInscEstadual: string;
    procedure SetInscEstadual(const Valor: string);
    function GetMunicipio: string;
    procedure SetMunicipio(const Valor: string);
    function GetUF: string;
    procedure SetUF(const Valor: string);
    function GetFax: string;
    procedure SetFax(const Valor: string);
    function GetResponsavel: string;
    procedure SetResponsavel(const Valor: string);
    function GetEndereco: string;
    procedure SetEndereco(const Valor: string);
    function GetComplemento: string;
    procedure SetComplemento(const Valor: string);
    function GetNumero: Integer;
    procedure SetNumero(const Valor: Integer);
    function GetBairro: string;
    procedure SetBairro(const Valor: string);
    function GetCEP: string;
    procedure SetCEP(const Valor: string);
    function GetFone: string;
    procedure SetFone(const Valor: string);
    function GetContribuinteIPI: Boolean;
    procedure SetContribuinteIPI(const Valor: Boolean);
    function GetSubstitutoTributario: Boolean;
    procedure SetSubstitutoTributario(const Valor: Boolean);
  public
    property RazaoSocial: string read GetRazaoSocial write SetRazaoSocial;
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property Endereco: string read GetEndereco write SetEndereco;
    property Complemento: string read GetComplemento write SetComplemento;
    property Numero: Integer read GetNumero write SetNumero default 0;
    property Bairro: string read GetBairro write SetBairro;
    property Municipio: string read GetMunicipio write SetMunicipio;
    property CEP: string read GetCEP write SetCEP;
    property UF: string read GetUF write SetUF;
    property Fax: string read GetFax write SetFax;
    property Fone: string read GetFone write SetFone;
    property Responsavel: string read GetResponsavel write SetResponsavel;
    property ContribuinteIPI: Boolean read GetContribuinteIPI write SetContribuinteIPI default False;
    property SubstitutoTributario: Boolean read GetSubstitutoTributario write SetSubstitutoTributario default False;
  end;

  TSintegra = class(TSintegraObject, ISintegra)
  private
    FOnErro: TErrorEvent;
    FDataInicial: TDateTime;
    FDataFinal: TDateTime;
    FNaturezaOperacao: TCodIdentificaOper;
    FFinalidade: TCodFinalidade;
    FRegistro1X: IRegistro1X;
    FRegistro5X: IRegistro5XLista;
    FRegistro55: IRegistro55Lista;
    FRegistro60M: IRegistro60MList;
    FRegistro60R: IRegistro60RLista;
    FRegistro61: IRegistro61Lista;
    FRegistro61R: IRegistro61RLista;
    FRegistro74: IRegistro74Lista;
    FRegistro75: IRegistro75Lista;
    FRegistro85: IRegistro85Lista;
    FRegistro86: IRegistro86Lista;
    FArquivoMagnetico: TArquivoMagnetico;
    function GetRegistro60M: IRegistro60MList;
    function GetRegistro60R: IRegistro60RLista;
    function GetDataFinal: TDateTime;
    function GetDataInicial: TDateTime;
    function GetRegistro1X: IRegistro1X;
    function GetFinalidade: TCodFinalidade;
    function GetRegistro55: IRegistro55Lista;
    function GetRegistro74: IRegistro74Lista;
    function GetNaturezaOperacao: TCodIdentificaOper;
    function GetArquivoMagnetico: TArquivoMagnetico;
    function GetRegistro5X: IRegistro5XLista;
    function GetRegistro61: IRegistro61Lista;
    function GetRegistro61R: IRegistro61RLista;
    function GetRegistro75: IRegistro75Lista;
    function GetRegistro85: IRegistro85Lista;
    function GetRegistro86: IRegistro86Lista;
    function GetVersao: string;
    procedure SetRegistro60M(const Valor: IRegistro60MList);
    procedure SetRegistro60R(const Valor: IRegistro60RLista);
    procedure SetDataFinal(const Valor: TDateTime);
    procedure SetDataInicial(const Valor: TDateTime);
    procedure SetRegistro1X(const Valor: IRegistro1X);
    procedure SetFinalidade(const Valor: TCodFinalidade);
    procedure SetRegistro55(const Valor: IRegistro55Lista);
    procedure SetRegistro74(const Valor: IRegistro74Lista);
    procedure SetNaturezaOperacao(const Valor: TCodIdentificaOper);
    procedure SetRegistro5X(const Valor: IRegistro5XLista);
    procedure SetRegistro61(const Valor: IRegistro61Lista);
    procedure SetRegistro61R(const Valor: IRegistro61RLista);
    procedure SetRegistro75(const Valor: IRegistro75Lista);
    procedure SetRegistro85(const Valor: IRegistro85Lista);
    procedure SetRegistro86(const Valor: IRegistro86Lista);
    function GetOnErro: TErrorEvent;
    procedure SetOnErro(const Value: TErrorEvent);
    procedure GerarErro(MensagemErro: String);
    procedure SetArquivoMagnetico(const Value: TArquivoMagnetico);
  protected
    function Registro_10: string;
    function Registro_11: string;
    function Registro_50: string;
    function Registro_60: string;
    function Registro_61: string;
    function Registro_70: string;
    function Registro_71: string;
    function Registro_74: string;
    function Registro_75: string;
    function Registro_85: string;
    function Registro_90: string;
    function VerificaCorrespondente(aProduto: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimparRegistros;
    function VerificaProduto(aProduto: string): Boolean;
    function GerarArquivo(aArquivo: string): Boolean;
    procedure Check(Condicao: Boolean; const Msg: string); overload;
    procedure Check(Condicao: Boolean; Msg: string; Fmt: array of const); overload;
    procedure Check(const StrNaoNula, Msg: string); overload;
    procedure Check(const StrNaoNula, Msg: string; Fmt: array of const); overload;
    procedure Check(Valor, Minimo, Maximo: Double; const Msg: string); overload;
    procedure Check(Valor, Minimo, Maximo: Double; const Msg: string; Fmt: array of const); overload;
    property OnErro: TErrorEvent read GetOnErro write SetOnErro;
    property Versao: string read GetVersao;
    property DataInicial: TDateTime read GetDataInicial write SetDataInicial;
    property DataFinal: TDateTime read GetDataFinal write SetDataFinal;
    property NatOperacao: TCodIdentificaOper read GetNaturezaOperacao write SetNaturezaOperacao;
    property Finalidade: TCodFinalidade read GetFinalidade write SetFinalidade;
    property ArquivoMagnetico: TArquivoMagnetico read GetArquivoMagnetico write SetArquivoMagnetico;
    property Registro1X: IRegistro1X read GetRegistro1X write SetRegistro1X;
    property Registro5X: IRegistro5XLista read GetRegistro5X write SetRegistro5X;
    property Registro60M: IRegistro60MList read GetRegistro60M write SetRegistro60M;
    property Registro60R: IRegistro60RLista read GetRegistro60R write SetRegistro60R;
    property Registro61: IRegistro61Lista read GetRegistro61 write SetRegistro61;
    property Registro61R: IRegistro61RLista read GetRegistro61R write SetRegistro61R;
    property Registro55: IRegistro55Lista read GetRegistro55 write SetRegistro55;
    property Registro75: IRegistro75Lista read GetRegistro75 write SetRegistro75;
    property Registro85: IRegistro85Lista read GetRegistro85 write SetRegistro85;
    property Registro86: IRegistro86Lista read GetRegistro86 write SetRegistro86;
    property Registro74: IRegistro74Lista read GetRegistro74 write SetRegistro74;
  end;

  TRegistro50 = class(TSintegraObject, IRegistro50)
  private
    fAliquota: Currency;
    fCFOP: SmallInt;
    fValorTotal: Currency;
    fBaseICMS: Currency;
    fValorICMS: Currency;
    fIsentasNTribut: Currency;
    fOutras: Currency;
    function GetAliquota: Currency;
    procedure SetAliquota(const Valor: Currency);
    function GetCFOP: SmallInt;
    procedure SetCFOP(const Valor: SmallInt);
    function GetValorTotal: Currency;
    procedure SetValorTotal(const Valor: Currency);
    function GetBaseICMS: Currency;
    procedure SetBaseICMS(const Valor: Currency);
    function GetValorICMS: Currency;
    procedure SetValorICMS(const Valor: Currency);
    function GetIsentasNTribut: Currency;
    procedure SetIsentasNTribut(const Valor: Currency);
    function GetOutras: Currency;
    procedure SetOutras(const Valor: Currency);
  public
    property Aliquota: Currency read GetAliquota write SetAliquota;
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property ValorICMS: Currency read GetValorICMS write SetValorICMS;
    property IsentasNTribut: Currency read GetIsentasNTribut write SetIsentasNTribut;
    property Outras: Currency read GetOutras write SetOutras;
  end;

  TRegistro50Lista = class(TInterfaceList, IRegistro50Lista)
  private
    function GetItem(Index: Integer): IRegistro50;
    procedure SetItem(Index: Integer; const Value: IRegistro50);
  public
    function New: IRegistro50;
    property Items[Index: Integer]: IRegistro50 read GetItem write SetItem;
  end;

  TRegistro51 = class(TSintegraObject, IRegistro51)
  private
    fCFOP: SmallInt;
    fValor: Currency;
    fValorIPI: Currency;
    fIsentaNTrib: Currency;
    fOutras: Currency;
    function GetCFOP: SmallInt;
    function GetIsentaNTrib: Currency;
    function GetOutras: Currency;
    function GetValor: Currency;
    function GetValorIPI: Currency;
    procedure SetCFOP(const Valor: SmallInt);
    procedure SetIsentaNTrib(const Valor: Currency);
    procedure SetOutras(const Valor: Currency);
    procedure SetValor(const Valor: Currency);
    procedure SetValorIPI(const Valor: Currency);
  public
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property Valor: Currency read GetValor write SetValor;
    property ValorIPI: Currency read GetValorIPI write SetValorIPI;
    property IsentaNTrib: Currency read GetIsentaNTrib write SetIsentaNTrib;
    property Outras: Currency read GetOutras write SetOutras;
  end;

  TRegistro51Lista = class(TInterfaceList, IRegistro51Lista)
  private
    function GetItem(Index: Integer): IRegistro51;
    procedure SetItem(Index: Integer; const Value: IRegistro51);
  public
    function New: IRegistro51;
    property Items[Index: Integer]: IRegistro51 read GetItem write SetItem;
  end;

  TRegistro53 = class(TSintegraObject, IRegistro53)
  private
    fCFOP: SmallInt;
    fBaseCalcICMSST: Currency;
    fICMSRetido: Currency;
    fDespAcessoria: Currency;
    FCodAntecipacao: string;
    function GetBaseCalcICMSST: Currency;
    function GetCFOP: SmallInt;
    function GetDespAcessoria: Currency;
    function GetICMSRetido: Currency;
    function GetCodAntecipacao: string;
    procedure SetBaseCalcICMSST(const Valor: Currency);
    procedure SetCFOP(const Valor: SmallInt);
    procedure SetDespAcessoria(const Valor: Currency);
    procedure SetICMSRetido(const Valor: Currency);
    procedure SetCodAntecipacao(const Value: string);
  public
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property BaseCalcICMSST: Currency read GetBaseCalcICMSST write SetBaseCalcICMSST;
    property ICMSRetido: Currency read GetICMSRetido write SetICMSRetido;
    property DespAcessoria: Currency read GetDespAcessoria write SetDespAcessoria;
    property CodAntecipacao: string read GetCodAntecipacao write SetCodAntecipacao;
  end;

  TRegistro53Lista = class(TInterfaceList, IRegistro53Lista)
  private
    function GetItem(Index: Integer): IRegistro53;
    procedure SetItem(Index: Integer; const Value: IRegistro53);
  public
    function New: IRegistro53;
    property Items[Index: Integer]: IRegistro53 read GetItem write SetItem;
  end;

  TRegistro54 = class(TSintegraObject, IRegistro54)
  private
    fCFOP: SmallInt;
    fCST: string;
    fNumItem: SmallInt;
    fCodProduto: string;
    fQuantidade: Extended;
    fValorProduto: Currency;
    fDesconto: Currency;
    fBaseICMS: Currency;
    fBaseICMSST: Currency;
    fIPI: Currency;
    fAliquota: Currency;
    function GetAliquota: Currency;
    function GetBaseICMS: Currency;
    function GetBaseICMSST: Currency;
    function GetCodProduto: string;
    function GetCST: string;
    function GetDesconto: Currency;
    function GetIPI: Currency;
    function GetNumItem: SmallInt;
    function GetQuantidade: Extended;
    function GetValorProduto: Currency;
    function GetCFOP: SmallInt;
    procedure SetAliquota(const Valor: Currency);
    procedure SetBaseICMS(const Valor: Currency);
    procedure SetBaseICMSST(const Valor: Currency);
    procedure SetCodProduto(const Valor: string);
    procedure SetCST(const Valor: string);
    procedure SetDesconto(const Valor: Currency);
    procedure SetIPI(const Valor: Currency);
    procedure SetNumItem(const Valor: SmallInt);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetValorProduto(const Valor: Currency);
    procedure SetCFOP(const Valor: SmallInt);
  public
    property NumItem: SmallInt read GetNumItem write SetNumItem default 0;
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property CST: string read GetCST write SetCST;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorProduto: Currency read GetValorProduto write SetValorProduto;
    property Desconto: Currency read GetDesconto write SetDesconto;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property BaseICMSST: Currency read GetBaseICMSST write SetBaseICMSST;
    property IPI: Currency read GetIPI write SetIPI;
    property Aliquota: Currency read GetAliquota write SetAliquota;
  end;

  TRegistro54Lista = class(TInterfaceList, IRegistro54Lista)
  private
    function GetItem(Index: Integer): IRegistro54;
    procedure SetItem(Index: Integer; const Value: IRegistro54);
  public
    function New: IRegistro54;
    property Items[Index: Integer]: IRegistro54 read GetItem write SetItem;
  end;

  TRegistro55 = class(TSintegraObject, IRegistro55)
  private
    fMesRef: SmallInt;
    fAnoRef: SmallInt;
    fNumConvenio: string;
    fVencimento: TDateTime;
    fDataPagamento: TDateTime;
    fUFContribuinte: string;
    fUFFavorecido: string;
    fBanco: SmallInt;
    fAgencia: SmallInt;
    fNumeroGNRE: string;
    fValor: Currency;
    function GetAgencia: SmallInt;
    function GetAnoRef: SmallInt;
    function GetBanco: SmallInt;
    function GetDataPagamento: TDateTime;
    function GetMesRef: SmallInt;
    function GetNumConvenio: string;
    function GetNumeroGNRE: string;
    function GetUFContribuinte: string;
    function GetUFFavorecido: string;
    function GetValor: Currency;
    function GetVencimento: TDateTime;
    procedure SetAgencia(const Valor: SmallInt);
    procedure SetAnoRef(const Valor: SmallInt);
    procedure SetBanco(const Valor: SmallInt);
    procedure SetDataPagamento(const Valor: TDateTime);
    procedure SetMesRef(const Valor: SmallInt);
    procedure SetNumConvenio(const Valor: string);
    procedure SetNumeroGNRE(const Valor: string);
    procedure SetUFContribuinte(const Valor: string);
    procedure SetUFFavorecido(const Valor: string);
    procedure SetValor(const Valor: Currency);
    procedure SetVencimento(const Valor: TDateTime);
  public
    property MesRef: SmallInt read GetMesRef write SetMesRef default 0;
    property AnoRef: SmallInt read GetAnoRef write SetAnoRef default 0;
    property NumConvenio: string read GetNumConvenio write SetNumConvenio;
    property Vencimento: TDateTime read GetVencimento write SetVencimento;
    property DataPagamento: TDateTime read GetDataPagamento write SetDataPagamento;
    property UFContribuinte: string read GetUFContribuinte write SetUFContribuinte;
    property UFFavorecido: string read GetUFFavorecido write SetUFFavorecido;
    property Banco: SmallInt read GetBanco write SetBanco;
    property Agencia: SmallInt read GetAgencia write SetAgencia;
    property NumeroGNRE: string read GetNumeroGNRE write SetNumeroGNRE;
    property Valor: Currency read GetValor write SetValor;
  end;

  TRegistro55Lista = class(TInterfaceList, IRegistro55Lista)
  private
    function GetItem(Index: Integer): IRegistro55;
    procedure SetItem(Index: Integer; const Value: IRegistro55);
  public
    function New: IRegistro55;
    property Items[Index: Integer]: IRegistro55 read GetItem write SetItem;
  end;

  TRegistro71 = class(TSintegraObject, IRegistro71)
  private
    FUF: String;
    FCNPJ: String;
    FInscEstadual: String;
    FData: TDateTime;
    FModelo: SmallInt;
    FNumero: Integer;
    FSerie: String;
    FValorTotal: Currency;
    function GetCNPJ: string;
    procedure SetCNPJ(const Valor: string);
    function GetInscEstadual: string;
    procedure SetInscEstadual(const Valor: string);
    function GetData: TDateTime;
    procedure SetData(const Valor: TDateTime);
    function GetUF: string;
    procedure SetUF(const Valor: string);
    function GetModelo: SmallInt;
    procedure SetModelo(const Valor: SmallInt);
    function GetSerie: string;
    procedure SetSerie(const Valor: string);
    function GetNumero: Integer;
    procedure SetNumero(const Valor: Integer);
    function GetValorTotal: Currency;
    procedure SetValorTotal(const Value: Currency);
  public
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property Data: TDateTime read GetData write SetData;
    property UF: string read GetUF write SetUF;
    property Modelo: SmallInt read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
    property Numero: Integer read GetNumero write SetNumero;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
  end;

  TRegistro71Lista = class(TInterfaceList, IRegistro71Lista)
  private
    function GetItem(Index: Integer): IRegistro71;
    procedure SetItem(Index: Integer; const Value: IRegistro71);
  public
    function New: IRegistro71;
    property Items[Index: Integer]: IRegistro71 read GetItem write SetItem;
  end;

  TRegistro5X = class(TSintegraObject, IRegistro5X)
  private
    fCFOP: SmallInt;
    fCNPJ: string;
    fInscEstadual: string;
    fData: TDateTime;
    fUF: string;
    fModelo: SmallInt;
    fSerie: string;
    fSubSerie: string;
    fNumero: Integer;
    fTipoEmitente: TTipoEmitente;
    fSeguro: Currency;
    fFrete: Currency;
    fDespAcessoria: Currency;
    fValorPisCofins: Currency;
    fValorComplementar: Currency;
    fValorServicoNaoTributado: Currency;
    fSituacao: TSituacao;
    fRegistro50: IRegistro50Lista;
    fRegistro54: IRegistro54Lista;
    fRegistro51: IRegistro51Lista;
    fRegistro53: IRegistro53Lista;
    fRegistro71: IRegistro71Lista;
    FModFrete: TModalidadeFrete;
    function GetCFOP: SmallInt;
    procedure SetCFOP(const Valor: SmallInt);
    function GetCNPJ: string;
    procedure SetCNPJ(const Valor: string);
    function GetInscEstadual: string;
    procedure SetInscEstadual(const Valor: string);
    function GetData: TDateTime;
    procedure SetData(const Valor: TDateTime);
    function GetUF: string;
    procedure SetUF(const Valor: string);
    function GetModelo: SmallInt;
    procedure SetModelo(const Valor: SmallInt);
    function GetSerie: string;
    procedure SetSerie(const Valor: string);
    function GetNumero: Integer;
    procedure SetNumero(const Valor: Integer);
    function GetTipoEmitente: TTipoEmitente;
    procedure SetTipoEmitente(const Valor: TTipoEmitente);
    function GetSituacao: TSituacao;
    procedure SetSituacao(const Valor: TSituacao);
    function GetRegistro54: IRegistro54Lista;
    procedure SetRegistro54(const Valor: IRegistro54Lista);
    function GetRegistro50: IRegistro50Lista;
    procedure SetRegistro50(const Valor: IRegistro50Lista);
    function GetDespAcessoria: Currency;
    procedure SetDespAcessoria(const Valor: Currency);
    function GetValorPisCofins: Currency;
    procedure SetValorPisCofins(const Valor: Currency);
    function GetValorComplementar: Currency;
    procedure SetValorComplementar(const Valor: Currency);
    function GetValorServicoNaoTributado: Currency;
    procedure SetValorServicoNaoTributado(const Valor: Currency);
    function GetFrete: Currency;
    procedure SetFrete(const Valor: Currency);
    function GetSeguro: Currency;
    procedure SetSeguro(const Valor: Currency);
    function GetRegistro51: IRegistro51Lista;
    procedure SetRegistro51(const Valor: IRegistro51Lista);
    function GetRegistro53: IRegistro53Lista;
    procedure SetRegistro53(const Valor: IRegistro53Lista);
    function GetModFrete: TModalidadeFrete;
    function GetSubSerie: string;
    procedure SetModFrete(const Value: TModalidadeFrete);
    procedure SetSubSerie(const Value: string);
    function GetRegistro71: IRegistro71Lista;
    procedure SetRegistro71(const Valor: IRegistro71Lista);
  public
    constructor Create;
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property Data: TDateTime read GetData write SetData;
    property UF: string read GetUF write SetUF;
    property Modelo: SmallInt read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
    property SubSerie: string read GetSubSerie write SetSubSerie;
    property Numero: Integer read GetNumero write SetNumero;
    property TipoEmitente: TTipoEmitente read GetTipoEmitente write SetTipoEmitente default tpeProprio;
    property ModFrete: TModalidadeFrete read GetModFrete write SetModFrete;
    property Frete: Currency read GetFrete write SetFrete;
    property Seguro: Currency read GetSeguro write SetSeguro;
    property DespAcessoria: Currency read GetDespAcessoria write SetDespAcessoria;
    property ValorPisCofins: Currency read GetValorPisCofins write SetValorPisCofins;
    property ValorComplementar: Currency read GetValorComplementar write SetValorComplementar;
    property ValorServicoNaoTributado: Currency read GetValorServicoNaoTributado write SetValorServicoNaoTributado;
    property Situacao: TSituacao read GetSituacao write SetSituacao default nfNormal;
    property Registro50: IRegistro50Lista read GetRegistro50 write SetRegistro50;
    property Registro54: IRegistro54Lista read GetRegistro54 write SetRegistro54;
    property Registro51: IRegistro51Lista read GetRegistro51 write SetRegistro51;
    property Registro53: IRegistro53Lista read GetRegistro53 write SetRegistro53;
    property Registro71: IRegistro71Lista read GetRegistro71 write SetRegistro71;
  end;

  TRegistro5XLista = class(TInterfaceList, IRegistro5XLista)
  private
    function GetItem(Index: Integer): IRegistro5X;
    procedure SetItem(Index: Integer; const Value: IRegistro5X);
    function GetTotalEntradas: Integer;
    function GetTotalSaidas: Integer;
  public
    function New: IRegistro5X;
    property Items[Index: Integer]: IRegistro5X read GetItem write SetItem;
    property TotalEntradas: Integer read GetTotalEntradas;
    property TotalSaidas: Integer read GetTotalSaidas;
  end;

  TRegistro60M = class(TSintegraObject, IRegistro60M)
  private
    fDataEmissao: TDateTime;
    fNumSerieEquip: string;
    fNumSequencial: Integer;
    fModDocFiscal: TModDocumento;
    fCOOInicial: Integer;
    fCOOFinal: Integer;
    fContReducaoZ: Integer;
    fContReinicioOper: Integer;
    fVendaBruta: Currency;
    fGTFinal: Currency;
    fCFICMS: IRegistro60ALista;
    fResumoDia: IRegistro60DLista;
    fCFItem: IRegistro60ILista;
    function GetDataEmissao: TDateTime;
    procedure SetDataEmissao(const Valor: TDatetime);
    function GetNumSerieEquip: string;
    procedure SetNumSerieEquip(const Valor: string);
    function GetNumSequencial: Integer;
    procedure SetNumSequencial(const Valor: Integer);
    function GetModDocFiscal: TModDocumento;
    procedure SetModDocFiscal(const Valor: TModDocumento);
    function GetCOOInicial: Integer;
    procedure SetCOOInicial(const Valor: Integer);
    function GetCOOFinal: Integer;
    procedure SetCOOFinal(const Valor: Integer);
    function GetContReducaoZ: Integer;
    procedure SetContReducaoZ(const Valor: Integer);
    function GetContReinicioOper: Integer;
    procedure SetContReinicioOper(const Valor: Integer);
    function GetVendaBruta: Currency;
    procedure SetVendaBruta(const Valor: Currency);
    function GetGTFinal: Currency;
    procedure SetGTFinal(const Valor: Currency);
    function GetRegistro60A: IRegistro60ALista;
    procedure SetRegistro60A(const Valor: IRegistro60ALista);
    function GetRegistro60D: IRegistro60DLista;
    procedure SetRegistro60D(const Valor: IRegistro60DLista);
    function GetRegistro60I: IRegistro60ILista;
    procedure SetRegistro60I(const Valor: IRegistro60ILista);
  public
    constructor Create;
    property DataEmissao: TDateTime read GetDataEmissao write SetDataEmissao;
    property NumSerieEquip: string read GetNumSerieEquip write SetNumSerieEquip;
    property NumSequencial: Integer read GetNumSequencial write SetNumSequencial default 0;
    property ModDocFiscal: TModDocumento read GetModDocFiscal write SetModDocFiscal;
    property COOInicial: Integer read GetCOOInicial write SetCOOInicial default 0;
    property COOFinal: Integer read GetCOOFinal write SetCOOFinal default 0;
    property ContReducaoZ: Integer read GetContReducaoZ write SetContReducaoZ default 0;
    property ContReinicioOper: Integer read GetContReinicioOper write SetContReinicioOper default 0;
    property VendaBruta: Currency read GetVendaBruta write SetVendaBruta;
    property GTFinal: Currency read GetGTFinal write SetGTFinal;
    property Registro60A: IRegistro60ALista read GetRegistro60A write SetRegistro60A;
    property CRegistro60D: IRegistro60DLista read GetRegistro60D write SetRegistro60D;
    property Registro60I: IRegistro60ILista read GetRegistro60I write SetRegistro60I;
  end;

  TRegistro60MLista = class(TInterfaceList, IRegistro60MList)
  private
    function GetItem(Index: Integer): IRegistro60M;
    procedure SetItem(Index: Integer; const Value: IRegistro60M);
  public
    function New: IRegistro60M;
    property Items[Index: Integer]: IRegistro60M read GetItem write SetItem;
  end;

  TRegistro60A = class(TSintegraObject, IRegistro60A)
  private
    fSitTributaria: string;
    fValorAcumulado: Currency;
    function GetSitTributaria: string;
    procedure SetSitTributaria(const Valor: string);
    function GetValorAcumulado: Currency;
    procedure SetValorAcumulado(const Valor: Currency);
  public
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
    property ValorAcumulado: Currency read GetValorAcumulado write SetValorAcumulado;
  end;

  TRegistro60ALista = class(TInterfaceList, IRegistro60ALista)
  private
    function GetItem(Index: Integer): IRegistro60A;
    procedure SetItem(Index: Integer; const Value: IRegistro60A);
    function Total: Currency;
  public
    function New: IRegistro60A;
    property Items[Index: Integer]: IRegistro60A read GetItem write SetItem;
  end;

  TRegistro60I = class(TSintegraObject, IRegistro60I)
  private
    fCOOCupom: Integer;
    fNumItem: Integer;
    fCodProduto: string;
    fQuantidade: Extended;
    fValorUnitario: Currency;
    fBaseICMS: Currency;
    fSitTributaria: string;
    fValorICMS: Currency;
    function GetBaseICMS: Currency;
    function GetCodProduto: string;
    function GetCOOCupom: Integer;
    function GetNumItem: Integer;
    function GetQuantidade: Extended;
    function GetSitTributaria: string;
    function GetValorICMS: Currency;
    function GetValorUnitario: Currency;
    procedure SetCodProduto(const Valor: string);
    procedure SetCOOCupom(const Valor: Integer);
    procedure SetNumItem(const Valor: Integer);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetSitTributaria(const Valor: string);
    procedure SetValorUnitario(const Valor: Currency);
    procedure SetValorICMS(const Valor: Currency);
    procedure SetBaseICMS(const Valor: Currency);
  public
    property COOCupom: Integer read GetCOOCupom write SetCOOCupom;
    property NumItem: Integer read GetNumItem write SetNumItem;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorUnitario: Currency read GetValorUnitario write SetValorUnitario;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
    property ValorICMS: Currency read GetValorICMS write SetValorICMS;
  end;

  TRegistro60ILista = class(TInterfaceList, IRegistro60ILista)
  private
    function GetItem(Index: Integer): IRegistro60I;
    procedure SetItem(Index: Integer; const Value: IRegistro60I);
  public
    function New: IRegistro60I;
    property Items[Index: Integer]: IRegistro60I read GetItem write SetItem;
  end;

  TRegistro60D = class(TSintegraObject, IRegistro60D)
  private
    fCodProduto: string;
    fQuantAcumulada: Extended;
    fValorAcumulado: Currency;
    fBaseCalcICMS: Currency;
    fSitTributaria: string;
    function GetSitTributaria: string;
    function GetBaseCalcICMS: Currency;
    function GetCodProduto: string;
    function GetQuantAcumulada: Extended;
    function GetValorAcumulado: Currency;
    function GetValorICMS: Currency;
    procedure SetSitTributaria(const Valor: string);
    procedure SetBaseCalcICMS(const Valor: Currency);
    procedure setCodProduto(const Valor: string);
    procedure SetQuantAcumulada(const Valor: Extended);
    procedure SetValorAcumulado(const Valor: Currency);
  public
    constructor Create;
    property CodProduto: string read GetCodProduto write setCodProduto;
    property QuantAcumulada: Extended read GetQuantAcumulada write SetQuantAcumulada;
    property ValorAcumulado: Currency read GetValorAcumulado write SetValorAcumulado;
    property BaseCalcICMS: Currency read GetBaseCalcICMS write SetBaseCalcICMS;
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
    property ValorICMS: Currency read GetValorICMS;
  end;

  TRegistro60DLista = class(TInterfaceList, IRegistro60DLista)
  private
    function GetItem(Index: Integer): IRegistro60D;
    procedure SetItem(Index: Integer; const Value: IRegistro60D);
  public
    function New: IRegistro60D;
    function GetTotalAcumulado: Currency;
    property Items[Index: Integer]: IRegistro60D read GetItem write SetItem;
    property TotalAcumulado: Currency read GetTotalAcumulado;
  end;

  TRegistro60R = class(TSintegraObject, IRegistro60R)
  private
    fMes: SmallInt;
    fANo: SmallInt;
    fCodProduto: string;
    fQuantidade: Extended;
    fValorAcumProduto: Currency;
    fValorAcumICMS: Currency;
    fSitTributaria: string;
    function GetAno: SmallInt;
    function GetCodProduto: string;
    function GetMes: SmallInt;
    function GetQuantidade: Extended;
    function GetSitTributaria: string;
    function GetValorAcumICMS: Currency;
    function GetValorAcumProduto: Currency;
    procedure SetAno(const Valor: SmallInt);
    procedure SetCodProduto(const Valor: string);
    procedure SetMes(const Valor: SmallInt);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetSitTributaria(const Valor: string);
    procedure SetValorAcumICMS(const Valor: Currency);
    procedure SetValorAcumProduto(const Valor: Currency);
  public
    property Mes: SmallInt read GetMes write SetMes default 0;
    property ANo: SmallInt read GetAno write SetAno default 0;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorAcumProduto: Currency read GetValorAcumProduto write SetValorAcumProduto;
    property ValorAcumICMS: Currency read GetValorAcumICMS write SetValorAcumICMS;
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
  end;

  TRegistro60RLista = class(TInterfaceList, IRegistro60RLista)
  private
    function GetItem(Index: Integer): IRegistro60R;
    procedure SetItem(Index: Integer; const Value: IRegistro60R);
  public
    function New: IRegistro60R;
    property Items[Index: Integer]: IRegistro60R read GetItem write SetItem;
  end;

  TRegistro61 = class(TSintegraObject, IRegistro61)
  private
    fDataEmissao: TDateTime;
    fModeloDoc: Integer;
    fSerie: string;
    fSubSerie: string;
    fNumInicial: Integer;
    fNumFinal: Integer;
    fValorTotal: Currency;
    fBaseICMS: Currency;
    fIsentasNTrib: Currency;
    fOutras: Currency;
    fAliquota: Currency;
    function GetDataEmissao: TDateTime;
    procedure SetDataEmissao(const Valor: TDateTime);
    function GetModeloDoc: Integer;
    procedure SetModeloDoc(const Valor: Integer);
    function GetSerie: string;
    procedure SetSerie(const Valor: string);
    function GetSubSerie: string;
    procedure SetSubSerie(const Valor: string);
    function GetNumInicial: Integer;
    procedure SetNumInicial(const Valor: Integer);
    function GetNumFinal: Integer;
    procedure SetNumFinal(const Valor: Integer);
    function GetValorTotal: Currency;
    procedure SetValorTotal(const Valor: Currency);
    function GetBaseICMS: Currency;
    procedure SetBaseICMS(const Valor: Currency);
    function GetIsentasNTrib: Currency;
    procedure SetIsentasNTrib(const Valor: Currency);
    function GetOutras: Currency;
    procedure SetOutras(const Valor: Currency);
    function GetAliquota: Currency;
    procedure SetAliquota(const Valor: Currency);
    function GetValorICMS: Currency;
  public
    constructor Create;
    property DataEmissao: TDateTime read GetDataEmissao write SetDataEmissao;
    property ModeloDoc: Integer read GetModeloDoc write SetModeloDoc default 0;
    property Serie: string read GetSerie write SetSerie;
    property SubSerie: string read GetSubSerie write SetSubSerie;
    property NumInicial: Integer read GetNumInicial write SetNumInicial default 0;
    property NumFinal: Integer read GetNumFinal write SetNumFinal default 0;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property IsentasNTrib: Currency read GetIsentasNTrib write SetIsentasNTrib;
    property Outras: Currency read GetOutras write SetOutras;
    property Aliquota: Currency read GetAliquota write SetAliquota;
    property ValorICMS: currency read GetValorICMS;
  end;

  TRegistro61Lista = class(TInterfaceList, IRegistro61Lista)
  private
    function GetItem(Index: Integer): IRegistro61;
    procedure SetItem(Index: Integer; const Value: IRegistro61);
  public
    function New: IRegistro61;
    property Items[Index: Integer]: IRegistro61 read GetItem write SetItem;
  end;

  TRegistro61R = class(TSintegraObject, IRegistro61R)
  private
    fMes: SmallInt;
    fAno: SmallInt;
    fCodProduto: string;
    fQuantidade: Extended;
    fValorBrutoProduto: Currency;
    fBaseCalcICMS: Currency;
    fAliquota: Currency;
    function GetMes: SmallInt;
    function GetAno: SmallInt;
    function GetCodProduto: string;
    function GetQuantidade: Extended;
    function GetValorBrutoProduto: Currency;
    function GetBaseCalcICMS: Currency;
    function GetAliquota: Currency;
    procedure SetMes(const Valor: SmallInt);
    procedure SetAno(const Valor: SmallInt);
    procedure SetCodProduto(const Valor: string);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetValorBrutoProduto(const Valor: Currency);
    procedure SetBaseCalcICMS(const Valor: Currency);
    procedure SetAliquota(const Valor: Currency);
  public
    property Mes: SmallInt read GetMes write SetMes default 0;
    property Ano: SmallInt read GetAno write SetAno default 0;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorBrutoProduto: Currency read GetValorBrutoProduto write SetValorBrutoProduto;
    property BaseCalcICMS: Currency read GetBaseCalcICMS write SetBaseCalcICMS;
    property Aliquota: Currency read GetAliquota write SetAliquota;
  end;

  TRegistro61RLista = class(TInterfaceList, IRegistro61RLista)
  private
    function GetItem(Index: Integer): IRegistro61R;
    procedure SetItem(Index: Integer; const Value: IRegistro61R);
  public
    function New: IRegistro61R;
    property Items[Index: Integer]: IRegistro61R read GetItem write SetItem;
  end;

  TRegistro74 = class(TSintegraObject, IRegistro74)
  private
    fTipoPosse: TTipoPosse;
    fCNPJ: string;
    fInscEstadual: string;
    fUF: string;
    fDataInventario: TDateTime;
    fCodProduto: string;
    fQuantidade: Extended;
    fValorTotal: Currency;
    function GetCNPJ: string;
    function GetCodPRoduto: string;
    function GetDataInventario: TDateTime;
    function GetInscEstadual: string;
    function GetQuantidade: Extended;
    function GetTipoPosse: TTipoPosse;
    function GetUF: string;
    function GetValorTotal: Currency;
    procedure SetCNPJ(const Valor: string);
    procedure SetCodPRoduto(const Valor: string);
    procedure SetDataInventario(const Valor: TDateTime);
    procedure SetInscEstadual(const Valor: string);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetTipoPosse(const Valor: TTipoPosse);
    procedure SetUF(const Valor: string);
    procedure SetValorTotal(const Valor: Currency);
  public
    property TipoPosse: TTipoPosse read GetTipoPosse write SetTipoPosse default tpo1;
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property UF: string read GetUF write SetUF;
    property DataInventario: TDateTime read GetDataInventario write SetDataInventario;
    property CodProduto: string read GetCodPRoduto write SetCodPRoduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
  end;

  TRegistro74Lista = class(TInterfaceList, IRegistro74Lista)
  private
    function GetItem(Index: Integer): IRegistro74;
    procedure SetItem(Index: Integer; const Value: IRegistro74);
  public
    function New: IRegistro74;
    property Items[Index: Integer]: IRegistro74 read GetItem write SetItem;
  end;

  TRegistro75 = class(TSintegraObject, IRegistro75)
  private
    fValidadeInicial: TDateTime;
    fValidadeFinal: TDateTime;
    fCodProduto: string;
    fCodNCM: string;
    fDescricao: string;
    fUnidade: string;
    fAliquotaIPI: Currency;
    fAliquotaICMS: Currency;
    fReducaoBaseCalc: Currency;
    fBaseICMSST: Currency;
    function GetAliquotaICMS: Currency;
    function GetAliquotaIPI: Currency;
    function GetBaseICMSST: Currency;
    function GetCodNCM: string;
    function GetCodProduto: string;
    function GetDescricao: string;
    function GetReducaoBaseCalc: Currency;
    function GetUnidade: string;
    function GetValidadeFinal: TDateTime;
    function GetValidadeInicial: TDateTime;
    procedure SetAliquotaICMS(const Valor: Currency);
    procedure SetAliquotaIPI(const Valor: Currency);
    procedure SetBaseICMSST(const Valor: Currency);
    procedure SetCodNCM(const Valor: string);
    procedure SetCodProduto(const Valor: string);
    procedure SetDescricao(const Valor: string);
    procedure SetReducaoBaseCalc(const Valor: Currency);
    procedure SetUnidade(const Valor: string);
    procedure SetValidadeFinal(const Valor: TDateTime);
    procedure SetValidadeInicial(const Valor: TDateTime);
  public
    property ValidadeInicial: TDateTime read GetValidadeInicial write SetValidadeInicial;
    property ValidadeFinal: TDateTime read GetValidadeFinal write SetValidadeFinal;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property CodNCM: string read GetCodNCM write SetCodNCM;
    property Descricao: string read GetDescricao write SetDescricao;
    property Unidade: string read GetUnidade write SetUnidade;
    property AliquotaIPI: Currency read GetAliquotaIPI write SetAliquotaIPI;
    property AliquotaICMS: Currency read GetAliquotaICMS write SetAliquotaICMS;
    property ReducaoBaseCalc: Currency read GetReducaoBaseCalc write SetReducaoBaseCalc;
    property BaseICMSST: Currency read GetBaseICMSST write SetBaseICMSST;
  end;

  TRegistro75Lista = class(TInterfaceList, IRegistro75Lista)
  private
    function GetItem(Index: Integer): IRegistro75;
    procedure SetItem(Index: Integer; const Value: IRegistro75);
  public
    function New: IRegistro75;
    function ExisteProduto(const AProduto: IRegistro75): Integer;
    property Items[Index: Integer]: IRegistro75 read GetItem write SetItem;
  end;

  TRegistro85 = class(TSintegraObject, IRegistro85)
  private
    fDeclaracao: string;
    fDataDeclaracao: TDateTime;
    fNaturezaExportacao: string;
    fRegistroExportacao: String;
    fDataRegistro: TDateTime;
    fConhecimento:string;
    fDataConhecimento:TDateTime;
    fTipoConhecimento: String;
    fPais:string;
    fDataAverbacao:TDateTime;
    fNumeroNotaFiscal:String;
    fDataNotaFiscal:TDateTime;
    fModelo: string;
    fSerie: string;
    function GetConhecimento: string;
    function GetDataAverbacao: TDateTime;
    function GetDataConhecimento: TDateTime;
    function GetDataDeclaracao: TDateTime;
    function GetDataNotaFiscal: TDateTime;
    function GetDataRegistro: TDateTime;
    function GetDeclaracao: String;
    function GetModelo: string;
    function GetNaturezaExportacao: string;
    function GetNumeroNotaFiscal: string;
    function GetPais: string;
    function GetRegistroExportacao: String;
    function GetSerie: string;
    function GetTipoConhecimento: String;
    procedure SetConhecimento(const Value: string);
    procedure SetDataAverbacao(const Value: TDateTime);
    procedure SetDataConhecimento(const Value: TDateTime);
    procedure SetDataDeclaracao(const Value: TDateTime);
    procedure SetDataNotaFiscal(const Value: TDateTime);
    procedure SetDataRegistro(const Value: TDateTime);
    procedure SetDeclaracao(const Value: String);
    procedure SetModelo(const Value: string);
    procedure SetNaturezaExportacao(const Value: string);
    procedure SetNumeroNotaFiscal(const Value: string);
    procedure SetPais(const Value: string);
    procedure SetRegistroExportacao(const Value: String);
    procedure SetSerie(const Value: string);
    procedure SetTipoConhecimento(const Value: String);
  public
    property Declaracao: String read GetDeclaracao write SetDeclaracao;
    property DataDeclaracao: TDateTime read GetDataDeclaracao write SetDataDeclaracao;
    property NaturezaExportacao: string read GetNaturezaExportacao write SetNaturezaExportacao;
    property RegistroExportacao: String read GetRegistroExportacao write SetRegistroExportacao;
    property DataRegistro: TDateTime read GetDataRegistro write SetDataRegistro;
    property Conhecimento: string read GetConhecimento write SetConhecimento;
    property DataConhecimento: TDateTime read GetDataConhecimento write SetDataConhecimento;
    property TipoConhecimento: String read GetTipoConhecimento write SetTipoConhecimento;
    property Pais: string read GetPais write SetPais;
    property DataAverbacao: TDateTime read GetDataAverbacao write SetDataAverbacao;
    property NumeroNotaFiscal: string read GetNumeroNotaFiscal write SetNumeroNotaFiscal;
    property DataNotaFiscal: TDateTime read GetDataNotaFiscal write SetDataNotaFiscal;
    property Modelo: string read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
  end;

  TRegistro85Lista = class(TInterfaceList, IRegistro85Lista)
  private
    function GetItem(Index: Integer): IRegistro85;
    procedure SetItem(Index: Integer; const Value: IRegistro85);
  public
    function New: IRegistro85;
    property Items[Index: Integer]: IRegistro85 read GetItem write SetItem;
  end;

  TRegistro86 = class(TSintegraObject, IRegistro86)
  private
    fRegistroExportacao:string;
    fDataRegistro:TDateTime;
    fCPFCNPJ: string;
    fInscricao: string;
    fUF: string;
    fNumeroNotaFiscal: string;
    fDataDocumento: TDateTime;
    fModelo: string;
    fSerie: string;
    fCodigo: string;
    fQuantidade: Currency;
    fValorUnitario :Currency;
    fValorTotalProduto: Currency;
    fRelacionamento: string;
    function GetCodigo: string;
    function GetCPFCNPJ: string;
    function GetDataDocumento: TDateTime;
    function GetDataRegistro: TDateTime;
    function GetInscricao: string;
    function GetModelo: string;
    function GetNumeroNotaFiscal: string;
    function GetQuantidade: Currency;
    function GetRegistroExportacao: string;
    function GetRelacionamento: string;
    function GetSerie: string;
    function GetUF: string;
    function GetValorTotalProduto: Currency;
    function GetValorUnitario: Currency;
    procedure SetCodigo(const Value: string);
    procedure SetCPFCNPJ(const Value: string);
    procedure SetDataDocumento(const Value: TDateTime);
    procedure SetDataRegistro(const Value: TDateTime);
    procedure SetInscricao(const Value: string);
    procedure SetModelo(const Value: string);
    procedure SetNumeroNotaFiscal(const Value: string);
    procedure SetQuantidade(const Value: Currency);
    procedure SetRegistroExportacao(const Value: string);
    procedure SetRelacionamento(const Value: string);
    procedure SetSerie(const Value: string);
    procedure SetUF(const Value: string);
    procedure SetValorTotalProduto(const Value: Currency);
    procedure SetValorUnitario(const Value: Currency);
  public
    property RegistroExportacao: string read GetRegistroExportacao write SetRegistroExportacao;
    property DataRegistro: TDateTime read GetDataRegistro write SetDataRegistro;
    property CPFCNPJ: string read GetCPFCNPJ write SetCPFCNPJ;
    property Inscricao: string read GetInscricao write SetInscricao;
    property UF: string read GetUF write SetUF;
    property NumeroNotaFiscal: string read GetNumeroNotaFiscal write SetNumeroNotaFiscal;
    property DataDocumento: TDateTime read GetDataDocumento write SetDataDocumento;
    property Modelo: string read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
    property Codigo: string read GetCodigo write SetCodigo;
    property Quantidade: Currency read GetQuantidade write SetQuantidade;
    property ValorUnitario: Currency read GetValorUnitario write SetValorUnitario;
    property ValorTotalProduto: Currency read GetValorTotalProduto write SetValorTotalProduto;
    property Relacionamento: string read GetRelacionamento write SetRelacionamento;
  end;

  TRegistro86Lista = class(TInterfaceList, IRegistro86Lista)
  private
    function GetItem(Index: Integer): IRegistro86;
    procedure SetItem(Index: Integer; const Value: IRegistro86);
  public
    function New: IRegistro86;
    property Items[Index: Integer]: IRegistro86 read GetItem write SetItem;
  end;

{ TSintegraObject }

function TSintegraObject.RFIll(Str: string; Tamanho: Integer = 0; Caracter: Char = ' '): string;
begin
  if (Tamanho > 0) and (Length(Str) > Tamanho) then
    Result := Copy(Str, 1, Tamanho)
  else
    Result := Str + StringOfChar(Caracter, Tamanho - Length(Str));

  if Debug then
    Result := Result + '|';
end;

function TSintegraObject.LFill(Str: string; Tamanho: Integer = 0; Caracter: Char = '0'): string;
begin
  if (Tamanho > 0) and (Length(Str) > Tamanho) then
    Result := Copy(Str, 1, Tamanho)
  else
    Result := StringOfChar(Caracter, Tamanho - length(Str)) + Str;

  if Debug then
    Result := Result + '|';
end;

function TSintegraObject.LFill(Valor: Currency; Tamanho: Integer; Decimais: Integer = 2; Caracter: Char = '0'): string;
var
intFor, intP: Integer;
begin
  intP := 1;
  //
  for intFor := 1 to Decimais do
  begin
     intP := intP * 10;
  end;
  Result := LFill(Trunc(Valor * intP), Tamanho, Caracter);
end;

function TSintegraObject.LFill(Valor: Integer; Tamanho: Integer; Caracter: Char = '0'): string;
begin
  Result := LFill(IntToStr(Valor), Tamanho, Caracter);
end;

function TSintegraObject.LFill(Valor: TDateTime): string;
begin
  Result := FormatDateTime('yyyymmdd', Valor);

  if Debug then
    Result := Result + '|';
end;

function TSintegraObject.RetornaNatureza(aNaturezaOper: TCodIdentificaOper): string;
begin
  case aNaturezaOper of
    opeInterestSubTributaria: Result := '1';
    opeInterestaduais: Result := '2';
    opeTotal: Result := '3';
  end;
end;

function TSintegraObject.RetornaFinalidade(aFinalidade: TCodFinalidade): string;
begin
  case aFinalidade of
    finNormal: Result := '1';
    finRetificacaoTotal: Result := '2';
    finRetificacaoAditiva: Result := '3';
    finRetificacaoCorretiva: Result := '4';
    finDesfazimento: Result := '5';
  end;
end;

function TSintegraObject.RetornaModDocumento(aTipoDocumento: TModDocumento): string;
begin
  case aTipoDocumento of
    modMaqRegistradora: Result := '2B';
    modPDV: Result := '2C';
    modECF: Result := '2D';
  end;
end;

function TSintegraObject.RetornaTipoEmitente(aTipoEmitente: TTipoEmitente): string;
begin
  case aTipoEmitente of
    tpeProprio: Result := 'P';
    tpeTerceiros: Result := 'T';
  end;
end;

function TSintegraObject.RetornaSituacao(aSituacao: TSituacao): string;
begin
  case aSituacao of
    nfNormal: Result := 'N';
    nfCancelado: Result := 'S';
    nfExtNormal: Result := 'E';
    nfExtCancelado: Result := 'X';
  end;
end;

function TSintegraObject.RetornaFrete(aModFrete: TModalidadeFrete): string;
begin
  case aModFrete of
    mdfCIF: Result := '1';
    mdfFOB: Result := '2';
    mdfOUTROS: Result := '0';
  end;
end;

function TSintegraObject.StrToNumero(aValor: string; aDecimais: Integer): Extended;
var
  i, iInicio, iFim: Integer;
  str: string;
begin
  str := '0';
  if Trim(aValor) <> '' then
  begin
    for i := 0 to Length(aValor) do
      if aValor[i] in ['0'..'9', '-'] then
        str := str + avalor[i];

    if (Trim(str) <> '') and (Trim(str) <> '-') then
    begin
      iInicio := Length(str) - aDecimais;
      iFim := Length(str);
      str := copy(str, 0, iInicio) + DecimalSeparator + Copy(str, iInicio + 1, iFim);
    end;
  end;

  Result := StrTofloat(str);
end;

function TSintegraObject.VerificaSitTributaria(aSitTributaria: string): Boolean;
var
curAliquota: Currency;
booValida: Boolean;
begin
  curAliquota := StrToNumero(aSitTributaria, 2);

  if curAliquota = 0 then
  begin
    if aSitTributaria = 'F' then
       booValida := True
    else
    if aSitTributaria = 'I' then
       booValida := True
    else
    if aSitTributaria = 'N' then
       booValida := True
    else
    if aSitTributaria = 'CANC' then
       booValida := True
    else
    if aSitTributaria = 'DESC' then
       booValida := True
    else
    if aSitTributaria = 'ISS' then
       booValida := True
    else
       booValida := False;
  end
  else
    booValida := True;
  //
  Result := booValida;
end;

function TSintegraObject.VerificaCST(aCST: string): Boolean;
const
  ListaCST: array[0..32] of string = (
    '000', '010', '020', '030', '040', '041', '050', '051',
    '060', '070', '090', '100', '110', '120', '130', '140',
    '141', '150', '151', '160', '170', '190', '200', '210',
    '220', '230', '240', '241', '250', '251', '260', '270',
    '290');
var
intFor: integer;
booEncontrado: Boolean;
begin
  intFor := 0;
  booEncontrado := False;
  while (not (booEncontrado)) and (intFor <= 32) do
  begin
    booEncontrado := ListaCST[intFor] = aCST;
    inc(intFor);
  end;

  Result := booEncontrado;
end;

function TSintegraObject.RetornaArquivoMagnetico(aTipo: TArquivoMagnetico): string;
begin
  case aTipo of
    tamConv1: Result := '1';
    tamConv2: Result := '2';
    tamConv3: Result := '3';
  end;
end;

function TSintegraObject.RetornaCodPosse(TipoPosse: TTipoPosse): string;
begin
  case TipoPosse of
    tpo1: Result := '1';
    tpo2: Result := '2';
    tpo3: Result := '3';
  end;
end;

function TSintegraObject.RetornaSoNumero(aNumero: string): string;
var
  intFor: integer;
begin
  Result := '';
  for intFor := 1 to Length(aNumero) do
  begin
    if aNumero[intFor] in ['0'..'9'] then
      Result := Result + aNumero[intFor];
  end;
end;

function TSintegraObject.RetiraMascara(aString: String): string;
var
  intFor: integer;
begin
  Result := '';
  for intFor := 1 to Length(aString) do
  begin
    if UpCase(aString[intFor]) in ['0'..'9', 'A'..'Z'] then
      Result := Result + aString[intFor];
  end;
end;

{ TRegistro1X }

function TRegistro1X.GetRazaoSocial: string;
begin
  Result := FRazaoSocial;
end;

procedure TRegistro1X.SetRazaoSocial(const Valor: string);
begin
  FRazaoSocial := Valor;
end;

function TRegistro1X.GetCNPJ: string;
begin
  Result := FCNPJ;
end;

procedure TRegistro1X.SetCNPJ(const Valor: string);
begin
  FCNPJ := RetornaSoNumero(Valor);
end;

function TRegistro1X.GetInscEstadual: string;
begin
  Result := FInscEstadual;
end;

procedure TRegistro1X.SetInscEstadual(const Valor: string);
begin
  FInscEstadual := RetiraMascara(Valor);
end;

function TRegistro1X.GetMunicipio: string;
begin
  Result := FMunicipio;
end;

procedure TRegistro1X.SetMunicipio(const Valor: string);
begin
  FMunicipio := Valor;
end;

function TRegistro1X.GetUF: string;
begin
  Result := FUF;
end;

procedure TRegistro1X.SetUF(const Valor: string);
begin
  FUF := UpperCase(Valor);
end;

function TRegistro1X.GetFax: string;
begin
  Result := FFax;
end;

procedure TRegistro1X.SetFax(const Valor: string);
begin
  FFax := Valor;
end;

function TRegistro1X.GetResponsavel: string;
begin
  Result := FResponsavel;
end;

procedure TRegistro1X.SetResponsavel(const Valor: string);
begin
  FResponsavel := Valor;
end;

function TRegistro1X.GetEndereco: string;
begin
  Result := FEndereco;
end;

procedure TRegistro1X.SetEndereco(const Valor: string);
begin
  FEndereco := Valor;
end;

function TRegistro1X.GetComplemento: string;
begin
  Result := FComplemento;
end;

procedure TRegistro1X.SetComplemento(const Valor: string);
begin
  FComplemento := Valor;
end;

function TRegistro1X.GetNumero: Integer;
begin
  Result := FNumero;
end;

procedure TRegistro1X.SetNumero(const Valor: Integer);
begin
  FNumero := Valor;
end;

function TRegistro1X.GetBairro: string;
begin
  Result := FBairro;
end;

procedure TRegistro1X.SetBairro(const Valor: string);
begin
  FBairro := Valor;
end;

function TRegistro1X.GetCEP: string;
begin
  Result := FCEP;
end;

procedure TRegistro1X.SetCEP(const Valor: string);
begin
  FCEP := Valor;
end;

function TRegistro1X.GetFone: string;
begin
  Result := FFone;
end;

procedure TRegistro1X.SetFone(const Valor: string);
begin
  FFone := RetornaSoNumero(Valor);
end;

function TRegistro1X.GetContribuinteIPI: Boolean;
begin
  Result := FContribuinteIPI;
end;

procedure TRegistro1X.SetContribuinteIPI(const Valor: Boolean);
begin
  FContribuinteIPI := Valor
end;

function TRegistro1X.GetSubstitutoTributario: Boolean;
begin
  Result := FSubstitutoTributario
end;

procedure TRegistro1X.SetSubstitutoTributario(const Valor: Boolean);
begin
  FSubstitutoTributario := Valor;
end;

{ TSintegra }

constructor TSintegra.Create;
begin
  inherited Create;

  FDataInicial := 0;
  FDataFinal := 0;

  FRegistro1X := TRegistro1X.Create;
  FRegistro5X := TRegistro5XLista.Create;
  FRegistro55 := TRegistro55Lista.Create;
  FRegistro60M := TRegistro60MLista.Create;
  FRegistro60R := TRegistro60RLista.Create;
  FRegistro61 := TRegistro61Lista.Create;
  FRegistro61R := TRegistro61RLista.Create;
  FRegistro74 := TRegistro74Lista.Create;
  FRegistro75 := TRegistro75Lista.Create;
  FRegistro85 := TRegistro85Lista.Create;
  FRegistro86 := TRegistro86Lista.Create;
end;

destructor TSintegra.Destroy;
begin
  //
  inherited;
end;

function TSintegra.GetOnErro: TErrorEvent;
begin
  Result := FOnErro;
end;

procedure TSintegra.SetOnErro(const Value: TErrorEvent);
begin
  FOnErro := Value;
end;

procedure TSintegra.GerarErro(MensagemErro: String);
begin
  if Assigned(FOnErro) then
    FOnErro(MensagemErro);
end;

procedure TSintegra.Check(Condicao: Boolean; const Msg: string);
begin
  if not Condicao then
    GerarErro(Msg);
end;

procedure TSintegra.Check(Condicao: Boolean; Msg: string; Fmt: array of const);
begin
  Check(Condicao, Format(Msg, Fmt));
end;

procedure TSintegra.Check(const StrNaoNula, Msg: string);
begin
  Check(Trim(StrNaoNula) <> '', Msg);
end;

procedure TSintegra.Check(const StrNaoNula, Msg: string; Fmt: array of const);
begin
  Check(StrNaoNula, Format(Msg, Fmt));
end;

procedure TSintegra.Check(Valor, Minimo, Maximo: Double; const Msg: string);
begin
  Check((Valor >= Minimo) and (Valor <= Maximo), Msg);
end;

procedure TSintegra.Check(Valor, Minimo, Maximo: Double; const Msg: string; Fmt: array of const);
begin
  Check(Valor, Minimo, Maximo, Format(Msg, Fmt));
end;

function TSintegra.GetVersao: string;
begin
  Result := '3.0a';
end;

function TSintegra.GetDataInicial: TDateTime;
begin
  Result := fDataInicial;
end;

procedure TSintegra.SetDataInicial(const Valor: TDateTime);
begin
  fDataInicial := Valor;
end;

function TSintegra.GetArquivoMagnetico: TArquivoMagnetico;
begin
   Result := fArquivoMagnetico;
end;

function TSintegra.GetDataFinal: TDateTime;
begin
  Result := fDataFinal;
end;

procedure TSintegra.SetArquivoMagnetico(const Value: TArquivoMagnetico);
begin
  fArquivoMagnetico := Value;
end;

procedure TSintegra.SetDataFinal(const Valor: TDateTime);
begin
  fDataFinal := Valor;
end;

function TSintegra.GetNaturezaOperacao: TCodIdentificaOper;
begin
  Result := fNaturezaOperacao;
end;

procedure TSintegra.SetNaturezaOperacao(const Valor: TCodIdentificaOper);
begin
  fNaturezaOperacao := Valor;
end;

function TSintegra.GetFinalidade: TCodFinalidade;
begin
  Result := fFinalidade;
end;

procedure TSintegra.SetFinalidade(const Valor: TCodFinalidade);
begin
  fFinalidade := Valor;
end;

function TSintegra.GetRegistro1X: IRegistro1X;
begin
  Result := FRegistro1X;
end;

procedure TSintegra.SetRegistro1X(const Valor: IRegistro1X);
begin
  FRegistro1X := Valor;
end;

function TSintegra.GetRegistro60M: IRegistro60MList;
begin
  Result := FRegistro60M;
end;

procedure TSintegra.SetRegistro60M(const Valor: IRegistro60MList);
begin
  FRegistro60M := Valor;
end;

function TSintegra.GetRegistro75: IRegistro75Lista;
begin
  Result := FRegistro75;
end;

function TSintegra.GetRegistro85: IRegistro85Lista;
begin
  Result := FRegistro85;
end;

function TSintegra.GetRegistro86: IRegistro86Lista;
begin
  Result := FRegistro86;
end;

procedure TSintegra.SetRegistro75(const Valor: IRegistro75Lista);
begin
  FRegistro75 := Valor;
end;

procedure TSintegra.SetRegistro85(const Valor: IRegistro85Lista);
begin
  FRegistro85 := Valor;
end;

procedure TSintegra.SetRegistro86(const Valor: IRegistro86Lista);
begin
  FRegistro86 := Valor;
end;

function TSintegra.GetRegistro60R: IRegistro60RLista;
begin
  Result := FRegistro60R;
end;

procedure TSintegra.SetRegistro60R(const Valor: IRegistro60RLista);
begin
  FRegistro60R := Valor;
end;

function TSintegra.GetRegistro61: IRegistro61Lista;
begin
  Result := FRegistro61;
end;

procedure TSintegra.SetRegistro61(const Valor: IRegistro61Lista);
begin
  FRegistro61 := Valor;
end;

function TSintegra.GetRegistro61R: IRegistro61RLista;
begin
  Result := FRegistro61R;
end;

procedure TSintegra.SetRegistro61R(const Valor: IRegistro61RLista);
begin
  FRegistro61R := Valor;
end;

function TSintegra.VerificaProduto(aProduto: string): Boolean;
var
  i: integer;
  Encontrado: Boolean;
begin
  i := 0;
  Encontrado := False;

  with Registro75 do
  begin
    while (Encontrado = False) and (i < Count) do
    begin
      Encontrado := Items[i].CodProduto = aProduto;
      inc(i, 1);
    end;
  end;

  Result := Encontrado;
end;

function TSintegra.GetRegistro5X: IRegistro5XLista;
begin
  Result := FRegistro5X;
end;

procedure TSintegra.SetRegistro5X(const Valor: IRegistro5XLista);
begin
  FRegistro5X := Valor
end;

function TSintegra.GetRegistro55: IRegistro55Lista;
begin
  Result := FRegistro55;
end;

procedure TSintegra.SetRegistro55(const Valor: IRegistro55Lista);
begin
  FRegistro55 := Valor
end;

function TSintegra.GetRegistro74: IRegistro74Lista;
begin
  Result := FRegistro74;
end;

procedure TSintegra.SetRegistro74(const Valor: IRegistro74Lista);
begin
  FRegistro74 := Valor
end;

procedure TSintegra.LimparRegistros;
begin
  Registro5X.Clear;
  Registro60M.Clear;
  Registro60R.Clear;
  Registro61.Clear;
  Registro61R.Clear;
  Registro75.Clear;
end;

function TSintegra.GerarArquivo(aArquivo: string): Boolean;
var
  txtFile: TextFile;
begin
  Result := True;

  if Trim(aArquivo) = '' then
     raise ESintegraException.Create('Caminho/Nome do arquivo não informado!');

  Check(DataInicial > 0, 'Checagem Inicial: Informe a Data Inicial do arquivo!');
  Check(Datafinal > 0, 'Checagem Inicial: Informe a Data final do arquivo!');
  Check(DayOf(DataInicial) = 1, 'Checagem Inicial: Data inicial deve corresponder ao primeiro dia do mês informado!');
  Check(DateOf(EndOfTheMonth(DataFinal)) = DateOf(Datafinal), 'Checagem Inicial: Data final deve corresponder ao último dia do mês informado!');
  Check(YearOf(DataInicial) > 1993, 'Checagem Inicial: O ano da data inicial do arquivo deve ser superior a 1993!');
  Check(DataFinal >= DataInicial, 'Checagem Inicial: Data final deve se maior que a Data inicial!');
  Check(Datafinal <= Date, 'Checagem Inicial: Data Final "%s" não pode ser superior a Data Atual "%s"!', [DateToStr(Datafinal), DateToStr(Date)]);
  Check(Registro1X.RazaoSocial, 'Checagem Inicial: Não informado: Razão Social');
  Check(Registro1X.Municipio, 'Checagem Inicial: Não informado: Municipio');
  Check(Registro1X.UF, 'Checagem Inicial: Não informado: Estado (UF)');
  Check(Registro1X.CNPJ, 'Checagem Inicial: Não informado: CNPJ/CPF');
  Check(Registro1X.InscEstadual, 'Checagem Inicial: Não informado: Inscrição Estadual');
  Check(Registro1X.Fone, 'Checagem Inicial: Não informado: Telefone para contado');
  Check(VerificaCEP(Registro1X.CEP, Registro1X.UF), 'Checagem Inicial: CEP Informado não é válido!');
  Check(VerificaCPF_CNPJ(Registro1X.CNPJ), 'Checagem Inicial: CNPJ informado inválido');
  Check(VerificaInscEstadual(Registro1X.InscEstadual, Registro1X.UF), 'Checagem Inicial: Inscrição Estadual informada inválida!');

  try
    AssignFile(txtFile, aArquivo);
    Rewrite(txtFile);

    { Geração do cabeçalho do arquivo, duas primeiras linhas com dados do contribuinte }
    Write(txtFile, Registro_10);

    { Geração do cabeçalho do arquivo, duas primeiras linhas com dados do contribuinte }
    Write(txtFile, Registro_11);

    { Geração dos registros referentes as Notas fiscais }
    if Registro5X.Count > 0 then
       Write(txtFile, Registro_50);

    { Geração dos registros referentes ao cupom fiscal, maquina registradora, PDV }
    if Registro60M.Count > 0 then
       Write(txtFile, Registro_60);

    { Geração dos registros referentes ao cupom fiscal, maquina registradora, PDV }
    if Registro61.Count > 0 then
      Write(txtFile, Registro_61);

    { Geração dos registros de Conhecimento de Transporte }
    if Registro5X.Count > 0 then
       Write(txtFile, Registro_70);

    { Geração dos registros dos dados do Conhecimento de Transporte }
    if Registro5X.Count > 0 then
       Write(txtFile, Registro_71);

    { Geração dos registros de inventário }
    if Registro74.Count > 0 then
       Write(txtFile, Registro_74);

    { Geração dos registros de produtos }
    if Registro75.Count > 0 then
       Write(txtFile, Registro_75);

    { Geração dos registros de exportação }
    if Registro85.Count > 0 then
       Write(txtFile, Registro_85);

    { Geração do Fim do arquivo }
    Write(txtFile, Registro_90);

    CloseFile(txtFile);
  except
    on E: Exception do
    begin
      if pos('I/O', E.Message) > 0 then
         raise ESintegraException.Create('Não foi possível gravar o arquivo, ele já se encontra aberto por outro aplicativo.')
      else
      begin
        CloseFile(txtFile);
        raise ESintegraException.Create(E.Message);
      end;
    end;
  end;
end;

function TSintegra.Registro_10: string;
begin
  with Registro1X do
  begin
    Check(VerificaCPF_CNPJ(CNPJ), 'Registro10: CPF/CNPJ "%s" digitado inválido!', [CNPJ]);
    Check(VerificaInscEstadual(InscEstadual, UF), 'Registro10: Inscrição Estadual "%s" inválida!', [InscEstadual]);
    Check(VerificaUF(UF), 'Registro10: UF digitado inválido!');
    Check(Length(Fax) <= 10, 'Registro10: Fax digitado inválido!');
    Check(Numero, 0, 99999, 'Registro10: Número digitado inválido!');
    Check(VerificaCEP(CEP, UF), 'Registro10: CEP "%s" digitado inválido!', [CEP]);

    Result := '10' +
              LFill(CNPJ, 14) +
              RFill(InscEstadual, 14) +
              RFill(RazaoSocial, 35) +
              RFill(Municipio, 30) +
              UF +
              LFill(Fax, 10) +
              LFill(DataInicial) +
              LFill(DataFinal) +
              RetornaArquivoMagnetico(ArquivoMagnetico) +
              RetornaNatureza(NatOperacao) +
              RetornaFinalidade(Finalidade) +
              #13#10;
  end;
end;

function TSintegra.Registro_11: string;
var
strComplemento: string;
begin
  with Registro1X do
  begin
    Check(VerificaCPF_CNPJ(CNPJ), 'Registro11: CPF/CNPJ "%s" digitado inválido!', [CNPJ]);
    Check(VerificaInscEstadual(InscEstadual, UF), 'Registro11: Inscrição Estadual "%s" inválida!', [InscEstadual]);
    Check(VerificaUF(UF), 'Registro11: UF digitado inválido!');
    Check(Length(Fax) <= 10, 'Registro11: Fax digitado inválido!');
    Check(Numero, 0, 99999, 'Registro11: Número digitado inválido!');
    Check(VerificaCEP(CEP, UF), 'Registro11: CEP "%s" digitado inválido!', [CEP]);

    if (Trim(Complemento) = '') and (Numero = 0) then
      strComplemento := 'SEM NÚMERO'
    else
      strComplemento := Complemento;

    Result := '11' +
              RFill(Endereco, 34) +
              LFill(Numero, 5) +
              RFill(strComplemento, 22) +
              RFill(Bairro, 15) +
              LFill(CEP, 8) +
              RFill(Responsavel, 28) +
              LFill(Fone, 12) +
              #13#10;
  end;
end;

function TSintegra.Registro_50: string;
var
aRegistro50, aRegistro51, aRegistro53, aRegistro54, aRegistro55: string;
strInscricao: string;
intITENS, intNFS, intICMS: Integer;
begin
  if Debug then
  begin
    aRegistro50 := '50CNPJ---------- IE------------ Data---- UF Md Ser Numero CFOP E Valor Total-- Base ICMS---- Valor ICMS--- Isentas------ Outras------- Aliq S'#13#10;
    aRegistro51 := '51CNPJ---------- IE------------ Data---- UF Ser Numero CFOP Valor ------- Valor IPI---- Isentas------ Outras------- Vazio--------------- S'#13#10;
    aRegistro53 := '';
    aRegistro54 := '54CNPJ---------- Md Ser Numero CFOP CST Itm CodProduto---- Quantidade- ValorProduto Desconto---- BaseICMS---- BaseICMSST-- IPI--------- Aliq'#13#10;
    aRegistro55 := '';
  end
  else
  begin
    aRegistro50 := '';
    aRegistro51 := '';
    aRegistro53 := '';
    aRegistro54 := '';
    aRegistro55 := '';
  end;

  for intNFS := 0 to Registro5X.Count - 1 do
  begin
    if Registro5X[intNFS].Modelo in kReg50 then
    begin
      with Registro5X[intNFS] do
      begin
        if Trim(InscEstadual) <> '' then
          strInscricao := InscEstadual
        else
          strInscricao := 'ISENTO';

        Check(Modelo > 0, 'Registro50: Modelo da Nota Fisca é obrigatório!');
        Check(VerificaUF(UF), 'Registro50: UF "%s" digitada inválida!', [UF]);
        Check(VerificaCPF_CNPJ(CNPJ), 'Registro50: CNPJ/CPF "%s" inválido!', [CNPJ]);
        Check(VerificaInscEstadual(strInscricao, UF), 'Registro50: Incr. Estadual "%s" Inválida para "%s"', [strInscricao, UF]);
        Check(Data, DataInicial, DataFinal, 'Registro50: Data "%s" não corresponde ao período informado "%s à %s"!', [DatetoStr(Data), DateToStr(DataInicial), DateToStr(DataFinal)]);
        Check(Numero, 1, 999999, 'Registro50: Número da Nota Fiscal inválido!');
        Check(Registro50.Count > 0, 'Registro50: Informe as alíquotas para a Nota Fiscal "%d"', [Numero]);

        if Registro50.Count > 0 then
        begin
          for intICMS := 0 to Registro50.Count - 1 do
          begin
            Check(Registro50[intICMS].Aliquota, 0, 99.99, 'Registro54: aliquota inválida!');
            Check(Registro50[intICMS].CFOP <> 0, 'Registro50: CFOP é campo obrigatório!');
            Check(Registro50[intICMS].ValorTotal > 0, 'Registro50: Valor Total para o CFOP/Aliquota é obrigatório!');
            Check(VerificaCFOP(Registro50[intICMS].CFOP), 'Registro50: Código do CFOP "%d" digitado Inválido!', [Registro50[intICMS].CFOP]);

            { verifica o CFOP }
            if IntToStr(Registro50[intICMS].CFOP)[1] in ['1', '5'] then
               Check(Registro1X.UF = UF, 'Registro50: Para CFOP''s começados em "1" ou "5" a UF deve ser igual ao do Informante!')
            else
            if IntToStr(Registro50[intICMS].CFOP)[1] in ['2', '6'] then
               Check(Registro1X.UF <> UF, 'Registro50: Para CFOP''s começados em "2" ou "6" a UF deve ser diferente da do Informante!')
            else
              Check(UF = 'EX', 'Registro50: Para CFOP''s começados em "3" ou "7" a UF deve ser "EX"!');

            aRegistro50 := aRegistro50 + '50' +
              LFill(CNPJ, 14) +
              RFill(strInscricao, 14) +
              LFill(Data) +
              RFill(UF) +
              LFill(Modelo, 2) +
              RFill(Serie, 3) +
              LFill(Numero, 6) +
              LFill(Registro50[intICMS].CFOP, 4) +
              RFill(RetornaTipoEmitente(TipoEmitente), 1) +
              LFill(Registro50[intICMS].ValorTotal, 13) +
              LFill(Registro50[intICMS].BaseICMS, 13) +
              LFill(Registro50[intICMS].ValorICMS, 13) +
              LFill(Registro50[intICMS].IsentasNTribut, 13) +
              LFill(Registro50[intICMS].Outras, 13) +
              LFill(Registro50[intICMS].Aliquota, 4) +
              RetornaSituacao(Situacao)
              + #13#10;
          end;
        end
        else
        begin
          aRegistro50 := aRegistro50 + '50' +
            LFill(CNPJ, 14) +
            RFill(strInscricao, 14) +
            LFill(Data) +
            RFill(UF) +
            LFill(Modelo, 2) +
            RFill(Serie, 3) +
            LFill(Numero, 6) +
            LFill('1000', 4) +
            RFill(RetornaTipoEmitente(TipoEmitente), 1) +
            LFill(0, 13) +
            LFill(0, 13) +
            LFill(0, 13) +
            LFill(0, 13) +
            LFill(0, 13) +
            LFill(0, 4) +
            RetornaSituacao(Situacao)
            + #13#10;
        end;

        { Registro do itens (Produtos) da Nota Fiscal }
        for intITENS := 0 to Registro54.Count - 1 do
        begin
          { Verifica se o Registro75 esta cadastrado no registro 75 }
          Check(VerificaProduto(Registro54[intITENS].CodProduto), 'Registro54: Produto "%s" não consta no registro 75!', [Registro54[intITENS].CodProduto]);
          Check(VerificaCST(Registro54[intITENS].CST), 'Registro54: CST "%s" inválido!', [Registro54[intITENS].CST]);
          Check(VerificaCFOP(Registro54[intITENS].CFOP), 'Registro54: No Registro50 o Código do CFOP "%d" digitado Inválido!', [Registro54[intITENS].CFOP]);
          Check(Registro54[intITENS].NumItem, 1, 999, 'Registro54: Número do Item inválido');

          aRegistro54 := aRegistro54 + '54' +
            LFill(CNPJ, 14) +
            LFill(Modelo, 2) +
            RFill(Serie, 3) +
            LFill(Numero, 6) +
            LFill(Registro54[intITENS].CFOP, 4) +
            RFill(Registro54[intITENS].CST, 3) +
            LFill(Registro54[intITENS].NumItem, 3) +
            RFill(Registro54[intITENS].CodProduto, 14) +
            LFill(Registro54[intITENS].Quantidade, 11, 3) +
            LFill(Registro54[intITENS].ValorProduto, 12) +
            LFill(Registro54[intITENS].Desconto, 12) +
            LFill(Registro54[intITENS].BaseICMS, 12) +
            LFill(Registro54[intITENS].BaseICMSST, 12) +
            LFill(Registro54[intITENS].IPI, 12) +
            LFill(Registro54[intITENS].Aliquota, 4) +
            #13#10;
        end;

        if Registro50.Count > 0 then
        begin
          if Frete <> 0 then
          begin
            aRegistro54 := aRegistro54 + '54' +
              LFill(CNPJ, 14) +
              LFill(Modelo, 2) +
              RFill(Serie, 3) +
              LFill(Numero, 6) +
              LFill(CFOP, 4) +
              RFill('', 3) +
              RFill('991') +
              RFill('', 14) +
              LFill(0, 11) +
              LFill(0, 12) +
              LFill(Frete, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 4) +
              #13#10;
          end;

          if Seguro <> 0 then
          begin
            aRegistro54 := aRegistro54 + '54' +
              LFill(CNPJ, 14) +
              LFill(Modelo, 2) +
              RFill(Serie, 3) +
              LFill(Numero, 6) +
              LFill(CFOP, 4) +
              RFill('', 3) +
              RFill('992') +
              RFill('', 14) +
              LFill(0, 11) +
              LFill(0, 12) +
              LFill(Seguro, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 4) +
              #13#10;
          end;

          if ValorPisCofins <> 0 then
          begin
            aRegistro54 := aRegistro54 + '54' +
              LFill(CNPJ, 14) +
              LFill(Modelo, 2) +
              RFill(Serie, 3) +
              LFill(Numero, 6) +
              LFill(CFOP, 4) +
              RFill('', 3) +
              RFill('993') +
              RFill('', 14) +
              LFill(0, 11) +
              LFill(0, 12) +
              LFill(ValorPisCofins, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 4) +
              #13#10;
          end;

          if ValorComplementar <> 0 then
          begin
            aRegistro54 := aRegistro54 + '54' +
              LFill(CNPJ, 14) +
              LFill(Modelo, 2) +
              RFill(Serie, 3) +
              LFill(Numero, 6) +
              LFill(CFOP, 4) +
              RFill('', 3) +
              RFill('997') +
              RFill('', 14) +
              LFill(0, 11) +
              LFill(0, 12) +
              LFill(ValorComplementar, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 4) +
              #13#10;
          end;

          if ValorServicoNaoTributado <> 0 then
          begin
            aRegistro54 := aRegistro54 + '54' +
              LFill(CNPJ, 14) +
              LFill(Modelo, 2) +
              RFill(Serie, 3) +
              LFill(Numero, 6) +
              LFill(CFOP, 4) +
              RFill('', 3) +
              RFill('998') +
              RFill('', 14) +
              LFill(0, 11) +
              LFill(0, 12) +
              LFill(ValorServicoNaoTributado, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 4) +
              #13#10;
          end;

          if DespAcessoria <> 0 then
          begin
            aRegistro54 := aRegistro54 + '54' +
              LFill(CNPJ, 14) +
              LFill(Modelo, 2) +
              RFill(Serie, 3) +
              LFill(Numero, 6) +
              LFill(CFOP, 4) +
              RFill('', 3) +
              RFill('999') +
              RFill('', 14) +
              LFill(0, 11) +
              LFill(0, 12) +
              LFill(DespAcessoria, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 12) +
              LFill(0, 4) +
              #13#10;
          end;
        end;

        { Registro 51 dos Contribuinte de IPI }
        for intITENS := 0 to Registro51.Count - 1 do
        begin
          Check(VerificaCFOP(Registro51[intITENS].CFOP), 'Registro51: Código do CFOP "%d" digitado Inválido!', [Registro51[intITENS].CFOP]);

          aRegistro51 := aRegistro51 + '51' +
            LFill(CNPJ, 14) +
            RFill(strInscricao, 14) +
            LFill(Data) +
            RFill(UF) +
            RFill(Serie, 3) +
            LFill(Numero, 6) +
            LFill(Registro51[intITENS].CFOP, 4) +
            LFill(Registro51[intITENS].Valor, 13) +
            LFill(Registro51[intITENS].ValorIPI, 13) +
            LFill(Registro51[intITENS].IsentaNTrib, 13) +
            LFill(Registro51[intITENS].Outras, 13) +
            RFill('', 20) +
            RetornaSituacao(Situacao) +
            #13#10;
        end;

        { Registros 53 dos Substitutos Tributarios }
        for intITENS := 0 to Registro53.Count - 1 do
        begin
          Check(VerificaCFOP(Registro53[intITENS].CFOP), 'Registro53: Código do CFOP "%s" digitado Inválido!', [Registro53[intITENS].CFOP]);

          aRegistro53 := aRegistro53 + '53' +
            LFill(CNPJ, 14) +
            RFill(strInscricao, 14) +
            LFill(Data) +
            RFill(UF) +
            LFill(Modelo, 2) +
            RFill(Serie, 3) +
            LFill(Numero, 6) +
            LFill(Registro53[intITENS].CFOP, 4) +
            RetornaTipoEmitente(TipoEmitente) +
            LFill(Registro53[intITENS].BaseCalcICMSST, 13) +
            LFill(Registro53[intITENS].ICMSRetido, 13) +
            LFill(Registro53[intITENS].DespAcessoria, 13) +
            RetornaSituacao(Situacao) +
            RFill(Registro53[intITENS].CodAntecipacao) +
            RFill('', 30) +
            #13#10;
        end;
      end;
    end;
  end;

  { Registro 55 para guias de recolhimento dos substitutos tributarios (Registro55) }
  for intITENS := 0 to Registro55.Count - 1 do
  begin
    with Registro55[intITENS] do
    begin
      Check(Agencia, 1, 9999, 'Registro55: Agência "%d" inválida!', [Agencia]);
      Check(MesRef, 1, 12, 'Registro55: Mês "%d" de referência inválido', [MesRef]);
      Check(AnoRef, 1, 9999, 'Registro55: Ano "%d" de referência inválido', [AnoRef]);
      Check(Banco, 1, 999, 'Registro55: Código do Banco "%d" inválido!', [Banco]);
      Check(VerificaUF(UFContribuinte), 'Registro55: UF "%s" Contribuinte inválida!', [UFContribuinte]);
      Check(VerificaUF(UFFavorecido), 'Registro55: UF "%s" Favorecido inválida!', [UFFavorecido]);

      Check(DataPagamento, DataInicial, DataFinal, 'Registro55: Data de pagamento "%s" fora do período informado "%s à %s"!',
        [DatetoStr(Valor), DatetoStr(DataInicial), DatetoStr(DataFinal)]);

      Check(Vencimento, DataInicial, DataFinal, 'Registro55: Data de vencimento "%s" fora do período informado "%s à %s"!',
        [DatetoStr(Valor), DatetoStr(DataInicial), DatetoStr(DataFinal)]);

      aRegistro55 := aRegistro55 + '55' +
        LFill(Registro1X.CNPJ, 14) +
        RFill(Registro1X.InscEstadual, 14) +
        LFill(DataPagamento) +
        RFill(UFContribuinte) +
        RFill(UFFavorecido) +
        LFill(Banco, 3) +
        LFill(Agencia, 4) +
        RFill(NumeroGNRE, 20) +
        LFill(Valor, 14) +
        LFill(Vencimento) +
        LFill(MesRef, 2) +
        LFill(AnoRef, 4) +
        RFill(NumConvenio, 30) +
        #13#10;
    end;
  end;

  Result := aRegistro50 + aRegistro51 + aRegistro53 + aRegistro54 + aRegistro55;
end;

function TSintegra.Registro_60: string;
var
aRegistro6X: string;
intFor60M, intFor60X: Integer;
datDataResumo: TDateTime;
begin
  aRegistro6X := '';

  for intFor60M := 0 to Registro60M.Count - 1 do
  begin
    with Registro60M[intFor60M] do
    begin
      { Verifica se o valor informado no 60M e difrente da soma dos 60a }
      Check(DataEmissao > 0, 'Registro60M: Informe a Data de Emissão!');
      Check(DataEmissao, DataInicial, DataFinal, 'Registro60M: Data "%s" não corresponde ao período informado "%s à %s"!', [DatetoStr(DataEmissao), DateToStr(DataInicial), DateToStr(DataFinal)]);
      Check(NumSequencial, 1, 999, 'Registro60M: Número sequencial do caixa inválido!');
      Check(COOInicial, 1, 999999, 'Registro60M: COO Inicial inválido');
      Check(COOFinal, 0, 999999, 'Registro60M: COO Final inválido');
      Check(COOFinal >= COOInicial, 'Registro60M: COO Final de dever ser maior ou igual ao Inicial!');
      Check(ContReducaoZ, 1, 999999, 'Registro60M: Contador de Redução Z inválido');
      Check(ContReinicioOper, 1, 999, 'Registro60M: Contador de Reinicio de Operacao inválido');
      Check(VendaBruta >= 0, 'Registro60M: Venda Bruta deve ser maior ou igual a 0(Zero)');
      Check(GTFinal >= 0, 'Registro60M: Grande Total final deve ser maior ou igual a 0(Zero)');
      Check(VendaBruta = Registro60A.Total, 'Registro60M: No Registro60A o Valor acumulado difere da somas das aliquotas informadas!');

      { Verifica se o valor informado no 60M e difrente da soma dos 60D }
      { Somente quando for gerar o resumo diario 60D }
      if Registro60D.Count > 0 then
        Check(VendaBruta = Registro60D.TotalAcumulado, 'Registro60D: No Registro60M o Valor acumulado difere da somas dos resumos informados! - ' + DateToStr(DataEmissao));

      aRegistro6X := aRegistro6X + '60M' +
        LFill(DataEmissao) +
        RFill(NumSerieEquip, 20) +
        LFill(NumSequencial, 3) +
        RetornaModDocumento(ModDocFiscal) +
        LFill(COOInicial, 6) +
        LFill(COOFinal, 6) +
        LFill(ContReducaoZ, 6) +
        LFill(ContReinicioOper, 3) +
        LFill(VendaBruta, 16) +
        LFill(GTFinal, 16) +
        RFill('', 37) + #13#10;

      { obrigatorio quando se gera o registro 60M }
      for intFor60X := 0 to Registro60A.Count - 1 do
      begin
        Check(Registro60A[intFor60X].ValorAcumulado >= 0, 'Registro60A: Valor Acumulado deve ser maior ou igual a 0(Zero)');
        Check(VerificaSitTributaria(Registro60A[intFor60X].SitTributaria), 'Registro60A: Situação Tributária inválida!');

        aRegistro6X := aRegistro6X + '60A' +
          LFill(DataEmissao) +
          RFill(NumSerieEquip, 20) +
          RFill(Registro60A[intFor60X].SitTributaria, 4) +
          LFill(Registro60A[intFor60X].ValorAcumulado, 12) +
          RFill('', 79) +
          #13#10;
      end;

      { Registro 60D Opcional (depende de legislacao da UF }
      if (Assigned(Registro60D)) and (Registro60D.Count > 0) then
      begin
        Check(Assigned(Registro75) and (Registro75.Count > 0), 'Registro75: Nenhum Produto foi assinalado!');

        for intFor60X := 0 to Registro60D.Count - 1 do
        begin
          Check(Registro60D[intFor60X].CodProduto, 'Registro60D: Informe o Código do Produto!');
          Check(Registro60D[intFor60X].QuantAcumulada >= 0, 'Registro60D: Quantidade Acumulada deve ser maior ou igual a 0(Zero)');
          Check(Registro60D[intFor60X].ValorAcumulado >= 0, 'Registro60D: Valor Acumulado deve ser maior ou igual a 0(Zero)');
          Check(Registro60D[intFor60X].BaseCalcICMS >= 0, 'Registro60D: Base ICMS deve ser maior ou igual a 0(Zero)');
          Check(VerificaSitTributaria(Registro60D[intFor60X].SitTributaria), 'Registro60D: Situação Tributária inválida!');

          if Pos(Registro60D[intFor60X].SitTributaria, 'F, I, ISS, N, CANC') = 0 then
          begin
             Check(((StrToNumero(Registro60D[intFor60X].SitTributaria, 2) > 0) and (Registro60D[intFor60X].BaseCalcICMS <> 0)),
                  'Registro60D: Base de calculo do ICMS deve possuir um valor quando a aliquota for tributada!');
          end;
          { Verifica se o Registro75 esta cadastrado no registro 75 }
          Check(VerificaProduto(Registro60D[intFor60X].CodProduto),
               'Registro60D: Produto "%s" não assinalado no Produto!', [Registro60D[intFor60X].CodProduto]);

          aRegistro6X := aRegistro6X + '60D' +
            LFill(DataEmissao) +
            RFill(NumSerieEquip, 20) +
            RFill(Registro60D[intFor60X].CodProduto, 14) +
            LFill(Registro60D[intFor60X].QuantAcumulada, 13, 3) +
            LFill(Registro60D[intFor60X].ValorAcumulado, 16) +
            LFill(Registro60D[intFor60X].BaseCalcICMS, 16) +
            RFill(Registro60D[intFor60X].SitTributaria, 4) +
            LFill(Registro60D[intFor60X].ValorICMS, 13) +
            RFill('', 19) +
            #13#10;

//            ShowMessage( LFill(Registro60D[intFor60X].ValorICMS, 13) );

        end;
      end;

      { Registro 60intFor60M Opcional (depende de legislacao da UF }
      if (Assigned(Registro60I)) and (Registro60I.Count > 0) then
      begin
        Check(Assigned(Registro75) and (Registro75.Count > 0), 'Registro75: Nenhum Produto foi assinalado!');

        for intFor60X := 0 to Registro60I.Count - 1 do
        begin
          { Verifica se o Registro75 esta cadastrado no registro 75 }
          Check(Registro60I[intFor60X].COOCupom, 1, 999999, 'Registro60I: COO do Cupom fiscal inválido!');
          Check(Registro60I[intFor60X].NumItem, 1, 999, 'Registro60I: Número sequencial do item no cupom inválido');
          Check(Registro60I[intFor60X].CodProduto, 'Registro60I: Informe o Código do Produto!');
          Check(Registro60I[intFor60X].Quantidade > 0, 'Registro60I: Quantidade deve ser maior que 0(Zero)!');
          Check(Registro60I[intFor60X].ValorUnitario > 0, 'Registro60I: Valor Unitário deve ser maior que 0(Zero)!');
          Check(Registro60I[intFor60X].BaseICMS >= 0, 'Registro60I: Base do ICMS dever ser maior ou igual a 0(Zero)!');
          Check(VerificaSitTributaria(Registro60I[intFor60X].SitTributaria), 'Registro60I: Situação Tributária inválida!');
          Check(VerificaProduto(Registro60I[intFor60X].CodProduto), 'Registro60I: Produto "%s" não assinalado no Produto!', [Registro60I[intFor60X].CodProduto]);

          aRegistro6X := aRegistro6X + '60I' +
            LFill(DataEmissao) +
            RFill(NumSerieEquip, 20) +
            RetornaModDocumento(ModDocFiscal) +
            LFill(Registro60I[intFor60X].COOCupom, 6) +
            LFill(Registro60I[intFor60X].NumItem, 3) +
            RFill(Registro60I[intFor60X].CodProduto, 14) +
            LFill(Registro60I[intFor60X].Quantidade, 13, 3) +
            LFill(Registro60I[intFor60X].ValorUnitario, 13) +
            LFill(Registro60I[intFor60X].BaseICMS, 12) +
            RFill(Registro60I[intFor60X].SitTributaria, 4) +
            LFill(Registro60I[intFor60X].ValorICMS, 12) +
            StringOfChar(' ', 16) + #13#10;
        end;
      end;
    end;
  end;

  { Registro 60R Opcional (depende de legislacao da UF) }
  if (Assigned(Registro60R)) and (Registro60R.Count > 0) then
  begin
    Check(Assigned(Registro75) and (Registro75.Count > 0), 'Registro75: Nenhum Produto foi assinalado!');

    for intFor60X := 0 to Registro60R.Count - 1 do
    begin
      datDataResumo := EncodeDate(Registro60R[intFor60X].Ano, Registro60R[intFor60X].Mes, 1);

      Check(datDataResumo, DataInicial, DataFinal, 'Registro60R: Mês/Ano "%s" fora do período informado "%s à %s"!', [FormatDateTime('mm/yyyy', datDataResumo), DateToStr(DataInicial), DateToStr(DataFinal)]);
      Check(Registro60R[intFor60X].Mes, 1, 12, 'Registro60R: Mês inválido!');
      Check(Registro60R[intFor60X].ANo, 1, 9999, 'Registro60R: Ano inválido!');
      Check(Registro60R[intFor60X].CodProduto, 'Registro60R: Informe o Código do Produto!');
      Check(Registro60R[intFor60X].Quantidade > 0, 'Registro60R: Quantidade dever ser maior que 0(Zero)!');
      Check(Registro60R[intFor60X].ValorAcumProduto > 0, 'Registro60R: Valor Acumulado dos produtos de ve ser maior que 0 (Zero)!');
      { Só faz a checagem se a tributação for diferencte de ISENTA. }
      if Registro60R[intFor60X].SitTributaria <> 'I' then
         Check(Registro60R[intFor60X].ValorAcumICMS > 0, 'Registro60R: Valor do ICMS dos produtos deve ser maior que 0 (Zero)!');
      //
      Check(VerificaSitTributaria(Registro60R[intFor60X].SitTributaria), 'Registro60R: Situação Tributária inválida!');
      Check(VerificaProduto(Registro60R[intFor60X].CodProduto), 'Registro60R: Produto "%s" não assinalado no Produto!', [Registro60R[intFor60X].CodProduto]);

      aRegistro6X := aRegistro6X + '60R' +
        LFill(Registro60R[intFor60X].Mes, 2) +
        LFill(Registro60R[intFor60X].ANo, 4) +
        RFill(Registro60R[intFor60X].CodProduto, 14) +
        LFill(Registro60R[intFor60X].Quantidade, 13, 3) +
        LFill(Registro60R[intFor60X].ValorAcumProduto, 16) +
        LFill(Registro60R[intFor60X].ValorAcumICMS, 16) +
        RFill(Registro60R[intFor60X].SitTributaria, 4) +
        StringOfChar(' ', 54) + #13#10;
    end;
  end;

  Result := aRegistro6X;
end;

function TSintegra.Registro_61: string;
var
aRegistro: string;
intFor: Integer;
datDataResumo: TDateTime;
begin
  { Registro 61 (Documentos emitidos a mao) }
  if (Assigned(Registro61)) and (Registro61.Count > 0) then
  begin
    for intFor := 0 to Registro61.Count - 1 do
    begin
//      GerarProgresso(Format(kMensagemProgresso, ['61']), Registro61.Count, intFor + 1);

      Check(Registro61[intFor].DataEmissao, DataInicial, DataFinal, 'Registro61: Data "%s" não corresponde ao período informado "%s à %s"!', [DatetoStr(Registro61[intFor].DataEmissao), DateToStr(DataInicial), DateToStr(DataFinal)]);
      Check(Registro61[intFor].ModeloDoc in [2, 4, 13..16], 'Registro61: Modelo de Documento inválido "%s" para o NFC!', [IntToStr(Registro61[intFor].ModeloDoc)]);
      if Trim(Registro61[intFor].Serie) <> '' then
        Check((Registro61[intFor].Serie = 'D') or (Registro61[intFor].Serie = 'U') or (Registro61[intFor].Serie = 'DU'), 'Registro61: Serie do Documento invalida "%s"!', [Registro61[intFor].Serie]);
      Check(Registro61[intFor].NumInicial, 0, 999999, 'Registro61: Número Inicial inválido!');
      Check(Registro61[intFor].NumFinal, 0, 999999, 'Registro61: Número Final inválido!');
      Check(Registro61[intFor].ValorTotal >= 0, 'Registro61: Valor Total deve ser maior ou igual a 0(Zero)');
      Check(Registro61[intFor].BaseICMS >= 0, 'Registro61: Valor da Base de ICMS deve ser maior ou igual a 0(Zero)');
      Check(Registro61[intFor].IsentasNTrib >= 0, 'Registro61: Valor de Isentas/Não Tributadas deve ser maior ou igual a 0(Zero)');
      Check(Registro61[intFor].Outras >= 0, 'Registro61: Valor de Outras deve ser maior ou igual a 0(Zero)');
      Check(Registro61[intFor].Aliquota, 0, 99.99, 'Registro61: Valor da aliquota inválido!');

      aRegistro := aRegistro + '61' +
        StringOfchar(' ', 14) +
        StringOfchar(' ', 14) +
        LFill(Registro61[intFor].DataEmissao) +
        LFill(Registro61[intFor].ModeloDoc, 2) +
        RFill(Registro61[intFor].Serie, 3) +
        RFill(Registro61[intFor].SubSerie, 2) +
        LFill(Registro61[intFor].NumInicial, 6) +
        LFill(Registro61[intFor].NumFinal, 6) +
        LFill(Registro61[intFor].ValorTotal, 13) +
        LFill(Registro61[intFor].BaseICMS, 13) +
        LFill(Registro61[intFor].ValorICMS, 12) +
        LFill(Registro61[intFor].IsentasNTrib, 13) +
        LFill(Registro61[intFor].Outras, 13) +
        LFill(Registro61[intFor].Aliquota, 4) +
        ' ' + #13#10;
    end;

    { Registro 61R (Itens dos documentos digitados a mao) }
    if Assigned(Registro61R) and (Registro61R.Count > 0) then
    begin
      for intFor := 0 to Registro61R.Count - 1 do
      begin
//        GerarProgresso(Format(kMensagemProgresso, ['61R']), Registro61R.Count, intFor + 1);

        datDataResumo := StrToDate('01/' + IntTosTr(Registro61R[intFor].Mes) + '/' + IntTosTr(Registro61R[intFor].Ano));
        Check(datDataResumo, DataInicial, DataFinal, 'Registro61R: Mês/Ano "%s" fora do período informado "%s à %s"!', [FormatDateTime('mm/yyyy', datDataResumo), DateToStr(DataInicial), DateToStr(DataFinal)]);
        Check(Registro61R[intFor].Aliquota, 0, 99.99, 'Registro61R: Alíquota inválida!');
        Check(Registro61R[intFor].BaseCalcICMS >= 0, 'Registro61R: Base de Cálculo do ICMS deve ser maior ou igual a 0(Zero)');
        Check(Registro61R[intFor].CodProduto, 'Registro61R: Informe o Código do Produto');
        Check(Registro61R[intFor].Mes, 1, 12, 'Registro61R: Mês informado inválido!');
        Check(Registro61R[intFor].Ano, 2003, 9999, 'Registro61R: Ano Informado inválido!');
        Check(Registro61R[intFor].Quantidade > 0, 'Registro61R: Quantidade deve ser maior que 0(Zero)!');
        Check(Registro61R[intFor].ValorBrutoProduto > 0, 'Registro61R: Valor Bruto deve ser maior que 0(Zero)!');

        aRegistro := aRegistro + '61R' +
          LFill(Registro61R[intFor].Mes, 2) +
          LFill(Registro61R[intFor].Ano, 4) +
          RFill(Registro61R[intFor].CodProduto, 14) +
          LFill(Registro61R[intFor].Quantidade, 13) +
          LFill(Registro61R[intFor].ValorBrutoProduto, 16) +
          LFill(Registro61R[intFor].BaseCalcICMS, 16) +
          LFill(Registro61R[intFor].Aliquota, 4) +
          StringOfChar(' ', 54) + #13#10;
      end;
    end;
  end;

  Result := aRegistro;
end;

function TSintegra.VerificaCorrespondente(aProduto: string): Boolean;
var
  intI, intA: Integer;
  booEncontrado: Boolean;
begin
  intI := 0;
  intA := 0;
  booEncontrado := False;

  if Assigned(Registro60M) then
  begin
    while (booEncontrado = False) and (intI < Registro60M.Count) do
    begin
      { Registro 60D }
      if Assigned(Registro60M[intI]) then
      begin
        while (booEncontrado = False) and (intA < Registro60M[intI].Registro60D.Count) do
        begin
          booEncontrado := Registro60M[intI].Registro60D[intA].CodProduto = aProduto;
          inc(intA, 1);
        end;
      end;

      { Registro 60intI }
      if Assigned(Registro60M[intI]) then
      begin
        intA := 0;
        while (booEncontrado = False) and (intA < Registro60M[intI].Registro60I.Count) do
        begin
          booEncontrado := Registro60M[intI].Registro60I[intA].CodProduto = aProduto;
          inc(intA, 1);
        end;
      end;

      inc(intI, 1);
    end;
  end;

  { Registro 60R }
  if Assigned(Registro60R) then
  begin
    intI := 0;
    while (booEncontrado = False) and (intI < Registro60R.Count) do
    begin
      booEncontrado := Registro60R[intI].CodProduto = aProduto;
      inc(intI, 1);
    end;
  end;

  { Registro 61R }
  if Assigned(Registro61R) then
  begin
    intI := 0;
    while (booEncontrado = False) and (intI < Registro61R.Count) do
    begin
      booEncontrado := Registro61R[intI].CodProduto = aProduto;
      inc(intI, 1);
    end;
  end;

  if Assigned(Registro5X) then
  begin
    intI := 0;
    while (booEncontrado = False) and (intI < Registro5X.Count) do
    begin
      { Registro 54 }
      if Assigned(Registro5X[intI].Registro54) then
      begin
        intA := 0;
        while (booEncontrado = False) and (intA < Registro5X[intI].Registro54.Count) do
        begin
          booEncontrado := Registro5X[intI].Registro54[intA].CodProduto = aProduto;
          inc(intA, 1);
        end;
      end;

      inc(intI);
    end;
  end;

  { Registro 74 }
  if Assigned(Registro74) then
  begin
    intI := 0;
    while (booEncontrado = False) and (intI < Registro74.Count) do
    begin
      booEncontrado := Registro74[intI].CodProduto = aProduto;
      inc(intI, 1);
    end;
  end;

  Result := booEncontrado;
end;

function TSintegra.Registro_70: string;
var
intFor5X, intFor50: integer;
sRegistro70: string;
begin
  sRegistro70 := '';

  for intFor5X := 0 to Registro5X.Count - 1 do
  begin
    with Registro5X[intFor5X] do
    begin
      if Modelo in kReg70 then
      begin
        Check(VerificaCPF_CNPJ(CNPJ), 'Registro70: No Registro50 o CNPJ inválido para o conhecimento número "%s"', [LFill(Numero, 6)]);
        Check(VerificaInscEstadual(InscEstadual, UF), 'Registro70: No Registro50 a Inscrição Estadual inválida para o conhecimento número "%s"', [LFill(Numero, 6)]);
        Check((Data >= DataInicial) and (Data <= DataFinal), 'Registro70: No Registro50 a Data de emissão "%s" fora do período "%s" a "%s" no conhecimento "%s".', [DateToStr(Data), DateToStr(DataInicial), DateToStr(DataFinal), LFill(Numero, 6)]);
        Check(VerificaUF(UF), 'Registro70: No Registro50 a UF "%s" inválida no conhecimento "%s"', [UF, LFill(Numero, 6)]);

        //@@
        // Como conhecimento de transporte não tem items a lista 54 (itens) é limpa.
        Registro54.Clear;

        for intFor50 := 0 to Registro50.Count - 1 do
        begin
          Check(VerificaCFOP(Registro50[intFor50].CFOP), 'Registro70: No Registro50 o CFOP "%d" inválido para o conhecimento número "%s"', [Registro50[intFor50].CFOP, LFill(Numero, 6)]);
          Check(Copy(IntToStr(Registro50[intFor50].CFOP), 2, 2) = '35', 'Registro70: No Registro50 o CFOP "%d" inválido para prestação de serviço de transporte no conhecimento número "%s".', [Registro50[intFor50].CFOP, LFill(Numero, 6)]);

          sRegistro70 := sRegistro70 + '70' +
            LFill(CNPJ, 14) +
            RFill(InscEstadual, 14) +
            LFill(Data) +
            UF +
            LFill(Modelo, 2) +
            RFill(Serie, 1) +
            RFill(SubSerie, 2) +
            LFill(Numero, 6) +
            IntToStr(Registro50[intFor50].CFOP) +
            LFill(Registro50[intFor50].ValorTotal, 13) +
            LFill(Registro50[intFor50].BaseIcms, 14) +
            LFill(Registro50[intFor50].ValorICMS, 14) +
            LFill(Registro50[intFor50].IsentasNTribut, 14) +
            LFill(Registro50[intFor50].Outras, 14) +
            RetornaFrete(ModFrete) +
            RetornaSituacao(Situacao) +
            #13#10;
        end;
      end;
    end;
  end;

  Result := sRegistro70;
end;

function TSintegra.Registro_71: string;
var
intFor5X, intFor71: integer;
sRegistro71: string;
begin
  sRegistro71 := '';

  for intFor5X := 0 to Registro5X.Count - 1 do
  begin
    with Registro5X[intFor5X] do
    begin
      if (Modelo in kReg71) then
      begin
        Check(VerificaCPF_CNPJ(CNPJ), 'Registro71: No Registro50 o CNPJ inválido para o conhecimento número "%s"', [LFill(Numero, 6)]);
        Check(VerificaInscEstadual(InscEstadual, UF), 'Registro71: No Registro50 a Inscrição Estadual inválida para o conhecimento número "%s"', [LFill(Numero, 6)]);
        Check((Data >= DataInicial) and (Data <= DataFinal), 'Registro71: No Registro50 a Data de emissão "%s" fora do período "%s" a "%s" no conhecimento "%s".', [DateToStr(Data), DateToStr(DataInicial), DateToStr(DataFinal), LFill(Numero, 6)]);
        Check(VerificaUF(UF), 'Registro71: No Registro50 a UF "%s" inválida no conhecimento "%s"', [UF, LFill(Numero, 6)]);

        for intFor71 := 0 to Registro71.Count - 1 do
        begin
          Check(VerificaCPF_CNPJ(Registro71[intFor71].CNPJ), 'Registro71: No Registro50 o CNPJ Nota fiscal inválido para o conhecimento número "%s"', [LFill(Numero, 6)]);
          Check(VerificaInscEstadual(Registro71[intFor71].InscEstadual, Registro71[intFor71].UF), 'Registro71: No Registro50 a Inscrição Estadual Nota fiscal inválida para o conhecimento número "%s"', [LFill(Numero, 6)]);
          Check((Registro71[intFor71].Data >= DataInicial) and (Registro71[intFor71].Data <= DataFinal), 'Registro71: No Registro50 a Data de emissão "%s" fora do período "%s" a "%s" no conhecimento "%s".', [DateToStr(Registro71[intFor71].Data), DateToStr(DataInicial), DateToStr(DataFinal), LFill(Numero, 6)]);
          Check(VerificaUF(Registro71[intFor71].UF), 'Registro71: No Registro50 a UF "%s" da Nota fiscal inválida no conhecimento "%s"', [UF, LFill(Numero, 6)]);

          sRegistro71 := sRegistro71 + '71' +
            LFill(CNPJ, 14) +
            RFill(InscEstadual, 14) +
            LFill(Data) +
            UF +
            LFill(Modelo, 2) +
            RFill(Serie, 1) +
            RFill(SubSerie, 2) +
            LFill(Numero, 6) +
            Registro71[intFor71].UF +
            LFill(Registro71[intFor71].CNPJ, 14) +
            RFill(Registro71[intFor71].InscEstadual, 14) +
            LFill(Registro71[intFor71].Data) +
            LFill(Registro71[intFor71].Modelo, 2) +
            RFill(Registro71[intFor71].Serie, 3) +
            LFill(Registro71[intFor71].Numero, 6) +
            LFIll(Registro71[intFor71].ValorTotal, 14) +
            RFill('', 12) +
            #13#10;
        end;
      end;
    end;
  end;

  Result := sRegistro71;
end;

function TSintegra.Registro_74: string;
var
sRegistro74, strInscEstadual: string;
intFor74: integer;
begin
  sRegistro74 := '';

  { Registro de Registro74 de Registro75 }
  for intFor74 := 0 to Registro74.Count - 1 do
  begin
    with Registro74[intFor74] do
    begin

      { Se o tipo de posse for igual a 1 (proprio) entao a inscrição deve ser em branco }
      if TipoPosse = tpo1 then
        strInscEstadual := ''
      else
        strInscEstadual := InscEstadual;

      Check(CodProduto, 'Registro74: informe o Código do produto!');
      Check(UF, 'Registro74: Informe a UF para o produto "%s"!', [CodProduto]);
      Check(DataInventario > 0, 'Registro74: Informe da data do inventário!');
      Check(DataInventario, DataInicial, DataFinal, 'Registro74: Data do Inventário "%s" fora do período informado "%s à %s"!', [DatetoStr(DataInventario), DatetoStr(DataInicial), DatetoStr(DataFinal)]);
      Check(VerificaUF(UF), 'Registro74: UF "%s" digitada inválida!', [UF]);
      Check(VerificaCPF_CNPJ(CNPJ), 'Registro74: CNPJ "%s" digitado inválido!', [CNPJ]);
      Check(VerificaInscEstadual(strInscEstadual, UF), 'Registro74: Inscrição Estadual "%s" digitada para o produto "%s" incorreta!', [InscEstadual, CodProduto]);
      Check(VerificaProduto(CodProduto), 'Registro74: Produto "%s" não assinalado no Produto!', [CodProduto]);

      sRegistro74 := sRegistro74 + '74' +
        LFill(DataInventario) +
        RFill(CodProduto, 14) +
        LFill(Quantidade, 13, 3) +
        LFill(ValorTotal, 13) +
        RetornaCodPosse(TipoPosse) +
        LFill(CNPJ, 14) +
        RFill(strInscEstadual, 14) +
        UF +
        RFill('', 45) +
        #13#10;
    end;
  end;

  Result := sRegistro74;
end;


function TSintegra.Registro_75: string;
var
sRegistro75: string;
intFor75: integer;
begin
  sRegistro75 := '';

  { Cadastro dos produtos }
  for intFor75 := 0 to Registro75.Count - 1 do
  begin
    { O registro 75 deve ter algum correspondente nos registros 54, 60D, 60I, 60R, 74, 77 }
//    if VerificaCorrespondente(Registro75[intFor75].CodProduto) then
//    begin
      with Registro75[intFor75] do
      begin
        Check(CodProduto, 'Registro75: Informe o Código do Produto!');
        Check(Descricao, 'Registro75: Informe a Descrição do produto!');
        Check(Unidade, 'Registro75: Informe a Unidade do produto!');
        Check(AliquotaICMS, 0, 99.99, 'Registro75: Aliquota de ICMS inválida!');
        Check(AliquotaIPI, 0, 999.99, 'Registro75: Alíquota de IPI inválido!');
        Check(ReducaoBaseCalc, 0, 999.99, 'Registro75: Redução da base de calculo inválida!');
        Check(ValidadeInicial > 0, 'Registro75: Informe a Validade inicial do produto "%s"!', [CodProduto]);
        Check(ValidadeFinal > 0, 'Registro75: Informe a Validade final do produto "%s"!', [CodProduto]);
        Check(ValidadeInicial, DataInicial, DataFinal, 'Registro75: Data "%s" não corresponde ao período informado "%s à %s"!', [DatetoStr(ValidadeInicial), DateToStr(DataInicial), DateToStr(DataFinal)]);
        Check(ValidadeFinal, DataInicial, DataFinal, 'Registro75: Data "%s" não corresponde ao período informado "%s à %s"!', [DatetoStr(ValidadeFinal), DateToStr(DataInicial), DateToStr(DataFinal)]);

        if Registro1X.ContribuinteIPI then
          Check(AliquotaICMS > 0, 'Registro75: Alíquota do IPI e obrigatória para empresas Contribuintes do IPI ');

        sRegistro75 := sRegistro75 + '75' +
          LFill(ValidadeInicial) +
          LFill(ValidadeFinal) +
          RFill(CodProduto, 14) +
          RFill(CodNCM, 8) +
          RFill(Descricao, 53) +
          RFill(Unidade, 6) +
          LFill(AliquotaIPI, 5) +
          LFill(AliquotaICMS, 4) +
          LFill(ReducaoBaseCalc, 5) +
          LFill(BaseICMSST, 13) +
          #13#10;
      end;
//    end
//    else { deleta o Registro75 se nao tiver nenhum correspondente }
//      Registro75.Delete(intFor75);
  end;

  Result := sRegistro75;
end;

function TSintegra.Registro_85: string;
var
sRegistro85: string;
intFor85: integer;
sRegistro86: string;
intFor86: integer;
begin
  sRegistro85 := '';

  { Cadastro dos produtos }
  for intFor85 := 0 to Registro85.Count - 1 do
  begin
      with Registro85[intFor85] do
      begin
        sRegistro85 := sRegistro85 + '85' +
          LFill(Declaracao, 11) +
          LFill(DataDeclaracao) +
          LFill(NaturezaExportacao) +
          LFill(RegistroExportacao, 12) +
          LFill(DataRegistro) +
          LFill(Conhecimento) +
          LFill(DataConhecimento) +
          LFill(TipoConhecimento, 2) +
          LFill(Pais, 4) +
          LFill(DataAverbacao) +
          LFill(NumeroNotaFiscal, 6) +
          LFill(DataNotaFiscal) +
          LFill(Modelo, 2) +
          LFill(Serie, 3) +
          LFill('', 19) +
          #13#10;
      end;
  end;

  sRegistro86 := '';

  { Cadastro dos produtos }
  for intFor86 := 0 to Registro86.Count - 1 do
  begin
      with Registro86[intFor86] do
      begin
        Check(Codigo, 'Registro86: informe o Código do Produto!');
        Check(UF, 'Registro86: Informe a UF para o produto "%s"!', [Codigo]);

        sRegistro86 := sRegistro86 + '86' +
          LFill(RegistroExportacao, 12) +
          LFill(DataRegistro) +
          LFill(CPFCNPJ) +
          LFill(Inscricao) +
          LFill(UF) +
          LFill(NumeroNotaFiscal, 6) +
          LFill(DataDocumento) +
          LFill(Modelo, 2) +
          LFill(Serie, 3) +
          LFill(Codigo) +
          LFill(Quantidade, 11, 3) +
          LFill(ValorUnitario, 12, 2) +
          LFill(ValorTotalProduto, 12, 2) +
          LFill(Relacionamento) +
          LFill('', 5) +
          #13#10;
      end;
  end;

  Result := sRegistro85 + sRegistro86;
end;

{ *******************************************************************************
  *  A quantidade de registros 90 deve ser informada usando as 6 ultimas
     posicoes do registro alinhado a direita Ex: '     1';

  *  Podem ser usados 9 contadores por registro 90 sendo 2 posicoes
     para o codigo do registro ex: 50, 54, etc e 8 posicoes para a quantidade
     de registros;

  *  o contador 99 deve informar a quantidade total incluindo
     os registros 10, 11, e 90 e deve ser informado somente no
     ultimo registro 90 caso haja mais de um registro 90;
******************************************************************************* }
function TSintegra.Registro_90: string;
var
  intFor,
  ToTReg90,
  TotReg50, TotReg51, TotReg53, TotReg54, TotReg56, TotReg55,
  TotReg60, TotReg61,
  TotReg70, TotReg71, TotReg74, TotReg75, TotReg76, TotReg77,
  TotReg85, TotReg86, TotReg88: Integer;

  strInicio, strLinha: string;

  lstLista: TStringList;

  procedure VerificaLinha;
  begin
    if Length(strLinha) = 90 then strLinha := strLinha + #13#10;
  end;
begin
  ToTReg90 := 3;
  TotReg50 := 0;
  TotReg51 := 0;
  TotReg53 := 0;
  TotReg54 := 0;
  TotReg56 := 0;
  TotReg55 := 0;
  TotReg60 := 0;
  TotReg61 := 0;
  TotReg70 := 0;
  TotReg71 := 0;
  TotReg74 := 0;
  TotReg75 := 0;
  TotReg76 := 0;
  TotReg77 := 0;
  TotReg85 := 0;
  TotReg86 := 0;
  TotReg88 := 0;

  strInicio := '90' + LFill(Registro1X.CNPJ, 14) + RFill(Registro1X.InscEstadual, 14);

  lstLista := TStringList.Create;
  try
    { Total de registros 5X }
    if (Assigned(Registro5X)) and (Registro5X.Count > 0) then
    begin
      for intFor := 0 to Registro5X.Count - 1 do
      begin
        with Registro5X[intFor] do
        begin
          if (Assigned(Registro50)) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg50 := TotReg50 + Registro50.Count;

          if (Assigned(Registro50)) and (Registro5X[intFor].Modelo in kReg50) and (Registro50.Count = 0) then
             TotReg50 := TotReg50 + 1;

          if Assigned(Registro51) then
             TotReg51 := TotReg51 + Registro51.Count;

          if Assigned(Registro53) then
             TotReg53 := TotReg53 + Registro53.Count;

          if (Assigned(Registro54)) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg54 := TotReg54 + Registro54.Count;

          if (Frete <> 0) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg54 := TotReg54 + 1;

          if (Seguro <> 0) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg54 := TotReg54 + 1;

          if (DespAcessoria <> 0) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg54 := TotReg54 + 1;

          if (ValorPisCofins <> 0) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg54 := TotReg54 + 1;

          if (ValorComplementar <> 0) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg54 := TotReg54 + 1;

          if (ValorServicoNaoTributado <> 0) and (Registro5X[intFor].Modelo in kReg50) then
             TotReg54 := TotReg54 + 1;
        end;
      end;
    end;

    { total de registros 60 }
    if (Assigned(Registro60M)) and (Registro60M.Count > 0) then
    begin
      TotReg60 := Registro60M.Count;

      if Assigned(Registro60R) and (Registro60R.Count > 0) then
         TotReg60 := TotReg60 + Registro60R.Count;

      for intFor := 0 to Registro60M.Count - 1 do
      begin
        with Registro60M[intFor] do
        begin
          if Assigned(Registro60A) then
             TotReg60 := TotReg60 + Registro60A.Count;

          if Assigned(Registro60D) then
             TotReg60 := TotReg60 + Registro60D.Count;

          if Assigned(Registro60I) then
             TotReg60 := TotReg60 + Registro60I.Count;
        end;
      end;
    end;

    { total de registro 61 }
    if Assigned(Registro61) and (Registro61.Count > 0) then
       TotReg61 := Registro61.Count;

    if Assigned(Registro61R) then
       TotReg61 := TotReg61 + Registro61R.Count;

    { total de registros 70 }
    if Assigned(Registro5X) and (Registro5X.Count > 0) then
    begin
      for intFor := 0 to Registro5X.Count - 1 do
      begin
        if Registro5X[intFor].Modelo in kReg70 then
           TotReg70 := TotReg70 + Registro5X[intFor].Registro50.Count;
      end;
    end;

    { total de registros 71 }
    if Assigned(Registro5X) and (Registro5X.Count > 0) then
    begin
      for intFor := 0 to Registro5X.Count - 1 do
      begin
          if Registro5X[intFor].Modelo in kReg71 then
             TotReg71 := TotReg71 + Registro5X[intFor].Registro71.Count;
      end;
    end;

    { total de registros 74 }
    if Assigned(Registro74) and (Registro74.Count > 0) then
       TotReg74 := Registro74.Count;

    { total de registros 75 }
    if Assigned(Registro75) and (Registro75.Count > 0) then
       TotReg75 := Registro75.Count;

    { total de registros 85 }
    if Assigned(Registro85) and (Registro85.Count > 0) then
       TotReg85 := Registro85.Count;

    { total de registros 86 }
    if Assigned(Registro86) and (Registro86.Count > 0) then
       TotReg86 := Registro86.Count;

    ToTReg90 := ToTReg90 +
                TotReg50 + TotReg51 + TotReg53 + TotReg54 + TotReg55 + TotReg56 +
                TotReg60 + TotReg61 +
                TotReg70 + TotReg71 + TotReg74 + TotReg75 + TotReg76 + TotReg77 +
                TotReg85 + TotReg86 + TotReg88;

    if TotReg50 > 0 then
    begin
      strLinha := strLinha + '50' + LFill(TotReg50, 8);
      VerificaLinha;
    end;

    if TotReg51 > 0 then
    begin
      strLinha := strLinha + '51' + LFill(TotReg51, 8);
      VerificaLinha;
    end;

    if TotReg53 > 0 then
    begin
      strLinha := strLinha + '53' + LFill(TotReg53, 8);
      VerificaLinha;
    end;

    if TotReg54 > 0 then
    begin
      strLinha := strLinha + '54' + LFill(TotReg54, 8);
      VerificaLinha;
    end;

    if TotReg56 > 0 then
    begin
      strLinha := strLinha + '56' + LFill(TotReg56, 8);
      VerificaLinha;
    end;

    if TotReg55 > 0 then
    begin
      strLinha := strLinha + '55' + LFill(TotReg55, 8);
      VerificaLinha;
    end;

    if TotReg60 > 0 then
    begin
      strLinha := strLinha + '60' + LFill(TotReg60, 8);
      VerificaLinha;
    end;

    if TotReg61 > 0 then
    begin
      strLinha := strLinha + '61' + LFill(TotReg61, 8);
      VerificaLinha;
    end;

    if TotReg70 > 0 then
    begin
      strLinha := strLinha + '70' + LFill(TotReg70, 8);
      VerificaLinha;
    end;

    if TotReg71 > 0 then
    begin
      strLinha := strLinha + '71' + LFill(TotReg71, 8);
      VerificaLinha;
    end;

    if TotReg74 > 0 then
    begin
      strLinha := strLinha + '74' + LFill(TotReg74, 8);
      VerificaLinha;
    end;

    if TotReg75 > 0 then
    begin
      strLinha := strLinha + '75' + LFill(TotReg75, 8);
      VerificaLinha;
    end;

    if TotReg76 > 0 then
    begin
      strLinha := strLinha + '76' + LFill(TotReg76, 8);
      VerificaLinha;
    end;

    if TotReg77 > 0 then
    begin
      strLinha := strLinha + '77' + LFill(TotReg77, 8);
      VerificaLinha;
    end;

    if TotReg85 > 0 then
    begin
      strLinha := strLinha + '85' + LFill(TotReg85, 8);
      VerificaLinha;
    end;

    if TotReg86 > 0 then
    begin
      strLinha := strLinha + '86' + LFill(TotReg86, 8);
      VerificaLinha;
    end;

    if TotReg88 > 0 then
    begin
      strLinha := strLinha + '88' + LFill(TotReg88, 8);
      VerificaLinha;
    end;

    strLinha := strLinha + '99' + LFill(ToTReg90, 8);

    lstLista.Add(strLinha);

    strLinha := '';
    for intFor := 0 to lstLista.Count - 1 do
    begin
      if Length(lstLista.Strings[intFor]) = 90 then
      begin
        strLinha := strLinha +
                    strInicio +
                    lstLista.Strings[intFor] +
                    RFill(IntToStr(lstLista.Count), 6) + #13#10;
      end
      else
      begin
        strLinha := strLinha +
                    strInicio +
                    RFill(lstLista.Strings[intFor], 90) +
                    LFill(lstLista.Count, 6, ' ') + #13#10;
      end;
    end;
  finally
    lstLista.Free;
  end;
  Result := strLinha;
end;

{ TRegistro5XLista }

function TRegistro5XLista.New: IRegistro5X;
begin
  Result := TRegistro5X.Create;
  Add(Result);
end;

function TRegistro5XLista.GetItem(Index: Integer): IRegistro5X;
begin
  Result := Get(Index) as IRegistro5X;
end;

procedure TRegistro5XLista.SetItem(Index: Integer; const Value: IRegistro5X);
begin
  Put(Index, Value);
end;

function TRegistro5XLista.GetTotalEntradas: Integer;
var
intForI, intForA: Integer;
begin
  Result := 0;
  //
  for intForI := 0 to Count - 1 do
  begin
     for intForA := 0 to Items[intForI].Registro50.Count - 1 do
     begin
       if IntToStr(Items[intForI].Registro50[intForA].CFOP)[1] in ['1', '2', '3'] then
          Result := Result + 1;
     end;
  end;
end;

function TRegistro5XLista.GetTotalSaidas: Integer;
var
intForI, intForA: Integer;
begin
  Result := 0;
  //
  for intForI := 0 to Count - 1 do
  begin
     for intForA := 0 to Items[intForI].Registro50.Count - 1 do
     begin
       if IntToStr(Items[intForI].Registro50[intForA].CFOP)[1] in ['5', '6', '7'] then
          Result := Result + 1;
     end;
  end;
end;

{ TRegistro5X }

constructor TRegistro5X.Create;
begin
  inherited;
  fRegistro50 := TRegistro50Lista.Create;
  fRegistro51 := TRegistro51Lista.Create;
  fRegistro53 := TRegistro53Lista.Create;
  fRegistro54 := TRegistro54Lista.Create;
  fRegistro71 := TRegistro71Lista.Create;
end;

function TRegistro5X.GetRegistro50: IRegistro50Lista;
begin
  Result := fRegistro50;
end;

function TRegistro5X.GetCFOP: SmallInt;
begin
  Result := fCFOP;
end;

function TRegistro5X.GetCNPJ: string;
begin
  Result := fCNPJ;
end;

function TRegistro5X.GetRegistro51: IRegistro51Lista;
begin
  Result := fRegistro51;
end;

function TRegistro5X.GetData: TDateTime;
begin
  Result := fData;
end;

function TRegistro5X.GetDespAcessoria: Currency;
begin
  Result := fDespAcessoria;
end;

function TRegistro5X.GetFrete: Currency;
begin
  Result := fFrete;
end;

function TRegistro5X.GetInscEstadual: string;
begin
  Result := fInscEstadual;
end;

function TRegistro5X.GetRegistro54: IRegistro54Lista;
begin
  Result := fRegistro54;
end;

function TRegistro5X.GetRegistro71: IRegistro71Lista;
begin
  Result := fRegistro71;
end;

function TRegistro5X.GetModelo: SmallInt;
begin
  Result := fModelo;
end;

function TRegistro5X.GetNumero: Integer;
begin
  Result := fNumero;
end;

function TRegistro5X.GetSeguro: Currency;
begin
  Result := fSeguro;
end;

function TRegistro5X.GetSerie: string;
begin
  Result := fSerie;
end;

function TRegistro5X.GetSituacao: TSituacao;
begin
  Result := fSituacao;
end;

function TRegistro5X.GetRegistro53: IRegistro53Lista;
begin
  Result := fRegistro53
end;

function TRegistro5X.GetTipoEmitente: TTipoEmitente;
begin
  Result := fTipoEmitente;
end;

function TRegistro5X.GetUF: string;
begin
  Result := fUF;
end;

function TRegistro5X.GetValorComplementar: Currency;
begin
   Result := fValorComplementar;
end;

function TRegistro5X.GetValorPisCofins: Currency;
begin
   Result := fValorPisCofins;
end;

function TRegistro5X.GetValorServicoNaoTributado: Currency;
begin
   Result := fValorServicoNaoTributado;
end;

procedure TRegistro5X.SetRegistro51(const Valor: IRegistro51Lista);
begin
  fRegistro51 := Valor
end;

procedure TRegistro5X.SetRegistro50(const Valor: IRegistro50Lista);
begin
  fRegistro50 := Valor;
end;

procedure TRegistro5X.SetCFOP(const Valor: SmallInt);
begin
  fCFOP := Valor
end;

procedure TRegistro5X.SetCNPJ(const Valor: string);
begin
  fCNPJ := RetornaSoNumero(Valor);
end;

procedure TRegistro5X.SetData(const Valor: TDateTime);
begin
  fData := Valor;
end;

procedure TRegistro5X.SetDespAcessoria(const Valor: Currency);
begin
  fDespAcessoria := Valor
end;

procedure TRegistro5X.SetFrete(const Valor: Currency);
begin
  fFrete := Valor;
end;

procedure TRegistro5X.SetInscEstadual(const Valor: string);
begin
  fInscEstadual := RetiraMascara(Valor);
end;

procedure TRegistro5X.SetRegistro54(const Valor: IRegistro54Lista);
begin
  fRegistro54 := Valor;
end;

procedure TRegistro5X.SetRegistro71(const Valor: IRegistro71Lista);
begin
  fRegistro71 := Valor;
end;

procedure TRegistro5X.SetModelo(const Valor: SmallInt);
begin
  fModelo := Valor;
end;

procedure TRegistro5X.SetNumero(const Valor: Integer);
begin
  fNumero := Valor;
end;

procedure TRegistro5X.SetSeguro(const Valor: Currency);
begin
  fSeguro := Valor;
end;

procedure TRegistro5X.SetSerie(const Valor: string);
begin
  fSerie := UpperCase(Valor);
end;

procedure TRegistro5X.SetSituacao(const Valor: TSituacao);
begin
  fSituacao := Valor;
end;

procedure TRegistro5X.SetRegistro53(const Valor: IRegistro53Lista);
begin
  fRegistro53 := Valor;
end;

procedure TRegistro5X.SetTipoEmitente(const Valor: TTipoEmitente);
begin
  fTipoEmitente := Valor;
end;

procedure TRegistro5X.SetUF(const Valor: string);
begin
  fUF := UpperCase(Valor);
end;

procedure TRegistro5X.SetValorComplementar(const Valor: Currency);
begin
   fValorComplementar := Valor;
end;

procedure TRegistro5X.SetValorPisCofins(const Valor: Currency);
begin
   fValorPisCofins := Valor;
end;

procedure TRegistro5X.SetValorServicoNaoTributado(const Valor: Currency);
begin
   fValorServicoNaoTributado := Valor;
end;

function TRegistro5X.GetModFrete: TModalidadeFrete;
begin
  Result := FModFrete;
end;

function TRegistro5X.GetSubSerie: string;
begin
  Result := fSubSerie;
end;

procedure TRegistro5X.SetModFrete(const Value: TModalidadeFrete);
begin
  FModFrete := Value;
end;

procedure TRegistro5X.SetSubSerie(const Value: string);
begin
  fSubSerie := UpperCase(Value);
end;

//function TRegistro5X.GetCargaNFCNPJ: String;
//begin
//  Result := FCargaNFCNPJ;
//end;

//function TRegistro5X.GetCargaNFData: TDateTime;
//begin
//  Result := FCargaNFData;
//end;

//function TRegistro5X.GetCargaNFInscEstadual: String;
//begin
//  Result := FCargaNFInscEstadual;
//end;

//function TRegistro5X.GetCargaNFModelo: SmallInt;
//begin
//  Result := FCargaNFModelo;
//end;

//function TRegistro5X.GetCargaNFNumero: Integer;
//begin
//  Result := FCargaNFNumero;
//end;

//function TRegistro5X.GetCargaNFSerie: String;
//begin
//  Result := FCargaNFSerie;
//end;

//function TRegistro5X.GetCargaNFUF: String;
//begin
//  Result := FCargaNFUF;
//end;

//function TRegistro5X.GetCargaNFVlrTotal: Currency;
//begin
//  Result := FCargaNFVlrTotal;
//end;

//procedure TRegistro5X.SetCargaNFCNPJ(const Value: String);
//begin
//  FCargaNFCNPJ := RetornaSoNumero(Value);
//end;

//procedure TRegistro5X.SetCargaNFData(const Value: TDateTime);
//begin
//  FCargaNFData := Value;
//end;

//procedure TRegistro5X.SetCargaNFInscEstadual(const Value: String);
//begin
//  FCargaNFInscEstadual := RetiraMascara(Value);
//end;

//procedure TRegistro5X.SetCargaNFModelo(const Value: SmallInt);
//begin
//  FCargaNFModelo := Value;
//end;

//procedure TRegistro5X.SetCargaNFNumero(const Value: Integer);
//begin
//  FCargaNFNumero := Value;
//end;

//procedure TRegistro5X.SetCargaNFSerie(const Value: String);
//begin
//  FCargaNFSerie := UpperCase(Value);
//end;

//procedure TRegistro5X.SetCargaNFUF(const Value: String);
//begin
//  FCargaNFUF := UpperCase(Value);
//end;

//procedure TRegistro5X.SetCargaNFVlrTotal(const Value: Currency);
//begin
//  FCargaNFVlrTotal := Value;
//end;

{ TRegistro54Lista }

function TRegistro54Lista.New: IRegistro54;
begin
  Result := TRegistro54.Create;
  Add(Result);
end;

function TRegistro54Lista.GetItem(Index: Integer): IRegistro54;
begin
  Result := Get(Index) as IRegistro54;
end;

procedure TRegistro54Lista.SetItem(Index: Integer; const Value: IRegistro54);
begin
  Put(Index, Value);
end;

{ TRegistro54 }

function TRegistro54.GetAliquota: Currency;
begin
  Result := fAliquota;
end;

function TRegistro54.GetBaseICMS: Currency;
begin
  Result := fBaseICMS;
end;

function TRegistro54.GetBaseICMSST: Currency;
begin
  Result := fBaseICMSST;
end;

function TRegistro54.GetCFOP: SmallInt;
begin
  Result := fCFOP;
end;

function TRegistro54.GetCodProduto: string;
begin
  Result := fCodProduto;
end;

function TRegistro54.GetCST: string;
begin
  Result := fCST;
end;

function TRegistro54.GetDesconto: Currency;
begin
  Result := fDesconto;
end;

function TRegistro54.GetIPI: Currency;
begin
  Result := fIPI;
end;

function TRegistro54.GetNumItem: SmallInt;
begin
  Result := fNumItem;
end;

function TRegistro54.GetQuantidade: Extended;
begin
  Result := fQuantidade;
end;

function TRegistro54.GetValorProduto: Currency;
begin
  Result := fValorProduto;
end;

procedure TRegistro54.SetAliquota(const Valor: Currency);
begin
  fAliquota := Valor;
end;

procedure TRegistro54.SetBaseICMS(const Valor: Currency);
begin
  fBaseICMS := Valor;
end;

procedure TRegistro54.SetBaseICMSST(const Valor: Currency);
begin
  fBaseICMSST := Valor;
end;

procedure TRegistro54.SetCFOP(const Valor: SmallInt);
begin
  fCFOP := Valor
end;

procedure TRegistro54.SetCodProduto(const Valor: string);
begin
  fCodProduto := Valor;
end;

procedure TRegistro54.SetCST(const Valor: string);
begin
  fCST := Valor;
end;

procedure TRegistro54.SetDesconto(const Valor: Currency);
begin
  fDesconto := Valor;
end;

procedure TRegistro54.SetIPI(const Valor: Currency);
begin
  fIPI := Valor;
end;

procedure TRegistro54.SetNumItem(const Valor: SmallInt);
begin
  fNumItem := Valor;
end;

procedure TRegistro54.SetQuantidade(const Valor: Extended);
begin
  fQuantidade := Valor;
end;

procedure TRegistro54.SetValorProduto(const Valor: Currency);
begin
  fValorProduto := Valor;
end;

{ TRegistro50 }

function TRegistro50.GetAliquota: Currency;
begin
  Result := fAliquota;
end;

function TRegistro50.GetBaseICMS: Currency;
begin
  Result := fBaseICMS;
end;

function TRegistro50.GetCFOP: SmallInt;
begin
  Result := fCFOP;
end;

function TRegistro50.GetIsentasNTribut: Currency;
begin
  Result := fIsentasNTribut;
end;

function TRegistro50.GetOutras: Currency;
begin
  Result := fOutras;
end;

function TRegistro50.GetValorICMS: Currency;
begin
  Result := fValorICMS;
end;

function TRegistro50.GetValorTotal: Currency;
begin
  Result := fValorTotal;
end;

procedure TRegistro50.SetCFOP(const Valor: SmallInt);
begin
  fCFOP := Valor;
end;

procedure TRegistro50.SetAliquota(const Valor: Currency);
begin
  fAliquota := Valor;
end;

procedure TRegistro50.SetBaseICMS(const Valor: Currency);
begin
  fBaseICMS := Valor;
end;

procedure TRegistro50.SetValorICMS(const Valor: Currency);
begin
  fValorICMS := Valor;
end;

procedure TRegistro50.SetOutras(const Valor: Currency);
begin
  fOutras := Valor;
end;

procedure TRegistro50.SetIsentasNTribut(const Valor: Currency);
begin
  fIsentasNTribut := Valor;
end;

procedure TRegistro50.SetValorTotal(const Valor: Currency);
begin
  fValorTotal := Valor;
end;

{ TRegistro50Lista }


function TRegistro50Lista.New: IRegistro50;
begin
  Result := TRegistro50.Create;
  Add(Result);
end;

function TRegistro50Lista.GetItem(Index: Integer): IRegistro50;
begin
  Result := Get(Index) as IRegistro50;
end;

procedure TRegistro50Lista.SetItem(Index: Integer; const Value: IRegistro50);
begin
  Put(Index, Value);
end;

{ TRegistro51Lista }

function TRegistro51Lista.New: IRegistro51;
begin
  Result := TRegistro51.Create;
  Add(Result);
end;

function TRegistro51Lista.GetItem(Index: Integer): IRegistro51;
begin
  Result := Get(Index) as IRegistro51;
end;

procedure TRegistro51Lista.SetItem(Index: Integer; const Value: IRegistro51);
begin
  Put(Index, Value);
end;

{ TRegistro51 }

function TRegistro51.GetCFOP: SmallInt;
begin
  Result := fCFOP;
end;

function TRegistro51.GetIsentaNTrib: Currency;
begin
  Result := fIsentaNTrib;
end;

function TRegistro51.GetOutras: Currency;
begin
  Result := fOutras;
end;

function TRegistro51.GetValor: Currency;
begin
  Result := fValor;
end;

function TRegistro51.GetValorIPI: Currency;
begin
  Result := fValorIPI;
end;

procedure TRegistro51.SetCFOP(const Valor: SmallInt);
begin
  fCFOP := Valor;
end;

procedure TRegistro51.SetIsentaNTrib(const Valor: Currency);
begin
  fIsentaNTrib := Valor;
end;

procedure TRegistro51.SetOutras(const Valor: Currency);
begin
  fOutras := Valor;
end;

procedure TRegistro51.SetValor(const Valor: Currency);
begin
  fValor := Valor;
end;

procedure TRegistro51.SetValorIPI(const Valor: Currency);
begin
  fValorIPI := Valor;
end;

{ TRegistro53Lista }

function TRegistro53Lista.New: IRegistro53;
begin
  Result := TRegistro53.Create;
  Add(Result);
end;

function TRegistro53Lista.GetItem(Index: Integer): IRegistro53;
begin
  Result := Get(Index) as IRegistro53;
end;

procedure TRegistro53Lista.SetItem(Index: Integer; const Value: IRegistro53);
begin
  Put(Index, Value);
end;

{ TRegistro53 }

function TRegistro53.GetBaseCalcICMSST: Currency;
begin
  Result := fBaseCalcICMSST;
end;

function TRegistro53.GetCFOP: SmallInt;
begin
  Result := fCFOP;
end;

function TRegistro53.GetCodAntecipacao: string;
begin
   Result := FCodAntecipacao;
end;

function TRegistro53.GetDespAcessoria: Currency;
begin
  Result := fDespAcessoria;
end;

function TRegistro53.GetICMSRetido: Currency;
begin
  Result := fICMSRetido;
end;

procedure TRegistro53.SetBaseCalcICMSST(const Valor: Currency);
begin
  fBaseCalcICMSST := Valor;
end;

procedure TRegistro53.SetCFOP(const Valor: SmallInt);
begin
  fCFOP := Valor;
end;

procedure TRegistro53.SetCodAntecipacao(const Value: string);
begin
  FCodAntecipacao := Value;
end;

procedure TRegistro53.SetDespAcessoria(const Valor: Currency);
begin
  fDespAcessoria := Valor;
end;

procedure TRegistro53.SetICMSRetido(const Valor: Currency);
begin
  fICMSRetido := Valor;
end;

{ TRegistro55Lista }

function TRegistro55Lista.New: IRegistro55;
begin
  Result := TRegistro55.Create;
  Add(Result);
end;

function TRegistro55Lista.GetItem(Index: Integer): IRegistro55;
begin
  Result := Get(Index) as IRegistro55;
end;

procedure TRegistro55Lista.SetItem(Index: Integer; const Value: IRegistro55);
begin
  Put(Index, Value);
end;

{ TRegistro55 }

function TRegistro55.GetAgencia: SmallInt;
begin
  Result := fAgencia;
end;

function TRegistro55.GetAnoRef: SmallInt;
begin
  Result := fAnoRef;
end;

function TRegistro55.GetBanco: SmallInt;
begin
  Result := fBanco;
end;

function TRegistro55.GetDataPagamento: TDateTime;
begin
  Result := fDataPagamento;
end;

function TRegistro55.GetMesRef: SmallInt;
begin
  Result := fMesRef;
end;

function TRegistro55.GetNumConvenio: string;
begin
  Result := fNumConvenio;
end;

function TRegistro55.GetNumeroGNRE: string;
begin
  Result := fNumeroGNRE;
end;

function TRegistro55.GetUFContribuinte: string;
begin
  Result := fUFContribuinte;
end;

function TRegistro55.GetUFFavorecido: string;
begin
  Result := fUFFavorecido;
end;

function TRegistro55.GetValor: Currency;
begin
  Result := fValor;
end;

function TRegistro55.GetVencimento: TDateTime;
begin
  Result := fVencimento;
end;

procedure TRegistro55.SetAgencia(const Valor: SmallInt);
begin
  fAgencia := Valor;
end;

procedure TRegistro55.SetAnoRef(const Valor: SmallInt);
begin
  fAnoRef := Valor;
end;

procedure TRegistro55.SetBanco(const Valor: SmallInt);
begin
  fBanco := Valor;
end;

procedure TRegistro55.SetDataPagamento(const Valor: TDateTime);
begin
  fDataPagamento := Valor;
end;

procedure TRegistro55.SetMesRef(const Valor: SmallInt);
begin
  fMesRef := Valor;
end;

procedure TRegistro55.SetNumConvenio(const Valor: string);
begin
  fNumConvenio := Valor;
end;

procedure TRegistro55.SetNumeroGNRE(const Valor: string);
begin
  fNumeroGNRE := Valor;
end;

procedure TRegistro55.SetUFContribuinte(const Valor: string);
begin
  fUFContribuinte := Uppercase(Valor);
end;

procedure TRegistro55.SetUFFavorecido(const Valor: string);
begin
  fUFFavorecido := Uppercase(Valor);
end;

procedure TRegistro55.SetValor(const Valor: Currency);
begin
  fValor := Valor;
end;

procedure TRegistro55.SetVencimento(const Valor: TDateTime);
begin
  fVencimento := Valor;
end;

{ TRegistro60M }

constructor TRegistro60M.Create;
begin
  inherited;
  fVendaBruta := 0;
  fGTFinal := 0;
  fCFICMS := TRegistro60ALista.Create;
  fResumoDia := TRegistro60DLista.Create;
  fCFItem := TRegistro60ILista.Create;
end;

function TRegistro60M.GetDataEmissao: TDateTime;
begin
  Result := fDataEmissao
end;

procedure TRegistro60M.SetDataEmissao(const Valor: TDateTime);
begin
  fDataEmissao := Valor
end;

function TRegistro60M.GetNumSerieEquip: string;
begin
  Result := fNumSerieEquip;
end;

procedure TRegistro60M.SetNumSerieEquip(const Valor: string);
begin
  fNumSerieEquip := Valor;
end;

function TRegistro60M.GetNumSequencial: Integer;
begin
  Result := fNumSequencial
end;

procedure TRegistro60M.SetNumSequencial(const Valor: Integer);
begin
  fNumSequencial := Valor;
end;

function TRegistro60M.GetModDocFiscal: TModDocumento;
begin
  Result := fModDocFiscal
end;

procedure TRegistro60M.SetModDocFiscal(const Valor: TModDocumento);
begin
  fModDocFiscal := Valor;
end;

function TRegistro60M.GetCOOInicial: Integer;
begin
  Result := fCOOInicial
end;

procedure TRegistro60M.SetCOOInicial(const Valor: Integer);
begin
  fCOOInicial := Valor;
end;

function TRegistro60M.GetCOOFinal: Integer;
begin
  Result := fCOOFinal
end;

procedure TRegistro60M.SetCOOFinal(const Valor: Integer);
begin
  fCOOFinal := Valor;
end;

function TRegistro60M.GetContReducaoZ: Integer;
begin
  Result := fContReducaoZ
end;

procedure TRegistro60M.SetContReducaoZ(const Valor: Integer);
begin
  fContReducaoZ := Valor;
end;

function TRegistro60M.GetContReinicioOper: Integer;
begin
  Result := fContReinicioOper
end;

procedure TRegistro60M.SetContReinicioOper(const Valor: Integer);
begin
  fContReinicioOper := Valor;
end;

function TRegistro60M.GetVendaBruta: Currency;
begin
  Result := fVendaBruta
end;

procedure TRegistro60M.SetVendaBruta(const Valor: Currency);
begin
  fVendaBruta := Valor;
end;

function TRegistro60M.GetGTFinal: Currency;
begin
  Result := fGTFinal
end;

procedure TRegistro60M.SetGTFinal(const Valor: Currency);
begin
  fGTFinal := Valor;
end;

function TRegistro60M.GetRegistro60A: IRegistro60ALista;
begin
  Result := fCFICMS;
end;

procedure TRegistro60M.SetRegistro60A(const Valor: IRegistro60ALista);
begin
  fCFICMS := Valor;
end;

function TRegistro60M.GetRegistro60D: IRegistro60DLista;
begin
  Result := fResumoDia;
end;

procedure TRegistro60M.SetRegistro60D(const Valor: IRegistro60DLista);
begin
  fResumoDia := Valor;
end;

function TRegistro60M.GetRegistro60I: IRegistro60ILista;
begin
  Result := fCFItem;
end;

procedure TRegistro60M.SetRegistro60I(const Valor: IRegistro60ILista);
begin
  fCFItem := Valor;
end;

{ TRegistro60MLista }

function TRegistro60MLista.New: IRegistro60M;
begin
  Result := TRegistro60M.Create;
  Add(Result);
end;

function TRegistro60MLista.GetItem(Index: Integer): IRegistro60M;
begin
  Result := Get(Index) as IRegistro60M;
end;

procedure TRegistro60MLista.SetItem(Index: Integer; const Value: IRegistro60M);
begin
  Put(Index, Value);
end;

{ TRegistro60A }

function TRegistro60A.GetValorAcumulado: Currency;
begin
  Result := fValorAcumulado
end;

procedure TRegistro60A.SetValorAcumulado(const Valor: Currency);
begin
  fValorAcumulado := Valor;
end;

function TRegistro60A.GetSitTributaria: string;
begin
  Result := fSitTributaria;
end;

procedure TRegistro60A.SetSitTributaria(const Valor: string);
begin
  fSitTributaria := Valor;
end;

{ TRegistro60ALista }

function TRegistro60ALista.New: IRegistro60A;
begin
  Result := TRegistro60A.Create;
  Add(Result);
end;

function TRegistro60ALista.GetItem(Index: Integer): IRegistro60A;
begin
  Result := Get(Index) as IRegistro60A;
end;

procedure TRegistro60ALista.SetItem(Index: Integer; const Value: IRegistro60A);
begin
  Put(Index, Value);
end;

function TRegistro60ALista.Total: Currency;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + items[i].ValorAcumulado;
end;

{ TRegistro60D }

constructor TRegistro60D.Create;
begin
  inherited;
  fBaseCalcICMS := 0;
  fSitTributaria := '0';
end;

function TRegistro60D.GetCodProduto: string;
begin
  Result := fCodProduto;
end;

procedure TRegistro60D.setCodProduto(const Valor: string);
begin
  fCodProduto := Valor;
end;

function TRegistro60D.GetQuantAcumulada: Extended;
begin
  Result := fQuantAcumulada
end;

procedure TRegistro60D.SetQuantAcumulada(const Valor: Extended);
begin
  fQuantAcumulada := Valor;
end;

function TRegistro60D.GetValorAcumulado: Currency;
begin
  Result := fValorAcumulado
end;

procedure TRegistro60D.SetValorAcumulado(const Valor: Currency);
begin
  fValorAcumulado := Valor;
end;

function TRegistro60D.GetBaseCalcICMS: Currency;
begin
  Result := fBaseCalcICMS
end;

procedure TRegistro60D.SetBaseCalcICMS(const Valor: Currency);
begin
  fBaseCalcICMS := Valor;
end;

function TRegistro60D.GetSitTributaria: string;
begin
  Result := fSitTributaria;
end;

procedure TRegistro60D.SetSitTributaria(const Valor: string);
begin
  fSitTributaria := Valor;
end;

function TRegistro60D.GetValorICMS: Currency;
var
  aAliquota: Currency;
begin
  aAliquota := StrToNumero(fSitTributaria, 2);
  Result := fBaseCalcICMS * (aAliquota / 100);
end;

{ TRegistro60DLista }

function TRegistro60DLista.New: IRegistro60D;
begin
  Result := TRegistro60D.Create;
  Add(Result);
end;

function TRegistro60DLista.GetItem(Index: Integer): IRegistro60D;
begin
  Result := Get(Index) as IRegistro60D;
end;

procedure TRegistro60DLista.SetItem(Index: Integer; const Value: IRegistro60D);
begin
  Put(Index, Value);
end;

function TRegistro60DLista.GetTotalAcumulado: Currency;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + items[i].ValorAcumulado;
end;

{ TRegistro60I }

function TRegistro60I.GetCOOCupom: Integer;
begin
  Result := fCOOCupom;
end;

procedure TRegistro60I.SetCOOCupom(const Valor: Integer);
begin
  fCOOCupom := Valor;
end;

function TRegistro60I.GetNumItem: Integer;
begin
  Result := fNumItem;
end;

procedure TRegistro60I.SetNumItem(const Valor: Integer);
begin
  fNumItem := Valor;
end;

function TRegistro60I.GetCodProduto: string;
begin
  Result := fCodProduto;
end;

procedure TRegistro60I.SetCodProduto(const Valor: string);
begin
  fCodProduto := Valor;
end;

function TRegistro60I.GetQuantidade: Extended;
begin
  Result := fQuantidade;
end;

procedure TRegistro60I.SetQuantidade(const Valor: Extended);
begin
  fQuantidade := Valor;
end;

function TRegistro60I.GetValorUnitario: Currency;
begin
  Result := fValorUnitario;
end;

procedure TRegistro60I.SetValorICMS(const Valor: Currency);
begin
   fValorICMS := Valor;
end;

procedure TRegistro60I.SetValorUnitario(const Valor: Currency);
begin
  fValorUnitario := Valor;
end;

function TRegistro60I.GetBaseICMS: Currency;
begin
  Result := fBaseICMS;
end;

procedure TRegistro60I.SetBaseICMS(const Valor: Currency);
begin
  fBaseICMS := Valor;
end;

function TRegistro60I.GetSitTributaria: string;
begin
  Result := fSitTributaria;
end;

procedure TRegistro60I.SetSitTributaria(const Valor: string);
begin
  fSitTributaria := Valor;
end;

function TRegistro60I.GetValorICMS: Currency;
var
  aAliquota: Currency;
begin
  aAliquota := StrToNumero(fSitTributaria, 2);
  Result := fBaseICMS * (aAliquota / 100);
end;

{ TRegistro60ILista }

function TRegistro60ILista.New: IRegistro60I;
begin
  Result := TRegistro60I.Create;
  Add(Result);
end;

function TRegistro60ILista.GetItem(Index: Integer): IRegistro60I;
begin
  Result := Get(Index) as IRegistro60I;
end;

procedure TRegistro60ILista.SetItem(Index: Integer; const Value: IRegistro60I);
begin
  Put(Index, Value);
end;

{ TRegistro60R }

function TRegistro60R.GetMes: SmallInt;
begin
  Result := fMes;
end;

procedure TRegistro60R.SetMes(const Valor: SmallInt);
begin
  fMes := Valor;
end;

function TRegistro60R.GetAno: SmallInt;
begin
  Result := fANo;
end;

procedure TRegistro60R.SetAno(const Valor: SmallInt);
begin
  fAno := Valor;
end;

function TRegistro60R.GetCodProduto: string;
begin
  Result := fCodProduto;
end;

procedure TRegistro60R.SetCodProduto(const Valor: string);
begin
  fCodProduto := Valor;
end;

function TRegistro60R.GetQuantidade: Extended;
begin
  Result := fQuantidade;
end;

procedure TRegistro60R.SetQuantidade(const Valor: Extended);
begin
  fQuantidade := Valor;
end;

function TRegistro60R.GetValorAcumProduto: Currency;
begin
  Result := fValorAcumProduto;
end;

procedure TRegistro60R.SetValorAcumProduto(const Valor: Currency);
begin
  fValorAcumProduto := Valor;
end;

function TRegistro60R.GetValorAcumICMS: Currency;
begin
  Result := fValorAcumICMS;
end;

procedure TRegistro60R.SetValorAcumICMS(const Valor: Currency);
begin
  fValorAcumICMS := Valor;
end;

function TRegistro60R.GetSitTributaria: string;
begin
  Result := fSitTributaria;
end;

procedure TRegistro60R.SetSitTributaria(const Valor: string);
begin
  fSitTributaria := Valor;
end;

{ TRegistro60RLista }

function TRegistro60RLista.New: IRegistro60R;
begin
  Result := TRegistro60R.Create;
  Add(Result);
end;

function TRegistro60RLista.GetItem(Index: Integer): IRegistro60R;
begin
  Result := Get(Index) as IRegistro60R;
end;

procedure TRegistro60RLista.SetItem(Index: Integer; const Value: IRegistro60R);
begin
  Put(Index, Value);
end;

{ TRegistro61 }

constructor TRegistro61.Create;
begin
  inherited;
  fValorTotal := 0;
  fBaseICMS := 0;
  fIsentasNTrib := 0;
  fOutras := 0;
  fAliquota := 0;
end;

function TRegistro61.GetDataEmissao: TDateTime;
begin
  Result := fDataEmissao;
end;

procedure TRegistro61.SetDataEmissao(const Valor: TDateTime);
begin
  fDataEmissao := Valor;
end;

function TRegistro61.GetModeloDoc: Integer;
begin
  Result := fModeloDoc;
end;

procedure TRegistro61.SetModeloDoc(const Valor: Integer);
begin
  fModeloDoc := Valor;
end;

function TRegistro61.GetSerie: string;
begin
  Result := fSerie;
end;

procedure TRegistro61.SetSerie(const Valor: string);
begin
  fSerie := UpperCase(Valor);
end;

function TRegistro61.GetSubSerie: string;
begin
  Result := fSubSerie;
end;

procedure TRegistro61.SetSubSerie(const Valor: string);
begin
  fSubSerie := UpperCase(Valor);
end;

function TRegistro61.GetNumInicial: Integer;
begin
  Result := fNumInicial;
end;

procedure TRegistro61.SetNumInicial(const Valor: Integer);
begin
  fNumInicial := Valor;
end;

function TRegistro61.GetNumFinal: Integer;
begin
  Result := fNumFinal;
end;

procedure TRegistro61.SetNumFinal(const Valor: Integer);
begin
  fNumFinal := Valor;
end;

function TRegistro61.GetValorTotal: Currency;
begin
  Result := fValorTotal;
end;

procedure TRegistro61.SetValorTotal(const Valor: Currency);
begin
  fValorTotal := Valor;
end;

function TRegistro61.GetBaseICMS: Currency;
begin
  Result := fBaseICMS;
end;

procedure TRegistro61.SetBaseICMS(const Valor: Currency);
begin
  fBaseICMS := Valor;
end;

function TRegistro61.GetIsentasNTrib: Currency;
begin
  Result := fIsentasNTrib;
end;

procedure TRegistro61.SetIsentasNTrib(const Valor: Currency);
begin
  fIsentasNTrib := Valor;
end;

function TRegistro61.GetOutras: Currency;
begin
  Result := fOutras;
end;

procedure TRegistro61.SetOutras(const Valor: Currency);
begin
  fOutras := Valor;
end;

function TRegistro61.GetAliquota: Currency;
begin
  Result := fAliquota;
end;

procedure TRegistro61.SetAliquota(const Valor: Currency);
begin
  fAliquota := Valor;
end;

function TRegistro61.GetValorICMS: Currency;
begin
  Result := fBaseICMS * (fAliquota / 100);
end;

{ TRegistro61Lista }

function TRegistro61Lista.New: IRegistro61;
begin
  Result := TRegistro61.Create;
  Add(Result);
end;

function TRegistro61Lista.GetItem(Index: Integer): IRegistro61;
begin
  Result := Get(Index) as IRegistro61;
end;

procedure TRegistro61Lista.SetItem(Index: Integer; const Value: IRegistro61);
begin
  Put(Index, Value);
end;

{ TRegistro61R }

function TRegistro61R.GetAliquota: Currency;
begin
  Result := fAliquota;
end;

function TRegistro61R.GetMes: SmallInt;
begin
  Result := fMes;
end;

function TRegistro61R.GetAno: SmallInt;
begin
  Result := fAno;
end;

function TRegistro61R.GetBaseCalcICMS: Currency;
begin
  Result := fBaseCalcICMS;
end;

function TRegistro61R.GetCodProduto: string;
begin
  Result := fCodProduto;
end;

function TRegistro61R.GetQuantidade: Extended;
begin
  Result := fQuantidade;
end;

function TRegistro61R.GetValorBrutoProduto: Currency;
begin
  Result := fValorBrutoProduto;
end;

procedure TRegistro61R.SetAliquota(const Valor: Currency);
begin
  fAliquota := valor;
end;

procedure TRegistro61R.SetBaseCalcICMS(const Valor: Currency);
begin
  fBaseCalcICMS := Valor;
end;

procedure TRegistro61R.SetCodProduto(const Valor: string);
begin
  fCodProduto := Valor;
end;

procedure TRegistro61R.SetMes(const Valor: SmallInt);
begin
  fMes := Valor;
end;

procedure TRegistro61R.SetAno(const Valor: SmallInt);
begin
  fAno := valor;
end;

procedure TRegistro61R.SetQuantidade(const Valor: Extended);
begin
  fQuantidade := Valor;
end;

procedure TRegistro61R.SetValorBrutoProduto(const Valor: Currency);
begin
  fValorBrutoProduto := Valor;
end;

{ TRegistro61RLista }

function TRegistro61RLista.New: IRegistro61R;
begin
  Result := TRegistro61R.Create;
  Add(Result);
end;

function TRegistro61RLista.GetItem(Index: Integer): IRegistro61R;
begin
  Result := Get(Index) as IRegistro61R;
end;

procedure TRegistro61RLista.SetItem(Index: Integer; const Value: IRegistro61R);
begin
  Put(Index, Value);
end;

{ TRegistro75 }

function TRegistro75.GetValidadeInicial: TDateTime;
begin
  Result := fValidadeInicial;
end;

procedure TRegistro75.SetValidadeInicial(const Valor: TDateTime);
begin
  fValidadeInicial := Valor;
end;

function TRegistro75.GetValidadeFinal: TDateTime;
begin
  Result := fValidadeFinal;
end;

procedure TRegistro75.SetValidadeFinal(const Valor: TDateTime);
begin
  fValidadeFinal := Valor;
end;

function TRegistro75.GetCodProduto: string;
begin
  Result := fCodProduto;
end;

procedure TRegistro75.SetCodProduto(const Valor: string);
begin
  fCodProduto := Valor;
end;

function TRegistro75.GetCodNCM: string;
begin
  Result := fCodNCM;
end;

procedure TRegistro75.SetCodNCM(const Valor: string);
begin
  fCodNCM := Valor;
end;

function TRegistro75.GetDescricao: string;
begin
  Result := fDescricao;
end;

procedure TRegistro75.SetDescricao(const Valor: string);
begin
  fDescricao := Valor;
end;

function TRegistro75.GetUnidade: string;
begin
  Result := fUnidade;
end;

procedure TRegistro75.SetUnidade(const Valor: string);
begin
  fUnidade := Valor;
end;

function TRegistro75.GetAliquotaIPI: Currency;
begin
  Result := fAliquotaIPI;
end;

procedure TRegistro75.SetAliquotaIPI(const Valor: Currency);
begin
  fAliquotaIPI := Valor;
end;

function TRegistro75.GetAliquotaICMS: Currency;
begin
  Result := fAliquotaICMS;
end;

procedure TRegistro75.SetAliquotaICMS(const Valor: Currency);
begin
  fAliquotaICMS := Valor;
end;

function TRegistro75.GetReducaoBaseCalc: Currency;
begin
  Result := fReducaoBaseCalc;
end;

procedure TRegistro75.SetReducaoBaseCalc(const Valor: Currency);
begin
  fReducaoBaseCalc := Valor;
end;

function TRegistro75.GetBaseICMSST: Currency;
begin
  Result := fBaseICMSST;
end;

procedure TRegistro75.SetBaseICMSST(const Valor: Currency);
begin
  fBaseICMSST := Valor;
end;

{ TRegistro75Lista }

function TRegistro75Lista.New: IRegistro75;
begin
  Result := TRegistro75.Create;
  Add(Result);
end;

function TRegistro75Lista.ExisteProduto(
  const AProduto: IRegistro75): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Count - 1 do
  begin
    if AProduto.CodProduto = Items[I].CodProduto then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TRegistro75Lista.GetItem(Index: Integer): IRegistro75;
begin
  Result := Get(Index) as IRegistro75;
end;

procedure TRegistro75Lista.SetItem(Index: Integer; const Value: IRegistro75);
begin
  Put(Index, Value);
end;

{ TRegistro74Lista }

function TRegistro74Lista.New: IRegistro74;
begin
  Result := TRegistro74.Create;
  Add(Result);
end;

function TRegistro74Lista.GetItem(Index: Integer): IRegistro74;
begin
  Result := Get(Index) as IRegistro74;
end;

procedure TRegistro74Lista.SetItem(Index: Integer; const Value: IRegistro74);
begin
  Put(Index, Value);
end;

{ TRegistro74 }

function TRegistro74.GetCNPJ: string;
begin
  Result := fCNPJ;
end;

function TRegistro74.GetCodPRoduto: string;
begin
  Result := fCodPRoduto;
end;

function TRegistro74.GetDataInventario: TDateTime;
begin
  Result := fDataInventario;
end;

function TRegistro74.GetInscEstadual: string;
begin
  Result := fInscEstadual;
end;

function TRegistro74.GetQuantidade: Extended;
begin
  Result := fQuantidade;
end;

function TRegistro74.GetTipoPosse: TTipoPosse;
begin
  Result := fTipoPosse;
end;

function TRegistro74.GetUF: string;
begin
  Result := fUF;
end;

function TRegistro74.GetValorTotal: Currency;
begin
  Result := fValorTotal;
end;

procedure TRegistro74.SetCNPJ(const Valor: string);
begin
  if fTipoPosse = tpo1 then
    fCNPJ := '0'
  else
    fCNPJ := RetornaSoNumero(Valor);
end;

procedure TRegistro74.SetCodPRoduto(const Valor: string);
begin
  fCodProduto := Valor;
end;

procedure TRegistro74.SetDataInventario(const Valor: TDateTime);
begin
  fDataInventario := Valor;
end;

procedure TRegistro74.SetInscEstadual(const Valor: string);
begin
  fInscEstadual := RetiraMascara(Valor);
end;

procedure TRegistro74.SetQuantidade(const Valor: Extended);
begin
  fQuantidade := Valor;
end;

procedure TRegistro74.SetTipoPosse(const Valor: TTipoPosse);
begin
  fTipoPosse := Valor;
end;

procedure TRegistro74.SetUF(const Valor: string);
begin
  fUF := Valor;
end;

procedure TRegistro74.SetValorTotal(const Valor: Currency);
begin
  fValorTotal := Valor;
end;

{ Factoring da Interface ISintegra }
function CreateSintegra(): ISintegra;
begin
  Result := TSintegra.Create;
end;

{ Funçoes Utilitárias }

{ Verifica se o CFOP e válido }
function VerificaCFOP(aCFOP: SmallInt): Boolean;
const
  ListaCFOP: array[0..522] of string = (
    '1101', '1102', '1111', '1113', '1116', '1117', '1118', '1120', '1121', '1122', '1124', '1125', '1126', '1151', '1152', '1153', '1154', '1201',
    '1202', '1203', '1204', '1205', '1206', '1207', '1208', '1209', '1251', '1252', '1253', '1254', '1255', '1256', '1257', '1301', '1302', '1303',
    '1304', '1305', '1306', '1351', '1352', '1353', '1354', '1355', '1356', '1401', '1403', '1406', '1407', '1408', '1409', '1410', '1411', '1414',
    '1415', '1451', '1452', '1501', '1503', '1504', '1551', '1552', '1553', '1554', '1555', '1556', '1557', '1601', '1602', '1603', '1604', '1650',
    '1651', '1652', '1653', '1658', '1659', '1660', '1661', '1662', '1663', '1664', '1901', '1902', '1903', '1904', '1905', '1906', '1907', '1908',
    '1909', '1910', '1911', '1912', '1913', '1914', '1915', '1916', '1917', '1918', '1919', '1920', '1921', '1922', '1923', '1924', '1925', '1926',
    '1949', '2101', '2102', '2111', '2113', '2116', '2117', '2118', '2120', '2121', '2122', '2124', '2125', '2126', '2151', '2152', '2153', '2154',
    '2201', '2202', '2203', '2204', '2205', '2206', '2207', '2208', '2209', '2251', '2252', '2253', '2254', '2255', '2256', '2257', '2301', '2302',
    '2303', '2304', '2305', '2306', '2351', '2352', '2353', '2354', '2355', '2356', '2401', '2403', '2406', '2407', '2408', '2409', '2410', '2411',
    '2414', '2415', '2501', '2503', '2504', '2551', '2552', '2553', '2554', '2555', '2556', '2557', '2603', '2651', '2652', '2653', '2658', '2659',
    '2660', '2661', '2662', '2663', '2664', '2901', '2902', '2903', '2904', '2905', '2906', '2907', '2908', '2909', '2910', '2911', '2912', '2913',
    '2914', '2915', '2916', '2917', '2918', '2919', '2920', '2921', '2922', '2923', '2924', '2925', '2949', '3101', '3102', '3126', '3127', '3201',
    '3202', '3205', '3206', '3207', '3211', '3251', '3301', '3351', '3352', '3353', '3354', '3355', '3356', '3503', '3551', '3553', '3556', '3650',
    '3651', '3652', '3653', '3930', '3949', '5101', '5102', '5103', '5104', '5105', '5106', '5109', '5110', '5111', '5112', '5113', '5114', '5115',
    '5116', '5117', '5118', '5119', '5120', '5122', '5123', '5124', '5125', '5151', '5152', '5153', '5155', '5156', '5201', '5202', '5205', '5206',
    '5207', '5208', '5209', '5210', '5251', '5252', '5253', '5254', '5255', '5256', '5257', '5258', '5301', '5302', '5303', '5304', '5305', '5306',
    '5307', '5351', '5352', '5353', '5354', '5355', '5356', '5357', '5401', '5402', '5403', '5405', '5408', '5409', '5410', '5411', '5412', '5413',
    '5414', '5415', '5451', '5501', '5502', '5503', '5551', '5552', '5553', '5554', '5555', '5556', '5557', '5601', '5602', '5603', '5650', '5651',
    '5652', '5653', '5654', '5655', '5656', '5657', '5658', '5659', '5660', '5661', '5662', '5663', '5664', '5665', '5666', '5901', '5902', '5903',
    '5904', '5905', '5906', '5907', '5908', '5909', '5910', '5911', '5912', '5913', '5914', '5915', '5916', '5917', '5918', '5919', '5920', '5921',
    '5922', '5923', '5924', '5925', '5926', '5927', '5928', '5929', '5931', '5932', '5949', '6101', '6102', '6103', '6104', '6105', '6106', '6107',
    '6108', '6109', '6110', '6111', '6112', '6113', '6114', '6115', '6116', '6117', '6118', '6119', '6120', '6122', '6123', '6124', '6125', '6151',
    '6152', '6153', '6155', '6156', '6201', '6202', '6205', '6206', '6207', '6208', '6209', '6210', '6251', '6252', '6253', '6254', '6255', '6256',
    '6257', '6258', '6301', '6302', '6303', '6304', '6305', '6306', '6307', '6351', '6352', '6353', '6354', '6355', '6356', '6357', '6401', '6402',
    '6403', '6404', '6408', '6409', '6410', '6411', '6412', '6413', '6414', '6415', '6501', '6502', '6503', '6551', '6552', '6553', '6554', '6555',
    '6556', '6557', '6603', '6650', '6651', '6652', '6653', '6654', '6655', '6656', '6657', '6658', '6659', '6660', '6661', '6662', '6663', '6664',
    '6665', '6666', '6901', '6902', '6903', '6904', '6905', '6906', '6907', '6908', '6909', '6910', '6911', '6912', '6913', '6914', '6915', '6916',
    '6917', '6918', '6919', '6920', '6921', '6922', '6923', '6924', '6925', '6929', '6931', '6932', '6949', '7101', '7102', '7105', '7106', '7127',
    '7201', '7202', '7205', '7206', '7207', '7210', '7211', '7251', '7301', '7358', '7501', '7551', '7553', '7556', '7650', '7651', '7654', '7930',
    '7949');
var
  i: Integer;
  Encontrado: Boolean;
begin
  i := 0;
  Encontrado := False;
  while (not (Encontrado)) and (i <= 522) do
  begin
    Encontrado := ListaCFOP[i] = IntToStr(aCFOP);
    inc(i);
  end;

  Result := Encontrado;
end;

{ Verifica se a UF digitada e valida }
function VerificaUF(aUF: string): Boolean;
const
  ListaUF: array[0..27] of string = (
    'AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF',
    'ES', 'GO', 'MA', 'MG', 'MS', 'MT', 'PA',
    'PB', 'PE', 'PI', 'PR', 'RJ', 'RN', 'RO',
    'RR', 'RS', 'SC', 'SE', 'SP', 'TO', 'EX');
var
  i: integer;
  Encontrado: Boolean;
begin
  i := 0;
  Encontrado := False;
  while (not (Encontrado)) and (i <= 27) do
  begin
    Encontrado := ListaUF[i] = aUF;
    inc(i);
  end;

  Result := Encontrado;
end;

{ Verifica se o CEP e valido }
function VerificaCEP(cCep: string; cEstado: string): Boolean;
var
  cCEP1: Integer;
begin
  if cCEP = '' then
  begin
    Result := False;
    Exit;
  end;

  cCEP1 := StrToInt(copy(cCep, 1, 3));

  if Length(trim(cCep)) > 0 then
  begin
    if Length(trim(copy(cCep, 6, 3))) < 3 then
      Result := False
    else if (cEstado = 'SP') and (cCEP1 >= 10) and (cCEP1 <= 199) then
      Result := True
    else if (cEstado = 'RJ') and (cCEP1 >= 200) and (cCEP1 <= 289) then
      Result := True
    else if (cEstado = 'ES') and (cCEP1 >= 290) and (cCEP1 <= 299) then
      Result := True
    else if (cEstado = 'MG') and (cCEP1 >= 300) and (cCEP1 <= 399) then
      Result := True
    else if (cEstado = 'BA') and (cCEP1 >= 400) and (cCEP1 <= 489) then
      Result := True
    else if (cEstado = 'SE') and (cCEP1 >= 490) and (cCEP1 <= 499) then
      Result := True
    else if (cEstado = 'PE') and (cCEP1 >= 500) and (cCEP1 <= 569) then
      Result := True
    else if (cEstado = 'AL') and (cCEP1 >= 570) and (cCEP1 <= 579) then
      Result := True
    else if (cEstado = 'PB') and (cCEP1 >= 580) and (cCEP1 <= 589) then
      Result := True
    else if (cEstado = 'RN') and (cCEP1 >= 590) and (cCEP1 <= 599) then
      Result := True
    else if (cEstado = 'CE') and (cCEP1 >= 600) and (cCEP1 <= 639) then
      Result := True
    else if (cEstado = 'PI') and (cCEP1 >= 640) and (cCEP1 <= 649) then
      Result := True
    else if (cEstado = 'MA') and (cCEP1 >= 650) and (cCEP1 <= 659) then
      Result := True
    else if (cEstado = 'PA') and (cCEP1 >= 660) and (cCEP1 <= 688) then
      Result := True
    else if (cEstado = 'AM') and ((cCEP1 >= 690) and (cCEP1 <= 692) or (cCEP1 >= 694) and (cCEP1 <= 698)) then
      Result := True
    else if (cEstado = 'AP') and (cCEP1 = 689) then
      Result := True
    else if (cEstado = 'RR') and (cCEP1 = 693) then
      Result := True
    else if (cEstado = 'AC') and (cCEP1 = 699) then
      Result := True
    else if ((cEstado = 'DF') or (cEstado = 'GO')) and (cCEP1 >= 700) and (cCEP1 <= 769) then
      Result := True
    else if (cEstado = 'TO') and (cCEP1 >= 770) and (cCEP1 <= 779) then
      Result := True
    else if (cEstado = 'MT') and (cCEP1 >= 780) and (cCEP1 <= 788) then
      Result := True
    else if (cEstado = 'MS') and (cCEP1 >= 790) and (cCEP1 <= 799) then
      Result := True
    else if (cEstado = 'RO') and (cCEP1 = 789) then
      Result := True
    else if (cEstado = 'PR') and (cCEP1 >= 800) and (cCEP1 <= 879) then
      Result := True
    else if (cEstado = 'SC') and (cCEP1 >= 880) and (cCEP1 <= 899) then
      Result := True
    else if (cEstado = 'RS') and (cCEP1 >= 900) and (cCEP1 <= 999) then
      Result := True
    else
      Result := False
  end
  else
    Result := True;
end;

{ Valida a inscrição estadual }
function VerificaInscEstadual(aInscricao, aTipo: string): Boolean;
var
  Contador: ShortInt;
  Casos: ShortInt;
  Digitos: ShortInt;

  Tabela_1: string;
  Tabela_2: string;
  Tabela_3: string;

  Base_1: string;
  Base_2: string;
  Base_3: string;

  Valor_1: ShortInt;

  Soma_1: Integer;
  Soma_2: Integer;

  Erro_1: ShortInt;
  Erro_2: ShortInt;
  Erro_3: ShortInt;

  Posicao_1: string;
  Posicao_2: string;

  Tabela: string;
  Rotina: string;
  Modulo: ShortInt;
  Peso: string;

  Digito: ShortInt;

  Resultado: string;
  Retorno: Boolean;
begin

  { Isento ja e aceito }
  if (aInscricao = 'ISENTO') or (Trim(aInscricao) = '') then
  begin
    Result := True;
    Exit;
  end;

  { Inscrição de produtor rural, não validar }
  if (Copy(aInscricao, 1, 2) = 'PR') then
  begin
    Result := True;
    Exit;
  end;

  try
    Tabela_1 := ' ';
    Tabela_2 := ' ';
    Tabela_3 := ' ';

    {                                                                               }
    {                                                                               }
    {         Valores possiveis para os digitos (j)                                 }
    {                                                                               }
    { 0 a 9 = Somente o digito indicado.                                            }
    {     N = Numeros 0 1 2 3 4 5 6 7 8 ou 9                                        }
    {     A = Numeros 1 2 3 4 5 6 7 8 ou 9                                          }
    {     B = Numeros 0 3 5 7 ou 8                                                  }
    {     C = Numeros 4 ou 7                                                        }
    {     D = Numeros 3 ou 4                                                        }
    {     E = Numeros 0 ou 8                                                        }
    {     F = Numeros 0 1 ou 5                                                      }
    {     G = Numeros 1 7 8 ou 9                                                    }
    {     H = Numeros 0 1 2 ou 3                                                    }
    {     I = Numeros 0 1 2 3 ou 4                                                  }
    {     J = Numeros 0 ou 9                                                        }
    {     K = Numeros 1 2 3 ou 9                                                    }
    {                                                                               }
    { ----------------------------------------------------------------------------- }
    {                                                                               }
    {         Valores possiveis para as rotinas (d) e (g)                           }
    {                                                                               }
    { A a E = Somente a Letra indicada.                                             }
    {     0 = B e D                                                                 }
    {     1 = C e E                                                                 }
    {     2 = A e E                                                                 }
    {                                                                               }
    { ----------------------------------------------------------------------------- }
    {                                                                               }
    {                                  C T  F R M  P  R M  P                        }
    {                                  A A  A O O  E  O O  E                        }
    {                                  S M  T T D  S  T D  S                        }
    {                                                                               }
    {                                  a b  c d e  f  g h  i  jjjjjjjjjjjjjj        }
    {                                  0000000001111111111222222222233333333        }
    {                                  1234567890123456789012345678901234567        }

    IF aTipo = 'AC'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     01NNNNNNX.14.00';
    IF aTipo = 'AC'   Then Tabela_2 := '2.13.0.E.11.02.E.11.01. 01NNNNNNNNNXY.13.14';
    IF aTipo = 'AL'   Then Tabela_1 := '1.09.0.0.11.01. .  .  .     24BNNNNNX.14.00';
    IF aTipo = 'AP'   Then Tabela_1 := '1.09.0.1.11.01. .  .  .     03NNNNNNX.14.00';
    IF aTipo = 'AP'   Then Tabela_2 := '2.09.1.1.11.01. .  .  .     03NNNNNNX.14.00';
    IF aTipo = 'AP'   Then Tabela_3 := '3.09.0.E.11.01. .  .  .     03NNNNNNX.14.00';
    IF aTipo = 'AM'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     0CNNNNNNX.14.00';
    IF aTipo = 'BA'   Then Tabela_1 := '1.08.0.E.10.02.E.10.03.      NNNNNNYX.14.13';
    IF aTipo = 'BA'   Then Tabela_2 := '2.08.0.E.11.02.E.11.03.      NNNNNNYX.14.13';
    IF aTipo = 'CE'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     0NNNNNNNX.14.13';
    IF aTipo = 'DF'   Then Tabela_1 := '1.13.0.E.11.02.E.11.01. 07DNNNNNNNNXY.13.14';
    IF aTipo = 'ES'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     0ENNNNNNX.14.00';
    IF aTipo = 'GO'   Then Tabela_1 := '1.09.1.E.11.01. .  .  .     1FNNNNNNX.14.00';
    IF aTipo = 'GO'   Then Tabela_2 := '2.09.0.E.11.01. .  .  .     1FNNNNNNX.14.00';
    IF aTipo = 'MA'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     12NNNNNNX.14.00';
    IF aTipo = 'MT'   Then Tabela_1 := '1.11.0.E.11.01. .  .  .   NNNNNNNNNNX.14.00';
    IF aTipo = 'MS'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     28NNNNNNX.14.00';
    IF aTipo = 'MG'   Then Tabela_1 := '1.13.0.2.10.10.E.11.11. NNNNNNNNNNNXY.13.14';
    IF aTipo = 'PA'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     15NNNNNNX.14.00';
    IF aTipo = 'PB'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     16NNNNNNX.14.00';
    IF aTipo = 'PR'   Then Tabela_1 := '1.10.0.E.11.09.E.11.08.    NNNNNNNNXY.13.14';
    IF aTipo = 'PE'   Then Tabela_1 := '1.14.1.E.11.07. .  .  .18ANNNNNNNNNNX.14.00';
    IF aTipo = 'PI'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     19NNNNNNX.14.00';
    IF aTipo = 'RJ'   Then Tabela_1 := '1.08.0.E.11.08. .  .  .      GNNNNNNX.14.00';
    IF aTipo = 'RN'   Then Tabela_1 := '1.09.0.0.11.01. .  .  .     20HNNNNNX.14.00';
    IF aTipo = 'RS'   Then Tabela_1 := '1.10.0.E.11.01. .  .  .    INNNNNNNNX.14.00';
    IF aTipo = 'RO'   Then Tabela_1 := '1.09.1.E.11.04. .  .  .     ANNNNNNNX.14.00';
    IF aTipo = 'RO'   Then Tabela_2 := '2.14.0.E.11.01. .  .  .NNNNNNNNNNNNNX.14.00';
    IF aTipo = 'RR'   Then Tabela_1 := '1.09.0.D.09.05. .  .  .     24NNNNNNX.14.00';
    IF aTipo = 'SC'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     NNNNNNNNX.14.00';
    IF aTipo = 'SP'   Then Tabela_1 := '1.12.0.D.11.12.D.11.13.  NNNNNNNNXNNY.11.14';
    IF aTipo = 'SP'   Then Tabela_2 := '2.12.0.D.11.12. .  .  .  NNNNNNNNXNNN.11.00';
    IF aTipo = 'SE'   Then Tabela_1 := '1.09.0.E.11.01. .  .  .     NNNNNNNNX.14.00';
    IF aTipo = 'TO'   Then Tabela_1 := '1.11.0.E.11.06. .  .  .   29JKNNNNNNX.14.00';
    IF aTipo = 'CNPJ' Then Tabela_1 := '1.14.0.E.11.21.E.11.22.NNNNNNNNNNNNXY.13.14';
    IF aTipo = 'CPF'  Then Tabela_1 := '1.11.0.E.11.31.E.11.32.   NNNNNNNNNXY.13.14';

    { Deixa somente os numeros }
    Base_1 := '';

    for Contador := 1 to 30 do
    begin
      if Pos(Copy(aInscricao, Contador, 1), '0123456789') <> 0 then
        Base_1 := Base_1 + Copy(aInscricao, Contador, 1);
    end;

    { Repete 3x - 1 para cada caso possivel }
    Casos := 0;

    Erro_1 := 0;
    Erro_2 := 0;
    Erro_3 := 0;

    while Casos < 3 do
    begin

      Casos := Casos + 1;

      IF Casos = 1 Then Tabela := Tabela_1;
      IF Casos = 2 Then Erro_1 := Erro_3  ;
      IF Casos = 2 Then Tabela := Tabela_2;
      IF Casos = 3 Then Erro_2 := Erro_3  ;
      IF Casos = 3 Then Tabela := Tabela_3;

      Erro_3 := 0;

      if Copy(Tabela, 1, 1) <> ' ' then
      begin

        { Verifica o Tamanho }
        if Length(Trim(Base_1)) <> (StrToInt(Copy(Tabela, 3, 2))) then
          Erro_3 := 1;

        if Erro_3 = 0 then
        begin

          { Ajusta o Tamanho }
          Base_2 := Copy('              ' + Base_1, Length('              ' + Base_1) - 13, 14);

          { Compara com valores possivel para cada uma da 14 posições }
          Contador := 0;

          while (Contador < 14) and (Erro_3 = 0) do
          begin

            Contador := Contador + 1;

            Posicao_1 := Copy(Copy(Tabela, 24, 14), Contador, 1);
            Posicao_2 := Copy(Base_2, Contador, 1);

            IF ( Posicao_1  = ' '        ) AND (      Posicao_2                 <> ' ' ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'N'        ) AND ( Pos( Posicao_2, '0123456789' )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'A'        ) AND ( Pos( Posicao_2, '123456789'  )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'B'        ) AND ( Pos( Posicao_2, '03578'      )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'C'        ) AND ( Pos( Posicao_2, '47'         )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'D'        ) AND ( Pos( Posicao_2, '34'         )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'E'        ) AND ( Pos( Posicao_2, '08'         )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'F'        ) AND ( Pos( Posicao_2, '015'        )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'G'        ) AND ( Pos( Posicao_2, '1789'       )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'H'        ) AND ( Pos( Posicao_2, '0123'       )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'I'        ) AND ( Pos( Posicao_2, '01234'      )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'J'        ) AND ( Pos( Posicao_2, '09'         )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1  = 'K'        ) AND ( Pos( Posicao_2, '1239'       )  =   0 ) Then Erro_3 := 1;
            IF ( Posicao_1 <>  Posicao_2 ) AND ( Pos( Posicao_1, '0123456789' )  >   0 ) Then Erro_3 := 1;

          end;

          { Calcula os Digitos }
          Rotina := ' ';
          Digitos := 000;
          Digito := 000;

          while (Digitos < 2) and (Erro_3 = 0) do
          begin

            Digitos := Digitos + 1;

            { Carrega peso }
            Peso := Copy(Tabela, 5 + (Digitos * 8), 2);

            if Peso <> '  ' then
            begin

              Rotina := Copy(Tabela, 0 + (Digitos * 8), 1);
              Modulo := StrToInt(Copy(Tabela, 2 + (Digitos * 8), 2));

              IF Peso = '01' Then Peso := '06.05.04.03.02.09.08.07.06.05.04.03.02.00';
              IF Peso = '02' Then Peso := '05.04.03.02.09.08.07.06.05.04.03.02.00.00';
              IF Peso = '03' Then Peso := '06.05.04.03.02.09.08.07.06.05.04.03.00.02';
              IF Peso = '04' Then Peso := '00.00.00.00.00.00.00.00.06.05.04.03.02.00';
              IF Peso = '05' Then Peso := '00.00.00.00.00.01.02.03.04.05.06.07.08.00';
              IF Peso = '06' Then Peso := '00.00.00.09.08.00.00.07.06.05.04.03.02.00';
              IF Peso = '07' Then Peso := '05.04.03.02.01.09.08.07.06.05.04.03.02.00';
              IF Peso = '08' Then Peso := '08.07.06.05.04.03.02.07.06.05.04.03.02.00';
              IF Peso = '09' Then Peso := '07.06.05.04.03.02.07.06.05.04.03.02.00.00';
              IF Peso = '10' Then Peso := '00.01.02.01.01.02.01.02.01.02.01.02.00.00';
              IF Peso = '11' Then Peso := '00.03.02.11.10.09.08.07.06.05.04.03.02.00';
              IF Peso = '12' Then Peso := '00.00.01.03.04.05.06.07.08.10.00.00.00.00';
              IF Peso = '13' Then Peso := '00.00.03.02.10.09.08.07.06.05.04.03.02.00';
              IF Peso = '21' Then Peso := '05.04.03.02.09.08.07.06.05.04.03.02.00.00';
              IF Peso = '22' Then Peso := '06.05.04.03.02.09.08.07.06.05.04.03.02.00';
              IF Peso = '31' Then Peso := '00.00.00.10.09.08.07.06.05.04.03.02.00.00';
              IF Peso = '32' Then Peso := '00.00.00.11.10.09.08.07.06.05.04.03.02.00';

              { Multiplica }
              Base_3 := Copy(('0000000000000000' + Trim(Base_2)), Length(('0000000000000000' + Trim(Base_2))) - 13, 14);

              Soma_1 := 0;
              Soma_2 := 0;

              for Contador := 1 to 14 do
              begin

                Valor_1 := (StrToInt(Copy(Base_3, Contador, 01)) * StrToInt(Copy(Peso, Contador * 3 - 2, 2)));

                Soma_1 := Soma_1 + Valor_1;

                if Valor_1 > 9 then
                  Valor_1 := Valor_1 - 9;

                Soma_2 := Soma_2 + Valor_1;

              end;

              { Ajusta valor da soma }
              IF Pos( Rotina, 'A2'  ) > 0 Then Soma_1 := Soma_2;
              IF Pos( Rotina, 'B0'  ) > 0 Then Soma_1 := Soma_1 * 10;
              IF Pos( Rotina, 'C1'  ) > 0 Then Soma_1 := Soma_1 + ( 5 + 4 * StrToInt( Copy( Tabela, 6, 1 ) ) );

              { Calcula o Digito }
              IF Pos( Rotina, 'D0'  ) > 0 Then Digito := Soma_1 Mod Modulo;
              IF Pos( Rotina, 'E12' ) > 0 Then Digito := Modulo - ( Soma_1 Mod Modulo);

              IF Digito < 10 Then Resultado := IntToStr( Digito );
              IF Digito = 10 Then Resultado := '0';
              IF Digito = 11 Then Resultado := Copy( Tabela, 6, 1 );

              { Verifica o Digito }
              if (Copy(Base_2, StrToInt(Copy(Tabela, 36 + (Digitos * 3), 2)), 1) <> Resultado) then
                Erro_3 := 1;
            end;
          end;
        end;
      end;
    end;

    { Retorna o resultado da Verificação }
    Retorno := FALSE;

    if (Trim(Tabela_1) <> '') and (ERRO_1 = 0) then Retorno := TRUE;
    if (Trim(Tabela_2) <> '') and (ERRO_2 = 0) then Retorno := TRUE;
    if (Trim(Tabela_3) <> '') and (ERRO_3 = 0) then Retorno := TRUE;

    if Trim(aInscricao) = 'ISENTO' then Retorno := TRUE;

    Result := Retorno;
  except
    Result := False;
  end;
end;

{ Verifica se o CPF/CNPJ e Valido }
function VerificaCPF_CNPJ(numero: string): Boolean;
var
  n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12: integer;
  d1, d2: integer;
  digitado, calculado: string;

begin
  Result := false;

  if Length(Numero) = 11 then
  begin
    n1 := StrToInt(numero[1]);
    n2 := StrToInt(numero[2]);
    n3 := StrToInt(numero[3]);
    n4 := StrToInt(numero[4]);
    n5 := StrToInt(numero[5]);
    n6 := StrToInt(numero[6]);
    n7 := StrToInt(numero[7]);
    n8 := StrToInt(numero[8]);
    n9 := StrToInt(numero[9]);

    d1 := n9 * 2 + n8 * 3 + n7 * 4 + n6 * 5 + n5 * 6 + n4 * 7 + n3 * 8 + n2 * 9 + n1 * 10;
    d1 := 11 - (d1 mod 11);
      if d1 >= 10 then d1 := 0;

    d2 := d1 * 2 + n9 * 3 + n8 * 4 + n7 * 5 + n6 * 6 + n5 * 7 + n4 * 8 + n3 * 9 + n2 * 10 + n1 * 11;
    d2 := 11 - (d2 mod 11);
      if d2>=10 then d2:=0;

    calculado := inttostr(d1) + inttostr(d2);
    digitado := numero[10] + numero[11];

    if calculado = digitado then Result := true;
  end;

  if Length(Numero) = 14 then
  begin
    n1 := StrToInt(numero[1]);
    n2 := StrToInt(numero[2]);
    n3 := StrToInt(numero[3]);
    n4 := StrToInt(numero[4]);
    n5 := StrToInt(numero[5]);
    n6 := StrToInt(numero[6]);
    n7 := StrToInt(numero[7]);
    n8 := StrToInt(numero[8]);
    n9 := StrToInt(numero[9]);
    n10 := StrToInt(numero[10]);
    n11 := StrToInt(numero[11]);
    n12 := StrToInt(numero[12]);

    d1 := n12 * 2 + n11 * 3 + n10 * 4 + n9 * 5 + n8 * 6 + n7 * 7 + n6 * 8 + n5 * 9 + n4 * 2 + n3 * 3 + n2 * 4 + n1 * 5;
    d1 := 11 - (d1 mod 11);
    if d1 >= 10 then d1 := 0;

    d2 := d1 * 2 + n12 * 3 + n11 * 4 + n10 * 5 + n9 * 6 + n8 * 7 + n7 * 8 + n6 * 9 + n5 * 2 + n4 * 3 + n3 * 4 + n2 * 5 + n1 * 6;
    d2 := 11 - (d2 mod 11);
    if d2 >= 10 then d2 := 0;

    calculado := inttostr(d1) + inttostr(d2);
    digitado := numero[13] + numero[14];

    if calculado = digitado then Result := true;
  end;
end;

{ TRegistro71 }

function TRegistro71.GetCNPJ: string;
begin
  Result := fCNPJ;
end;

function TRegistro71.GetData: TDateTime;
begin
  Result := fData;
end;

function TRegistro71.GetInscEstadual: string;
begin
  Result := fInscEstadual;
end;

function TRegistro71.GetModelo: SmallInt;
begin
  Result := fModelo;
end;

function TRegistro71.GetNumero: Integer;
begin
  Result := fNumero;
end;

function TRegistro71.GetSerie: string;
begin
  Result := fSerie;
end;

function TRegistro71.GetUF: string;
begin
  Result := fUF;
end;

function TRegistro71.GetValorTotal: Currency;
begin
  Result := fValorTotal;
end;

procedure TRegistro71.SetCNPJ(const Valor: string);
begin
  FCNPJ := Valor
end;

procedure TRegistro71.SetData(const Valor: TDateTime);
begin
  FData := Valor
end;

procedure TRegistro71.SetInscEstadual(const Valor: string);
begin
  FInscEstadual := RetiraMascara(Valor)
end;

procedure TRegistro71.SetModelo(const Valor: SmallInt);
begin
  FModelo := Valor
end;

procedure TRegistro71.SetNumero(const Valor: Integer);
begin
  FNumero := Valor;
end;

procedure TRegistro71.SetSerie(const Valor: string);
begin
  FSerie := Valor;
end;

procedure TRegistro71.SetUF(const Valor: string);
begin
  FUF := Valor
end;

procedure TRegistro71.SetValorTotal(const Value: Currency);
begin
  FValorTotal := Value;
end;

{ TRegistro71Lista }

function TRegistro71Lista.New: IRegistro71;
begin
  Result := TRegistro71.Create;
  Add(Result);
end;

function TRegistro71Lista.GetItem(Index: Integer): IRegistro71;
begin
  Result := Get(Index) as IRegistro71;
end;

procedure TRegistro71Lista.SetItem(Index: Integer; const Value: IRegistro71);
begin
  Put(Index, Value);
end;

{ TRegistro85 }

function TRegistro85.GetConhecimento: string;
begin
  Result := fConhecimento;
end;

function TRegistro85.GetDataAverbacao: TDateTime;
begin
  Result := fDataAverbacao;
end;

function TRegistro85.GetDataConhecimento: TDateTime;
begin
  Result := fDataConhecimento;
end;

function TRegistro85.GetDataDeclaracao: TDateTime;
begin
  Result := fDataDeclaracao;
end;

function TRegistro85.GetDataNotaFiscal: TDateTime;
begin
  Result := fDataNotaFiscal;
end;

function TRegistro85.GetDataRegistro: TDateTime;
begin
  Result := fDataRegistro;
end;

function TRegistro85.GetDeclaracao: String;
begin
  Result := fDeclaracao;
end;

function TRegistro85.GetModelo: string;
begin
  Result := fModelo;
end;

function TRegistro85.GetNaturezaExportacao: string;
begin
  Result := fNaturezaExportacao;
end;

function TRegistro85.GetNumeroNotaFiscal: string;
begin
  Result := fNumeroNotaFiscal;
end;

function TRegistro85.GetPais: string;
begin
  Result := fPais;
end;

function TRegistro85.GetRegistroExportacao: String;
begin
  Result := fRegistroExportacao;
end;

function TRegistro85.GetSerie: string;
begin
  Result := fSerie;
end;

function TRegistro85.GetTipoConhecimento: String;
begin
  Result := fTipoConhecimento;
end;

procedure TRegistro85.SetConhecimento(const Value: string);
begin
   fConhecimento := Value;
end;

procedure TRegistro85.SetDataAverbacao(const Value: TDateTime);
begin
  fDataAverbacao := Value;
end;

procedure TRegistro85.SetDataConhecimento(const Value: TDateTime);
begin
  fDataConhecimento := Value;
end;

procedure TRegistro85.SetDataDeclaracao(const Value: TDateTime);
begin
  fDataDeclaracao := Value;
end;

procedure TRegistro85.SetDataNotaFiscal(const Value: TDateTime);
begin
  fDataNotaFiscal := Value;
end;

procedure TRegistro85.SetDataRegistro(const Value: TDateTime);
begin
  fDataRegistro := Value;
end;

procedure TRegistro85.SetDeclaracao(const Value: String);
begin
  fDeclaracao := Value;
end;

procedure TRegistro85.SetModelo(const Value: string);
begin
  fModelo := Value;
end;

procedure TRegistro85.SetNaturezaExportacao(const Value: string);
begin
  fNaturezaExportacao := Value;
end;

procedure TRegistro85.SetNumeroNotaFiscal(const Value: string);
begin
  fNumeroNotaFiscal := Value;
end;

procedure TRegistro85.SetPais(const Value: string);
begin
  fPais := Value;
end;

procedure TRegistro85.SetRegistroExportacao(const Value: String);
begin
  fRegistroExportacao := Value;
end;

procedure TRegistro85.SetSerie(const Value: string);
begin
  fSerie := Value;
end;

procedure TRegistro85.SetTipoConhecimento(const Value: String);
begin
  fTipoConhecimento := Value;
end;

{ TRegistro86 }

function TRegistro86.GetCodigo: string;
begin
  Result := fCodigo;
end;

function TRegistro86.GetCPFCNPJ: string;
begin
  Result := fCPFCNPJ;
end;

function TRegistro86.GetDataDocumento: TDateTime;
begin
  Result := fDataDocumento;
end;

function TRegistro86.GetDataRegistro: TDateTime;
begin
  Result := fDataRegistro;
end;

function TRegistro86.GetInscricao: string;
begin
  Result := fInscricao;
end;

function TRegistro86.GetModelo: string;
begin
  Result := fModelo;
end;

function TRegistro86.GetNumeroNotaFiscal: string;
begin
  Result := fNumeroNotaFiscal;
end;

function TRegistro86.GetQuantidade: Currency;
begin
  Result := fQuantidade;
end;

function TRegistro86.GetRegistroExportacao: string;
begin
  Result := fRegistroExportacao;
end;

function TRegistro86.GetRelacionamento: string;
begin
  Result := fRelacionamento;
end;

function TRegistro86.GetSerie: string;
begin
  Result := fSerie;
end;

function TRegistro86.GetUF: string;
begin
  Result := fUF;
end;

function TRegistro86.GetValorTotalProduto: Currency;
begin
  Result := fValorTotalProduto;
end;

function TRegistro86.GetValorUnitario: Currency;
begin
  Result := fValorUnitario;
end;

procedure TRegistro86.SetCodigo(const Value: string);
begin
  fCodigo := Value;
end;

procedure TRegistro86.SetCPFCNPJ(const Value: string);
begin
  fCPFCNPJ := Value;
end;

procedure TRegistro86.SetDataDocumento(const Value: TDateTime);
begin
  fDataDocumento := Value;
end;

procedure TRegistro86.SetDataRegistro(const Value: TDateTime);
begin
  fDataRegistro := Value;
end;

procedure TRegistro86.SetInscricao(const Value: string);
begin
  fInscricao := Value;
end;

procedure TRegistro86.SetModelo(const Value: string);
begin
  fModelo := Value;
end;

procedure TRegistro86.SetNumeroNotaFiscal(const Value: string);
begin
  fNumeroNotaFiscal := Value;
end;

procedure TRegistro86.SetQuantidade(const Value: Currency);
begin
  fQuantidade := Value;
end;

procedure TRegistro86.SetRegistroExportacao(const Value: string);
begin
  fRegistroExportacao := Value;
end;

procedure TRegistro86.SetRelacionamento(const Value: string);
begin
  fRelacionamento := Value;
end;

procedure TRegistro86.SetSerie(const Value: string);
begin
  fSerie := Value;
end;

procedure TRegistro86.SetUF(const Value: string);
begin
  fUF := Value;
end;

procedure TRegistro86.SetValorTotalProduto(const Value: Currency);
begin
  fValorTotalProduto := Value;
end;

procedure TRegistro86.SetValorUnitario(const Value: Currency);
begin
  fValorUnitario := Value;
end;

{ TRegistro85Lista }

function TRegistro85Lista.GetItem(Index: Integer): IRegistro85;
begin
  Result := Get(Index) as IRegistro85;
end;

function TRegistro85Lista.New: IRegistro85;
begin
  Result := TRegistro85.Create;
  Add(Result);
end;

procedure TRegistro85Lista.SetItem(Index: Integer;
  const Value: IRegistro85);
begin
  Put(Index, Value);
end;

{ TRegistro86Lista }

function TRegistro86Lista.GetItem(Index: Integer): IRegistro86;
begin
  Result := Get(Index) as IRegistro86;
end;

function TRegistro86Lista.New: IRegistro86;
begin
  Result := TRegistro86.Create;
  Add(Result);
end;

procedure TRegistro86Lista.SetItem(Index: Integer;
  const Value: IRegistro86);
begin
  Put(Index, Value);
end;

end.
