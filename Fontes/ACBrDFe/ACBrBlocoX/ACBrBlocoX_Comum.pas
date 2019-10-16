{******************************************************************************}
{ Projeto: Componente ACBrBlocoX                                               }
{ Biblioteca multiplataforma de componentes Delphi para Geração de arquivos    }
{ do Bloco X                                                                   }
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
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBlocoX_Comum;

interface

uses
  Classes, SysUtils, pcnGerador;

type
  EACBrBlocoXException = class(Exception);

  TACBrBlocoX_TipoCodigo = (tpcGTIN, tpcEAN, tpcProprio);
  TACBrBlocoX_SitTributaria = (stIsento, stNaoTributado, stSubstTributaria, stTributado, stISSQN);
  TACBrBlocoX_URLWebService = (wsRecepcao, wsBlocoX);
  TACBrBlocoX_Ippt = (ipptProprio, ipptTerceiros);
  TVersaoER = (erv0204, erv0205, erv0206);

  TACBrBlocoX_Codigo = class
  private
    FTipo: TACBrBlocoX_TipoCodigo;
    FCodigoGTIN : string;
    FCodigoCEST : string;
    FCodigoNCMSH : string;
    FCodigoProprio : string;

  public
    property Tipo: TACBrBlocoX_TipoCodigo read FTipo write FTipo;
    property CodigoGTIN  : String read FCodigoGTIN write FCodigoGTIN;
    property CodigoCEST  : String read FCodigoCEST write FCodigoCEST;
    property CodigoNCMSH : String read FCodigoNCMSH write FCodigoNCMSH;
    property CodigoProprio : String read FCodigoProprio write FCodigoProprio;

  end;

  TACBrBlocoX_Produto = class(TCollectionItem)
  private
    FCodigo: TACBrBlocoX_Codigo;
    FDescricao: String;
    FValorUnitario: Double;
    FIppt: TACBrBlocoX_Ippt;
    FAliquota: Double;
    FUnidade: String;
    FQuantidade: double;
    FValorDesconto : Double;
    FValorAcrescimo : Double;
    FValorCancelamento : Double;
    FValorTotalLiquido : Double;
    FIndicadorArredondamento: Boolean;
    FSituacaoTributaria: TACBrBlocoX_SitTributaria;
    FSituacaoEstoque : string;

    FValorTotalAquisicaoMercadoria : Double;
    FQuantidadeTotalAquisicaoMercadoria : Double;
    FValorTotalICMSDebitoFornecedor : Double;
    FValorBaseCalculoICMSST : Double;
    FValorTotalICMSST       : Double;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Codigo: TACBrBlocoX_Codigo read FCodigo write FCodigo;

    property Descricao: String read FDescricao write FDescricao;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property Unidade: String read FUnidade write FUnidade;
    property ValorUnitario: Double read FValorUnitario write FValorUnitario;
    property SituacaoTributaria: TACBrBlocoX_SitTributaria read FSituacaoTributaria write FSituacaoTributaria;
    property Aliquota: Double read FAliquota write FAliquota;
    property IndicadorArredondamento: Boolean read FIndicadorArredondamento write FIndicadorArredondamento;
    property Ippt: TACBrBlocoX_Ippt read FIppt write FIppt;
    property SituacaoEstoque: String read FSituacaoEstoque write FSituacaoEstoque;

    property ValorDesconto: Double  read FValorDesconto write FValorDesconto;
    property ValorAcrescimo: Double read FValorAcrescimo write FValorAcrescimo;
    property ValorCancelamento: Double read FValorCancelamento write FValorCancelamento;
    property ValorTotalLiquido: Double read FValorTotalLiquido write FValorTotalLiquido;

    property ValorTotalAquisicaoMercadoria: Double read FValorTotalAquisicaoMercadoria write FValorTotalAquisicaoMercadoria;
    property QuantidadeTotalAquisicaoMercadoria: Double read FQuantidadeTotalAquisicaoMercadoria write FQuantidadeTotalAquisicaoMercadoria;
    property ValorTotalICMSDebitoFornecedor: Double read FValorTotalICMSDebitoFornecedor write FValorTotalICMSDebitoFornecedor;
    property ValorBaseCalculoICMSST: Double read FValorBaseCalculoICMSST write FValorBaseCalculoICMSST;
    property ValorTotalICMSST: Double read FValorTotalICMSST write FValorTotalICMSST;

  end;

  TACBrBlocoX_Servico = class(TACBrBlocoX_Produto);

  TACBrBlocoX_Produtos = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TACBrBlocoX_Produto;
    procedure SetItem(Index: integer; const Value: TACBrBlocoX_Produto);
  public
    function Add: TACBrBlocoX_Produto;
    function Insert(Index: integer): TACBrBlocoX_Produto;

    property Items[Index: integer]: TACBrBlocoX_Produto read GetItem write SetItem; default;
  end;

  TACBrBlocoX_Servicos = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TACBrBlocoX_Servico;
    procedure SetItem(Index: integer; const Value: TACBrBlocoX_Servico);
  public
    function Add: TACBrBlocoX_Servico;
    function Insert(Index: integer): TACBrBlocoX_Servico;

    property Items[Index: integer]: TACBrBlocoX_Servico read GetItem write SetItem; default;
  end;

  { TACBrBlocoX_BaseFile }

  TACBrBlocoX_BaseFile = class(TComponent)
  protected
    FACBrBlocoX: TComponent;
    FGerador: TGerador;
    FXMLOriginal: string;
    FXMLAssinado: string;
    FRemoverEncodingXMLAssinado: Boolean;

    procedure GerarDadosEstabelecimento;
    procedure GerarDadosPafECF;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property XMLOriginal: string read FXMLOriginal;
    property XMLAssinado: string read FXMLAssinado;
    property RemoverEncodingXMLAssinado: Boolean read FRemoverEncodingXMLAssinado write FRemoverEncodingXMLAssinado;

    procedure GerarXML(const Assinar: Boolean = True); virtual;
    procedure SaveToFile(const AXmlFileName: String; const AAssinar: Boolean = True); virtual;
  end;

  function TipoCodigoToStr(const AValue: TACBrBlocoX_TipoCodigo): String;
  function SituacaoTributariaToStr(const AValue: TACBrBlocoX_SitTributaria): String;
  function IpptToStr(const AValue: TACBrBlocoX_Ippt): String;

  function StrToTipoCodigo(var OK: Boolean; const AValue: String): TACBrBlocoX_TipoCodigo;
  function StrToSituacaoTributaria(var OK: Boolean; const AValue: String): TACBrBlocoX_SitTributaria;
  function StrToIppt(var OK: Boolean; const AValue: String): TACBrBlocoX_Ippt;

  function ZipFile(const DadosXML: string; const NomeArquivo: String): AnsiString;
  function UnZipFile(const DadosXML: string): AnsiString;

implementation

uses
  ACBrBlocoX, ACBrUtil, ACBrCompress, pcnConversao, synacode;

function TipoCodigoToStr(const AValue: TACBrBlocoX_TipoCodigo): String;
begin
  Result := EnumeradoToStr(AValue,
    ['GTIN', 'EAN', 'Proprio'],
    [tpcGTIN, tpcEAN, tpcProprio]
  );
end;

function SituacaoTributariaToStr(const AValue: TACBrBlocoX_SitTributaria): String;
begin
  Result := EnumeradoToStr(AValue,
    ['Isento', 'Nao tributado', 'Substituicao tributaria', 'Tributado pelo ICMS', 'Tributado pelo ISSQN'],
    [stIsento, stNaoTributado, stSubstTributaria, stTributado, stISSQN]
  );
end;

function IpptToStr(const AValue: TACBrBlocoX_Ippt): String;
begin
  Result := EnumeradoToStr(AValue,
    ['Proprio', 'Terceiros'],
    [ipptProprio, ipptTerceiros]
  );
end;

function StrToTipoCodigo(var OK: Boolean; const AValue: String): TACBrBlocoX_TipoCodigo;
begin
  Result := StrToEnumerado(OK, AValue,
    ['GTIN', 'EAN', 'Proprio'],
    [tpcGTIN, tpcEAN, tpcProprio]
  );
end;

function StrToSituacaoTributaria(var OK: Boolean; const AValue: String): TACBrBlocoX_SitTributaria;
begin
  Result := StrToEnumerado(OK, AValue,
    ['I', 'N', 'F', 'T', 'S'],
    [stIsento, stNaoTributado, stSubstTributaria, stTributado, stISSQN]
  );
end;

function StrToIppt(var OK: Boolean; const AValue: String): TACBrBlocoX_Ippt;
begin
  Result := StrToEnumerado(OK, AValue,
    ['P', 'T'],
    [ipptProprio, ipptTerceiros]
  );
end;

function ZipFile(const DadosXML: string; const NomeArquivo: String): AnsiString;
begin
  Result := ACBrCompress.ZipFileCompress(DadosXML, NomeArquivo);
end;

function UnZipFile(const DadosXML: string): AnsiString;
begin
  Result := ACBrCompress.ZipFileDeCompress(DadosXML);
end;

{ TACBrBlocoX_Produto }

constructor TACBrBlocoX_Produto.Create(Collection: TCollection);
begin
  inherited;
  FCodigo := TACBrBlocoX_Codigo.Create;
end;

destructor TACBrBlocoX_Produto.Destroy;
begin
  FCodigo.Free;
  inherited;
end;

{ TACBrBlocoX_Produtos }

function TACBrBlocoX_Produtos.Add: TACBrBlocoX_Produto;
begin
  Result := TACBrBlocoX_Produto(inherited Add);
end;

function TACBrBlocoX_Produtos.GetItem(Index: integer): TACBrBlocoX_Produto;
begin
  Result := TACBrBlocoX_Produto(inherited Items[Index]);
end;

function TACBrBlocoX_Produtos.Insert(Index: integer): TACBrBlocoX_Produto;
begin
  Result := TACBrBlocoX_Produto(inherited Insert(Index));
end;

procedure TACBrBlocoX_Produtos.SetItem(Index: integer;
  const Value: TACBrBlocoX_Produto);
begin
  Items[Index].Assign(Value);
end;

{ TACBrBlocoX_Servicos }

function TACBrBlocoX_Servicos.Add: TACBrBlocoX_Servico;
begin
  Result := TACBrBlocoX_Servico(inherited Add);
end;

function TACBrBlocoX_Servicos.GetItem(Index: integer): TACBrBlocoX_Servico;
begin
  Result := TACBrBlocoX_Servico(inherited Items[Index]);
end;

function TACBrBlocoX_Servicos.Insert(Index: integer): TACBrBlocoX_Servico;
begin
  Result := TACBrBlocoX_Servico(inherited Insert(Index));
end;

procedure TACBrBlocoX_Servicos.SetItem(Index: integer;
  const Value: TACBrBlocoX_Servico);
begin
  Items[Index].Assign(Value);
end;

{ TACBrBlocoX_BaseFile }

constructor TACBrBlocoX_BaseFile.Create(AOwner: TComponent);
begin
  inherited;

  FACBrBlocoX := TACBrBlocoX(AOwner);
  FGerador := TGerador.Create;
  FRemoverEncodingXMLAssinado := False;
end;

destructor TACBrBlocoX_BaseFile.Destroy;
begin
  FGerador.Free;
  inherited;
end;

procedure TACBrBlocoX_BaseFile.GerarXML(const Assinar: Boolean = True);
begin
  raise EACBrBlocoXException.Create('Método não implementado "GerarXML"');
end;

procedure TACBrBlocoX_BaseFile.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  raise EACBrBlocoXException.Create('Método não implementado "SaveToFileName"');
end;

procedure TACBrBlocoX_BaseFile.GerarDadosEstabelecimento;
begin
  FGerador.wGrupo('Estabelecimento');
  with TACBrBlocoX(FACBrBlocoX) do
  begin
    FGerador.wCampo(tcStr, '', 'Ie', 0, 0, 1, Estabelecimento.Ie);
  end;
  FGerador.wGrupo('/Estabelecimento');
end;

procedure TACBrBlocoX_BaseFile.GerarDadosPafECF;
begin
  FGerador.wGrupo('PafEcf');
  with TACBrBlocoX(FACBrBlocoX) do
  begin
    FGerador.wCampo(tcStr, '', 'NumeroCredenciamento', 0, 0, 1, PafECF.NumeroCredenciamento);
  end;
  FGerador.wGrupo('/PafEcf');
end;

end.
