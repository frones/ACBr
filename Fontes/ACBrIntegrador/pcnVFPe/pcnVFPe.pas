{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2017 André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esse arquivo usa a classe  PCN (c) 2009 - Paulo Casagrande                  }
{  PCN - Projeto Cooperar NFe       (Found at URL:  www.projetocooperar.org)   }
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

unit pcnVFPe
;

interface

uses
  SysUtils, Classes, ACBrIntegradorResposta;

type

{$M+}
  { TEnviarPagamento }
  TEnviarPagamento = class
  private
    FIdentificador: Integer;
    FChaveAcessoValidador: String;
    FChaveRequisicao: String;
    FEstabelecimento: String;
    FSerialPOS: String;
    FCNPJ: String;
    FIcmsBase: Double;
    FValorTotalVenda: Double;
    FHabilitarMultiplosPagamentos: Boolean;
    FHabilitarControleAntiFraude: Boolean;
    FCodigoMoeda: String;
    FEmitirCupomNFCE: Boolean;
    FOrigemPagamento: String;

    function GetXMLString: AnsiString;
    procedure SetXMLString(const AValue : AnsiString) ;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear ;

    property AsXMLString : AnsiString read GetXMLString write SetXMLString ;
  published
    property Identificador: Integer read FIdentificador write FIdentificador;
    property ChaveAcessoValidador: String read FChaveAcessoValidador write FChaveAcessoValidador;
    property ChaveRequisicao: String read FChaveRequisicao write FChaveRequisicao;
    property Estabelecimento: String read FEstabelecimento write FEstabelecimento;
    property SerialPOS: String read FSerialPOS write FSerialPOS;
    property CNPJ: String read FCNPJ write FCNPJ;
    property IcmsBase: Double read FIcmsBase write FIcmsBase;
    property ValorTotalVenda: Double read FValorTotalVenda write FValorTotalVenda;
    property HabilitarMultiplosPagamentos: Boolean read FHabilitarMultiplosPagamentos write FHabilitarMultiplosPagamentos;
    property HabilitarControleAntiFraude: Boolean read FHabilitarControleAntiFraude write FHabilitarControleAntiFraude;
    property CodigoMoeda: String read FCodigoMoeda write FCodigoMoeda;
    property EmitirCupomNFCE: Boolean read FEmitirCupomNFCE write FEmitirCupomNFCE;
    property OrigemPagamento: String read FOrigemPagamento write FOrigemPagamento;
  end;

  { TRespostaPagamento }
  TRespostaPagamento = class
  private
    FIntegradorResposta : TIntegradorResposta;
    FIDPagamento: Integer;
    FMensagem: String;
    FStatusPagamento: String;
    FXML : AnsiString;
    FRetorno: String;

    procedure SetXMLString(const AValue: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property AsXMLString : AnsiString  write SetXMLString ;
  published
    property IntegradorResposta: TIntegradorResposta read FIntegradorResposta write FIntegradorResposta;
    property IDPagamento: Integer read FIDPagamento write FIDPagamento;
    property Mensagem: String read FMensagem write FMensagem;
    property StatusPagamento: String read FStatusPagamento write FStatusPagamento;
    property XML: AnsiString read FXML write SetXMLString;
    property Retorno: String read FRetorno write FRetorno;
  end;

  { TVerificarStatusValidador }
  TVerificarStatusValidador = class
  private
    FIdentificador: Integer;
    FChaveAcessoValidador: String;
    FIDFila: Integer;
    FCNPJ: String;

    function GetXMLString: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear ;

    property AsXMLString : AnsiString read GetXMLString ;
  published
    property Identificador: Integer read FIdentificador write FIdentificador;
    property ChaveAcessoValidador: String read FChaveAcessoValidador write FChaveAcessoValidador;
    property IDFila: Integer read FIDFila write FIDFila;
    property CNPJ: String read FCNPJ write FCNPJ;
  end;

  { TRespostaVerificarStatusValidador }
  TRespostaVerificarStatusValidador = class
  private
    FIntegradorResposta : TIntegradorResposta;
    FCodigoAutorizacao : String;
    FBin : String;
    FDonoCartao : String;
    FDataExpiracao : String;
    FInstituicaoFinanceira : String;
    FParcelas : Integer;
    FUltimosQuatroDigitos : Integer;
    FCodigoPagamento : String;
    FValorPagamento : Double;
    FIDFila : Integer;
    FTipo : String;
    FXML : AnsiString;
    FRetorno: String;

    procedure SetXMLString(const AValue: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property AsXMLString : AnsiString  write SetXMLString ;
  published
    property IntegradorResposta: TIntegradorResposta read FIntegradorResposta write FIntegradorResposta;
    property CodigoAutorizacao: String read FCodigoAutorizacao write FCodigoAutorizacao;
    property Bin: String read FBin write FBin;
    property DonoCartao: String read FDonoCartao write FDonoCartao;
    property DataExpiracao: String read FDataExpiracao write FDataExpiracao;
    property InstituicaoFinanceira: String read FInstituicaoFinanceira write FInstituicaoFinanceira;
    property Parcelas: Integer read FParcelas write FParcelas;
    property UltimosQuatroDigitos: Integer read FUltimosQuatroDigitos write FUltimosQuatroDigitos;
    property CodigoPagamento: String read FCodigoPagamento write FCodigoPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property IDFila: Integer read FIDFila write FIDFila;
    property Tipo: String read FTipo write FTipo;
    property XML: AnsiString read FXML write SetXMLString;
    property Retorno: String read FRetorno write FRetorno;
  end;

  { TRespostaFiscal }
  TRespostaFiscal = class
  private
    FIdentificador: Integer;
    FChaveAcessoValidador: String;
    FIDFila: Integer;
    FChaveAcesso: String;
    FNsu: String;
    FNumerodeAprovacao: String;
    FBandeira: String;
    FAdquirente: String;
    FCNPJ: String;
    FImpressaoFiscal: String;
    FNumeroDocumento: String;

    function GetXMLString: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear ;

    property AsXMLString : AnsiString read GetXMLString ;
  published
    property Identificador: Integer read FIdentificador write FIdentificador;
    property ChaveAcessoValidador: String read FChaveAcessoValidador write FChaveAcessoValidador;
    property IDFila: Integer read FIDFila write FIDFila;
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property Nsu: String read FNsu write FNsu;
    property NumerodeAprovacao: String read FNumerodeAprovacao write FNumerodeAprovacao;
    property Bandeira: String read FBandeira write FBandeira;
    property Adquirente: String read FAdquirente write FAdquirente;
    property CNPJ: String read FCNPJ write FCNPJ;
    property ImpressaoFiscal: String read FImpressaoFiscal write FImpressaoFiscal;
    property NumeroDocumento: String read FNumeroDocumento write FNumeroDocumento;
  end;

  { TRetornoRespostaFiscal }
  TRetornoRespostaFiscal = class
  private
    FIntegradorResposta : TIntegradorResposta;
    FIdRespostaFiscal: String;
    FXML: AnsiString;

    procedure SetXMLString(const AValue: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property AsXMLString : AnsiString  write SetXMLString ;
  published
    property IntegradorResposta: TIntegradorResposta read FIntegradorResposta write FIntegradorResposta;
    property IdRespostaFiscal: String read FIdRespostaFiscal write FIdRespostaFiscal;
    property XML: AnsiString read FXML write SetXMLString;
  end;

  { TStatusPagamento }
  TStatusPagamento = class
  private
    FIdentificador: Integer;
    FChaveAcessoValidador: String;
    FCodigoAutorizacao : String;
    FBin : String;
    FDonoCartao : String;
    FDataExpiracao : String;
    FInstituicaoFinanceira : String;
    FParcelas : Integer;
    FCodigoPagamento : String;
    FValorPagamento : Double;
    FIDFila : Integer;
    FTipo : String;
    FUltimosQuatroDigitos : Integer;

    function GetXMLString: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear ;

    property AsXMLString : AnsiString read GetXMLString;
  published
    property Identificador: Integer read FIdentificador write FIdentificador;
    property ChaveAcessoValidador: String read FChaveAcessoValidador write FChaveAcessoValidador;
    property CodigoAutorizacao: String read FCodigoAutorizacao write FCodigoAutorizacao;
    property Bin: String read FBin write FBin;
    property DonoCartao: String read FDonoCartao write FDonoCartao;
    property DataExpiracao: String read FDataExpiracao write FDataExpiracao;
    property InstituicaoFinanceira: String read FInstituicaoFinanceira write FInstituicaoFinanceira;
    property Parcelas: Integer read FParcelas write FParcelas;
    property CodigoPagamento: String read FCodigoPagamento write FCodigoPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property IDFila: Integer read FIDFila write FIDFila;
    property Tipo: String read FTipo write FTipo;
    property UltimosQuatroDigitos: Integer read FUltimosQuatroDigitos write FUltimosQuatroDigitos;
  end;

  { TRespostaStatusPagamento }
  TRespostaStatusPagamento = class
  private
    FIntegradorResposta : TIntegradorResposta;
    FRetorno: String;
    FXML: AnsiString;

    procedure SetXMLString(const AValue: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property AsXMLString : AnsiString  write SetXMLString ;
  published
    property IntegradorResposta: TIntegradorResposta read FIntegradorResposta write FIntegradorResposta;
    property Retorno: String read FRetorno write FRetorno;
    property XML : AnsiString read FXML write SetXMLString;
  end;

{$M-}

implementation

Uses
  pcnVFPeW, pcnVFPeR ;

{ TRespostaStatusPagamento }

procedure TRespostaStatusPagamento.SetXMLString(const AValue: AnsiString);
var
 LocRespostaStatusPagamentoR : TRespostaStatusPagamentoR;
begin
  FXML := AValue;
  LocRespostaStatusPagamentoR := TRespostaStatusPagamentoR.Create(Self);
  try
    LocRespostaStatusPagamentoR.Leitor.Arquivo := AValue;
    LocRespostaStatusPagamentoR.LerXml;
  finally
    LocRespostaStatusPagamentoR.Free
  end;
end;

constructor TRespostaStatusPagamento.Create;
begin
  FIntegradorResposta := TIntegradorResposta.Create;
  Clear;
end;

destructor TRespostaStatusPagamento.Destroy;
begin
  FIntegradorResposta.Free;
  inherited Destroy;
end;

procedure TRespostaStatusPagamento.Clear;
begin
  FIntegradorResposta.Clear;
  FRetorno := '';
end;

{ TStatusPagamento }

function TStatusPagamento.GetXMLString: AnsiString;
var
  LocStatusPagamentoW : TStatusPagamentoW ;
begin
  Result  := '';
  LocStatusPagamentoW := TStatusPagamentoW.Create(Self);
  try
    LocStatusPagamentoW.Gerador.Opcoes.IdentarXML := False;

    LocStatusPagamentoW.GerarXml();
    Result := LocStatusPagamentoW.Gerador.ArquivoFormatoXML;
  finally
    LocStatusPagamentoW.Free;
  end ;
end;

constructor TStatusPagamento.Create;
begin
  Clear;
end;

destructor TStatusPagamento.Destroy;
begin
  inherited Destroy;
end;

procedure TStatusPagamento.Clear;
begin
  FIdentificador := 0;
  FChaveAcessoValidador := '';
  FCodigoAutorizacao := '';
  FBin := '';
  FDonoCartao := '';
  FDataExpiracao := '';
  FInstituicaoFinanceira := '';
  FParcelas := 0;
  FCodigoPagamento := '';
  FValorPagamento := 0;
  FIDFila := 0;
  FTipo := '';
  FUltimosQuatroDigitos := 0;
end;

{ TRespostaFiscal }

function TRespostaFiscal.GetXMLString: AnsiString;
var
  LocRespostaFiscalW : TRespostaFiscalW ;
begin
  Result  := '';
  LocRespostaFiscalW := TRespostaFiscalW.Create(Self);
  try
    LocRespostaFiscalW.Gerador.Opcoes.IdentarXML := False;

    LocRespostaFiscalW.GerarXml();
    Result := LocRespostaFiscalW.Gerador.ArquivoFormatoXML;
  finally
    LocRespostaFiscalW.Free;
  end ;
end;

constructor TRespostaFiscal.Create;
begin
  Clear;
end;

destructor TRespostaFiscal.Destroy;
begin
  inherited Destroy;
end;

procedure TRespostaFiscal.Clear;
begin
  FIdentificador := 0;
  FChaveAcessoValidador := '';
  FIDFila := 0;
  FChaveAcesso := '';
  FNsu := '';
  FNumerodeAprovacao := '';
  FBandeira := '';
  FAdquirente := '';
  FCNPJ := '';
  FImpressaoFiscal := '';
  FNumeroDocumento := '';
end;

{ TRespostaVerificarStatusValidador }

procedure TRespostaVerificarStatusValidador.SetXMLString(const AValue: AnsiString);
var
 LocRespostaVerificarStatusValidadorR : TRespostaVerificarStatusValidadorR;
begin
  FXMl := AValue;
  LocRespostaVerificarStatusValidadorR := TRespostaVerificarStatusValidadorR.Create(Self);
  try
    LocRespostaVerificarStatusValidadorR.Leitor.Arquivo := AValue;
    LocRespostaVerificarStatusValidadorR.LerXml;
  finally
    LocRespostaVerificarStatusValidadorR.Free
  end;
end;

constructor TRespostaVerificarStatusValidador.Create;
begin
  FIntegradorResposta := TIntegradorResposta.Create;
  Clear;
end;

destructor TRespostaVerificarStatusValidador.Destroy;
begin
  FIntegradorResposta.Free;
  inherited Destroy;
end;

procedure TRespostaVerificarStatusValidador.Clear;
begin
  FIntegradorResposta.Clear;
  FCodigoAutorizacao := '';
  FBin := '';
  FDonoCartao := '';
  FDataExpiracao := '';
  FInstituicaoFinanceira := '';
  FParcelas := 0;
  FUltimosQuatroDigitos := 0;
  FCodigoPagamento := '';
  FValorPagamento := 0;
  FIDFila := 0;
  FTipo := '';
  FRetorno := '';
end;

{ TVerificarStatusValidador }

function TVerificarStatusValidador.GetXMLString: AnsiString;
var
  LocVerificarStatusValidadorW : TVerificarStatusValidadorW ;
begin
  Result  := '';
  LocVerificarStatusValidadorW := TVerificarStatusValidadorW.Create(Self);
  try
    LocVerificarStatusValidadorW.Gerador.Opcoes.IdentarXML := False;

    LocVerificarStatusValidadorW.GerarXml();
    Result := LocVerificarStatusValidadorW.Gerador.ArquivoFormatoXML;
  finally
    LocVerificarStatusValidadorW.Free;
  end ;
end;

constructor TVerificarStatusValidador.Create;
begin
  Clear;
end;

destructor TVerificarStatusValidador.Destroy;
begin
  inherited Destroy;
end;

procedure TVerificarStatusValidador.Clear;
begin
  FIdentificador := 0;
  FChaveAcessoValidador := '';
  FIDFila := 0;
  FCNPJ := '';
end;

{ TRespostaPagamento }

procedure TRespostaPagamento.SetXMLString(const AValue: AnsiString);
var
 LocRespostaPagamentoR : TRespostaPagamentoR;
begin
  FXML := AValue;
  LocRespostaPagamentoR := TRespostaPagamentoR.Create(Self);
  try
    LocRespostaPagamentoR.Leitor.Arquivo := AValue;
    LocRespostaPagamentoR.LerXml;
  finally
    LocRespostaPagamentoR.Free
  end;
end;

constructor TRespostaPagamento.Create;
begin
  FIntegradorResposta := TIntegradorResposta.Create;
  Clear;
end;

destructor TRespostaPagamento.Destroy;
begin
  FIntegradorResposta.Free;
  inherited Destroy;
end;

procedure TRespostaPagamento.Clear;
begin
  FIntegradorResposta.Clear;
  FIDPagamento := 0;
  FMensagem    := '';
  FStatusPagamento := '';
  FRetorno     := '';;
end;

{ TEnviarPagamento }
constructor TEnviarPagamento.Create;
begin
  Clear;
end;

destructor TEnviarPagamento.Destroy;
begin
  inherited Destroy;
end;

procedure TEnviarPagamento.Clear;
begin
  FIdentificador := 0;
  FChaveAcessoValidador := '';
  FChaveRequisicao := '';
  FEstabelecimento := '';
  FSerialPOS := '';
  FCNPJ := '';
  FIcmsBase := 0;
  FValorTotalVenda := 0;	
end;

function TEnviarPagamento.GetXMLString(): AnsiString;
var
  LocEnviarPagamentoW : TEnviarPagamentoW ;
begin
  Result  := '';
  LocEnviarPagamentoW := TEnviarPagamentoW.Create(Self);
  try
    LocEnviarPagamentoW.Gerador.Opcoes.IdentarXML := False;

    LocEnviarPagamentoW.GerarXml();
    Result := LocEnviarPagamentoW.Gerador.ArquivoFormatoXML;
  finally
    LocEnviarPagamentoW.Free;
  end ;
end;

procedure TEnviarPagamento.SetXMLString(const AValue: AnsiString);
var
 LocEnviarPagamentoR : TEnviarPagamentoR;
begin
  LocEnviarPagamentoR := TEnviarPagamentoR.Create(Self);
  try
    LocEnviarPagamentoR.Leitor.Arquivo := AValue;
    LocEnviarPagamentoR.LerXml;
  finally
    LocEnviarPagamentoR.Free
  end;
end;

{ TRetornoRespostaFiscal }

procedure TRetornoRespostaFiscal.Clear;
begin
  FIntegradorResposta.Clear;
  FIdRespostaFiscal := '';
end;

constructor TRetornoRespostaFiscal.Create;
begin
  FIntegradorResposta := TIntegradorResposta.Create;
  Clear;
end;

destructor TRetornoRespostaFiscal.Destroy;
begin
  FIntegradorResposta.Free;
  inherited Destroy;
end;

procedure TRetornoRespostaFiscal.SetXMLString(const AValue: AnsiString);
var
 LocRespostaFiscalR : TRetornoRespostaFiscalR;
begin
  FXML := AValue;
  LocRespostaFiscalR := TRetornoRespostaFiscalR.Create(Self);
  try
    LocRespostaFiscalR.Leitor.Arquivo := AValue;
    LocRespostaFiscalR.LerXml;
  finally
    LocRespostaFiscalR.Free
  end;
end;

end.
 
