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

unit pcnVFPeW;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, pcnVFPe, ACBrUtil;

const
  cChaveAcessoIntegrador = '25CFE38D-3B92-46C0-91CA-CFF751A82D3D';

type

  { TConstrutor }
  TConstrutor = class
  private
    FGerador: TGerador;
  public
    constructor Create(AGerador: TGerador);
    procedure GerarConstructor( Nome, Valor: String );
  end;

  { TMetodo }
  TMetodo = class
  private
    FGerador: TGerador;
    FAdicionarParametros : Boolean;
  public
    constructor Create(AGerador: TGerador);
    procedure GerarMetodo( Valor : Integer; Componente, Metodo : String );
    procedure FinalizarMetodo;

    property AdicionarParametros : Boolean read FAdicionarParametros write FAdicionarParametros default True;
  end;

  { TParametro }
  TParametro = class
  private
    FGerador: TGerador;
  public
    constructor Create(AGerador: TGerador);
    procedure GerarParametro( Nome: String; Valor: Variant; Tipo: TpcnTipoCampo; ParseTextoXML: Boolean = True );
  end;

{$M+}
  { TEnviarPagamentoW }
  TEnviarPagamentoW = class
  private
    FGerador: TGerador;
    FEnviarPagamento: TEnviarPagamento;
    function GetOpcoes: TGeradorOpcoes;
  public
    constructor Create(AOwner: TEnviarPagamento);
    destructor Destroy; override;
    function GerarXml: boolean;
  published
    property Gerador: TGerador read FGerador ;
    property EnviarPagamento: TEnviarPagamento read FEnviarPagamento write FEnviarPagamento;
    property Opcoes: TGeradorOpcoes read GetOpcoes ;
  end;

  { TVerificarStatusValidadorW }
  TVerificarStatusValidadorW = class
  private
    FGerador: TGerador;
    FVerificarStatusValidador: TVerificarStatusValidador;
    function GetOpcoes: TGeradorOpcoes;
  public
    constructor Create(AOwner: TVerificarStatusValidador);
    destructor Destroy; override;
    function GerarXml: boolean;
  published
    property Gerador: TGerador read FGerador ;
    property VerificarStatusValidador: TVerificarStatusValidador read FVerificarStatusValidador write FVerificarStatusValidador;
    property Opcoes: TGeradorOpcoes read GetOpcoes ;
  end;

  { TRespostaFiscalW }
  TRespostaFiscalW = class
  private
    FGerador: TGerador;
    FRespostaFiscal: TRespostaFiscal;
    function GetOpcoes: TGeradorOpcoes;
  public
    constructor Create(AOwner: TRespostaFiscal);
    destructor Destroy; override;
    function GerarXml: boolean;
  published
    property Gerador: TGerador read FGerador ;
    property RespostaFiscal: TRespostaFiscal read FRespostaFiscal write FRespostaFiscal;
    property Opcoes: TGeradorOpcoes read GetOpcoes ;
  end;

  { TStatusPagamentoW }
  TStatusPagamentoW = class
  private
    FGerador: TGerador;
    FStatusPagamento: TStatusPagamento;
    function GetOpcoes: TGeradorOpcoes;
  public
    constructor Create(AOwner: TStatusPagamento);
    destructor Destroy; override;
    function GerarXml: boolean;
  published
    property Gerador: TGerador read FGerador ;
    property StatusPagamento: TStatusPagamento read FStatusPagamento write FStatusPagamento;
    property Opcoes: TGeradorOpcoes read GetOpcoes ;
  end;

{$M-}

implementation

{ TConstrutor }

constructor TConstrutor.Create(AGerador: TGerador);
begin
   FGerador := AGerador;
end;

procedure TConstrutor.GerarConstructor(Nome, Valor: String);
begin
  FGerador.wGrupo('Construtor');
  FGerador.wGrupo('Parametros');
  FGerador.wGrupo('Parametro');
  FGerador.wCampo(tcStr, '', 'Nome', 1, 99, 1, Nome, 'Nome do Construtor');
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Valor, 'Valor do Construtor');
  FGerador.wGrupo('/Parametro');
  FGerador.wGrupo('/Parametros');
  FGerador.wGrupo('/Construtor');
end;

{ TMetodo }

constructor TMetodo.Create(AGerador: TGerador);
begin
  FGerador := AGerador;
  FAdicionarParametros := True;
end;

procedure TMetodo.GerarMetodo(Valor: Integer; Componente, Metodo: String);
begin
  FGerador.wGrupo('Integrador');

  FGerador.wGrupo('Identificador');
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Valor, 'Valor do Identificador');
  FGerador.wGrupo('/Identificador');

  FGerador.wGrupo('Componente Nome="'+Componente+'"');
  FGerador.wGrupo('Metodo Nome="'+Metodo+'"');

  if AdicionarParametros then
    FGerador.wGrupo('Parametros');
end;

procedure TMetodo.FinalizarMetodo;
begin
  if AdicionarParametros then
    FGerador.wGrupo('/Parametros');

  FGerador.wGrupo('/Metodo');
  FGerador.wGrupo('/Componente');
  FGerador.wGrupo('/Integrador');
end;

{ TParametro }

constructor TParametro.Create(AGerador: TGerador);
begin
  FGerador := AGerador;
end;

procedure TParametro.GerarParametro(Nome: String; Valor: Variant;
  Tipo: TpcnTipoCampo; ParseTextoXML: Boolean);
begin
  FGerador.wGrupo('Parametro');
  FGerador.wCampo(tcStr, '', 'Nome', 1, 99, 1, Nome, 'Nome do Parâmetro');
  FGerador.wCampo(Tipo , '', 'Valor', 1, 99, 1, Valor, 'Valor do Parâmetro',ParseTextoXML);
  FGerador.wGrupo('/Parametro');
end;


{ TStatusPagamentoW }

function TStatusPagamentoW.GetOpcoes: TGeradorOpcoes;
begin
  Result := FGerador.Opcoes;
end;

constructor TStatusPagamentoW.Create(AOwner: TStatusPagamento);
begin
  FStatusPagamento := AOwner;
  FGerador := TGerador.Create;
end;

destructor TStatusPagamentoW.Destroy;
begin
  FGerador.Free;
  inherited Destroy;
end;

function TStatusPagamentoW.GerarXml: boolean;
var
  Metodo : TMetodo;
  Construtor : TConstrutor;
  Parametro: TParametro;
begin
  Gerador.LayoutArquivoTXT.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  Metodo := TMetodo.Create(Gerador);
  try
    Metodo.AdicionarParametros := False;
    Metodo.GerarMetodo(StatusPagamento.Identificador,'VFP-e','EnviarStatusPagamento');

    Construtor := TConstrutor.Create(Gerador);
    try
      Construtor.GerarConstructor('chaveAcessoValidador', StatusPagamento.ChaveAcessoValidador);
    finally
      Construtor.Free;
    end;

    Gerador.wGrupo('Parametros');

    Parametro := TParametro.Create(Gerador);
    try
      Parametro.GerarParametro('CodigoAutorizacao'  , StatusPagamento.CodigoAutorizacao, tcStr);
      Parametro.GerarParametro('Bin'                , StatusPagamento.Bin, tcStr);
      Parametro.GerarParametro('DonoCartao'         , StatusPagamento.DonoCartao, tcStr);
      Parametro.GerarParametro('DataExpiracao'      , StatusPagamento.DataExpiracao, tcStr);
      Parametro.GerarParametro('InstituicaoFinanceira', StatusPagamento.InstituicaoFinanceira, tcStr);
      Parametro.GerarParametro('Parcelas'           , StatusPagamento.Parcelas, tcInt);
      Parametro.GerarParametro('CodigoPagamento'    , StatusPagamento.CodigoPagamento, tcStr);
      Parametro.GerarParametro('ValorPagamento'     , StatusPagamento.ValorPagamento, tcDe2);
      Parametro.GerarParametro('idFila'             , StatusPagamento.IDFila, tcInt);
      Parametro.GerarParametro('Tipo'               , StatusPagamento.Tipo, tcStr);
      Parametro.GerarParametro('UltimosQuatroDigitos', StatusPagamento.UltimosQuatroDigitos, tcInt);
    finally
      Parametro.Free;
    end;

    Metodo.AdicionarParametros := True;
    Metodo.FinalizarMetodo;
  finally
    Metodo.Free;
  end;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

{ TRespostaFiscalW }

function TRespostaFiscalW.GetOpcoes: TGeradorOpcoes;
begin
  Result := FGerador.Opcoes;
end;

constructor TRespostaFiscalW.Create(AOwner: TRespostaFiscal);
begin
  FRespostaFiscal := AOwner;
  FGerador := TGerador.Create;
end;

destructor TRespostaFiscalW.Destroy;
begin
  FGerador.Free;
  inherited Destroy;
end;

function TRespostaFiscalW.GerarXml: boolean;
var
  Metodo : TMetodo;
  Construtor : TConstrutor;
  Parametro: TParametro;
begin
  Gerador.LayoutArquivoTXT.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  Metodo := TMetodo.Create(Gerador);
  try
    Metodo.AdicionarParametros := False;
    Metodo.GerarMetodo(RespostaFiscal.Identificador,'VFP-e','RespostaFiscal');

    Construtor := TConstrutor.Create(Gerador);
    try
      Construtor.GerarConstructor('chaveAcessoValidador', RespostaFiscal.ChaveAcessoValidador);
    finally
      Construtor.Free;
    end;

    Gerador.wGrupo('Parametros');

    Parametro := TParametro.Create(Gerador);
    try
      Parametro.GerarParametro('idFila'           , RespostaFiscal.IDFila, tcInt);
      Parametro.GerarParametro('ChaveAcesso'      , RespostaFiscal.ChaveAcesso, tcStr);
      Parametro.GerarParametro('Nsu'              , RespostaFiscal.Nsu, tcStr);
      Parametro.GerarParametro('NumerodeAprovacao', RespostaFiscal.NumerodeAprovacao, tcStr);
      Parametro.GerarParametro('Bandeira'         , RespostaFiscal.Bandeira, tcStr);
      Parametro.GerarParametro('Adquirente'       , RespostaFiscal.Adquirente, tcStr);
      Parametro.GerarParametro('CNPJ'             , RespostaFiscal.CNPJ, tcStr);
      Parametro.GerarParametro('ImpressaoFiscal'  , RespostaFiscal.ImpressaoFiscal, tcStr);
      Parametro.GerarParametro('NumeroDocumento'  , RespostaFiscal.NumeroDocumento, tcStr);
    finally
      Parametro.Free;
    end;

    Metodo.AdicionarParametros := True;
    Metodo.FinalizarMetodo;
  finally
    Metodo.Free;
  end;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

{ TVerificarStatusValidadorW }

function TVerificarStatusValidadorW.GetOpcoes: TGeradorOpcoes;
begin
  Result := FGerador.Opcoes;
end;

constructor TVerificarStatusValidadorW.Create(AOwner: TVerificarStatusValidador);
begin
  FVerificarStatusValidador := AOwner;
  FGerador := TGerador.Create;
end;

destructor TVerificarStatusValidadorW.Destroy;
begin
  FGerador.Free;
  inherited Destroy;
end;

function TVerificarStatusValidadorW.GerarXml: boolean;
var
  Metodo : TMetodo;
  Construtor : TConstrutor;
  Parametro: TParametro;
begin
  Gerador.LayoutArquivoTXT.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  Metodo := TMetodo.Create(Gerador);
  try
    Metodo.AdicionarParametros := False;
    Metodo.GerarMetodo(VerificarStatusValidador.Identificador,'VFP-e','VerificarStatusValidador');

    Construtor := TConstrutor.Create(Gerador);
    try
      Construtor.GerarConstructor('chaveAcessoValidador', VerificarStatusValidador.ChaveAcessoValidador);
    finally
      Construtor.Free;
    end;

    Gerador.wGrupo('Parametros');

    Parametro := TParametro.Create(Gerador);
    try
      Parametro.GerarParametro('idFila' , VerificarStatusValidador.IDFila, tcInt);
      Parametro.GerarParametro('cnpj'   , VerificarStatusValidador.CNPJ, tcStr);
    finally
      Parametro.Free;
    end;

    Metodo.AdicionarParametros := True;
    Metodo.FinalizarMetodo;
  finally
    Metodo.Free;
  end;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

{ TEnviarPagamentoW }

constructor TEnviarPagamentoW.Create(AOwner: TEnviarPagamento);
begin
  FEnviarPagamento := AOwner;
  FGerador := TGerador.Create;
end;

destructor TEnviarPagamentoW.Destroy;
begin
  FGerador.Free;
  inherited Destroy;
end;

function TEnviarPagamentoW.GetOpcoes: TGeradorOpcoes;
begin
  Result := FGerador.Opcoes;
end;

function TEnviarPagamentoW.GerarXml(): boolean;
var
  Metodo : TMetodo;
  Construtor : TConstrutor;
  Parametro: TParametro;
begin
  Gerador.LayoutArquivoTXT.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  Metodo := TMetodo.Create(Gerador);
  try
    Metodo.AdicionarParametros := False;
    Metodo.GerarMetodo(EnviarPagamento.Identificador,'VFP-e','EnviarPagamento');

    Construtor := TConstrutor.Create(Gerador);
    try
      Construtor.GerarConstructor('chaveAcessoValidador', EnviarPagamento.ChaveAcessoValidador);
    finally
      Construtor.Free;
    end;

    Gerador.wGrupo('Parametros');

    Parametro := TParametro.Create(Gerador);
    try
      Parametro.GerarParametro('ChaveRequisicao'  , EnviarPagamento.ChaveRequisicao, tcStr);
      Parametro.GerarParametro('Estabelecimento'  , EnviarPagamento.Estabelecimento, tcStr);
      Parametro.GerarParametro('SerialPos'        , EnviarPagamento.SerialPOS, tcStr);
      Parametro.GerarParametro('Cnpj'             , EnviarPagamento.CNPJ, tcStr);
      Parametro.GerarParametro('IcmsBase'         , EnviarPagamento.IcmsBase, tcDe2);
      if EnviarPagamento.ChaveAcessoValidador = cChaveAcessoIntegrador then
        Parametro.GerarParametro('ValorTotalVenda'  ,FloatToString( EnviarPagamento.ValorTotalVenda, ',' ), tcStr)
      else
        Parametro.GerarParametro('ValorTotalVenda'  , EnviarPagamento.ValorTotalVenda, tcDe2);
      Parametro.GerarParametro('HabilitarMultiplosPagamentos', EnviarPagamento.HabilitarMultiplosPagamentos, tcBoolStr);
      Parametro.GerarParametro('HabilitarControleAntiFraude' , EnviarPagamento.HabilitarControleAntiFraude, tcBoolStr);
      Parametro.GerarParametro('CodigoMoeda'      , EnviarPagamento.CodigoMoeda, tcStr);
      Parametro.GerarParametro('EmitirCupomNFCE'  , EnviarPagamento.EmitirCupomNFCE, tcBoolStr);
      Parametro.GerarParametro('OrigemPagamento'  , EnviarPagamento.OrigemPagamento, tcStr);
    finally
      Parametro.Free;
    end;

    Metodo.AdicionarParametros := True;
    Metodo.FinalizarMetodo;
  finally
    Metodo.Free;
  end;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

