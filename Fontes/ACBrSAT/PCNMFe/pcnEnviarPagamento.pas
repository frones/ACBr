{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 André Ferreira de Moraes               }
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

unit pcnEnviarPagamento
;

interface

uses
  SysUtils, Classes,
  pcnConversao;

type

  { TEnviarPagamento }

  TEnviarPagamento = class(TPersistent)
  private
    FIdentificador: String;
    FChaveAcessoValidador: String;
    FChaveRequisicao: String;
    FEstabelecimento: String;
    FSerialPOS: String;
    FCNPJ: String;
    FValorOperacaoSujeitaICMS: Currency;
    FValorTotalVenda: Currency;	 
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear ;

    function LoadFromFile(AFileName : String): boolean;
    function SaveToFile(AFileName : String; ApenasTagsAplicacao: Boolean = false): boolean;
    function GetXMLString: AnsiString;
    procedure SetXMLString(AValue : AnsiString) ;

    property AsXMLString : AnsiString read GetXMLString write SetXMLString ;
  published
        property Identificador: String read FIdentificador write FIdentificador;
        property ChaveAcessoValidador: String read FChaveAcessoValidador write FChaveAcessoValidador;
        property ChaveRequisicao: String read FChaveRequisicao write FChaveRequisicao;
        property Estabelecimento: String read FEstabelecimento write FEstabelecimento;
        property SerialPOS: String read FSerialPOS write FSerialPOS;
        property CNPJ: String read FCNPJ write FCNPJ;
        property ValorOperacaoSujeitaICMS: Currency read FValorOperacaoSujeitaICMS write FValorOperacaoSujeitaICMS;
        property ValorTotalVenda: Currency read FValorTotalVenda write FValorTotalVenda;   		
   
  end;

implementation

Uses pcnEnviarPagamentoW, pcnEnviarPagamentoR ;

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
  FIdentificador := '';
  FChaveAcessoValidador := '';
  FChaveRequisicao := '';
  FEstabelecimento := '';
  FSerialPOS := '';
  FCNPJ := '';
  FValorOperacaoSujeitaICMS := 0;
  FValorTotalVenda := 0;	
end;

function TEnviarPagamento.LoadFromFile(AFileName: String): boolean;
var
  SL : TStringList;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.LoadFromFile( AFileName );
    AsXMLString := SL.Text;
    Result      := True;
  finally
    SL.Free;
  end;
end;

function TEnviarPagamento.SaveToFile(AFileName: String; ApenasTagsAplicacao: Boolean
  ): boolean;
var
  SL : TStringList;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Text := GetXMLString;
    SL.SaveToFile( AFileName );
    Result := True;
  finally
    SL.Free;
  end;
end;

function TEnviarPagamento.GetXMLString(): AnsiString;
var
  LocEnviarPagamentoW : TEnviarPagamentoW ;
begin
  Result  := '';
  LocEnviarPagamentoW := TEnviarPagamentoW.Create(Self);
  try
    LocEnviarPagamentoW.Gerador.Opcoes.IdentarXML := True;
    LocEnviarPagamentoW.Gerador.Opcoes.TamanhoIdentacao := 3;

    LocEnviarPagamentoW.GerarXml();
    Result := LocEnviarPagamentoW.Gerador.ArquivoFormatoXML;
  finally
    LocEnviarPagamentoW.Free;
  end ;
end;

procedure TEnviarPagamento.SetXMLString(AValue: AnsiString);
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

end.
 
