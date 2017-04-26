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

unit pcnEnviarPagamentoR;

interface uses

  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnEnviarPagamento;

type

  TEnviarPagamentoR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FEnviarPagamento: TEnviarPagamento;
  public
    constructor Create(AOwner: TEnviarPagamento);
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property EnviarPagamento: TEnviarPagamento read FEnviarPagamento write FEnviarPagamento;
  end;

  ////////////////////////////////////////////////////////////////////////////////

implementation

uses ACBrConsts;

{ TCFeR }

constructor TEnviarPagamentoR.Create(AOwner: TEnviarPagamento);
begin
  FLeitor := TLeitor.Create;
  FEnviarPagamento := AOwner;
end;

destructor TEnviarPagamentoR.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TEnviarPagamentoR.LerXml: boolean;
var
  ok: boolean;
begin
  Result := False;
  EnviarPagamento.Clear;
  ok := true;

  if Leitor.rExtrai(1, 'Integrador') <> '' then
  begin
    EnviarPagamento.Identificador        := Leitor.rCampo(tcStr, 'identificador');
    EnviarPagamento.ChaveAcessoValidador := Leitor.rCampo(tcStr, 'chaveAcessoValidador');
    EnviarPagamento.ChaveRequisicao      := Leitor.rCampo(tcStr, 'chaveRequisicao');
    EnviarPagamento.Estabelecimento      := Leitor.rCampo(tcStr, 'Estabelecimento');
    EnviarPagamento.CNPJ                 := Leitor.rCampo(tcStr, 'CNPJ');
    EnviarPagamento.SerialPOS            := Leitor.rCampo(tcStr, 'SerialPOS');
    EnviarPagamento.ValorOperacaoSujeitaICMS := Leitor.rCampo(tcDe2, 'ValorOperacaoSujeitaICMS');
    EnviarPagamento.ValorTotalVenda      := Leitor.rCampo(tcDe2, 'ValorTotalVenda');
  end ;

  Result := True;
end;

end.
