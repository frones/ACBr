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
unit ACBrIntegradorResposta;

interface

uses
  Classes, SysUtils, pcnLeitor, pcnConversao;

type

  { TIntegradorResposta }

  TIntegradorResposta = class(TPersistent)
  private
    FLeitor: TLeitor;
    FIdentificador: Integer;
    FCodigo: String;
    FValor: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure LerResposta( XML : String );
  published
    property Identificador: Integer read FIdentificador write FIdentificador;
    property Codigo: String read FCodigo write FCodigo;
    property Valor: String read FValor write FValor;
  end;

implementation

{ TIntegradorResposta }

constructor TIntegradorResposta.Create;
begin
  FLeitor := TLeitor.Create;
end;

destructor TIntegradorResposta.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

procedure TIntegradorResposta.Clear;
begin
  FIdentificador := 0;
  FCodigo        := '';
  FValor         := '';
end;

procedure TIntegradorResposta.LerResposta(XML: String);
begin
  FLeitor.Arquivo := XML;

  if FLeitor.rExtrai(1, 'Integrador') <> '' then
  begin
    if FLeitor.rExtrai(2, 'Identificador') <> '' then
      FIdentificador := FLeitor.rCampo(tcInt, 'valor');

    if FLeitor.rExtrai(2, 'IntegradorResposta') <> '' then
    begin
      FCodigo := FLeitor.rCampo(tcStr, 'Codigo');
      FValor  := FLeitor.rCampo(tcStr, 'Valor');
    end;
  end ;
end;

end.

