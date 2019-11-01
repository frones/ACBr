{******************************************************************************}
{ Projeto: Componente ACBrLCDPR                                                }
{  Biblioteca multiplataforma de componentes Delphi para geração do LCDPR -    }
{ Lirvro Caixa Digital do Produtor Rural                                       }
{                                                                              }
{                                                                              }
{ Desenvolvimento e doação ao Projeto ACBr: Willian Hübner                     }
{                                                                              }
{ Ajustes e correções para doação: Elton Barbosa (EMBarbosa)                   }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
unit uDadosContador;

interface

type
  TContador = Class
  private
    FFONE: String;
    FEMAIL: String;
    FIDENT_NOME: String;
    FIND_CRC: String;
    FIDENT_CPF_CNPJ: String;
    procedure SetEMAIL(const Value: String);
    procedure SetFONE(const Value: String);
    procedure SetIDENT_CPF_CNPJ(const Value: String);
    procedure SetIDENT_NOME(const Value: String);
    procedure SetIND_CRC(const Value: String);
  public
    property IDENT_NOME : String read FIDENT_NOME write SetIDENT_NOME;
    property IDENT_CPF_CNPJ : String read FIDENT_CPF_CNPJ write SetIDENT_CPF_CNPJ;
    property IND_CRC : String read FIND_CRC write SetIND_CRC;
    property EMAIL : String read FEMAIL write SetEMAIL;
    property FONE : String read FFONE write SetFONE;
  End;

implementation

uses
  ACBrUtil;

{ TContador }

procedure TContador.SetEMAIL(const Value: String);
begin
  FEMAIL := Value;
end;

procedure TContador.SetFONE(const Value: String);
begin
  FFONE := OnlyNumber(Value);
end;

procedure TContador.SetIDENT_CPF_CNPJ(const Value: String);
begin
  FIDENT_CPF_CNPJ := Value;
end;

procedure TContador.SetIDENT_NOME(const Value: String);
begin
  FIDENT_NOME := Value;
end;

procedure TContador.SetIND_CRC(const Value: String);
begin
  FIND_CRC := Value;
end;

end.
