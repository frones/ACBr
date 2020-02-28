{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti e Isaque Pinheiro            }
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

unit ACBrECFBloco_Q;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroQ100List = class;

  /// Registro Q001 - Abertura do Bloco Q – Livro Caixa
  TRegistroQ001 = class(TOpenBlocos)
  private
    FRegistroQ100         : TRegistroQ100List; // NIVEL 2
  public
    property RegistroQ100: TRegistroQ100List read FRegistroQ100 write FRegistroQ100;
  end;

  /// Registro Q100 - Demonstrativo do Livro Caixa

  { TRegistroQ100 }

  TRegistroQ100 = class(TBlocos)
  private
    fVL_SAIDA: Variant;
    fNUM_DOC: String;
    fHIST: String;
    fVL_ENTRADA: Variant;
    fDATA: TDateTime;
    fSLD_FIN: Variant;
  public
    property DATA: TDateTime read fDATA write fDATA;
    property NUM_DOC: String read fNUM_DOC write fNUM_DOC;
    property HIST: String read fHIST write fHIST;
    property VL_ENTRADA: Variant read fVL_ENTRADA write fVL_ENTRADA;
    property VL_SAIDA: Variant read fVL_SAIDA write fVL_SAIDA;
    property SLD_FIN: Variant read fSLD_FIN write fSLD_FIN;
  end;

  /// Registro Q100 - Lista

  TRegistroQ100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroQ100;
    procedure SetItem(Index: Integer; const Value: TRegistroQ100);
  public
    function New: TRegistroQ100;
    property Items[Index: Integer]: TRegistroQ100 read GetItem write SetItem;
  end;

  /// Registro Q990 - ENCERRAMENTO DO Bloco Q
  TRegistroQ990 = class(TCloseBlocos)
  end;

implementation

{ TRegistroQ100List }

function TRegistroQ100List.GetItem(Index: Integer): TRegistroQ100;
begin
   Result := TRegistroQ100(Inherited Items[Index]);
end;

function TRegistroQ100List.New: TRegistroQ100;
begin
  Result := TRegistroQ100.Create;
  Add(Result);
end;

procedure TRegistroQ100List.SetItem(Index: Integer; const Value: TRegistroQ100);
begin
  Put(Index, Value);
end;


end.

