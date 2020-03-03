{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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
unit ACBrPAF_A;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  //A2 - Total diário de meios de pagamento
  TRegistroA2 = class
  private
    FRegistroValido: Boolean;
    FDT: TDateTime;           //Data do movimento
    FMEIO_PGTO: string;       //Meio de pagamento registrado nos documentos emitidos
    FTIPO_DOC: string;        //Código do tipo de documento a que se refere o pagamento, conforme tabela descrita no item 6.2.1.2
    FVL: Currency;            //Valor total, com duas casas decimais
  public
    constructor Create; virtual;

    property RegistroValido: Boolean read FRegistroValido write FRegistroValido;
    property DT: TDateTime           read FDT             write FDT;
    property MEIO_PGTO: string       read FMEIO_PGTO      write FMEIO_PGTO;
    property TIPO_DOC: string        read FTIPO_DOC       write FTIPO_DOC;
    property VL: Currency            read FVL             write FVL;
  end;

  TRegistroA2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroA2;
    procedure SetItem(Index: Integer; const Value: TRegistroA2);
  public
    function New: TRegistroA2;
    property Items[Index: Integer]: TRegistroA2 read GetItem write SetItem; default;
  end;

  
implementation

{ TRegistroA2 }  
constructor TRegistroA2.Create;
begin
  fRegistroValido := True;
end;

{ TRegistroA2List }
function TRegistroA2List.GetItem(Index: Integer): TRegistroA2;
begin
  Result := TRegistroA2(inherited Items[Index]);
end;

function TRegistroA2List.New: TRegistroA2;
begin
  Result := TRegistroA2.Create;
  Add(Result);
end;

procedure TRegistroA2List.SetItem(Index: Integer;
  const Value: TRegistroA2);
begin
  Put(Index, Value);
end;

end.
