{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Carlos H. Marian                                }
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

unit ACBrPAF_Z;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type

  /// REGISTRO TIPO Z1 - IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF
  TRegistroZ1 = class(TRegistroX1);

  // REGISTRO TIPO Z2 - IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF:
  TRegistroZ2 = Class(TRegistroX1);

  // REGISTRO TIPO Z3 - IDENTIFICAÇÃO DO PAF-ECF
  TRegistroZ3 = Class(TRegistroX3);

  // REGISTRO TIPO Z4 – Totalização de vendas a CPF/CNPJ:
  TRegistroZ4 = Class
  private
    fCPF_CNPJ: string;         /// Número do CPF/CNPJ identificado no campo previsto no item 2 do Requsito VIII.
    fVl_Total: Currency ;  /// Total de vendas no mês, com duas casas decimais, ao CPF/CNPJ indicado no campo 02.
    fData_INI: TDateTime;  /// Primeiro dia do mês a que se refere o relatório de vendas ao CPF/CNPJ identificado no campo 02
    fData_FIN: TDateTime;  /// Último dia do mês a que se refere o relatório de vendas ao CPF/CNPJ identificado no campo 02
  public
    property CPF_CNPJ: string read FCPF_CNPJ write FCPF_CNPJ;
    property VL_TOTAL: Currency  read fVl_Total write fVl_Total;
    property DATA_INI: TDateTime read fData_INI write fData_INI;
    property DATA_FIM: TDateTime read fData_FIN write fData_FIN;
  end;

  /// REGISTRO TIPO Z4 – Lista
  TRegistroZ4List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroZ4;
    procedure SetItem(Index: Integer; const Value: TRegistroZ4);
  public
    function New: TRegistroZ4;
    property Items[Index: Integer]: TRegistroZ4 read GetItem write SetItem;
  end;

  // REGISTRO TIPO Z9 - TOTALIZAÇÃO DO ARQUIVO
  TRegistroZ9 = Class(TRegistroX9);

  // REGISTRO TIPO EAD - ASSINATURA DIGITAL

implementation

{ TRegistroZ4List }

function TRegistroZ4List.GetItem(Index: Integer): TRegistroZ4;
begin
  Result := TRegistroZ4(inherited Items[Index]);
end;

function TRegistroZ4List.New: TRegistroZ4;
begin
  Result := TRegistroZ4.Create;
  Add(Result);
end;

procedure TRegistroZ4List.SetItem(Index: Integer; const Value: TRegistroZ4);
begin
  Put(Index, Value);
end;

end.
