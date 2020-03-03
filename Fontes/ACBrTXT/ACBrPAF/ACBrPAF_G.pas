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
unit ACBrPAF_G;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  //G2 - Movimento diário de pedágio
  TRegistroG2 = class
  private
    fCNPJ: string;                   // CNPJ do estabelecimento usuário do ECF
    fNUM_FAB: string;                // Número de fabricação do ECF
    fMF_ADICIONAL: string;           // Letra indicativa de MF adicional
    fTIPO_ECF: string;               // Tipo de ECF
    fMARCA_ECF: string;              // Marca do ECF
    fMODELO_ECF: string;             // Modelo do ECF
    fNUM_CAB: Integer;               // Nº de identificação da cabine de pedagio
    fDT: TDateTime;                  // Data do movimento
    fCOO_INI: Integer;               // COO do primeiro documento emitido no dia
    fCOO_FIN: Integer;               // COO do ultimo documento emitido no dia
    fCCF_INI: Integer;               // CCF do primeiro documento emitido no dia
    fCCF_FIN: Integer;               // CCF do ultimo documento emitido no dia
    fVL_2EIX_SIMPLES: Currency;      // Valor total de pedagio 2 eixos rodagem simples (automovel,..)
    fVL_2EIX_SIMPLES_MOTO: Currency; // Valor total de pedagio 2 eixos rodagem simples (Motos, bicicletas a motor)
    fVL_2EIX_DUPLA: Currency;        // Valor total de pedagio 2 eixos rodagem dupla
    fVL_3EIX_SIMPLES: Currency;      // Valor total de pedagio 3 eixos rodagem simples
    fVL_3EIX_DUPLA: Currency;        // Valor total de pedagio 3 eixos rodagem dupla
    fVL_4EIX_SIMPLES: Currency;      // Valor total de pedagio 4 eixos rodagem simples
    fVL_4EIX_DUPLA: Currency;        // Valor total de pedagio 4 eixos rodagem dupla
    fVL_5EIX_DUPLA: Currency;        // Valor total de pedagio 5 eixos rodagem dupla
    fVL_6EIX_DUPLA: Currency;        // Valor total de pedagio 6 eixos rodagem dupla
    fVL_OUTROS: Currency;            // Valor veiculos não enquadrados nos campos anteriores
    fVL_TOTAL_DIA: Currency;         // Valor total do dia
    fQTDE_VEIC_ISENTO: Integer;      // Quantidade de veiculos isentos de pedagio
    fLOCALIZACAO: String;            // Localização do pedagio (Rodovia, KM e Municipio)
  public
    property CNPJ: string         read fCNPJ         write fCNPJ;
    property NUM_FAB: string      read fNUM_FAB      write fNUM_FAB;
    property MF_ADICIONAL: string read fMF_ADICIONAL write fMF_ADICIONAL;
    property TIPO_ECF: string     read fTIPO_ECF     write fTIPO_ECF;
    property MARCA_ECF: string    read fMARCA_ECF    write fMARCA_ECF;
    property MODELO_ECF: string   read fMODELO_ECF   write fMODELO_ECF;
    property NUM_CAB: Integer     read fNUM_CAB      write fNUM_CAB;
    property DT: TDateTime        read fDT           write fDT;
    property COO_INI: integer     read fCOO_INI      write fCOO_INI;
    property COO_FIN: integer     read fCOO_FIN      write fCOO_FIN;
    property CCF_INI: integer     read fCCF_INI      write fCCF_INI;
    property CCF_FIN: integer     read fCCF_FIN      write fCCF_FIN;
    property VL_2EIX_SIMPLES: Currency      read fVL_2EIX_SIMPLES      write fVL_2EIX_SIMPLES;
    property VL_2EIX_SIMPLES_MOTO: Currency read fVL_2EIX_SIMPLES_MOTO write fVL_2EIX_SIMPLES_MOTO;
    property VL_2EIX_DUPLA: Currency        read fVL_2EIX_DUPLA        write fVL_2EIX_DUPLA;
    property VL_3EIX_SIMPLES: Currency      read fVL_3EIX_SIMPLES      write fVL_3EIX_SIMPLES;
    property VL_3EIX_DUPLA: Currency        read fVL_3EIX_DUPLA        write fVL_3EIX_DUPLA;
    property VL_4EIX_SIMPLES: Currency      read fVL_4EIX_SIMPLES      write fVL_4EIX_SIMPLES;
    property VL_4EIX_DUPLA: Currency        read fVL_4EIX_DUPLA        write fVL_4EIX_DUPLA;
    property VL_5EIX_DUPLA: Currency        read fVL_5EIX_DUPLA        write fVL_5EIX_DUPLA;
    property VL_6EIX_DUPLA: Currency        read fVL_6EIX_DUPLA        write fVL_6EIX_DUPLA;
    property VL_OUTROS: Currency            read fVL_OUTROS            write fVL_OUTROS;
    property VL_TOTAL_DIA: Currency         read fVL_TOTAL_DIA         write fVL_TOTAL_DIA;
    property QTDE_VEIC_ISENTO: Integer      read fQTDE_VEIC_ISENTO     write fQTDE_VEIC_ISENTO;
    property LOCALIZACAO: string            read fLOCALIZACAO          write fLOCALIZACAO;
  end;

  TRegistroG2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG2;
    procedure SetItem(Index: Integer; const Value: TRegistroG2);
  public
    function New: TRegistroG2;
    property Items[Index: Integer]: TRegistroG2 read GetItem write SetItem;
  end;

  
implementation

{ TRegistroG2List }
function TRegistroG2List.GetItem(Index: Integer): TRegistroG2;
begin
  Result := TRegistroG2(inherited Items[Index]);
end;

function TRegistroG2List.New: TRegistroG2;
begin
  Result := TRegistroG2.Create;
  Add(Result);
end;

procedure TRegistroG2List.SetItem(Index: Integer;
  const Value: TRegistroG2);
begin
  Put(Index, Value);
end;

end.
