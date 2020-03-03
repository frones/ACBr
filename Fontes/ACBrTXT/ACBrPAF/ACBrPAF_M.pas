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
unit ACBrPAF_M;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  //M2 - Cupom de embarque
  TRegistroM2 = class
  private
    fCNPJ: string;                   // CNPJ da matriz da empresa do serviço de transporte
    fIE: string;                     // Inscrição Estadual da empresa do serviço de transporte
    fIM: string;                     // Inscrição Municipal da empresa do serviço de transporte
    fNUM_FAB: string;                // Número de fabricação do ECF
    fMF_ADICIONAL: string;           // Letra indicativa de MF adicional
    fTIPO_ECF: string;               // Tipo de ECF
    fMARCA_ECF: string;              // Marca do ECF
    fMODELO_ECF: string;             // Modelo do ECF
    fNUM_USU: integer;               // Nº de ordem do usuário do ECF
    fCCF: Integer;                   // CCF
    fCOO: Integer;                   // COO do DAV
    fDT_EMI: TDateTime;              // Data e hora de emissão do bilhete de passagem
    fCOD_MOD: String;                // Código da modalidade do transporte
    fCOD_CAT: String;                // Código da categoria do transporte
    fID_LINHA: string;               // Identificação da linha
    fCOD_ORIG: string;               // Codigo do ponto de origem da prestação do serviço
    fCOD_DEST: string;               // Codigo do ponto de destino da prestação do serviço
    fCOD_TSER: string;               // Codigo do tipo de serviço
    fDT_VIA: TDateTime;              // Data e hora da viagem
    fTIP_VIA: String;                // Tipo de viagem
    fPOLTRONA: Integer;              // Nº da poltrona
    fPLATAFORMA: string;             // Plataforma de embarque
    fCOD_DESC: string;               // Código do desconto
    fVL_TARIFA: Currency;            // Valor da tarifa
    fALIQ: currency;                 // Alíquota de ICMS
    fVL_PEDAGIO: Currency;           // Valor do pedágio
    fVL_TAXA: Currency;              // Valor da taxa de embarque
    fVL_TOTAL: Currency;             // Valor total
    fFORM_PAG: String;               // Forma de pagamento
    fVL_PAGO: Currency;              // Valor pago
    fNOME_PAS: string;               // Nome do passageiro
    fNDOC_PAS: string;               // Nº documento do passageiro
    fSAC: string;                    // Nº SAC
    fAGENCIA: string;                // Razão social da agencia emissora do bilhete
  public
    property CNPJ: string         read fCNPJ         write fCNPJ;
    property IE: string           read fIE           write FIE;
    property IM: string           read FIM           write FIM;
    property NUM_FAB: string      read fNUM_FAB      write fNUM_FAB;
    property MF_ADICIONAL: string read fMF_ADICIONAL write fMF_ADICIONAL;
    property TIPO_ECF: string     read fTIPO_ECF     write fTIPO_ECF;
    property MARCA_ECF: string    read fMARCA_ECF    write fMARCA_ECF;
    property MODELO_ECF: string   read fMODELO_ECF   write fMODELO_ECF;
    property NUM_USU: integer     read fNUM_USU      write fNUM_USU;
    property COO: integer         read fCOO          write fCOO;
    property CCF: integer         read fCCF          write fCCF;
    property DT_EMI: TDateTime    read fDT_EMI       write fDT_EMI;
    property COD_MOD: String      read fCOD_MOD      write fCOD_MOD;
    property COD_CAT: String      read fCOD_CAT      write fCOD_CAT;
    property ID_LINHA: String     read fID_LINHA     write fID_LINHA;
    property COD_ORIG: String     read fCOD_ORIG     write fCOD_ORIG;
    property COD_DEST: String     read fCOD_DEST     write fCOD_DEST;
    property COD_TSER: string     read fCOD_TSER     write fCOD_TSER;
    property DT_VIA: TDateTime    read fDT_VIA       write fDT_VIA;
    property TIP_VIA: String      read fTIP_VIA      write fTIP_VIA;
    property POLTRONA: Integer    read fPOLTRONA     write fPOLTRONA;
    property PLATAFORMA: string   read fPLATAFORMA   write fPLATAFORMA;
    property COD_DESC: string     read fCOD_DESC     write fCOD_DESC;
    property VL_TARIFA: Currency  read fVL_TARIFA    write fVL_TARIFA;
    property ALIQ: Currency       read fALIQ         write fALIQ;
    property VL_PEDAGIO: Currency read fVL_PEDAGIO   write fVL_PEDAGIO;
    property VL_TAXA: Currency    read fVL_TAXA      write fVL_TAXA;
    property VL_TOTAL: Currency   read fVL_TOTAL     write fVL_TOTAL;
    property FORM_PAG: String     read fFORM_PAG     write fFORM_PAG;
    property VL_PAGO: Currency    read fVL_PAGO      write fVL_PAGO;
    property NOME_PAS: string     read fNOME_PAS     write fNOME_PAS;
    property NDOC_PAS: string     read fNDOC_PAS     write fNDOC_PAS;
    property SAC: string          read fSAC          write fSAC;
    property AGENCIA: string      read fAGENCIA      write fAGENCIA;
  end;

  TRegistroM2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM2;
    procedure SetItem(Index: Integer; const Value: TRegistroM2);
  public
    function New: TRegistroM2;
    property Items[Index: Integer]: TRegistroM2 read GetItem write SetItem;
  end;

  
implementation

{ TRegistroM2List }
function TRegistroM2List.GetItem(Index: Integer): TRegistroM2;
begin
  Result := TRegistroM2(inherited Items[Index]);
end;

function TRegistroM2List.New: TRegistroM2;
begin
  Result := TRegistroM2.Create;
  Add(Result);
end;

procedure TRegistroM2List.SetItem(Index: Integer;
  const Value: TRegistroM2);
begin
  Put(Index, Value);
end;

end.
