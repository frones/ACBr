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

unit ACBrPAF_H;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO H1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF

  TRegistroH1 = class(TRegistroX1)
  private
    fDT_EST: TDateTime;
    fMODELO_ECF: string;
    fMARCA_ECF: string;
    fTIPO_ECF: string;
    fNUM_FAB: string;
    fMF_ADICIONAL: string;
    fRegistroValido: Boolean;
  public
    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_FAB: string read fNUM_FAB write fNUM_FAB;
    property MF_ADICIONAL: string read fMF_ADICIONAL write fMF_ADICIONAL;
    property TIPO_ECF: string read fTIPO_ECF write fTIPO_ECF;
    property MARCA_ECF: string read fMARCA_ECF write fMARCA_ECF;
    property MODELO_ECF: string read fMODELO_ECF write fMODELO_ECF;
    property DT_EST: TDateTime read fDT_EST write fDT_EST;
  end;

  /// REGISTRO TIPO H2 - TROCO EM CARTÃO CONFORME ANEXO XV

  TRegistroH2 = class
  private
    fRegistroValido: boolean;
    fCNPJ_CRED_CARTAO : string; /// Identificação da credenciadora do cartão
    fNUM_FAB: string;                // Número de fabricação do ECF
    fMF_ADICIONAL: string;           // Letra indicativa de MF adicional
    fTIPO_ECF: string;               // Tipo de ECF
    fMARCA_ECF: string;              // Marca do ECF
    fMODELO_ECF: string;             // Modelo do ECF
    fCOO: Integer;     /// Contador de Ordem de Operação do Cupom Fiscal onde o troco foi registrado
    fCCF: Integer;    /// Nº do contador do respectivo documento emitido
    fVLR_TROCO: Currency;       /// Valor do troco, para o meio de pagamento cartão de crédito ou débito
    fDT_TROCO: TDateTime;   /// Data da emissão do Cupom Fiscal
    fCPF : string; /// CPF do adquirente do título de captalização
    fTitulo : Integer; /// Nº do título de captalização adquirido
    FCNPJ: string; /// CNPJ da entidade recebedora da doação
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;

    property CNPJ_CRED_CARTAO: string read fCNPJ_CRED_CARTAO write fCNPJ_CRED_CARTAO;
    property NUM_FAB: string      read fNUM_FAB      write fNUM_FAB;
    property MF_ADICIONAL: string read fMF_ADICIONAL write fMF_ADICIONAL;
    property TIPO_ECF: string     read fTIPO_ECF     write fTIPO_ECF;
    property MARCA_ECF: string    read fMARCA_ECF    write fMARCA_ECF;
    property MODELO_ECF: string   read fMODELO_ECF   write fMODELO_ECF;
    property COO: Integer read fCOO write fCOO;
    property CCF: Integer read fCCF write fCCF;
    property VLR_TROCO: Currency read fVLR_TROCO write fVLR_TROCO;
    property DT_TROCO: TDateTime read fDT_TROCO write fDT_TROCO;
    property CPF: string read fCPF write fCPF;
    property Titulo: Integer read fTitulo write fTitulo;
    property CNPJ: string read FCNPJ write FCNPJ;
  end;

  /// REGISTRO H2 - Lista

  TRegistroH2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroH2;
    procedure SetItem(Index: Integer; const Value: TRegistroH2);
  public
    function New: TRegistroH2;
    property Items[Index: Integer]: TRegistroH2 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO H9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroH9 = class(TRegistroX9)
  end;

implementation

(* TRegistroH2List *)

function TRegistroH2List.GetItem(Index: Integer): TRegistroH2;
begin
  Result := TRegistroH2(inherited Items[Index]);
end;

function TRegistroH2List.New: TRegistroH2;
begin
  Result := TRegistroH2.Create;
  Add(Result);
end;

procedure TRegistroH2List.SetItem(Index: Integer; const Value: TRegistroH2);
begin
  Put(Index, Value);
end;

{ TRegistroH2 }

constructor TRegistroH2.Create;
begin
   fRegistroValido := True;
end;

end.
