{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Macgayver Armini Apolonio e Rodrigo Buschmann   }
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

unit ACBrEFDBloco_0_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEFDBase,
  ACBrUtil.Strings, ACBrSpedFiscal, ACBrEFDBlocos;

type
  TACBrSpedFiscalImportar_Bloco0 = class(TACBrSpedFiscalImportar_Base)
  private
    procedure Reg0000;
    procedure Reg0001;
    procedure Reg0002;
    procedure Reg0005;
    procedure Reg0015;
    procedure Reg0100;
    procedure Reg0150;
    procedure Reg0175;
    procedure Reg0190;
    procedure Reg0200;
    procedure Reg0205;
    procedure Reg0206;
    procedure Reg0220;
    procedure Reg0300;
    Procedure Reg0305;
    procedure Reg0400;
    procedure Reg0450;
    procedure Reg0460;
    procedure Reg0500;
    procedure Reg0600;
  public
    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

procedure TACBrSpedFiscalImportar_Bloco0.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = '0000') then Reg0000
  else if (vHead = '0001') then Reg0001
  else if (vHead = '0002') then Reg0002
  else if (vHead = '0005') then Reg0005
  else if (vHead = '0015') then Reg0015
  else if (vHead = '0100') then Reg0100
  else if (vHead = '0150') then Reg0150
  else if (vHead = '0175') then Reg0175
  else if (vHead = '0190') then Reg0190
  else if (vHead = '0200') then Reg0200
  else if (vHead = '0205') then Reg0205
  else if (vHead = '0206') then Reg0206
  else if (vHead = '0220') then Reg0220
  else if (vHead = '0300') then Reg0300
  else if (vHead = '0305') then Reg0305
  else if (vHead = '0400') then Reg0400
  else if (vHead = '0450') then Reg0450
  else if (vHead = '0460') then Reg0460
  else if (vHead = '0500') then Reg0500
  else if (vHead = '0600') then Reg0600;
end;

// identificação da entidade
procedure TACBrSpedFiscalImportar_Bloco0.Reg0000;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0000New do
  begin
    COD_VER := StrToCodVer(Valor);
    COD_FIN := StrToCodFin(Valor);
    DT_INI := ValorD;
    DT_FIN := ValorD;
    NOME := Valor;
    CNPJ := Valor;
    CPF := Valor;
    UF := Valor;
    IE := Valor;
    COD_MUN := ValorI;
    IM := Valor;
    SUFRAMA := Valor;
    IND_PERFIL := StrToIndPerfil(Valor);
    IND_ATIV := StrToIndAtiv(Valor);
  end;
end;

// abertura do bloco 0
procedure TACBrSpedFiscalImportar_Bloco0.Reg0001;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco0.Reg0002;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0002New do
  begin
    CLAS_ESTAB_IND := Valor;
  end;
end;

// dados complementares da entidade
procedure TACBrSpedFiscalImportar_Bloco0.Reg0005;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0005New do
  begin
    FANTASIA := Valor;
    CEP := Valor;
    ENDERECO := Valor;
    NUM := Valor;
    COMPL := Valor;
    BAIRRO := Valor;
    FONE := Valor;
    FAX := Valor;
    EMAIL := Valor;
  end;
end;

// dados do contribuinte substituto
procedure TACBrSpedFiscalImportar_Bloco0.Reg0015;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0015New do
  begin
    UF_ST := Valor;
    IE_ST := Valor;
  end;
end;

// dados do contabilista
procedure TACBrSpedFiscalImportar_Bloco0.Reg0100;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0100New do
  begin
    NOME := Valor;
    CPF := Valor;
    CRC := Valor;
    CNPJ := Valor;
    CEP := Valor;
    ENDERECO := Valor;
    NUM := Valor;
    COMPL := Valor;
    BAIRRO := Valor;
    FONE := Valor;
    FAX := Valor;
    EMAIL := Valor;
    COD_MUN := ValorI;
  end;
end;

// cadastro do participante
procedure TACBrSpedFiscalImportar_Bloco0.Reg0150;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0150New do
  begin
    COD_PART := Valor;
    NOME := Valor;
    COD_PAIS := Valor;
    CNPJ := Valor;
    CPF := Valor;
    IE := Valor;
    COD_MUN := ValorI;
    SUFRAMA := Valor;
    ENDERECO := Valor;
    NUM := Valor;
    COMPL := Valor;
    BAIRRO := Valor;
  end;
end;

// alteração da tabela de cadastro de participante
procedure TACBrSpedFiscalImportar_Bloco0.Reg0175;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0175New do
  begin
    DT_ALT := ValorD;
    NR_CAMPO := Valor;
    CONT_ANT := Valor;
  end;
end;

// identificação das unidades de medida
procedure TACBrSpedFiscalImportar_Bloco0.Reg0190;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0190New do
  begin
    UNID := Valor;
    DESCR := Valor;
  end;
end;

// identificação do item
procedure TACBrSpedFiscalImportar_Bloco0.Reg0200;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0200New do
  begin
    COD_ITEM := Valor;
    DESCR_ITEM := Valor;
    COD_BARRA := Valor;
    COD_ANT_ITEM := Valor;
    UNID_INV := Valor;
    TIPO_ITEM := StrToTipoItem(Valor);
    COD_NCM := Valor;
    EX_IPI := Valor;
    COD_GEN := Valor;
    COD_LST := Valor;
    ALIQ_ICMS := ValorFV;
    CEST := Valor;
  end;
end;

// alteração do item
procedure TACBrSpedFiscalImportar_Bloco0.Reg0205;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0205New do
  begin
    DESCR_ANT_ITEM := Valor;
    DT_INI := ValorD;
    DT_FIN := ValorD;
    COD_ANT_ITEM := Valor;
  end;
end;

// código ANP
procedure TACBrSpedFiscalImportar_Bloco0.Reg0206;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0206New do
  begin
    COD_COMB := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco0.Reg0220;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0220New do
  begin
    UNID_CONV := Valor;
    FAT_CONV := ValorFV;
  end;
end;

// cadastro de bens ou componentes do ativo imobilizado
procedure TACBrSpedFiscalImportar_Bloco0.Reg0300;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0300New do
  begin
    COD_IND_BEM := Valor;
    IDENT_MERC  := ValorI;
    DESCR_ITEM  := Valor; 
    COD_PRNC    := Valor;   
    COD_CTA     := Valor;    
    NR_PARC     := ValorF;    
  end;
end;

// informações sobre a utilização do bem
procedure TACBrSpedFiscalImportar_Bloco0.Reg0305;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0305New do
  begin
    COD_CCUS  := Valor; 
    FUNC      := Valor;      
    VIDA_UTIL := ValorI;
  end;
end;

// natureza de operação/prestação
procedure TACBrSpedFiscalImportar_Bloco0.Reg0400;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0400New do
  begin
    COD_NAT := Valor;
    DESCR_NAT := Valor;
  end;
end;

// inf complementar doc fiscal
procedure TACBrSpedFiscalImportar_Bloco0.Reg0450;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0450New do
  begin
    COD_INF := Valor;
    TXT := Valor;
  end;
end;

//Tabela de Observações do Lançamento Fiscal
procedure TACBrSpedFiscalImportar_Bloco0.Reg0460;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0460New do
  begin
    COD_OBS := Valor;
    TXT := Valor;
  end;
end;

//plano de contas
procedure TACBrSpedFiscalImportar_Bloco0.Reg0500;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0500New do
  begin
    DT_ALT := ValorD;
    COD_NAT_CC := Valor;
    IND_CTA := Valor;
    NIVEL := Valor;
    COD_CTA := Valor;
    NOME_CTA := Valor;
  end;
end;

// centro de custo
procedure TACBrSpedFiscalImportar_Bloco0.Reg0600;
begin
  with ACBrSpedFiscal.Bloco_0.Registro0600New do
  begin
    DT_ALT := ValorD;
    COD_CCUS := Valor;
    CCUS := Valor;
  end;
end;

end.
