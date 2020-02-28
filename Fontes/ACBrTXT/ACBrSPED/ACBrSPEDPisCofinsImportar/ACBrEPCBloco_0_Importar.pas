{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Macgayver Armini Apolonio                       }
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

unit ACBrEPCBloco_0_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEPCBase,
  ACBrUtil, ACBrSpedPisCofins, ACBrEPCBlocos;

type
  TACBrSpedPCImportar_Bloco0 = class(TACBrSpedPCImportar_Base)
  private
    procedure Reg0000;
    procedure Reg0001;
    procedure Reg0035;
    procedure Reg0100;
    procedure Reg0110;
    procedure Reg0111;
    procedure Reg0120;
    procedure Reg0140;
    procedure Reg0145;
    procedure Reg0150;
    procedure Reg0190;
    procedure Reg0200;
    procedure Reg0205;
    procedure Reg0206;
    procedure Reg0208;
    procedure Reg0400;
    procedure Reg0450;
    procedure Reg0500;
    procedure Reg0600;
  public
    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

procedure TACBrSpedPCImportar_Bloco0.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = '0000') then
    Reg0000
  else if (vHead = '0001') then
    Reg0001
  else if (vHead = '0035') then
    Reg0035
  else if (vHead = '0100') then
    Reg0100
  else if (vHead = '0110') then
    Reg0110
  else if (vHead = '0111') then
    Reg0111
  else if (vHead = '0120') then
    Reg0120
  else if (vHead = '0140') then
    Reg0140
  else if (vHead = '0145') then
    Reg0145
  else if (vHead = '0150') then
    Reg0150
  else if (vHead = '0190') then
    Reg0190
  else if (vHead = '0200') then
    Reg0200
  else if (vHead = '0205') then
    Reg0205
  else if (vHead = '0206') then
    Reg0206
  else if (vHead = '0208') then
    Reg0208
  else if (vHead = '0400') then
    Reg0400
  else if (vHead = '0450') then
    Reg0450
  else if (vHead = '0500') then
    Reg0500
  else if (vHead = '0600') then
    Reg0600;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0000;
begin
  with ACBrSpedPisCofins.Bloco_0.Registro0000New do
  begin
    COD_VER := StrToCodVer(Valor);
    TIPO_ESCRIT := StrToTipoEscrit(Valor);
    IND_SIT_ESP := StrToIndSitEsp(Valor);
    NUM_REC_ANTERIOR := Valor;
    DT_INI := ValorD;
    DT_FIN := ValorD;
    NOME := Valor;
    CNPJ := Valor;
    UF := Valor;
    COD_MUN := ValorI;
    SUFRAMA := Valor;
    IND_NAT_PJ := StrToIndNatPJ(Valor);
    IND_ATIV := StrToIndAtiv(Valor);
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0001;

begin
  with ACBrSpedPisCofins.Bloco_0.Registro0001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0035;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0035New do
  begin
    COD_SCP := Valor;
    DESC_SCP := Valor;
    INF_COMP := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0100;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0100New do
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

procedure TACBrSpedPCImportar_Bloco0.Reg0110;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0110New do
  begin
    COD_INC_TRIB := StrToCodIncTrib(Valor);
    IND_APRO_CRED := StrToIndAproCred(Valor);
    COD_TIPO_CONT := StrToCodTipoCont(Valor);
    IND_REG_CUM := StrToIndRegCum(Valor);
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0111;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0111New do
  begin
    REC_BRU_NCUM_TRIB_MI := ValorF;
    REC_BRU_NCUM_NT_MI := ValorF;
    REC_BRU_NCUM_EXP := ValorF;
    REC_BRU_CUM := ValorF;
    REC_BRU_TOTAL := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0120;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0120New do
  begin
    MES_DISPENSA := Valor;
    INF_COMP := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0140;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0140New do
  begin
    COD_EST := Valor;
    NOME := Valor;
    CNPJ := Valor;
    UF := Valor;
    IE := Valor;
    COD_MUN := ValorI;
    IM := Valor;
    SUFRAMA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0145;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0145New do
  begin
    COD_INC_TRIB := Valor;
    VL_REC_TOT := ValorF;
    VL_REC_ATIV := ValorF;
    VL_REC_DEMAIS_ATIV := ValorF;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0150;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0150New do
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

procedure TACBrSpedPCImportar_Bloco0.Reg0190;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0190New do
  begin
    UNID := Valor;
    DESCR := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0200;
begin
  with ACBrSpedPisCofins.Bloco_0.Registro0200New do
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
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0205;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0205New do
  begin
    DESCR_ANT_ITEM := Valor;
    DT_INI := ValorD;
    DT_FIM := ValorD;
    COD_ANT_ITEM := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0206;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0206New do
  begin
    COD_COMB := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0208;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0208New do
  begin
    COD_TAB := StrToIndCodIncidencia(Valor);
    COD_GRU := Valor;
    MARCA_COM := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0400;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0400New do
  begin
    COD_NAT := Valor;
    DESCR_NAT := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0450;
begin
  with ACBrSpedPisCofins.Bloco_0.Registro0450New do
  begin
    COD_INF := Valor;
    TXT := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0500;
begin
  with ACBrSpedPisCofins.Bloco_0.Registro0500New do
  begin
    DT_ALT := ValorD;
    COD_NAT_CC := StrToNaturezaConta(Valor);
    IND_CTA := StrToIndCTA(Valor);
    NIVEL := Valor;
    COD_CTA := Valor;
    NOME_CTA := Valor;
    COD_CTA_REF := Valor;
    CNPJ_EST := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco0.Reg0600;
begin

  with ACBrSpedPisCofins.Bloco_0.Registro0600New do
  begin
    DT_ALT := ValorD;
    COD_CCUS := Valor;
    CCUS := Valor;
  end;
end;

end.
