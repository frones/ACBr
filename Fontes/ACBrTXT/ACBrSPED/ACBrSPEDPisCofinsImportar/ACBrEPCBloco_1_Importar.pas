{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Macgayver Armini Apolonio            }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 23/02/2015: Macgayver Armini Apolonio
|*  - Criação
*******************************************************************************}
unit ACBrEPCBloco_1_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEPCBase,
  ACBrUtil, ACBrSpedPisCofins, ACBrEPCBlocos;

type
  TACBrSpedPCImportar_Bloco1 = class(TACBrSpedPCImportar_Base)
  private
  public
    procedure Reg1001;
    procedure Reg1010;
    procedure Reg1020;
    procedure Reg1050;
    procedure Reg1100;
    procedure Reg1101;
    procedure Reg1102;
    procedure Reg1200;
    procedure Reg1210;
    procedure Reg1220;
    procedure Reg1300;
    procedure Reg1500;
    procedure Reg1501;
    procedure Reg1502;
    procedure Reg1600;
    procedure Reg1610;
    procedure Reg1620;
    procedure Reg1700;
    procedure Reg1800;
    procedure Reg1809;
    procedure Reg1900;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

{ TACBrSpedPCImportar_Bloco1 }

procedure TACBrSpedPCImportar_Bloco1.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = '1001') then
    Reg1001
  else if (vHead = '1010') then
    Reg1010
  else if (vHead = '1020') then
    Reg1020
  else if (vHead = '1050') then
    Reg1050
  else if (vHead = '1100') then
    Reg1100
  else if (vHead = '1101') then
    Reg1101
  else if (vHead = '1102') then
    Reg1102
  else if (vHead = '1200') then
    Reg1200
  else if (vHead = '1210') then
    Reg1210
  else if (vHead = '1220') then
    Reg1220
  else if (vHead = '1300') then
    Reg1300
  else if (vHead = '1500') then
    Reg1500
  else if (vHead = '1501') then
    Reg1501
  else if (vHead = '1502') then
    Reg1502
  else if (vHead = '1600') then
    Reg1600
  else if (vHead = '1610') then
    Reg1610
  else if (vHead = '1620') then
    Reg1620
  else if (vHead = '1700') then
    Reg1700
  else if (vHead = '1800') then
    Reg1800
  else if (vHead = '1809') then
    Reg1809
  else if (vHead = '1900') then
    Reg1900;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1001;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1010;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1010New do
  begin
    NUM_PROC := Valor;
    ID_SEC_JUD := Valor;
    ID_VARA := Valor;
    IND_NAT_ACAO := ValorI;
    DESC_DEC_JUD := Valor;
    DT_SENT_JUD := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1020;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1020New do
  begin
    NUM_PROC := Valor;
    IND_NAT_ACAO := ValorI;
    DT_DEC_ADM := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1050;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1050New do
  begin
    DT_REF := ValorD;
    IND_AJ_BC := StrToCodAjBaseCalcContrib(Valor);
    CNPJ := Valor;
    VL_AJ_TOT := ValorF;
    VL_AJ_CST01 := ValorF;
    VL_AJ_CST02 := ValorF;
    VL_AJ_CST03 := ValorF;
    VL_AJ_CST04 := ValorF;
    VL_AJ_CST05 := ValorF;
    VL_AJ_CST06 := ValorF;
    VL_AJ_CST07 := ValorF;
    VL_AJ_CST08 := ValorF;
    VL_AJ_CST09 := ValorF;
    VL_AJ_CST49 := ValorF;
    VL_AJ_CST99 := ValorF;
    IND_APROP := StrToIndicadorApropAjuste(Valor);
    NUM_REC := Valor;
    INFO_COMPL := Valor;
   end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1100;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1100New do
  begin
    PER_APU_CRED := ValorI;
    ORIG_CRED := ValorI;
    CNPJ_SUC := Valor;
    COD_CRED := ValorI;
    VL_CRED_APU := ValorF;
    VL_CRED_EXT_APU := ValorF;
    VL_TOT_CRED_APU := ValorF;
    VL_CRED_DESC_PA_ANT := ValorF;
    VL_CRED_PER_PA_ANT := ValorF;
    VL_CRED_DCOMP_PA_ANT := ValorF;
    SD_CRED_DISP_EFD := ValorF;
    VL_CRED_DESC_EFD := ValorF;
    VL_CRED_PER_EFD := ValorF;
    VL_CRED_DCOMP_EFD := ValorF;
    VL_CRED_TRANS := ValorF;
    VL_CRED_OUT := ValorF;
    SLD_CRED_FIM := ValorF;

  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1101;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1101New do
  begin
    COD_PART := Valor;
    COD_ITEM := Valor;
    COD_MOD := Valor;
    SER := Valor;
    SUB_SER := Valor;
    NUM_DOC := ValorI;
    DT_OPER := ValorD;
    CHV_NFE := Valor;
    VL_OPER := ValorF;
    CFOP := ValorI;
    NAT_BC_CRED := Valor;
    IND_ORIG_CRED := ValorI;
    CST_PIS := ValorI;
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    COD_CTA := Valor;
    COD_CCUS := Valor;
    DESC_COMPL := Valor;
    PER_ESCRIT := ValorI;
    CNPJ := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1102;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1102New do
  begin
    VL_CRED_PIS_TRIB_MI := ValorF;
    VL_CRED_PIS_NT_MI := ValorF;
    VL_CRED_PIS_EXP := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1200;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1200New do
  begin
    PER_APUR_ANT := ValorI;
    NAT_CONT_REC := Valor;
    VL_CONT_APUR := ValorF;
    VL_CRED_PIS_DESC := ValorF;
    VL_CONT_DEV := ValorF;
    VL_OUT_DED := ValorF;
    VL_CONT_EXT := ValorF;
    VL_MUL := ValorF;
    VL_JUR := ValorF;
    DT_RECOL := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1210;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1210New do
  begin
    CNPJ := Valor;
    CST_PIS := StrToCstPis(Valor);
    COD_PART := Valor;
    DT_OPER := ValorD;
    VL_OPER := ValorF;
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    COD_CTA := Valor;
    DESC_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1220;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1220New do
  begin
    PER_APU_CRED := ValorI;
    ORIG_CRED := ValorI;
    COD_CRED := ValorI;
    VL_CRED := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1300;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1300New do
  begin
    IND_NAT_RET := ValorI;
    PR_REC_RET := ValorI;
    VL_RET_APU := ValorF;
    VL_RET_DED := ValorF;
    VL_RET_PER := ValorF;
    VL_RET_DCOMP := ValorF;
    SLD_RET := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1500;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1500New do
  begin
    PER_APU_CRED := ValorI;
    ORIG_CRED := ValorI;
    CNPJ_SUC := Valor;
    COD_CRED := ValorI;
    VL_CRED_APU := ValorF;
    VL_CRED_EXT_APU := ValorF;
    VL_TOT_CRED_APU := ValorF;
    VL_CRED_DESC_PA_ANT := ValorF;
    VL_CRED_PER_PA_ANT := ValorF;
    VL_CRED_DCOMP_PA_ANT := ValorF;
    SD_CRED_DISP_EFD := ValorF;
    VL_CRED_DESC_EFD := ValorF;
    VL_CRED_PER_EFD := ValorF;
    VL_CRED_DCOMP_EFD := ValorF;
    VL_CRED_TRANS := ValorF;
    VL_CRED_OUT := ValorF;
    SLD_CRED_FIM := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1501;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1501New do
  begin
    COD_PART := Valor;
    COD_ITEM := Valor;
    COD_MOD := Valor;
    SER := Valor;
    SUB_SER := Valor;
    NUM_DOC := ValorI;
    DT_OPER := ValorD;
    CHV_NFE := Valor;
    VL_OPER := ValorF;
    CFOP := ValorI;
    NAT_BC_CRED := Valor;
    IND_ORIG_CRED := ValorI;
    CST_COFINS := ValorI;
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
    COD_CCUS := Valor;
    DESC_COMPL := Valor;
    PER_ESCRIT := ValorI;
    CNPJ := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1502;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1502New do
  begin
    VL_CRED_COFINS_TRIB_MI := ValorF;
    VL_CRED_COFINS_NT_MI := ValorF;
    VL_CRED_COFINS_EXP := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1600;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1600New do
  begin
    PER_APUR_ANT := ValorI;
    NAT_CONT_REC := Valor;
    VL_CONT_APUR := ValorF;
    VL_CRED_COFINS_DESC := ValorF;
    VL_CONT_DEV := ValorF;
    VL_OUT_DED := ValorF;
    VL_CONT_EXT := ValorF;
    VL_MUL := ValorF;
    VL_JUR := ValorF;
    DT_RECOL := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1610;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1610New do
  begin
    CNPJ := Valor;
    CST_COFINS := StrToCstCofins(Valor);
    COD_PART := Valor;
    DT_OPER := ValorD;
    VL_OPER := ValorF;
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
    DESC_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1620;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1620New do
  begin
    PER_APU_CRED := ValorI;
    ORIG_CRED := ValorI;
    COD_CRED := ValorI;
    VL_CRED := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1700;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1700New do
  begin
    IND_NAT_RET := ValorI;
    PR_REC_RET := ValorI;
    VL_RET_APU := ValorF;
    VL_RET_DED := ValorF;
    VL_RET_PER := ValorF;
    VL_RET_DCOMP := ValorF;
    SLD_RET := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1800;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1800New do
  begin
    INC_IMOB := Valor;
    REC_RECEB_RET := ValorF;
    REC_FIN_RET := ValorF;
    BC_RET := ValorF;
    ALIQ_RET := ValorF;
    VL_REC_UNI := ValorF;
    DT_REC_UNI := ValorD;
    COD_REC := Valor;
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1809;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1809New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_Bloco1.Reg1900;
begin
  with ACBrSpedPisCofins.Bloco_1.Registro1900New do
  begin
    CNPJ := Valor;
    COD_MOD := Valor;
    SER := Valor;
    SUB_SER := Valor;
    COD_SIT := StrToCodSitF(Valor);
    VL_TOT_REC := ValorF;
    QUANT_DOC := ValorI;
    CST_PIS := StrToCstPis(Valor);
    CST_COFINS := StrToCstCofins(Valor);
    CFOP := ValorI;
    INF_COMPL := Valor;
    COD_CTA := Valor;
  end;
end;

end.
