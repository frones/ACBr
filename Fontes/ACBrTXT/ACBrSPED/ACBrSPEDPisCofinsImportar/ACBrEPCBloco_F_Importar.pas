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

unit ACBrEPCBloco_F_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEPCBase,
  ACBrUtil, ACBrSpedPisCofins, ACBrEPCBlocos;

type
  TACBrSpedPCImportar_BlocoF = class(TACBrSpedPCImportar_Base)
  private
  public
    procedure RegF001;
    procedure RegF010;
    procedure RegF100;
    procedure RegF111;
    procedure RegF120;
    procedure RegF129;
    procedure RegF130;
    procedure RegF139;
    procedure RegF150;
    procedure RegF200;
    procedure RegF205;
    procedure RegF210;
    procedure RegF211;
    procedure RegF500;
    procedure RegF509;
    procedure RegF510;
    procedure RegF519;
    procedure RegF525;
    procedure RegF550;
    procedure RegF559;
    procedure RegF560;
    procedure RegF569;
    procedure RegF600;
    procedure RegF700;
    procedure RegF800;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

{ TACBrSpedPCImportar_BlocoF }

procedure TACBrSpedPCImportar_BlocoF.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'F001') then
    RegF001
  else if (vHead = 'F010') then
    RegF010
  else if (vHead = 'F100') then
    RegF100
  else if (vHead = 'F111') then
    RegF111
  else if (vHead = 'F120') then
    RegF120
  else if (vHead = 'F129') then
    RegF129
  else if (vHead = 'F130') then
    RegF130
  else if (vHead = 'F139') then
    RegF139
  else if (vHead = 'F150') then
    RegF150
  else if (vHead = 'F200') then
    RegF200
  else if (vHead = 'F205') then
    RegF205
  else if (vHead = 'F210') then
    RegF210
  else if (vHead = 'F211') then
    RegF211
  else if (vHead = 'F500') then
    RegF500
  else if (vHead = 'F509') then
    RegF509
  else if (vHead = 'F510') then
    RegF510
  else if (vHead = 'F519') then
    RegF519
  else if (vHead = 'F525') then
    RegF525
  else if (vHead = 'F550') then
    RegF550
  else if (vHead = 'F559') then
    RegF559
  else if (vHead = 'F560') then
    RegF560
  else if (vHead = 'F569') then
    RegF569
  else if (vHead = 'F600') then
    RegF600
  else if (vHead = 'F700') then
    RegF700
  else if (vHead = 'F800') then
    RegF800;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF001;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF010;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF010New do
  begin
    CNPJ := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF100;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF100New do
  begin
    IND_OPER := StrToIndTpOperacaoReceita(Valor);
    COD_PART := Valor;
    COD_ITEM := Valor;
    DT_OPER := ValorD;
    VL_OPER := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    NAT_BC_CRED := StrToNatBcCred(Valor);
    IND_ORIG_CRED := StrToIndOrigCred(Valor);
    COD_CTA := Valor;
    COD_CCUS := Valor;
    DESC_DOC_OPER := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF111;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF111New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF120;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF120New do
  begin
    NAT_BC_CRED := StrToNatBcCred(Valor);
    IDENT_BEM_IMOB := Valor;
    IND_ORIG_CRED := Valor;
    IND_UTIL_BEM_IMOB := Valor;
    VL_OPER_DEP := ValorF;
    PARC_OPER_NAO_BC_CRED := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
    COD_CCUS := Valor;
    DESC_BEM_IMOB := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF129;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF129New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF130;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF130New do
  begin
    NAT_BC_CRED := StrToNatBcCred(Valor);
    IDENT_BEM_IMOB := Valor;
    IND_ORIG_CRED := Valor;
    IND_UTIL_BEM_IMOB := Valor;
    MES_OPER_AQUIS := Valor;
    VL_OPER_AQUIS := ValorF;
    PARC_OPER_NAO_BC_CRED := ValorF;
    VL_BC_CRED := ValorF;
    IND_NR_PARC := ValorI;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
    COD_CCUS := Valor;
    DESC_BEM_IMOB := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF139;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF139New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF150;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF150New do
  begin
    NAT_BC_CRED := StrToNatBcCred(Valor);
    VL_TOT_EST := ValorF;
    EST_IMP := ValorI;
    VL_BC_EST := ValorF;
    VL_BC_MEN_EST := ValorF;
    CST_PIS := StrToCstPis(Valor);
    ALIQ_PIS := ValorF;
    VL_CRED_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    ALIQ_COFINS := ValorF;
    VL_CRED_COFINS := ValorF;
    DESC_EST := Valor;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF200;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF200New do
  begin
    IND_OPER := Valor;
    UNID_IMOB := Valor;
    IDENT_EMP := Valor;
    DESC_UNID_IMOB := Valor;
    NUM_CONT := Valor;
    CPF_CNPJ_ADQU := Valor;
    DT_OPER := ValorD;
    VL_TOT_VEND := ValorF;
    VL_REC_ACUM := ValorF;
    VL_TOT_REC := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    PERC_REC_RECEB := ValorF;
    IND_NAT_EMP := Valor;
    INF_COMP := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF205;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF205New do
  begin
    VL_CUS_INC_ACUM_ANT := ValorF;
    VL_CUS_INC_PER_ESC := ValorF;
    VL_CUS_INC_ACUM := ValorF;
    VL_EXC_BC_CUS_INC_ACUM := ValorF;
    VL_BC_CUS_INC := ValorF;
    CST_PIS := StrToCstPis(Valor);
    ALIQ_PIS := ValorF;
    VL_CRED_PIS_ACUM := ValorF;
    VL_CRED_PIS_DESC_ANT := ValorF;
    VL_CRED_PIS_DESC := ValorF;
    VL_CRED_PIS_DESC_FUT := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    ALIQ_COFINS := ValorF;
    VL_CRED_COFINS_ACUM := ValorF;
    VL_CRED_COFINS_DESC_ANT := ValorF;
    VL_CRED_COFINS_DESC := ValorF;
    VL_CRED_COFINS_DESC_FUT := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF210;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF210New do
  begin
    VL_CUS_ORC := ValorF;
    VL_EXC := ValorF;
    VL_CUS_ORC_AJU := ValorF;
    VL_BC_CRED := ValorF;
    CST_PIS := StrToCstPis(Valor);
    ALIQ_PIS := ValorF;
    VL_CRED_PIS_UTIL := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    ALIQ_COFINS := ValorF;
    VL_CRED_COFINS_UTIL := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF211;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF211New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF500;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF500New do
  begin
    VL_REC_CAIXA := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_DESC_PIS := ValorF;
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_DESC_COFINS := ValorF;
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_MOD := Valor;
    CFOP := ValorI;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF509;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF509New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF510;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF510New do
  begin
    VL_REC_CAIXA := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_DESC_PIS := ValorF;
    QUANT_BC_PIS := ValorF;
    ALIQ_PIS_QUANT := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_DESC_COFINS := ValorF;
    QUANT_BC_COFINS := ValorF;
    ALIQ_COFINS_QUANT := ValorF;
    VL_COFINS := ValorF;
    COD_MOD := Valor;
    CFOP := ValorI;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF519;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF519New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF525;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF525New do
  begin
    VL_REC := ValorF;
    IND_REC := StrToInd_Rec(Valor);
    CNPJ_CPF := Valor;
    NUM_DOC := Valor;
    COD_ITEM := Valor;
    VL_REC_DET := ValorF;
    CST_PIS := StrToCstPis(Valor);
    CST_COFINS := StrToCstCofins(Valor);
    INFO_COMPL := Valor;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF550;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF550New do
  begin
    VL_REC_COMP := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_DESC_PIS := ValorF;
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_DESC_COFINS := ValorF;
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_MOD := Valor;
    CFOP := ValorI;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF559;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF559New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF560;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF560New do
  begin
    VL_REC_COMP := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_DESC_PIS := ValorF;
    QUANT_BC_PIS := ValorF;
    ALIQ_PIS_QUANT := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_DESC_COFINS := ValorF;
    QUANT_BC_COFINS := ValorF;
    ALIQ_COFINS_QUANT := ValorF;
    VL_COFINS := ValorF;
    COD_MOD := Valor;
    CFOP := ValorI;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF569;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF569New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF600;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF600New do
  begin
    IND_NAT_RET := StrToIndNatRetFonte(Valor);
    DT_RET := ValorD;
    VL_BC_RET := ValorF;
    VL_RET := ValorF;
    COD_REC := Valor;
    IND_NAT_REC := StrToIndNatRec(Valor);
    CNPJ := Valor;
    VL_RET_PIS := ValorF;
    VL_RET_COFINS := ValorF;
    IND_DEC := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF700;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF700New do
  begin
    IND_ORI_DED := StrToIndOrigemDiversas(Valor);
    IND_NAT_DED := StrToIndNatDeducao(Valor);
    VL_DED_PIS := ValorF;
    VL_DED_COFINS := ValorF;
    VL_BC_OPER := ValorF;
    CNPJ := Valor;
    INF_COMP := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoF.RegF800;
begin
  with ACBrSpedPisCofins.Bloco_F.RegistroF800New do
  begin
    IND_NAT_EVEN := Valor;
    DT_EVEN := ValorD;
    CNPJ_SUCED := Valor;
    PA_CONT_CRED := Valor;
    COD_CRED := StrToCodCred(Valor);
    VL_CRED_PIS := ValorF;
    VL_CRED_COFINS := ValorF;
    PER_CRED_CIS := ValorF;
  end;
end;

end.
