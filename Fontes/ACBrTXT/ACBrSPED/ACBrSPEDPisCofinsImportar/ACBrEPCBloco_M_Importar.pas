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


unit ACBrEPCBloco_M_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEPCBase,
  ACBrUtil, ACBrSpedPisCofins, ACBrEPCBlocos;

type
  TACBrSpedPCImportar_BlocoM = class(TACBrSpedPCImportar_Base)
  private
  public
    procedure RegM001;
    procedure RegM100;
    procedure RegM105;
    procedure RegM110;
    procedure RegM115;
    procedure RegM200;
    procedure RegM205;
    procedure RegM210;
    procedure RegM211;
    procedure RegM215;
    procedure RegM220;
    procedure RegM225;
    procedure RegM230;
    procedure RegM300;
    procedure RegM350;
    procedure RegM400;
    procedure RegM410;
    procedure RegM500;
    procedure RegM505;
    procedure RegM510;
    procedure RegM515;
    procedure RegM600;
    procedure RegM605;
    procedure RegM610;
    procedure RegM611;
    procedure RegM615;
    procedure RegM620;
    procedure RegM625;
    procedure RegM630;
    procedure RegM700;
    procedure RegM800;
    procedure RegM810;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

{ TACBrSpedPCImportar_BlocoI }

procedure TACBrSpedPCImportar_BlocoM.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'M001') then
    RegM001
  else if (vHead = 'M100') then
    RegM100
  else if (vHead = 'M105') then
    RegM105
  else if (vHead = 'M110') then
    RegM110
  else if (vHead = 'M115') then
    RegM115
  else if (vHead = 'M200') then
    RegM200
  else if (vHead = 'M205') then
    RegM205
  else if (vHead = 'M210') then
    RegM210
  else if (vHead = 'M211') then
    RegM211
  else if (vHead = 'M215') then
    RegM215
  else if (vHead = 'M220') then
    RegM220
  else if (vHead = 'M225') then
    RegM225
  else if (vHead = 'M230') then
    RegM230
  else if (vHead = 'M300') then
    RegM300
  else if (vHead = 'M350') then
    RegM350
  else if (vHead = 'M400') then
    RegM400
  else if (vHead = 'M410') then
    RegM410
  else if (vHead = 'M500') then
    RegM500
  else if (vHead = 'M505') then
    RegM505
  else if (vHead = 'M510') then
    RegM510
  else if (vHead = 'M515') then
    RegM515
  else if (vHead = 'M600') then
    RegM600
  else if (vHead = 'M605') then
    RegM605
  else if (vHead = 'M610') then
    RegM610
  else if (vHead = 'M611') then
    RegM611
  else if (vHead = 'M615') then
    RegM615
  else if (vHead = 'M620') then
    RegM620
  else if (vHead = 'M625') then
    RegM625
  else if (vHead = 'M630') then
    RegM630
  else if (vHead = 'M700') then
    RegM700
  else if (vHead = 'M800') then
    RegM800
  else if (vHead = 'M810') then
    RegM810;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM001;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM100;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM100New do
  begin
    COD_CRED := Valor;
    IND_CRED_ORI := StrToIndCredOri(Valor);
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_CRED := ValorF;
    VL_AJUS_ACRES := ValorF;
    VL_AJUS_REDUC := ValorF;
    VL_CRED_DIF := ValorF;
    VL_CRED_DISP := ValorF;
    IND_DESC_CRED := StrToIndDescCred(Valor);
    VL_CRED_DESC := ValorFV;
    SLD_CRED := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM105;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM105New do
  begin
    NAT_BC_CRED := StrToNatBcCred(Valor);
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS_TOT := ValorFV;
    VL_BC_PIS_CUM := ValorFV;
    VL_BC_PIS_NC := ValorFV;
    VL_BC_PIS := ValorFV;
    QUANT_BC_PIS_TOT := ValorFV;
    QUANT_BC_PIS := ValorFV;
    DESC_CRED := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM110;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM110New do
  begin
    IND_AJ := StrToIndAJ(Valor);
    VL_AJ := ValorF;
    COD_AJ := StrToCodAj(Valor);
    NUM_DOC := Valor;
    DESCR_AJ := Valor;
    DT_REF := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM115;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM115New do
  begin
    DET_VALOR_AJ := ValorF;
    CST_PIS := StrToCstPis(Valor);
    DET_BC_CRED := ValorF;
    DET_ALIQ := ValorF;
    DT_OPER_AJ := ValorD;
    DESC_AJ := Valor;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM200;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM200New do
  begin
    VL_TOT_CONT_NC_PER := ValorF;
    VL_TOT_CRED_DESC := ValorF;
    VL_TOT_CRED_DESC_ANT := ValorF;
    VL_TOT_CONT_NC_DEV := ValorF;
    VL_RET_NC := ValorF;
    VL_OUT_DED_NC := ValorF;
    VL_CONT_NC_REC := ValorF;
    VL_TOT_CONT_CUM_PER := ValorF;
    VL_RET_CUM := ValorF;
    VL_OUT_DED_CUM := ValorF;
    VL_CONT_CUM_REC := ValorF;
    VL_TOT_CONT_REC := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM205;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM205New do
  begin
    NUM_CAMPO := Valor;
    COD_REC := Valor;
    VL_DEBITO := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM210;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM210New do
  begin
    COD_CONT := StrToCodCont(Valor);
    VL_REC_BRT := ValorF;
    VL_BC_CONT := ValorF;
    if (ACBrSpedPisCofins.Bloco_0.Registro0000.COD_VER >= vlVersao310) then
    begin
       VL_AJUS_ACRES_BC_PIS := ValorF;
       VL_AJUS_REDUC_BC_PIS := ValorF;
       VL_BC_CONT_AJUS := ValorF;
    end;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_CONT_APUR := ValorF;
    VL_AJUS_ACRES := ValorF;
    VL_AJUS_REDUC := ValorF;
    VL_CONT_DIFER := ValorFV;
    VL_CONT_DIFER_ANT := ValorFV;
    VL_CONT_PER := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM211;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM211New do
  begin
    IND_TIP_COOP := StrToIndTipCoop(Valor);
    VL_BC_CONT_ANT_EXC_COOP := ValorF;
    VL_EXC_COOP_GER := ValorF;
    VL_EXC_ESP_COOP := ValorF;
    VL_BC_CONT := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM215;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM215New do
  begin
    IND_AJ_BC := StrToIndAJ(Valor);
    VL_AJ_BC := ValorF;
    COD_AJ_BC := StrToCodAjBaseCalcContrib(Valor);
    NUM_DOC := Valor;
    DESCR_AJ_BC := Valor;
    DT_REF := ValorD;
    COD_CTA := Valor;
    CNPJ := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM220;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM220New do
  begin
    IND_AJ := StrToIndAJ(Valor);
    VL_AJ := ValorF;
    COD_AJ := StrToCodAj(Valor);
    NUM_DOC := Valor;
    DESCR_AJ := Valor;
    DT_REF := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM225;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM225New do
  begin
    DET_VALOR_AJ := ValorF;
    CST_PIS := StrToCstPis(Valor);
    DET_BC_CRED := ValorF;
    DET_ALIQ := ValorF;
    DT_OPER_AJ := ValorD;
    DESC_AJ := Valor;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM230;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM230New do
  begin
    CNPJ := Valor;
    VL_VEND := ValorF;
    VL_NAO_RECEB := ValorF;
    VL_CONT_DIF := ValorF;
    VL_CRED_DIF := ValorF;
    COD_CRED := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM300;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM300New do
  begin
    COD_CONT := StrToCodCont(Valor);
    VL_CONT_APUR_DIFER := ValorF;
    NAT_CRED_DESC := StrToNatCredDesc(Valor);
    VL_CRED_DESC_DIFER := ValorF;
    VL_CONT_DIFER_ANT := ValorF;
    PER_APUR := Valor;
    DT_RECEB := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM350;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM350New do
  begin
    VL_TOT_FOL := ValorF;
    VL_EXC_BC := ValorF;
    VL_TOT_BC := ValorF;
    ALIQ_PIS_FOL := ValorF;
    VL_TOT_CONT_FOL := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM400;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM400New do
  begin
    CST_PIS := StrToCstPis(Valor);
    VL_TOT_REC := ValorF;
    COD_CTA := Valor;
    DESC_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM410;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM410New do
  begin
    NAT_REC := Valor;
    VL_REC := ValorF;
    COD_CTA := Valor;
    DESC_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM500;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM500New do
  begin
    COD_CRED := Valor;
    IND_CRED_ORI := StrToIndCredOri(Valor);
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_CRED := ValorF;
    VL_AJUS_ACRES := ValorF;
    VL_AJUS_REDUC := ValorF;
    VL_CRED_DIFER := ValorF;
    VL_CRED_DISP := ValorFV;
    IND_DESC_CRED := StrToIndDescCred(Valor);
    VL_CRED_DESC := ValorFV;
    SLD_CRED := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM505;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM505New do
  begin
    NAT_BC_CRED := StrToNatBcCred(Valor);
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS_TOT := ValorFV;
    VL_BC_COFINS_CUM := ValorFV;
    VL_BC_COFINS_NC := ValorFV;
    VL_BC_COFINS := ValorFV;
    QUANT_BC_COFINS_TOT := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    DESC_CRED := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM510;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM510New do
  begin
    IND_AJ := StrToIndAJ(Valor);
    VL_AJ := ValorF;
    COD_AJ := StrToCodAj(Valor);
    NUM_DOC := Valor;
    DESCR_AJ := Valor;
    DT_REF := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM515;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM515New do
  begin
    DET_VALOR_AJ := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    DET_BC_CRED := ValorF;
    DET_ALIQ := ValorF;
    DT_OPER_AJ := ValorD;
    DESC_AJ := Valor;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM600;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM600New do
  begin
    VL_TOT_CONT_NC_PER := ValorF;
    VL_TOT_CRED_DESC := ValorF;
    VL_TOT_CRED_DESC_ANT := ValorF;
    VL_TOT_CONT_NC_DEV := ValorF;
    VL_RET_NC := ValorF;
    VL_OUT_DED_NC := ValorF;
    VL_CONT_NC_REC := ValorF;
    VL_TOT_CONT_CUM_PER := ValorF;
    VL_RET_CUM := ValorF;
    VL_OUT_DED_CUM := ValorF;
    VL_CONT_CUM_REC := ValorF;
    VL_TOT_CONT_REC := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM605;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM605New do
  begin
    NUM_CAMPO := Valor;
    COD_REC := Valor;
    VL_DEBITO := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM610;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM610New do
  begin
    COD_CONT := StrToCodCont(Valor);
    VL_REC_BRT := ValorF;
    VL_BC_CONT := ValorF;
    if (ACBrSpedPisCofins.Bloco_0.Registro0000.COD_VER >= vlVersao310) then
    begin
       VL_AJUS_ACRES_BC_COFINS := ValorF;
       VL_AJUS_REDUC_BC_COFINS := ValorF;
       VL_BC_CONT_AJUS := ValorF;
    end;
    ALIQ_COFINS := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_CONT_APUR := ValorF;
    VL_AJUS_ACRES := ValorF;
    VL_AJUS_REDUC := ValorF;
    VL_CONT_DIFER := ValorFV;
    VL_CONT_DIFER_ANT := ValorFV;
    VL_CONT_PER := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM611;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM611New do
  begin
    IND_TIP_COOP := StrToIndTipCoop(Valor);
    VL_BC_CONT_ANT_EXC_COOP := ValorF;
    VL_EXC_COOP_GER := ValorF;
    VL_EXC_ESP_COOP := ValorF;
    VL_BC_CONT := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM615;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM615New do
  begin
    IND_AJ_BC := StrToIndAJ(Valor);
    VL_AJ_BC := ValorF;
    COD_AJ_BC := StrToCodAjBaseCalcContrib(Valor);
    NUM_DOC := Valor;
    DESCR_AJ_BC := Valor;
    DT_REF := ValorD;
    COD_CTA := Valor;
    CNPJ := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM620;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM620New do
  begin
    IND_AJ := StrToIndAJ(Valor);
    VL_AJ := ValorF;
    COD_AJ := StrToCodAj(Valor);
    NUM_DOC := Valor;
    DESCR_AJ := Valor;
    DT_REF := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM625;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM625New do
  begin
    DET_VALOR_AJ := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    DET_BC_CRED := ValorF;
    DET_ALIQ := ValorF;
    DT_OPER_AJ := ValorD;
    DESC_AJ := Valor;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM630;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM630New do
  begin
    VL_VEND := ValorF;
    VL_NAO_RECEB := ValorF;
    VL_CONT_DIF := ValorF;
    VL_CRED_DIF := ValorF;
    COD_CRED := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM700;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM700New do
  begin
    COD_CONT := StrToCodCont(Valor);
    VL_CONT_APUR_DIFER := ValorF;
    NAT_CRED_DESC := StrToNatCredDesc(Valor);
    VL_CRED_DESC_DIFER := ValorF;
    VL_CONT_DIFER_ANT := ValorF;
    PER_APUR := Valor;
    DT_RECEB := ValorD;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM800;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM800New do
  begin
    CST_COFINS := StrToCstCofins(Valor);
    VL_TOT_REC := ValorF;
    COD_CTA := Valor;
    DESC_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoM.RegM810;
begin
  with ACBrSpedPisCofins.Bloco_M.RegistroM810New do
  begin
    NAT_REC := Valor;
    VL_REC := ValorF;
    COD_CTA := Valor;
    DESC_COMPL := Valor;
  end;
end;

end.
