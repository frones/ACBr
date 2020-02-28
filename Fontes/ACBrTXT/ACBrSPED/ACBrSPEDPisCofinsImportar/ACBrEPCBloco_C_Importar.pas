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

unit ACBrEPCBloco_C_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEPCBase,
  ACBrUtil, ACBrSpedPisCofins, ACBrEPCBlocos;

type
  TACBrSpedPCImportar_BlocoC = class(TACBrSpedPCImportar_Base)
  private
  public
    procedure RegC001;
    procedure RegC010;
    procedure RegC100;
    procedure RegC110;
    procedure RegC111;
    procedure RegC120;
    procedure RegC170;
    procedure RegC175;
    procedure RegC180;
    procedure RegC181;
    procedure RegC185;
    procedure RegC188;
    procedure RegC190;
    procedure RegC191;
    procedure RegC195;
    procedure RegC198;
    procedure RegC199;

    procedure RegC380;
    procedure RegC381;
    procedure RegC385;
    procedure RegC395;
    procedure RegC396;

    procedure RegC400;
    procedure RegC405;
    procedure RegC481;
    procedure RegC485;
    procedure RegC489;
    procedure RegC490;
    procedure RegC491;
    procedure RegC495;
    procedure RegC499;

    procedure RegC500;
    procedure RegC501;
    procedure RegC505;
    procedure RegC509;

    procedure RegC600;
    procedure RegC601;
    procedure RegC605;
    procedure RegC609;

    procedure RegC800;
    procedure RegC810;
    procedure RegC820;
    procedure RegC830;
    procedure RegC860;
    procedure RegC870;
    procedure RegC880;
    procedure RegC890;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation


{ TACBrSpedPCImportar_BlocoC }

procedure TACBrSpedPCImportar_BlocoC.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'C001') then
    RegC001
  else if (vHead = 'C010') then
    RegC010
  else if (vHead = 'C100') then
    RegC100
  else if (vHead = 'C110') then
    RegC110
  else if (vHead = 'C111') then
    RegC111
  else if (vHead = 'C120') then
    RegC120
  else if (vHead = 'C170') then
    RegC170
  else if (vHead = 'C175') then
    RegC175
  else if (vHead = 'C180') then
    RegC180
  else if (vHead = 'C181') then
    RegC181
  else if (vHead = 'C185') then
    RegC185
  else if (vHead = 'C188') then
    RegC188
  else if (vHead = 'C190') then
    RegC190
  else if (vHead = 'C191') then
    RegC191
  else if (vHead = 'C195') then
    RegC195
  else if (vHead = 'C198') then
    RegC198
  else if (vHead = 'C199') then
    RegC199
  else if (vHead = 'C380') then
    RegC380
  else if (vHead = 'C381') then
    RegC381
  else if (vHead = 'C385') then
    RegC385
  else if (vHead = 'C395') then
    RegC395
  else if (vHead = 'C396') then
    RegC396
  else if (vHead = 'C400') then
    RegC400
  else if (vHead = 'C405') then
    RegC405
  else if (vHead = 'C481') then
    RegC481
  else if (vHead = 'C485') then
    RegC485
  else if (vHead = 'C489') then
    RegC489
  else if (vHead = 'C490') then
    RegC490
  else if (vHead = 'C491') then
    RegC491
  else if (vHead = 'C495') then
    RegC495
  else if (vHead = 'C499') then
    RegC499
  else if (vHead = 'C500') then
    RegC500
  else if (vHead = 'C501') then
    RegC501
  else if (vHead = 'C505') then
    RegC505
  else if (vHead = 'C509') then
    RegC509
  else if (vHead = 'C600') then
    RegC600
  else if (vHead = 'C601') then
    RegC601
  else if (vHead = 'C605') then
    RegC605
  else if (vHead = 'C609') then
    RegC609
  else if (vHead = 'C800') then
    RegC800
  else if (vHead = 'C810') then
    RegC810
  else if (vHead = 'C820') then
    RegC820
  else if (vHead = 'C830') then
    RegC830
  else if (vHead = 'C860') then
    RegC860
  else if (vHead = 'C870') then
    RegC870
  else if (vHead = 'C880') then
    RegC880
  else if (vHead = 'C890') then
    RegC890;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC001;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC010;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC010New do
  begin
    CNPJ := Valor;
    IND_ESCRI := StrToIndEscrituracao(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC100;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC100New do
  begin
    IND_OPER := StrToIndTipoOper(Valor);
    IND_EMIT := StrToEmitente(Valor);
    COD_PART := Valor;
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    SER := Valor;
    NUM_DOC := Valor;
    CHV_NFE := Valor;
    DT_DOC := ValorD;
    DT_E_S := ValorD;
    VL_DOC := ValorF;
    IND_PGTO := StrToIndPgto(Valor);
    VL_DESC := ValorF;
    VL_ABAT_NT := ValorF;
    VL_MERC := ValorF;
    IND_FRT := StrToIndFrt(Valor);
    VL_FRT := ValorF;
    VL_SEG := ValorF;
    VL_OUT_DA := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_BC_ICMS_ST := ValorF;
    VL_ICMS_ST := ValorF;
    VL_IPI := ValorF;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
    VL_PIS_ST := ValorF;
    VL_COFINS_ST := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC110;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC110New do
  begin
    COD_INF := Valor;
    TXT_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC111;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC111New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC120;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC120New do
  begin
    COD_DOC_IMP := StrToDoctoImporta(Valor);
    NUM_DOC__IMP := Valor;
    PIS_IMP := ValorF;
    COFINS_IMP := ValorF;
    NUM_ACDRAW := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC170;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC170New do
  begin
    NUM_ITEM := Valor;
    COD_ITEM := Valor;
    DESCR_COMPL := Valor;
    QTD := ValorFV;
    UNID := Valor;
    VL_ITEM := ValorF;
    VL_DESC := ValorF;
    IND_MOV := StrToIndMovFisica(Valor);
    CST_ICMS := StrToCstIcms(Valor);
    CFOP := Valor;
    COD_NAT := Valor;
    VL_BC_ICMS := ValorF;
    ALIQ_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_BC_ICMS_ST := ValorF;
    ALIQ_ST := ValorF;
    VL_ICMS_ST := ValorF;
    IND_APUR := StrToApuracaoIPI(Valor); // Parei Aqui
    CST_IPI := StrToCstIpi(Valor);
    COD_ENQ := Valor;
    VL_BC_IPI := ValorF;
    ALIQ_IPI := ValorF;
    VL_IPI := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorFV;
    ALIQ_PIS_PERC := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_R := ValorFV;
    VL_PIS := ValorFV;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS_PERC := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_R := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC175;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC175New do
  begin
    CFOP := Valor;
    VL_OPR := ValorFV;
    VL_DESC := ValorFV;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC180;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC180New do
  begin
    COD_MOD := Valor;
    DT_DOC_INI := ValorD;
    DT_DOC_FIN := ValorD;
    COD_ITEM := Valor;
    COD_NCM := Valor;
    EX_IPI := Valor;
    VL_TOT_ITEM := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC181;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC181New do
  begin
    CST_PIS := StrToCstPis(Valor);
    CFOP := Valor;
    VL_ITEM := ValorF;
    VL_DESC := ValorFV;
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC185;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC185New do
  begin
    CST_COFINS        := StrToCstCofins(Valor);
    CFOP              := Valor;
    VL_ITEM           := ValorF;
    VL_DESC           := ValorFV;
    VL_BC_COFINS      := ValorFV;
    ALIQ_COFINS       := ValorFV;
    QUANT_BC_COFINS   := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS         := ValorFV;
    COD_CTA           := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC188;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC188New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC190;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC190New do
  begin
    COD_MOD := Valor;
    DT_REF_INI := ValorD;
    DT_REF_FIN := ValorD;
    COD_ITEM := Valor;
    COD_NCM := Valor;
    EX_IPI := Valor;
    VL_TOT_ITEM := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC191;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC191New do
  begin
    CNPJ_CPF_PART := Valor;
    CST_PIS := StrToCstPis(Valor);
    CFOP := ValorI;
    VL_ITEM := ValorF;
    VL_DESC := ValorFV;
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC195;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC195New do
  begin
    CNPJ_CPF_PART := Valor;
    CST_COFINS := StrToCstCofins(Valor);
    CFOP := ValorI;
    VL_ITEM := ValorF;
    VL_DESC := ValorFV;
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC198;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC198New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC199;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC199New do
  begin
    COD_DOC_IMP := StrToDoctoImporta(Valor);
    NUM_DOC__IMP := Valor;
    VL_PIS_IMP := ValorF;
    VL_COFINS_IMP := ValorF;
    NUM_ACDRAW := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC380;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC380New do
  begin
    COD_MOD := Valor;
    DT_DOC_INI := ValorD;
    DT_DOC_FIN := ValorD;
    NUM_DOC_INI := ValorI;
    NUM_DOC_FIN := ValorI;
    VL_DOC := ValorF;
    VL_DOC_CANC := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC381;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC381New do
  begin
    CST_PIS := StrToCstPis(Valor);
    COD_ITEM := Valor;
    VL_ITEM := ValorF;
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC385;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC385New do
  begin
    CST_COFINS := StrToCstCofins(Valor);
    COD_ITEM := Valor;
    VL_ITEM := ValorF;
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC395;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC395New do
  begin
    COD_MOD := Valor;
    COD_PART := Valor;
    SER := Valor;
    SUB_SER := Valor;
    NUM_DOC := Valor;
    DT_DOC := ValorD;
    VL_DOC := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC396;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC396New do
  begin
    COD_ITEM := Valor;
    VL_ITEM := ValorF;
    VL_DESC := ValorF;
    NAT_BC_CRED := StrToNatBcCred(Valor);
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC400;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC400New do
  begin
    COD_MOD := Valor;
    ECF_MOD := Valor;
    ECF_FAB := Valor;
    ECF_CX := ValorI;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC405;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC405New do
  begin
    DT_DOC := ValorD;
    CRO := ValorI;
    CRZ := ValorI;
    NUM_COO_FIN := ValorI;
    GT_FIN := ValorF;
    VL_BRT := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC481;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC481New do
  begin
    CST_PIS := StrToCstPis(Valor);
    VL_ITEM := ValorF;
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    COD_ITEM := Valor;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC485;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC485New do
  begin
    CST_COFINS := StrToCstCofins(Valor);
    VL_ITEM := ValorF;
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS := ValorFV;
    COD_ITEM := Valor;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC489;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC489New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC490;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC490New do
  begin
    DT_DOC_INI := ValorD;
    DT_DOC_FIN := ValorD;
    COD_MOD := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC491;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC491New do
  begin
    COD_ITEM := Valor;
    CST_PIS := StrToCstPis(Valor);
    CFOP := ValorI;
    VL_ITEM := ValorF;
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC495;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC495New do
  begin
    COD_ITEM := Valor;
    CST_COFINS := StrToCstCofins(Valor);
    CFOP := ValorI;
    VL_ITEM := ValorF;
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC499;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC499New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC500;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC500New do
  begin
    COD_PART := Valor;
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    SER := Valor;
    SUB := ValorI;
    NUM_DOC := ValorI;
    DT_DOC := ValorD;
    DT_ENT := ValorD;
    VL_DOC := ValorF;
    VL_ICMS := ValorF;
    COD_INF := Valor;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC501;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC501New do
  begin
    CST_PIS := StrToCstPis(Valor);
    VL_ITEM := ValorF;
    NAT_BC_CRED := StrToNatBcCred(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS  := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC505;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC505New do
  begin
    CST_COFINS := StrToCstCofins(Valor);
    VL_ITEM := ValorF;
    NAT_BC_CRED := StrToNatBcCred(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC509;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC509New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC600;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC600New do
  begin
    COD_MOD := Valor;
    COD_MUN := ValorI;
    SER := Valor;
    SUB := ValorI;
    COD_CONS := ValorI;
    QTD_CONS := ValorI;
    QTD_CANC := ValorI;
    DT_DOC := ValorD;
    VL_DOC := ValorF;
    VL_DESC := ValorF;
    CONS := ValorI;
    VL_FORN := ValorF;
    VL_SERV_NT := ValorF;
    VL_TERC := ValorF;
    VL_DA := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_BC_ICMS_ST := ValorF;
    VL_ICMS_ST := ValorF;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC601;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC601New do
  begin
    CST_PIS := StrToCstPis(Valor);
    VL_ITEM := ValorF;
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC605;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC605New do
  begin
    CST_COFINS := StrToCstCofins(Valor);
    VL_ITEM := ValorF;
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC609;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC609New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC800;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC800New do
  begin
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    NUM_CFE := ValorI;
    DT_DOC := ValorD;
    VL_CFE := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC810;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC810New do
  begin
    CFOP := Valor;
    VL_ITEM := ValorF;
    COD_ITEM := Valor;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    VL_PIS := ValorFV;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC820;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC820New do
  begin
    CFOP := Valor;
    VL_ITEM := ValorF;
    COD_ITEM := Valor;
    CST_PIS := StrToCstPis(Valor);
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    CST_COFINS := StrToCstCofins(Valor);
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC830;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC830New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC860;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC860New do
  begin
    COD_MOD := Valor;
    NR_SAT := ValorI;
    DT_DOC := ValorD;
    DOC_INI := ValorI;
    DOC_FIM := ValorI;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC870;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC870New do
  begin
    COD_ITEM := Valor;
    CFOP := Valor;
    VL_ITEM := ValorF;
    VL_DESC := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorFV;
    ALIQ_PIS := ValorFV;
    VL_PIS := ValorFV;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorFV;
    ALIQ_COFINS := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC880;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC880New do
  begin
    CFOP := Valor;
    VL_ITEM := ValorF;
    COD_ITEM := Valor;
    CST_PIS := StrToCstPis(Valor);
    QUANT_BC_PIS := ValorFV;
    ALIQ_PIS_QUANT := ValorFV;
    VL_PIS := ValorFV;
    CST_COFINS := StrToCstCofins(Valor);
    QUANT_BC_COFINS := ValorFV;
    ALIQ_COFINS_QUANT := ValorFV;
    VL_COFINS := ValorFV;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoC.RegC890;
begin
  with ACBrSpedPisCofins.Bloco_C.RegistroC890New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

end.
