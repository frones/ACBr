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


unit ACBrEPCBloco_D_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEPCBase,
  ACBrUtil, ACBrSpedPisCofins, ACBrEPCBlocos;

type
  TACBrSpedPCImportar_BlocoD = class(TACBrSpedPCImportar_Base)
  private
  public
    procedure RegD001;
    procedure RegD010;
    procedure RegD100;
    procedure RegD101;
    procedure RegD105;
    procedure RegD111;
    procedure RegD200;
    procedure RegD201;
    procedure RegD205;
    procedure RegD209;
    procedure RegD300;
    procedure RegD309;
    procedure RegD350;
    procedure RegD359;
    procedure RegD500;
    procedure RegD501;
    procedure RegD505;
    procedure RegD509;
    procedure RegD600;
    procedure RegD601;
    procedure RegD605;
    procedure RegD609;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

{ TACBrSpedPCImportar_BlocoD }

procedure TACBrSpedPCImportar_BlocoD.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'D001') then
    RegD001
  else if (vHead = 'D010') then
    RegD010
  else if (vHead = 'D100') then
    RegD100
  else if (vHead = 'D101') then
    RegD101
  else if (vHead = 'D105') then
    RegD105
  else if (vHead = 'D111') then
    RegD111
  else if (vHead = 'D200') then
    RegD200
  else if (vHead = 'D201') then
    RegD201
  else if (vHead = 'D205') then
    RegD205
  else if (vHead = 'D209') then
    RegD209
  else if (vHead = 'D300') then
    RegD300
  else if (vHead = 'D309') then
    RegD309
  else if (vHead = 'D350') then
    RegD350
  else if (vHead = 'D359') then
    RegD359
  else if (vHead = 'D500') then
    RegD500
  else if (vHead = 'D501') then
    RegD501
  else if (vHead = 'D505') then
    RegD505
  else if (vHead = 'D509') then
    RegD509
  else if (vHead = 'D600') then
    RegD600
  else if (vHead = 'D601') then
    RegD601
  else if (vHead = 'D605') then
    RegD605
  else if (vHead = 'D609') then
    RegD609;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD001;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD010;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD010New do
  begin
    CNPJ := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD100;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD100New do
  begin
    IND_OPER := Valor;
    IND_EMIT := StrToIndEmit(Valor);
    COD_PART := Valor;
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    SER := Valor;
    SUB := Valor;
    NUM_DOC := Valor;
    CHV_CTE := Valor;
    DT_DOC := ValorD;
    DT_A_P := ValorD;
    TP_CT_e := Valor;
    CHV_CTE_REF := Valor;
    VL_DOC := ValorF;
    VL_DESC := ValorF;
    IND_FRT := StrToIndFrt(Valor);
    VL_SERV := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_NT := ValorF;
    COD_INF := Valor;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD101;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD101New do
  begin
    IND_NAT_FRT := StrToNaturezaFrtContratado(Valor);
    VL_ITEM := ValorF;
    CST_PIS := StrToCstPis(Valor);
    NAT_BC_CRED := StrToNatBcCred(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD105;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD105New do
  begin
    IND_NAT_FRT := StrToNaturezaFrtContratado(Valor);
    VL_ITEM := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    NAT_BC_CRED := StrToNatBcCred(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD111;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD111New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD200;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD200New do
  begin
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    SER := Valor;
    SUB := Valor;
    NUM_DOC_INI := ValorI;
    NUM_DOC_FIN := ValorI;
    CFOP := ValorI;
    DT_REF := ValorD;
    VL_DOC := ValorF;
    VL_DESC := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD201;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD201New do
  begin
    CST_PIS := StrToCstPis(Valor);
    VL_ITEM := ValorF;
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD205;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD205New do
  begin
    CST_COFINS := StrToCstCofins(Valor);
    VL_ITEM := ValorF;
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD209;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD209New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD300;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD300New do
  begin
    COD_MOD := Valor;
    SER := Valor;
    SUB := ValorI;
    NUM_DOC_INI := ValorI;
    NUM_DOC_FIN := ValorI;
    CFOP := ValorI;
    DT_REF := ValorD;
    VL_DOC := ValorF;
    VL_DESC := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    CST_COFINS :=  StrToCstCofins(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD309;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD309New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD350;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD350New do
  begin
    COD_MOD := Valor;
    ECF_MOD := Valor;
    ECF_FAB := Valor;
    DT_DOC := ValorD;
    CRO := ValorI;
    CRZ := ValorI;
    NUM_COO_FIN := ValorI;
    GT_FIN := ValorF;
    VL_BRT := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := Valor;
    ALIQ_PIS := Valor;
    QUANT_BC_PIS := Valor;
    ALIQ_PIS_QUANT := Valor;
    VL_PIS := Valor;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := Valor;
    ALIQ_COFINS := Valor;
    QUANT_BC_COFINS := Valor;
    ALIQ_COFINS_QUANT := Valor;
    VL_COFINS := Valor;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD359;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD359New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD500;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD500New do
  begin
    IND_OPER := StrToIndOper(Valor);
    IND_EMIT := StrToIndEmit(Valor);
    COD_PART := Valor;
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    SER := Valor;
    SUB := ValorI;
    NUM_DOC := ValorI;
    DT_DOC := ValorD;
    DT_A_P := ValorD;
    VL_DOC := ValorF;
    VL_DESC := ValorF;
    VL_SERV := ValorF;
    VL_SERV_NT := ValorF;
    VL_TERC := ValorF;
    VL_DA := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    COD_INF := Valor;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD501;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD501New do
  begin
    CST_PIS := StrToCstPis(Valor);
    VL_ITEM := ValorF;
    NAT_BC_CRED := StrToNatBcCred(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD505;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD505New do
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

procedure TACBrSpedPCImportar_BlocoD.RegD509;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD509New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD600;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD600New do
  begin
    COD_MOD := Valor;
    COD_MUN := ValorI;
    SER := Valor;
    SUB := ValorI;
    IND_REC :=  StrToIndRec(Valor);
    QTD_CONS := ValorI;
    DT_DOC_INI := ValorD;
    DT_DOC_FIN := ValorD;
    VL_DOC := ValorF;
    VL_DESC := ValorF;
    VL_SERV := ValorF;
    VL_SERV_NT := ValorF;
    VL_TERC := ValorF;
    VL_DA := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD601;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD601New do
  begin
    COD_CLASS := ValorI;
    VL_ITEM := ValorF;
    VL_DESC := ValorF;
    CST_PIS := StrToCstPis(Valor);
    VL_BC_PIS := ValorF;
    ALIQ_PIS := ValorF;
    VL_PIS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD605;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD605New do
  begin
    COD_CLASS := ValorI;
    VL_ITEM := ValorF;
    VL_DESC := ValorF;
    CST_COFINS := StrToCstCofins(Valor);
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoD.RegD609;
begin
  with ACBrSpedPisCofins.Bloco_D.RegistroD609New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

end.
