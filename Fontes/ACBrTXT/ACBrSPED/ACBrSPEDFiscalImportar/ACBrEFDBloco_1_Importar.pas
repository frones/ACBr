{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Macgayver Armini Apolonio e Marcelo Silva       }
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

unit ACBrEFDBloco_1_Importar;

interface

uses
  Classes,
  SysUtils,
  ACBrEFDBase,
  ACBrUtil.Strings,
  ACBrSpedFiscal,
  ACBrEFDBlocos;

type
  TACBrSpedFiscalImportar_Bloco1 = class(TACBrSpedFiscalImportar_Base)
  private
    procedure Reg1001;
    procedure Reg1010;
    procedure Reg1200;
    procedure Reg1210;
    procedure Reg1250;
    procedure Reg1255;
    procedure Reg1300;
    procedure Reg1310;
    procedure Reg1320;
    procedure Reg1350;
    procedure Reg1360;
    procedure Reg1370;
    procedure Reg1600;
    procedure Reg1601;
    procedure Reg1900;
    procedure Reg1910;
    procedure Reg1920;
    procedure Reg1921;
    procedure Reg1922;
    procedure Reg1923;
    procedure Reg1925;
    procedure Reg1926;
    procedure Reg1960;
    procedure Reg1970;
    procedure Reg1975;
    procedure Reg1980;
    procedure Reg1990;
  public
    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

{ TACBrSpedFiscalImportar_Bloco1 }

procedure TACBrSpedFiscalImportar_Bloco1.AnalisaRegistro(
  const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;

  vHead := Head;

  if (vHead = '1001')      then Reg1001
  else if (vHead = '1010') then Reg1010
  else if (vHead = '1200') then Reg1200
  else if (vHead = '1210') then Reg1210
  else if (vHead = '1250') then Reg1250
  else if (vHead = '1255') then Reg1255
  else if (vHead = '1300') then Reg1300
  else if (vHead = '1310') then Reg1310
  else if (vHead = '1320') then Reg1320
  else if (vHead = '1350') then Reg1350
  else if (vHead = '1360') then Reg1360
  else if (vHead = '1370') then Reg1370
  else if (vHead = '1600') then Reg1600
  else if (vHead = '1601') then Reg1601
  else If (vHead = '1900') then Reg1900
  else If (vHead = '1910') then Reg1910
  else If (vHead = '1920') then Reg1920
  else If (vHead = '1921') then Reg1921
  else If (vHead = '1922') then Reg1922
  else If (vHead = '1923') then Reg1923
  else If (vHead = '1925') then Reg1925
  else If (vHead = '1926') then Reg1926
  else If (vHead = '1960') then Reg1960
  else If (vHead = '1970') then Reg1970
  else If (vHead = '1975') then Reg1975
  else If (vHead = '1980') then Reg1980
  else If (vHead = '1990') then Reg1990;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1001;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1010;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1010New do
  begin
    IND_EXP   := Valor;
    IND_CCRF  := Valor;
    IND_COMB  := Valor;
    IND_USINA := Valor;
    IND_VA    := Valor;
    IND_EE    := Valor;
    IND_CART  := Valor;
    IND_FORM  := Valor;
    IND_AER   := Valor;
    IND_GIAF1 := Valor;
    IND_GIAF3 := Valor;
    IND_GIAF4 := Valor;
    IND_REST_RESSARC_COMPL_ICMS := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1200;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1200New do
  begin
    COD_AJ_APUR := Valor;
    SLD_CRED := ValorF;
    CRED_APR := ValorF;
    CRED_RECEB := ValorF;
    CRED_UTIL := ValorF;
    SLD_CRED_FIM := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1210;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1210New do
  begin
    TIPO_UTIL := Valor;
    NR_DOC := Valor;
    VL_CRED_UTIL := ValorF;
    CHV_DOCe := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1250;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1250New do
  begin
    VL_CREDITO_ICMS_OP := ValorF;
    VL_ICMS_ST_REST := ValorF;
    VL_FCP_ST_REST := ValorF;
    VL_ICMS_ST_COMPL := ValorF;
    VL_FCP_ST_COMPL := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1255;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1255New do
  begin
    COD_MOT_REST_COMPL := Valor;
    VL_CREDITO_ICMS_OP_MOT := ValorF;
    VL_ICMS_ST_REST_MOT := ValorF;
    VL_FCP_ST_REST_MOT := ValorF;
    VL_ICMS_ST_COMPL_MOT := ValorF;
    VL_FCP_ST_COMPL_MOT := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1300;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1300New do
  begin
    COD_ITEM := Valor;
    DT_FECH := ValorD;
    ESTQ_ABERT := ValorF;
    VOL_ENTR := ValorF;
    VOL_DISP := ValorF;
    VOL_SAIDAS := ValorF;
    ESTQ_ESCR := ValorF;
    VAL_AJ_PERDA := ValorF;
    VAL_AJ_GANHO := ValorF;
    FECH_FISICO := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1310;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1310New do
  begin
    NUM_TANQUE := Valor;
    ESTQ_ABERT := ValorF;
    VOL_ENTR := ValorF;
    VOL_DISP := ValorF;
    VOL_SAIDAS := ValorF;
    ESTQ_ESCR := ValorF;
    VAL_AJ_PERDA := ValorF;
    VAL_AJ_GANHO := ValorF;
    FECH_FISICO := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1320;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1320New do
  begin
    NUM_BICO := Valor;
    NR_INTERV := Valor;
    MOT_INTERV := Valor;
    NOM_INTERV := Valor;
    CNPJ_INTERV := Valor;
    CPF_INTERV := Valor;
    VAL_FECHA := ValorF;
    VAL_ABERT := ValorF;
    VOL_AFERI := ValorF;
    VOL_VENDAS := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1350;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1350New do
  begin
    SERIE := Valor;
    FABRICANTE := Valor;
    MODELO := Valor;
    TIPO_MEDICAO := TACBrMedicao(ValorI);
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1360;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1360New do
  begin
    NUM_LACRE := Valor;
    DT_APLICACAO := ValorD;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1370;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1370New do
  begin
    NUM_BICO := Valor;
    COD_ITEM := Valor;
    NUM_TANQUE := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1600;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1600New do
  begin
    COD_PART    := Valor;
    TOT_CREDITO := ValorF;
    TOT_DEBITO  := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1601;
begin
  with ACBrSpedFiscal.Bloco_1.Registro1601New do
  begin
    COD_PART_IP := Valor;
    COD_PART_IT := Valor;
    TOT_VS      := ValorF;
    TOT_ISS     := ValorF;
    TOT_OUTROS  := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1900;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1900New Do
   Begin
      IND_APUR_ICMS        := Valor;
      DESCR_COMPL_OUT_APUR := Valor;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1910;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1910New Do
   Begin
      DT_INI := ValorD;
      DT_FIN := ValorD;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1920;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1920New Do
   Begin
      VL_TOT_TRANSF_DEBITOS_OA  := ValorF;
      VL_TOT_AJ_DEBITOS_OA      := ValorF;
      VL_ESTORNOS_CRED_OA       := ValorF;
      VL_TOT_TRANSF_CREDITOS_OA := ValorF;
      VL_TOT_AJ_CREDITOS_OA     := ValorF;
      VL_ESTORNOS_DEB_OA        := ValorF;
      VL_SLD_CREDOR_ANT_OA      := ValorF;
      VL_SLD_APURADO_OA         := ValorF;
      VL_TOT_DED                := ValorF;
      VL_ICMS_RECOLHER_OA       := ValorF;
      VL_SLD_CREDOR_TRANSP_OA   := ValorF;
      DEB_ESP_OA                := ValorF;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1921;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1921New Do
   Begin
      COD_AJ_APUR    := Valor;
      DESCR_COMPL_AJ := Valor;
      VL_AJ_APUR     := ValorF;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1922;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1922New Do
   Begin
      NUM_DA    := Valor;
      NUM_PROC  := Valor;
      IND_PROC  := Valor;
      PROC      := Valor;
      TXT_COMPL := Valor;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1923;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1923New Do
   Begin
      COD_PART   := Valor;
      COD_MOD    := Valor;
      SER        := Valor;
      SUB        := Valor;
      NUM_DOC    := Valor;
      DT_DOC     := ValorD;
      COD_ITEM   := Valor;
      VL_AJ_ITEM := ValorF;
      CHV_DOCe   := Valor;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1925;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1925New Do
   Begin
      COD_INF_ADIC   := Valor;
      VL_INF_ADIC    := ValorF;
      DESCR_COMPL_AJ := Valor;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1926;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1926New Do
   Begin
      COD_OR    := Valor;
      VL_OR     := ValorF;
      DT_VCTO   := ValorD;
      COD_REC   := Valor;
      NUM_PROC  := Valor;
      IND_PROC  := Valor;
      PROC      := Valor;
      TXT_COMPL := Valor;
      MES_REF   := Valor;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1960;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1960New Do
   Begin
      IND_AP := Valor;
      G1_01  := ValorF;
      G1_02  := ValorF;
      G1_03  := ValorF;
      G1_04  := ValorF;
      G1_05  := ValorF;
      G1_06  := ValorF;
      G1_07  := ValorF;
      G1_08  := ValorF;
      G1_09  := ValorF;
      G1_10  := ValorF;
      G1_11  := ValorF;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1970;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1970New Do
   Begin
      IND_AP := Valor;
      G3_01  := ValorF;
      G3_02  := ValorF;
      G3_03  := ValorF;
      G3_04  := ValorF;
      G3_05  := ValorF;
      G3_06  := ValorF;
      G3_07  := ValorF;
      G3_T   := ValorF;
      G3_08  := ValorF;
      G3_09  := ValorF;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1975;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1975New Do
   Begin
      ALIQ_IMP_BASE := ValorF;
      G3_10         := ValorF;
      G3_11         := ValorF;
      G3_12         := ValorF;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1980;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1980New Do
   Begin
      IND_AP := Valor;
      G4_01  := ValorF;
      G4_02  := ValorF;
      G4_03  := ValorF;
      G4_04  := ValorF;
      G4_05  := ValorF;
      G4_06  := ValorF;
      G4_07  := ValorF;
      G4_08  := ValorF;
      G4_09  := ValorF;
      G4_10  := ValorF;
      G4_11  := ValorF;
      G4_12  := ValorF;
   End;
End;

procedure TACBrSpedFiscalImportar_Bloco1.Reg1990;
Begin
   With ACBrSpedFiscal.Bloco_1.Registro1990 Do
   Begin
      QTD_LIN_1 := ValorI;
   End;
End;

End.
