{******************************************************************************}
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

unit ACBrEFDBloco_C_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEFDBase,
  ACBrUtil.Strings,
  ACBrSpedFiscal, ACBrEFDBlocos;

type
  TACBrSpedFiscalImportar_BlocoC = class(TACBrSpedFiscalImportar_Base)
  private
  public
    procedure RegC001;
    procedure RegC100;
    procedure RegC101;
    procedure RegC105;
    procedure RegC110;
    procedure RegC111;
    procedure RegC112;
    procedure RegC113;
    procedure RegC114;
    procedure RegC115;
    procedure RegC116;
    procedure RegC120;
    procedure RegC130;
    procedure RegC140;
    procedure RegC141;
    procedure RegC160;
    procedure RegC165;
    procedure RegC170;
    procedure RegC171;
    procedure RegC172;
    procedure RegC173;
    procedure RegC174;
    procedure RegC175;
    procedure RegC176;
    procedure RegC177;
    procedure RegC178;
    procedure RegC179;

    procedure RegC190;
    procedure RegC195;
    procedure RegC197;

    procedure RegC300;
    procedure RegC310;
    procedure RegC320;
    procedure RegC321;
    procedure RegC350;
    procedure RegC370;
    procedure RegC390;

    procedure RegC400;
    procedure RegC405;
    procedure RegC410;
    procedure RegC420;
    procedure RegC425;
    procedure RegC460;
    procedure RegC465;
    procedure RegC470;
    procedure RegC490;
    procedure RegC495;

    procedure RegC500;
    procedure RegC510;
    procedure RegC590;

    procedure RegC600;
    procedure RegC601;
    procedure RegC610;
    procedure RegC690;

    procedure RegC700;
    procedure RegC790;
    procedure RegC791;

    procedure RegC800;
    procedure RegC850;
    procedure RegC860;
    procedure RegC890;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation


{ TACBrSpedFiscalImportar_BlocoC }

procedure TACBrSpedFiscalImportar_BlocoC.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'C001') then RegC001
  else if (vHead = 'C100') then RegC100
  else if (vHead = 'C101') then RegC101
  else if (vHead = 'C105') then RegC105
  else if (vHead = 'C110') then RegC110
  else if (vHead = 'C111') then RegC111
  else if (vHead = 'C112') then RegC112
  else if (vHead = 'C113') then RegC113
  else if (vHead = 'C114') then RegC114
  else if (vHead = 'C115') then RegC115
  else if (vHead = 'C116') then RegC116
  else if (vHead = 'C120') then RegC120
  else if (vHead = 'C130') then RegC130
  else if (vHead = 'C140') then RegC140
  else if (vHead = 'C141') then RegC141
  else if (vHead = 'C160') then RegC160
  else if (vHead = 'C165') then RegC165
  else if (vHead = 'C170') then RegC170
  else if (vHead = 'C171') then RegC171
  else if (vHead = 'C172') then RegC172
  else if (vHead = 'C173') then RegC173
  else if (vHead = 'C174') then RegC174
  else if (vHead = 'C175') then RegC175
  else if (vHead = 'C176') then RegC176
  else if (vHead = 'C177') then RegC177
  else if (vHead = 'C178') then RegC178
  else if (vHead = 'C179') then RegC179
  else if (vHead = 'C190') then RegC190
  else if (vHead = 'C195') then RegC195
  else if (vHead = 'C197') then RegC197
  else if (vHead = 'C300') then RegC300
  else if (vHead = 'C310') then RegC310
  else if (vHead = 'C320') then RegC320
  else if (vHead = 'C321') then RegC321
  else if (vHead = 'C350') then RegC350
  else if (vHead = 'C370') then RegC370
  else if (vHead = 'C390') then RegC390
  else if (vHead = 'C400') then RegC400
  else if (vHead = 'C405') then RegC405
  else if (vHead = 'C410') then RegC410
  else if (vHead = 'C420') then RegC420
  else if (vHead = 'C425') then RegC425
  else if (vHead = 'C460') then RegC460
  else if (vHead = 'C465') then RegC465
  else if (vHead = 'C470') then RegC470
  else if (vHead = 'C490') then RegC490
  else if (vHead = 'C495') then RegC495
  else if (vHead = 'C500') then RegC500
  else if (vHead = 'C510') then RegC510
  else if (vHead = 'C590') then RegC590
  else if (vHead = 'C600') then RegC600
  else if (vHead = 'C601') then RegC601
  else if (vHead = 'C610') then RegC610
  else if (vHead = 'C690') then RegC690
  else if (vHead = 'C700') then RegC700
  else if (vHead = 'C791') then RegC791
  else if (vHead = 'C800') then RegC800
  else if (vHead = 'C850') then RegC850
  else if (vHead = 'C860') then RegC860
  else if (vHead = 'C890') then RegC890;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC001;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC100;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC100New do
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

procedure TACBrSpedFiscalImportar_BlocoC.RegC101;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC101New do
  begin
    VL_FCP_UF_DEST := ValorF;
    VL_ICMS_UF_DEST := ValorF;
    VL_ICMS_UF_REM := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC105;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC105New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC110;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC110New do
  begin
    COD_INF := Valor;
    TXT_COMPL := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC111;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC111New do
  begin
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC112;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC112New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC113;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC113New do
  begin
    IND_OPER := StrToIndOper(Valor);
    IND_EMIT := StrToIndEmit(Valor);
    COD_PART := Valor;
    COD_MOD := Valor;
    SER := Valor;
    SUB := Valor;
    NUM_DOC := Valor;
    DT_DOC := ValorD;
    CHV_DOCe := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC114;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC114New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC115;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC115New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC116;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC116New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC120;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC120New do
  begin
    COD_DOC_IMP := StrToDoctoImporta(Valor);
    NUM_DOC__IMP := Valor;
    PIS_IMP := ValorF;
    COFINS_IMP := ValorF;
    NUM_ACDRAW := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC130;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC130New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC140;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC140New do
  begin
    IND_EMIT := StrToIndEmit(Valor);
    IND_TIT := StrToIndTipoTitulo(Valor);
    DESC_TIT := Valor;
    NUM_TIT := Valor;
    QTD_PARC := ValorI;
    VL_TIT := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC141;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC141New do
  begin
    NUM_PARC := Valor;
    DT_VCTO := ValorD;
    VL_PARC := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC160;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC160New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC165;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC165New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC170;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC170New do
  begin
    NUM_ITEM := Valor;
    COD_ITEM := Valor;
    DESCR_COMPL := Valor;
    QTD := ValorF;
    UNID := Valor;
    VL_ITEM := ValorF;
    VL_DESC := ValorF;
    IND_MOV := StrToIndMovFisica(Valor);
    CST_ICMS := Valor;
    CFOP := Valor;
    COD_NAT := Valor;
    VL_BC_ICMS := ValorF;
    ALIQ_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_BC_ICMS_ST := ValorF;
    ALIQ_ST := ValorF;
    VL_ICMS_ST := ValorF;
    IND_APUR := StrToApuracaoIPI(Valor);
    CST_IPI := Valor;
    COD_ENQ := Valor;
    VL_BC_IPI := ValorF;
    ALIQ_IPI := ValorF;
    VL_IPI := ValorF;
    CST_PIS := Valor;
    VL_BC_PIS := ValorF;
    ALIQ_PIS_PERC := ValorF;
    QUANT_BC_PIS := ValorF;
    ALIQ_PIS_R := ValorF;
    VL_PIS := ValorF;
    CST_COFINS := Valor;
    VL_BC_COFINS := ValorF;
    ALIQ_COFINS_PERC := ValorF;
    QUANT_BC_COFINS := ValorF;
    ALIQ_COFINS_R := ValorF;
    VL_COFINS := ValorF;
    COD_CTA := Valor;
    VL_ABAT_NT := ValorF
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC171;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC171New do
  begin
    NUM_TANQUE := Valor;
    QTDE := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC172;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC172New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC173;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC173New do
  begin
    LOTE_MED := Valor;
    QTD_ITEM := ValorF;
    DT_FAB := ValorD;
    DT_VAL := ValorD;
    IND_MED := StrToTipoBaseMedicamento(Valor);
    TP_PROD := StrToTipoProduto(Valor);
    VL_TAB_MAX := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC174;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC174New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC175;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC175New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC176;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC176New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC177;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC177New do
  begin
    COD_INF_ITEM := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC178;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC178New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC179;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC179New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC190;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC190New do
  begin
    CST_ICMS := Valor;
    CFOP := Valor;
    ALIQ_ICMS := ValorF;
    VL_OPR := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_BC_ICMS_ST := ValorF;
    VL_ICMS_ST := ValorF;
    VL_RED_BC := ValorF;
    VL_IPI := ValorF;
    COD_OBS := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC195;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC195New do
  begin
      COD_OBS := Valor;    /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
      TXT_COMPL := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC197;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC197New do
  begin
    COD_AJ:= Valor;           /// Código do ajustes/benefício/incentivo, conforme tabela indicada no item 5.3.
    DESCR_COMPL_AJ:= Valor;   /// Descrição complementar do ajuste da apuração, nos casos em que o código da tabela for “9999”
    COD_ITEM := Valor;         /// Código do item (campo 02 do Registro 0200)
    VL_BC_ICMS := ValorF;     /// Base de cálculo do ICMS ou do ICMS ST
    ALIQ_ICMS := ValorF;      /// Alíquota do ICMS
    VL_ICMS := ValorF;        /// Valor do ICMS ou do ICMS ST
    VL_OUTROS := ValorF;      /// Outros valores
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC300;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC300New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC310;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC310New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC320;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC320New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC321;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC321New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC350;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC350New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC370;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC370New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC390;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC390New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC400;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC400New do
  begin
    COD_MOD := Valor;
    ECF_MOD := Valor;
    ECF_FAB := Valor;
    ECF_CX := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC405;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC405New do
  begin
    DT_DOC := ValorD;
    CRO := ValorI;
    CRZ := ValorI;
    NUM_COO_FIN := ValorI;
    GT_FIN := ValorF;
    VL_BRT := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC410;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC410New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC420;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC420New do
  begin
    COD_TOT_PAR := Valor;
    VLR_ACUM_TOT := ValorF;
    NR_TOT := ValorI;
    DESCR_NR_TOT := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC425;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC425New do
  begin
    COD_ITEM := Valor;
    QTD := ValorF;
    UNID := Valor;
    VL_ITEM := ValorF;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC460;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC460New do
  begin
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    NUM_DOC := Valor;
    DT_DOC := ValorD;
    VL_DOC := ValorF;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
    CPF_CNPJ := Valor;
    NOM_ADQ := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC465;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC465New do
  begin
    CHV_CFE := Valor;
    NUM_CCF := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC470;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC470New do
  begin
    COD_ITEM := Valor;
    QTD := ValorF;
    QTD_CANC := ValorF;
    UNID := Valor;
    VL_ITEM := ValorF;
    CST_ICMS := Valor;
    CFOP := Valor;
    ALIQ_ICMS := ValorF;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
  end;
end;


procedure TACBrSpedFiscalImportar_BlocoC.RegC490;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC490New do
  begin
    CST_ICMS := Valor;
    CFOP := Valor;
    ALIQ_ICMS := ValorF;
    VL_OPR := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    COD_OBS := valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC495;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC495New do
  begin
    ALIQ_ICMS := ValorF;
    COD_ITEM := Valor;
    QTD := ValorF;
    QTD_CANC := ValorF;
    UNID := Valor;
    VL_ITEM := ValorF;
    VL_DESC := ValorF;
    VL_CANC := ValorF;
    VL_ACMO := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_ISEN := ValorF;
    VL_NT := ValorF;
    VL_ICMS_ST := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC500;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC500New do
  begin
    IND_OPER := StrToIndTipoOper(Valor);
    IND_EMIT := StrToEmitente(Valor);
    COD_PART := Valor;
    COD_MOD := Valor;
    COD_SIT := StrToCodSit(Valor);
    SER := Valor;
    SUB := Valor;
    COD_CONS := Valor;
    NUM_DOC := Valor;
    DT_DOC := ValorD;
    DT_E_S := ValorD;
    VL_DOC := ValorF;
    VL_DESC := ValorF;
    VL_FORN := ValorF;
    VL_SERV_NT := ValorF;
    VL_TERC := ValorF;
    VL_DA := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_BC_ICMS_ST := ValorF;
    VL_ICMS_ST := ValorF;
    COD_INF := Valor;
    VL_PIS := ValorF;
    VL_COFINS := ValorF;
    TP_LIGACAO := StrToTpLigacao(Valor);
    COD_GRUPO_TENSAO := StrToGrupoTensao(Valor);
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC510;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC510New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC590;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC590New do
  begin
    CST_ICMS := Valor;
    CFOP := Valor;
    ALIQ_ICMS := ValorF;
    VL_OPR := ValorF;
    VL_BC_ICMS := ValorF;
    VL_ICMS := ValorF;
    VL_BC_ICMS_ST := ValorF;
    VL_ICMS_ST := ValorF;
    VL_RED_BC := ValorF;
    COD_OBS := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC600;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC600New do
  begin
    COD_MOD := Valor;
    COD_MUN := Valor;
    SER := Valor;
    SUB := Valor;
    COD_CONS := Valor;
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

procedure TACBrSpedFiscalImportar_BlocoC.RegC601;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC601New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC610;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC610New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC690;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC690New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC700;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC700New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC790;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC790New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC791;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC791New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC800;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC800New do
  begin
    COD_MOD    := Valor;
    COD_SIT    := StrToCodSit(Valor);
    NUM_CFE    := Valor;
    DT_DOC     := ValorD;
    VL_CFE     := ValorF;
    VL_PIS     := ValorF;
    VL_COFINS  := ValorF;
    CNPJ_CPF   := Valor;
    NR_SAT     := Valor;
    CHV_CFE    := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC850;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC850New do
  begin
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC860;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC860New do
  begin
    COD_MOD := Valor;
    NR_SAT := Valor;
    DT_DOC := ValorD;
    DOC_INI := Valor;
    DOC_FIN := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoC.RegC890;
begin
  with ACBrSpedFiscal.Bloco_C.RegistroC890New do
  begin
  end;
end;

end.
