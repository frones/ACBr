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

unit ACBrEFDBloco_E_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEFDBase,
  ACBrUtil.Strings,
  ACBrSpedFiscal, ACBrEFDBlocos;

type
  TACBrSpedFiscalImportar_BlocoE = class(TACBrSpedFiscalImportar_Base)
  private
  public
    procedure RegE001;
    procedure RegE100;
    procedure RegE110;
    procedure RegE111;
    procedure RegE112;
    procedure RegE113;
    procedure RegE115;
    procedure RegE116;
    procedure RegE200;
    procedure RegE210;
    procedure RegE220;
    procedure RegE230;
    procedure RegE240;
    procedure RegE250;
    procedure RegE300;
    procedure RegE310;
    procedure RegE311;
    procedure RegE312;
    procedure RegE313;
    procedure RegE316;
    procedure RegE500;
    procedure RegE510;
    procedure RegE520;
    procedure RegE530;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

{ TACBrSpedFiscalImportar_BlocoE }

procedure TACBrSpedFiscalImportar_BlocoE.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'E001') then RegE001
  else if (vHead = 'E100') then RegE100
  else if (vHead = 'E110') then RegE110
  else if (vHead = 'E111') then RegE111
  else if (vHead = 'E112') then RegE112
  else if (vHead = 'E113') then RegE113
  else if (vHead = 'E115') then RegE115
  else if (vHead = 'E116') then RegE116
  else if (vHead = 'E200') then RegE200
  else if (vHead = 'E210') then RegE210
  else if (vHead = 'E220') then RegE220
  else if (vHead = 'E230') then RegE230
  else if (vHead = 'E240') then RegE240
  else if (vHead = 'E250') then RegE250
  else if (vHead = 'E300') then RegE300
  else if (vHead = 'E310') then RegE310
  else if (vHead = 'E311') then RegE311
  else if (vHead = 'E312') then RegE312
  else if (vHead = 'E313') then RegE313
  else if (vHead = 'E316') then RegE316
  else if (vHead = 'E500') then RegE500
  else if (vHead = 'E510') then RegE510
  else if (vHead = 'E520') then RegE520
  else if (vHead = 'E530') then RegE530;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE001;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE100;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE100New do
  begin
    DT_INI := ValorD;
    DT_FIN := ValorD;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE110;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE110New do
  begin
    VL_TOT_DEBITOS := ValorF;
    VL_AJ_DEBITOS := ValorF;
    VL_TOT_AJ_DEBITOS := ValorF;
    VL_ESTORNOS_CRED := ValorF;
    VL_TOT_CREDITOS := ValorF;
    VL_AJ_CREDITOS := ValorF;
    VL_TOT_AJ_CREDITOS := ValorF;
    VL_ESTORNOS_DEB := ValorF;
    VL_SLD_CREDOR_ANT := ValorF;
    VL_SLD_APURADO := ValorF;
    VL_TOT_DED := ValorF;
    VL_ICMS_RECOLHER := ValorF;
    VL_SLD_CREDOR_TRANSPORTAR := ValorF;
    DEB_ESP := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE111;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE111New do
  begin
    COD_AJ_APUR := Valor;
    DESCR_COMPL_AJ := Valor;
    VL_AJ_APUR := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE112;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE112New do
  begin
    NUM_DA := Valor;
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
    PROC := Valor;
    TXT_COMPL := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE113;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE113New do
  begin
    COD_PART := Valor;
    COD_MOD := Valor;
    SER := Valor;
    SUB := Valor;
    NUM_DOC := Valor;
    DT_DOC := ValorD;
    COD_ITEM := Valor;
    VL_AJ_ITEM := ValorF;
    CHV_NFE := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE115;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE115New do
  begin
    COD_INF_ADIC := Valor;
    VL_INF_ADIC := ValorF;
    DESCR_COMPL_AJ := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE116;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE116New do
  begin
    COD_OR := Valor;
    VL_OR := ValorF;
    DT_VCTO := ValorD;
    COD_REC := Valor;
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
    PROC := Valor;
    TXT_COMPL := Valor;
    MES_REF := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE200;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE200New do
  begin
    UF := Valor;
    DT_INI := ValorD;
    DT_FIN := ValorD;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE210;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE210New do
  begin
    IND_MOV_ST := StrToMovimentoSt(Valor);
    VL_SLD_CRED_ANT_ST := ValorF;
    VL_DEVOL_ST := ValorF;
    VL_RESSARC_ST := ValorF;
    VL_OUT_CRED_ST := ValorF;
    VL_AJ_CREDITOS_ST := ValorF;
    VL_RETENCAO_ST := ValorF;
    VL_OUT_DEB_ST := ValorF;
    VL_AJ_DEBITOS_ST := ValorF;
    VL_SLD_DEV_ANT_ST := ValorF;
    VL_DEDUCOES_ST := ValorF;
    VL_ICMS_RECOL_ST := ValorF;
    VL_SLD_CRED_ST_TRANSPORTAR := ValorF;
    DEB_ESP_ST := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE220;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE220New do
  begin
    COD_AJ_APUR := Valor;
    DESCR_COMPL_AJ := Valor;
    VL_AJ_APUR := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE230;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE230New do
  begin
    NUM_DA := Valor;
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
    PROC := Valor;
    TXT_COMPL := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE240;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE240New do
  begin
    COD_PART := Valor;
    COD_MOD := Valor;
    SER := Valor;
    SUB := Valor;
    NUM_DOC := Valor;
    DT_DOC := ValorD;
    COD_ITEM := Valor;
    VL_AJ_ITEM := ValorF;
    CHV_NFE := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE250;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE250New do
  begin
    COD_OR := Valor;
    VL_OR := ValorF;
    DT_VCTO := ValorD;
    COD_REC := Valor;
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
    PROC := Valor;
    TXT_COMPL := Valor;
    MES_REF := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE300;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE300New do
  begin
    UF := Valor;
    DT_INI := ValorD;
    DT_FIN := ValorD;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE310;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE310New do
  begin
    IND_MOV_DIFAL := StrToMovimentoDIFAL(Valor);
    VL_SLD_CRED_ANT_DIF := ValorF;
    VL_TOT_DEBITOS_DIFAL := ValorF;
    VL_OUT_DEB_DIFAL := ValorF;
    VL_TOT_CREDITOS_DIFAL := ValorF;
    VL_OUT_CRED_DIFAL := ValorF;
    VL_SLD_DEV_ANT_DIFAL := ValorF;
    VL_DEDUCOES_DIFAL := ValorF;
    VL_RECOL_DIFAL := ValorF;
    VL_SLD_CRED_TRANSPORTAR_DIFAL := ValorF;
    DEB_ESP_DIFAL := ValorF;
    VL_SLD_CRED_ANT_FCP := ValorF;
    VL_TOT_DEB_FCP := ValorF;
    VL_OUT_DEB_FCP := ValorF;
    VL_TOT_CRED_FCP := ValorF;
    VL_OUT_CRED_FCP := ValorF;
    VL_SLD_DEV_ANT_FCP := ValorF;
    VL_DEDUCOES_FCP := ValorF;
    VL_RECOL_FCP := ValorF;
    VL_SLD_CRED_TRANSPORTAR_FCP := ValorF;
    DEB_ESP_FCP := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE311;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE311New do
  begin
    COD_AJ_APUR := Valor;
    DESCR_COMPL_AJ := Valor;
    VL_AJ_APUR := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE312;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE312New do
  begin
    NUM_DA := Valor;
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
    PROC := Valor;
    TXT_COMPL := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE313;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE313New do
  begin
    COD_PART := Valor;
    COD_MOD := Valor;
    SER := Valor;
    SUB := Valor;
    NUM_DOC := Valor;
    CHV_DOCe := Valor;
    DT_DOC := ValorD;
    COD_ITEM := Valor;
    VL_AJ_ITEM := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE316;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE316New do
  begin
    COD_OR := Valor;
    VL_OR := ValorF;
    DT_VCTO := ValorD;
    COD_REC := Valor;
    NUM_PROC := Valor;
    IND_PROC := StrToOrigemProcesso(Valor);
    PROC := Valor;
    TXT_COMPL := Valor;
    MES_REF := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE500;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE500New do
  begin
    IND_APUR := StrToApuracaoIPI(Valor);
    DT_INI := ValorD;
    DT_FIN := ValorD;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE510;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE510New do
  begin
    CFOP := Valor;
    CST_IPI := Valor;
    VL_CONT_IPI := ValorF;
    VL_BC_IPI := ValorF;
    VL_IPI := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE520;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE520New do
  begin
    VL_SD_ANT_IPI := ValorF;
    VL_DEB_IPI := ValorF;
    VL_CRED_IPI := ValorF;
    VL_OD_IPI := ValorF;
    VL_OC_IPI := ValorF;
    VL_SC_IPI := ValorF;
    VL_SD_IPI := ValorF;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoE.RegE530;
begin
  with ACBrSpedFiscal.Bloco_E.RegistroE530New do
  begin
    IND_AJ := StrToTipoAjuste(Valor);
    VL_AJ := ValorF;
    COD_AJ := Valor;
    IND_DOC := StrToOrigemDocto(Valor);
    NUM_DOC := Valor;
    DESCR_AJ := Valor;
  end;
end;

end.
