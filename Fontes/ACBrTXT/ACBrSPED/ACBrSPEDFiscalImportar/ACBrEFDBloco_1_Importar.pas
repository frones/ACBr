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
  ACBrUtil,
  ACBrSpedFiscal,
  ACBrEFDBlocos;

type
  TACBrSpedFiscalImportar_Bloco1 = class(TACBrSpedFiscalImportar_Base)
  private
    procedure Reg1001;
    procedure Reg1010;
    procedure Reg1300;
    procedure Reg1310;
    procedure Reg1320;
    procedure Reg1350;
    procedure Reg1360;
    procedure Reg1370;
    procedure Reg1600;
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

  if (vHead = '1001') then 
    Reg1001
  else if (vHead = '1010') then
    Reg1010
  else if (vHead = '1300') then
    Reg1300
  else if (vHead = '1310') then
    Reg1310
  else if (vHead = '1320') then
    Reg1320
  else if (vHead = '1350') then
    Reg1350
  else if (vHead = '1360') then
    Reg1360
  else if (vHead = '1370') then
    Reg1370
  else if (vHead = '1600') then
    Reg1600;
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

end.
