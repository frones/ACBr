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
unit ACBrEPCBloco_P_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEPCBase,
  ACBrUtil, ACBrSpedPisCofins, ACBrEPCBlocos;

type
  TACBrSpedPCImportar_BlocoP = class(TACBrSpedPCImportar_Base)
  private
  public
    procedure RegP001;
    procedure RegP010;
    procedure RegP100;
    procedure RegP110;
    procedure RegP199;
    procedure RegP200;
    procedure RegP210;

    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

procedure TACBrSpedPCImportar_BlocoP.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'P001') then
    RegP001
  else if (vHead = 'P010') then
    RegP010
  else if (vHead = 'P100') then
    RegP100
  else if (vHead = 'P110') then
    RegP110
  else if (vHead = 'P199') then
    RegP199
  else if (vHead = 'P200') then
    RegP200
  else if (vHead = 'P210') then
    RegP210;
end;

procedure TACBrSpedPCImportar_BlocoP.RegP001;
begin
  with ACBrSpedPisCofins.Bloco_P.RegistroP001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

procedure TACBrSpedPCImportar_BlocoP.RegP010;
begin
  with ACBrSpedPisCofins.Bloco_P.RegistroP010New do
  begin
    CNPJ := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoP.RegP100;
begin
  with ACBrSpedPisCofins.Bloco_P.RegistroP100New do
  begin
    DT_INI := ValorD;
    DT_FIM := ValorD;
    VL_REC_TOT_EST := ValorF;
    COD_ATIV_ECON := Valor;
    VL_REC_ATIV_ESTAB := ValorF;
    VL_EXC := ValorF;
    VL_BC_CONT := ValorF;
    ALIQ_CONT := ValorF;
    VL_CONT_APU := ValorF;
    COD_CTA := Valor;
    INFO_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoP.RegP110;
begin
  with ACBrSpedPisCofins.Bloco_P.RegistroP110New do
  begin
    NUM_CAMPO := Valor;
    COD_DET := Valor;
    DET_VALOR := ValorF;
    INF_COMPL := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoP.RegP199;
begin
  with ACBrSpedPisCofins.Bloco_P.RegistroP199New do
  begin
    NUM_PROC := Valor;
    IND_PROC := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoP.RegP200;
begin
  with ACBrSpedPisCofins.Bloco_P.RegistroP200New do
  begin
    PER_REF := Valor;
    VL_TOT_CONT_APU := ValorF;
    VL_TOT_AJ_REDUC := ValorF;
    VL_TOT_AJ_ACRES := ValorF;
    VL_TOT_CONT_DEV := ValorF;
    COD_REC         := Valor;
  end;
end;

procedure TACBrSpedPCImportar_BlocoP.RegP210;
begin
  with ACBrSpedPisCofins.Bloco_P.RegistroP210New do
  begin
    IND_AJ := Valor;
    VL_AJ := ValorF;
    COD_AJ := Valor;
    NUM_DOC := Valor;
    DESCR_AJ := Valor;
    DT_REF := ValorD;
  end;
end;

end.
