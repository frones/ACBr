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
|* 23/02/2015: Macgayver Armini Apolonio - Criação
|* 03/07/2017: Rodrigo Buschmann | Digibyte - Importação ICMS Fiscal
*******************************************************************************}
unit ACBrEFDBloco_H_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEFDBase,
  ACBrUtil, ACBrSpedFiscal, ACBrEFDBlocos;

type
  TACBrSpedFiscalImportar_BlocoH = class(TACBrSpedFiscalImportar_Base)
  private
    procedure RegH001;
    procedure RegH005;
    procedure RegH010;
    procedure RegH020;
  public
    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

procedure TACBrSpedFiscalImportar_BlocoH.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'H001') then RegH001
  else if (vHead = 'H005') then RegH005
  else if (vHead = 'H010') then RegH010
  else if (vHead = 'H020') then RegH020
end;


// abertura do bloco H
procedure TACBrSpedFiscalImportar_BlocoH.RegH001;
begin
  with ACBrSpedFiscal.Bloco_H.RegistroH001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;

// dados totais do inventario
procedure TACBrSpedFiscalImportar_BlocoH.RegH005;
begin
  with ACBrSpedFiscal.Bloco_H.RegistroH005New do
  begin
    DT_INV := ValorD;
    VL_INV := ValorF;
    MOT_INV := StrToMotInv(Valor);
  end;
end;

// dados do inventario
procedure TACBrSpedFiscalImportar_BlocoH.RegH010;
begin
  with ACBrSpedFiscal.Bloco_H.RegistroH010New do
  begin
    COD_ITEM    := Valor;
    UNID        := Valor;
    QTD         := ValorF;
    VL_UNIT     := ValorF;
    VL_ITEM     := ValorF;
    IND_PROP    := StrToIndProp( Valor );
    COD_PART    := Valor;
    TXT_COMPL   := Valor;
    COD_CTA     := Valor;
    VL_ITEM_IR  := ValorF;
  end;
end;

// dados de informacoes complementares do invetario
procedure TACBrSpedFiscalImportar_BlocoH.RegH020;
begin
  with ACBrSpedFiscal.Bloco_H.RegistroH020New do
  begin
    CST_ICMS := Valor;
    BC_ICMS := ValorF;
    VL_ICMS := ValorF;
  end;
end;


end.
