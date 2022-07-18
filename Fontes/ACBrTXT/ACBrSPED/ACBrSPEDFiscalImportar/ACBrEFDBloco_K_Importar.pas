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

unit ACBrEFDBloco_K_Importar;

interface

uses
  Classes,
  SysUtils,

  ACBrEFDBase,
  ACBrUtil.Strings,
  ACBrSpedFiscal, ACBrEFDBlocos;

type
  TACBrSpedFiscalImportar_BlocoK = class(TACBrSpedFiscalImportar_Base)
  private
    procedure RegK001;
    procedure RegK100;
    procedure RegK200;
    procedure RegK990;
  public
    procedure AnalisaRegistro(const inDelimitador: TStrings); override;
  end;

implementation

procedure TACBrSpedFiscalImportar_BlocoK.AnalisaRegistro(const inDelimitador: TStrings);
var
  vHead: string;
begin
  inherited;
  vHead := Head;
  if (vHead = 'K001') then RegK001
  else if (vHead = 'K100') then RegK100
  else if (vHead = 'K200') then RegK200
  else if (vHead = 'K990') then RegK990;
end;


// abertura do bloco K
procedure TACBrSpedFiscalImportar_BlocoK.RegK001;
begin
  with ACBrSpedFiscal.Bloco_K.RegistroK001New do
  begin
    IND_MOV := StrToIndMov(Valor);
  end;
end;


procedure TACBrSpedFiscalImportar_BlocoK.RegK100;
begin
  with ACBrSpedFiscal.Bloco_K.RegistroK100New do
  begin
    DT_INI := ValorD;
    DT_FIN := ValorD;
  end;
end;

// dados do inventario
procedure TACBrSpedFiscalImportar_BlocoK.RegK200;
begin
  with ACBrSpedFiscal.Bloco_K.RegistroK200New do
  begin
    DT_EST      := ValorD;
    COD_ITEM    := Valor;
    QTD         := ValorF;
    IND_EST     := StrToIndEst( Valor );
    COD_PART    := Valor;
  end;
end;

procedure TACBrSpedFiscalImportar_BlocoK.RegK990;
begin
  with ACBrSpedFiscal.Bloco_K.RegistroK990 do
  begin
    QTD_LIN_K := ValorI;
  end;
end;

end.
