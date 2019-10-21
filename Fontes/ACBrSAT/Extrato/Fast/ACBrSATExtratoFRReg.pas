{******************************************************************************}
{ Projeto: Componente ACBrSAT                                                  }
{ Biblioteca multiplataforma de componentes Delphi para emissão Cupom Fiscal   }
{ Eletrônico SAT                                                               }
{                                                                              }
{ Direitos Autorais Reservados (c) 2018 Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
|*
******************************************************************************}
{$I ACBr.inc}

unit ACBrSATExtratoFRReg;

interface

uses
  Classes, SysUtils, ACBrSATExtratoClass, ACBrSATExtratoReportClass,
  pcnCFe, pcnCFeCanc, pcnConversao;

type

  { TACBrSATExtratoFast }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoFast = class( TACBrSATExtratoReportClass )
  private
  protected
    procedure Imprimir;
  public
    procedure ImprimirExtrato(ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;
  end ;

implementation

{$IFNDEF FPC}
//  {$R ACBrSATExtratoFast.dcr}
{$ENDIF}

{ TACBrSATExtratoFast }

procedure Register;
begin
  RegisterComponents('ACBrSAT',[TACBrSATExtratoFast]);
end;

procedure TACBrSATExtratoFast.Imprimir;
begin

end;

procedure TACBrSATExtratoFast.ImprimirExtrato(ACFe: TCFe);
begin
  inherited;
  Imprimir;
end;

procedure TACBrSATExtratoFast.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  inherited;
  Imprimir;
end;

procedure TACBrSATExtratoFast.ImprimirExtratoResumido(ACFe: TCFe);
begin
  inherited;
  Imprimir;
end;

end.
