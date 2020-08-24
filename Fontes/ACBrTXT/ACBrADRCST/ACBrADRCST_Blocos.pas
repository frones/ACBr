{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Ribamar M. Santos                               }
{                              Juliomar Marchetti                              }
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

{$I ACBr.inc}

unit ACBrADRCST_Blocos;

interface

Uses
  SysUtils,
  Classes,
  DateUtils,
  ACBrTXTClass,
  StrUtils;

type
  { TBlocos }

  TBlocos = class
  private
    FREG: string;
  public
    constructor Create;overload;
    property REG: string read FREG;
  end;

  { TCloseBlocos }

  TCloseBlocos = class(TBlocos)
  private
    FQTD_LIN: integer; /// quantidade de linhas
  public
    constructor Create;

    property QTD_LIN: integer read FQTD_LIN;
    procedure Incrementa;
  end;

implementation

{ TCloseBlocos }

constructor TCloseBlocos.Create;
begin
  inherited Create;
  FQTD_LIN := 0;
end;

procedure TCloseBlocos.Incrementa;
begin
  FQTD_LIN := FQTD_LIN + 1;
end;

constructor TBlocos.Create;
begin
  inherited Create;
  FREG := UpperCase(MidStr(ClassName, Length(ClassName) - 3, 4));
  if Length(FREG) <> 4 then
    raise Exception.Create('O tipo do Registro não foi informado corretamente!');
end;

end.
