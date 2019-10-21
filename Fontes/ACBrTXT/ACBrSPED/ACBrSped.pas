{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Isaque Pinheiro                      }
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
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrSped;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass;

type
  TWriteRegistroEvent = procedure(var ALinha: String) of object;
  TCheckRegistroEvent = procedure(ARegistro: TObject; var AAbortar: Boolean) of object;

  EACBrSPEDException = class(Exception);

  { TACBrSPED }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrSPED = class(TACBrTXTClass)
  private
    FDT_INI: TDateTime;  /// Data inicial das informações contidas no arquivo
    FDT_FIN: TDateTime;  /// Data final das informações contidas no arquivo
    FGravado: Boolean;
    procedure CriaRegistros; virtual;
    procedure LiberaRegistros; virtual;
  public
    procedure LimpaRegistros;virtual;
    property DT_INI : TDateTime read FDT_INI  write FDT_INI;
    property DT_FIN : TDateTime read FDT_FIN  write FDT_FIN;
    property Gravado: Boolean   read FGravado write FGravado ;
  end;

implementation

{ TACBrSPED }

procedure TACBrSPED.CriaRegistros;
begin

end;

procedure TACBrSPED.LiberaRegistros;
begin

end;

procedure TACBrSPED.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

end.
