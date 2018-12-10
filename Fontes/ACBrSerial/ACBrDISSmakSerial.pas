{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
|* 30/09/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrDISSmakSerial
******************************************************************************}

{$I ACBr.inc}

unit ACBrDISSmakSerial;

interface
uses ACBrDISClass,
     Classes;

type

{ TACBrDISSmakSerial }

TACBrDISSmakSerial = class( TACBrDISClass )
  public 
    constructor Create(AOwner: TComponent); override;

    procedure LimparDisplay ; override ;
    procedure LimparLinha( Linha: Integer ) ; override;

    procedure PosicionarCursor(Linha, Coluna: Integer ) ; override ;
    procedure Escrever( const Texto : String ) ; override ;
end ;

implementation
Uses
  SysUtils,
  ACBrConsts,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5{$ENDIF} ;

{ TACBrDISSmakSerial }

constructor TACBrDISSmakSerial.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Smak Serial' ;
  LinhasCount := 2 ;
  Colunas     := 40 ;
end;


procedure TACBrDISSmakSerial.LimparDisplay;
begin
  fpDevice.EnviaString( FF );
end;

procedure TACBrDISSmakSerial.LimparLinha(Linha: Integer);
begin
  if Linha = 1 then
    fpDevice.EnviaString( #31 )
  else
    fpDevice.EnviaString( #30 );
end;

procedure TACBrDISSmakSerial.PosicionarCursor(Linha, Coluna: Integer);
begin
  fpDevice.EnviaString( #18 + chr(Coluna) + chr(Linha) );
end;

procedure TACBrDISSmakSerial.Escrever(const Texto: String);
begin
  fpDevice.EnviaString( STX + Texto + ETX );
end;

end.
