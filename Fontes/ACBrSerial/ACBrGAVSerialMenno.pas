{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 28/06/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrGAV
******************************************************************************}

{$I ACBr.inc}

unit ACBrGAVSerialMenno;

interface
uses ACBrGAVClass, 
     Classes ;

type
TACBrGAVSerialMenno = class( TACBrGAVClass )
  private

  protected
    function GetGavetaAberta: Boolean; override ;

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;

    Procedure AbreGaveta  ; override ;
    Property GavetaAberta : Boolean read GetGavetaAberta ;
end ;


implementation

uses SysUtils, ACBrUtil
  {$IFNDEF COMPILER6_UP} ,Windows {$ENDIF} ;

{ TACBrGAVSerialMenno }

constructor TACBrGAVSerialMenno.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Menno Serial' ;
  fpAberturaIntervalo := 5000 ;
end;

procedure TACBrGAVSerialMenno.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise Exception.Create(ACBrStr('Esse modelo de Gaveta requer'+#10+
                            'Porta Serial: (COM1, COM2, COM3, ...)'));

  inherited Ativar ; { Abre porta serial }

  try
     fpDevice.Serial.DTR := true ;
     fpDevice.Serial.RTS := true ;
     CalculaProximaAbertura ;
  except
     Desativar ;
     raise
  end ;
end;

procedure TACBrGAVSerialMenno.AbreGaveta;
begin
  Inherited AbreGaveta ;

  fpDevice.Serial.RTS := false ;
  Sleep(200) ;
  fpDevice.Serial.DTR := true ;
  fpDevice.Serial.RTS := true ;
  
  CalculaProximaAbertura ;
end;

function TACBrGAVSerialMenno.GetGavetaAberta: Boolean;
begin
  fpDevice.Serial.DTR := False;
  Sleep(300);
  Result := not fpDevice.Serial.DSR;
  fpDevice.Serial.DTR := true ;
end;

end.
