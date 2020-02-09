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

unit ACBrGAVImpressoraComum;

interface
uses
  Classes,
  ACBrGAVClass, ACBrDeviceSerial;

type
TACBrGAVImpressoraComum = class( TACBrGAVClass )
  private
    fsComando : String ;
    
  protected

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;
    Procedure AbreGaveta  ; override ;
end ;


implementation
uses ACBrDevice, ACBrUtil,
     SysUtils;

{ TACBrGAVImpressoraComum }

constructor TACBrGAVImpressoraComum.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpDevice.HandShake := hsRTS_CTS ;
  fpModeloStr := 'Conectada a Imp.Comum' ;
  fsComando   := '' ;
end;

procedure TACBrGAVImpressoraComum.Ativar;
begin
  if fpDevice.Porta = '' then
     raise Exception.Create(ACBrStr('Para usar Gaveta "gavImpressoraComum" deve ser'+
                            ' definida uma Porta Paralela ou Serial'));

  if StrComando = '' then
     raise Exception.Create(ACBrStr('Para usar Gaveta "gavImpressoraComum" deve ser'+
                            ' definido uma String de comando em StrComando'));

  inherited Ativar ; { Apenas ajusta fpAtivo }

  if fpDevice.IsSerialPort then
  begin
     fpDevice.Serial.RaiseExcept     := true ;
     fpDevice.Serial.DeadlockTimeout := 1000 ;
  end ;

  { Traduzindo StrComando }
  fsComando := TraduzComando( StrComando ) ;
end;

procedure TACBrGAVImpressoraComum.AbreGaveta;
begin
  Inherited AbreGaveta ;

  fpDevice.EnviaString( fsComando );

  CalculaProximaAbertura ;
end;

end.
