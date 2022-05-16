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

{$I ACBr.inc}

unit ACBrGAVImpressoraECF;

interface
uses ACBrGAVClass, 
     Classes ;

type
TACBrGAVImpressoraECF = class( TACBrGAVClass )
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

uses ACBrUtil.Strings,
     SysUtils;

{ TACBrGAVImpressoraECF }

constructor TACBrGAVImpressoraECF.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr    := 'Conectada a ECF' ;
  fpDevice.Porta := '' ;
end;

procedure TACBrGAVImpressoraECF.Ativar;
begin
  if not Assigned( fpECF ) then
     raise Exception.Create(ACBrStr('Para Gaveta "gavImpressoraECF", ACBrGAV deve estar'+
                            ' ligado a um componente ACBrECF'));

  if not fpECF.Ativo then
     raise Exception.Create(ACBrStr('Para usar Gaveta "gavImpressoraECF", ACBrECF deve'+
                            ' estar Ativo'));

  inherited Ativar ; { Apenas ajusta fpAtivo }
end;

procedure TACBrGAVImpressoraECF.AbreGaveta;
begin
  Inherited AbreGaveta ;

  fpECF.AbreGaveta ;
  CalculaProximaAbertura ;
end;

function TACBrGAVImpressoraECF.GetGavetaAberta: Boolean;
begin
  result := fpECF.GavetaAberta ;
end;

end.
