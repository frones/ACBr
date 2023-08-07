{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibPIXCDBase;

interface

uses
  Classes, SysUtils, ACBrLibComum, ACBrLibPIXCDDataModule, ACBrPIXCD;

type
  { TACBrLibPIXCD }
  TACBrLibPIXCD = class (TACBrLib)
    private
      FPIXCDDM: TLibPIXCDDM;

    protected
      procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
      procedure Executar; Override;

    public
      constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
      destructor Destroy; override;

      property PIXCDDM: TLibPIXCDDM read FPIXCDDM;
  end;

implementation

Uses
  ACBrLibConsts, ACBrLibConfig,
  ACBrLibPIXCDConfig, ACBrLibPIXCDRespostas;

{ TACBrLibPIXCD }

constructor TACBrLibPIXCD.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FPIXCDDM := TLibPIXCDDM.Create(Nil);
  FPIXCDDM.Lib := Self;
end;

destructor TACBrLibPIXCD.Destroy;
begin
  FPIXCDDM.Free;

  inherited Destroy;
end;

procedure TACBrLibPIXCD.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibPIXCDConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibPIXCD.Executar;
begin
  inherited Executar;
  FPIXCDDM.AplicarConfiguracoes;
end;

end.

