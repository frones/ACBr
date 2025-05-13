{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit iiBrasil.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  IniFiles,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { TNFSeR_iiBrasil204 }

  TNFSeR_iiBrasil204 = class(TNFSeR_ABRASFv2)
  protected

    procedure LerINISecaoQuartos(const AINIRec: TMemIniFile); override;
  public

  end;

implementation

uses
  ACBrUtil.Base;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     iiBrasil
//==============================================================================

{ TNFSeR_iiBrasil204 }

procedure TNFSeR_iiBrasil204.LerINISecaoQuartos(const AINIRec: TMemIniFile);
var
  I: Integer;
  sSecao, sFim: string;
begin
  i := 1;
  while true do
  begin
    sSecao := 'Quartos' + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'CodigoInternoQuarto', 'FIM');

    if(Length(sFim) <= 0) or (sFim = 'FIM')then
      break;

    with NFSe.Quartos.New do
    begin
      CodigoInternoQuarto := StrToIntDef(sFim, 0);
      QtdHospedes := AINIRec.ReadInteger(sSecao, 'QtdHospedes', 0);
      CheckIn := AINIRec.ReadDateTime(sSecao, 'CheckIn', 0);
      QtdDiarias := AINIRec.ReadInteger(sSecao, 'QtdDiarias', 0);
      ValorDiaria := StrToFloatDef(AINIRec.ReadString(sSecao, 'ValorDiaria', ''), 0);
    end;

    Inc(i);
  end;
end;

end.
