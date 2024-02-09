{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit pgnreConsResLoteGNRE;

interface

uses
  SysUtils, Classes,
  ACBrDFeConsts,
  pcnConversao, pcnGerador;

type
  TConsResLoteGNRE = class(TObject)
  private
    FGerador: TGerador;
    Fambiente: TpcnTipoAmbiente;
    FnumeroRecibo: string;
    FIncluirPDFGuias: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: boolean;

    property Gerador: TGerador read FGerador write FGerador;
    property ambiente: TpcnTipoAmbiente read Fambiente write Fambiente;
    property numeroRecibo: string read FnumeroRecibo write FnumeroRecibo;
    property IncluirPDFGuias: Boolean read FIncluirPDFGuias write FIncluirPDFGuias;
  end;

const
  DSC_NREC = 'Numero do recibo';

implementation

{ TConsResLoteGNRE }

constructor TConsResLoteGNRE.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TConsResLoteGNRE.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TConsResLoteGNRE.GerarXML: boolean;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('TConsLote_GNRE ' + NAME_SPACE_GNRE);

  Gerador.wCampo(tcStr, '', 'ambiente    ', 01, 01, 1, tpAmbToStr(FAmbiente), DSC_TPAMB);
  Gerador.wCampo(tcStr, '', 'numeroRecibo', 10, 10, 1, FnumeroRecibo, DSC_NREC);

  if IncluirPDFGuias then
    Gerador.wCampo(tcStr, '', 'incluirPDFGuias', 1, 1, 1, 'S', '');

  Gerador.wGrupo('/TConsLote_GNRE');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
 