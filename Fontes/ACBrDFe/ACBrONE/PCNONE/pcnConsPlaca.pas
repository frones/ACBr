{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit pcnConsPlaca;

interface

uses
  SysUtils, Classes,
  ACBrDFeConsts,
  pcnAuxiliar, pcnConversao, pcnGerador, ACBrUtil.Base,
  pcnONEConsts;

type

  TConsPlaca = class
  private
    FGerador: TGerador;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FPlaca: String;
    FVersao: String;
    FdtRef: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;

    property Gerador: TGerador       read FGerador    write FGerador;
    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: String        read FverAplic   write FverAplic;
    property Placa: String           read FPlaca      write FPlaca;
    property Versao: String          read FVersao     write FVersao;
    property dtRef: TDateTime        read FdtRef      write FdtRef;
  end;

implementation

{ TConsPlaca }

constructor TConsPlaca.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TConsPlaca.Destroy;
begin
  FGerador.Free;

  inherited;
end;

function TConsPlaca.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('oneConsPorPlaca ' + NAME_SPACE_ONE + ' versao="' + Versao + '"');

  Gerador.wCampo(tcStr, 'EP03', 'tpAmb     ', 01, 01, 1, tpAmbToStr(tpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'EP04', 'verAplic  ', 01, 20, 1, verAplic, DSC_verAplic);
  Gerador.wCampo(tcStr, 'EP06', 'placa     ', 07, 07, 1, Placa, DSC_Placa);
  Gerador.wCampo(tcDat, 'EP07', 'dtRef     ', 10, 10, 0, FdtRef, DSC_DataRef);
  Gerador.wCampo(tcStr, 'EP08', 'indCompRet', 01, 01, 1, '1');

  Gerador.wGrupo('/oneConsPorPlaca');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

