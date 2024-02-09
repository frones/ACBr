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

unit pcteConsSitCTe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador,
  ACBrDFeConsts,
  pcteConsts;

type

  TConsSitCTe = class
  private
    FGerador: TGerador;
    FtpAmb: TpcnTipoAmbiente;
    FchCTe: String;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;

    property Gerador: TGerador       read FGerador write FGerador;
    property tpAmb: TpcnTipoAmbiente read FtpAmb   write FtpAmb;
    property chCTe: String           read FchCTe   write FchCTe;
    property Versao: String          read FVersao  write FVersao;
  end;

implementation

uses
  ACBrDFeUtil,
  ACBrUtil.Strings;

{ TConsSitCTe }

constructor TConsSitCTe.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TConsSitCTe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TConsSitCTe.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('consSitCTe ' + NAME_SPACE_CTE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcStr, 'EP03', 'tpAmb', 001, 001, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'EP04', 'xServ', 009, 009, 1, 'CONSULTAR');
  Gerador.wCampo(tcEsp, 'EP05', 'chCTe', 044, 044, 1, OnlyNumber(FchCTe), DSC_CHAVE);
  if not ValidarChave(FchCTe) then
    Gerador.wAlerta('EP05', 'chCTe', '', 'Chave do CTe inválida');
  Gerador.wGrupo('/consSitCTe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

