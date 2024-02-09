{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pmdfeConsSitMDFe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador,
  ACBrUtil.Strings,
  pmdfeConsts;

type

  TConsSitMDFe = class
  private
    FGerador: TGerador;
    FtpAmb: TpcnTipoAmbiente;
    FchMDFe: String;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    property Gerador: TGerador       read FGerador write FGerador;
    property tpAmb: TpcnTipoAmbiente read FtpAmb   write FtpAmb;
    property chMDFe: String          read FchMDFe  write FchMDFe;
    property Versao: String          read FVersao  write FVersao;
  end;

implementation

uses
  ACBrDFeConsts,
  ACBrDFeUtil;

{ TConsSitMDFe }

constructor TConsSitMDFe.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TConsSitMDFe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TConsSitMDFe.GerarXML: Boolean;
begin
 Gerador.ArquivoFormatoXML := '';

 Gerador.wGrupo('consSitMDFe ' + NAME_SPACE_MDFE + ' versao="' + Versao + '"');
 Gerador.wCampo(tcStr, 'CP03', 'tpAmb ', 01, 01, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
 Gerador.wCampo(tcStr, 'CP04', 'xServ ', 09, 09, 1, 'CONSULTAR');
 Gerador.wCampo(tcEsp, 'CP05', 'chMDFe', 44, 44, 1, OnlyNumber(FchMDFe), DSC_CHAVE);
 if not ValidarChave(FchMDFe)
  then Gerador.wAlerta('CP05', 'chMDFe', '', 'Chave do MDFe inválida');
 Gerador.wGrupo('/consSitMDFe');

 Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

