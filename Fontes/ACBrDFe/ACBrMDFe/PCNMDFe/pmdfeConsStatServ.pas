{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeConsStatServ;

interface
uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnGerador, pmdfeConversaoMDFe, pcnConsts;

type

  TConsStatServ = class(TPersistent)
  private
    FGerador: TGerador;
    FtpAmb: TpcnTipoAmbiente;
    FcUF: Integer;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
  published
    property Gerador: TGerador       read FGerador write FGerador;
    property tpAmb: TpcnTipoAmbiente read FtpAmb   write FtpAmb;
    property cUF: Integer            read FcUF     write FcUF;
    property Versao: String          read FVersao  write FVersao;
  end;

implementation

{ TConsStatServ }

constructor TConsStatServ.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TConsStatServ.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TConsStatServ.GerarXML: Boolean;
begin
 Gerador.ArquivoFormatoXML := '';

 Gerador.wGrupo('consStatServMDFe ' + NAME_SPACE_MDFE + ' versao="' + Versao + '"');
 Gerador.wCampo(tcStr, 'DP03', 'tpAmb', 1, 1, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
 Gerador.wCampo(tcStr, 'DP04', 'xServ', 6, 6, 1, 'STATUS', DSC_XSERV);
 Gerador.wGrupo('/consStatServMDFe');

 Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

